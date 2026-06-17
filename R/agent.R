# agent.R ---------------------------------------------------------------------
# The Agent: a persona + an LLMR config + tools + memory + budgets, with a
# trace of everything it does. Failures raise typed conditions; an error is
# never stored as if the model had said it.

#' Spending and effort limits for an agent
#'
#' Budgets are hard limits, checked before every model call. When a limit
#' would be exceeded, the agent raises a condition of class
#' `llmragent_budget_error` instead of calling the API, so a runaway loop
#' cannot quietly spend money. Catch it with `tryCatch()` if you want
#' graceful degradation.
#'
#' @param max_calls Maximum number of model calls.
#' @param max_tokens Maximum total tokens (sent + received) across calls.
#' @param max_tool_calls Maximum executed tool invocations.
#' @param max_seconds Wall-clock ceiling, measured from the agent's first call.
#' @return An object of class `agent_budget`.
#' @examples
#' b <- budget(max_calls = 10, max_tokens = 50000)
#'
#' \dontrun{
#' cfg <- LLMR::llm_config("groq", "openai/gpt-oss-20b")
#' frugal <- agent("Frugal", cfg, budget = budget(max_calls = 2))
#' frugal$chat("one")
#' frugal$chat("two")
#' tryCatch(frugal$chat("three"),
#'          llmragent_budget_error = function(e) "refused before spending")
#' }
#' @export
budget <- function(max_calls = Inf, max_tokens = Inf,
                   max_tool_calls = Inf, max_seconds = Inf) {
  structure(
    list(max_calls = max_calls, max_tokens = max_tokens,
         max_tool_calls = max_tool_calls, max_seconds = max_seconds),
    class = "agent_budget"
  )
}

#' The Agent class
#'
#' Construct with [agent()]. The methods documented here form the public
#' interface: `chat()` for stateful exchanges, `reply()` for stateless ones
#' (used by [conversation()]), `ask_structured()` for schema-shaped answers,
#' plus accessors for the trace, usage, and transcript.
#'
#' @name Agent-class
#' @aliases Agent
#' @export
Agent <- R6::R6Class(
  "Agent",
  public = list(
    #' @field name Display name, used in transcripts.
    name = NULL,
    #' @field persona System prompt: who the agent is and how it behaves.
    persona = NULL,
    #' @field config The LLMR model configuration.
    config = NULL,
    #' @field tools List of `LLMR::llm_tool()` objects available to the agent.
    tools = NULL,
    #' @field memory The memory object (see [memory]).
    memory = NULL,
    #' @field budget The [budget()] limits.
    budget = NULL,

    #' @description Create an agent (prefer the [agent()] constructor).
    #' @param name Display name.
    #' @param config An `LLMR::llm_config()`.
    #' @param persona System prompt; character scalar or NULL.
    #' @param tools List of `LLMR::llm_tool()` objects (or a single one).
    #' @param memory A memory object; default `memory_buffer(40)`.
    #' @param budget A [budget()] object.
    #' @param quiet If TRUE, `chat()` does not print replies.
    #' @param caller Internal seam for tests: a function
    #'   `(config, messages, tools, ...)` returning an `llmr_response`.
    #' @param stream_caller Internal seam for tests: a function
    #'   `(config, messages, callback, ...)` returning an `llmr_response`.
    initialize = function(name, config, persona = NULL, tools = list(),
                          memory = memory_buffer(), budget = LLMRagent::budget(),
                          quiet = FALSE, caller = NULL, stream_caller = NULL) {
      stopifnot(is.character(name), length(name) == 1L, nzchar(name))
      .check_config(config)
      if (inherits(tools, "llmr_tool")) tools <- list(tools)
      stopifnot(is.list(tools))
      if (!inherits(budget, "agent_budget")) {
        stop("`budget` must be created with budget().", call. = FALSE)
      }
      self$name <- name
      self$config <- config
      self$persona <- persona
      self$tools <- tools
      self$memory <- memory
      self$budget <- budget
      private$quiet <- isTRUE(quiet)
      private$caller <- caller %||% private$default_caller
      private$stream_caller <- stream_caller %||% private$default_stream_caller
      private$trace_rows <- list()
      invisible(self)
    },

    #' @description Stateful exchange: the message and the reply are stored in
    #'   memory, so consecutive calls form a conversation. Tools (if any) are
    #'   executed automatically through LLMR's tool loop.
    #' @param text User message (character scalar).
    #' @param ... Passed to the underlying LLMR call (e.g. `tries`).
    #' @param stream If TRUE, print the reply token by token as it is
    #'   generated (via `LLMR::call_llm_stream()`). Streaming is unavailable
    #'   when the agent has tools; such calls fall back to the tool loop with
    #'   a one-time warning.
    #' @return The reply text, invisibly. The full `llmr_response` of the last
    #'   exchange is available as `$last_response`.
    chat = function(text, ..., stream = FALSE) {
      stopifnot(is.character(text), length(text) == 1L)
      private$assert_budget()
      if (self$memory$needs_compaction()) {
        t0 <- Sys.time()
        cres <- self$memory$compact(self$config)
        cdur <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
        if (inherits(cres, "llmr_response")) {
          private$account(cres, cdur, event = "compact")
        } else {
          private$trace_add("compact", duration = cdur)
        }
      }
      if (isTRUE(stream) && length(self$tools)) {
        warning("Streaming is unavailable for agents with tools; ",
                "using the regular tool loop instead.", call. = FALSE)
        stream <- FALSE
      }
      msgs <- private$context_for(text)
      resp <- if (isTRUE(stream)) {
        private$call_model_stream(msgs, ...)
      } else {
        private$call_model(msgs, ...)
      }
      reply <- as.character(resp)
      self$memory$add("user", text)
      self$memory$add("assistant", reply)
      if (!private$quiet && !isTRUE(stream)) {
        cli::cli_text("{.strong {self$name}}: {reply}")
      }
      invisible(reply)
    },

    #' @description Stateless exchange: persona and tools apply, but nothing
    #'   is written to memory. [conversation()] uses this so the shared
    #'   transcript remains the single source of truth.
    #' @param messages A character scalar, named character vector, or message
    #'   list as accepted by `LLMR::call_llm()`. The persona is prepended as a
    #'   system message when the input does not carry one.
    #' @param ... Passed to the underlying LLMR call.
    #' @return The reply text (character scalar).
    reply = function(messages, ...) {
      private$assert_budget()
      msgs <- private$with_persona(messages)
      resp <- private$call_model(msgs, ...)
      as.character(resp)
    },

    #' @description Ask for a schema-shaped answer, parsed into an R list.
    #'   Stateless (memory is not written); validation is local.
    #' @param text The question or instruction.
    #' @param schema A JSON Schema (R list).
    #' @param ... Passed to the underlying LLMR call.
    #' @return The parsed object (list), or NULL when parsing failed; the raw
    #'   reply stays available in `$last_response`.
    ask_structured = function(text, schema, ...) {
      stopifnot(is.list(schema))
      private$assert_budget()
      cfg <- LLMR::enable_structured_output(self$config, schema = schema)
      msgs <- private$with_persona(text)
      resp <- private$call_model(msgs, ..., .config_override = cfg,
                                 .tools_override = list())
      LLMR::llm_parse_structured(resp)
    },

    #' @description The trace: one row per event (model calls, tool runs,
    #'   memory compactions, budget stops) with tokens and timing.
    trace = function() {
      if (!length(private$trace_rows)) {
        return(tibble::tibble(
          ts = as.POSIXct(character(0)), event = character(0),
          tokens_sent = integer(0), tokens_received = integer(0),
          tool = character(0), duration = numeric(0), note = character(0)))
      }
      do.call(rbind, lapply(private$trace_rows, function(r) {
        tibble::tibble(ts = r$ts, event = r$event,
                       tokens_sent = r$tokens_sent %||% NA_integer_,
                       tokens_received = r$tokens_received %||% NA_integer_,
                       tool = r$tool %||% NA_character_,
                       duration = r$duration %||% NA_real_,
                       note = r$note %||% NA_character_)
      }))
    },

    #' @description Totals: calls made, tokens spent, tool calls, elapsed time.
    usage = function() {
      tibble::tibble(
        calls = private$n_calls,
        tokens_sent = private$tok_sent,
        tokens_received = private$tok_rec,
        tokens_total = private$tok_sent + private$tok_rec,
        tool_calls = private$n_tool_calls,
        seconds = if (is.null(private$t_first)) 0 else
          as.numeric(difftime(Sys.time(), private$t_first, units = "secs"))
      )
    },

    #' @description The agent's own conversation memory as a tibble.
    transcript = function() {
      msgs <- self$memory$get()
      tibble::tibble(
        role = vapply(msgs, `[[`, "", "role"),
        content = vapply(msgs, `[[`, "", "content")
      )
    },

    #' @description Forget the conversation (memory only; trace and usage
    #'   counters are kept, since money was spent).
    reset = function() { self$memory$clear(); invisible(self) },

    #' @description Restore accounting after [load_agent()] (internal). Call,
    #'   token, and tool counters carry over so budgets stay binding across
    #'   sessions; the wall-clock budget restarts.
    #' @param usage A list or one-row frame with `calls`, `tokens_sent`,
    #'   `tokens_received`, `tool_calls`.
    #' @param trace A trace tibble as returned by `$trace()`.
    restore_accounting = function(usage = NULL, trace = NULL) {
      if (!is.null(usage)) {
        private$n_calls      <- as.integer(usage$calls %||% 0L)
        private$tok_sent     <- as.integer(usage$tokens_sent %||% 0L)
        private$tok_rec      <- as.integer(usage$tokens_received %||% 0L)
        private$n_tool_calls <- as.integer(usage$tool_calls %||% 0L)
      }
      if (is.data.frame(trace) && nrow(trace)) {
        private$trace_rows <- lapply(seq_len(nrow(trace)), function(i) {
          as.list(trace[i, , drop = FALSE])
        })
      }
      invisible(self)
    },

    #' @description Compact printout.
    #' @param ... Ignored.
    print = function(...) {
      u <- self$usage()
      cat(sprintf("<Agent %s | %s/%s | %d tool(s) | %s memory>\n",
                  self$name, self$config$provider, self$config$model,
                  length(self$tools), class(self$memory)[1]))
      cat(sprintf("  calls: %d | tokens: %d sent, %d received | tool calls: %d\n",
                  u$calls, u$tokens_sent, u$tokens_received, u$tool_calls))
      if (!is.null(self$persona)) {
        p <- self$persona
        if (nchar(p) > 80) p <- paste0(substr(p, 1, 77), "...")
        cat("  persona: ", p, "\n", sep = "")
      }
      invisible(self)
    },

    #' @field last_response The `llmr_response` from the most recent call.
    last_response = NULL
  ),

  private = list(
    quiet = FALSE,
    caller = NULL,
    stream_caller = NULL,
    trace_rows = list(),
    n_calls = 0L,
    n_tool_calls = 0L,
    tok_sent = 0L,
    tok_rec = 0L,
    t_first = NULL,

    default_caller = function(config, messages, tools, ...) {
      if (length(tools)) {
        remaining <- self$budget$max_tool_calls - private$n_tool_calls
        LLMR::call_llm_tools(config, messages, tools = tools,
                             max_tool_calls = remaining, ...)
      } else {
        LLMR::call_llm_robust(config, messages, ...)
      }
    },

    default_stream_caller = function(config, messages, callback, ...) {
      LLMR::call_llm_stream(config, messages, callback = callback, ...)
    },

    with_persona = function(messages) {
      if (is.character(messages) && is.null(names(messages))) {
        messages <- c(user = unname(messages)[1])
      }
      has_system <- (is.character(messages) && "system" %in% names(messages)) ||
        (is.list(messages) && any(vapply(messages, function(m)
          identical(m$role, "system"), logical(1))))
      if (!is.null(self$persona) && !has_system) {
        if (is.character(messages)) {
          messages <- c(system = self$persona, messages)
        } else {
          messages <- c(list(list(role = "system", content = self$persona)), messages)
        }
      }
      messages
    },

    context_for = function(text) {
      msgs <- list()
      if (!is.null(self$persona)) {
        msgs <- c(msgs, list(list(role = "system", content = self$persona)))
      }
      msgs <- c(msgs, self$memory$get(query = text))
      c(msgs, list(list(role = "user", content = text)))
    },

    call_model = function(messages, ..., .config_override = NULL,
                          .tools_override = NULL) {
      cfg <- .config_override %||% self$config
      tls <- .tools_override %||% self$tools
      if (is.null(private$t_first)) private$t_first <- Sys.time()
      t0 <- Sys.time()
      resp <- tryCatch(
        private$caller(cfg, messages, tls, ...),
        llmr_tool_limit = function(e) {
          private$trace_add("budget_stop", note = "max_tool_calls")
          rlang::abort(
            message = sprintf(
              "Agent '%s' budget exceeded: max_tool_calls (mid tool loop).",
              self$name),
            class = c("llmragent_budget_error", "error", "condition"),
            parent = e)
        })
      private$account(resp, as.numeric(difftime(Sys.time(), t0, units = "secs")))
      resp
    },

    call_model_stream = function(messages, ...) {
      if (is.null(private$t_first)) private$t_first <- Sys.time()
      if (!private$quiet) cli::cli_text("{.strong {self$name}}:")
      cb <- if (private$quiet) function(chunk) invisible(NULL)
            else function(chunk) cat(chunk)
      t0 <- Sys.time()
      resp <- private$stream_caller(self$config, messages, callback = cb, ...)
      if (!private$quiet) cat("\n")
      private$account(resp, as.numeric(difftime(Sys.time(), t0, units = "secs")))
      resp
    },

    account = function(resp, dur, event = "call") {
      if (identical(event, "call")) self$last_response <- resp

      # a tool loop is several model calls; LLMR reports the aggregate in
      # attr "tool_loop", while tokens(resp) covers only the final call
      loop <- attr(resp, "tool_loop")
      if (is.list(loop) && !is.null(loop$model_calls)) {
        private$n_calls <- private$n_calls + max(1L, as.integer(loop$model_calls))
        sent <- loop$sent; rec <- loop$rec
      } else {
        u <- LLMR::tokens(resp)
        private$n_calls <- private$n_calls + 1L
        sent <- u$sent; rec <- u$rec
      }
      private$tok_sent <- .add_na0(private$tok_sent, sent)
      private$tok_rec  <- .add_na0(private$tok_rec, rec)

      hist <- attr(resp, "tool_history")
      if (is.data.frame(hist) && nrow(hist)) {
        private$n_tool_calls <- private$n_tool_calls + nrow(hist)
        for (i in seq_len(nrow(hist))) {
          private$trace_add("tool", tool = hist$name[i],
                            note = substr(hist$result[i], 1L, 200L))
        }
      }
      private$trace_add(event,
                        tokens_sent = suppressWarnings(as.integer(sent)),
                        tokens_received = suppressWarnings(as.integer(rec)),
                        duration = dur)
      invisible(NULL)
    },

    trace_add = function(event, tokens_sent = NA_integer_,
                         tokens_received = NA_integer_, tool = NA_character_,
                         duration = NA_real_, note = NA_character_) {
      private$trace_rows[[length(private$trace_rows) + 1L]] <- list(
        ts = Sys.time(), event = event, tokens_sent = tokens_sent,
        tokens_received = tokens_received, tool = tool,
        duration = duration, note = note)
      invisible(NULL)
    },

    assert_budget = function() {
      b <- self$budget
      fail <- function(what, detail) {
        private$trace_add("budget_stop", note = what)
        rlang::abort(
          message = sprintf("Agent '%s' budget exceeded: %s (%s).",
                            self$name, what, detail),
          class = c("llmragent_budget_error", "error", "condition"))
      }
      if (private$n_calls + 1L > b$max_calls) {
        fail("max_calls", sprintf("%d calls made", private$n_calls))
      }
      if ((private$tok_sent + private$tok_rec) >= b$max_tokens) {
        fail("max_tokens", sprintf("%d tokens used", private$tok_sent + private$tok_rec))
      }
      if (private$n_tool_calls >= b$max_tool_calls) {
        fail("max_tool_calls", sprintf("%d tool calls made", private$n_tool_calls))
      }
      if (!is.null(private$t_first)) {
        el <- as.numeric(difftime(Sys.time(), private$t_first, units = "secs"))
        if (el >= b$max_seconds) fail("max_seconds", sprintf("%.1f s elapsed", el))
      }
      invisible(NULL)
    }
  )
)

#' Create an agent
#'
#' An agent is a persona plus a model: it remembers its conversation (see
#' [memory]), can call R functions you expose as tools (via
#' `LLMR::llm_tool()`), and stops cold when its [budget()] runs out. Use
#' `agent$chat()` for a stateful conversation, `agent$ask_structured()` for
#' schema-shaped answers, and pass agents to [conversation()], [debate()],
#' [focus_group()], [interview()], or [deliberate()] for multi-agent work.
#'
#' Two design decisions worth knowing:
#'
#' - **Failures are errors, not replies.** If a call fails, the typed LLMR
#'   condition propagates; nothing is written into memory, so an API hiccup
#'   cannot masquerade as something the model said.
#' - **Budgets are checked before, not after.** The agent refuses the call
#'   that would break the limit, raising `llmragent_budget_error`.
#'
#' @param name Display name (used in transcripts).
#' @param config An `LLMR::llm_config()` for a generative model.
#' @param persona Optional system prompt: who this agent is, what it wants,
#'   how it speaks. For social-science personas, write it like a character
#'   brief: background, dispositions, speech style.
#' @param tools A `LLMR::llm_tool()` or list of them. Tool calls the model
#'   makes are executed automatically and fed back until it answers.
#' @param memory A [memory] object; default keeps the last 40 messages.
#' @param budget A [budget()]; default unlimited.
#' @param quiet If TRUE, `chat()` does not echo replies to the console.
#' @return An [Agent] (R6) object.
#' @examples
#' \dontrun{
#' cfg <- LLMR::llm_config("groq", "openai/gpt-oss-20b", temperature = 0.7)
#' ada <- agent("Ada", cfg,
#'              persona = "You are Ada, a meticulous statistician. Be brief.")
#' ada$chat("In one sentence: what is overfitting?")
#' ada$chat("And how would you detect it?")   # remembers the thread
#' ada$chat("Walk me through cross-validation.", stream = TRUE)  # live tokens
#' ada$usage()
#' }
#' @seealso [budget()], [memory], [agent_as_tool()], [agent_pipeline()],
#'   [conversation()], [agent_experiment()]
#' @export
agent <- function(name, config, persona = NULL, tools = list(),
                  memory = memory_buffer(), budget = LLMRagent::budget(),
                  quiet = FALSE) {
  Agent$new(name = name, config = config, persona = persona, tools = tools,
            memory = memory, budget = budget, quiet = quiet)
}
