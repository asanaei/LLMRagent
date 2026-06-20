# agent.R ---------------------------------------------------------------------
# The Agent: a persona + an LLMR config + tools + memory + budgets, with a
# trace of everything it does. Failures raise typed conditions; an error is
# never stored as if the model had said it.

#' Spending and effort limits for an agent
#'
#' Budgets are hard limits, checked before every model call. When a limit
#' would be exceeded, the agent raises a condition of class
#' `llmragent_budget_error` instead of calling the API, so a runaway loop
#' cannot spend without leaving a record. Catch it with `tryCatch()` if you want
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
                          guardrails = NULL,
                          quiet = FALSE, caller = NULL, stream_caller = NULL) {
      stopifnot(is.character(name), length(name) == 1L, nzchar(name))
      .check_config(config)
      if (inherits(tools, "llmr_tool")) tools <- list(tools)
      stopifnot(is.list(tools))
      if (!inherits(budget, "agent_budget")) {
        stop("`budget` must be created with budget().", call. = FALSE)
      }
      if (!is.null(guardrails) && !inherits(guardrails, "agent_guardrails")) {
        if (inherits(guardrails, "agent_guardrail")) {
          guardrails <- LLMRagent::guardrails(guardrails)
        } else {
          stop("`guardrails` must be built with guardrails().", call. = FALSE)
        }
      }
      private$guardrails <- guardrails
      self$name <- name
      self$config <- config
      # A persona may be supplied as a plain string (the common case) or as a
      # persona_frame() (richer, with source/scope/variant hashes). Normalize to
      # the string every existing call site reads; stash the frame for provenance.
      if (inherits(persona, "persona_frame")) {
        private$.persona_frame <- persona
        self$persona <- persona$text
      } else {
        self$persona <- persona
      }
      self$tools <- tools
      self$memory <- memory
      self$budget <- budget
      private$quiet <- isTRUE(quiet)
      private$caller <- caller %||% private$default_caller
      private$stream_caller <- stream_caller %||% private$default_stream_caller
      private$spans <- list()
      private$agent_id <- .llmragent_id("agent")
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
      private$run_guardrails("input", text)
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
      # Output guardrails run before the reply enters memory: a blocked output
      # must not be stored (consistent with "failures are never replies").
      private$run_guardrails("output", reply)
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
      private$run_guardrails("input", private$payload_text(messages))
      msgs <- private$with_persona(messages)
      resp <- private$call_model(msgs, ...)
      out <- as.character(resp)
      private$run_guardrails("output", out)
      out
    },

    #' @description Ask for a schema-shaped answer, parsed into an R list.
    #'   Stateless (memory is not written); the reply is parsed locally.
    #' @param text The question or instruction: a character scalar, or a message
    #'   list as accepted by `LLMR::call_llm()` (the persona is prepended as a
    #'   system message when the list has none).
    #' @param schema A JSON Schema (R list).
    #' @param ... Passed to the underlying LLMR call.
    #' @return The parsed object (list), or NULL when parsing failed; the raw
    #'   reply stays available in `$last_response`.
    ask_structured = function(text, schema, ...) {
      stopifnot(is.list(schema))
      private$assert_budget()
      private$run_guardrails("input", private$payload_text(text))
      cfg <- LLMR::enable_structured_output(self$config, schema = schema)
      msgs <- private$with_persona(text)
      resp <- private$call_model(msgs, ..., .config_override = cfg,
                                 .tools_override = list())
      private$run_guardrails("output", as.character(resp))
      LLMR::llm_parse_structured(resp)
    },

    #' @description The trace: one row per event (model calls, tool runs,
    #'   memory compactions, budget stops) with tokens and timing. This is a
    #'   projection of the internal span store onto the legacy event vocabulary;
    #'   richer event types (guardrail, approval, stream) and span linkage are
    #'   available via `as_agent_run(agent)` and `tibble::as_tibble(run, "event")`.
    trace = function() {
      empty <- tibble::tibble(
        ts = as.POSIXct(character(0)), event = character(0),
        tokens_sent = integer(0), tokens_received = integer(0),
        tool = character(0), duration = numeric(0), note = character(0))
      legacy <- c("call", "compact", "tool", "budget_stop")
      rows <- Filter(function(s) (s$event_type %||% "") %in% legacy, private$spans)
      if (!length(rows)) return(empty)
      do.call(rbind, lapply(rows, function(s) {
        tibble::tibble(
          ts = s$started_at %||% Sys.time(),
          event = s$event_type,
          tokens_sent = s$tokens_sent %||% NA_integer_,
          tokens_received = s$tokens_received %||% NA_integer_,
          tool = s$tool %||% NA_character_,
          duration = s$duration_s %||% NA_real_,
          note = s$note %||% NA_character_)
      }))
    },

    #' @description The internal span store (one row per event, richer than
    #'   `trace()`): the source the run object reads. Mainly for `as_agent_run()`;
    #'   most users want `trace()` or a run object.
    internal_spans = function() private$spans,

    #' @description Bind this agent to an active run so its spans are stamped
    #'   with `run_id`/`parent_id` (internal; used by the conversation and
    #'   delegation machinery). Pass `run_id = NULL` to unbind.
    #' @param run_id The active run's id, or NULL to unbind.
    #' @param parent A parent span id for nesting (e.g. a supervisor's tool span).
    bind_run = function(run_id = NULL, parent = NA_character_) {
      private$.run_id <- run_id
      private$.run_parent <- parent
      invisible(self)
    },

    #' @description This agent's stable id (assigned at construction; used to
    #'   attribute spans and to detect shared instances across experiment cells).
    id = function() private$agent_id,

    #' @description The agent's `persona_frame()` if one was supplied, else NULL
    #'   (a plain-string persona). Used by the provenance layer for persona
    #'   hashing and scope conditions.
    persona_frame = function() private$.persona_frame,

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
    #' @param spans A list of span records as returned by `$internal_spans()`.
    #' @param agent_id The agent's persisted id (so a reloaded agent keeps its
    #'   identity for leakage checks).
    restore_accounting = function(usage = NULL, spans = NULL, agent_id = NULL) {
      if (!is.null(usage)) {
        private$n_calls      <- as.integer(usage$calls %||% 0L)
        private$tok_sent     <- as.integer(usage$tokens_sent %||% 0L)
        private$tok_rec      <- as.integer(usage$tokens_received %||% 0L)
        private$n_tool_calls <- as.integer(usage$tool_calls %||% 0L)
      }
      if (is.list(spans) && length(spans)) private$spans <- spans
      if (!is.null(agent_id) && nzchar(agent_id)) private$agent_id <- agent_id
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
    spans = list(),
    agent_id = NA_character_,
    .persona_frame = NULL,
    guardrails = NULL,
    .run_id = NULL,
    .run_parent = NA_character_,
    last_messages = NULL,
    last_cfg = NULL,
    n_calls = 0L,
    n_tool_calls = 0L,
    tok_sent = 0L,
    tok_rec = 0L,
    t_first = NULL,

    default_caller = function(config, messages, tools, ...) {
      if (!length(tools)) {
        return(LLMR::call_llm_robust(config, messages, ...))
      }
      if (.has_gated_tool(tools)) {
        # An approval-gated tool needs a pausable loop, which LLMR's loop is not.
        # Use the native loop (built on LLMR's exported primitives); it raises
        # llmragent_pending_approval when the model calls a gated tool.
        return(.native_tool_loop(config, messages, tools, agent = self, ...))
      }
      remaining <- self$budget$max_tool_calls - private$n_tool_calls
      LLMR::call_llm_tools(config, messages, tools = tools,
                           max_tool_calls = remaining, ...)
    },

    default_stream_caller = function(config, messages, callback, ...) {
      LLMR::call_llm_stream(config, messages, callback = callback, ...)
    },

    # Best-effort user-text extraction from any message shape, for input
    # guardrails (a bare string, a named vector, or a list of role/content).
    payload_text = function(messages) {
      if (is.character(messages)) {
        nm <- names(messages)
        if (is.null(nm)) return(paste(messages, collapse = "\n"))
        u <- messages[nm %in% c("user", "")]
        return(paste(if (length(u)) u else messages, collapse = "\n"))
      }
      if (is.list(messages)) {
        txt <- vapply(messages, function(m) {
          if (is.list(m) && identical(m$role, "user") && is.character(m$content))
            m$content[1] else ""
        }, character(1))
        txt <- txt[nzchar(txt)]
        if (length(txt)) return(paste(txt, collapse = "\n"))
      }
      ""
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
      # Stash what we are about to send so account() can build the canonical
      # per-call provenance row (llm_response_record needs the request + config).
      private$last_messages <- messages
      private$last_cfg <- cfg
      if (is.null(private$t_first)) private$t_first <- Sys.time()
      t0 <- Sys.time()
      resp <- tryCatch(
        private$caller(cfg, messages, tls, ...),
        llmr_tool_limit = function(e) {
          private$span_add("budget_stop", note = "max_tool_calls")
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
      private$last_messages <- messages
      private$last_cfg <- self$config
      if (is.null(private$t_first)) private$t_first <- Sys.time()
      if (!private$quiet) cli::cli_text("{.strong {self$name}}:")
      cb <- if (private$quiet) function(chunk) invisible(NULL)
            else function(chunk) cat(chunk)
      t0 <- Sys.time()
      resp <- private$stream_caller(self$config, messages, callback = cb, ...)
      if (!private$quiet) cat("\n")
      # A streamed call is still a model call: keep event = "call" so trace()
      # semantics are unchanged. The span records streamed = TRUE in $meta.
      private$account(resp, as.numeric(difftime(Sys.time(), t0, units = "secs")),
                      event = "call", streamed = TRUE)
      resp
    },

    account = function(resp, dur, event = "call", streamed = FALSE) {
      if (identical(event, "call")) self$last_response <- resp
      ended <- Sys.time()
      started <- ended - dur

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

      # The canonical per-call provenance row (16 columns incl. request_hash,
      # served model_version, success). Composing LLMR rather than reinventing.
      rec_row <- tryCatch(
        LLMR::llm_response_record(
          resp, request = private$last_messages, config = private$last_cfg,
          started_at = started, ended_at = ended),
        error = function(e) NULL)
      ok <- if (is.data.frame(rec_row)) isTRUE(rec_row$success[[1]]) else TRUE

      hist <- attr(resp, "tool_history")
      if (is.data.frame(hist) && nrow(hist)) {
        private$n_tool_calls <- private$n_tool_calls + nrow(hist)
        for (i in seq_len(nrow(hist))) {
          private$span_add(
            "tool", tool = hist$name[i],
            note = substr(hist$result[i], 1L, 200L),
            meta = list(
              arguments = hist$arguments[i],
              result = hist$result[i],
              arguments_hash = LLMR::llm_hash(hist$arguments[i]),
              result_hash = LLMR::llm_hash(hist$result[i]),
              round = if ("round" %in% names(hist)) hist$round[i] else NA_integer_))
          # Tool-stage guardrails: check each executed call's name/arguments/
          # result. A blocking guardrail aborts the exchange (a poisoned tool
          # result does not silently reach downstream use); every verdict is
          # recorded as a guardrail event.
          private$run_tool_guardrails(hist$name[i], hist$arguments[i], hist$result[i])
        }
      }
      private$span_add(
        event,
        tokens_sent = suppressWarnings(as.integer(sent)),
        tokens_received = suppressWarnings(as.integer(rec)),
        duration = dur, started_at = started, ended_at = ended,
        status = if (ok) "ok" else "error",
        request_hash = if (is.data.frame(rec_row)) rec_row$request_hash[[1]] else NA_character_,
        response_id = if (is.data.frame(rec_row)) rec_row$response_id[[1]] else NA_character_,
        # Carry the rendered request messages, the served model id, the usage,
        # and the reply so the archive can emit a true LLMR audit-log record
        # (request body included) even when llm_log_enable() was not active.
        meta = list(record = rec_row, streamed = isTRUE(streamed),
                    request = private$last_messages,
                    provider = private$last_cfg$provider %||% NA_character_,
                    model = private$last_cfg$model %||% NA_character_,
                    model_version = if (inherits(resp, "llmr_response")) resp$model_version else NA_character_,
                    finish_reason = if (inherits(resp, "llmr_response")) resp$finish_reason else NA_character_,
                    text = if (inherits(resp, "llmr_response")) resp$text else NA_character_,
                    usage = if (inherits(resp, "llmr_response")) resp$usage else NULL))
      invisible(NULL)
    },

    # The single low-level event-recording primitive. Every span carries
    # identity (span_id/parent_id/run_id/agent_id) so a run object can stitch
    # multiple agents' events into one graph.
    span_add = function(event_type, tokens_sent = NA_integer_,
                        tokens_received = NA_integer_, tool = NA_character_,
                        duration = NA_real_, note = NA_character_,
                        status = NA_character_, started_at = NULL, ended_at = NULL,
                        request_hash = NA_character_, response_id = NA_character_,
                        meta = NULL) {
      now <- Sys.time()
      private$spans[[length(private$spans) + 1L]] <- list(
        span_id = .llmragent_id("span"),
        parent_id = private$.run_parent %||% NA_character_,
        run_id = private$.run_id %||% NA_character_,
        agent_id = private$agent_id,
        event_type = event_type,
        status = status,
        started_at = started_at %||% now,
        ended_at = ended_at %||% now,
        duration_s = duration,
        tokens_sent = tokens_sent,
        tokens_received = tokens_received,
        tool = tool,
        request_hash = request_hash,
        response_id = response_id,
        note = note,
        meta = meta)
      invisible(NULL)
    },

    # Emit one guardrail span per evaluated check (pass or fail). Surfaced in the
    # event level, never in trace() (the legacy vocabulary filter omits it).
    guard_record = function(name, status, reason) {
      private$span_add("guardrail", tool = name, status = status, note = reason)
    },

    # Run input/output guardrails for this agent over `payload`. Raises
    # llmragent_guardrail_block on a blocking failure; records every verdict.
    run_guardrails = function(stage, payload) {
      gs <- private$guardrails
      if (is.null(gs) || !length(gs)) return(invisible(NULL))
      .run_guardrails(gs, stage = stage, payload = as.character(payload)[1],
                      context = list(agent = self$name),
                      record = function(n, s, r) private$guard_record(n, s, r))
    },

    # Run tool-stage guardrails over one executed tool call. The payload is a
    # list(name, arguments, result); a blocking guardrail aborts the exchange.
    run_tool_guardrails = function(name, arguments, result) {
      gs <- private$guardrails
      if (is.null(gs) || !length(gs)) return(invisible(NULL))
      if (!any(vapply(gs, function(g) identical(g$stage, "tool"), logical(1)))) {
        return(invisible(NULL))
      }
      payload <- list(name = name, arguments = arguments, result = result)
      .run_guardrails(gs, stage = "tool", payload = payload,
                      context = list(agent = self$name, phase = "post"),
                      record = function(n, s, r) private$guard_record(n, s, r))
    },

    # Back-compat shim for the few non-call event emitters (budget_stop,
    # compaction fallback). Forwards to span_add with the legacy fields.
    trace_add = function(event, tokens_sent = NA_integer_,
                         tokens_received = NA_integer_, tool = NA_character_,
                         duration = NA_real_, note = NA_character_) {
      private$span_add(event, tokens_sent = tokens_sent,
                       tokens_received = tokens_received, tool = tool,
                       duration = duration, note = note)
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
#' `LLMR::llm_tool()`), and refuses further calls when its [budget()] runs out. Use
#' `agent$chat()` for a stateful conversation, `agent$ask_structured()` for
#' schema-shaped answers, and pass agents to [conversation()], [debate()],
#' [focus_group()], [interview()], or [deliberate()] for multi-agent work.
#'
#' Two design decisions worth knowing:
#'
#' - **Failures are errors, not replies.** If a call fails, the typed LLMR
#'   condition propagates; nothing is written into memory, so an API hiccup
#'   is never stored as something the model said.
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
#' @param guardrails Optional [guardrails()] to check the agent's inputs,
#'   outputs, and tool calls. A blocked check raises `llmragent_guardrail_block`
#'   and is recorded as an event; default `NULL` means no guardrails.
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
                  guardrails = NULL, quiet = FALSE) {
  Agent$new(name = name, config = config, persona = persona, tools = tools,
            memory = memory, budget = budget, guardrails = guardrails,
            quiet = quiet)
}
