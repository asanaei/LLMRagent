# approval.R ------------------------------------------------------------------
# Human approval gates for high-risk tools. LLMR's tool loop runs all tools to
# completion with no pause point, so when an agent has any approval-gated tool
# the agent runs a thin, controllable tool loop built on LLMR's EXPORTED
# primitives (tool_calls() + call_llm_robust + the documented message-append
# shape). When the model calls a gated tool, the loop suspends by raising a
# resumable condition carrying a serializable checkpoint; resume_run() rebuilds
# the agent, splices the approved (or denied/edited) result, and continues.
# The checkpoint carries the loop's accounting, so the resumed exchange is
# charged once.

#' Run a native, pausable tool loop
#'
#' Built on `LLMR::tool_calls()` and `LLMR::call_llm_robust()` so it inherits
#' provider handling. Mirrors `LLMR::call_llm_tools()`'s accounting: it returns
#' the final `llmr_response` with `tool_history` and `tool_loop` attributes. When
#' the model calls a tool whose governance marks `requires_approval = TRUE`, the
#' loop raises `llmragent_pending_approval` carrying a checkpoint. The same
#' pause points let agents enforce tool guardrails before execution and budgets
#' before each model round. Like `LLMR::call_llm_tools()`, it never runs a call
#' whose arguments are not a JSON object, nor any call in a reply cut off by
#' the output-token limit: the model receives an error result instead, and the
#' call neither pauses for approval nor counts toward the tool-call budget.
#'
#' @keywords internal
#' @noRd
.native_tool_loop <- function(config, messages, tools, agent = NULL,
                              max_rounds = 8L, tries = 3L, wait_seconds = 2,
                              convo = NULL, history = NULL,
                              agg = NULL) {
  is_anthropic <- identical(config$provider, "anthropic")
  # This loop speaks exactly two tool protocols: Anthropic's and the
  # OpenAI-compatible one. Treating any other provider as OpenAI-shaped would
  # corrupt the conversation (e.g. Gemini), so refuse clearly, matching
  # LLMR::call_llm_tools()'s own refusal.
  if (!is_anthropic && identical(config$provider, "gemini")) {
    stop("Native tool loops support OpenAI-compatible and Anthropic ",
         "providers; this loop does not yet drive the Gemini ",
         "tool protocol.", call. = FALSE)
  }
  tool_index <- stats::setNames(tools, vapply(tools, `[[`, "", "name"))
  # The agent's cumulative tool-call ceiling is enforced inside this loop too.
  budget_max <- if (inherits(agent, "Agent")) agent$budget$max_tool_calls else Inf
  # provider-shaped tool defs on the config
  cfg <- config
  mp <- cfg$model_params %||% list()
  mp$tools <- if (is_anthropic) .native_tools_anthropic(tools) else .native_tools_openai(tools)
  cfg$model_params <- mp

  convo <- convo %||% .native_normalize(messages)
  history <- history %||% list()
  agg <- agg %||% list(
    model_calls = 0L, sent = 0L, rec = 0L, tool_calls = 0L, saw = FALSE,
    accounted_model_calls = 0L, accounted_sent = 0L, accounted_rec = 0L,
    accounted_tool_calls = 0L, n_unknown_usage = 0L, accounted_unknown_usage = 0L)
  if (is.null(agg$n_unknown_usage)) agg$n_unknown_usage <- .loop_unknown_usage(agg)
  if (inherits(agent, "Agent")) {
    agg <- agent$.__enclos_env__$private$account_pending_loop(agg)
  }

  for (round in seq_len(max_rounds)) {
    if (inherits(agent, "Agent")) {
      agent$.__enclos_env__$private$assert_budget()
    }
    resp <- LLMR::call_llm_robust(cfg, convo, tries = tries,
                                  wait_seconds = wait_seconds, verbose = FALSE)
    agg$model_calls <- agg$model_calls + 1L
    u <- LLMR::tokens(resp)
    agg$n_unknown_usage <- agg$n_unknown_usage +
      as.integer(is.na(.usage_count(u$sent)) || is.na(.usage_count(u$rec)))
    if (length(u$sent) == 1L && !is.na(u$sent)) { agg$sent <- agg$sent + as.integer(u$sent); agg$saw <- TRUE }
    if (length(u$rec) == 1L && !is.na(u$rec)) { agg$rec <- agg$rec + as.integer(u$rec); agg$saw <- TRUE }
    if (inherits(agent, "Agent")) {
      agg <- agent$.__enclos_env__$private$account_pending_loop(agg)
    }

    calls <- LLMR::tool_calls(resp)
    truncated <- identical(resp$finish_reason, "length")
    if (!length(calls)) {
      attr(resp, "messages") <- convo
      attr(resp, "tool_history") <- .native_history(history)
      attr(resp, "tool_loop") <- list(
        model_calls = agg$model_calls,
        sent = if (agg$saw) agg$sent else NA_integer_,
        rec = if (agg$saw) agg$rec else NA_integer_,
        tool_calls = agg$tool_calls,
        accounted_model_calls = agg$accounted_model_calls %||% 0L,
        accounted_sent = agg$accounted_sent %||% 0L,
        accounted_rec = agg$accounted_rec %||% 0L,
        accounted_tool_calls = agg$accounted_tool_calls %||% 0L,
        n_unknown_usage = agg$n_unknown_usage,
        accounted_unknown_usage = agg$accounted_unknown_usage %||% 0L)
      return(resp)
    }

    # append the assistant tool-call turn (provider-shaped)
    if (is_anthropic) {
      convo <- append(convo, list(list(role = "assistant", content = resp$raw$content)))
    } else {
      amsg <- resp$raw$choices[[1]]$message; amsg$content <- amsg$content %||% ""
      convo <- append(convo, list(amsg))
    }

    result_blocks <- list()
    for (cl in calls) {
      refusal <- .native_tool_refusal(cl, truncated)
      if (!is.null(refusal)) {
        history[[length(history) + 1L]] <- .native_history_entry(round, cl, refusal)
        result_blocks <- .native_append_result(result_blocks, convo, cl, refusal,
                                                is_anthropic)
        if (!is_anthropic) convo <- attr(result_blocks, "convo")
        next
      }
      tools_used <- if (inherits(agent, "Agent")) {
        agent$usage()$tool_calls
      } else {
        agg$tool_calls
      }
      if (tools_used >= budget_max) {
        if (inherits(agent, "Agent")) {
          agent$.__enclos_env__$private$budget_fail(
            "max_tool_calls", sprintf("%d tool calls made", tools_used))
        }
        stop("Tool-call budget exceeded.", call. = FALSE)
      }
      tool <- tool_index[[cl$name]]
      gov <- if (is.null(tool)) NULL else .tool_governance(tool)
      if (!is.null(gov) && isTRUE(gov$requires_approval)) {
        # SUSPEND: build a checkpoint the caller can resume.
        cp <- .make_checkpoint(agent = agent, config = config, tools = tools,
                               convo = convo, history = history, agg = agg,
                               round = round, pending = cl, is_anthropic = is_anthropic,
                               max_rounds = max_rounds, tries = tries,
                               wait_seconds = wait_seconds,
                               result_blocks = result_blocks,
                               remaining = calls[match(cl$id, vapply(calls, `[[`, "", "id")):length(calls)])
        rlang::abort(
          message = sprintf("Tool '%s' requires human approval; run paused. Use approve_tool_call() then resume_run().", cl$name),
          class = c("llmragent_pending_approval", "condition"),
          checkpoint = cp)
      }
      # ungated tool inside the gated loop: execute now
      if (inherits(agent, "Agent")) {
        agent$.__enclos_env__$private$run_tool_guardrails(
          cl$name, cl$arguments, phase = "pre")
      }
      result <- .native_exec(tool, cl)
      agg$tool_calls <- agg$tool_calls + 1L
      if (inherits(agent, "Agent")) {
        agg <- agent$.__enclos_env__$private$account_pending_loop(agg)
      }
      history[[length(history) + 1L]] <- .native_history_entry(round, cl, result)
      result_blocks <- .native_append_result(result_blocks, convo, cl, result,
                                              is_anthropic)
      if (!is_anthropic) convo <- attr(result_blocks, "convo")
    }
    if (is_anthropic && length(result_blocks)) {
      convo <- append(convo, list(list(role = "user", content = result_blocks)))
    }
  }

  warning("Native tool loop reached max_rounds with tool calls pending.", call. = FALSE)
  attr(resp, "messages") <- convo
  attr(resp, "tool_history") <- .native_history(history)
  attr(resp, "tool_loop") <- list(model_calls = agg$model_calls,
    sent = if (agg$saw) agg$sent else NA_integer_,
    rec = if (agg$saw) agg$rec else NA_integer_, tool_calls = agg$tool_calls,
    accounted_model_calls = agg$accounted_model_calls %||% 0L,
    accounted_sent = agg$accounted_sent %||% 0L,
    accounted_rec = agg$accounted_rec %||% 0L,
    accounted_tool_calls = agg$accounted_tool_calls %||% 0L,
        n_unknown_usage = agg$n_unknown_usage,
        accounted_unknown_usage = agg$accounted_unknown_usage %||% 0L)
  resp
}

# ---- provider plumbing (reusing LLMR's exported tool object shape) -----------

.native_tools_openai <- function(tools) {
  lapply(tools, function(t) list(type = "function",
    "function" = list(name = t$name, description = t$description, parameters = t$schema)))
}
.native_tools_anthropic <- function(tools) {
  lapply(tools, function(t) list(name = t$name, description = t$description,
                                 input_schema = t$schema))
}
.native_normalize <- function(messages) {
  if (is.list(messages) && length(messages) && is.list(messages[[1]]) &&
      !is.null(messages[[1]]$role)) return(messages)
  if (is.character(messages)) {
    nm <- names(messages)
    if (is.null(nm)) return(lapply(messages, function(x) list(role = "user", content = x)))
    return(unname(lapply(seq_along(messages), function(i)
      list(role = nm[i] %||% "user", content = unname(messages[i])))))
  }
  messages
}
# Tool arguments as LLMR::tool_calls() reads them (LLMR 0.8.13): absent,
# empty, or `null` text means no arguments, which tools without parameters
# rely on; any other text must parse to a JSON object. Used for calls recorded
# before tool_calls() reported `arguments_ok`, such as those in an approval
# checkpoint saved by an earlier version.
.native_parse_arguments <- function(raw) {
  none <- structure(list(), names = character(0))
  if (is.null(raw)) return(list(arguments = none, raw = NA_character_, ok = TRUE))
  if (is.list(raw)) return(list(arguments = raw, raw = NA_character_, ok = TRUE))
  if (!is.character(raw) || length(raw) != 1L || is.na(raw)) {
    return(list(arguments = none, raw = paste(format(raw), collapse = " "), ok = FALSE))
  }
  if (!nzchar(trimws(raw)) || identical(trimws(raw), "null")) {
    return(list(arguments = none, raw = raw, ok = TRUE))
  }
  args <- tryCatch(jsonlite::fromJSON(raw, simplifyVector = FALSE),
                   error = function(e) NULL)
  is_object <- is.list(args) && (!length(args) || !is.null(names(args)))
  if (!is_object) return(list(arguments = none, raw = raw, ok = FALSE))
  list(arguments = args, raw = raw, ok = TRUE)
}

# The tool result sent back instead of running a call, or NULL when the call
# may run. The wording is LLMR::call_llm_tools()'s, so a model sees the same
# message whichever loop runs its tools: a reply cut off by the output-token
# limit may hold a partial call, and arguments that did not parse would leave
# the tool to its defaults.
.native_truncation_refusal <- paste0(
  "ERROR: the reply was cut off by the output-token limit while ",
  "this tool call was being written, so the tool was not run. ",
  "Call it again with complete arguments.")

.native_tool_refusal <- function(call, truncated = FALSE) {
  if (isTRUE(truncated)) return(.native_truncation_refusal)
  if (!isTRUE(call$arguments_ok %||% TRUE)) {
    return(paste0("ERROR: the arguments for this tool call are not valid JSON, ",
                  "so the tool was not run. Received: ",
                  substr(call$arguments_raw %||% "", 1L, 500L)))
  }
  NULL
}

# Whether a tool-history row records a call that was refused rather than run.
# Both loops keep the raw text of unparseable arguments and answer a truncated
# reply with a fixed message, so the row itself says which it was.
.tool_row_refused <- function(arguments, result) {
  unparsed <- is.character(arguments) && length(arguments) == 1L &&
    !is.na(arguments) && !.native_parse_arguments(arguments)$ok
  unparsed || identical(result, .native_truncation_refusal)
}

# A call recorded before tool_calls() reported `arguments_ok` (an older
# checkpoint) gets the flag from the argument text in the assistant turn that
# requested it. Provider-parsed arguments (Anthropic) are always usable.
.native_call_flags <- function(cl, convo) {
  if (!is.null(cl$arguments_ok)) return(cl)
  raw <- NULL
  for (m in rev(convo %||% list())) {
    tcs <- if (is.list(m)) m$tool_calls else NULL
    if (!is.list(tcs)) next
    for (tc in tcs) if (identical(tc$id, cl$id)) raw <- tc$`function`$arguments %||% ""
    if (!is.null(raw)) break
  }
  parsed <- .native_parse_arguments(raw)
  cl$arguments_raw <- parsed$raw
  cl$arguments_ok <- parsed$ok
  cl
}

.native_history_entry <- function(round, cl, result) {
  list(round = round, name = cl$name, arguments = cl$arguments,
       arguments_raw = if (isTRUE(cl$arguments_ok %||% TRUE)) NULL else cl$arguments_raw,
       result = result)
}

.native_exec <- function(tool, cl) {
  if (is.null(tool)) return(paste0("ERROR: unknown tool '", cl$name, "'"))
  res <- tryCatch(do.call(tool$fn, cl$arguments),
                  error = function(e) paste0("ERROR: ", conditionMessage(e)))
  if (is.character(res) && length(res) == 1L) return(res)
  tryCatch(as.character(jsonlite::toJSON(res, auto_unbox = TRUE, null = "null")),
           error = function(e) paste(utils::capture.output(print(res)), collapse = "\n"))
}
.native_append_result <- function(result_blocks, convo, cl, result, is_anthropic) {
  if (is_anthropic) {
    result_blocks[[length(result_blocks) + 1L]] <- list(
      type = "tool_result", tool_use_id = cl$id, content = result)
    return(result_blocks)
  }
  convo <- append(convo, list(list(role = "tool", tool_call_id = cl$id, content = result)))
  attr(result_blocks, "convo") <- convo
  result_blocks
}
.native_history <- function(history) {
  if (!length(history)) {
    return(tibble::tibble(round = integer(0), name = character(0),
                          arguments = character(0), result = character(0)))
  }
  tibble::tibble(
    round = vapply(history, function(h) as.integer(h$round), integer(1)),
    name = vapply(history, function(h) as.character(h$name), character(1)),
    arguments = vapply(history, function(h)
      h$arguments_raw %||%
        as.character(jsonlite::toJSON(h$arguments, auto_unbox = TRUE, null = "null")),
      character(1)),
    result = vapply(history, function(h) as.character(h$result), character(1)))
}

# ---- checkpoints ------------------------------------------------------------

.make_checkpoint <- function(agent, config, tools, convo, history, agg, round,
                             pending, is_anthropic, max_rounds, tries,
                             wait_seconds, result_blocks, remaining) {
  agent_state <- if (inherits(agent, "Agent")) {
    list(name = agent$name, persona = agent$persona, config = agent$config,
         memory = agent$memory$state(), agent_id = agent$id(),
         usage = as.list(agent$usage()[1, c("calls","tokens_sent","tokens_received","tool_calls","n_unknown_usage")]),
         budget = agent$budget, spans = agent$internal_spans(),
         # Guardrails travel with the checkpoint (closures serialize through
         # RDS, like the tools); dropping them would let a resume silently
         # bypass the agent's policy.
         guardrails = agent$guardrail_set())
  } else NULL
  structure(list(
    schema = "llmragent_checkpoint/1",
    created_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    agent_state = agent_state,
    config = config,
    # The tool objects (closures serialize through RDS), so resume_run() can
    # rebuild the agent WITH its tools and actually execute the approved call.
    tools = tools,
    convo = convo, history = history, agg = agg, round = round,
    is_anthropic = is_anthropic, max_rounds = max_rounds, tries = tries,
    wait_seconds = wait_seconds,
    # The already-executed ungated results from this turn, and the still-pending
    # calls after the gated one, so a multi-tool turn resumes without dropping work.
    result_blocks = result_blocks,
    remaining = remaining,
    pending = list(id = pending$id, name = pending$name, arguments = pending$arguments,
                   arguments_raw = pending$arguments_raw %||% NA_character_,
                   arguments_ok = pending$arguments_ok %||% TRUE,
                   arguments_hash = LLMR::llm_hash(pending$arguments)),
    decision = NULL
  ), class = "llmragent_checkpoint")
}

#' Approve, reject, or edit a pending tool call
#'
#' When a run pauses at an approval-gated tool, it surfaces a checkpoint (the
#' `checkpoint` field of the `llmragent_pending_approval` condition, or the
#' object returned by `human_gate()` in batch mode). Inspect
#' `checkpoint$pending` (the tool name, the parsed arguments, the argument text
#' as the model sent it, and an argument hash), or print the checkpoint, then
#' record a decision with this function and continue with [resume_run()].
#'
#' The loop never pauses for a call whose arguments are not a JSON object; it
#' returns an error result to the model instead, as `LLMR::call_llm_tools()`
#' does. A checkpoint saved by an earlier version can still hold such a call.
#' Approving it is an error, since the tool would run without the arguments the
#' model meant to send; reject it, or supply the arguments with
#' `decision = "edit"`.
#'
#' @param checkpoint A `llmragent_checkpoint`.
#' @param decision `"approve"` (run the tool as requested), `"reject"` (skip it;
#'   the model is told the call was denied), or `"edit"` (run it with
#'   `edit`-modified arguments).
#' @param edit For `decision = "edit"`, the replacement argument list.
#' @return The checkpoint with the decision recorded.
#' @seealso [human_gate()], [resume_run()], [agent_tool()]
#' @export
approve_tool_call <- function(checkpoint, decision = c("approve", "reject", "edit"),
                              edit = NULL) {
  stopifnot(inherits(checkpoint, "llmragent_checkpoint"))
  decision <- match.arg(decision)
  if (identical(decision, "edit") && !is.list(edit)) {
    stop("decision = \"edit\" needs `edit` (a replacement argument list).", call. = FALSE)
  }
  pend <- .native_call_flags(checkpoint$pending, checkpoint$convo)
  if (identical(decision, "approve") && !isTRUE(pend$arguments_ok)) {
    stop(sprintf(paste0(
      "The arguments of the pending call to '%s' are not valid JSON, so it cannot ",
      "be approved as sent: %s. Reject it, or supply the arguments with ",
      "decision = \"edit\"."), pend$name, .native_show_raw(pend$arguments_raw)),
      call. = FALSE)
  }
  checkpoint$decision <- list(decision = decision, edit = edit)
  checkpoint
}

#' Mark a point or a tool as requiring human approval
#'
#' `human_gate()` is a marker used in two places. As a wrapper, `human_gate(tool)`
#' returns the tool with its approval requirement set, equivalent to building it
#' with `agent_tool(..., requires_approval = TRUE)`. As a workflow node, it
#' pauses a run for sign-off until [resume_workflow()] is called. In an agent's
#' tool list, any approval-required tool makes the agent run a pausable tool
#' loop.
#'
#' @param x A tool (an `llmr_tool` / [agent_tool()]) to gate, or a name (label)
#'   when used as a standalone gate marker.
#' @param prompt Optional text shown to the human reviewer.
#' @return The gated tool (when `x` is a tool), or a gate marker object of
#'   class `llmragent_human_gate`.
#' @seealso [approve_tool_call()], [resume_run()], [agent_tool()],
#'   [run_workflow()]
#' @export
human_gate <- function(x, prompt = NULL) {
  if (inherits(x, "llmr_tool")) {
    gov <- .tool_governance(x) %||% list(side_effects = "external",
      requires_approval = FALSE, timeout_s = NULL, max_calls = Inf, max_bytes = Inf,
      state = local({ e <- new.env(parent = emptyenv()); e$n_calls <- 0L; e }))
    gov$requires_approval <- TRUE
    gov$approval_prompt <- prompt
    x$governance <- gov
    attr(x, "governance") <- NULL
    class(x) <- unique(c("agent_tool", class(x)))
    return(x)
  }
  structure(list(name = as.character(x)[1], prompt = prompt),
            class = "llmragent_human_gate")
}

#' Resume a paused run after a tool-approval decision
#'
#' Rebuilds the paused agent from the checkpoint, applies the recorded decision
#' (approve / reject / edit), and continues the tool loop to completion. The
#' resumed work keeps the original accounting, so the run reads as one
#' continuous exchange: model rounds charged before the pause, including their
#' count of calls with unreported usage, are not charged again.
#'
#' @param checkpoint A `llmragent_checkpoint` with a decision recorded by
#'   [approve_tool_call()].
#' @param ... Reserved.
#' @return An object of class `agent_resume_result` with `text` (the final
#'   reply), `agent` (the rebuilt agent, or `NULL` when the checkpoint carries
#'   no agent state), and `checkpoint` (the checkpoint that was resumed).
#' @seealso [human_gate()], [approve_tool_call()]
#' @export
resume_run <- function(checkpoint, ...) {
  stopifnot(inherits(checkpoint, "llmragent_checkpoint"))
  if (is.null(checkpoint$decision)) {
    stop("No decision recorded; call approve_tool_call() first.", call. = FALSE)
  }
  # The tool objects travel in the checkpoint (closures serialize), so the
  # approved call can actually run on resume.
  tools <- checkpoint$tools %||% list()

  # Rebuild the agent WITH its tools (its config holds an env-var key reference,
  # so this is portable across sessions/machines).
  st <- checkpoint$agent_state
  ag <- NULL
  if (!is.null(st)) {
    mem <- memory_restore(st$memory)
    ag <- agent(name = st$name, config = st$config, persona = st$persona,
                tools = tools, memory = mem, budget = st$budget %||% budget(),
                guardrails = st$guardrails)
    ag$restore_accounting(usage = st$usage, spans = st$spans, agent_id = st$agent_id)
  }

  pend <- .native_call_flags(checkpoint$pending, checkpoint$convo)
  dec <- checkpoint$decision
  agg <- checkpoint$agg
  if (!is.null(ag)) {
    agg <- ag$.__enclos_env__$private$account_pending_loop(agg)
  }
  # Resolve the pending tool's result per the decision. The gated tool lives in
  # the checkpoint's tool list (and on the rebuilt agent).
  find_in <- function(name) {
    for (t in tools) if (identical(t$name, name)) return(t)
    .find_tool(ag, name)
  }
  # An approval recorded for a call whose arguments did not parse (possible
  # only in a checkpoint from an earlier version) does not run the tool: the
  # model receives the same error result the loop would have sent.
  refusal <- if (identical(dec$decision, "approve")) .native_tool_refusal(pend) else NULL
  if (!is.null(refusal)) {
    warning(sprintf(paste0(
      "The approved call to '%s' was not run: its arguments are not valid JSON. ",
      "The model received an error result."), pend$name), call. = FALSE)
  }
  result <- if (!is.null(refusal)) refusal else switch(dec$decision,
    reject = sprintf("DENIED: the human reviewer rejected the call to '%s'.", pend$name),
    {  # approve or edit
      args <- if (identical(dec$decision, "edit")) dec$edit else pend$arguments
      tool <- find_in(pend$name)
      if (!is.null(ag)) {
        ag$.__enclos_env__$private$run_tool_guardrails(
          pend$name, args, phase = "pre")
      }
      if (is.null(tool)) {
        sprintf("ERROR: tool '%s' is not available on resume; re-attach it to the agent.", pend$name)
      } else .native_exec(tool, list(name = pend$name, arguments = args, id = pend$id))
    })

  # Splice the decided result and continue the native loop from the checkpoint.
  # The checkpoint's result_blocks (the ungated calls already executed in this
  # turn) and remaining (the turn's calls after the gated one) must both be
  # restored: the assistant turn already names every call id, so a resume that
  # answered only the gated one would send a conversation the provider rejects.
  is_anthropic <- isTRUE(checkpoint$is_anthropic)
  cl <- list(id = pend$id, name = pend$name, arguments = pend$arguments)
  result_blocks <- checkpoint$result_blocks %||% list()
  convo <- checkpoint$convo
  # An edited call records the arguments that actually ran, not the originals.
  args_used <- if (identical(dec$decision, "edit")) dec$edit else pend$arguments
  rb <- .native_append_result(result_blocks, convo, cl, result, is_anthropic)
  if (!is_anthropic) convo <- attr(rb, "convo") else result_blocks <- rb
  history <- c(checkpoint$history, list(list(round = checkpoint$round,
    name = pend$name, arguments = args_used,
    arguments_raw = if (is.null(refusal)) NULL else pend$arguments_raw,
    result = result)))
  if (is.null(refusal)) agg$tool_calls <- agg$tool_calls + 1L
  if (!is.null(ag)) {
    agg <- ag$.__enclos_env__$private$account_pending_loop(agg)
  }

  # Work through the rest of the suspended turn's calls (remaining[[1]] is the
  # gated call just decided). An ungated one executes now; a second gated one
  # suspends again with an updated checkpoint, exactly as the loop would.
  rest <- checkpoint$remaining %||% list()
  if (length(rest) > 1L) rest <- rest[-1L] else rest <- list()
  for (i in seq_along(rest)) {
    cl2 <- .native_call_flags(rest[[i]], checkpoint$convo)
    refusal2 <- .native_tool_refusal(cl2)
    if (!is.null(refusal2)) {
      history <- c(history, list(.native_history_entry(checkpoint$round, cl2, refusal2)))
      rb <- .native_append_result(result_blocks, convo, cl2, refusal2, is_anthropic)
      if (!is_anthropic) convo <- attr(rb, "convo") else result_blocks <- rb
      next
    }
    tool2 <- find_in(cl2$name)
    gov2 <- if (is.null(tool2)) NULL else .tool_governance(tool2)
    if (!is.null(gov2) && isTRUE(gov2$requires_approval)) {
      cp2 <- .make_checkpoint(agent = ag, config = checkpoint$config,
                              tools = tools, convo = convo, history = history,
                              agg = agg, round = checkpoint$round,
                              pending = cl2, is_anthropic = is_anthropic,
                              max_rounds = checkpoint$max_rounds,
                              tries = checkpoint$tries,
                              wait_seconds = checkpoint$wait_seconds,
                              result_blocks = result_blocks,
                              remaining = rest[i:length(rest)])
      rlang::abort(
        message = sprintf("Tool '%s' requires human approval; run paused. Use approve_tool_call() then resume_run().", cl2$name),
        class = c("llmragent_pending_approval", "condition"),
        checkpoint = cp2)
    }
    if (!is.null(ag)) {
      ag$.__enclos_env__$private$run_tool_guardrails(
        cl2$name, cl2$arguments, phase = "pre")
    }
    result2 <- .native_exec(tool2, cl2)
    agg$tool_calls <- agg$tool_calls + 1L
    if (!is.null(ag)) {
      agg <- ag$.__enclos_env__$private$account_pending_loop(agg)
    }
    history <- c(history, list(.native_history_entry(checkpoint$round, cl2, result2)))
    rb <- .native_append_result(result_blocks, convo, cl2, result2, is_anthropic)
    if (!is_anthropic) convo <- attr(rb, "convo") else result_blocks <- rb
  }
  if (is_anthropic && length(result_blocks)) {
    convo <- append(convo, list(list(role = "user", content = result_blocks)))
  }

  # Even without a rebuilt agent (no agent_state), the serialized tools must
  # drive the continuation, or every later tool call would be "unknown tool".
  tools_for_loop <- if (!is.null(ag)) ag$tools else tools
  resp <- .native_tool_loop(
    config = checkpoint$config, messages = NULL, tools = tools_for_loop,
    agent = ag, max_rounds = checkpoint$max_rounds, tries = checkpoint$tries,
    wait_seconds = checkpoint$wait_seconds, convo = convo, history = history, agg = agg)

  if (!is.null(ag)) {
    # account the resumed exchange on the rebuilt agent so usage/spans continue
    ag$.__enclos_env__$private$account(resp, resp$duration_s %||% 0)
  }
  structure(
    list(text = as.character(resp), agent = ag, checkpoint = checkpoint),
    class = "agent_resume_result")
}

#' @rdname resume_run
#' @param x An `agent_resume_result` object.
#' @export
as.character.agent_resume_result <- function(x, ...) x$text

#' @rdname resume_run
#' @export
print.agent_resume_result <- function(x, ...) {
  cat(x$text, "\n", sep = "")
  invisible(x)
}

# Argument text for a reviewer, quoted so that empty or blank text is visible.
.native_show_raw <- function(raw) {
  if (is.null(raw) || !length(raw) || is.na(raw[1])) return("(none)")
  txt <- as.character(raw[1])
  if (nchar(txt) > 500L) txt <- paste0(substr(txt, 1L, 500L), "...")
  encodeString(txt, quote = "\"")
}

#' @keywords internal
#' @noRd
.find_tool <- function(agent, name) {
  if (is.null(agent)) return(NULL)
  for (t in agent$tools) if (identical(t$name, name)) return(t)
  NULL
}

#' @export
print.llmragent_checkpoint <- function(x, ...) {
  cat(sprintf("<llmragent_checkpoint | pending tool: %s | %s>\n",
              x$pending$name, x$created_at))
  pend <- .native_call_flags(x$pending, x$convo)
  if (!isTRUE(pend$arguments_ok)) {
    cat("  arguments (not valid JSON; approving would not run the tool):",
        .native_show_raw(pend$arguments_raw), "\n")
  } else if (is.character(pend$arguments_raw) && !is.na(pend$arguments_raw)) {
    cat("  arguments as sent:", .native_show_raw(pend$arguments_raw), "\n")
  } else {
    cat("  arguments:", as.character(jsonlite::toJSON(pend$arguments, auto_unbox = TRUE)), "\n")
  }
  cat("  decision: ", if (is.null(x$decision)) "none (call approve_tool_call())" else x$decision$decision, "\n")
  invisible(x)
}
