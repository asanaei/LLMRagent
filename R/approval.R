# approval.R ------------------------------------------------------------------
# Human approval gates for high-risk tools. LLMR's tool loop runs all tools to
# completion with no pause point, so when an agent has any approval-gated tool
# the agent runs a thin, controllable tool loop built on LLMR's EXPORTED
# primitives (tool_calls() + call_llm_robust + the documented message-append
# shape). When the model calls a gated tool, the loop suspends by raising a
# resumable condition carrying a serializable checkpoint; resume_run() rebuilds
# the agent, splices the approved (or denied/edited) result, and continues.
# This is the R-native equivalent of PydanticAI-style deferred tools.

#' Run a native, pausable tool loop (used when a tool requires approval)
#'
#' Built on `LLMR::tool_calls()` and `LLMR::call_llm_robust()` so it inherits
#' provider handling. Mirrors `LLMR::call_llm_tools()`'s accounting: it returns
#' the final `llmr_response` with `tool_history` and `tool_loop` attributes. When
#' the model calls a tool whose governance marks `requires_approval = TRUE`, the
#' loop raises `llmragent_pending_approval` carrying a checkpoint.
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
    stop("Approval-gated tools support OpenAI-compatible and Anthropic ",
         "providers; the pausable tool loop does not yet drive the Gemini ",
         "tool protocol.", call. = FALSE)
  }
  tool_index <- stats::setNames(tools, vapply(tools, `[[`, "", "name"))
  # The agent's cumulative tool-call ceiling, enforced inside this loop too
  # (LLMR's loop enforces it via max_tool_calls; this loop must not be a
  # bypass). Prior exchanges' spend is the agent's counter; this exchange's is
  # agg$tool_calls, which resume_run() carries across a pause.
  budget_max <- if (inherits(agent, "Agent")) agent$budget$max_tool_calls else Inf
  prior_tool_calls <- if (inherits(agent, "Agent")) agent$usage()$tool_calls else 0L
  # provider-shaped tool defs on the config
  cfg <- config
  mp <- cfg$model_params %||% list()
  mp$tools <- if (is_anthropic) .native_tools_anthropic(tools) else .native_tools_openai(tools)
  cfg$model_params <- mp

  convo <- convo %||% .native_normalize(messages)
  history <- history %||% list()
  agg <- agg %||% list(model_calls = 0L, sent = 0L, rec = 0L, tool_calls = 0L, saw = FALSE)

  for (round in seq_len(max_rounds)) {
    resp <- LLMR::call_llm_robust(cfg, convo, tries = tries,
                                  wait_seconds = wait_seconds, verbose = FALSE)
    agg$model_calls <- agg$model_calls + 1L
    u <- LLMR::tokens(resp)
    if (length(u$sent) == 1L && !is.na(u$sent)) { agg$sent <- agg$sent + as.integer(u$sent); agg$saw <- TRUE }
    if (length(u$rec) == 1L && !is.na(u$rec)) { agg$rec <- agg$rec + as.integer(u$rec); agg$saw <- TRUE }

    calls <- LLMR::tool_calls(resp)
    if (!length(calls)) {
      attr(resp, "messages") <- convo
      attr(resp, "tool_history") <- .native_history(history)
      attr(resp, "tool_loop") <- list(
        model_calls = agg$model_calls,
        sent = if (agg$saw) agg$sent else NA_integer_,
        rec = if (agg$saw) agg$rec else NA_integer_,
        tool_calls = agg$tool_calls)
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
      # Cumulative budget check BEFORE executing (or suspending on) the next
      # call, mirroring assert_budget()'s fail-closed order. The condition also
      # carries llmr_tool_limit so call_model()'s handler accounts the spend.
      if (prior_tool_calls + agg$tool_calls >= budget_max) {
        rlang::abort(
          message = sprintf(
            "Agent '%s' budget exceeded: max_tool_calls (%d tool calls made).",
            if (inherits(agent, "Agent")) agent$name else "?",
            prior_tool_calls + agg$tool_calls),
          class = c("llmragent_budget_error", "llmr_tool_limit",
                    "error", "condition"))
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
      result <- .native_exec(tool, cl)
      agg$tool_calls <- agg$tool_calls + 1L
      history[[length(history) + 1L]] <- list(round = round, name = cl$name,
                                              arguments = cl$arguments, result = result)
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
    rec = if (agg$saw) agg$rec else NA_integer_, tool_calls = agg$tool_calls)
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
      as.character(jsonlite::toJSON(h$arguments, auto_unbox = TRUE, null = "null")), character(1)),
    result = vapply(history, function(h) as.character(h$result), character(1)))
}

# ---- checkpoints ------------------------------------------------------------

.make_checkpoint <- function(agent, config, tools, convo, history, agg, round,
                             pending, is_anthropic, max_rounds, tries,
                             wait_seconds, result_blocks, remaining) {
  agent_state <- if (inherits(agent, "Agent")) {
    list(name = agent$name, persona = agent$persona, config = agent$config,
         memory = agent$memory$state(), agent_id = agent$id(),
         usage = as.list(agent$usage()[1, c("calls","tokens_sent","tokens_received","tool_calls")]),
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
                   arguments_hash = LLMR::llm_hash(pending$arguments)),
    decision = NULL
  ), class = "llmragent_checkpoint")
}

#' Approve, reject, or edit a pending tool call
#'
#' When a run pauses at an approval-gated tool, it surfaces a checkpoint (the
#' `checkpoint` field of the `llmragent_pending_approval` condition, or the
#' object returned by `human_gate()` in batch mode). Inspect
#' `checkpoint$pending` (the tool name, arguments, and an argument hash), then
#' record a decision with this function and continue with [resume_run()].
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
  checkpoint$decision <- list(decision = decision, edit = edit)
  checkpoint
}

#' Mark a point or a tool as requiring human approval
#'
#' `human_gate()` is a marker used in two places. As a wrapper, `human_gate(tool)`
#' returns the tool with its approval requirement set, equivalent to building it
#' with `agent_tool(..., requires_approval = TRUE)`. As a workflow node (Stage 4),
#' it pauses a run for sign-off. In an agent's tool list, any approval-required
#' tool makes the agent run a pausable tool loop.
#'
#' @param x A tool (an `llmr_tool` / [agent_tool()]) to gate, or a name (label)
#'   when used as a standalone gate marker.
#' @param prompt Optional text shown to the human reviewer.
#' @return The gated tool (when `x` is a tool), or a gate marker object.
#' @seealso [approve_tool_call()], [resume_run()], [agent_tool()]
#' @export
human_gate <- function(x, prompt = NULL) {
  if (inherits(x, "llmr_tool")) {
    gov <- attr(x, "governance") %||% list(side_effects = "external",
      requires_approval = FALSE, timeout_s = NULL, max_calls = Inf, max_bytes = Inf,
      state = local({ e <- new.env(parent = emptyenv()); e$n_calls <- 0L; e }))
    gov$requires_approval <- TRUE
    gov$approval_prompt <- prompt
    attr(x, "governance") <- gov
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
#' continuous exchange.
#'
#' @param checkpoint A `llmragent_checkpoint` with a decision recorded by
#'   [approve_tool_call()].
#' @param ... Reserved.
#' @return The final reply text (character scalar). The rebuilt agent is
#'   attached as `attr(x, "agent")`.
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

  pend <- checkpoint$pending
  dec <- checkpoint$decision
  # Resolve the pending tool's result per the decision. The gated tool lives in
  # the checkpoint's tool list (and on the rebuilt agent).
  find_in <- function(name) {
    for (t in tools) if (identical(t$name, name)) return(t)
    .find_tool(ag, name)
  }
  result <- switch(dec$decision,
    reject = sprintf("DENIED: the human reviewer rejected the call to '%s'.", pend$name),
    {  # approve or edit
      args <- if (identical(dec$decision, "edit")) dec$edit else pend$arguments
      tool <- find_in(pend$name)
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
    name = pend$name, arguments = args_used, result = result)))
  agg <- checkpoint$agg; agg$tool_calls <- agg$tool_calls + 1L

  # Work through the rest of the suspended turn's calls (remaining[[1]] is the
  # gated call just decided). An ungated one executes now; a second gated one
  # suspends again with an updated checkpoint, exactly as the loop would.
  rest <- checkpoint$remaining %||% list()
  if (length(rest) > 1L) rest <- rest[-1L] else rest <- list()
  for (i in seq_along(rest)) {
    cl2 <- rest[[i]]
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
    result2 <- .native_exec(tool2, cl2)
    agg$tool_calls <- agg$tool_calls + 1L
    history <- c(history, list(list(round = checkpoint$round, name = cl2$name,
                                    arguments = cl2$arguments, result = result2)))
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
  out <- as.character(resp)
  attr(out, "agent") <- ag
  attr(out, "checkpoint") <- checkpoint
  out
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
  cat("  arguments:", as.character(jsonlite::toJSON(x$pending$arguments, auto_unbox = TRUE)), "\n")
  cat("  decision: ", if (is.null(x$decision)) "none (call approve_tool_call())" else x$decision$decision, "\n")
  invisible(x)
}
