# guardrails.R ----------------------------------------------------------------
# Logged checks at three boundaries: an agent's inputs, its outputs, and its
# tool calls. A guardrail is a named predicate; a failure becomes BOTH a typed
# condition (loud to the caller) and a durable event (in the run's event graph),
# never a silent drop. Guardrails attach to an agent and (later) to workflow
# nodes; they are kept node-agnostic so the same objects serve both.

#' Define a guardrail
#'
#' A guardrail is a named check run at one boundary of an agent: its input (the
#' user text), its output (the model's reply), or a tool call (a tool's name and
#' arguments before it runs, or its result after). The check returns `TRUE` to
#' pass, or a short reason string to fail. On failure the guardrail either
#' blocks (raises a typed condition and records the event), warns, or merely
#' flags; in every case the decision is recorded as an event, so a blocked input
#' is analyzable rather than invisible.
#'
#' @param name A short label for the guardrail (appears in events).
#' @param check A function `(payload, context) -> TRUE | reason`. `payload` is
#'   the text (input/output) or a `list(name=, arguments=, result=)` (tool).
#'   `context` is a small list with `stage`, `agent`, and `phase` (`"pre"` or
#'   `"post"` for tools). Return `TRUE` to pass, or a non-empty character reason
#'   to fail.
#' @param on_fail What to do on failure: `"block"` (raise
#'   `llmragent_guardrail_block` and stop), `"warn"` (warn and continue), or
#'   `"flag"` (record only). Default `"block"`.
#' @param stage Which boundary: `"input"`, `"output"`, or `"tool"`.
#' @return An object of class `agent_guardrail`.
#' @seealso [guardrails()], [agent()]
#' @examples
#' no_pii <- guardrail(
#'   "no_ssn",
#'   function(payload, context) {
#'     if (grepl("\\b\\d{3}-\\d{2}-\\d{4}\\b", payload)) "contains an SSN" else TRUE
#'   },
#'   stage = "input"
#' )
#' @export
guardrail <- function(name, check,
                      on_fail = c("block", "warn", "flag"),
                      stage = c("input", "output", "tool")) {
  stopifnot(is.character(name), length(name) == 1L, nzchar(name),
            is.function(check))
  on_fail <- match.arg(on_fail)
  stage <- match.arg(stage)
  structure(list(name = name, check = check, on_fail = on_fail, stage = stage),
            class = "agent_guardrail")
}

#' Collect guardrails
#'
#' Bundle one or more [guardrail()] objects to attach to an [agent()] via
#' `agent(..., guardrails = guardrails(...))`.
#'
#' @param ... [guardrail()] objects.
#' @return An object of class `agent_guardrails` (a list).
#' @seealso [guardrail()]
#' @export
guardrails <- function(...) {
  gs <- list(...)
  if (length(gs) == 1L && is.list(gs[[1]]) && !inherits(gs[[1]], "agent_guardrail")) {
    gs <- gs[[1]]
  }
  ok <- vapply(gs, inherits, logical(1), what = "agent_guardrail")
  if (length(gs) && !all(ok)) {
    stop("All arguments to guardrails() must be guardrail() objects.", call. = FALSE)
  }
  structure(gs, class = "agent_guardrails")
}

#' @export
print.agent_guardrail <- function(x, ...) {
  cat(sprintf("<agent_guardrail %s | stage=%s | on_fail=%s>\n",
              x$name, x$stage, x$on_fail))
  invisible(x)
}

#' @export
print.agent_guardrails <- function(x, ...) {
  cat(sprintf("<agent_guardrails | %d check(s)>\n", length(x)))
  for (g in x) cat(sprintf("  - %s (%s, %s)\n", g$name, g$stage, g$on_fail))
  invisible(x)
}

# A typed condition for a blocked payload (catchable like the budget error).
#' @keywords internal
#' @noRd
.guardrail_block <- function(name, stage, reason) {
  rlang::abort(
    message = sprintf("Guardrail '%s' blocked the %s: %s", name, stage, reason),
    class = c("llmragent_guardrail_block", "error", "condition"),
    guardrail = name, stage = stage, reason = reason)
}

# Run the guardrails for one stage over a payload. Returns a list of verdicts
# list(name=, status=, reason=) for event recording; raises on a blocking
# failure. `record` is a function(name, status, reason) the Agent installs to
# emit a guardrail span. Pure otherwise.
#' @keywords internal
#' @noRd
.run_guardrails <- function(gs, stage, payload, context = list(), record = NULL) {
  if (is.null(gs) || !length(gs)) return(invisible(list()))
  context$stage <- stage
  out <- list()
  for (g in gs) {
    if (!identical(g$stage, stage)) next
    verdict <- tryCatch(g$check(payload, context),
                        error = function(e) paste0("guardrail error: ", conditionMessage(e)))
    passed <- isTRUE(verdict)
    reason <- if (passed) NA_character_ else as.character(verdict)[1]
    status <- if (passed) "ok" else switch(g$on_fail,
      block = "blocked", warn = "warned", flag = "flagged")
    if (is.function(record)) record(g$name, status, reason)
    out[[length(out) + 1L]] <- list(name = g$name, status = status, reason = reason)
    if (!passed) {
      if (identical(g$on_fail, "block")) .guardrail_block(g$name, stage, reason)
      if (identical(g$on_fail, "warn")) {
        warning(sprintf("Guardrail '%s' (%s): %s", g$name, stage, reason),
                call. = FALSE)
      }
    }
  }
  invisible(out)
}
