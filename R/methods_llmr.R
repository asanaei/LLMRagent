# methods_llmr.R --------------------------------------------------------------
# LLMRagent's run objects join the wider LLMR ecosystem through three shared
# generics that LLMR owns: diagnostics() (the machine-readable health numbers),
# report() (the methods-section prose), and reset() (clear an apparatus's
# state). LLMR defines the generics and an erroring default; here we register
# the LLMRagent methods. The token and outcome figures are not recomputed by
# hand: they are read back through LLMR::llm_usage() over the run's call-level
# rows, so a run's diagnostics agree to the integer with everything else the
# ecosystem reports for the same calls.

#' LLMR-family methods for LLMRagent run objects
#'
#' LLMRagent registers methods on three generics LLMR defines:
#' [LLMR::diagnostics()] (machine-readable health numbers), [LLMR::report()]
#' (a methods-section draft), and [LLMR::reset()] (clear an apparatus's state).
#' The token and outcome counts come from [LLMR::llm_usage()] over the run's
#' call-level rows, so they agree with the rest of the ecosystem.
#'
#' @name llmragent-methods
#' @importFrom LLMR diagnostics report reset
NULL

# Re-export the three generics so they are callable with only LLMRagent
# attached (e.g. `diagnostics(run)` in user code and examples), rather than
# requiring `LLMR::diagnostics()`.

#' @importFrom LLMR diagnostics
#' @export
LLMR::diagnostics

#' @importFrom LLMR report
#' @export
LLMR::report

#' @importFrom LLMR reset
#' @export
LLMR::reset

# ---- diagnostics ------------------------------------------------------------

#' Machine-readable diagnostics for an agent run
#'
#' Returns the small set of health numbers behind an [agent_run]: call counts,
#' token totals, tool and blocked-event counts, wall-clock duration, and the
#' study's `manifest_hash`. Token and outcome figures are read through
#' [LLMR::llm_usage()] over the run's call-level rows, so they match the rest of
#' the LLMR ecosystem to the integer.
#'
#' @param x An object accepted by [as_agent_run()] (an [Agent], a conversation
#'   or preset result, a pipeline, an experiment, or an `agent_run`).
#' @param ... Unused.
#' @return A one-row tibble with `run_id`, `kind`, `n_calls`, `n_ok`,
#'   `n_failed`, `ok_rate`, `n_truncated`, `n_filtered`, `tokens_sent`,
#'   `tokens_received`, `tokens_total`, `reasoning_tokens`, `n_tool_calls`,
#'   `n_blocked`, `duration_s`, and `manifest_hash`.
#' @seealso [report()], [LLMR::llm_usage()], [agent_manifest()]
#' @examples
#' \dontrun{
#' a <- agent("Aria", LLMR::llm_config("groq", "openai/gpt-oss-20b"))
#' a$chat("Hello")
#' diagnostics(a)
#' }
#' @exportS3Method LLMR::diagnostics agent_run
diagnostics.agent_run <- function(x, ...) {
  run <- as_agent_run(x)

  calls <- tryCatch(
    tibble::as_tibble(as_tibble(run, level = "call")),
    error = function(e) NULL)
  u <- tryCatch(
    if (!is.null(calls) && nrow(calls)) LLMR::llm_usage(calls) else .usage_zero(),
    error = function(e) .usage_zero())

  n_tool <- tryCatch(nrow(as_tibble(run, level = "tool")), error = function(e) 0L)

  events <- tryCatch(as_tibble(run, level = "event"), error = function(e) NULL)
  n_blocked <- if (!is.null(events) && "status" %in% names(events)) {
    sum(events$status %in% c("blocked", "stopped"))
  } else 0L

  mh <- tryCatch(agent_manifest(run)$manifest_hash, error = function(e) NA_character_)

  tibble::tibble(
    run_id           = run$run_id,
    kind             = run$kind,
    n_calls          = u$n,
    n_ok             = u$n_ok,
    n_failed         = u$n_failed,
    ok_rate          = u$ok_rate,
    n_truncated      = u$n_truncated,
    n_filtered       = u$n_filtered,
    tokens_sent      = u$sent_tokens,
    tokens_received  = u$rec_tokens,
    tokens_total     = u$total_tokens,
    reasoning_tokens = u$reasoning_tokens,
    n_tool_calls     = as.integer(n_tool),
    n_blocked        = as.integer(n_blocked),
    # The call level names wall-clock `duration_s`; LLMR::llm_usage() reads a
    # `duration` column, so sum our column directly rather than taking u$duration_s.
    duration_s       = if (!is.null(calls) && "duration_s" %in% names(calls))
                         sum(calls$duration_s, na.rm = TRUE) else NA_real_,
    manifest_hash    = mh
  )
}

# A bare Agent dispatches here (its class is c("Agent", "R6"), not agent_run);
# the run view is materialized by the same as_agent_run() the agent_run method
# opens with, so the two agree row for row.
#' @rdname diagnostics.agent_run
#' @exportS3Method LLMR::diagnostics Agent
diagnostics.Agent <- function(x, ...) {
  diagnostics.agent_run(as_agent_run(x), ...)
}

# A zero-call usage row, shaped like LLMR::llm_usage() output, for empty runs.
#' @keywords internal
#' @noRd
.usage_zero <- function() {
  tibble::tibble(
    n = 0L, n_ok = 0L, n_failed = 0L, ok_rate = NA_real_,
    n_truncated = 0L, n_filtered = 0L,
    sent_tokens = 0L, rec_tokens = 0L, total_tokens = 0L,
    reasoning_tokens = 0L, cached_tokens = 0L, n_unknown_tokens = 0L,
    duration_s = 0)
}

#' Machine-readable diagnostics for an agent experiment
#'
#' Returns the cell-level health numbers behind an [agent_experiment()] result:
#' how many cells ran, how many failed, the failure rate, and the total
#' wall-clock seconds. When the frame carries a `rep` column, the number of
#' distinct conditions and the replication count are reported too. Robust to a
#' frame missing the `error`, `duration`, or `rep` columns.
#'
#' @param x An [agent_experiment()] result (a tibble of class `agent_experiment`).
#' @param ... Unused.
#' @return A one-row tibble with `n_cells`, `n_failed`, `n_ok`,
#'   `failure_rate`, `total_duration_s`, and (when a `rep` column is present)
#'   `n_conditions` and `reps`.
#' @seealso [agent_experiment()], [report()]
#' @exportS3Method LLMR::diagnostics agent_experiment
diagnostics.agent_experiment <- function(x, ...) {
  n_cells <- nrow(x)
  err <- if ("error" %in% names(x)) x$error else rep(NA_character_, n_cells)
  n_failed <- sum(!is.na(err))
  n_ok <- n_cells - n_failed
  dur <- if ("duration" %in% names(x)) x$duration else rep(NA_real_, n_cells)

  out <- tibble::tibble(
    n_cells          = as.integer(n_cells),
    n_failed         = as.integer(n_failed),
    n_ok             = as.integer(n_ok),
    failure_rate     = if (n_cells) n_failed / n_cells else NA_real_,
    total_duration_s = sum(dur, na.rm = TRUE)
  )
  if ("rep" %in% names(x)) {
    reps <- length(unique(stats::na.omit(x$rep)))
    out$n_conditions <- if (reps > 0L) as.integer(n_cells / reps) else NA_integer_
    out$reps <- as.integer(reps)
  }
  out
}

# ---- report -----------------------------------------------------------------

#' Draft a methods-section report for an agent run
#'
#' Composes a short, citable account of a run: a design header (kind, run id,
#' the participating agents, and a compact workflow summary), the model and
#' token paragraph drafted by [LLMR::llm_methods_text()] over the run's
#' call-level rows, and a one-line limits note. When no calibration is attached
#' and the run is not a calibrated inference, the note states that the results
#' are model-conditioned and are not estimates of a human population.
#'
#' @param x An object accepted by [as_agent_run()].
#' @param ... May include `task`, a one-clause description of what the model was
#'   asked to do (spliced into the methods paragraph).
#' @return An object of class `agent_report`: a character vector with a print
#'   method that `cat()`s the lines.
#' @seealso [diagnostics()], [LLMR::llm_methods_text()]
#' @examples
#' \dontrun{
#' a <- agent("Aria", LLMR::llm_config("groq", "openai/gpt-oss-20b"))
#' a$chat("Hello")
#' report(a, task = "to answer a factual question")
#' }
#' @exportS3Method LLMR::report agent_run
report.agent_run <- function(x, ...) {
  run <- as_agent_run(x)
  dots <- list(...)
  task <- dots$task %||% NULL

  agents <- run$participants$name %||% character(0)
  agents <- stats::na.omit(as.character(agents))
  agent_line <- if (length(agents)) paste(agents, collapse = ", ") else "(none recorded)"

  header <- c(
    sprintf("Agentic run: kind = %s, run_id = %s.", run$kind, run$run_id),
    sprintf("Agents (%d): %s.", length(agents), agent_line),
    paste0("Workflow: ", .workflow_summary(run), ".")
  )

  calls <- tryCatch(
    tibble::as_tibble(as_tibble(run, level = "call")),
    error = function(e) NULL)
  methods <- if (!is.null(calls)) {
    tryCatch(LLMR::llm_methods_text(calls, task = task),
             error = function(e) "Methods paragraph unavailable (no call records).")
  } else {
    "Methods paragraph unavailable (no call records)."
  }

  # Scope any population-estimate phrasing in the drafted methods to the run's
  # claim type (a no-op for calibrated inference); then append the limits note.
  methods <- tryCatch(llm_claim_lint(methods, run = run, action = "scope"),
                      error = function(e) methods)

  body <- c(header, "", methods)
  note <- .claim_note(run)
  if (!is.null(note)) body <- c(body, "", note)

  structure(body, class = "agent_report")
}

# A bare Agent dispatches here; delegate through the run view.
#' @rdname report.agent_run
#' @exportS3Method LLMR::report Agent
report.Agent <- function(x, ...) {
  report.agent_run(as_agent_run(x), ...)
}

# Compact one-line description of how the run was orchestrated, from its design.
#' @keywords internal
#' @noRd
.workflow_summary <- function(run) {
  d <- run$design %||% list()
  if (!length(d)) return(run$kind %||% "single run")
  scalar <- function(v) {
    if (is.null(v) || !length(v)) return(NULL)
    if (length(v) == 1L && (is.atomic(v))) return(as.character(v))
    if (is.atomic(v)) return(paste0("[", length(v), "]"))
    NULL
  }
  parts <- character(0)
  for (nm in names(d)) {
    s <- scalar(d[[nm]])
    if (!is.null(s)) parts <- c(parts, paste0(nm, " = ", s))
  }
  if (!length(parts)) return(run$kind %||% "single run")
  paste(parts, collapse = "; ")
}

# The limits/claim note. Returns NULL when the run carries a calibrated
# inference or an attached calibration object (the caller then appends nothing);
# otherwise the model-conditioned caveat. Kept as a small helper so a later
# claim-type stage can branch on run$claim_type and reuse the calibration check.
#' @keywords internal
#' @noRd
.claim_note <- function(run) {
  is_calibrated <- identical(run$claim_type %||% NA_character_, "calibrated_inference")
  has_calibration <- !is.null(run$calibration)
  if (is_calibrated || has_calibration) return(NULL)
  paste0(
    "These results are model-conditioned; they are not estimates of a human ",
    "population unless validated against human data (no calibration attached).")
}

#' Draft a short report for an agent experiment
#'
#' A compact account of an [agent_experiment()] result: a header with the cell
#' and failure counts, then a per-condition breakdown over the design columns
#' (every column that is not `result`, `error`, `duration`, or `rep`), reporting
#' each condition's cell count and failures.
#'
#' @param x An [agent_experiment()] result (a tibble of class `agent_experiment`).
#' @param ... Unused.
#' @return An object of class `agent_report` (a character vector with a print
#'   method).
#' @seealso [diagnostics()], [agent_experiment()]
#' @exportS3Method LLMR::report agent_experiment
report.agent_experiment <- function(x, ...) {
  n_cells <- nrow(x)
  err <- if ("error" %in% names(x)) x$error else rep(NA_character_, n_cells)
  n_failed <- sum(!is.na(err))

  header <- sprintf("Agent experiment: %d cell(s), %d failed.", n_cells, n_failed)

  design_cols <- setdiff(names(x), c("result", "error", "duration", "rep"))
  breakdown <- character(0)
  if (length(design_cols) && n_cells) {
    grp <- tryCatch(.experiment_breakdown(x, design_cols, err),
                    error = function(e) character(0))
    if (length(grp)) breakdown <- c("Per condition:", grp)
  }

  body <- c(header, if (length(breakdown)) c("", breakdown))
  structure(body, class = "agent_report")
}

# Group an experiment frame by its design columns and report cells/failures per
# condition. Base R only (split on the interaction of the design columns).
#' @keywords internal
#' @noRd
.experiment_breakdown <- function(x, design_cols, err) {
  keys <- do.call(paste, c(lapply(design_cols, function(cn) {
    paste0(cn, "=", as.character(x[[cn]]))
  }), sep = ", "))
  idx <- split(seq_len(nrow(x)), keys)
  vapply(names(idx), function(k) {
    rows <- idx[[k]]
    sprintf("  - %s: %d cell(s), %d failed", k, length(rows),
            sum(!is.na(err[rows])))
  }, character(1), USE.NAMES = FALSE)
}

#' @exportS3Method print agent_report
print.agent_report <- function(x, ...) {
  cat(paste(unclass(x), collapse = "\n"), "\n", sep = "")
  invisible(x)
}

# ---- reset ------------------------------------------------------------------

#' Clear an agent's memory
#'
#' `LLMR::reset(agent)` delegates to the agent's own `agent$reset()` method, so
#' the two agree: both clear the agent's conversation memory, returning the
#' agent invisibly. Use it to reuse one configured agent across independent
#' trials without leaking state between them.
#'
#' @param x An [Agent].
#' @param ... Unused.
#' @return The agent, invisibly.
#' @seealso [Agent]
#' @examples
#' \dontrun{
#' a <- agent("Aria", LLMR::llm_config("groq", "openai/gpt-oss-20b"))
#' a$chat("Remember the number 7.")
#' LLMR::reset(a)        # same as a$reset()
#' }
#' @exportS3Method LLMR::reset Agent
reset.Agent <- function(x, ...) {
  x$reset()
  invisible(x)
}
