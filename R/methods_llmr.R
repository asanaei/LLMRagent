# methods_llmr.R --------------------------------------------------------------
# LLMRagent's run objects join the wider LLMR ecosystem through three shared
# generics that LLMR owns: diagnostics() (the machine-readable health numbers),
# report() (the methods-section prose), and reset() (clear an apparatus's
# state). LLMR defines the generics and an erroring default; here we register
# the LLMRagent methods. The token and outcome figures are not recomputed by
# hand: they are read back through LLMR::llm_usage() over the run's call-level
# rows, so a run's diagnostics agree with everything else the ecosystem
# reports for the same calls. A level that cannot be read yields NA figures
# and a warning, never zeros.

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
#' token totals, tool and blocked-event counts, summed call durations, and the
#' study's `manifest_hash`. Token and outcome figures are read through
#' [LLMR::llm_usage()] over the run's call-level rows, so they match the rest of
#' the LLMR ecosystem.
#'
#' Token sums omit missing values, and `n_unknown_tokens` counts the successful
#' calls that reported no total, so a sum of zero with a positive
#' `n_unknown_tokens` does not mean that nothing was used. When a level of the
#' run cannot be read, the figures that depend on it are `NA` and a warning
#' names the level and the reason; the other figures are still computed. A run
#' with no calls has zero counts and sums. `duration_s` adds the recorded call
#' durations rather than measuring elapsed time, and is `NA` when calls exist
#' but none recorded a duration. An agent's own accounting (`agent$usage()`)
#' can include tool-loop rounds, compactions, and embedding operations that
#' have no call row, so the two need not agree.
#'
#' @param x An `agent_run` or an [Agent]. Convert other results with
#'   [as_agent_run()] first.
#' @param ... Unused.
#' @return A one-row tibble with the following columns.
#'   \describe{
#'     \item{`run_id`, `kind`}{Run identifier and kind.}
#'     \item{`n_calls`, `n_ok`, `n_failed`, `ok_rate`}{Stored call counts and success rate.}
#'     \item{`n_truncated`, `n_filtered`}{Counts of recorded finish reasons.}
#'     \item{`tokens_sent`, `tokens_received`, `tokens_total`, `reasoning_tokens`}{Sums of reported counts.}
#'     \item{`n_tool_calls`}{Rows in the tool view.}
#'     \item{`n_blocked`}{Events with status `"blocked"` or `"stopped"`.}
#'     \item{`duration_s`}{Sum of available call durations in seconds.}
#'     \item{`manifest_hash`}{Study manifest hash, or `NA` when unavailable.}
#'     \item{`n_unknown_tokens`}{Successful rows missing total-token usage,
#'       excluding recorded batch follow-on rows.}
#'   }
#' @examples
#' a <- agent("Aria", LLMR::llm_config("groq", "openai/gpt-oss-20b"), quiet = TRUE)
#' diagnostics(a)   # no calls yet: zero counts and sums
#'
#' \dontrun{
#' a$chat("Hello")
#' diagnostics(a)
#' }
#' @seealso [report()], [LLMR::llm_usage()], [agent_manifest()]
#' @exportS3Method LLMR::diagnostics agent_run
diagnostics.agent_run <- function(x, ...) {
  run <- as_agent_run(x)
  extracted <- .read_run_level(run, "call")
  calls <- extracted$data
  u <- .call_usage(extracted)
  tools <- .read_run_level(run, "tool")
  events <- .read_run_level(run, "event")
  n_blocked <- NA_integer_
  if (is.null(events$error)) {
    if ("status" %in% names(events$data)) {
      n_blocked <- sum(events$data$status %in% c("blocked", "stopped"))
    } else if (!nrow(events$data)) {
      n_blocked <- 0L
    } else {
      warning("The 'event' level could not be summarized. Missing status column.", call. = FALSE)
    }
  }
  mh <- tryCatch(.agent_manifest(run, extracted)$manifest_hash,
                 error = function(e) NA_character_)
  # Stored response records use duration_s; llm_usage() reads duration.
  # Sum the available response-record durations here.
  duration <- tryCatch({
    if (is.null(calls)) NA_real_ else if (!nrow(calls)) 0 else
      if (!"duration_s" %in% names(calls) || all(is.na(calls$duration_s))) NA_real_ else
        sum(calls$duration_s, na.rm = TRUE)
  }, error = function(e) {
    warning("The 'call' level durations could not be summarized. duration_s is NA. ",
            conditionMessage(e), call. = FALSE)
    NA_real_
  })
  tibble::tibble(
    run_id = run$run_id, kind = run$kind,
    n_calls = u$n, n_ok = u$n_ok, n_failed = u$n_failed, ok_rate = u$ok_rate,
    n_truncated = u$n_truncated, n_filtered = u$n_filtered,
    tokens_sent = u$sent_tokens, tokens_received = u$rec_tokens,
    tokens_total = u$total_tokens, reasoning_tokens = u$reasoning_tokens,
    n_tool_calls = if (is.null(tools$error)) as.integer(nrow(tools$data)) else NA_integer_,
    n_blocked = as.integer(n_blocked), duration_s = duration, manifest_hash = mh,
    n_unknown_tokens = u$n_unknown_tokens)
}

# Keep extraction failures separate from valid empty views.
.read_run_level <- function(run, level, warn = TRUE) {
  tryCatch(list(data = tibble::as_tibble(as_tibble(run, level = level)), error = NULL),
    error = function(e) {
      msg <- sprintf("The '%s' level could not be read. Values that depend on it are NA. %s",
                     level, conditionMessage(e))
      if (warn) warning(msg, call. = FALSE)
      list(data = NULL, error = msg)
    })
}

.usage_na <- function(n = NA_integer_) {
  tibble::tibble(
    n = as.integer(n), n_ok = NA_integer_, n_failed = NA_integer_, ok_rate = NA_real_,
    n_truncated = NA_integer_, n_filtered = NA_integer_, sent_tokens = NA_integer_,
    rec_tokens = NA_integer_, total_tokens = NA_integer_, reasoning_tokens = NA_integer_,
    cached_tokens = NA_integer_, n_unknown_tokens = NA_integer_, duration_s = NA_real_)
}

.unknown_tokens <- function(calls) {
  n <- nrow(calls)
  ok <- calls[["success"]] %||% rep(NA, n)
  total <- calls[["total_tokens"]] %||% rep(NA_integer_, n)
  batch_i <- calls[["rowpack_row"]] %||% rep(NA_integer_, n)
  sum((ok %in% TRUE) & is.na(total) & !(!is.na(batch_i) & batch_i > 1L))
}

.call_usage <- function(extracted) {
  if (!is.null(extracted$error)) return(.usage_na())
  calls <- extracted$data
  if (!nrow(calls)) return(.usage_zero())
  tryCatch({
    u <- LLMR::llm_usage(calls)
    if (is.null(u[["n_unknown_tokens"]])) u$n_unknown_tokens <- .unknown_tokens(calls)
    u
  }, error = function(e) {
    warning("Call usage could not be summarized. Usage statistics are NA. ",
            conditionMessage(e), call. = FALSE)
    .usage_na(nrow(calls))
  })
}

# Methods text uses the same extraction outcome as the caller's statistics.
.run_methods <- function(extracted, task = NULL) {
  if (!is.null(extracted$error)) {
    return(paste("Methods text is unavailable because the call records could not be read.",
                 extracted$error))
  }
  calls <- extracted$data
  class(calls) <- unique(c("llmr_experiment", class(calls)))
  methods <- tryCatch(LLMR::report(calls, task = task), error = function(e) {
    msg <- paste("Methods text could not be generated from the call records.", conditionMessage(e))
    warning(msg, call. = FALSE)
    msg
  })
  unknown <- .unknown_tokens(calls)
  if (unknown > 0L) methods <- c(methods, sprintf(
    "%d successful call record(s) lack total-token usage. The token sums include reported counts.", unknown))
  methods
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
#' token paragraph drafted by [LLMR::report()] over the run's
#' call-level rows, and a one-line limits note stating that the results are
#' model-conditioned and are not estimates of a human population.
#'
#' When some successful calls reported no total-token usage, the report says
#' so beside the token sums. When the call records cannot be read, or the
#' methods paragraph or the claim-language check cannot be produced, the
#' report warns and says in its text what is missing.
#'
#' @param x An `agent_run` or an [Agent]. Convert other results with
#'   [as_agent_run()] first.
#' @param ... May include `task`, a one-clause description of what the model was
#'   asked to do (spliced into the methods paragraph).
#' @return An object of class `agent_report`: a character vector with a print
#'   method that `cat()`s the lines.
#' @examples
#' a <- agent("Aria", LLMR::llm_config("groq", "openai/gpt-oss-20b"), quiet = TRUE)
#' report(a, task = "to answer a factual question")
#'
#' \dontrun{
#' a$chat("Hello")
#' report(a, task = "to answer a factual question")
#' }
#' @seealso [diagnostics()], [agent_manifest()], [LLMR::report()]
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

  methods <- .run_methods(.read_run_level(run, "call"), task)
  methods <- tryCatch(llm_claim_lint(methods, run = run, action = "scope"),
    error = function(e) {
      msg <- paste("The claim-language check could not be completed.", conditionMessage(e))
      warning(msg, call. = FALSE)
      c(methods, msg)
    })

  body <- c(header, "", methods)
  note <- .claim_note()
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

# The limits note appended to every run report.
#' @keywords internal
#' @noRd
.claim_note <- function() {
  paste0(
    "These results are model-conditioned; they are not estimates of a human ",
    "population without external human validation.")
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
