# experiment.R ------------------------------------------------------------------
# Factorial experiments over agent runs: conditions x replications, sequential
# or parallel, with per-cell error capture and one tidy results frame.

#' Run a factorial agent experiment
#'
#' Takes a design frame (one row per condition), runs `run_fn` once per
#' condition and replication, and returns the design with `rep`, `result`
#' (list-column), `error`, and `duration` columns. A failing cell records its
#' error message and does not stop the others; re-run failures by filtering
#' on `!is.na(error)`.
#'
#' `run_fn` receives the condition as a named list plus the replication
#' number, and should build its agents *inside* the function so every cell
#' starts fresh:
#'
#' ```r
#' run_fn <- function(cond, rep) {
#'   cfg <- LLMR::llm_config("groq", "openai/gpt-oss-20b", temperature = 0.9)
#'   panel <- list(
#'     agent("A", cfg, persona = cond$persona_a),
#'     agent("B", cfg, persona = cond$persona_b)
#'   )
#'   deliberate(panel, cond$proposal, quiet = TRUE)
#' }
#' ```
#'
#' A note on seeds: model replies are sampled server-side, so a local seed
#' never affects them. Set one only when your code draws random numbers
#' itself (a `run_fn` that randomizes stimuli, or the `"random"` turn policy
#' inside it); under `parallel = TRUE` the workers then use statistically
#' sound parallel streams (`future.seed`). Combine with
#' `LLMR::llm_log_enable()` to keep a per-call audit file of the whole
#' experiment.
#'
#' @param design A data frame; each row is a condition, each column a factor
#'   of the design (personas, prompts, treatments, model names, ...).
#' @param run_fn `function(cond, rep)` where `cond` is one row of `design` as
#'   a named list. Whatever it returns is stored in the `result` list-column.
#' @param reps Replications per condition (default 1).
#' @param parallel If TRUE, cells run concurrently via the `future` framework
#'   (set a plan first, e.g. `future::plan(future::multisession)`).
#' @param quiet FALSE prints one progress line per completed cell.
#' @return `design` expanded by replication, plus `rep`, `result`, `error`,
#'   and `duration` columns.
#' @examples
#' \dontrun{
#' design <- expand.grid(
#'   temperature = c(0.2, 1.0),
#'   framing = c("gains", "losses"),
#'   stringsAsFactors = FALSE
#' )
#' res <- agent_experiment(design, reps = 3, run_fn = function(cond, rep) {
#'   cfg <- LLMR::llm_config("groq", "openai/gpt-oss-20b",
#'                           temperature = cond$temperature)
#'   a <- agent("Subject", cfg, quiet = TRUE)
#'   a$reply(paste("Decide under", cond$framing, "framing: ..."))
#' })
#' }
#' @seealso [deliberate()], [debate()], [LLMR::llm_usage()]
#' @export
agent_experiment <- function(design, run_fn, reps = 1L,
                             parallel = FALSE, quiet = FALSE) {
  stopifnot(is.data.frame(design), nrow(design) >= 1L, is.function(run_fn))
  reps <- max(1L, as.integer(reps))

  cells <- design[rep(seq_len(nrow(design)), each = reps), , drop = FALSE]
  rownames(cells) <- NULL
  cells$rep <- rep(seq_len(reps), times = nrow(design))

  one <- function(i) {
    cond <- as.list(cells[i, setdiff(names(cells), "rep"), drop = FALSE])
    t0 <- Sys.time()
    out <- tryCatch(
      list(result = run_fn(cond, cells$rep[i]), error = NA_character_),
      error = function(e) list(result = NULL, error = conditionMessage(e))
    )
    dur <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
    if (!quiet) {
      cli::cli_text("cell {i}/{nrow(cells)} {if (is.na(out$error)) 'done' else 'FAILED'} ({round(dur, 1)}s)")
    }
    list(result = out$result, error = out$error, duration = dur)
  }

  results <- if (isTRUE(parallel)) {
    if (!requireNamespace("future.apply", quietly = TRUE)) {
      stop("parallel = TRUE requires the 'future.apply' package.", call. = FALSE)
    }
    future.apply::future_lapply(seq_len(nrow(cells)), one, future.seed = TRUE)
  } else {
    lapply(seq_len(nrow(cells)), one)
  }

  cells$result   <- lapply(results, `[[`, "result")
  err <- vapply(results, function(r) r$error %||% NA_character_, character(1))
  cells$error    <- err
  cells$duration <- vapply(results, `[[`, 0, "duration")
  out <- tibble::as_tibble(cells)
  class(out) <- unique(c("agent_experiment", class(out)))
  out
}

#' @exportS3Method as_agent_run agent_experiment
as_agent_run.agent_experiment <- function(x, ...) {
  # Recurse into each cell's result: when a cell returned a classed result that
  # carries provenance (a deliberate(), a debate(), a chat agent, ...), fold its
  # events in, tagged with the design row. Cells whose result is not convertible
  # are skipped (their work was not on the agent provenance path).
  design_cols <- setdiff(names(x), c("result", "error", "duration"))
  rid <- .llmragent_id("run")
  spans <- list()
  participants <- list()
  utts <- list()
  for (i in seq_len(nrow(x))) {
    res <- x$result[[i]]
    if (is.null(res)) next
    sub <- tryCatch(as_agent_run(res), error = function(e) NULL)
    if (is.null(sub)) next
    cell_tag <- paste(vapply(design_cols, function(cn)
      paste0(cn, "=", as.character(x[[cn]][i])), character(1)), collapse = "; ")
    for (s in sub$spans) { s$run_id <- rid; s$note <- paste0("[", cell_tag, "] ", s$note %||% ""); spans[[length(spans) + 1L]] <- s }
    if (nrow(sub$participants)) participants[[length(participants) + 1L]] <- sub$participants
    su <- tryCatch(as_tibble(sub, "utterance"), error = function(e) NULL)
    if (!is.null(su) && nrow(su)) { su$run_id <- rid; su$call_id <- cell_tag; utts[[length(utts) + 1L]] <- su }
  }
  prov <- list(
    run_id = rid, kind = "experiment",
    design = list(conditions = design_cols, n_cells = nrow(x)),
    spans = spans,
    participants = if (length(participants)) tibble::as_tibble(unique(as.data.frame(do.call(rbind, participants), stringsAsFactors = FALSE))) else .empty_participants(),
    created_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    llmr_log = .llmragent_active_log(), agents = list())
  .run_from_provenance(prov,
    utterances = if (length(utts)) do.call(rbind, utts) else NULL,
    artifacts = list(design = as.data.frame(x[, design_cols, drop = FALSE])))
}
