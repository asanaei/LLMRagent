# leakage.R --------------------------------------------------------------------
# A read-only diagnostic for state leakage across the cells of an experiment.
# The anti-pattern: a run_fn that closes over an agent built *outside* the
# function, so every cell shares one live instance and its memory, counters,
# and identity bleed from cell to cell. Each cell should build its agents
# inside the function so it starts clean. Because every Agent carries a stable
# id (assigned at construction), the same id appearing in two cells is the
# tell: fresh-per-cell agents get distinct ids. This checks; it never enforces.

#' Detect shared state across experiment cells
#'
#' `check_state_leakage()` inspects the cells of an [agent_experiment()] (or a
#' plain list of convertible results) and reports whether any cell shares a live
#' agent with another. A correct experiment builds its agents *inside*
#' `run_fn`, so each cell gets fresh agents with distinct ids; an agent built
#' once *outside* `run_fn` and reused leaks its memory and identity across
#' cells, which silently couples conditions that should be independent.
#'
#' The check is read-only and conservative: every cell is converted through
#' [as_agent_run()] inside a `tryCatch()`, and a cell whose result is not
#' run-able (a number, a string, anything off the provenance path) is skipped
#' rather than treated as evidence. Two kinds of leak are reported:
#'
#' - `shared_agent_instance`: one `agent_id` participates in two different
#'   cells. Distinct cells with a fresh agent each cannot collide on an id, so
#'   a shared id is direct evidence of one reused instance.
#' - `memory_bleed`: a stronger signal on top of a shared id, raised when the
#'   later cell's agent memory already contains content hashed from the earlier
#'   cell, i.e. the agent literally remembers the prior condition.
#'
#' @param x An [agent_experiment()] result (the tibble with a `result`
#'   list-column), or a plain list whose elements are [Agent]s or `agent_run`
#'   objects (each element treated as one cell).
#' @return An object of class `agent_leakage_report`: a list with `leaks` (a
#'   tibble with columns `cell_i`, `cell_j`, `agent_id`, `kind`, `evidence`),
#'   `clean` (`TRUE` when no leaks were found), and `n_cells`.
#' @examples
#' \dontrun{
#' design <- data.frame(framing = c("gains", "losses"))
#'
#' # leaks: one agent is built once and reused by every cell
#' shared <- agent("S", LLMR::llm_config("groq", "openai/gpt-oss-20b"))
#' bad <- agent_experiment(design, reps = 1, run_fn = function(cond, rep) {
#'   shared$chat(cond$framing); shared
#' })
#' check_state_leakage(bad)          # clean = FALSE
#'
#' # clean: a fresh agent is built inside every cell
#' good <- agent_experiment(design, reps = 1, run_fn = function(cond, rep) {
#'   a <- agent("S", LLMR::llm_config("groq", "openai/gpt-oss-20b"))
#'   a$chat(cond$framing); a
#' })
#' check_state_leakage(good)         # clean = TRUE
#' }
#' @seealso [agent_experiment()], [as_agent_run()]
#' @export
check_state_leakage <- function(x) {
  cells <- .leakage_cells(x)
  n_cells <- cells$n
  results <- cells$results

  # Per cell: the participating agent_ids and the set of content hashes found
  # in that cell's agent memory (state level). Everything is guarded so a weird
  # cell degrades to "no evidence" instead of crashing the whole check.
  ids    <- vector("list", n_cells)   # character vector of agent_ids per cell
  hashes <- vector("list", n_cells)   # named list: agent_id -> char vec of content hashes
  for (i in seq_len(n_cells)) {
    info <- tryCatch(.leakage_cell_info(results[[i]]), error = function(e) NULL)
    ids[[i]]    <- if (is.null(info)) character(0) else info$ids
    hashes[[i]] <- if (is.null(info)) list()      else info$hashes
  }

  leaks <- list()
  for (i in seq_len(n_cells)) {
    ids_i <- ids[[i]]
    if (!length(ids_i)) next
    j_seq <- seq_len(n_cells)
    for (j in j_seq[j_seq > i]) {
      shared_ids <- intersect(ids_i, ids[[j]])
      for (aid in shared_ids) {
        # Shared instance: the same id lives in two distinct cells.
        leaks[[length(leaks) + 1L]] <- list(
          cell_i = i, cell_j = j, agent_id = aid,
          kind = "shared_agent_instance",
          evidence = sprintf("agent_id %s participates in cells %d and %d",
                             aid, i, j))
        # Memory bleed: the later cell's memory for this id already holds
        # content hashed from the earlier cell. Stronger evidence than a shared
        # id alone; derived from hash overlap, no API and no content stored.
        hi <- hashes[[i]][[aid]] %||% character(0)
        hj <- hashes[[j]][[aid]] %||% character(0)
        carried <- intersect(hi, hj)
        if (length(carried)) {
          leaks[[length(leaks) + 1L]] <- list(
            cell_i = i, cell_j = j, agent_id = aid,
            kind = "memory_bleed",
            evidence = sprintf(
              "agent_id %s in cell %d remembers %d message(s) from cell %d",
              aid, j, length(carried), i))
        }
      }
    }
  }

  leaks_tbl <- if (length(leaks)) {
    tibble::tibble(
      cell_i   = vapply(leaks, function(l) as.integer(l$cell_i), integer(1)),
      cell_j   = vapply(leaks, function(l) as.integer(l$cell_j), integer(1)),
      agent_id = vapply(leaks, function(l) as.character(l$agent_id), character(1)),
      kind     = vapply(leaks, function(l) as.character(l$kind), character(1)),
      evidence = vapply(leaks, function(l) as.character(l$evidence), character(1)))
  } else {
    tibble::tibble(
      cell_i = integer(0), cell_j = integer(0), agent_id = character(0),
      kind = character(0), evidence = character(0))
  }

  structure(
    list(leaks = leaks_tbl, clean = (nrow(leaks_tbl) == 0L), n_cells = n_cells),
    class = "agent_leakage_report")
}

# Resolve the input into a list of per-cell results plus a cell count. An
# agent_experiment (or any data frame carrying a `result` list-column) exposes
# its cells through that column; a plain list treats each element as a cell.
#' @keywords internal
#' @noRd
.leakage_cells <- function(x) {
  if (inherits(x, "agent_experiment") || (is.data.frame(x) && "result" %in% names(x))) {
    res <- x$result
    if (!is.list(res)) res <- as.list(res)
    return(list(results = res, n = length(res)))
  }
  if (is.list(x)) {
    return(list(results = x, n = length(x)))
  }
  stop("check_state_leakage() expects an agent_experiment result (a tibble ",
       "with a `result` list-column) or a list of Agent / agent_run objects.",
       call. = FALSE)
}

# For one cell's result, return its participating agent_ids and, per agent_id,
# the set of llm_hash()ed memory contents at that point. Non-convertible cells
# (a bare number, a string) raise inside as_agent_run() and are caught upstream.
#' @keywords internal
#' @noRd
.leakage_cell_info <- function(res) {
  if (is.null(res)) return(list(ids = character(0), hashes = list()))
  run <- as_agent_run(res)
  parts <- run$participants
  ids <- if (is.data.frame(parts) && "agent_id" %in% names(parts)) {
    unique(as.character(parts$agent_id))
  } else character(0)
  ids <- ids[!is.na(ids) & nzchar(ids)]

  st <- tryCatch(as_tibble(run, "state"), error = function(e) NULL)
  hashes <- list()
  if (is.data.frame(st) && nrow(st) &&
      all(c("agent_id", "content") %in% names(st))) {
    for (aid in unique(as.character(st$agent_id))) {
      txt <- st$content[as.character(st$agent_id) == aid]
      txt <- txt[!is.na(txt)]
      if (length(txt)) {
        hashes[[aid]] <- vapply(txt, function(z) LLMR::llm_hash(z),
                                character(1), USE.NAMES = FALSE)
      }
    }
  }
  list(ids = ids, hashes = hashes)
}

#' @export
print.agent_leakage_report <- function(x, ...) {
  cat(sprintf("<agent_leakage_report | %d cell(s) | clean: %s>\n",
              x$n_cells, if (isTRUE(x$clean)) "TRUE" else "FALSE"))
  if (isTRUE(x$clean)) {
    cat("  No shared agents, memory, or state detected across cells.\n")
  } else {
    # Summarize the leak kinds in plain text so their full names are always
    # shown; the tibble below abbreviates columns to the console width, which
    # can truncate a kind like "shared_agent_instance" on a narrow terminal.
    counts <- table(x$leaks$kind)
    for (k in names(counts)) cat(sprintf("  %s: %d\n", k, counts[[k]]))
    print(x$leaks)
  }
  invisible(x)
}
