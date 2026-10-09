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
#' The check is read-only: every cell is converted through [as_agent_run()]. A
#' cell that cannot be examined (a failed cell, a result off the provenance
#' path such as a number or reply text, or a result whose conversion fails) is
#' not treated as evidence either way, so when any cell is unexamined and no
#' leak is found, the check is inconclusive: `clean` is `NA` and a warning
#' names the reasons. Two kinds of leak are reported:
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
#'   `clean` (`TRUE` when every cell was examined and no leak was found,
#'   `FALSE` when a leak was found, `NA` when no leak was found but some cells
#'   could not be examined), `n_cells`, `n_examined`, and `unexamined` (a
#'   tibble with columns `cell` and `reason`).
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
  # in that cell's agent memory (state level). A cell that cannot be examined
  # contributes no evidence and is recorded with its reason, so that the report
  # never calls an unexamined experiment clean.
  ids    <- vector("list", n_cells)   # character vector of agent_ids per cell
  hashes <- vector("list", n_cells)   # named list: agent_id -> char vec of content hashes
  reason <- rep(NA_character_, n_cells)
  for (i in seq_len(n_cells)) {
    res <- results[[i]]
    info <- NULL
    if (is.null(res)) {
      reason[i] <- "the cell has no result (it failed or returned NULL)"
    } else if (!.has_run_method(res)) {
      reason[i] <- sprintf("the cell returned a <%s>, which carries no agents", class(res)[1])
    } else {
      info <- tryCatch(.leakage_cell_info(res), error = function(e) e)
      if (inherits(info, "error")) {
        reason[i] <- paste("conversion failed:", conditionMessage(info))
        info <- NULL
      }
    }
    ids[[i]]    <- if (is.null(info)) character(0) else info$ids
    hashes[[i]] <- if (is.null(info)) list()      else info$hashes
  }
  unexamined <- tibble::tibble(cell = which(!is.na(reason)),
                               reason = reason[!is.na(reason)])

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

  clean <- if (nrow(leaks_tbl)) FALSE else if (nrow(unexamined)) NA else TRUE
  if (is.na(clean)) {
    cli::cli_warn(c(
      sprintf("The leakage check is inconclusive: %d of %d cell(s) could not be examined.",
              nrow(unexamined), n_cells),
      i = sprintf("First reason (cell %d): %s", unexamined$cell[1],
                  gsub("}", "}}", gsub("{", "{{", unexamined$reason[1], fixed = TRUE), fixed = TRUE)),
      i = "Return the agents or the run from `run_fn` so each cell can be inspected."))
  }
  structure(
    list(leaks = leaks_tbl, clean = clean, n_cells = n_cells,
         n_examined = n_cells - nrow(unexamined), unexamined = unexamined),
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
              x$n_cells, if (is.na(x$clean)) "NA" else if (isTRUE(x$clean)) "TRUE" else "FALSE"))
  n_unexamined <- if (is.data.frame(x$unexamined)) nrow(x$unexamined) else 0L
  if (isTRUE(x$clean)) {
    cat("  No shared agents, memory, or state detected across cells.\n")
  } else if (is.na(x$clean)) {
    cat(sprintf("  Inconclusive: no leak found, but %d of %d cell(s) could not be examined.\n",
                n_unexamined, x$n_cells))
    print(x$unexamined)
  } else {
    if (n_unexamined) {
      cat(sprintf("  %d cell(s) could not be examined; see `unexamined`.\n", n_unexamined))
    }
    # Summarize the leak kinds in plain text so their full names are always
    # shown; the tibble below abbreviates columns to the console width, which
    # can truncate a kind like "shared_agent_instance" on a narrow terminal.
    counts <- table(x$leaks$kind)
    for (k in names(counts)) cat(sprintf("  %s: %d\n", k, counts[[k]]))
    print(x$leaks)
  }
  invisible(x)
}
