# workflow.R ------------------------------------------------------------------
# A small, sequential, explicit-state graph runtime. Nodes are
# agents, plain R functions, evaluators, or human gates; edges may be
# conditional; loops are bounded. State is one explicit list passed between
# nodes. A checkpoint (an RDS state snapshot + an append-only JSONL event line)
# is written after every node, so a run can resume without rerunning completed
# nodes, fork at a snapshot, and replay with state-hash verification. The
# existing pipeline()/conversation() are EXPRESSIBLE as workflows (constructors
# below) but keep their own implementations as the simpler interface: nobody is
# pushed onto the graph for a simple study.

# ---- construction -----------------------------------------------------------

#' Build an agent workflow (a small, explicit graph)
#'
#' A workflow is a directed graph whose nodes transform a shared, explicit
#' `state` list. Build it with `agent_workflow()` then [add_node()] and
#' [add_edge()]; run it with [run_workflow()]. The runtime is minimal:
#' sequential execution, one mutable state, and a checkpoint after each node. A
#' run is therefore auditable and resumable. Reach for it only when a study genuinely
#' needs branching, looping, or mid-run human review; [agent_pipeline()] and
#' [conversation()] remain the simpler interfaces.
#'
#' @param name A label for the workflow.
#' @return An `agent_workflow` object (built up immutably by [add_node()] /
#'   [add_edge()]).
#' @seealso [add_node()], [add_edge()], [run_workflow()], [resume_workflow()],
#'   [fork_workflow()], [replay_run()]
#' @examples
#' wf <- agent_workflow("classify") |>
#'   add_node("clean", function(state) { state$x <- trimws(state$input); state }) |>
#'   add_node("label", function(state) { state$label <- nchar(state$x) > 3; state }) |>
#'   add_edge("clean", "label")
#' run <- run_workflow(wf, input = "  hello  ")
#' run$state$label
#' @export
agent_workflow <- function(name) {
  structure(list(name = as.character(name)[1], nodes = list(),
                 edges = tibble::tibble(from = character(0), to = character(0)),
                 edge_when = list(), entry = NA_character_),
            class = "agent_workflow")
}

#' Add a node to a workflow
#'
#' A node transforms the shared `state`. It may be: an [Agent] (its `reply()` is
#' called on `state[[input_key]]` and the result written to `state[[output_key]]`);
#' a plain `function(state)` returning the new state; an evaluator (a function or
#' `Agent` + `schema` writing a structured verdict into state, used by
#' conditional edges); or a [human_gate()] (pauses the run for sign-off).
#'
#' @param wf An `agent_workflow`.
#' @param name Node name (unique within the workflow).
#' @param node An `Agent`, a `function(state)`, or a `human_gate()` marker.
#' @param input_key,output_key For an agent node: where to read the prompt and
#'   write the reply in `state` (default `"input"` / the node name).
#' @param schema For an evaluator agent node: a JSON schema; the parsed verdict
#'   is written to `state[[output_key]]`.
#' @param ... Reserved.
#' @return The workflow, with the node added. The first node added is the entry.
#' @seealso [agent_workflow()], [add_edge()]
#' @export
add_node <- function(wf, name, node, input_key = "input", output_key = NULL,
                     schema = NULL, ...) {
  stopifnot(inherits(wf, "agent_workflow"))
  name <- as.character(name)[1]
  if (name %in% names(wf$nodes)) stop("Duplicate node name: ", name, call. = FALSE)
  kind <- if (inherits(node, "Agent")) (if (!is.null(schema)) "evaluator_agent" else "agent")
          else if (inherits(node, "llmragent_human_gate")) "human_gate"
          else if (is.function(node)) "function"
          else stop("A node must be an Agent, a function(state), or a human_gate().",
                    call. = FALSE)
  wf$nodes[[name]] <- list(name = name, kind = kind, node = node,
                           input_key = input_key,
                           output_key = output_key %||% name, schema = schema)
  if (is.na(wf$entry)) wf$entry <- name
  wf
}

#' Add an edge to a workflow
#'
#' Connects two nodes. With `when = NULL` the edge is unconditional; with a
#' predicate `when = function(state) -> logical` it is taken only when the
#' predicate holds. After a node, the runtime takes the first outgoing edge whose
#' predicate is `TRUE` (or the unconditional edge). A back-edge forms a loop,
#' bounded by `max_steps` in [run_workflow()].
#'
#' @param wf An `agent_workflow`.
#' @param from,to Node names.
#' @param when Optional `function(state) -> logical`.
#' @return The workflow, with the edge added.
#' @seealso [add_node()], [run_workflow()]
#' @export
add_edge <- function(wf, from, to, when = NULL) {
  stopifnot(inherits(wf, "agent_workflow"))
  wf$edges <- rbind(wf$edges, tibble::tibble(from = as.character(from)[1],
                                             to = as.character(to)[1]))
  # Append the predicate WITHOUT deleting the slot: `list[[i]] <- NULL` removes
  # the element in R, so an unconditional (NULL) edge must be appended as a
  # length-1 list to keep edge_when aligned with the edges rows.
  wf$edge_when <- c(wf$edge_when, list(when))
  wf
}

#' @export
print.agent_workflow <- function(x, ...) {
  cat(sprintf("<agent_workflow '%s' | %d node(s), %d edge(s) | entry: %s>\n",
              x$name, length(x$nodes), nrow(x$edges), x$entry %||% "?"))
  for (nm in names(x$nodes)) cat(sprintf("  - %s (%s)\n", nm, x$nodes[[nm]]$kind))
  invisible(x)
}

# ---- execution --------------------------------------------------------------

#' Run a workflow
#'
#' Executes the graph from its entry node, threading one explicit `state` list,
#' writing a checkpoint (state snapshot + event line) after each node. Stops at a
#' terminal node (no outgoing edge taken), when a human gate pauses the run, or
#' when `max_steps` node executions is reached.
#'
#' @param wf An `agent_workflow`.
#' @param input The initial input, placed at `state$input`.
#' @param state Optional initial state list (merged with `input`).
#' @param checkpoint_dir Optional directory for checkpoints (state RDS + a
#'   `run.jsonl` event log + a `cursor.json`). When `NULL`, a temporary directory
#'   is used so resume/replay still work within the session.
#' @param max_steps Maximum node executions before stopping (loop guard).
#' @param quiet If `FALSE`, print one line per node.
#' @param ... Passed to agent nodes' `reply()`.
#' @return An object of class `agent_workflow_run`: a list with `status`
#'   (`"done"`, `"paused"`, or `"failed"`), `state`, `checkpoint_dir`, `steps`
#'   (a tibble of node/status/state_hash), and (when paused) `checkpoint`.
#' @seealso [resume_workflow()], [fork_workflow()], [replay_run()]
#' @export
run_workflow <- function(wf, input = NULL, state = NULL, checkpoint_dir = NULL,
                         max_steps = 64L, quiet = TRUE, ...) {
  stopifnot(inherits(wf, "agent_workflow"))
  if (!length(wf$nodes)) stop("The workflow has no nodes.", call. = FALSE)
  dir <- checkpoint_dir %||% file.path(tempdir(), paste0("wf-", .llmragent_id("ck")))
  dir.create(file.path(dir, "state"), recursive = TRUE, showWarnings = FALSE)

  st <- state %||% list()
  if (!is.null(input)) st$input <- input
  run_id <- .llmragent_id("wfrun")
  .wf_save_state(dir, 0L, "entry", st)
  .wf_log(dir, run_id, "entry", "ok", LLMR::llm_hash(st), 0L)

  .wf_drive(wf, st, dir, run_id, node = wf$entry, step = 0L,
            max_steps = max_steps, quiet = quiet, ...)
}

# The driver: execute nodes following edges until terminal / paused / budget.
#' @keywords internal
#' @noRd
.wf_drive <- function(wf, st, dir, run_id, node, step, max_steps, quiet, ...) {
  steps <- list()
  cur <- node
  while (!is.na(cur)) {
    step <- step + 1L
    if (step > max_steps) {
      .wf_cursor(dir, run_id, cur, step, "failed")
      rlang::abort(sprintf("Workflow exceeded max_steps = %d at node '%s'.", max_steps, cur),
                   class = c("llmragent_workflow_error", "error", "condition"))
    }
    nd <- wf$nodes[[cur]]
    if (is.null(nd)) stop("Edge points to unknown node: ", cur, call. = FALSE)

    # human gate: pause and return a resumable checkpoint
    if (identical(nd$kind, "human_gate")) {
      cp <- structure(list(
        schema = "llmragent_wf_checkpoint/1", run_id = run_id, wf = wf,
        state = st, next_node = .wf_next(wf, cur, st), gate = nd$name,
        prompt = nd$node$prompt, dir = dir, step = step, max_steps = max_steps,
        decision = NULL, created_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")),
        class = "llmragent_wf_checkpoint")
      .wf_save_state(dir, step, cur, st)
      .wf_log(dir, run_id, cur, "paused", LLMR::llm_hash(st), step)
      .wf_cursor(dir, run_id, .wf_next(wf, cur, st), step, "paused")
      steps[[length(steps) + 1L]] <- list(node = cur, status = "paused",
                                          state_hash = LLMR::llm_hash(st))
      return(.wf_result("paused", st, dir, steps, checkpoint = cp))
    }

    out <- tryCatch(.wf_exec_node(nd, st, ...),
                    error = function(e) structure(list(err = conditionMessage(e)),
                                                  class = "wf_node_error"))
    if (inherits(out, "wf_node_error")) {
      .wf_save_state(dir, step, cur, st)
      .wf_log(dir, run_id, cur, "failed", LLMR::llm_hash(st), step, note = out$err)
      .wf_cursor(dir, run_id, cur, step, "failed")
      steps[[length(steps) + 1L]] <- list(node = cur, status = "failed",
                                          state_hash = LLMR::llm_hash(st))
      res <- .wf_result("failed", st, dir, steps)
      res$error <- out$err
      return(res)
    }
    st <- out
    sh <- LLMR::llm_hash(st)
    .wf_save_state(dir, step, cur, st)
    .wf_log(dir, run_id, cur, "ok", sh, step)
    steps[[length(steps) + 1L]] <- list(node = cur, status = "ok", state_hash = sh)
    if (!quiet) cli::cli_text("{.strong [wf:{cur}]} done")
    cur <- .wf_next(wf, cur, st)
  }
  .wf_cursor(dir, run_id, NA_character_, step, "done")
  .wf_result("done", st, dir, steps)
}

# Execute one node, returning the new state.
#' @keywords internal
#' @noRd
.wf_exec_node <- function(nd, st, ...) {
  switch(nd$kind,
    "function" = {
      res <- nd$node(st)
      if (!is.list(res)) { st[[nd$output_key]] <- res; res <- st }
      res
    },
    agent = {
      prompt <- st[[nd$input_key]] %||% st$input %||% ""
      st[[nd$output_key]] <- nd$node$reply(prompt, ...)
      st
    },
    evaluator_agent = {
      prompt <- st[[nd$input_key]] %||% st$input %||% ""
      st[[nd$output_key]] <- nd$node$ask_structured(prompt, schema = nd$schema, ...)
      st
    },
    stop("Unknown node kind: ", nd$kind, call. = FALSE))
}

# The next node after `cur`: first outgoing edge whose predicate holds (or the
# unconditional one). NA when terminal.
#' @keywords internal
#' @noRd
.wf_next <- function(wf, cur, st) {
  idx <- which(wf$edges$from == cur)
  if (!length(idx)) return(NA_character_)
  for (i in idx) {
    w <- wf$edge_when[[i]]
    if (is.null(w)) return(wf$edges$to[i])
    ok <- tryCatch(isTRUE(w(st)), error = function(e) FALSE)
    if (ok) return(wf$edges$to[i])
  }
  NA_character_
}

# ---- checkpoint I/O ---------------------------------------------------------

#' @keywords internal
#' @noRd
.wf_save_state <- function(dir, step, node, st) {
  f <- file.path(dir, "state", sprintf("%03d_%s.rds", step, .wf_safe(node)))
  saveRDS(st, f)
  invisible(f)
}
#' @keywords internal
#' @noRd
.wf_log <- function(dir, run_id, node, status, state_hash, step, note = NA_character_) {
  rec <- list(ts = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"), schema_version = "1.0",
              kind = "workflow_node", run_id = run_id, node = node, status = status,
              state_hash = state_hash, step = step, note = note)
  line <- jsonlite::toJSON(rec, auto_unbox = TRUE, null = "null", na = "null")
  cat(as.character(line), "\n", sep = "", file = file.path(dir, "run.jsonl"), append = TRUE)
}
#' @keywords internal
#' @noRd
.wf_cursor <- function(dir, run_id, next_node, step, status) {
  cur <- list(run_id = run_id, next_node = next_node, step = step, status = status)
  writeLines(as.character(jsonlite::toJSON(cur, auto_unbox = TRUE, null = "null", na = "null")),
             file.path(dir, "cursor.json"))
}
#' @keywords internal
#' @noRd
.wf_safe <- function(x) gsub("[^A-Za-z0-9_.-]+", "_", x)

#' @keywords internal
#' @noRd
.wf_result <- function(status, st, dir, steps, checkpoint = NULL) {
  step_tbl <- if (length(steps)) do.call(rbind, lapply(steps, function(s)
    tibble::tibble(node = s$node, status = s$status, state_hash = s$state_hash)))
    else tibble::tibble(node = character(0), status = character(0), state_hash = character(0))
  out <- list(status = status, state = st, checkpoint_dir = dir, steps = step_tbl)
  if (!is.null(checkpoint)) out$checkpoint <- checkpoint
  structure(out, class = "agent_workflow_run")
}

#' @export
print.agent_workflow_run <- function(x, ...) {
  cat(sprintf("<agent_workflow_run | status: %s | %d node(s) executed>\n",
              x$status, nrow(x$steps)))
  if (identical(x$status, "paused")) cat("  paused at a human gate; approve and resume_workflow().\n")
  if (identical(x$status, "failed")) cat("  failed: ", x$error %||% "?", "\n", sep = "")
  cat("  checkpoint dir: ", x$checkpoint_dir, "\n", sep = "")
  invisible(x)
}

# ---- resume / fork / replay -------------------------------------------------

#' Resume a paused or failed workflow run
#'
#' Continues from the last good checkpoint without rerunning completed nodes. For
#' a run paused at a human gate, supply `approve = TRUE` to proceed (or `FALSE`
#' to stop). For a failed run, resume retries from the failed node.
#'
#' @param x A paused/failed `agent_workflow_run`, its `checkpoint`, or a
#'   `checkpoint_dir` path (for a failed run, also pass `wf`).
#' @param wf The `agent_workflow` (required when `x` is a bare `checkpoint_dir`
#'   from a failed run, which carries no embedded workflow).
#' @param approve For a gated pause: `TRUE` to continue past the gate.
#' @param max_steps Loop guard for the continuation.
#' @param quiet If `FALSE`, print progress.
#' @param ... Passed to agent nodes.
#' @return An `agent_workflow_run`.
#' @seealso [run_workflow()], [fork_workflow()], [replay_run()]
#' @export
resume_workflow <- function(x, wf = NULL, approve = TRUE, max_steps = 64L,
                            quiet = TRUE, ...) {
  cp <- .wf_resume_checkpoint(x, wf)
  if (!approve) {
    cli::cli_text("Workflow gate not approved; run remains paused.")
    return(invisible(.wf_result("paused", cp$state, cp$dir, list(), checkpoint = cp)))
  }
  .wf_drive(cp$wf, cp$state, cp$dir, cp$run_id, node = cp$next_node, step = cp$step,
            max_steps = max_steps, quiet = quiet, ...)
}

# Resolve a resume target into a checkpoint: a paused checkpoint (carries the
# workflow + next_node), or a failed run reconstructed from its dir (needs wf;
# resumes from the failed node, i.e. the cursor's node).
#' @keywords internal
#' @noRd
.wf_resume_checkpoint <- function(x, wf) {
  if (inherits(x, "llmragent_wf_checkpoint")) return(x)
  if (inherits(x, "agent_workflow_run") && !is.null(x$checkpoint)) return(x$checkpoint)
  # failed run: reconstruct from the dir + cursor
  dir <- .wf_dir_of(x)
  cur <- tryCatch(jsonlite::fromJSON(file.path(dir, "cursor.json")), error = function(e) NULL)
  if (is.null(cur)) stop("No cursor.json to resume from in ", dir, call. = FALSE)
  if (is.null(wf)) {
    stop("Resuming a failed run needs the workflow: resume_workflow(dir, wf = ).", call. = FALSE)
  }
  snaps <- sort(list.files(file.path(dir, "state"), pattern = "\\.rds$"))
  st <- if (length(snaps)) readRDS(file.path(dir, "state", snaps[length(snaps)])) else list()
  # failed -> retry from the recorded node; paused -> continue from next_node
  node <- if (identical(cur$status, "failed")) cur$next_node else cur$next_node
  structure(list(wf = wf, state = st, next_node = node, dir = dir,
                 step = (cur$step %||% 1L) - 1L, run_id = cur$run_id),
            class = "llmragent_wf_checkpoint")
}

#' Fork a workflow run at a checkpoint
#'
#' Copies the state at a chosen step into a new directory with a fresh run id,
#' optionally mutating it, and runs forward from there. The original run is
#' untouched: resume continues the same run, fork branches a new one.
#'
#' @param x A `checkpoint_dir` or `agent_workflow_run`.
#' @param wf The `agent_workflow` (needed to run the branch).
#' @param at Step index to fork from (default: the last completed step).
#' @param new_dir Directory for the branch (default: a fresh temp dir).
#' @param mutate Optional `function(state) -> state` applied before running.
#' @param max_steps,quiet,... As in [run_workflow()].
#' @return An `agent_workflow_run` for the branch.
#' @seealso [run_workflow()], [resume_workflow()]
#' @export
fork_workflow <- function(x, wf, at = NULL, new_dir = NULL, mutate = NULL,
                          max_steps = 64L, quiet = TRUE, ...) {
  dir <- .wf_dir_of(x)
  snaps <- sort(list.files(file.path(dir, "state"), pattern = "\\.rds$"))
  if (!length(snaps)) stop("No state snapshots to fork from.", call. = FALSE)
  pick <- if (is.null(at)) snaps[length(snaps)] else {
    hit <- snaps[grepl(sprintf("^%03d_", at), snaps)]
    if (!length(hit)) stop("No snapshot at step ", at, call. = FALSE) else hit[1]
  }
  st <- readRDS(file.path(dir, "state", pick))
  if (is.function(mutate)) st <- mutate(st)
  step <- as.integer(sub("_.*$", "", pick))
  node_name <- sub("^[0-9]+_", "", sub("\\.rds$", "", pick))
  nd <- .wf_resolve_node(wf, node_name)
  bdir <- new_dir %||% file.path(tempdir(), paste0("wf-fork-", .llmragent_id("ck")))
  dir.create(file.path(bdir, "state"), recursive = TRUE, showWarnings = FALSE)
  run_id <- .llmragent_id("wffork")
  .wf_save_state(bdir, step, node_name, st)
  .wf_log(bdir, run_id, node_name, "ok", LLMR::llm_hash(st), step)
  # Continue from the node AFTER the forked one -- except at step 0: the
  # "entry" snapshot is the state BEFORE any node ran, so a fork there must
  # execute the entry node itself, not skip it.
  start <- if (step == 0L) wf$entry else .wf_next(wf, nd, st)
  .wf_drive(wf, st, bdir, run_id, node = start, step = step,
            max_steps = max_steps, quiet = quiet, ...)
}

#' Replay a workflow run, verifying state hashes
#'
#' Re-executes the graph from the original input and compares each node's
#' resulting state hash to the recorded one. `verify = "structural"` (default)
#' checks deterministic nodes (functions/evaluators) exactly and, for model
#' (agent) nodes, does not require the sampled text to match (model output is
#' nondeterministic); `verify = "strict"` requires every node to match (sound
#' only for fully deterministic graphs or archive-served calls). A mismatch
#' raises `llmragent_replay_mismatch` naming the first divergent node.
#'
#' A run containing a [human_gate()] replays up to the gate and pauses there,
#' verifying the executed nodes before it; a pause is not an executed node, so
#' its `replay_match` is `NA` and it occupies no comparison position.
#'
#' @param x A `checkpoint_dir` or `agent_workflow_run`.
#' @param wf The `agent_workflow`.
#' @param verify `"structural"` or `"strict"`.
#' @param max_steps,quiet,... As in [run_workflow()].
#' @return An `agent_workflow_run` for the replay (its `steps` carries a
#'   `replay_match` column).
#' @seealso [run_workflow()]
#' @export
replay_run <- function(x, wf, verify = c("structural", "strict"),
                       max_steps = 64L, quiet = TRUE, ...) {
  verify <- match.arg(verify)
  dir <- .wf_dir_of(x)
  recorded <- .wf_read_log(dir)
  if (!nrow(recorded)) stop("No recorded run to replay.", call. = FALSE)
  input_state <- tryCatch(readRDS(file.path(dir, "state",
    sort(list.files(file.path(dir, "state"), pattern = "\\.rds$"))[1])),
    error = function(e) list())

  fresh <- run_workflow(wf, state = input_state,
                        checkpoint_dir = file.path(tempdir(), paste0("wf-replay-", .llmragent_id("ck"))),
                        max_steps = max_steps, quiet = quiet, ...)

  # Compare by SEQUENCE POSITION, not node name: the i-th executed node of the
  # replay against the i-th recorded "ok" step. This is correct for loops (each
  # visit to a node is a distinct position) where a name-based match would
  # collapse every visit onto the first recorded one. A divergence in the
  # executed sequence (different node order, or a different path through a
  # branch) is itself a mismatch.
  # Drop the "entry" event (the initial state snapshot, not an executed node):
  # fresh$steps never contains it, so it must not occupy position 1 of the
  # recorded sequence either. Paused (human-gate) steps are dropped from BOTH
  # sides: rec_ok keeps only "ok" rows, and a fresh replay of a gated run
  # pauses at the gate, whose row is a pause marker, not an executed node --
  # keeping it on one side only would shift every later position. A gate's
  # replay_match is NA (a pause is not hash-verified).
  rec_ok <- recorded[recorded$status == "ok" & recorded$node != "entry", ]
  fresh_idx <- which(fresh$steps$status != "paused")
  matches <- rep(NA, nrow(fresh$steps))
  for (j in seq_along(fresh_idx)) {
    i <- fresh_idx[j]
    nm <- fresh$steps$node[i]
    fh <- fresh$steps$state_hash[i]
    is_model <- nm %in% names(wf$nodes) &&
      wf$nodes[[nm]]$kind %in% c("agent", "evaluator_agent")
    if (j > nrow(rec_ok)) {
      # the replay executed more steps than were recorded: a path divergence
      matches[i] <- FALSE
      rlang::abort(
        sprintf("Replay diverged: step %d ('%s') has no recorded counterpart.", j, nm),
        class = c("llmragent_replay_mismatch", "error", "condition"), node = nm)
    }
    rnode <- rec_ok$node[j]; rh <- rec_ok$state_hash[j]
    # a different node at this position is a path divergence (always a mismatch)
    if (!identical(nm, rnode)) {
      matches[i] <- FALSE
      rlang::abort(
        sprintf("Replay diverged at step %d: recorded node '%s', replayed '%s'.", j, rnode, nm),
        class = c("llmragent_replay_mismatch", "error", "condition"), node = nm)
    }
    same <- identical(rh, fh)
    matches[i] <- same
    if (!same && (identical(verify, "strict") || !is_model)) {
      rlang::abort(
        sprintf("Replay mismatch at step %d node '%s': recorded %s, replayed %s (verify = %s).",
                j, nm, substr(rh, 1, 12), substr(fh, 1, 12), verify),
        class = c("llmragent_replay_mismatch", "error", "condition"),
        node = nm, recorded = rh, replayed = fh)
    }
  }
  fresh$steps$replay_match <- matches
  fresh
}

# ---- expressibility: pipeline / conversation as workflows -------------------

#' Express an agent pipeline as a workflow
#'
#' Builds the linear graph equivalent of [agent_pipeline()] (node i is agent i,
#' each feeding the next). The original [agent_pipeline()] keeps its own simple
#' implementation; this exists to show the graph can express it and to let a linear
#' pipeline gain checkpointing when wanted.
#'
#' @param agents A list of [Agent]s, in order.
#' @return An `agent_workflow`.
#' @seealso [agent_pipeline()], [run_workflow()]
#' @export
workflow_from_pipeline <- function(agents) {
  if (inherits(agents, "Agent")) agents <- list(agents)
  stopifnot(is.list(agents), length(agents) >= 1L)
  wf <- agent_workflow("pipeline")
  prev <- NULL
  for (i in seq_along(agents)) {
    nm <- paste0("stage_", i)
    wf <- add_node(wf, nm, agents[[i]],
                   input_key = if (i == 1L) "input" else paste0("stage_", i - 1L),
                   output_key = nm)
    if (!is.null(prev)) wf <- add_edge(wf, prev, nm)
    prev <- nm
  }
  wf
}

# ---- internal helpers -------------------------------------------------------

#' @keywords internal
#' @noRd
.wf_as_checkpoint <- function(x) {
  if (inherits(x, "llmragent_wf_checkpoint")) return(x)
  if (inherits(x, "agent_workflow_run") && !is.null(x$checkpoint)) return(x$checkpoint)
  stop("No workflow checkpoint found; pass a paused run or its $checkpoint.", call. = FALSE)
}
#' @keywords internal
#' @noRd
.wf_dir_of <- function(x) {
  if (is.character(x)) return(x)
  if (inherits(x, "agent_workflow_run")) return(x$checkpoint_dir)
  if (inherits(x, "llmragent_wf_checkpoint")) return(x$dir)
  stop("Cannot find a checkpoint directory for this object.", call. = FALSE)
}
#' @keywords internal
#' @noRd
.wf_read_log <- function(dir) {
  f <- file.path(dir, "run.jsonl")
  if (!file.exists(f)) return(tibble::tibble(node = character(0), status = character(0), state_hash = character(0)))
  lines <- readLines(f, warn = FALSE); lines <- lines[nzchar(trimws(lines))]
  recs <- lapply(lines, function(l) tryCatch(jsonlite::fromJSON(l), error = function(e) NULL))
  recs <- Filter(Negate(is.null), recs)
  if (!length(recs)) return(tibble::tibble(node = character(0), status = character(0), state_hash = character(0)))
  do.call(rbind, lapply(recs, function(r) tibble::tibble(
    node = r$node %||% NA_character_, status = r$status %||% NA_character_,
    state_hash = r$state_hash %||% NA_character_, step = r$step %||% NA_integer_)))
}
#' @keywords internal
#' @noRd
.wf_resolve_node <- function(wf, node_name) {
  if (node_name %in% names(wf$nodes)) return(node_name)
  if (identical(node_name, "entry")) return(wf$entry)
  node_name
}
