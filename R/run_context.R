# run_context.R --------------------------------------------------------------
# Identity minting and the lightweight "run context" that ties several
# independent R6 agents' span stores into one run's event graph. A run is
# opened before a multi-agent procedure (a conversation, a preset), binds each
# participating agent so its spans are stamped with the run id, and is closed at
# the end to consolidate the events. The classed result keeps the handle; the
# run object is materialized on demand by as_agent_run().

# A process-local monotone counter, so ids are unique within a session without a
# uuid dependency. The time component keeps them sortable and unique across
# sessions. (R, not a workflow script: Sys.time() is available here.)
.llmragent_id_counter <- local({
  n <- 0L
  function() { n <<- n + 1L; n }
})

#' Mint a short, unique id with a kind prefix
#' @keywords internal
#' @noRd
.llmragent_id <- function(prefix = "id") {
  ts <- format(Sys.time(), "%Y%m%d%H%M%S")
  sprintf("%s-%s-%06d", prefix, ts, .llmragent_id_counter())
}

# The active LLMR audit-log path, or NULL when logging is off. Uses LLMR's
# public accessor (llm_log_active) rather than reaching the private option.
#' @keywords internal
#' @noRd
.llmragent_active_log <- function() {
  st <- tryCatch(LLMR::llm_log_active(), error = function(e) NULL)
  if (is.null(st) || !isTRUE(st$active)) return(NULL)
  st$path
}

#' Open a run context
#'
#' Mints a run id, ensures each participating agent has a stable id, and binds
#' each agent so its subsequently-created spans carry this run id. Returns a
#' handle (a small environment) the closing call and the materializer read.
#' @keywords internal
#' @noRd
.run_open <- function(kind, design = list(), agents = list()) {
  if (inherits(agents, "Agent")) agents <- list(agents)
  # Pull in specialists reachable through delegate-tools so their consultation
  # spans land in the same run. De-duplicate by agent id (an agent may appear as
  # both a participant and a delegate).
  delegates <- unlist(lapply(agents, .delegate_agents), recursive = FALSE)
  all_agents <- c(agents, delegates)
  if (length(all_agents)) {
    ids <- vapply(all_agents, function(a) a$id(), character(1))
    all_agents <- all_agents[!duplicated(ids)]
  }
  rc <- new.env(parent = emptyenv())
  rc$run_id <- .llmragent_id("run")
  rc$kind <- kind
  rc$design <- design
  rc$agents <- all_agents
  rc$created_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  rc$llmr_log <- .llmragent_active_log()
  # Record each agent's pre-run span high-water mark so .run_close gathers only
  # spans created during this run, and bind the agent to the run.
  rc$marks <- vapply(all_agents, function(a) length(a$internal_spans()), integer(1))
  for (a in all_agents) a$bind_run(rc$run_id)
  rc
}

#' Close a run context: unbind agents and return consolidated provenance
#'
#' Returns a list carrying the run id, kind, design, the participants tibble,
#' and the consolidated events tibble (the spans created during the run across
#' all participating agents). This list is what a classed result stores in
#' `$provenance` and what `.run_materialize()` turns into an `agent_run`.
#' @keywords internal
#' @noRd
.run_close <- function(rc) {
  agents <- rc$agents
  for (a in agents) a$bind_run(NULL)

  ev_rows <- list()
  for (i in seq_along(agents)) {
    a <- agents[[i]]
    sp <- a$internal_spans()
    from <- rc$marks[[i]] + 1L
    if (from <= length(sp)) {
      run_sp <- sp[from:length(sp)]
      # only spans actually stamped with this run id (defensive)
      run_sp <- Filter(function(s) identical(s$run_id %||% NA_character_, rc$run_id), run_sp)
      ev_rows <- c(ev_rows, run_sp)
    }
  }

  participants <- if (length(agents)) {
    do.call(rbind, lapply(agents, function(a) tibble::tibble(
      agent_id = a$id(),
      name     = a$name,
      provider = a$config$provider %||% NA_character_,
      model    = a$config$model %||% NA_character_,
      persona_hash = .agent_persona_hash(a))))
  } else {
    tibble::tibble(agent_id = character(0), name = character(0),
                   provider = character(0), model = character(0),
                   persona_hash = character(0))
  }

  list(
    run_id = rc$run_id,
    kind = rc$kind,
    design = rc$design,
    created_at = rc$created_at,
    llmr_log = rc$llmr_log,
    participants = participants,
    spans = ev_rows,
    agents = agents,
    pkg_versions = .llmragent_pkg_versions()
  )
}

# Internal: a persona hash for an agent, reading its persona_frame when present
# (via the public accessor), else hashing the plain string.
#' @keywords internal
#' @noRd
.agent_persona_hash <- function(a) {
  pf <- tryCatch(a$persona_frame(), error = function(e) NULL)
  if (!is.null(pf)) return(pf$hash %||% hash_persona(pf$text, a$name))
  hash_persona(a$persona %||% "", a$name)
}

#' @keywords internal
#' @noRd
.llmragent_pkg_versions <- function() {
  list(
    LLMRagent = as.character(utils::packageVersion("LLMRagent")),
    LLMR      = as.character(utils::packageVersion("LLMR")),
    R         = as.character(getRversion())
  )
}
