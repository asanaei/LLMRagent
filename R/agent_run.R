# agent_run.R -----------------------------------------------------------------
# The unified run object. Every high-level result (a chat agent, a conversation,
# a preset, a pipeline, an experiment, a fanout synthesis result) converts to an
# `agent_run` via as_agent_run(). The substrate (span records) is captured
# eagerly and cheaply during a run; the run object and its tidy views are
# projected on demand, so the trivial agent$chat("hi") path allocates nothing
# extra. One accessor, as_tibble(run, level=), exposes the run at five grains
# with stable columns.

#' @importFrom tibble as_tibble
NULL

# ---- construction -----------------------------------------------------------

# Build an agent_run from a normalized provenance list (run_id/kind/design/
# spans/participants/...). Internal; users call as_agent_run().
#' @keywords internal
#' @noRd
.new_agent_run <- function(prov) {
  structure(
    list(
      run_id       = prov$run_id %||% .llmragent_id("run"),
      kind         = prov$kind %||% "run",
      design       = prov$design %||% list(),
      spans        = prov$spans %||% list(),
      participants = prov$participants %||% .empty_participants(),
      utterances   = prov$utterances %||% NULL,   # kind-specific transcript rows, when supplied
      artifacts    = prov$artifacts %||% list(),
      agents       = prov$agents %||% list(),
      # Data-only record of the agents' tools, generation parameters, and
      # memory when the run closed; NULL for a live view of a bare Agent.
      snapshot     = prov$snapshot %||% NULL,
      created_at   = prov$created_at %||% format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
      llmr_log     = prov$llmr_log %||% NULL,
      claim_type   = prov$claim_type %||% NA_character_,
      pkg_versions = prov$pkg_versions %||% .llmragent_pkg_versions()
    ),
    class = "agent_run"
  )
}

.empty_participants <- function() {
  tibble::tibble(agent_id = character(0), name = character(0),
                 provider = character(0), model = character(0),
                 persona_hash = character(0))
}

# ---- the generic ------------------------------------------------------------

#' Convert an LLMRagent result to a unified run object
#'
#' `as_agent_run()` turns any high-level result into an `agent_run`: a single
#' object that exposes a run at five levels through one tidy accessor,
#' `tibble::as_tibble(run, level = c("utterance","event","call","tool","state"))`,
#' and that backs [agent_manifest()], [archive_agent_study()],
#' [diagnostics()], and [report()].
#'
#' Provenance is captured during every model call, so
#' converting costs nothing during the run; the views are built on demand. For a
#' bare [Agent], the result is a live view of the agent's session so far. A
#' completed conversation, preset, or pipeline records its agents' tools,
#' generation parameters, and memory as they were when it closed, so reusing or
#' changing those agents later does not change its manifest or state view.
#' Workflow runs are not converted; they keep their own checkpoint and replay
#' record (see [run_workflow()]).
#'
#' The call level holds the stored [LLMR::llm_response_record()] rows. A tool
#' exchange takes several model rounds but stores one record, for its final
#' response, and memory compactions and embedding operations appear only as
#' events. Records written under different LLMR versions are bound by column
#' name; a column absent from some records is filled with missing values of
#' the type the other records use. A call event whose record could not be
#' built (status `"unrecorded"`) makes the call level an error that names the
#' event, while the other levels remain available.
#'
#' @param x An [Agent]; a conversation, debate, focus group, interview, or
#'   deliberation; an `agent_pipeline_run`; an `agent_fanout_result`; an
#'   `agent_experiment`; or an `agent_run`.
#' @param ... Unused. `tibble::as_tibble()` also accepts the level name as
#'   its second, unnamed argument.
#' @return An object of class `agent_run`; `tibble::as_tibble()` returns the
#'   requested level as a tibble.
#' @aliases agent_run
#' @seealso [agent_manifest()], [archive_agent_study()], [diagnostics()],
#'   [report()]
#' @export
as_agent_run <- function(x, ...) UseMethod("as_agent_run")

#' @export
as_agent_run.agent_run <- function(x, ...) x

#' @export
as_agent_run.default <- function(x, ...) {
  cls <- paste(class(x), collapse = ", ")
  stop(sprintf(paste0(
    "Cannot convert an object of class <%s> to an agent_run. Pass an Agent, a ",
    "conversation/preset result, or a pipeline/experiment/fanout result."),
    cls), call. = FALSE)
}

#' @export
as_agent_run.Agent <- function(x, ...) {
  spans <- x$internal_spans()
  # stamp a synthetic run id onto the live view (the agent is not inside a run)
  rid <- .llmragent_id("run")
  spans <- lapply(spans, function(s) { if (is.na(s$run_id %||% NA_character_)) s$run_id <- rid; s$agent_id <- s$agent_id %||% x$id(); s })
  participants <- tibble::tibble(
    agent_id = x$id(), name = x$name,
    provider = x$config$provider %||% NA_character_,
    model = x$config$model %||% NA_character_,
    persona_hash = .agent_persona_hash(x))
  utt <- .utterances_from_transcript(x$transcript(), speaker = x$name, rid = rid)
  .new_agent_run(list(
    run_id = rid, kind = "chat", design = list(),
    spans = spans, participants = participants, utterances = utt,
    agents = list(x), llmr_log = .llmragent_active_log()))
}

# Classed multi-agent results carry a $provenance handle (from .run_close).
# A shared worker turns it into an agent_run, optionally attaching kind-specific
# transcript rows and artifacts.
#' @keywords internal
#' @noRd
.run_from_provenance <- function(prov, utterances = NULL, artifacts = list(),
                                 design = NULL) {
  prov$utterances <- utterances
  prov$artifacts <- artifacts
  if (!is.null(design)) prov$design <- design
  .new_agent_run(prov)
}

# ---- the tidy accessor ------------------------------------------------------

#' @rdname as_agent_run
#' @param level The grain to return: `"utterance"` (analysis grain),
#'   `"event"` (every span), `"call"` (stored [LLMR::llm_response_record()]
#'   rows), `"tool"` (tool invocations with arg/result hashes), or
#'   `"state"` (each participating agent's memory at run end).
#' @exportS3Method tibble::as_tibble agent_run
as_tibble.agent_run <- function(x, ..., level = c("utterance","event","call","tool","state")) {
  valid <- c("utterance","event","call","tool","state")
  # The tibble::as_tibble() generic puts everything after `x` into `...`, so a
  # positional level (as_tibble(run, "call")) lands in `...`, not `level`.
  # Accept it positionally for ergonomics: if `level` was not named and the
  # first unnamed dot is a valid level, use it.
  if (missing(level)) {
    dots <- list(...)
    nm <- names(dots)
    cand <- dots[if (is.null(nm)) seq_along(dots) else which(!nzchar(nm))]
    hit <- Find(function(v) is.character(v) && length(v) == 1L && v %in% valid, cand)
    if (!is.null(hit)) level <- hit
  }
  level <- match.arg(level)
  switch(level,
    event     = .run_events(x),
    call      = .run_calls(x),
    tool      = .run_tools(x),
    state     = .run_state(x),
    utterance = .run_utterances(x))
}

# event level: the full span store as a flat tibble.
.run_events <- function(x) {
  cols <- function(s, k, default) { v <- s[[k]]; if (is.null(v) || length(v) != 1L) default else v }
  if (!length(x$spans)) {
    return(tibble::tibble(
      run_id = character(0), span_id = character(0), parent_id = character(0),
      event_type = character(0), status = character(0),
      started_at = as.POSIXct(character(0)), ended_at = as.POSIXct(character(0)),
      duration_s = numeric(0), tokens_sent = integer(0), tokens_received = integer(0),
      tool = character(0), agent_id = character(0), request_hash = character(0),
      response_id = character(0), note = character(0)))
  }
  do.call(rbind, lapply(x$spans, function(s) tibble::tibble(
    run_id = s$run_id %||% x$run_id,
    span_id = cols(s, "span_id", NA_character_),
    parent_id = cols(s, "parent_id", NA_character_),
    event_type = cols(s, "event_type", NA_character_),
    status = cols(s, "status", NA_character_),
    started_at = s$started_at %||% as.POSIXct(NA),
    ended_at = s$ended_at %||% as.POSIXct(NA),
    duration_s = cols(s, "duration_s", NA_real_),
    tokens_sent = cols(s, "tokens_sent", NA_integer_),
    tokens_received = cols(s, "tokens_received", NA_integer_),
    tool = cols(s, "tool", NA_character_),
    agent_id = cols(s, "agent_id", NA_character_),
    request_hash = cols(s, "request_hash", NA_character_),
    response_id = cols(s, "response_id", NA_character_),
    note = cols(s, "note", NA_character_))))
}

# call level: the cached llm_response_record rows (16 cols) + run/span/agent ids.
.run_calls <- function(x) {
  recs <- list()
  for (s in x$spans) {
    if (identical(s$event_type, "call")) {
      rr <- s$meta$record
      if (!.usable_record(rr)) {
        stop(sprintf("Call span '%s' has no readable response record.%s",
                     s$span_id %||% "unknown",
                     if (!is.null(s$meta$record_error)) paste0(" ", s$meta$record_error) else ""),
             call. = FALSE)
      }
      rr$run_id <- s$run_id %||% x$run_id
      rr$span_id <- s$span_id
      rr$agent_id <- s$agent_id
      recs[[length(recs) + 1L]] <- rr
    }
  }
  if (!length(recs)) {
    base <- LLMR::llm_response_record(simpleError("none"))[0, , drop = FALSE]
    base$run_id <- character(0); base$span_id <- character(0); base$agent_id <- character(0)
    return(base)
  }
  .bind_records(recs)
}

# Bind one-row records by column name. Records written by different LLMR
# versions can lack a column; it is filled with a missing value of the type the
# other records use, and only columns present in some record enter the result.
# Columns whose types cannot be combined (character with integer, say) are an
# error rather than a silent coercion.
.bind_records <- function(recs) {
  cols <- unique(unlist(lapply(recs, names), use.names = FALSE))
  family <- function(v) {
    if (is.logical(v) && all(is.na(v))) return(NA_character_)
    if (inherits(v, c("POSIXt", "Date", "difftime"))) return(class(v)[1])
    if (is.factor(v) || is.character(v)) return("character")
    if (is.logical(v) || is.integer(v) || is.double(v)) return("numeric")
    class(v)[1]
  }
  for (cn in cols) {
    present <- Filter(Negate(is.null), lapply(recs, function(r) r[[cn]]))
    fams <- unique(stats::na.omit(vapply(present, family, character(1))))
    if (length(fams) > 1L) {
      stop(sprintf("Call records store column '%s' with incompatible types (%s).",
                   cn, paste(fams, collapse = ", ")), call. = FALSE)
    }
    typed <- Filter(function(v) !is.na(family(v)), present)
    proto <- if (length(typed)) typed[[1]] else present[[1]]
    for (i in seq_along(recs)) {
      if (is.null(recs[[i]][[cn]])) {
        recs[[i]][[cn]] <- proto[NA_integer_][rep(1L, nrow(recs[[i]]))]
      }
    }
  }
  out <- do.call(rbind, lapply(recs, function(r) r[, cols, drop = FALSE]))
  rownames(out) <- NULL
  tibble::as_tibble(out)
}

.usable_record <- function(x) {
  is.data.frame(x) && nrow(x) == 1L && "success" %in% names(x) &&
    is.logical(x$success)
}

# tool level: tool spans expanded with governance metadata.
.run_tools <- function(x) {
  rows <- list()
  for (s in x$spans) {
    if (!identical(s$event_type %||% "", "tool")) next
    m <- s$meta %||% list()
    rows[[length(rows) + 1L]] <- tibble::tibble(
      run_id = s$run_id %||% x$run_id, span_id = s$span_id, agent_id = s$agent_id,
      round = m$round %||% NA_integer_, name = s$tool %||% NA_character_,
      arguments_hash = m$arguments_hash %||% NA_character_,
      result_hash = m$result_hash %||% NA_character_,
      arguments = m$arguments %||% NA_character_,
      result = m$result %||% s$note %||% NA_character_,
      status = s$status %||% NA_character_,
      duration_s = s$duration_s %||% NA_real_,
      ts = s$started_at %||% as.POSIXct(NA))
  }
  if (!length(rows)) {
    return(tibble::tibble(
      run_id = character(0), span_id = character(0), agent_id = character(0),
      round = integer(0), name = character(0), arguments_hash = character(0),
      result_hash = character(0), arguments = character(0), result = character(0),
      status = character(0), duration_s = numeric(0), ts = as.POSIXct(character(0))))
  }
  do.call(rbind, rows)
}

# state level: each participating agent's memory transcript at run end. A run
# that closed carries that memory in its snapshot, so reusing an agent later
# cannot change it; a live view of a bare Agent reads the agent's memory now.
.run_state <- function(x) {
  st <- x$snapshot$state
  if (is.data.frame(st)) {
    st$run_id <- rep(x$run_id, nrow(st))
    return(st)
  }
  .state_of_agents(x$agents %||% list(), x$run_id)
}

# Internal: the memory transcripts of `agents`, one row per message.
.state_of_agents <- function(agents, run_id) {
  rows <- list()
  for (a in agents) {
    tr <- tryCatch(a$transcript(), error = function(e) NULL)
    if (is.null(tr) || !nrow(tr)) next
    for (i in seq_len(nrow(tr))) {
      rows[[length(rows) + 1L]] <- tibble::tibble(
        run_id = run_id, agent_id = a$id(), name = a$name, role = "memory",
        message_index = i, msg_role = tr$role[i], content = tr$content[i])
    }
  }
  if (!length(rows)) {
    return(tibble::tibble(
      run_id = character(0), agent_id = character(0), name = character(0),
      role = character(0), message_index = integer(0), msg_role = character(0),
      content = character(0)))
  }
  do.call(rbind, rows)
}

# utterance level: kind-specific transcript rows unioned to a stable schema.
.run_utterances <- function(x) {
  empty <- tibble::tibble(
    run_id = character(0), turn = integer(0), speaker = character(0),
    role = character(0), text = character(0), phase = character(0),
    question_id = integer(0), call_id = character(0), ts = as.POSIXct(character(0)))
  u <- x$utterances
  if (is.null(u) || !nrow(u)) return(empty)
  # fill any missing stable columns with typed NA, keep column order
  for (nm in names(empty)) if (!nm %in% names(u)) u[[nm]] <- empty[[nm]][NA_integer_][seq_len(nrow(u))]
  u$run_id <- x$run_id
  tibble::as_tibble(u[, names(empty), drop = FALSE])
}

# Build utterance rows from a (role, content) memory transcript (chat agent).
#' @keywords internal
#' @noRd
.utterances_from_transcript <- function(tr, speaker, rid) {
  if (is.null(tr) || !nrow(tr)) return(NULL)
  tibble::tibble(
    run_id = rid, turn = seq_len(nrow(tr)),
    speaker = ifelse(tr$role == "assistant", speaker, tr$role),
    role = tr$role, text = tr$content,
    phase = NA_character_, question_id = NA_integer_,
    call_id = NA_character_, ts = as.POSIXct(NA))
}

# Build utterance rows from a conversation/preset transcript tibble (which has
# turn/speaker/text plus maybe phase/round/question_id). Speaker turns are
# "user" unless they are an agent participant, but for a shared transcript we
# label by structural role: every recorded turn is content authored by a named
# speaker, so role = "assistant" for participants and "user"/"system" otherwise
# is not meaningful here; we record role = "speaker".
#' @keywords internal
#' @noRd
.utterances_from_dialogue <- function(transcript, rid) {
  if (is.null(transcript) || !nrow(transcript)) return(NULL)
  phase <- if ("phase" %in% names(transcript)) as.character(transcript$phase)
           else if ("round" %in% names(transcript)) paste0("round_", transcript$round)
           else NA_character_
  qid <- if ("question_id" %in% names(transcript)) as.integer(transcript$question_id) else NA_integer_
  tibble::tibble(
    run_id = rid,
    turn = if ("turn" %in% names(transcript)) as.integer(transcript$turn) else seq_len(nrow(transcript)),
    speaker = as.character(transcript$speaker),
    role = "speaker",
    text = as.character(transcript$text),
    phase = phase, question_id = qid,
    call_id = NA_character_, ts = as.POSIXct(NA))
}

# ---- print ------------------------------------------------------------------

#' @export
print.agent_run <- function(x, ...) {
  n_calls <- length(Filter(function(s) identical(s$event_type %||% "", "call"), x$spans))
  n_tools <- length(Filter(function(s) identical(s$event_type %||% "", "tool"), x$spans))
  cat(sprintf("<agent_run %s | kind=%s | %d agent(s) | %d call(s), %d tool call(s)>\n",
              x$run_id, x$kind, nrow(x$participants), n_calls, n_tools))
  if (nrow(x$participants)) {
    cat("  agents: ", paste(x$participants$name, collapse = ", "), "\n", sep = "")
  }
  if (!is.na(x$claim_type)) cat("  claim type: ", x$claim_type, "\n", sep = "")
  cat("  levels: as_tibble(run, 'utterance'|'event'|'call'|'tool'|'state')\n")
  invisible(x)
}
