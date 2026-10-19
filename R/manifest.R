# manifest.R ------------------------------------------------------------------
# The study manifest hashes a run's declared specification: design, personas,
# tool specifications, orchestration metadata, model ids, parameters, and
# package versions. It records those declared inputs rather than sampled
# replies. The hash does not identify unrecorded ambient state.

#' Hash a persona
#'
#' A stable identity for a persona prompt, reusing LLMR's content-hash
#' convention. Two agents with the same brief and name hash identically; any
#' wording change flips the hash.
#'
#' @param persona Character scalar (the persona brief) or a [persona_frame()].
#' @param name Optional agent name folded into the hash (two agents with the
#'   same brief but different display names are arguably different personas).
#' @return A 64-character SHA-256 hex string (see [LLMR::llm_hash()]).
#' @seealso [persona_frame()], [agent_manifest()]
#' @export
hash_persona <- function(persona, name = NULL) {
  if (inherits(persona, "persona_frame")) {
    return(LLMR::llm_hash(list(
      text = persona$text %||% "",
      attributes = persona$attributes,
      source = persona$source,
      name = name)))
  }
  LLMR::llm_hash(list(text = as.character(persona %||% ""), name = name))
}

#' Hash a tool's declared specification
#'
#' Hashes a tool's name, description, argument schema, governance policy (side
#' effects, approval, limits), and R function body. Captured values in the
#' function's enclosing environment are omitted, so this identifies the
#' declared specification rather than the complete executable apparatus.
#'
#' @param tool An [LLMR::llm_tool()] or a governed [agent_tool()].
#' @return A 64-character SHA-256 hex string.
#' @seealso [agent_tool()], [agent_manifest()]
#' @export
hash_tool_spec <- function(tool) {
  gov <- tool$governance %||% attr(tool, "governance")
  # Hash the RAW user function (deparsed), not the governed wrapper: the wrapper
  # closes over a mutable call-counter environment, so hashing it would make a
  # tool's identity change after it runs. The raw function is the actual
  # instrument; the policy fields below capture the governance.
  fn_id <- if (!is.null(gov) && is.function(gov$raw_fn)) gov$raw_fn else tool$fn
  LLMR::llm_hash(list(
    name        = tool$name %||% NA_character_,
    description = tool$description %||% NA_character_,
    schema      = tool$schema,
    fn          = fn_id,
    side_effects     = if (!is.null(gov)) gov$side_effects else NULL,
    requires_approval = if (!is.null(gov)) gov$requires_approval else NULL,
    timeout_s   = if (!is.null(gov)) gov$timeout_s else NULL,
    max_calls   = if (!is.null(gov)) gov$max_calls else NULL,
    max_bytes   = if (!is.null(gov)) gov$max_bytes else NULL))
}

#' Build the study manifest for a run
#'
#' One object tying together the recorded design, personas, declared tool
#' specifications, orchestration metadata, served model identifiers, generation
#' parameters, and package versions. Its `manifest_hash` changes when a hashed
#' component changes. It does not hash transcripts, replies, or unrecorded
#' ambient state such as values captured by a tool closure.
#'
#' The manifest also reports, outside the hash, how many calls the run made
#' and the tokens they reported. `n_unknown_tokens` counts the calls, failed
#' ones included, whose sent, received, or total count is missing, as
#' [LLMR::llm_usage()] does. The model list is read from the call records
#' when there are any (`models_basis = "calls"`) and from the participating
#' agents otherwise (`"participants"`); since the model list is hashed, the
#' two bases can give different hashes for otherwise identical runs. When the
#' call records cannot be read, the manifest warns, reports the call count and
#' token sums as `NA`, and takes the model list from the participants. An
#' experiment's design records the number of cells that contributed events
#' (`n_converted`); that count describes the outcome rather than the
#' specification and is not hashed.
#'
#' @param run An object accepted by [as_agent_run()] (a chat agent, a
#'   conversation, a preset, a pipeline, an experiment, an
#'   `agent_fanout_result`, or an `agent_run`).
#' @return An object of class `agent_manifest`: a list with `manifest_hash`,
#'   `kind`, `run_id`, `design`, `personas`, `tools`, `workflow`, `models`,
#'   `params`, `pkg_versions`, `created_at`, `n_calls`, `tokens_sent`,
#'   `tokens_received`, `models_basis`, and `n_unknown_tokens`. Print shows
#'   the short hash, kind, models, and headline counts.
#' @seealso [hash_persona()], [hash_tool_spec()], [archive_agent_study()],
#'   [LLMR::llm_hash()]
#' @export
agent_manifest <- function(run) {
  r <- as_agent_run(run)

  extracted <- .read_run_level(r, "call", warn = FALSE)
  if (!is.null(extracted$error)) warning(
    "Call records could not be read. Call counts and token summaries are NA; the model list uses participants. ",
    extracted$error, call. = FALSE)
  .agent_manifest(r, extracted)
}

.agent_manifest <- function(r, extracted) {
  calls <- extracted$data
  models <- if (!is.null(calls) && nrow(calls)) {
    cols <- intersect(c("provider", "model", "model_version"), names(calls))
    tibble::as_tibble(unique(as.data.frame(calls[, cols, drop = FALSE], stringsAsFactors = FALSE)))
  } else {
    cols <- intersect(c("provider", "model"), names(r$participants))
    tibble::as_tibble(unique(as.data.frame(r$participants[, cols, drop = FALSE], stringsAsFactors = FALSE)))
  }

  tools_tbl <- .manifest_tools(r)
  params_tbl <- .manifest_params(r)

  # The hash covers the run's specification. An experiment's design also
  # reports how many cells contributed events (`n_converted`); that count is an
  # outcome, so it stays out of the hash, which keeps an experiment's hash equal
  # to the one computed before the count was recorded.
  spec_design <- r$design
  if (is.list(spec_design)) spec_design$n_converted <- NULL
  body <- list(
    kind         = r$kind,
    design       = spec_design,
    personas     = r$participants[, intersect(c("name", "persona_hash"), names(r$participants)), drop = FALSE],
    tools        = tools_tbl,
    workflow     = list(kind = r$kind, design = spec_design),
    models       = models,
    params       = params_tbl,
    pkg_versions = r$pkg_versions
  )
  out <- list(
    manifest_hash = LLMR::llm_hash(body),
    kind          = r$kind,
    run_id        = r$run_id,
    design        = r$design,
    personas      = body$personas,
    tools         = tools_tbl,
    workflow      = body$workflow,
    models        = models,
    params        = params_tbl,
    pkg_versions  = r$pkg_versions,
    created_at    = r$created_at,
    n_calls       = if (!is.null(calls)) nrow(calls) else NA_integer_,
    tokens_sent   = if (!is.null(calls) && "sent_tokens" %in% names(calls)) sum(calls$sent_tokens, na.rm = TRUE) else NA_integer_,
    tokens_received = if (!is.null(calls) && "rec_tokens" %in% names(calls)) sum(calls$rec_tokens, na.rm = TRUE) else NA_integer_,
    models_basis = if (!is.null(calls) && nrow(calls)) "calls" else "participants",
    n_unknown_tokens = if (!is.null(calls)) .unknown_tokens(calls) else NA_integer_
  )
  structure(out, class = "agent_manifest")
}

# Internal: collect the participating agents' tools into a hashed tibble. A
# closed run reports the tools its agents had when it closed (its snapshot), so
# a tool added later does not enter an earlier run's manifest.
#' @keywords internal
#' @noRd
.manifest_tools <- function(r) {
  if (is.data.frame(r$snapshot$tools)) return(r$snapshot$tools)
  .tools_of_agents(r$agents %||% list())
}

#' @keywords internal
#' @noRd
.tools_of_agents <- function(agents) {
  rows <- list()
  for (a in agents) {
    tls <- tryCatch(a$tools, error = function(e) list())
    for (t in tls) {
      rows[[length(rows) + 1L]] <- tibble::tibble(
        agent_id = a$id(), tool_name = t$name %||% NA_character_,
        tool_spec_hash = hash_tool_spec(t))
    }
  }
  if (!length(rows)) {
    return(tibble::tibble(agent_id = character(0), tool_name = character(0),
                          tool_spec_hash = character(0)))
  }
  do.call(rbind, rows)
}

# Internal: answer-relevant generation parameters per agent, as they were when
# a closed run closed (its snapshot), else as they are now.
#' @keywords internal
#' @noRd
.manifest_params <- function(r) {
  if (is.data.frame(r$snapshot$params)) return(r$snapshot$params)
  .params_of_agents(r$agents %||% list())
}

#' @keywords internal
#' @noRd
.params_of_agents <- function(agents) {
  rows <- list()
  for (a in agents) {
    mp <- tryCatch(a$config$model_params, error = function(e) NULL) %||% list()
    keep <- mp[intersect(names(mp), c("temperature", "top_p", "top_k", "max_tokens",
                                      "seed", "frequency_penalty", "presence_penalty"))]
    for (nm in names(keep)) {
      rows[[length(rows) + 1L]] <- tibble::tibble(
        agent_id = a$id(), param = nm,
        value = as.character(keep[[nm]])[1])
    }
  }
  if (!length(rows)) {
    return(tibble::tibble(agent_id = character(0), param = character(0),
                          value = character(0)))
  }
  do.call(rbind, rows)
}

#' @rdname agent_manifest
#' @param x An `agent_manifest`.
#' @param ... Unused.
#' @export
print.agent_manifest <- function(x, ...) {
  cat(sprintf("<agent_manifest %s | %s | %s call(s)>\n",
              substr(x$manifest_hash, 1L, 12L), x$kind %||% "?",
              format(x$n_calls %||% 0L)))
  if (nrow(x$models)) {
    mv <- if ("model_version" %in% names(x$models)) x$models$model_version else x$models$model
    cat("  models:  ", paste(unique(stats::na.omit(mv)), collapse = ", "),
        if (!is.null(x$models_basis)) paste0(" (from ", x$models_basis, ")"),
        "\n", sep = "")
  }
  cat(sprintf("  personas: %d | tools: %d | params: %d\n",
              nrow(x$personas), nrow(x$tools), nrow(x$params)))
  cat("  created: ", x$created_at %||% "?", "\n", sep = "")
  invisible(x)
}
