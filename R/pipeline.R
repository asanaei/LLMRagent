# pipeline.R --------------------------------------------------------------------
# Sequential handoffs: each agent transforms the previous agent's output. The
# division of labor lives in the personas; the pipeline just moves the text.

#' Run input through a chain of agents
#'
#' Each agent receives the previous agent's output as its message (the first
#' receives `input`), transforms it according to its persona, and hands the
#' result on. A common use is a fixed sequence of narrow specialists:
#' extract, then translate, then critique. Each stage is easy to inspect,
#' test, and swap.
#'
#' Stages are stateless (`reply()`), so the same agents can serve in several
#' pipelines, and a pipeline can run many inputs without cross-contamination.
#' Every intermediate product is kept: the returned `steps` tibble has one
#' row per stage with the exact input and output of each agent.
#'
#' @param agents A list of [Agent]s, in order. A single agent is allowed.
#' @param input The text handed to the first agent.
#' @param quiet If FALSE (default), each stage's output prints as it arrives.
#' @param ... Passed to each agent's underlying LLMR call.
#' @return An object of class `agent_pipeline_run`: a list with `steps`
#'   (tibble: `step`, `agent`, `input`, `output`) and `output` (the final
#'   text). `as.data.frame()` returns the steps.
#' @examples
#' \dontrun{
#' cfg <- LLMR::llm_config("groq", "openai/gpt-oss-20b", temperature = 0.3)
#'
#' run <- agent_pipeline(
#'   list(
#'     agent("Extractor", cfg, persona =
#'       "Extract every factual claim as a numbered list. Nothing else."),
#'     agent("Checker", cfg, persona =
#'       "For each numbered claim, mark VERIFIABLE or VAGUE, one line each."),
#'     agent("Editor", cfg, persona =
#'       "Rewrite the original message keeping only VERIFIABLE claims.")
#'   ),
#'   input = "Our app doubled retention, won three awards, and users love it."
#' )
#' run$output      # the final text
#' run$steps       # every intermediate product
#' }
#' @seealso [agent_as_tool()] for model-directed (rather than fixed) routing.
#' @export
agent_pipeline <- function(agents, input, quiet = FALSE, ...) {
  if (inherits(agents, "Agent")) agents <- list(agents)
  stopifnot(is.list(agents), length(agents) >= 1L)
  for (a in agents) {
    if (!inherits(a, "Agent")) stop("`agents` must be Agent objects.", call. = FALSE)
  }
  rc <- .run_open("pipeline",
                  design = list(stage_order = vapply(agents, function(a) a$name, character(1))),
                  agents = agents)
  on.exit(for (a in agents) a$bind_run(NULL), add = TRUE)
  current <- as.character(input)[1]
  rows <- vector("list", length(agents))
  for (i in seq_along(agents)) {
    a <- agents[[i]]
    out <- a$reply(current, ...)
    rows[[i]] <- tibble::tibble(step = i, agent = a$name,
                                input = current, output = out)
    if (!quiet) {
      cli::cli_text("{.strong [{i}/{length(agents)}] {a$name}}: {out}")
    }
    current <- out
  }
  structure(list(steps = do.call(rbind, rows), output = current,
                 provenance = .run_close(rc)),
            class = "agent_pipeline_run")
}

#' @exportS3Method as_agent_run agent_pipeline_run
as_agent_run.agent_pipeline_run <- function(x, ...) {
  prov <- x$provenance
  st <- x$steps
  utt <- tibble::tibble(
    run_id = prov$run_id, turn = st$step, speaker = st$agent,
    role = "speaker", text = st$output, phase = NA_character_,
    question_id = NA_integer_, call_id = NA_character_, ts = as.POSIXct(NA))
  .run_from_provenance(prov, utterances = utt, artifacts = list(steps = st))
}

#' @export
print.agent_pipeline_run <- function(x, ...) {
  cat(sprintf("<agent_pipeline_run | %d stage(s): %s>\n",
              nrow(x$steps), paste(x$steps$agent, collapse = " -> ")))
  cat(x$output, "\n")
  invisible(x)
}

#' @export
as.data.frame.agent_pipeline_run <- function(x, ...) {
  as.data.frame(x$steps, ...)
}
