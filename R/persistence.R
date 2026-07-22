# persistence.R ----------------------------------------------------------------
# Save and restore agents. The config stores its API key as an environment
# reference (LLMR's secret handle), never the key itself, so a saved agent is
# safe to share and fully functional on any machine with the variable set.

#' Save an agent to disk
#'
#' Writes the agent's name, persona, config, memory contents, guardrails, and
#' trace to an RDS file. Tools are functions and are not serialized; re-attach
#' them at load time via `load_agent(tools = ...)`. Guardrails are serialized
#' (their check functions travel through RDS) and restored automatically.
#'
#' A config carrying a literal API key (one passed as a string rather than
#' the usual environment-variable reference) is refused: saving it would
#' write the key to disk. Rebuild the config from an environment variable.
#'
#' @param x An [Agent].
#' @param path File path (`.rds`).
#' @return `path`, invisibly.
#' @examples
#' \dontrun{
#' cfg <- LLMR::llm_config("groq", "openai/gpt-oss-20b")
#' a <- agent("Ada", cfg, persona = "Terse.")
#' a$chat("Remember this: the project deadline is March 3.")
#' save_agent(a, "ada.rds")
#'
#' # a later session, any machine with GROQ_API_KEY set:
#' ada <- load_agent("ada.rds")
#' ada$chat("When is the deadline?")   # the memory came along
#' }
#' @seealso [load_agent()]
#' @export
save_agent <- function(x, path) {
  stopifnot(inherits(x, "Agent"))
  if (inherits(x$config$api_key, "llmr_secret_literal")) {
    stop("This agent's config carries a literal API key; saving would write ",
         "the key to disk. Rebuild the config from an environment variable ",
         "(the default, or LLMR::llm_api_key_env()) and save again.",
         call. = FALSE)
  }
  state <- list(
    llmragent_version = as.character(utils::packageVersion("LLMRagent")),
    name = x$name,
    persona = if (!is.null(x$persona_frame())) x$persona_frame() else x$persona,
    config = x$config,
    memory = x$memory$state(),
    spans = x$internal_spans(),
    agent_id = x$id(),
    usage = as.list(x$usage()[1, c("calls", "tokens_sent",
                                   "tokens_received", "tool_calls")]),
    budget = x$budget,
    guardrails = x$guardrail_set()
  )
  saveRDS(state, path)
  invisible(path)
}

#' Load an agent from disk
#'
#' Restores an agent saved with [save_agent()]: same persona, config, memory
#' contents, budget, guardrails, and accounting. Because the config holds an
#' environment-variable reference rather than a key, the loaded agent works
#' immediately wherever that variable is set. Call, token, and tool counters
#' carry over, so a budget keeps binding across sessions; the wall-clock
#' (`max_seconds`) budget restarts at load.
#'
#' @param path File path written by [save_agent()].
#' @param tools Tools to re-attach (functions are not serialized).
#' @param embed_config Required only when the saved agent used
#'   [memory_recall()]; the embedding config to rebuild it with.
#' @return An [Agent].
#' @export
load_agent <- function(path, tools = list(), embed_config = NULL) {
  state <- readRDS(path)
  if (!is.list(state) || is.null(state$name) || is.null(state$config)) {
    stop("File does not contain a saved LLMRagent agent.", call. = FALSE)
  }
  mem <- memory_restore(state$memory, embed_config = embed_config)
  out <- agent(name = state$name, config = state$config,
               persona = state$persona, tools = tools, memory = mem,
               budget = state$budget %||% budget(),
               guardrails = state$guardrails)
  out$restore_accounting(usage = state$usage, spans = state$spans,
                         agent_id = state$agent_id)
  out
}
