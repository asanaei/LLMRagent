# 00_package.R ----------------------------------------------------------------

#' LLMRagent: agents, multi-agent conversations, and agent experiments
#'
#' LLMRagent builds on LLMR's provider layer to provide:
#'
#' - [agent()]: an agent with a persona, an LLMR model config, native tool
#'   calling, pluggable memory, and hard budgets; replies can stream live
#'   with `chat(stream = TRUE)`.
#' - [agent_as_tool()]: expose an agent as a tool, so other agents can
#'   delegate to it -- the primitive behind supervisor/specialist
#'   hierarchies.
#' - [agent_pipeline()]: run text through a fixed chain of specialists,
#'   keeping every intermediate product.
#' - [conversation()]: multi-agent conversations over a shared, attributed
#'   transcript, with turn-taking policies and stop rules; presets
#'   [debate()], [focus_group()], [interview()], and [deliberate()].
#' - [agent_experiment()]: factorial designs over conditions and replications,
#'   run sequentially or in parallel, returning one tidy results frame.
#' - [think_harder()]: an orchestrator that uses one strong model to plan and
#'   synthesize while many cheap models do the heavy lifting.
#'
#' Every run yields a tidy transcript and a trace of calls, tokens, and
#' timings. Combine with `LLMR::llm_log_enable()` for a complete per-call
#' audit file.
#'
#' @keywords internal
#' @importFrom rlang %||%
#' @importFrom R6 R6Class
"_PACKAGE"

utils::globalVariables(c("speaker", "turn"))

# Internal: render a conversation transcript as readable dialogue.
.render_dialogue <- function(transcript) {
  if (!nrow(transcript)) return("")
  paste(sprintf("%s: %s", transcript$speaker, transcript$text), collapse = "\n\n")
}

# Internal: one place to validate llm_config arguments.
.check_config <- function(config, arg = "config") {
  if (!inherits(config, "llm_config")) {
    stop(sprintf("`%s` must be an LLMR::llm_config() object.", arg), call. = FALSE)
  }
  invisible(config)
}

# Internal: validate that a config is for embeddings (mirrors LLMR's
# inference: explicit embedding flag, or "embedding" in the model name).
.check_embed_config <- function(config, arg = "embed_config") {
  .check_config(config, arg)
  is_embed <- isTRUE(config$embedding) ||
    (is.null(config$embedding) &&
       grepl("embedding", config$model %||% "", ignore.case = TRUE))
  if (!is_embed) {
    stop(sprintf(paste0(
      "`%s` must be an embedding config (e.g. llm_config(..., embedding ",
      "= TRUE) or a model whose name contains \"embedding\")."), arg),
      call. = FALSE)
  }
  invisible(config)
}

# Internal: NA-safe integer add for token accounting.
.add_na0 <- function(a, b) {
  b <- suppressWarnings(as.integer(b %||% 0L))
  if (length(b) != 1L || is.na(b)) b <- 0L
  a + b
}
