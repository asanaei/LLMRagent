# 00_package.R ----------------------------------------------------------------

#' LLMRagent: agents, multi-agent conversations, and agent experiments
#'
#' LLMRagent builds on LLMR's provider layer to provide:
#'
#' - [agent()]: an agent with a persona and an LLMR model config. It calls
#'   native tools, keeps memory, holds to a budget, and can stream replies
#'   with `chat(stream = TRUE)`.
#' - [agent_as_tool()]: expose an agent as a tool, so other agents can
#'   delegate to it; this is the primitive behind supervisor/specialist
#'   hierarchies.
#' - [agent_pipeline()]: run text through a fixed chain of specialists,
#'   keeping every intermediate product.
#' - [conversation()]: multi-agent conversations over a shared, attributed
#'   transcript, with turn-taking policies and stop rules; presets
#'   [debate()], [focus_group()], [interview()], and [deliberate()].
#' - [agent_experiment()]: factorial designs over conditions and replications,
#'   run sequentially or in parallel, returning one tidy results frame.
#' - [agent_fanout_synthesis()]: an orchestrator that uses one strong model to
#'   plan and synthesize while many cheap models draft answers in parallel.
#'
#' Every run yields a tidy transcript and a trace of calls, tokens, and
#' timings. Combine with `LLMR::llm_log_enable()` for a complete per-call
#' audit file.
#'
#' @keywords internal
#' @importFrom rlang %||%
#' @importFrom R6 R6Class
"_PACKAGE"

utils::globalVariables(c("speaker", "turn", "text"))

# Internal: render a conversation transcript as readable dialogue. Retained for
# third-person / analysis surfaces (moderator next-speaker choice, judge
# verdicts, summaries) and for the "flat" message mode.
.render_dialogue <- function(transcript) {
  if (!nrow(transcript)) return("")
  paste(sprintf("%s: %s", transcript$speaker, transcript$text), collapse = "\n\n")
}

# The conversation message mode. "roleflip" (default) renders each speaker's own
# prior turns as assistant messages and others as labeled user messages, via
# LLMR's provider-safe builder; this is what reduces self-repetition. "flat"
# reproduces the legacy single-user-message behavior (the whole transcript
# pasted into one user turn). Settable for experiments via
# options(LLMRagent.msg_mode = "flat") or per call.
.msg_mode <- function(mode = NULL) {
  m <- mode %||% getOption("LLMRagent.msg_mode", "roleflip")
  m <- match.arg(as.character(m), c("roleflip", "flat"))
  m
}

# Run `code` with the message mode resolved and pinned for the duration. A
# `msg_mode` argument on a public conversation function passes through here:
# NULL keeps the caller's global option (or the "roleflip" default); an explicit
# value overrides it for this run only, then the previous option is restored.
.with_msg_mode <- function(msg_mode, code) {
  resolved <- .msg_mode(msg_mode)
  old <- getOption("LLMRagent.msg_mode")
  options(LLMRagent.msg_mode = resolved)
  on.exit(options(LLMRagent.msg_mode = old), add = TRUE)
  force(code)
}

# Internal: build the message list for `speaker`'s next turn from the shared
# transcript. `sys` is the persona + role instruction (-> system); `turn` is the
# trailing "your turn" / current-question cue (-> final user turn). In "flat"
# mode it reproduces the legacy c(system=, user="Dialogue so far:\n...") shape;
# in "roleflip" mode it delegates to LLMR::transcript_as_messages(). All
# role-flip / coalesce / sanitize logic lives in LLMR.
.dialogue_messages <- function(transcript, speaker, sys = NULL, turn = NULL,
                               mode = NULL) {
  mode <- .msg_mode(mode)
  if (identical(mode, "flat")) {
    usr <- if (nrow(transcript)) {
      paste0("Dialogue so far:\n\n", .render_dialogue(transcript),
             if (!is.null(turn)) paste0("\n\n", turn) else "")
    } else {
      turn %||% ""
    }
    out <- list()
    if (!is.null(sys) && nzchar(sys)) out <- c(out, list(list(role = "system", content = sys)))
    c(out, list(list(role = "user", content = usr)))
  } else {
    LLMR::transcript_as_messages(
      transcript  = transcript[, c("speaker", "text"), drop = FALSE],
      speaker     = speaker,
      system      = sys,
      instruction = turn
    )
  }
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
