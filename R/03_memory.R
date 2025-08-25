    
    #' Memory Interfaces
    #'
    #' Minimal memory implementations. BufferMemory keeps the last `n` messages.
    #' @name memory-api
    NULL

    #' Create a new BufferMemory
    #'
    #' @param n Number of most recent messages to keep.
    #' @return Buffer memory object.
    #' @examples
    #' mem <- new_buffer_memory(3)
    #' mem$add(create_message("user","hi"))
    #' @export
    new_buffer_memory <- function(n = 10) {
      stopifnot(is.numeric(n), n >= 1)
      env <- new.env(parent = emptyenv())
      env$n <- as.integer(n)
      env$messages <- list()
      env$add <- function(msg) {
        stopifnot(inherits(msg, "llmr_agent_message"))
        env$messages <- c(env$messages, list(msg))
        if (length(env$messages) > env$n) {
          env$messages <- utils::tail(env$messages, env$n)
        }
        invisible(TRUE)
      }
      env$get <- function() env$messages
      env$reset <- function() {
        env$messages <- list()
        invisible(TRUE)
      }
      class(env) <- c("llmr_buffer_memory", class(env))
      env
    }

    #' Summary Memory (via LLMR model config)
    #'
    #' Summarizes recent conversation using the provided LLMR `model_config`. The
    #' configuration can be set at creation or per-call. No external calls are made
    #' unless a valid `model_config` is provided.
    #'
    #' @param model_config Optional LLMR model config from `LLMR::llm_config()`.
    #' @param ... Reserved for future use.
    #' @return An environment with: `$add(msg)`, `$get()`, `$set_config(cfg)`,
    #'   `$summary(max_chars = 500, model_config = NULL, system_prompt = NULL)`.
    #' @examplesIf nzchar(Sys.getenv("OPENAI_API_KEY")) && identical(Sys.getenv("LLMRAgent_RUN_EXAMPLES"), "true")
    #' # Provide config at creation or per call
    #' sm <- new_summary_memory()
    #' sm$add(create_message("user","hello"))
    #' # sm$summary(model_config = cfg)
    #' @seealso [new_buffer_memory()], [memory-api]
    #' @export
    new_summary_memory <- function(model_config = NULL, ...) {
      env <- new.env(parent = emptyenv())
      env$messages <- list()
      env$model_config <- model_config
      env$add <- function(msg) {
        stopifnot(inherits(msg, "llmr_agent_message"))
        env$messages <- c(env$messages, list(msg))
        invisible(TRUE)
      }
      env$get <- function() env$messages
      env$set_config <- function(cfg) { env$model_config <- cfg; invisible(TRUE) }
      env$summary <- function(max_chars = 500L, model_config = NULL, system_prompt = NULL) {
        msgs <- env$messages
        if (!length(msgs)) return("")
        cfg <- model_config %||% env$model_config
        if (is.null(cfg)) {
          stop("model_config is required; pass via new_summary_memory(model_config=...) or summary(..., model_config=...).")
        }
        if (!requireNamespace("LLMR", quietly = TRUE)) {
          stop("Package 'LLMR' is required for summarization.")
        }
        # Build summarization prompt and recent context
        recent <- utils::tail(msgs, 12)
        sys <- system_prompt %||% sprintf(
          "You are a summarizer. Produce a concise summary of the conversation under %d characters. Focus on user goals, facts, constraints, and decisions.",
          as.integer(max_chars)
        )
        convo <- format_messages_for_api(recent)
        full_msgs <- c(list(list(role = "system", content = sys)), convo)
        resp <- .call_llm_guarded(
          config = cfg,
          messages = full_msgs,
          json_flag = FALSE
        )
        # Extract text consistently
        out <- if (is.character(resp)) resp[1] else if (is.list(resp) && !is.null(resp$text)) as.character(resp$text)[1] else if (is.list(resp) && !is.null(resp$content)) as.character(resp$content)[1] else as.character(resp)[1]
        out <- out %||% ""
        if (nchar(out) > max_chars) {
          out <- substr(out, 1, max_chars - 1)
          out <- paste0(out, "\u2026")
        }
        out
      }
      class(env) <- c("llmr_summary_memory", class(env))
      env
    }
