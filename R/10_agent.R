    
    #' Create Agent with Model Config
    #'
    #' Simple agent that uses LLMR model configuration directly.
    #' Automatically tracks token usage across all interactions.
    #' Requires a valid LLMR model config to function.
    #'
    #' @param system_prompt System message to prepend.
    #' @param model_config LLMR model config from `LLMR::llm_config()`. Required.
    #' @param memory Memory object from `new_buffer_memory()` or `new_summary_memory()`.
    #' @param summarizer_model_config Optional LLMR config used by summary memory.
    #'   If `NULL` and `memory` is a summary memory without its own config, defaults to `model_config`.
    #' @return An agent environment with built-in usage tracking.
    #' @examplesIf nzchar(Sys.getenv("OPENAI_API_KEY")) && identical(Sys.getenv("LLMRAgent_RUN_EXAMPLES"), "true")
    #' if (requireNamespace("LLMR", quietly = TRUE) && Sys.getenv("OPENAI_API_KEY") != "") {
    #'   config <- LLMR::llm_config(provider = "openai", model = "gpt-4", 
    #'                              api_key = Sys.getenv("OPENAI_API_KEY"))
    #'   ag <- new_agent(system_prompt = "Be brief.", model_config = config)
    #'   # agent_reply(ag, "Hello")
    #'   # usage <- agent_usage(ag)  # Check token usage
    #' }
    #' @seealso [agent_reply()], [agent_usage()], [agent_usage_reset()]
    #' @export
    new_agent <- function(system_prompt = "", model_config, memory = new_buffer_memory(10),
                          summarizer_model_config = NULL) {
      if (missing(model_config) || is.null(model_config)) {
        stop("model_config is required. Use LLMR::llm_config() to create one.")
      }
      if (!inherits(model_config, "llm_config")) {
        stop("model_config must inherit class 'llm_config'.")
      }
      if (!requireNamespace("LLMR", quietly = TRUE)) {
        stop("Package 'LLMR' is required. Please install it.")
      }

      env <- new.env(parent = emptyenv())
      env$system_prompt <- as.character(system_prompt)[1]
      env$memory <- memory
      env$model_config <- model_config

      if (inherits(env$memory, "llmr_summary_memory")) {
        default_sum_cfg <- summarizer_model_config %||% model_config
        if (is.null(env$memory$model_config)) {
          if (is.function(env$memory$set_config)) env$memory$set_config(default_sum_cfg) else env$memory$model_config <- default_sum_cfg
        }
      }
      env$set_summarizer_config <- function(cfg) {
        if (inherits(env$memory, "llmr_summary_memory")) {
          if (is.function(env$memory$set_config)) env$memory$set_config(cfg) else env$memory$model_config <- cfg
          invisible(TRUE)
        } else {
          stop("Current memory does not support summarizer configuration.")
        }
      }

      env$usage_history <- list()
      env$total_tokens_in <- 0L
      env$total_tokens_out <- 0L
      env$total_tokens <- 0L
      env
    }

    #' Ask the agent to reply
    #' 
    #' @param agent Agent created by `new_agent()`.
    #' @param user_text Character scalar user input.
    #' @param json Logical; if TRUE (default) request JSON-structured output when supported.
    #' @param schema Optional JSON Schema (R list) to enforce structured output when `json = TRUE`.
    #'   If `NULL` and `json = TRUE`, the agent requests a generic JSON object via provider-appropriate toggles.
    #' @param strict Logical; when `schema` is supplied on OpenAI-compatible providers, pass `strict = TRUE` by default.
    #' @param return_object Logical; if TRUE, return the underlying `LLMR::llmr_response` object instead of character text.
    #' @return Character scalar assistant reply.
    #' @examples
    #' # Minimal deterministic example without API call:
    #' dummy <- new.env(); dummy$memory <- new_buffer_memory(2)
    #' dummy$system_prompt <- ""; dummy$model_config <- list()
    #' # agent_reply(dummy, "hello", json = FALSE)
    #' @export
    agent_reply <- function(agent, user_text, json = TRUE, schema = NULL, strict = TRUE, return_object = FALSE) {
      stopifnot(is.environment(agent))

      msgs <- list()
      if (nzchar(agent$system_prompt)) {
        msgs <- c(msgs, list(create_message("system", agent$system_prompt)))
      }
      user_msg <- create_message("user", user_text)
      msgs <- c(msgs, agent$memory$get(), list(user_msg))

      resp <- do.call(
        what = .call_llm_guarded,
        args = list(
          config    = agent$model_config,
          messages  = format_messages_for_api(msgs),
          json_flag = json,
          schema    = schema,
          strict    = strict
        )
      )

      # --- text extraction (works for llmr_response OR list) ---
      reply_text <- if (inherits(resp, "llmr_response")) {
        as.character(resp)
      } else if (is.character(resp)) {
        resp[1]
      } else if (is.list(resp) && !is.null(resp$text)) {
        as.character(resp$text)[1]
      } else if (is.list(resp) && !is.null(resp$content)) {
        as.character(resp$content)[1]
      } else {
        as.character(resp)[1]
      }
      reply_text <- reply_text %||% ""

      # --- token extraction (handles llmr_response OR list) ---
      extract_counts <- function(r) {
        u <- tryCatch(LLMR::tokens(r), error = function(e) NULL)
        if (is.list(u)) {
          ti <- as.integer(u$sent %||% u$input %||% u$prompt %||% u$prompt_tokens %||% u$input_tokens %||% 0L)
          to <- as.integer(u$rec  %||% u$output %||% u$completion %||% u$completion_tokens %||% u$output_tokens %||% 0L)
          tt <- as.integer(u$total %||% u$total_tokens %||% (ti + to))
          return(list(`in` = ti, `out` = to, total = tt))
        }
        if (is.list(r)) {
          us <- r$usage %||% (r$meta %||% list())$usage %||% NULL
          if (!is.null(us)) {
            ti <- as.integer(us$prompt_tokens %||% us$input_tokens %||% us$sent %||% 0L)
            to <- as.integer(us$completion_tokens %||% us$output_tokens %||% us$rec %||% 0L)
            tt <- as.integer(us$total_tokens %||% (ti + to))
            return(list(`in` = ti, `out` = to, total = tt))
          }
        }
        list(`in` = 0L, `out` = 0L, total = 0L)
      }

      counts <- extract_counts(resp)
      tokens_in    <- as.integer(counts[["in"]]    %||% 0L)
      tokens_out   <- as.integer(counts[["out"]]   %||% 0L)
      tokens_total <- as.integer(counts[["total"]] %||% (tokens_in + tokens_out))

      model_used <- {
        if (inherits(resp, "llmr_response")) resp$model %||% NA_character_
        else if (is.list(resp)) resp$model %||% (resp$meta %||% list())$model %||% NA_character_
        else NA_character_
      }

      agent$total_tokens_in  <- agent$total_tokens_in  + tokens_in
      agent$total_tokens_out <- agent$total_tokens_out + tokens_out
      agent$total_tokens     <- agent$total_tokens     + tokens_total

      fr <- if (inherits(resp, "llmr_response")) LLMR::finish_reason(resp) else NA_character_

      agent$usage_history <- append(agent$usage_history, list(list(
        timestamp     = Sys.time(),
        tokens_in     = tokens_in,
        tokens_out    = tokens_out,
        tokens_total  = tokens_total,
        finish_reason = fr,
        model         = model_used,
        user_text     = substr(user_text, 1, 100),
        reply_text    = substr(reply_text, 1, 100)
      )))

      out_msg <- create_message("assistant", reply_text)
      agent$memory$add(user_msg)
      agent$memory$add(out_msg)
      if (isTRUE(return_object) && inherits(resp, "llmr_response")) {
        return(resp)
      }
      reply_text
    }

    #' Get agent's cumulative token usage
    #'
    #' Returns the total token usage for this agent across all interactions.
    #' 
    #' @param agent Agent created by `new_agent()`.
    #' @return List with components:
    #'   \item{total_tokens_in}{Cumulative input tokens}
    #'   \item{total_tokens_out}{Cumulative output tokens}  
    #'   \item{total_tokens}{Cumulative total tokens}
    #'   \item{interactions}{Number of interactions}
    #'   \item{history}{List of interaction records with timestamps and token details}
    #' @examplesIf nzchar(Sys.getenv("OPENAI_API_KEY")) && identical(Sys.getenv("LLMRAgent_RUN_EXAMPLES"), "true")
    #' if (requireNamespace("LLMR", quietly = TRUE) && Sys.getenv("OPENAI_API_KEY") != "") {
    #'   config <- LLMR::llm_config(provider = "openai", model = "gpt-4",
    #'                              api_key = Sys.getenv("OPENAI_API_KEY"))
    #'   ag <- new_agent(system_prompt = "Be brief.", model_config = config)
    #'   # agent_reply(ag, "hello")
    #'   # usage <- agent_usage(ag)
    #'   # cat("Total tokens used:", usage$total_tokens)
    #'   # cat("Interactions:", usage$interactions)
    #' }
    #' @seealso [agent_reply()], [agent_usage_reset()]
    #' @export
    agent_usage <- function(agent) {
      stopifnot(is.environment(agent))
      list(
        total_tokens_in = agent$total_tokens_in,
        total_tokens_out = agent$total_tokens_out,
        total_tokens = agent$total_tokens,
        interactions = length(agent$usage_history),
        history = agent$usage_history
      )
    }

    #' Reset agent's token usage tracking
    #'
    #' Clears the usage history and resets counters to zero.
    #' 
    #' @param agent Agent created by `new_agent()`.
    #' @return Nothing (invisibly).
    #' @examplesIf nzchar(Sys.getenv("OPENAI_API_KEY")) && identical(Sys.getenv("LLMRAgent_RUN_EXAMPLES"), "true")
    #' if (requireNamespace("LLMR", quietly = TRUE) && Sys.getenv("OPENAI_API_KEY") != "") {
    #'   config <- LLMR::llm_config(provider = "openai", model = "gpt-4",
    #'                              api_key = Sys.getenv("OPENAI_API_KEY"))
    #'   ag <- new_agent(system_prompt = "Be brief.", model_config = config)
    #'   # agent_reply(ag, "hello")  
    #'   # agent_usage_reset(ag)  # Clear usage history
    #' }
    #' @export
    agent_usage_reset <- function(agent) {
      stopifnot(is.environment(agent))
      agent$usage_history <- list()
      agent$total_tokens_in <- 0
      agent$total_tokens_out <- 0
      agent$total_tokens <- 0
      invisible(NULL)
    }
