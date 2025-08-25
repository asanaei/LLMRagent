    
    #' Internal Utilities
    #'
    #' @keywords internal
    #' @noRd
    `%||%` <- function(x, y) if (is.null(x)) y else x

    #' Internal logger
    #' @keywords internal
    #' @noRd
    .rl <- function(...) {
      msg <- paste0("[LLMRAgent] ", paste0(..., collapse = ""))
      message(msg)
    }

    #' Validate that an object is JSON-serializable
    #' @param x any
    #' @return logical
    #' @keywords internal
    #' @noRd
    check_json <- function(x) {
      out <- try(jsonlite::toJSON(x, auto_unbox = TRUE), silent = TRUE)
      if (inherits(out, "try-error")) return(FALSE)
      isTRUE(jsonlite::validate(out))
    }

    #' Estimate tokens (very rough character-based heuristic)
    #' @param text character
    #' @return integer approximate tokens
    #' @keywords internal
    #' @noRd
    token_est <- function(text) {
      if (length(text) == 0 || is.null(text)) return(0L)
      as.integer(nchar(paste(text, collapse = " ")) / 4)
    }

    #' Call LLM with structured-output guard (LLMR >= 0.6.0)
    #' @param config an LLMR llm_config
    #' @param messages normalized messages for provider
    #' @param json_flag logical: if TRUE, request JSON object via enable_structured_output(); if FALSE, plain text
    #' @param schema optional JSON Schema (R list) to enforce structured output when provider supports it
    #' @param strict logical: pass strict=TRUE to enable_structured_output for OpenAI-compatible providers
    #' @keywords internal
    #' @noRd
    .call_llm_guarded <- function(config, messages, json_flag = TRUE, schema = NULL, strict = TRUE) {
      cfg2 <- config
      if (isTRUE(json_flag)) {
        # if schema is provided and is a list, pass it; otherwise request generic JSON object
        cfg2 <- tryCatch({
          if (!requireNamespace("LLMR", quietly = TRUE)) stop("Package 'LLMR' not available.")
          LLMR::enable_structured_output(config, schema = schema, strict = isTRUE(strict))
        }, error = function(e) {
          .rl("enable_structured_output failed: ", conditionMessage(e), "; proceeding without provider toggle.")
          config
        })
      }
      res <- try(LLMR::call_llm_robust(config = cfg2, messages = messages), silent = TRUE)
      if (inherits(res, "try-error") && isTRUE(json_flag)) {
        .rl("Structured-output call failed; retrying without structured output.")
        res <- LLMR::call_llm_robust(config = config, messages = messages)
      }
      res
    }
