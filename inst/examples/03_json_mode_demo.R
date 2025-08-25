## JSON Mode Demo (requires API key)
## Run: source(system.file("examples", "03_json_mode_demo.R", package = "LLMRAgent"))

library(LLMRAgent)

if (requireNamespace("LLMR", quietly = TRUE)) {

  # Example with JSON mode enabled
  if (Sys.getenv("OPENAI_API_KEY") != "") {
    config <- LLMR::llm_config(
      provider = "openai",
      model = "gpt-4o-mini",
      api_key = Sys.getenv("OPENAI_API_KEY")
    )

    agent <- new_agent(
      system_prompt = "You are a data analyst. Always respond with structured data in JSON format.",
      model_config = config
    )

    cat("Testing JSON mode with structured data request:\n")

    # Request structured data with JSON mode
    reply <- agent_reply(
      agent,
      "Give me information about the programming language R: name, year created, and main use case. Format as JSON.",
      json = TRUE
    )

    cat("JSON Response:\n", reply, "\n", sep = "")

    # Try to parse the JSON
    tryCatch({
      parsed <- LLMR::llm_parse_structured(reply)
      cat("\nParsed structured JSON:\n")
      str(parsed)
    }, error = function(e) {
      cat("\nNote: Could not robustly parse JSON:", e$message, "\n")
    })

    # Optional: Schema mode
    schema <- list(
      type = "object",
      properties = list(
        name = list(type = "string"),
        year = list(type = "integer")
      ),
      required = list("name","year"),
      additionalProperties = FALSE
    )

    reply2 <- agent_reply(
      agent,
      "For 'R programming language', return {name, year} as JSON.",
      json   = TRUE,
      schema = schema
    )
    cat("\nSchema-enforced JSON:\n", reply2, "\n", sep = "")
    str(LLMR::llm_parse_structured(reply2))

  } else {
    cat("OPENAI_API_KEY not set. Set it to test JSON mode.\n")
  }

} else {
  cat("LLMR package not available. Install it to use JSON mode.\n")
}
