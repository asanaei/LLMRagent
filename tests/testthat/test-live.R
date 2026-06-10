# Live smoke tests on an inexpensive open-weight model. Gated on the key and
# never run on CRAN.

skip_if_no_groq <- function() {
  testthat::skip_if(!nzchar(Sys.getenv("GROQ_API_KEY")), "Requires GROQ_API_KEY")
  testthat::skip_on_cran()
}

test_that("live: agent chat, tools, and structured asking work end to end", {
  skip_if_no_groq()
  cfg <- LLMR::llm_config("groq", "openai/gpt-oss-20b", temperature = 0)

  a <- agent("Ada", cfg, persona = "Answer with one word when possible.",
             quiet = TRUE)
  r <- a$chat("What is the capital of France? One word.")
  expect_match(r, "Paris", ignore.case = TRUE)
  expect_equal(a$usage()$calls, 1L)

  doubler <- LLMR::llm_tool(function(x) as.character(as.numeric(x) * 2),
    name = "double_it", description = "Doubles a number.",
    parameters = list(x = list(type = "number")))
  tl <- agent("T", cfg, tools = doubler, quiet = TRUE)
  r2 <- tl$chat("Use double_it on 21 and report only the result.")
  expect_match(r2, "42")
  expect_gte(tl$usage()$tool_calls, 1L)

  v <- a$ask_structured("Is water wet?",
    schema = list(type = "object",
                  properties = list(answer = list(type = "string",
                                                  enum = list("yes", "no"))),
                  required = list("answer")))
  expect_true(v$answer %in% c("yes", "no"))
})

test_that("live: streaming chat produces text and accounts usage", {
  skip_if_no_groq()
  cfg <- LLMR::llm_config("groq", "openai/gpt-oss-20b", temperature = 0)
  a <- agent("S", cfg, quiet = TRUE)
  printed <- capture.output(r <- a$chat("Reply with exactly: streaming works",
                                        stream = TRUE))
  expect_match(r, "streaming works", ignore.case = TRUE)
  expect_equal(a$usage()$calls, 1L)
})

test_that("live: delegation and pipelines run end to end", {
  skip_if_no_groq()
  cfg <- LLMR::llm_config("groq", "openai/gpt-oss-20b", temperature = 0)

  spec <- agent("Mathy", cfg, quiet = TRUE,
                persona = "Answer arithmetic questions with only the number.")
  lead <- agent("Lead", cfg, quiet = TRUE,
                persona = "For any arithmetic, use the ask_mathy tool. Report only the final number.",
                tools = list(agent_as_tool(spec)))
  r <- lead$chat("What is 17 * 3? Use your tool.")
  expect_match(r, "51")
  expect_gte(spec$usage()$calls, 1L)   # the work landed on the specialist

  run <- agent_pipeline(
    list(agent("Caps", cfg, quiet = TRUE,
               persona = "Repeat the message in UPPERCASE. Output only that."),
         agent("Mark", cfg, quiet = TRUE,
               persona = "Append the word DONE to the message. Output only that.")),
    input = "hello world", quiet = TRUE)
  expect_match(run$output, "DONE")
  expect_equal(nrow(run$steps), 2L)
})

test_that("live: a deliberation returns structured votes", {
  skip_if_no_groq()
  cfg <- LLMR::llm_config("groq", "openai/gpt-oss-20b", temperature = 0.8)
  panel <- list(
    agent("Mo", cfg, persona = "Supports modernization. Terse.", quiet = TRUE),
    agent("Fi", cfg, persona = "Worries about costs. Terse.", quiet = TRUE)
  )
  d <- deliberate(panel, "Move the archive fully online.", rounds = 1,
                  quiet = TRUE)
  expect_equal(nrow(d$transcript), 2L)
  expect_true(all(d$votes$vote %in% c("yes", "no", "abstain")))
})
