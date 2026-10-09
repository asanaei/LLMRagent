# A completed run's record is fixed when the run closes: reusing or changing an
# agent afterwards does not rewrite it. A bare Agent's view stays live.

echo_tool <- function() {
  LLMR::llm_tool(function(x) x, name = "echo", description = "Echo the input.",
                 parameters = list(x = list(type = "string")))
}

finished_conversation <- function() {
  a <- fake_agent("Ana", list("one", "two", "three"))
  b <- fake_agent("Ben", list("uno", "dos", "tres"))
  a$config$model_params$temperature <- 0.7
  conv <- conversation(list(a, b), topic = "t", max_turns = 2, quiet = TRUE)
  list(conv = conv, a = a)
}

test_that("later changes to an agent do not rewrite a finished run", {
  f <- finished_conversation()
  run1 <- as_agent_run(f$conv)
  m1 <- agent_manifest(run1)
  s1 <- tibble::as_tibble(run1, "state")

  f$a$config$model_params$temperature <- 0
  f$a$tools <- list(echo_tool())
  f$a$chat("a later message")

  run2 <- as_agent_run(f$conv)
  m2 <- agent_manifest(run2)
  expect_identical(m2$params, m1$params)
  expect_identical(m2$params$value[m2$params$param == "temperature"], "0.7")
  expect_identical(nrow(m2$tools), 0L)
  expect_identical(tibble::as_tibble(run2, "state"), s1)
  expect_false(any(grepl("a later message", tibble::as_tibble(run2, "state")$content)))
  expect_identical(m2$manifest_hash, m1$manifest_hash)
})

test_that("a bare Agent is still a live view", {
  a <- fake_agent("Ana", list("one", "two"))
  a$chat("first")
  a$tools <- list(echo_tool())
  a$chat("second")
  m <- agent_manifest(as_agent_run(a))
  expect_identical(m$tools$tool_name, "echo")
  expect_true(any(grepl("second", tibble::as_tibble(as_agent_run(a), "state")$content)))
})

test_that("a run object without a snapshot keeps the earlier live behavior", {
  f <- finished_conversation()
  run <- as_agent_run(f$conv)
  run$snapshot <- NULL
  f$a$chat("a later message")
  expect_true(any(grepl("a later message", tibble::as_tibble(run, "state")$content)))
})
