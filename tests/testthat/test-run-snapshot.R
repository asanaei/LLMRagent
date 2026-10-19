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

test_that("a second conversation, a reset, and delayed archiving leave a run's record intact", {
  a <- fake_agent("Ana", list("noted", "one", "two", "noted again", "three", "four"))
  b <- fake_agent("Ben", list("uno", "dos", "tres", "cuatro"))
  a$config$model_params$temperature <- 0.7
  a$chat("remember the first study")
  conv1 <- conversation(list(a, b), topic = "first", max_turns = 2, quiet = TRUE)
  run1 <- as_agent_run(conv1)
  m1 <- agent_manifest(run1)
  s1 <- tibble::as_tibble(run1, "state")
  expect_true(any(grepl("remember the first study", s1$content)))
  now <- archive_agent_study(conv1, tempfile("arc-now"))

  # Reuse the agents: more memory, a second conversation, then a reset and a
  # parameter change.
  a$chat("between the studies")
  conv2 <- conversation(list(a, b), topic = "second", max_turns = 2, quiet = TRUE)
  a$reset()
  a$config$model_params$temperature <- 0
  a$tools <- list(echo_tool())
  expect_identical(nrow(a$transcript()), 0L)

  later <- as_agent_run(conv1)
  expect_identical(tibble::as_tibble(later, "state"), s1)
  expect_identical(agent_manifest(later)$manifest_hash, m1$manifest_hash)
  # The second conversation recorded the memory it closed with, before the reset.
  s2 <- tibble::as_tibble(as_agent_run(conv2), "state")
  expect_true(any(grepl("between the studies", s2$content)))
  expect_false(any(grepl("between the studies", s1$content)))

  # Archiving the first conversation now writes the record it had at close.
  delayed <- archive_agent_study(conv1, tempfile("arc-later"))
  for (f in c("manifest.json", "state.csv", "tools.csv", "transcript.csv")) {
    expect_identical(readLines(file.path(delayed$path, f)),
                     readLines(file.path(now$path, f)), info = f)
  }
  expect_identical(delayed$manifest_hash, now$manifest_hash)

  # A result saved before the agents changed reads back with the same record.
  path <- tempfile(fileext = ".rds")
  saveRDS(conv1, path)
  reread <- as_agent_run(readRDS(path))
  expect_identical(tibble::as_tibble(reread, "state"), s1)
  expect_identical(agent_manifest(reread)$manifest_hash, m1$manifest_hash)
  unlink(c(path, now$path, delayed$path), recursive = TRUE)
})
