test_that("round-robin conversation builds a shared transcript", {
  a <- fake_agent("Ana", list("Ana says one", "Ana says two"))
  b <- fake_agent("Ben", list("Ben says one", "Ben says two"))
  conv <- conversation(list(a, b), topic = "testing", max_turns = 4,
                       quiet = TRUE)
  tr <- conv$transcript
  expect_equal(nrow(tr), 4L)
  expect_identical(tr$speaker, c("Ana", "Ben", "Ana", "Ben"))
  expect_identical(tr$text[3], "Ana says two")
})

test_that("each speaker sees the full attributed dialogue", {
  seen <- list()
  watcher <- function(config, messages, tools, ...) {
    seen[[length(seen) + 1L]] <<- messages
    fake_response("noted")
  }
  cfg <- LLMR::llm_config("groq", "fake-model")
  a <- Agent$new("Ana", cfg, caller = watcher, quiet = TRUE)
  b <- Agent$new("Ben", cfg, caller = watcher, quiet = TRUE)
  conversation(list(a, b), topic = "context test", max_turns = 3, quiet = TRUE)
  third <- seen[[3]]
  usr <- unname(third[names(third) == "user"])
  expect_match(usr, "Ana: noted")
  expect_match(usr, "Ben: noted")
})

test_that("an opening statement lands on the transcript", {
  a <- fake_agent("Ana", list("x"))
  b <- fake_agent("Ben", list("y"))
  conv <- conversation(list(a, b), topic = "t", opening = "Welcome all.",
                       max_turns = 2, quiet = TRUE)
  expect_identical(conv$transcript$speaker[1], "Facilitator")
  expect_identical(conv$transcript$turn[1], 0L)
})

test_that("random policy never repeats a speaker back to back", {
  a <- fake_agent("Ana", list("x"))
  b <- fake_agent("Ben", list("y"))
  c <- fake_agent("Cyd", list("z"))
  set.seed(110)
  conv <- conversation(list(a, b, c), topic = "t", turn_policy = "random",
                       max_turns = 12, quiet = TRUE)
  sp <- conv$transcript$speaker
  expect_false(any(sp[-1] == sp[-length(sp)]))
})

test_that("stop_when ends the conversation early", {
  a <- fake_agent("Ana", list("keep", "keep"))
  b <- fake_agent("Ben", list("DONE"))
  conv <- conversation(list(a, b), topic = "t", max_turns = 10, quiet = TRUE,
                       stop_when = function(tr) any(grepl("DONE", tr$text)))
  expect_equal(nrow(conv$transcript), 2L)
})

test_that("duplicate agent names are rejected", {
  a <- fake_agent("Same", list("x"))
  b <- fake_agent("Same", list("y"))
  expect_error(conversation(list(a, b), topic = "t"), "unique")
})
