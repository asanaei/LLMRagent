# replay_run() verifies the whole recorded run: a replay that stops early, a
# damaged log, and a missing input snapshot are errors, not passes.

three_steps <- function(last_edge = function(state) TRUE) {
  agent_workflow("w") |>
    add_node("a", function(state) { state$v <- (state$input %||% 0) + 1; state }) |>
    add_node("b", function(state) { state$v <- state$v * 2; state }) |>
    add_node("c", function(state) { state$v <- state$v - 3; state }) |>
    add_edge("a", "b") |>
    add_edge("b", "c", when = last_edge)
}

test_that("a replay that finishes before the end of the record is a mismatch", {
  run <- run_workflow(three_steps(), input = 1, quiet = TRUE)
  expect_error(
    replay_run(run, three_steps(last_edge = function(state) FALSE),
               verify = "strict", quiet = TRUE),
    class = "llmragent_replay_mismatch")
  expect_error(
    replay_run(run, three_steps(last_edge = function(state) FALSE), quiet = TRUE),
    "finished after 2 step\\(s\\), but the record has 3")
  expect_no_error(replay_run(run, three_steps(), verify = "strict", quiet = TRUE))
})

test_that("a malformed line in the recorded log stops the replay", {
  run <- run_workflow(three_steps(), input = 1, quiet = TRUE)
  logf <- file.path(run$checkpoint_dir, "run.jsonl")
  lines <- readLines(logf)
  writeLines(c(lines[1:2], '{"node":"b","status":"ok","state_ha', lines[-(1:2)]), logf)
  expect_error(replay_run(run, three_steps(), quiet = TRUE), "Line 3 .* is not valid JSON")
})

test_that("a missing input snapshot stops the replay", {
  run <- run_workflow(three_steps(), input = 1, quiet = TRUE)
  unlink(list.files(file.path(run$checkpoint_dir, "state"), full.names = TRUE))
  expect_error(replay_run(run, three_steps(), quiet = TRUE),
               "input state could not be read")
})
