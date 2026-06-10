test_that("agent_pipeline chains outputs and keeps every step", {
  a1 <- fake_agent("A", list("alpha"))
  a2 <- fake_agent("B", list("beta"))
  a3 <- fake_agent("C", list("gamma"))
  run <- agent_pipeline(list(a1, a2, a3), input = "start", quiet = TRUE)

  expect_s3_class(run, "agent_pipeline_run")
  expect_identical(run$output, "gamma")
  expect_identical(run$steps$input,  c("start", "alpha", "beta"))
  expect_identical(run$steps$output, c("alpha", "beta", "gamma"))
  expect_identical(as.data.frame(run)$agent, c("A", "B", "C"))
  # stages are stateless: nothing written to agent memories
  expect_equal(nrow(a1$transcript()), 0L)
})

test_that("a single agent is a valid one-stage pipeline", {
  solo <- fake_agent("Solo", list("done"))
  run <- agent_pipeline(solo, "x", quiet = TRUE)
  expect_identical(run$output, "done")
  expect_equal(nrow(run$steps), 1L)
})

test_that("agent_pipeline validates its inputs", {
  expect_error(agent_pipeline(list("nope"), "x"), "Agent objects")
  expect_error(agent_pipeline(list(), "x"))
})
