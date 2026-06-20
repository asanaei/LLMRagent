# Stage 4: the workflow runtime. Offline via plain function nodes + fake agents.

test_that("a linear function-node workflow runs and threads state", {
  wf <- agent_workflow("w") |>
    add_node("clean", function(state) { state$x <- trimws(state$input); state }) |>
    add_node("label", function(state) { state$label <- nchar(state$x) > 3; state }) |>
    add_edge("clean", "label")
  run <- run_workflow(wf, input = "  hello  ", quiet = TRUE)
  expect_s3_class(run, "agent_workflow_run")
  expect_identical(run$status, "done")
  expect_identical(run$state$x, "hello")
  expect_true(run$state$label)
  expect_equal(nrow(run$steps), 2L)
})

test_that("agent nodes call reply() and write to state", {
  a <- fake_agent("A", list("AGENT-REPLY"))
  wf <- agent_workflow("w") |> add_node("ask", a, input_key = "input", output_key = "answer")
  run <- run_workflow(wf, input = "question", quiet = TRUE)
  expect_identical(run$state$answer, "AGENT-REPLY")
})

test_that("conditional edges route on a predicate", {
  wf <- agent_workflow("w") |>
    add_node("score", function(state) { state$ok <- state$input > 5; state }) |>
    add_node("high", function(state) { state$path <- "high"; state }) |>
    add_node("low",  function(state) { state$path <- "low"; state }) |>
    add_edge("score", "high", when = function(state) isTRUE(state$ok)) |>
    add_edge("score", "low",  when = function(state) !isTRUE(state$ok))
  expect_identical(run_workflow(wf, input = 9, quiet = TRUE)$state$path, "high")
  expect_identical(run_workflow(wf, input = 2, quiet = TRUE)$state$path, "low")
})

test_that("a loop bounded by max_steps raises rather than spinning", {
  wf <- agent_workflow("w") |>
    add_node("tick", function(state) { state$n <- (state$n %||% 0L) + 1L; state }) |>
    add_edge("tick", "tick")   # infinite self-loop
  expect_error(run_workflow(wf, input = NULL, max_steps = 5L, quiet = TRUE),
               class = "llmragent_workflow_error")
})

test_that("a failed node yields status failed with snapshots intact", {
  wf <- agent_workflow("w") |>
    add_node("a", function(state) { state$a <- 1L; state }) |>
    add_node("b", function(state) stop("boom")) |>
    add_edge("a", "b")
  run <- run_workflow(wf, input = NULL, quiet = TRUE)
  expect_identical(run$status, "failed")
  expect_true(grepl("boom", run$error))
  # the first node's snapshot exists
  snaps <- list.files(file.path(run$checkpoint_dir, "state"))
  expect_true(any(grepl("_a\\.rds$", snaps)))
})

test_that("resume reruns only the failed node", {
  # Use an environment for the flag so the node closures and the test share one
  # binding (a local + <<- would not, due to R scoping).
  ctl <- new.env(); ctl$a <- 0L; ctl$b <- 0L; ctl$ok_b <- FALSE
  wf <- agent_workflow("w") |>
    add_node("a", function(state) { ctl$a <- ctl$a + 1L; state$a <- 1L; state }) |>
    add_node("b", function(state) {
      ctl$b <- ctl$b + 1L
      if (!ctl$ok_b) stop("not yet"); state$b <- 2L; state }) |>
    add_edge("a", "b")
  run <- run_workflow(wf, input = NULL, quiet = TRUE)
  expect_identical(run$status, "failed")
  expect_equal(ctl$a, 1L); expect_equal(ctl$b, 1L)
  # repair and resume from the failed node (pass the dir + wf): 'a' must NOT run again
  ctl$ok_b <- TRUE
  run2 <- resume_workflow(run$checkpoint_dir, wf = wf, quiet = TRUE)
  expect_identical(run2$status, "done")
  expect_equal(ctl$a, 1L)   # 'a' ran once total
  expect_equal(ctl$b, 2L)   # 'b' ran twice (fail + successful resume)
})

test_that("a human-gate node pauses and resume continues", {
  wf <- agent_workflow("w") |>
    add_node("prep", function(state) { state$ready <- TRUE; state }) |>
    add_node("gate", human_gate("approve_step")) |>
    add_node("after", function(state) { state$done <- TRUE; state }) |>
    add_edge("prep", "gate") |>
    add_edge("gate", "after")
  run <- run_workflow(wf, input = NULL, quiet = TRUE)
  expect_identical(run$status, "paused")
  expect_s3_class(run$checkpoint, "llmragent_wf_checkpoint")
  resumed <- resume_workflow(run, approve = TRUE, quiet = TRUE)
  expect_identical(resumed$status, "done")
  expect_true(resumed$state$done)
})

test_that("replay verifies deterministic state hashes and catches corruption", {
  wf <- agent_workflow("w") |>
    add_node("step1", function(state) { state$v <- (state$input %||% 0) + 1; state }) |>
    add_node("step2", function(state) { state$v <- state$v * 2; state }) |>
    add_edge("step1", "step2")
  run <- run_workflow(wf, input = 10, quiet = TRUE)
  # clean replay passes (deterministic graph re-runs to the same state hashes)
  rp <- replay_run(run, wf, verify = "strict", quiet = TRUE)
  expect_true(all(rp$steps$replay_match %in% c(TRUE, NA)))

  # Tamper with a recorded state hash in run.jsonl: a faithful replay now
  # disagrees with the (corrupted) record, and strict verification catches it.
  logf <- file.path(run$checkpoint_dir, "run.jsonl")
  lines <- readLines(logf)
  lines <- sub("\"state_hash\":\"[0-9a-f]+\"",
               "\"state_hash\":\"deadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeef\"",
               lines)
  writeLines(lines, logf)
  expect_error(replay_run(run, wf, verify = "strict", quiet = TRUE),
               class = "llmragent_replay_mismatch")
})

test_that("fork branches a new run from a snapshot without rerunning it", {
  wf <- agent_workflow("w") |>
    add_node("a", function(state) { state$a <- 1L; state }) |>
    add_node("b", function(state) { state$b <- (state$a %||% 0L) + state$bump; state }) |>
    add_edge("a", "b")
  run <- run_workflow(wf, input = NULL, state = list(bump = 10L), quiet = TRUE)
  expect_identical(run$status, "done")
  # fork at the first step, bump differently
  branch <- fork_workflow(run, wf, at = 1L,
                          mutate = function(state) { state$bump <- 100L; state },
                          quiet = TRUE)
  expect_identical(branch$status, "done")
  expect_false(identical(branch$checkpoint_dir, run$checkpoint_dir))  # new run dir
})

test_that("workflow_from_pipeline reproduces a linear chain", {
  a1 <- fake_agent("E", list("EXTRACTED"))
  a2 <- fake_agent("R", list("REWRITTEN"))
  wf <- workflow_from_pipeline(list(a1, a2))
  expect_s3_class(wf, "agent_workflow")
  run <- run_workflow(wf, input = "raw text", quiet = TRUE)
  expect_identical(run$status, "done")
  expect_identical(run$state$stage_2, "REWRITTEN")
})

test_that("replay verifies a LOOPING workflow by sequence position", {
  # a counter that loops 3 times; each visit to 'tick' is a distinct sequence
  # position, so a name-based hash match would wrongly collapse them.
  wf <- agent_workflow("w") |>
    add_node("tick", function(state) { state$n <- (state$n %||% 0L) + 1L; state }) |>
    add_node("done", function(state) { state$final <- state$n; state }) |>
    add_edge("tick", "tick", when = function(state) (state$n %||% 0L) < 3L) |>
    add_edge("tick", "done", when = function(state) (state$n %||% 0L) >= 3L)
  run <- run_workflow(wf, input = NULL, max_steps = 20L, quiet = TRUE)
  expect_identical(run$status, "done")
  expect_equal(run$state$final, 3L)
  # the loop visited 'tick' 3 times, then 'done'
  expect_equal(sum(run$steps$node == "tick"), 3L)
  # a clean replay of this deterministic loop matches at every position
  rp <- replay_run(run, wf, verify = "strict", quiet = TRUE)
  expect_true(all(rp$steps$replay_match %in% c(TRUE, NA)))
  expect_equal(nrow(rp$steps), nrow(run$steps))   # same sequence length
})

test_that("agent_calibrate OLS accepts a two-sided formula with y absent from data", {
  withr::local_seed(110)
  N <- 400L; n <- 80L
  d <- data.frame(x1 = rnorm(N), x2 = rnorm(N))       # covariates only
  y <- 1 + 2 * d$x1 - d$x2 + rnorm(N, sd = 0.5)
  pred <- y + 0.3 + rnorm(N, sd = 0.4)                # biased predictions
  cal <- agent_calibrate(predictions = pred, gold = y[1:n],
                         method = "ppi", estimand = "ols",
                         formula = y ~ x1 + x2, data = d, id = 1:n)
  expect_s3_class(cal, "agent_calibration")
  x1 <- cal$estimate$estimate[cal$estimate$term == "x1"]
  expect_true(abs(x1 - 2) < 0.3)                      # recovers the true slope
})
