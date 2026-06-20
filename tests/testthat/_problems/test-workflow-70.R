# Extracted from test-workflow.R:70

# test -------------------------------------------------------------------------
calls <- new.env()
calls$a <- 0L
calls$b <- 0L
ok_b <- FALSE
wf <- agent_workflow("w") |>
    add_node("a", function(state) { calls$a <- calls$a + 1L; state$a <- 1L; state }) |>
    add_node("b", function(state) {
      calls$b <- calls$b + 1L
      if (!ok_b) stop("not yet"); state$b <- 2L; state }) |>
    add_edge("a", "b")
run <- run_workflow(wf, input = NULL, quiet = TRUE)
expect_identical(run$status, "failed")
expect_equal(calls$a, 1L)
expect_equal(calls$b, 1L)
ok_b <<- TRUE
run2 <- resume_workflow(run$checkpoint_dir, wf = wf, quiet = TRUE)
expect_identical(run2$status, "done")
