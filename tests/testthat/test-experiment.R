test_that("agent_experiment expands the design and captures results", {
  design <- expand.grid(x = c(1, 2), g = c("a", "b"), stringsAsFactors = FALSE)
  res <- agent_experiment(design, reps = 2, quiet = TRUE,
                          run_fn = function(cond, rep) {
                            paste0(cond$g, cond$x, "-r", rep)
                          })
  expect_equal(nrow(res), 8L)
  expect_true(all(c("rep", "result", "error", "duration") %in% names(res)))
  expect_identical(res$result[[1]], "a1-r1")
  expect_identical(res$result[[2]], "a1-r2")
  expect_true(all(is.na(res$error)))
})

test_that("a failing cell records its error and the rest proceed", {
  design <- data.frame(x = c(1, 2, 3))
  res <- agent_experiment(design, quiet = TRUE,
                          run_fn = function(cond, rep) {
                            if (cond$x == 2) stop("cell exploded")
                            cond$x * 10
                          })
  expect_identical(res$error[2], "cell exploded")
  expect_null(res$result[[2]])
  expect_equal(res$result[[3]], 30)
})
