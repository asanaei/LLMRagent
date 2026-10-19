# agent_experiment() keeps the researcher's design intact, and converting an
# experiment records what happened to every cell.

test_that("design columns the result would overwrite are refused before running", {
  ran <- 0L
  for (nm in c("rep", "result", "error", "duration")) {
    design <- data.frame(x = 1:2)
    design[[nm]] <- c("a", "b")
    expect_error(
      agent_experiment(design, quiet = TRUE,
                       run_fn = function(cond, rep) { ran <<- ran + 1L; 1 }),
      sprintf("'%s'", nm))
  }
  expect_identical(ran, 0L)
})

test_that("reps must be one positive whole number", {
  bad <- list(0, -2, 2.7, NA, NA_real_, c(1, 2), "3", Inf, -Inf, NaN, 1e10,
              NULL, TRUE)
  shown <- c("0", "-2", "2.7", "NA", "NA_real_", "c(1, 2)", '"3"', "Inf",
             "-Inf", "NaN", "1e+10", "NULL", "TRUE")
  ran <- 0L
  for (i in seq_along(bad)) {
    expect_error(agent_experiment(data.frame(x = 1), quiet = TRUE, reps = bad[[i]],
                                  run_fn = function(cond, rep) { ran <<- ran + 1L; 1 }),
                 paste0("`reps` must be one positive whole number, not ", shown[i], "."),
                 fixed = TRUE)
  }
  expect_identical(ran, 0L)
  expect_error(agent_experiment(data.frame(x = 1), quiet = TRUE, reps = 1:100,
                                run_fn = function(cond, rep) 1),
               "not 1:100.", fixed = TRUE)
  res <- agent_experiment(data.frame(x = 1), quiet = TRUE, reps = 2,
                          run_fn = function(cond, rep) rep)
  expect_identical(res$rep, 1:2)
})

test_that("converting an experiment records a status for every cell", {
  design <- data.frame(x = c(1, 2, 3))
  res <- agent_experiment(design, quiet = TRUE, run_fn = function(cond, rep) {
    if (cond$x == 2) stop("cell exploded")
    if (cond$x == 3) {
      a <- fake_agent("A", "hello")
      a$reply("hi")
      return(a)
    }
    "plain reply text"
  })
  expect_warning(run <- as_agent_run(res),
                 "2 of 3 experiment cell\\(s\\) contributed no events")
  cells <- run$artifacts$cells
  expect_identical(cells$cell, 1:3)
  expect_identical(cells$status, c("no_provenance", "failed", "converted"))
  expect_identical(cells$error[2], "cell exploded")
  expect_match(cells$cell_note[1], "character")
  expect_identical(names(run$artifacts$design), c("x", "rep"))
})

test_that("a design factor named like a status column is left untouched", {
  design <- data.frame(status = c("treated", "control"))
  res <- agent_experiment(design, quiet = TRUE, run_fn = function(cond, rep) "text")
  run <- suppressWarnings(as_agent_run(res))
  expect_identical(run$artifacts$design$status, c("treated", "control"))
})
