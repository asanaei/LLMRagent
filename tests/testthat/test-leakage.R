# State-leakage diagnostic across experiment cells. All offline via fake_agent:
# a fresh agent per cell is clean; one agent built outside run_fn and reused is
# the leak the check exists to catch.

test_that("fresh-per-cell agents leave no leakage", {
  design <- data.frame(x = c(1, 2))
  run_fn <- function(cond, rep) {
    a <- fake_agent("X", list("ok"))
    a$chat("q")
    a
  }
  res <- agent_experiment(design, run_fn, reps = 1, quiet = TRUE)
  rep <- check_state_leakage(res)
  expect_s3_class(rep, "agent_leakage_report")
  expect_true(rep$clean)
  expect_identical(nrow(rep$leaks), 0L)
  expect_identical(rep$n_cells, nrow(res))
})

test_that("an agent shared across cells is flagged with its id", {
  design <- data.frame(x = c(1, 2))
  shared <- fake_agent("S", list("a", "b", "c", "d"))
  run_fn <- function(cond, rep) {
    shared$chat("q")
    shared
  }
  res <- agent_experiment(design, run_fn, reps = 1, quiet = TRUE)
  rep <- check_state_leakage(res)
  expect_s3_class(rep, "agent_leakage_report")
  expect_false(rep$clean)
  expect_true(nrow(rep$leaks) >= 1L)

  # the shared id is named, spanning the two cells
  expect_true(shared$id() %in% rep$leaks$agent_id)
  expect_true("shared_agent_instance" %in% rep$leaks$kind)
  shared_rows <- rep$leaks[rep$leaks$kind == "shared_agent_instance", ]
  expect_identical(shared_rows$cell_i, 1L)
  expect_identical(shared_rows$cell_j, 2L)

  # memory bleeds too: cell 2's memory carries cell 1's content
  expect_true("memory_bleed" %in% rep$leaks$kind)
})

test_that("the report prints both clean and leaky states", {
  design <- data.frame(x = c(1, 2))
  clean <- check_state_leakage(
    agent_experiment(design, quiet = TRUE,
                     run_fn = function(cond, rep) { a <- fake_agent("X", list("ok")); a$chat("q"); a }))
  # call the method directly so the test does not depend on the regenerated
  # NAMESPACE S3 registration that a full build (document()) provides.
  out_clean <- paste(utils::capture.output(print.agent_leakage_report(clean)),
                     collapse = "\n")
  expect_true(grepl("clean: TRUE", out_clean, fixed = TRUE))
  expect_true(grepl("No shared agents", out_clean))

  shared <- fake_agent("S", list("a", "b", "c", "d"))
  leaky <- check_state_leakage(
    agent_experiment(design, quiet = TRUE,
                     run_fn = function(cond, rep) { shared$chat("q"); shared }))
  out_leaky <- paste(utils::capture.output(print.agent_leakage_report(leaky)),
                     collapse = "\n")
  expect_true(grepl("clean: FALSE", out_leaky, fixed = TRUE))
  expect_true(grepl("shared_agent_instance", out_leaky))
})

test_that("a plain list of agents is checked as one cell each", {
  a <- fake_agent("A", list("ok")); a$chat("q")
  b <- fake_agent("B", list("ok")); b$chat("q")
  clean <- check_state_leakage(list(a, b))
  expect_true(clean$clean)
  expect_identical(clean$n_cells, 2L)

  shared <- fake_agent("S", list("a", "b")); shared$chat("q")
  leaky <- check_state_leakage(list(shared, shared))
  expect_false(leaky$clean)
  expect_true(shared$id() %in% leaky$leaks$agent_id)
})

test_that("non-runnable cells are skipped, not mistaken for leaks", {
  design <- data.frame(x = c(1, 2, 3))
  res <- agent_experiment(design, quiet = TRUE,
                          run_fn = function(cond, rep) cond$x * 10)  # plain numbers
  rep <- check_state_leakage(res)
  expect_true(rep$clean)
  expect_identical(rep$n_cells, 3L)
})

test_that("a non-experiment, non-list input errors clearly", {
  expect_error(check_state_leakage(42), "agent_experiment")
})
