test_that("predicate errors stop selection before later branches", {
  later <- 0L
  wf <- agent_workflow("branch") |>
    add_node("source", function(s) s) |>
    add_node("target", function(s) { later <<- later + 1L; s }) |>
    add_edge("source", "target", function(s) missing_predicate_value) |>
    add_edge("source", "target")
  r <- run_workflow(wf)
  expect_identical(r$status, "failed")
  expect_match(r$error, "source -> target.*missing_predicate_value")
  expect_identical(later, 0L)
  expect_identical(r$steps$status, "ok")
})

test_that("predicate conditions identify the edge and preserve selection semantics", {
  wf <- agent_workflow("selection") |>
    add_node("a", identity) |> add_node("b", identity) |>
    add_edge("a", "b", function(s) stop("bad predicate"))
  expect_error(.wf_next(wf, "a", list()), "a -> b.*bad predicate",
               class = "llmragent_workflow_predicate_error")
  for (value in list(FALSE, NA, 1L, "TRUE", c(TRUE, TRUE), NULL)) {
    wf$edge_when <- list(function(s) value)
    expect_identical(.wf_next(wf, "a", list()), NA_character_)
  }
  wf <- add_edge(wf, "a", "b", function(s) stop("must not run"))
  wf$edge_when[1] <- list(function(s) TRUE)
  expect_identical(.wf_next(wf, "a", list()), "b")
  wf$edge_when[1] <- list(NULL)
  expect_identical(.wf_next(wf, "a", list()), "b")
})

test_that("edge failures save post-node state and resume without a node visit", {
  visits <- 0L
  ready <- FALSE
  wf <- agent_workflow("resume") |>
    add_node("source name", function(s) { visits <<- visits + 1L; s$n <- visits; s }) |>
    add_node("end", identity) |>
    add_edge("source name", "end", function(s) {
      if (!ready) stop("repair needed")
      TRUE
    })
  r <- run_workflow(wf)
  cursor <- jsonlite::fromJSON(file.path(r$checkpoint_dir, "cursor.json"))
  expect_identical(cursor$phase, "edge")
  expect_identical(cursor$step, 1L)
  expect_identical(cursor$source, "source name")
  log <- lapply(readLines(file.path(r$checkpoint_dir, "run.jsonl")), jsonlite::fromJSON)
  expect_identical(log[[2]]$status, "ok")
  expect_identical(log[[3]]$kind, "workflow_edge")
  expect_identical(log[[3]]$status, "failed")
  expect_identical(log[[3]]$from, "source name")
  expect_identical(log[[3]]$to, "end")
  expect_identical(log[[3]]$message, "repair needed")
  ready <- TRUE
  continued <- resume_workflow(r, wf = wf, max_steps = 2L)
  expect_identical(continued$status, "done")
  expect_identical(visits, 1L)
  expect_identical(continued$steps$node, "end")
  expect_identical(continued$state$n, 1L)
})

test_that("gate routing is evaluated once and repaired routing still pauses", {
  n <- 0L
  ready <- FALSE
  wf <- agent_workflow("gate") |>
    add_node("gate", human_gate("review")) |>
    add_node("first", identity) |> add_node("second", identity) |>
    add_edge("gate", "first", function(s) {
      n <<- n + 1L
      if (!ready) stop("gate routing unavailable")
      n %% 2L == 0L
    }) |> add_edge("gate", "second")
  failed <- run_workflow(wf)
  expect_identical(failed$status, "failed")
  expect_identical(n, 1L)
  ready <- TRUE
  paused <- resume_workflow(failed, wf = wf, approve = TRUE, max_steps = 2L)
  expect_identical(paused$status, "paused")
  expect_identical(n, 2L)
  cur <- jsonlite::fromJSON(file.path(paused$checkpoint_dir, "cursor.json"))
  expect_identical(cur$next_node, paused$checkpoint$next_node)
  expect_identical(cur$next_node, "first")
  done <- resume_workflow(paused$checkpoint_dir, wf = wf, max_steps = 2L)
  expect_identical(done$steps$node, "first")
  expect_identical(n, 2L)
  cur <- jsonlite::fromJSON(file.path(done$checkpoint_dir, "cursor.json"))
  expect_identical(cur$step, 2L)
  expect_null(cur$next_node)
  again <- resume_workflow(done, wf = wf)
  expect_identical(again$status, "done")
  expect_identical(nrow(again$steps), 0L)
})

test_that("legacy cursor snapshots use numeric step order and node retry", {
  path <- withr::local_tempdir()
  dir.create(file.path(path, "state"))
  saveRDS(list(n = 999L), file.path(path, "state", "999_a.rds"))
  saveRDS(list(n = 1000L), file.path(path, "state", "1000_a.rds"))
  jsonlite::write_json(list(run_id = "old", next_node = "a", step = 1000L,
                            status = "failed"), file.path(path, "cursor.json"), auto_unbox = TRUE)
  wf <- agent_workflow("old") |> add_node("a", function(s) { s$n <- s$n + 1L; s })
  r <- resume_workflow(path, wf = wf, max_steps = 1000L)
  expect_identical(r$state$n, 1001L)
  expect_identical(nrow(r$steps), 1L)
  writeLines("invalid RDS", file.path(path, "state", "1000_a.rds"))
  expect_error(resume_workflow(path, wf = wf), "state snapshot could not be read")
})

test_that("fork predicate failures belong to the branch directory", {
  ready <- TRUE
  wf <- agent_workflow("fork") |> add_node("a", identity) |> add_node("b", identity) |>
    add_edge("a", "b", function(s) { if (!ready) stop("fork failed"); TRUE })
  source <- run_workflow(wf)
  source_files <- list.files(source$checkpoint_dir, recursive = TRUE, full.names = TRUE)
  hashes <- vapply(source_files, .file_sha256, "")
  ready <- FALSE
  branch <- fork_workflow(source, wf, at = 1L)
  expect_identical(branch$status, "failed")
  expect_match(branch$error, "a -> b.*fork failed")
  expect_identical(nrow(branch$steps), 0L)
  cursor <- jsonlite::fromJSON(file.path(branch$checkpoint_dir, "cursor.json"))
  expect_identical(cursor$phase, "edge")
  expect_identical(vapply(source_files, .file_sha256, ""), hashes)
})

test_that("replay remains failed after a predicate error without an edge step", {
  wf <- agent_workflow("replay") |> add_node("a", identity) |> add_node("b", identity) |>
    add_edge("a", "b", function(s) stop("edge unavailable"))
  original <- run_workflow(wf)
  replayed <- replay_run(original, wf)
  expect_identical(replayed$status, "failed")
  expect_match(replayed$error, "edge unavailable")
  expect_identical(replayed$steps$node, "a")
  expect_identical(replayed$steps$replay_match, TRUE)
})

test_that("repeated gate edge failures do not create additional node visits", {
  wf <- agent_workflow("gate") |> add_node("review", human_gate("review")) |>
    add_node("end", identity) |> add_edge("review", "end", function(s) stop("routing failed"))
  first <- run_workflow(wf)
  second <- resume_workflow(first, wf = wf)
  expect_identical(second$status, "failed")
  expect_identical(nrow(second$steps), 0L)
  log <- .wf_read_log(second$checkpoint_dir)
  expect_identical(sum(log$node == "review" & log$status == "ok"), 1L)
})
