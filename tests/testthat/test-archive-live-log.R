# archive_agent_study() copies only this run's records from a live LLMR audit
# log, and never the whole log, which can hold other studies' calls.

unrelated_line <- paste0(
  '{"ts":"2026-06-01T10:00:01+0000","schema_version":"1.0","kind":"call",',
  '"provider":"openai","model":"other","status":200,"request":{"messages":',
  '[{"role":"user","content":"UNRELATED SECRET STUDY"}],"temperature":0},',
  '"usage":{"sent":5,"rec":1},"response_id":"x-1","text":"unrelated reply"}')

private_run <- function() {
  a <- fake_agent("Aria", "private reply")
  a$chat("my private question")
  as_agent_run(a)
}

own_lines <- function(run) {
  f <- tempfile(fileext = ".jsonl")
  on.exit(unlink(f), add = TRUE)
  .write_audit_calls(run, f)
  readLines(f)
}

archive_with_log <- function(run, lines, ...) {
  live <- tempfile(fileext = ".jsonl")
  writeLines(lines, live)
  run$llmr_log <- live
  dir <- tempfile("arc")
  arc <- archive_agent_study(run, dir, ...)
  list(arc = arc, calls = readLines(file.path(dir, "calls.jsonl")))
}

test_that("a live log that cannot be read is not copied", {
  run <- private_run()
  lines <- c(unrelated_line, own_lines(run), '{"ts":"2026-06-01T10:00:0')
  expect_warning(out <- archive_with_log(run, lines, include_messages = FALSE),
                 "could not be read")
  expect_false(any(grepl("UNRELATED SECRET", out$calls)))
  expect_identical(out$arc$calls_source, "audit_from_spans")
})

test_that("a run without hashed calls does not pull the live log in", {
  ex <- agent_experiment(data.frame(x = 1), quiet = TRUE,
                         run_fn = function(cond, rep) "reply text")
  run <- suppressWarnings(as_agent_run(ex))
  expect_warning(out <- archive_with_log(run, unrelated_line), "no request hashes")
  expect_false(any(grepl("UNRELATED SECRET", out$calls)))
})

test_that("an identical earlier request with another response id is left out", {
  run <- private_run()
  own <- own_lines(run)
  pilot <- sub('"response_id":"[^"]*"', '"response_id":"pilot-1"', own)
  out <- archive_with_log(run, c(pilot, own, unrelated_line))
  expect_identical(out$calls, own)
  expect_identical(out$arc$calls_source, "live_llmr_log")
})

test_that("a readable live log yields this run's records verbatim", {
  run <- private_run()
  own <- own_lines(run)
  out <- archive_with_log(run, c(unrelated_line, own))
  expect_identical(out$calls, own)
})

test_that("a privacy scrub that cannot parse its file removes it", {
  run <- private_run()
  live <- tempfile(fileext = ".jsonl")
  writeLines(c(unrelated_line, own_lines(run)), live)
  run$llmr_log <- live
  dir <- tempfile("arc")
  real_read <- LLMR::llm_log_read
  n <- 0L
  with_stub_llmr("llm_log_read", function(log) {
    n <<- n + 1L
    if (n > 1L) stop("cannot parse") else real_read(log)
  }, {
    expect_warning(arc <- archive_agent_study(run, dir, include_messages = FALSE),
                   "privacy options could not be applied")
  })
  expect_false(file.exists(file.path(dir, "calls.jsonl")))
  expect_false("calls.jsonl" %in% arc$files)
})

test_that("copied records the run cannot vouch for are reported", {
  run <- private_run()
  own <- own_lines(run)
  # Two log records share the run's request hash and neither has a response id,
  # as when a pilot sent the identical request before the run: both are kept,
  # which is more records than the run made calls.
  no_id <- sub('"response_id":"[^"]*",?', "", own)
  expect_warning(out <- archive_with_log(run, c(no_id, no_id, unrelated_line)),
                 "keeps 2 record\\(s\\) .* for 1 call\\(s\\).*2 of the copied record\\(s\\) were matched on request hash alone")
  expect_length(out$calls, 2L)
  # The run's own calls lacking response ids: matching falls back to the hash.
  run2 <- private_run()
  own2 <- own_lines(run2)
  run2$spans[[1]]$meta$record$response_id <- NA_character_
  expect_warning(archive_with_log(run2, c(own2, unrelated_line)),
                 "1 of the copied record\\(s\\) were matched on request hash alone")
  # One record per call, each matched on its response id: no warning.
  expect_no_warning(archive_with_log(run, c(own, unrelated_line)))
})
