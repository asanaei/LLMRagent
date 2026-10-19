test_that("manifest construction failure does not publish a substitute", {
  local_mocked_bindings(agent_manifest = function(...) stop("manifest unavailable"))
  r <- as_agent_run(fake_agent("A", list("reply")))
  path <- withr::local_tempdir()
  expect_warning(arc <- archive_agent_study(r, path), "manifest unavailable")
  expect_false(file.exists(file.path(path, "manifest.json")))
  expect_false("manifest.json" %in% arc$files)
  expect_true("manifest.json" %in% arc$write_failures$file)
})

archive_fixture <- function() {
  a <- fake_agent("A", list("reply"))
  a$chat("question")
  as_agent_run(a)
}

expect_archive_failure <- function(arc, file, message) {
  expect_false(file %in% arc$files)
  expect_false(file.exists(file.path(arc$path, file)))
  expect_true(file %in% arc$write_failures$file)
  expect_match(arc$write_failures$error[match(file, arc$write_failures$file)], message)
}

test_that("each serialization failure removes its output and retains successes", {
  for (target in c("transcript.csv", "tools.csv", "state.csv", "events.jsonl",
                   "calls.jsonl", "run.rds", "README-methods.md", "manifest.json")) {
    local({
      file <- target
      lines <- .write_lines
      jsonl <- .write_jsonl
      audit <- .write_audit_calls
      csv <- utils::write.csv
      rds <- base::saveRDS
      fail <- function(dest) {
        writeLines("partial", dest)
        stop(paste("failed", file))
      }
      local_mocked_bindings(
        .write_lines = function(lines, file) {
          if (basename(file) == target) fail(file) else lines_writer(lines, file)
        },
        .write_jsonl = function(df, file, ...) {
          if (basename(file) == target) fail(file) else jsonl(df, file, ...)
        },
        .write_audit_calls = function(r, dest, ...) {
          if (basename(dest) == target) fail(dest) else audit(r, dest, ...)
        })
      lines_writer <- lines
      local_mocked_bindings(write.csv = function(x, file, ...) {
        if (basename(file) == target) fail(file) else csv(x, file, ...)
      }, .package = "utils")
      local_mocked_bindings(saveRDS = function(object, file, ...) {
        if (basename(file) == target) fail(file) else rds(object, file, ...)
      }, .package = "base")
      path <- withr::local_tempdir()
      writeLines("older output", file.path(path, file))
      writeLines("unrelated", file.path(path, "keep.txt"))
      expect_warning(arc <- archive_agent_study(archive_fixture(), path, overwrite = TRUE),
                     paste("failed", file))
      expect_archive_failure(arc, file, paste("failed", file))
      expect_true("hashes.sha256" %in% arc$files)
      expect_identical(readLines(file.path(path, "keep.txt")), "unrelated")
      expect_true(all(file.exists(file.path(path, arc$files))))
    })
  }
})

test_that("manifest serialization failure differs from construction failure", {
  r <- archive_fixture()
  original <- jsonlite::toJSON
  local_mocked_bindings(toJSON = function(x, ...) {
    if (is.list(x) && "models_basis" %in% names(x)) stop("manifest serialization failed")
    original(x, ...)
  }, .package = "jsonlite")
  expect_warning(arc <- archive_agent_study(r, withr::local_tempdir()), "manifest serialization failed")
  expect_archive_failure(arc, "manifest.json", "serialization")
  expect_false(is.na(arc$manifest_hash))
})

test_that("artifact failures do not discard preceding or following artifacts", {
  r <- archive_fixture()
  r$artifacts <- list(first = data.frame(x = 1L), bad = list(x = 0L), last = list(x = 2L))
  writer <- .write_lines
  local_mocked_bindings(.write_lines = function(lines, file) {
    if (basename(file) == "bad.json") stop("artifact write failed")
    writer(lines, file)
  })
  expect_warning(arc <- archive_agent_study(r, withr::local_tempdir()), "artifacts/bad.json")
  expect_archive_failure(arc, "artifacts/bad.json", ".+")
  expect_true(all(c("artifacts/first.csv", "artifacts/last.json", "hashes.sha256") %in% arc$files))
})

test_that("seals cover final README bytes and invisibility is retained", {
  visible <- withVisible(archive_agent_study(archive_fixture(), withr::local_tempdir()))
  expect_false(visible$visible)
  arc <- visible$value
  expect_identical(arc$write_failures, tibble::tibble(file = character(), error = character()))
  inventory <- readLines(file.path(arc$path, "README-methods.md"))
  expect_true(all(paste0("- ", arc$files) %in% inventory))
  seal <- readLines(file.path(arc$path, "hashes.sha256"))
  expect_identical(substr(seal, 1L, 64L), unname(vapply(
    file.path(arc$path, substring(seal, 67L)), .file_sha256, "")))
})

test_that("hash and seal-write failures remove old seals and finalize inventory", {
  for (mode in c("hash", "write")) local({
    path <- withr::local_tempdir()
    writeLines("stale seal", file.path(path, "hashes.sha256"))
    writer <- .write_lines
    hasher <- .file_sha256
    local_mocked_bindings(
      .file_sha256 = function(f) { if (mode == "hash") stop("hash failed"); hasher(f) },
      .write_lines = function(lines, file) {
        if (mode == "write" && basename(file) == "hashes.sha256") {
          writeLines("partial", file)
          stop("seal write failed")
        }
        writer(lines, file)
      })
    expect_warning(arc <- archive_agent_study(archive_fixture(), path, overwrite = TRUE), "failed")
    expect_archive_failure(arc, "hashes.sha256", "failed")
    expect_false(any(grepl("hashes.sha256", readLines(file.path(path, "README-methods.md")), fixed = TRUE)))
    expect_false(any(grepl("sealed by", capture.output(print(arc)), fixed = TRUE)))
  })
})

test_that("a failed README rewrite removes its previous seal claim", {
  real <- .write_readme
  n <- 0L
  local_mocked_bindings(.file_sha256 = function(f) stop("hash failed"),
    .write_readme = function(...) {
      n <<- n + 1L
      if (n == 2L) stop("rewrite failed")
      real(...)
    })
  expect_warning(arc <- archive_agent_study(archive_fixture(), withr::local_tempdir()), "rewrite failed")
  expect_archive_failure(arc, "README-methods.md", "rewrite failed")
  expect_archive_failure(arc, "hashes.sha256", "hash failed")
})

test_that("checksums reject missing or unreadable inputs and accept empty files", {
  path <- tempfile(tmpdir = tempdir())
  expect_error(.file_sha256(path), "file is missing")
  writeLines(character(), path)
  expect_identical(.file_sha256(path), digest::digest(raw(), algo = "sha256", serialize = FALSE))
  local_mocked_bindings(readBin = function(...) stop("read failed"), .package = "base")
  expect_error(.file_sha256(path), "read failed")
})

test_that("archive extraction failures omit only dependent outputs", {
  for (level in c("utterance", "event", "call", "tool", "state")) local({
    accessor <- as_tibble.agent_run
    local_mocked_bindings(as_tibble.agent_run = function(x, ..., level) {
      if (level == target) stop(paste(target, "unavailable"))
      accessor(x, level = level)
    })
    target <- level
    file <- c(utterance = "transcript.csv", event = "events.jsonl", call = "calls.jsonl",
              tool = "tools.csv", state = "state.csv")[[level]]
    arc <- suppressWarnings(archive_agent_study(archive_fixture(), withr::local_tempdir()))
    expect_archive_failure(arc, file, paste(level, "unavailable"))
    expect_archive_failure(arc, "run.rds", paste(level, "unavailable"))
    expect_identical(is.na(arc$n_calls), level == "call")
    expect_true("hashes.sha256" %in% arc$files)
  })
  empty <- archive_agent_study(as_agent_run(fake_agent("empty", list("x"))), withr::local_tempdir())
  expect_identical(empty$n_calls, 0L)
  expect_true(all(c("calls.jsonl", "events.jsonl", "run.rds") %in% empty$files))
  expect_identical(nrow(empty$write_failures), 0L)
})

test_that("redaction failure after log selection removes the copied file", {
  r <- archive_fixture()
  live <- tempfile(tmpdir = tempdir())
  .write_audit_calls(r, live)
  r$llmr_log <- live
  ready <- FALSE
  select <- .filter_log_to_hashes
  local_mocked_bindings(.filter_log_to_hashes = function(...) {
    n <- select(...)
    ready <<- TRUE
    n
  })
  redactor <- function(x) { if (ready) stop("redactor failed"); x }
  expect_warning(arc <- archive_agent_study(r, withr::local_tempdir(), redact = redactor),
                 "privacy options could not be applied.*redactor failed")
  expect_archive_failure(arc, "calls.jsonl", "redactor failed")
  expect_identical(arc$calls_source, "none")
  expect_false(arc$llm_log_compatible)
})

test_that("a writer that produces no file cannot claim an older output", {
  local_mocked_bindings(.write_jsonl = function(...) invisible(NULL))
  path <- withr::local_tempdir()
  writeLines("old events", file.path(path, "events.jsonl"))
  expect_warning(arc <- archive_agent_study(archive_fixture(), path, overwrite = TRUE),
                 "writer did not produce a file")
  expect_archive_failure(arc, "events.jsonl", "writer did not produce a file")
})

test_that("invalid checksums fail the seal and failed calls can still be archived", {
  r <- archive_fixture()
  r$spans[[1]]$meta$record <- LLMR::llm_response_record(simpleError("call failed"))
  r$spans[[1]]$status <- "error"
  arc <- archive_agent_study(r, withr::local_tempdir())
  expect_true("hashes.sha256" %in% arc$files)
  expect_identical(arc$n_calls, 1L)
  local_mocked_bindings(.file_sha256 = function(f) NA_character_)
  expect_warning(arc <- archive_agent_study(r, withr::local_tempdir()), "Invalid checksum")
  expect_archive_failure(arc, "hashes.sha256", "Invalid checksum")
})
