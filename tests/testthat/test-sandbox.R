# Confined (sandboxed) tools. All offline: every test injects an executor=
# through the seam, so nothing spawns a process or a container and callr is not
# needed. The fake executors honour the contract
# executor(fn, args, workdir, timeout_s) ->
#   list(stdout, result, files, status, error).

# A reusable fake executor that just runs the function in-process and reports
# success with no files written.
ok_executor <- function(fn, args, workdir, timeout_s) {
  list(stdout = "", result = do.call(fn, args),
       files = character(0), status = "ok", error = NA)
}

test_that("sandbox_tool is an llmr_tool that runs through an injected executor", {
  t <- sandbox_tool(
    function(x) x * 2, name = "double", description = "d",
    parameters = list(x = list(type = "number")),
    mode = "tempdir", executor = ok_executor)

  expect_s3_class(t, "llmr_tool")
  expect_identical(t$name, "double")

  out <- t$fn(x = 21)
  expect_identical(as.character(out), "42")            # stringified result
  sb <- attr(out, "sandbox")
  expect_identical(sb$status, "ok")
  expect_identical(sb$mode, "tempdir")
})

test_that("an oversized result is truncated and flagged", {
  big <- paste(rep("x", 1000), collapse = "")
  t <- sandbox_tool(
    function() big, name = "big", description = "d",
    mode = "tempdir", max_bytes = 50,
    executor = function(fn, args, workdir, timeout_s)
      list(stdout = "", result = do.call(fn, args),
           files = character(0), status = "ok", error = NA))

  out <- t$fn()
  expect_match(out, "truncated")
  expect_lt(nchar(out, type = "bytes"), nchar(big, type = "bytes"))
  expect_identical(attr(out, "sandbox")$status, "ok")
})

test_that("a timeout from the executor is reported, not raised", {
  t <- sandbox_tool(
    function() "never returned", name = "slow", description = "d",
    mode = "tempdir", timeout_s = 1,
    executor = function(fn, args, workdir, timeout_s)
      list(stdout = "", result = NULL, files = character(0),
           status = "timeout", error = NA))

  out <- t$fn()
  expect_identical(attr(out, "sandbox")$status, "timeout")
  expect_match(out, "TIMEOUT|timeout|exceeded", ignore.case = TRUE)
})

test_that("a write outside the permitted paths is a sandbox violation", {
  t <- sandbox_tool(
    function() "tried to escape", name = "escape", description = "d",
    mode = "tempdir", allow_paths = NULL,
    executor = function(fn, args, workdir, timeout_s)
      list(stdout = "", result = do.call(fn, args),
           files = c("/etc/passwd" = "deadbeef"),    # outside workdir + allow
           status = "ok", error = NA))

  expect_error(t$fn(), class = "llmragent_sandbox_violation")
})

test_that("a reported write outside allow_paths fires the violation (executor seam)", {
  # The honest contract: the violation check applies to whatever the executor
  # REPORTS. When the executor=seam reports a file outside allow_paths -- as a
  # real container-mode executor would for an escaped write -- the violation
  # fires. (The default callr executor cannot see absolute-path writes; this is
  # exactly why the container-mode executor= seam is the documented boundary.)
  outside <- file.path(tempdir(), "escaped_out.txt")
  t <- sandbox_tool(
    function() "wrote outside", name = "escape2", description = "d",
    mode = "read_only", allow_paths = "/some/allowed/root",
    executor = function(fn, args, workdir, timeout_s)
      list(stdout = "", result = do.call(fn, args),
           files = stats::setNames("deadbeef", outside),   # reported, outside allow
           status = "ok", error = NA))

  err <- tryCatch(t$fn(), error = function(e) e)
  expect_s3_class(err, "llmragent_sandbox_violation")
  expect_true(outside %in% err$paths)
})

test_that("default-executor guarantee is workdir-scoped: a relative write under workdir is hashed and allowed", {
  # The default executor's honest guarantee is "confine and audit writes WITHIN
  # the scratch workdir". Emulate that here without spawning a process: an
  # executor that, like the default one, writes a RELATIVE path under the
  # workdir it was handed and reports it. In tempdir mode the workdir is always
  # permitted, so the write is hashed and allowed, not a violation.
  workdir_seen <- NULL
  scoped_executor <- function(fn, args, workdir, timeout_s) {
    workdir_seen <<- workdir
    rel <- file.path(workdir, "produced.txt")   # a write landing inside workdir
    writeLines("payload", rel)
    list(stdout = "", result = do.call(fn, args),
         files = rel, status = "ok", error = NA)  # bare path -> hashed by wrapper
  }
  t <- sandbox_tool(
    function() "ok", name = "producer", description = "d",
    mode = "tempdir", allow_paths = NULL, executor = scoped_executor)

  out <- t$fn()
  sb <- attr(out, "sandbox")
  expect_identical(sb$status, "ok")              # no violation: write is under workdir
  produced <- file.path(workdir_seen, "produced.txt")
  expect_true(produced %in% names(sb$out_hashes))
  expect_true(all(!is.na(sb$out_hashes)))        # the workdir write was hashed
})

test_that("a write inside allow_paths is permitted", {
  dir <- normalizePath(tempfile("allowed_"), winslash = "/", mustWork = FALSE)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(dir, recursive = TRUE, force = TRUE), add = TRUE)
  target <- file.path(dir, "out.txt")

  t <- sandbox_tool(
    function() "wrote in allowed dir", name = "writer", description = "d",
    mode = "read_only", allow_paths = dir,
    executor = function(fn, args, workdir, timeout_s)
      list(stdout = "", result = do.call(fn, args),
           files = stats::setNames("cafef00d", target),
           status = "ok", error = NA))

  out <- t$fn()
  expect_identical(attr(out, "sandbox")$status, "ok")
  expect_true(target %in% names(attr(out, "sandbox")$out_hashes))
})

test_that("container mode without an executor errors about the executor", {
  expect_error(
    sandbox_tool(function() 1, name = "c", description = "d", mode = "container"),
    "container|executor")
})

test_that("governance records side effects and the sandbox mode", {
  t_w <- sandbox_tool(function() 1, name = "w", description = "d",
                      mode = "tempdir", executor = ok_executor)
  gov_w <- attr(t_w, "governance")
  expect_identical(gov_w$side_effects, "write")
  expect_identical(gov_w$sandbox$mode, "tempdir")
  expect_false(gov_w$requires_approval)

  t_r <- sandbox_tool(function() 1, name = "r", description = "d",
                      mode = "read_only", allow_paths = "/tmp/ok",
                      executor = ok_executor)
  gov_r <- attr(t_r, "governance")
  expect_identical(gov_r$side_effects, "read")
  expect_identical(gov_r$sandbox$mode, "read_only")
  expect_identical(gov_r$sandbox$allow_paths, "/tmp/ok")
})

test_that("an existing llmr_tool can be re-confined, reusing its name", {
  base <- LLMR::llm_tool(
    function(x) x + 1, name = "inc", description = "increments x",
    parameters = list(x = list(type = "number")))
  t <- sandbox_tool(base, mode = "tempdir", executor = ok_executor)

  expect_s3_class(t, "llmr_tool")
  expect_identical(t$name, "inc")
  expect_identical(as.character(t$fn(x = 4)), "5")
})

test_that("input file arguments are hashed for provenance", {
  f <- tempfile(fileext = ".txt")
  writeLines("some input", f)
  on.exit(unlink(f), add = TRUE)

  t <- sandbox_tool(
    function(path) "read it", name = "reader", description = "d",
    parameters = list(path = list(type = "string")),
    mode = "read_only", executor = ok_executor)

  out <- t$fn(path = f)
  in_h <- attr(out, "sandbox")$in_hashes
  expect_true(normalizePath(f, winslash = "/", mustWork = FALSE) %in%
                normalizePath(names(in_h), winslash = "/", mustWork = FALSE) |
                f %in% names(in_h))
  expect_true(all(!is.na(in_h)))
})
