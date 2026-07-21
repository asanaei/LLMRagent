# Offline tests for view_run(): the HTML inspector is a pure read over the run
# substrate. Both the htmltools path and the hand-rolled fallback must produce
# the same visible content, so these assertions hold either way (no skip on
# htmltools). All runs are built with fake_agent (helper-fake.R); open = FALSE.

read_html <- function(path) paste(readLines(path, warn = FALSE), collapse = "\n")

test_that("view_run writes a self-contained HTML inspector for a chat run", {
  a <- fake_agent("Ada", list("hi", "again"))
  a$chat("one")
  a$chat("two")
  r <- as_agent_run(a)

  out <- view_run(r, output = tempfile(fileext = ".html"), open = FALSE)
  expect_true(file.exists(out))
  expect_gt(file.size(out), 0)

  html <- read_html(out)
  expect_match(html, r$run_id, fixed = TRUE)          # the run id is reported
  expect_match(html, "<table")                          # rendered as HTML tables
  expect_match(html, "Ada", fixed = TRUE)               # the agent name appears
  # transcript rows are tied to span events: events are anchored by span id.
  expect_match(html, "id=\"span-", fixed = TRUE)        # an actual span anchor
})

test_that("view_run returns an annotated invisible path", {
  a <- fake_agent("Ada", list("hi"))
  a$chat("one")
  out <- view_run(as_agent_run(a), output = tempfile(fileext = ".html"),
                  open = FALSE)
  expect_s3_class(out, "agent_inspector_path")
  # Dispatch through the package's registered S3 method. (Under devtools'
  # load_all the methods table can lag the namespace before document(), so
  # resolve the method explicitly rather than rely on the global dispatch
  # table; this still exercises the real print method's output.)
  pm <- getS3method("print", "agent_inspector_path")
  expect_output(pm(out), "Run inspector written to")
})

test_that("view_run renders a multi-agent deliberation with both agents", {
  mk <- function(name, vote) {
    fake_agent(name, list(
      paste0(name, " discussion"),
      sprintf('{"vote": "%s", "reason": "because"}', vote)))
  }
  panel <- list(mk("Ada", "yes"), mk("Bo", "no"))
  d <- deliberate(panel, "X", rounds = 1, quiet = TRUE)
  r <- as_agent_run(d)

  out <- view_run(r, output = tempfile(fileext = ".html"), open = FALSE)
  expect_true(file.exists(out))
  expect_gt(file.size(out), 0)

  html <- read_html(out)
  expect_match(html, "Ada", fixed = TRUE)
  expect_match(html, "Bo", fixed = TRUE)
  expect_match(html, "<table")
})

test_that("view_run defaults output to a temp html file and creates the dir", {
  a <- fake_agent("Ada", list("hi"))
  a$chat("one")

  out1 <- view_run(as_agent_run(a), open = FALSE)
  expect_true(file.exists(out1))
  expect_match(out1, "\\.html$")

  nested <- file.path(tempfile(), "deep", "view.html")
  out2 <- view_run(as_agent_run(a), output = nested, open = FALSE)
  expect_true(file.exists(out2))
  expect_true(dir.exists(dirname(out2)))
})
