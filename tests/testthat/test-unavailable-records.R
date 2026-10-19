test_that("unreadable call spans give unavailable diagnostics", {
  a <- fake_agent("A", list("reply"))
  a$chat("question")
  r <- as_agent_run(a)
  r$spans[[1]]$meta$record <- NULL
  expect_error(tibble::as_tibble(r, "call"), "no readable response record")
  expect_warning(d <- diagnostics(r), "call.*level could not be read")
  expect_true(is.na(d$n_calls))
  expect_true(is.na(d$tokens_total))
  expect_true("n_unknown_tokens" %in% names(d))
  expect_identical(nrow(tibble::as_tibble(r, "event")), 1L)
})

test_that("agent accounting retains missing usage separately", {
  a <- fake_agent("A", list(fake_response(sent = NA_integer_, rec = 5L)))
  a$chat("question")
  expect_identical(a$usage()$tokens_sent, 0L)
  expect_identical(a$usage()$tokens_received, 5L)
  expect_identical(a$usage()$n_unknown_usage, 1L)
})

record_fixture <- function() {
  a <- fake_agent("A", list(fake_response()))
  a$chat("question")
  r <- as_agent_run(a)
  r$run_id <- "fixed-run"
  r$created_at <- "2026-01-01T00:00:00+0000"
  r
}

test_that("diagnostic levels fail independently and retain their schemas", {
  r <- record_fixture()
  expected <- names(diagnostics(r))
  for (level in c("call", "tool", "event")) local({
    nm <- c(call = ".run_calls", tool = ".run_tools", event = ".run_events")[[level]]
    args <- setNames(list(function(x) stop("extraction failed")), nm)
    do.call(local_mocked_bindings, c(args, list(.env = environment())))
    expect_warning(d <- diagnostics(r), paste0("'", level, "' level could not be read.*extraction failed"))
    expect_identical(names(d), expected)
    expect_identical(nrow(d), 1L)
    expect_identical(is.na(d$n_calls), level == "call")
    expect_identical(is.na(d$n_tool_calls), level == "tool")
    expect_identical(is.na(d$n_blocked), level == "event")
    if (level != "call") expect_identical(d$tokens_sent, 10L)
  })
})

test_that("usage summarization failure retains the readable row count", {
  r <- record_fixture()
  with_stub_llmr("llm_usage", function(...) stop("usage failed"), {
    expect_warning(d <- diagnostics(r), "usage could not be summarized.*usage failed")
    expect_identical(d$n_calls, 1L)
    expect_identical(d$n_ok, NA_integer_)
    expect_identical(d$ok_rate, NA_real_)
    expect_identical(d$tokens_total, NA_integer_)
    expect_identical(d$n_unknown_tokens, NA_integer_)
    expect_equal(d$duration_s, 0.01)
  })
  r$spans[[1]]$meta$record$duration_s <- NA_real_
  expect_identical(diagnostics(r)$duration_s, NA_real_)
  r$spans <- list()
  expect_equal(diagnostics(r)$duration_s, 0)
})

test_that("unknown usage follows LLMR's rule in diagnostics, manifests, and reports", {
  r <- record_fixture()
  row <- r$spans[[1]]
  # One row per hard case. Unknown: a total reported without its parts, one
  # part only, a failed call without usage, and the first row of a batch
  # without usage. Known: both parts without a total (the total is their
  # sum), a failed call that reported usage, and a complete row. Not counted:
  # a batch follow-on row and a skipped row.
  cases <- data.frame(
    sent    = c(NA, 10L, 10L, NA, 10L, NA, NA, 10L, NA),
    rec     = c(NA, NA, 5L, NA, 5L, NA, NA, 5L, NA),
    total   = c(15L, NA, NA, NA, 15L, NA, NA, 15L, NA),
    success = c(TRUE, TRUE, TRUE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE),
    finish  = c("stop", "stop", "stop", "error", "error", NA, "skipped", "stop", NA),
    batch_i = c(NA, NA, NA, NA, NA, 2L, NA, NA, 1L))
  r$spans <- lapply(seq_len(nrow(cases)), function(i) {
    s <- row
    s$span_id <- paste0("s", i)
    s$meta$record$sent_tokens <- cases$sent[i]
    s$meta$record$rec_tokens <- cases$rec[i]
    s$meta$record$total_tokens <- cases$total[i]
    s$meta$record$success <- cases$success[i]
    s$meta$record$finish_reason <- cases$finish[i]
    s$meta$record$rowpack_row <- cases$batch_i[i]
    s
  })
  calls <- tibble::as_tibble(r, "call")
  expect_identical(LLMR::llm_usage(calls)$n_unknown_tokens, 4L)
  expect_identical(.unknown_tokens(calls), 4L)
  expect_identical(diagnostics(r)$n_unknown_tokens, 4L)
  expect_identical(agent_manifest(r)$n_unknown_tokens, 4L)
  expect_true(any(grepl("4 call record(s) lack a sent, received, or total token count",
                        report(r), fixed = TRUE)))
  # The package's own count agrees with LLMR's on each case taken alone.
  for (i in seq_len(nrow(cases))) {
    one <- calls[i, , drop = FALSE]
    expect_identical(.unknown_tokens(one), LLMR::llm_usage(one)$n_unknown_tokens,
                     info = paste("case", i))
  }
  # A usage summary without the count falls back to the package's own.
  real <- LLMR::llm_usage
  with_stub_llmr("llm_usage", function(x) { out <- real(x); out$n_unknown_tokens <- NULL; out }, {
    expect_identical(diagnostics(r)$n_unknown_tokens, 4L)
  })
  r$spans <- r$spans[8]
  expect_identical(diagnostics(r)$n_unknown_tokens, 0L)
  # A record without token columns at all counts as unknown.
  r$spans[[1]]$meta$record[c("sent_tokens", "rec_tokens", "total_tokens")] <- NULL
  expect_identical(.unknown_tokens(tibble::as_tibble(r, "call")), 1L)
})

test_that("manifest metadata leaves the existing hashed body unchanged", {
  r <- record_fixture()
  r$pkg_versions <- list(LLMRagent = "0.8.2", LLMR = "0.8.12")
  r$participants$agent_id <- "fixed-agent"
  r$snapshot <- list(tools = tibble::tibble(), params = tibble::tibble())
  calls <- as_tibble(r, "call")
  body <- list(kind = r$kind, design = r$design,
    personas = r$participants[, c("name", "persona_hash"), drop = FALSE],
    tools = r$snapshot$tools, workflow = list(kind = r$kind, design = r$design),
    models = tibble::as_tibble(unique(as.data.frame(calls[, c("provider", "model", "model_version")]))),
    params = r$snapshot$params, pkg_versions = r$pkg_versions)
  m <- agent_manifest(r)
  expect_identical(m$manifest_hash, LLMR::llm_hash(body))
  expect_identical(m$models_basis, "calls")
  expect_identical(m$n_calls, 1L)
  r$spans <- list()
  expect_no_warning(empty <- agent_manifest(r))
  expect_identical(empty$n_calls, 0L)
  expect_identical(empty$models_basis, "participants")
  local_mocked_bindings(.run_calls = function(x) stop("cannot read calls"))
  expect_warning(missing <- agent_manifest(r), "Call records could not be read.*cannot read calls")
  expect_identical(missing$n_calls, NA_integer_)
  expect_identical(missing$tokens_sent, NA_integer_)
  expect_identical(missing$n_unknown_tokens, NA_integer_)
  expect_identical(missing$models_basis, "participants")
})

test_that("record construction failures retain replies across agent methods", {
  for (method in c("chat", "reply", "stream", "structured")) local({
    n <- 0L
    caller <- function(...) { n <<- n + 1L; fake_response('{"answer":"yes"}') }
    a <- Agent$new("A", LLMR::llm_config("groq", "fake"), quiet = TRUE,
                   caller = caller, stream_caller = caller)
    with_stub_llmr("llm_response_record", function(...) stop("record construction failed"), {
      expect_warning(out <- switch(method,
        chat = a$chat("q"), reply = a$reply("q"), stream = a$chat("q", stream = TRUE),
        structured = a$ask_structured("q", list(type = "object"))), "record construction failed")
    })
    expect_identical(n, 1L)
    if (method == "structured") expect_identical(out$answer, "yes") else expect_match(out, "yes")
    expect_match(a$last_response$text, "yes")
    expect_identical(a$usage()$tokens_total, 15L)
    expect_identical(a$usage()$n_unknown_usage, 0L)
    ev <- as_tibble(as_agent_run(a), "event")
    expect_identical(ev$status, "unrecorded")
    expect_match(ev$note, "record construction failed")
    expect_match(a$internal_spans()[[1]]$meta$record_error, "record construction failed")
  })
})

test_that("compaction recording failure retains the summary and the reply", {
  m <- memory_summary(threshold_chars = 50, keep_last = 2)
  a <- fake_agent("A", list(strrep("x", 40), strrep("y", 40), "reply"), memory = m)
  a$chat("first")
  a$chat("second")
  real <- LLMR::llm_response_record
  with_stub_llmr("call_llm_robust", function(...) fake_response("SUMMARY"), {
    with_stub_llmr("llm_response_record", function(resp, ...) {
      if (identical(resp$text, "SUMMARY")) stop("summary record failed")
      real(resp, ...)
    }, {
      expect_warning(out <- a$chat("third"), "summary record failed")
    })
  })
  expect_identical(out, "reply")
  expect_identical(a$usage()$calls, 4L)
  ev <- as_tibble(as_agent_run(a), "event")
  expect_identical(ev$status[ev$event_type == "compact"], "unrecorded")
  expect_true(any(grepl("SUMMARY", a$transcript()$content)))
})

test_that("compatible record schemas bind by name without adding optional model columns", {
  r <- record_fixture()
  r$spans[[1]]$meta$record$model_version <- NULL
  r$spans[[2]] <- r$spans[[1]]
  r$spans[[2]]$span_id <- "other"
  r$spans[[2]]$meta$record$optional_count <- 2L
  calls <- as_tibble(r, "call")
  expect_identical(calls$optional_count, c(NA_integer_, 2L))
  expect_false("model_version" %in% names(calls))
  expect_identical(names(agent_manifest(r)$models), c("provider", "model"))
  r$spans[[1]]$meta$record$sent_tokens <- "incompatible"
  expect_warning(d <- diagnostics(r), "call.*level could not be read")
  expect_identical(d$n_calls, NA_integer_)
})

test_that("saved accounting round-trips and old accounting loads as unknown without warning", {
  a <- fake_agent("A", list(fake_response(sent = NULL, rec = NA_integer_)))
  a$chat("q")
  expect_identical(nrow(a$trace()), 1L)
  expect_identical(a$trace()$tokens_sent, NA_integer_)
  path <- tempfile(tmpdir = tempdir(), fileext = ".rds")
  save_agent(a, path)
  expect_identical(load_agent(path)$usage()$n_unknown_usage, 1L)
  saved <- readRDS(path)
  saved$usage$n_unknown_usage <- NULL
  saveRDS(saved, path)
  expect_no_warning(old <- load_agent(path))
  expect_identical(old$usage()$n_unknown_usage, NA_integer_)
  expect_identical(old$usage()$tokens_total, 0L)
})

test_that("finite token budgets use recorded counts and warn when incomplete", {
  a <- fake_agent("A", list(fake_response(sent = NA_integer_, rec = 5L)),
                  budget = budget(max_tokens = 10L))
  a$chat("one")
  warnings <- character()
  withCallingHandlers(a$chat("two"), warning = function(w) {
    warnings <<- c(warnings, conditionMessage(w))
    invokeRestart("muffleWarning")
  })
  expect_length(warnings, 2L)
  expect_true(all(grepl("max_tokens is checked against recorded tokens only", warnings)))
  expect_identical(a$usage()$tokens_total, 10L)
  expect_warning(expect_error(a$chat("three"), class = "llmragent_budget_error"),
                 "recorded tokens only")
  expect_identical(a$usage()$calls, 2L)
  a$restore_accounting(list(calls = 0L, tokens_sent = 0L, tokens_received = 0L, tool_calls = 0L))
  warnings <- character()
  withCallingHandlers(a$chat("old history"), warning = function(w) {
    warnings <<- c(warnings, conditionMessage(w))
    invokeRestart("muffleWarning")
  })
  expect_true(length(warnings) > 0L)
  expect_true(all(grepl("recorded tokens only", warnings)))
})

test_that("reports and both inspector renderers show unavailable records", {
  r <- record_fixture()
  r$spans[[1]]$meta$record <- NULL
  expect_warning(report <- report(r), "call.*level could not be read")
  expect_true(any(grepl("call records could not be read", report, fixed = TRUE)))
  head <- suppressWarnings(.view_header(r))
  levels <- list(call = suppressWarnings(.view_level(r, "call")))
  expect_match(paste(.view_build_text(head, levels), collapse = "\n"), "unavailable")
  expect_match(.df_to_html_table(levels$call), "no readable response record")
  if (requireNamespace("htmltools", quietly = TRUE)) {
    levels$event <- as_tibble(r, "event")
    levels$utterance <- as_tibble(r, "utterance")
    levels$tool <- as_tibble(r, "tool")
    expect_match(.view_build_html_impl(head, levels), "unavailable")
  }
  local_mocked_bindings(llm_claim_lint = function(...) stop("lint failed"))
  expect_warning(report <- report(record_fixture()), "claim-language check could not be completed.*lint failed")
  expect_true(any(grepl("check could not be completed", report, fixed = TRUE)))
})

test_that("native loop unknown usage survives serialized approval without double counting", {
  tool_reply <- function(name, sent, rec) {
    resp <- fake_response("", sent = sent, rec = rec, finish = "tool")
    resp$raw <- list(choices = list(list(message = list(content = "", tool_calls = list(
      list(id = paste0("call_", name), type = "function",
           `function` = list(name = name, arguments = '{}')))))))
    resp
  }
  free <- agent_tool(function() "free", name = "free", description = "d")
  gated <- agent_tool(function() "approved", name = "gated", description = "d",
                      requires_approval = TRUE)
  replies <- list(tool_reply("free", 10L, 5L), tool_reply("gated", NA_integer_, 7L),
                  fake_response("done", sent = 2L, rec = NA_integer_))
  calls <- 0L
  a <- agent("A", LLMR::llm_config("groq", "fake"), tools = list(free, gated), quiet = TRUE)
  with_stub_llmr("call_llm_robust", function(...) {
    calls <<- calls + 1L
    replies[[calls]]
  }, {
    cp <- tryCatch(a$chat("q"), llmragent_pending_approval = function(e) e$checkpoint)
    expect_s3_class(cp, "llmragent_checkpoint")
    expect_identical(cp$agent_state$usage$n_unknown_usage, 1L)
    expect_identical(cp$agg$n_unknown_usage, 1L)
    expect_identical(cp$agg$accounted_unknown_usage, 1L)
    expect_identical(a$usage()$calls, 2L)
    file <- tempfile(tmpdir = tempdir(), fileext = ".rds")
    saveRDS(cp, file)
    out <- resume_run(approve_tool_call(readRDS(file), "approve"))
    expect_identical(out$text, "done")
    u <- out$agent$usage()
    expect_identical(u$calls, 3L)
    expect_identical(u$tokens_sent, 12L)
    expect_identical(u$tokens_received, 12L)
    expect_identical(u$n_unknown_usage, 2L)
    expect_identical(u$tool_calls, 2L)
    expect_identical(calls, 3L)
    save_agent(out$agent, file)
    expect_identical(load_agent(file)$usage()$n_unknown_usage, 2L)
  })
})

test_that("aggregate loops consume supplied unknown usage and report NA when it is absent", {
  resp <- fake_response()
  attr(resp, "tool_loop") <- list(model_calls = 3L, sent = 10L, rec = 5L,
                                   n_unknown_usage = 2L)
  a <- fake_agent("A", list(resp))
  expect_no_warning(a$chat("q"))
  expect_identical(a$usage()$n_unknown_usage, 2L)
  attr(resp, "tool_loop")$n_unknown_usage <- NULL
  b <- fake_agent("B", list(resp))
  expect_no_warning(b$chat("q"))
  expect_identical(b$usage()$n_unknown_usage, NA_integer_)
  expect_identical(b$usage()$tokens_total, 15L)
})

test_that("invalid durations give a warning and leave independent statistics intact", {
  r <- record_fixture()
  r$spans[[1]]$meta$record$duration_s <- "invalid duration"
  expect_warning(d <- diagnostics(r), "call.*durations could not be summarized")
  expect_identical(d$duration_s, NA_real_)
  expect_identical(d$n_calls, 1L)
  expect_identical(d$tokens_total, 15L)
})
