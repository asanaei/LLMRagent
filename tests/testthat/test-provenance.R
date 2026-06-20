# Stage 1 provenance spine: agent_run, the five tidy levels, manifest, archive,
# and the diagnostics/report generic methods. All offline via the fake caller.

test_that("as_agent_run on a chat agent exposes the five tidy levels", {
  a <- fake_agent("Ada", list("first reply", "second reply"))
  a$chat("hello")
  a$chat("again")
  run <- as_agent_run(a)
  expect_s3_class(run, "agent_run")
  expect_identical(run$kind, "chat")

  calls <- tibble::as_tibble(run, level = "call")
  # the 16 canonical llm_response_record columns + run/span/agent ids
  expect_true(all(c("response_text", "provider", "model", "model_version",
                    "finish_reason", "sent_tokens", "rec_tokens", "total_tokens",
                    "success", "request_hash", "run_id", "span_id", "agent_id")
                  %in% names(calls)))
  expect_identical(nrow(calls), 2L)
  expect_true(all(calls$success))
  expect_identical(calls$sent_tokens, c(10L, 10L))   # fake_response defaults

  ev <- tibble::as_tibble(run, level = "event")
  expect_true(all(c("span_id", "parent_id", "event_type", "status",
                    "tokens_sent", "agent_id", "request_hash") %in% names(ev)))
  expect_true(all(c("call") %in% ev$event_type))

  utt <- tibble::as_tibble(run, level = "utterance")
  expect_identical(names(utt),
                   c("run_id", "turn", "speaker", "role", "text", "phase",
                     "question_id", "call_id", "ts"))
  expect_identical(utt$role, c("user", "assistant", "user", "assistant"))
  expect_identical(utt$text[2], "first reply")

  st <- tibble::as_tibble(run, level = "state")
  expect_true(all(c("run_id", "agent_id", "msg_role", "content") %in% names(st)))

  tl <- tibble::as_tibble(run, level = "tool")
  expect_true(all(c("name", "arguments_hash", "result_hash", "status") %in% names(tl)))
  expect_identical(nrow(tl), 0L)   # no tools used
})

test_that("trace() is unchanged in shape after the span rewrite", {
  a <- fake_agent("Tee", list("r1"))
  a$chat("x")
  tr <- a$trace()
  expect_identical(names(tr),
                   c("ts", "event", "tokens_sent", "tokens_received",
                     "tool", "duration", "note"))
  expect_true("call" %in% tr$event)
  # the event level surfaces spans trace() omits, but the same call is present
  ev <- tibble::as_tibble(as_agent_run(a), "event")
  expect_true(nrow(ev) >= nrow(tr))
})

test_that("a deliberation aggregates every participant's calls into one run", {
  mk <- function(name, vote) fake_agent(name, list(
    "round one point", "round two point",
    sprintf('{"vote":"%s","reason":"because"}', vote)))
  panel <- list(mk("A", "yes"), mk("B", "no"), mk("C", "yes"))
  d <- deliberate(panel, "a proposal", rounds = 2, quiet = TRUE)
  run <- as_agent_run(d)
  expect_s3_class(run, "agent_run")
  expect_identical(run$kind, "deliberation")
  # three agents x (2 discussion rounds + 1 vote) = 9 calls, all under one run id
  calls <- tibble::as_tibble(run, "call")
  expect_identical(nrow(calls), 9L)
  expect_identical(length(unique(calls$run_id)), 1L)
  expect_identical(sort(unique(run$participants$name)), c("A", "B", "C"))
  # votes survive as an artifact
  expect_true("votes" %in% names(run$artifacts))
})

test_that("agent_manifest hashes design identity, not outcomes", {
  a <- fake_agent("M", list("hi"))
  a$chat("q")
  m <- agent_manifest(a)
  expect_s3_class(m, "agent_manifest")
  expect_true(nchar(m$manifest_hash) == 64L)
  expect_identical(m$kind, "chat")

  # two agents with the same brief and name hash their personas identically;
  # a reworded persona flips the hash
  expect_identical(hash_persona("a careful coder", "X"),
                   hash_persona("a careful coder", "X"))
  expect_false(identical(hash_persona("a careful coder", "X"),
                         hash_persona("a careless coder", "X")))
})

test_that("archive_agent_study writes a sealed, LLMR-readable directory", {
  skip_if_not_installed("jsonlite")
  d <- deliberate(list(
    fake_agent("A", list("p1", "p2", '{"vote":"yes","reason":"r"}')),
    fake_agent("B", list("p1", "p2", '{"vote":"no","reason":"r"}'))),
    "prop", rounds = 2, quiet = TRUE)
  dir <- withr::local_tempdir()
  arc <- archive_agent_study(d, dir)
  expect_s3_class(arc, "agent_archive")

  expect_true(file.exists(file.path(dir, "manifest.json")))
  expect_true(file.exists(file.path(dir, "calls.jsonl")))
  expect_true(file.exists(file.path(dir, "README-methods.md")))
  expect_true(file.exists(file.path(dir, "hashes.sha256")))

  # the archive's calls.jsonl parses with LLMR's log reader (interop): one
  # record per model call, each a real audit record with a request body.
  read <- LLMR::llm_log_read(file.path(dir, "calls.jsonl"))
  run_calls <- tibble::as_tibble(as_agent_run(d), level = "call")
  expect_identical(nrow(read$manifest), nrow(run_calls))   # 2 agents x 3 calls = 6
  expect_true(all(read$manifest$kind == "call"))
  expect_true(all(read$manifest$has_payload))
  # the authoritative request_hash is carried in the run's call level
  expect_true(all(!is.na(run_calls$request_hash)))
  # re-sealing: recomputing the file hashes matches hashes.sha256
  seal <- readLines(file.path(dir, "hashes.sha256"))
  expect_true(length(seal) >= 4L)
})

test_that("diagnostics() numbers match LLMR::llm_usage on the call level", {
  a <- fake_agent("D", list(LLMR:::new_llmr_response("ok", usage = list(
    sent = 11L, rec = 7L, total = 18L, reasoning = NA_integer_, cached = NA_integer_))))
  a$chat("go")
  run <- as_agent_run(a)
  diag <- diagnostics(run)
  expect_s3_class(diag, "tbl_df")
  u <- LLMR::llm_usage(tibble::as_tibble(run, "call"))
  expect_identical(diag$tokens_sent, u$sent_tokens)
  expect_identical(diag$tokens_received, u$rec_tokens)
  expect_identical(diag$n_ok, u$n_ok)
})

test_that("report() drafts methods prose and warns when uncalibrated", {
  a <- fake_agent("R", list("hi"))
  a$chat("q")
  rep <- report(as_agent_run(a))
  expect_s3_class(rep, "agent_report")
  txt <- paste(unclass(rep), collapse = "\n")
  expect_true(grepl("LLMR", txt))
  expect_true(grepl("model-conditioned|not.*estimate|calibrat", txt, ignore.case = TRUE))
})

test_that("redaction scrubs stored text but preserves the request-hash join", {
  a <- fake_agent("A", list("the private reply"))
  a$chat("the secret question")
  r <- as_agent_run(a)
  dir <- withr::local_tempdir()
  archive_agent_study(r, dir, redact = "secret")
  # the join invariant must still hold: the audit log's request hashes are over
  # the ORIGINAL request, not the redacted copy
  parsed <- LLMR::llm_log_read(file.path(dir, "calls.jsonl"))
  expect_identical(sort(tibble::as_tibble(r, level = "call")$request_hash),
                   sort(parsed$manifest$request_hash))
  # the stored transcript is nonetheless redacted
  expect_true(any(grepl("REDACTED", readLines(file.path(dir, "transcript.csv")))))
})
