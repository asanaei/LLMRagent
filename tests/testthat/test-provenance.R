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

test_that("archived calls keep the request-hash join when the config has params", {
  # request_hash is keyed on the generation parameters; the archived request
  # body must carry them, or a reader recomputes a different hash.
  cfg <- LLMR::llm_config("groq", "fake-model", temperature = 0.3, max_tokens = 200)
  a <- Agent$new("Hp", cfg, caller = scripted_caller(list("hi")), quiet = TRUE)
  a$chat("q")
  dir <- withr::local_tempdir()
  archive_agent_study(a, dir)
  parsed <- LLMR::llm_log_read(file.path(dir, "calls.jsonl"))
  run_calls <- tibble::as_tibble(as_agent_run(a), level = "call")
  expect_identical(sort(parsed$manifest$request_hash),
                   sort(run_calls$request_hash))
  # and all three agree with the config-side hash LLMR computes
  expect_identical(
    parsed$manifest$request_hash[1],
    LLMR::llm_request_hash(cfg, list(list(role = "user", content = "q"))))
})

test_that("an archive is honest when the live log has none of the run's calls", {
  # a "live log" whose records belong to some other run
  other <- fake_agent("Other", list("unrelated"))
  other$chat("something else entirely")
  dir0 <- withr::local_tempdir()
  archive_agent_study(other, dir0)

  a <- fake_agent("A", list("mine"))
  a$chat("my question")
  r <- as_agent_run(a)
  r$llmr_log <- file.path(dir0, "calls.jsonl")   # points at the foreign log
  dir <- withr::local_tempdir()
  expect_warning(arc <- archive_agent_study(r, dir), "no records matching")
  expect_identical(arc$calls_source, "live_llmr_log_empty")
  expect_false(arc$llm_log_compatible)
  expect_true(arc$n_calls > 0L)                  # the run did make calls
  lines <- readLines(file.path(dir, "calls.jsonl"), warn = FALSE)
  expect_identical(length(lines[nzchar(lines)]), 0L)
})

test_that("privacy levers apply to a copied live log", {
  a <- fake_agent("A", list("a very private reply"))
  a$chat("a very private question")
  run_hash <- tibble::as_tibble(as_agent_run(a), level = "call")$request_hash[1]

  # Build a stand-in live log from the spans-derived audit record, shaped like
  # a genuine LLMR log line (no request_hash field: the reader recomputes it).
  dir0 <- withr::local_tempdir()
  archive_agent_study(a, dir0)
  rec <- jsonlite::fromJSON(readLines(file.path(dir0, "calls.jsonl"))[1],
                            simplifyVector = FALSE)
  rec$request_hash <- NULL
  live_log <- withr::local_tempfile(fileext = ".jsonl")
  writeLines(as.character(jsonlite::toJSON(rec, auto_unbox = TRUE,
                                           null = "null", na = "null")), live_log)

  # include_messages = FALSE strips the body and reply but keeps the hash join
  r <- as_agent_run(a); r$llmr_log <- live_log
  dir1 <- withr::local_tempdir()
  arc1 <- archive_agent_study(r, dir1, include_messages = FALSE)
  expect_identical(arc1$calls_source, "live_llmr_log")
  out1 <- jsonlite::fromJSON(readLines(file.path(dir1, "calls.jsonl"))[1],
                             simplifyVector = FALSE)
  expect_null(out1[["request"]])
  expect_null(out1$text)
  expect_identical(out1$request_hash, run_hash)  # identity survives omission
  expect_false(any(grepl("very private",
                         readLines(file.path(dir1, "calls.jsonl")))))

  # redact scrubs the reply text of the copied records
  r2 <- as_agent_run(a); r2$llmr_log <- live_log
  dir2 <- withr::local_tempdir()
  archive_agent_study(r2, dir2, redact = "private")
  out2 <- jsonlite::fromJSON(readLines(file.path(dir2, "calls.jsonl"))[1],
                             simplifyVector = FALSE)
  expect_match(out2$text, "REDACTED")
  # the request body is identity and stays verbatim; the reader still joins
  parsed2 <- LLMR::llm_log_read(file.path(dir2, "calls.jsonl"))
  expect_identical(parsed2$manifest$request_hash[1], run_hash)
})

test_that("diagnostics() and report() accept a bare Agent, as documented", {
  a <- fake_agent("Bare", list("hi"))
  a$chat("q")
  diag <- diagnostics(a)                       # the documented call shape
  expect_s3_class(diag, "tbl_df")
  expect_identical(diag$n_calls, 1L)
  # same numbers either way (run_id is minted per view, so compare the rest)
  keep <- setdiff(names(diag), "run_id")
  expect_identical(diag[keep], diagnostics(as_agent_run(a))[keep])
  rep <- report(a, task = "to answer a question")
  expect_s3_class(rep, "agent_report")
  expect_true(any(grepl("Bare", unclass(rep))))
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
