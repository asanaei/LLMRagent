# A failed fan-out worker is a failed call in the converted run, not a gap.

fanout_with_failed_worker <- function() {
  cfg <- LLMR::llm_config("groq", "cheap")
  workers <- tibble::tibble(
    approach = c("A", "B"),
    config = list(cfg, cfg),
    messages = list(c(user = "solve via A"), c(user = "solve via B")),
    response = list(fake_response("draft A"), NULL),
    success = c(TRUE, FALSE),
    error_message = c(NA_character_, "HTTP 500: server error"),
    response_text = c("draft A", NA_character_),
    sent_tokens = c(10L, NA_integer_), rec_tokens = c(5L, NA_integer_))
  structure(list(
    answer = "final", plan = tibble::tibble(title = c("A", "B"), instructions = c("a", "b")),
    workers = workers, verification = NULL, revised = FALSE,
    provenance = list(run_id = "run-fanout-test", kind = "agent_fanout_synthesis",
                      design = list(n_approaches = 2L, verify = FALSE,
                                    strong_model = "strong", cheap_model = "cheap"),
                      strong_calls = list(), created_at = "2026-10-09T12:00:00+0000",
                      llmr_log = NULL)),
    class = "agent_fanout_result")
}

test_that("a failed worker is counted as a failed call", {
  run <- as_agent_run(fanout_with_failed_worker())
  calls <- as_tibble(run, "call")
  expect_identical(nrow(calls), 2L)
  expect_identical(calls$success, c(TRUE, FALSE))
  expect_match(calls$error_message[2], "HTTP 500")
  expect_false(is.na(calls$request_hash[2]))
  d <- diagnostics(run)
  expect_identical(d$n_failed, 1L)
  expect_equal(d$ok_rate, 0.5)
})

test_that("worker construction failures and legacy workers remain observable", {
  x <- fanout_with_failed_worker()
  with_stub_llmr("llm_response_record", function(...) stop("worker record failed"), {
    warnings <- character()
    r <- withCallingHandlers(as_agent_run(x), warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    })
    expect_length(warnings, 2L)
    expect_true(all(grepl("worker record failed", warnings)))
  })
  ev <- as_tibble(r, "event")
  expect_identical(ev$status, c("unrecorded", "error"))
  expect_true(all(grepl("worker record failed", ev$note)))
  expect_error(as_tibble(r, "call"), "no readable response record")
  x$workers$response <- NULL
  x$workers$finish_reason <- c("stop", NA_character_)
  legacy <- as_agent_run(x)
  expect_identical(as_tibble(legacy, "call")$success, c(TRUE, FALSE))
  x$workers[c("finish_reason", "response_text", "sent_tokens", "rec_tokens")] <- NULL
  expect_warning(incomplete <- as_agent_run(x), "no stored response")
  expect_identical(as_tibble(incomplete, "event")$status[1], "unrecorded")
})


test_that("strong-call fallback failure preserves the synthesis and usage", {
  plan <- '{"approaches":[{"title":"A","instructions":"a"},{"title":"B","instructions":"b"}]}'
  n <- 0L
  with_stub_llmr("call_llm_robust", function(...) {
    n <<- n + 1L
    fake_response(if (n == 1L) plan else "final answer")
  }, {
    with_stub_llmr("call_llm_par", function(experiments, ...) {
      tibble::tibble(approach = experiments$approach, success = TRUE,
                     response_text = "draft", finish_reason = "stop")
    }, {
      with_stub_llmr("llm_response_record", function(...) stop("record failed twice"), {
        warnings <- character()
        out <- withCallingHandlers(agent_fanout_synthesis("q", LLMR::llm_config("groq", "strong"),
          LLMR::llm_config("groq", "cheap"), verify = FALSE, quiet = TRUE), warning = function(w) {
            warnings <<- c(warnings, conditionMessage(w))
            invokeRestart("muffleWarning")
          })
        expect_identical(n, 2L)
        expect_identical(out$answer, "final answer")
        expect_length(warnings, 2L)
        expect_true(all(grepl("record failed twice", warnings)))
      })
    })
  })
  r <- as_agent_run(out)
  events <- as_tibble(r, "event")
  expect_identical(events$status[events$agent_id == "strong"], rep("unrecorded", 2L))
  expect_identical(events$tokens_sent[events$agent_id == "strong"], rep(10L, 2L))
  expect_error(as_tibble(r, "call"), "no readable response record")
})
