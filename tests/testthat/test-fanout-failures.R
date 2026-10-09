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
