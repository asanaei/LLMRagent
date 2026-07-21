test_that("agent_fanout_synthesis runs plan, work, synthesis, and verification", {
  plan_json <- '{"approaches": [
    {"title": "approach A", "instructions": "try A"},
    {"title": "approach B", "instructions": "try B"}
  ]}'
  robust_calls <- 0L
  stub_robust <- function(config, messages, ...) {
    robust_calls <<- robust_calls + 1L
    switch(robust_calls,
      fake_response(plan_json),                                   # plan
      fake_response("the synthesized answer"),                    # synthesis
      fake_response('{"sound": false, "flaws": ["missing cost"]}'),# verify
      fake_response("the revised answer"))                        # revision
  }
  stub_par <- function(experiments, ...) {
    tibble::tibble(
      approach = experiments$approach,
      response_text = paste("draft for", experiments$approach),
      success = TRUE,
      sent_tokens = 100L, rec_tokens = 50L, total_tokens = 150L
    )
  }
  strong <- LLMR::llm_config("deepseek", "fake-strong")
  cheap  <- LLMR::llm_config("groq", "fake-cheap")

  with_stub_llmr("call_llm_robust", stub_robust, {
    with_stub_llmr("call_llm_par", stub_par, {
      out <- agent_fanout_synthesis("hard problem", strong, cheap,
                                    n_approaches = 2, quiet = TRUE)
      expect_s3_class(out, "agent_fanout_result")
      expect_identical(out$provenance$kind, "agent_fanout_synthesis")
      expect_identical(as_agent_run(out)$kind, "agent_fanout_synthesis")
      expect_output(print(out), "agent_fanout_result")
      expect_equal(nrow(out$plan), 2L)
      expect_equal(nrow(out$workers), 2L)
      expect_true(out$revised)
      expect_identical(out$answer, "the revised answer")
      expect_false(isTRUE(out$verification$sound))
    })
  })
})

test_that("a sound answer skips revision", {
  robust_calls <- 0L
  stub_robust <- function(config, messages, ...) {
    robust_calls <<- robust_calls + 1L
    switch(robust_calls,
      fake_response('{"approaches": [{"title":"A","instructions":"a"},{"title":"B","instructions":"b"}]}'),
      fake_response("final answer"),
      fake_response('{"sound": true, "flaws": []}'))
  }
  stub_par <- function(experiments, ...) {
    tibble::tibble(approach = experiments$approach,
                   response_text = "draft", success = TRUE)
  }
  with_stub_llmr("call_llm_robust", stub_robust, {
    with_stub_llmr("call_llm_par", stub_par, {
      out <- agent_fanout_synthesis("p", LLMR::llm_config("a", "m"),
                                    LLMR::llm_config("b", "m"), quiet = TRUE)
      expect_false(out$revised)
      expect_identical(out$answer, "final answer")
    })
  })
})
