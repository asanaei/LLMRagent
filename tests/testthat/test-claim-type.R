# Stage 3: claim-type discipline and the population-claim prose lint. Offline.

test_that("supported claim types are accepted and recorded", {
  a <- fake_agent("A", list("x")); a$chat("hi")
  r <- mark_claim_type(as_agent_run(a), "theory_probe")
  expect_s3_class(r, "agent_run")
  expect_identical(r$claim_type, "theory_probe")
  expect_null(attr(r, "claim_type"))
  r2 <- mark_claim_type(as_agent_run(a), "instrument_pilot")
  expect_identical(r2$claim_type, "instrument_pilot")
  r3 <- mark_claim_type(as_agent_run(a), "coding")
  expect_identical(r3$claim_type, "coding")
})

test_that("the prose lint scopes population-estimate language", {
  txt <- "We find that 60% of Americans support the policy."
  out <- llm_claim_lint(txt, run = NULL, action = "scope")
  expect_true(length(out) > length(txt))           # a caveat was appended
  expect_true(any(grepl("model-conditioned", out)))
})

test_that("the prose lint can error on population claims", {
  txt <- "The population's true preference is recovered here."
  expect_error(llm_claim_lint(txt, run = NULL, action = "error"),
               class = "llmragent_claim_error")
})
