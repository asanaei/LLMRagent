# Stage 3: claim-type discipline and the population-claim prose lint. Offline.

test_that("calibrated_inference is refused without calibration", {
  a <- fake_agent("A", list("x")); a$chat("hi")
  r <- as_agent_run(a)
  expect_error(mark_claim_type(r, "calibrated_inference"),
               class = "llmragent_claim_error")
})

test_that("weaker claim types are accepted and recorded", {
  a <- fake_agent("A", list("x")); a$chat("hi")
  r <- mark_claim_type(as_agent_run(a), "theory_probe")
  expect_identical(r$claim_type, "theory_probe")
  r2 <- mark_claim_type(as_agent_run(a), "instrument_pilot")
  expect_identical(r2$claim_type, "instrument_pilot")
})

test_that("calibrated_inference is allowed once a calibration is attached", {
  a <- fake_agent("A", list("x")); a$chat("hi")
  r <- as_agent_run(a)
  r$calibration <- list(calibrated = TRUE)   # stand-in for an agent_calibration
  expect_silent(rr <- mark_claim_type(r, "calibrated_inference"))
  expect_identical(rr$claim_type, "calibrated_inference")
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

test_that("calibrated runs pass the lint unchanged", {
  a <- fake_agent("A", list("x")); a$chat("hi")
  r <- as_agent_run(a); r$calibration <- list(calibrated = TRUE)
  txt <- "We estimate that 60% of Americans support the policy."
  expect_identical(llm_claim_lint(txt, run = r), txt)  # calibration -> no caveat
})

test_that("report() scopes an uncalibrated run's prose", {
  # a chat agent whose reply contains a population claim
  a <- fake_agent("A", list("Based on this, 70% of Americans believe X."))
  a$chat("survey?")
  r <- mark_claim_type(as_agent_run(a), "theory_probe")
  rep <- report(r)
  expect_s3_class(rep, "agent_report")
  # the report should carry either the scope caveat or the limits note
  expect_true(any(grepl("model-conditioned|not.*estimate|scope", rep, ignore.case = TRUE)))
})
