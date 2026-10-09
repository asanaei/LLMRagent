# The robustness battery counts failed measurements as failures, keeps its
# summary defined when nothing could be measured, and uses every replicate.

fake_cfg <- function() LLMR::llm_config("groq", "fake")

test_that("a measure that fails counts as a failure, not as a stable cell", {
  run_fn <- function(cond, rep) if (identical(as.character(cond$temperature), "1")) "bad" else "yes"
  expect_warning(
    batt <- agent_robustness(run_fn, vary = list(temperature = c("0", "1")), reps = 2,
                             measure = function(r) if (r == "bad") stop("cannot parse") else r,
                             config = fake_cfg()),
    "measure failed in 2 of 4")
  expect_identical(sum(!is.na(batt$cells$measure_error)), 2L)
  lvl1 <- batt$by_axis[batt$by_axis$level == "1", ]
  expect_identical(lvl1$failure_rate, 1)
  expect_equal(batt$overall$failure_rate, 0.5)
})

test_that("with nothing measured the summary is one row of NAs", {
  run_fn <- function(cond, rep) "x"
  expect_warning(
    batt <- agent_robustness(run_fn, vary = list(model = c("a", "b")),
                             measure = function(r) stop("broken measure"),
                             config = fake_cfg()),
    "measure failed in 2 of 2")
  expect_identical(nrow(batt$overall), 1L)
  expect_true(is.na(batt$overall$fragile))
  expect_true(is.na(batt$overall$max_instability))
  expect_true(all(is.na(batt$by_axis$dispersion)))
  expect_output(print(batt), "fragile: NA")
})

test_that("an NA measure counts as a failed measurement", {
  run_fn <- function(cond, rep) "x"
  expect_warning(
    batt <- agent_robustness(run_fn, vary = list(model = c("a", "b")),
                             measure = function(r) NA, config = fake_cfg()),
    "measure failed")
  expect_true(all(batt$cells$measure_error == "the measure returned NA"))
})

test_that("one observed value per level keeps a dispersion of zero", {
  batt <- agent_robustness(function(cond, rep) "x", vary = list(model = c("a", "b")),
                           measure = function(r) r, config = fake_cfg())
  expect_identical(batt$by_axis$dispersion, c(0, 0))
})

test_that("agreement uses every replicate when the design has several axes", {
  rows_seen <- integer(0)
  with_stub_llmr("llm_agreement", function(data, cols, metric, ...) {
    rows_seen <<- c(rows_seen, nrow(data))
    list(summary = list(krippendorff_alpha = 0.5))
  }, {
    agent_robustness(function(cond, rep) paste0("v", rep),
                     vary = list(temperature = c("0", "1"), model = c("a", "b")),
                     reps = 5, measure = function(r) r, config = fake_cfg())
  })
  # Each axis pairs its two levels over (other axis level x replicate) units.
  expect_identical(rows_seen, c(10L, 10L))
})

test_that("a paraphrase shortfall warns and none at all is an error", {
  testthat::local_mocked_bindings(
    .generate_paraphrases = function(prompt, n, config) "Is it right?",
    .package = "LLMRagent")
  expect_warning(
    agent_robustness(function(cond, rep) "x", measure = function(r) r,
                     vary = list(prompt = vary_prompt(prompt = "Is it fine?",
                                                      paraphrase = 3, config = fake_cfg()))),
    "Asked for 3 paraphrases and received 1")
  testthat::local_mocked_bindings(
    .generate_paraphrases = function(prompt, n, config) character(0),
    .package = "LLMRagent")
  expect_error(
    agent_robustness(function(cond, rep) "x", measure = function(r) r,
                     vary = list(prompt = vary_prompt(prompt = "Is it fine?",
                                                      paraphrase = 3, config = fake_cfg()))),
    "no usable paraphrases")
})
