# Calibration bridge: the math is the point. Everything here is offline and
# deterministic; set.seed(110) is used only where data generation must be
# replicable (the coverage / known-bias demonstrations).

test_that("naive plug-in is biased and PPI/DSL recover the truth (proportion)", {
  set.seed(110)
  N <- 2000L; n <- 200L
  bias_rate <- 0.18                       # flip ~18% of labels -> a known bias
  truth <- rbinom(N, 1L, 0.40)
  flip  <- runif(N) < bias_rate
  pred  <- ifelse(flip, 1L - truth, truth)
  true_mean <- mean(truth)

  lab <- sample(N, n)
  gold_contract <- list(gold = truth[lab], pred_on_gold = pred[lab])

  # naive: the plug-in average of the model labels, demonstrably off
  cal_naive <- agent_calibrate(pred, gold = gold_contract,
                               method = "naive", estimand = "proportion")
  expect_s3_class(cal_naive, "agent_calibration")
  naive_est <- cal_naive$estimate$estimate[1L]
  expect_gt(abs(naive_est - true_mean), 0.02)   # biased away from the truth

  # ppi: rectified, recovers the truth inside its 95% CI
  cal_ppi <- agent_calibrate(pred, gold = gold_contract,
                             method = "ppi", estimand = "proportion")
  est <- cal_ppi$estimate
  expect_true(est$conf_low[1L] <= true_mean && true_mean <= est$conf_high[1L])
  expect_true(cal_ppi$calibrated)
  # the corrected point estimate is much closer to the truth than the naive one
  expect_lt(abs(est$estimate[1L] - true_mean), abs(naive_est - true_mean))

  # dsl == ppi for the mean under random sampling
  cal_dsl <- agent_calibrate(pred, gold = gold_contract,
                             method = "dsl", estimand = "proportion")
  expect_equal(cal_dsl$estimate$estimate[1L], est$estimate[1L])
  expect_equal(cal_dsl$estimate$std_error[1L], est$std_error[1L])

  # sizes and agreement block are populated
  expect_identical(cal_ppi$n_total, N)
  expect_identical(cal_ppi$n_labeled, n)
  expect_true(cal_ppi$agreement$accuracy > 0.7 && cal_ppi$agreement$accuracy < 0.95)
  expect_false(is.na(cal_ppi$agreement$alpha))
})

test_that("a continuous mean is recovered the same way", {
  set.seed(110)
  N <- 2000L; n <- 200L
  truth <- rnorm(N, mean = 3, sd = 1)
  bias <- 0.5
  pred  <- truth + bias + rnorm(N, sd = 0.4)
  lab <- sample(N, n)
  cal <- agent_calibrate(pred, gold = list(gold = truth[lab], pred_on_gold = pred[lab]),
                         method = "ppi", estimand = "mean")
  est <- cal$estimate
  expect_true(est$conf_low[1L] <= mean(truth) && mean(truth) <= est$conf_high[1L])
  # naive (the bare predicted mean) is off by ~the bias
  expect_gt(abs(cal$naive$estimate[1L] - mean(truth)), 0.3)
})

test_that("id-aligned gold contract matches the explicit one", {
  set.seed(110)
  N <- 500L; n <- 60L
  truth <- rbinom(N, 1L, 0.5)
  pred  <- ifelse(runif(N) < 0.2, 1L - truth, truth)
  ids   <- paste0("u", seq_len(N))
  lab   <- sample(N, n)

  cal_explicit <- agent_calibrate(
    pred, gold = list(gold = truth[lab], pred_on_gold = pred[lab]),
    method = "ppi", estimand = "proportion")

  gold_tbl <- tibble::tibble(id = ids[lab], value = truth[lab])
  cal_id <- agent_calibrate(
    pred, gold = gold_tbl, id = ids,
    method = "ppi", estimand = "proportion")

  expect_equal(cal_explicit$estimate$estimate[1L], cal_id$estimate$estimate[1L])
  expect_equal(cal_explicit$estimate$std_error[1L], cal_id$estimate$std_error[1L])
})

test_that("attach_calibration flips run$calibration and the manifest hash", {
  a <- fake_agent("A", list("x"))
  a$chat("hi")
  r <- as_agent_run(a)
  expect_null(r$calibration)
  m1 <- agent_manifest(r)$manifest_hash

  set.seed(110)
  truth <- rbinom(800, 1L, 0.4)
  pred  <- ifelse(runif(800) < 0.2, 1L - truth, truth)
  lab   <- sample(800, 100)
  cal <- agent_calibrate(pred, gold = list(gold = truth[lab], pred_on_gold = pred[lab]),
                         method = "ppi", estimand = "proportion")

  r2 <- attach_calibration(r, cal)
  expect_false(is.null(r2$calibration))
  expect_s3_class(r2$calibration, "agent_calibration")
  m2 <- agent_manifest(r2)$manifest_hash
  expect_false(identical(m1, m2))
  expect_identical(r2$claim_type, "calibrated_inference")
})

test_that("diagnostics(cal) returns one row with a nonzero naive bias", {
  set.seed(110)
  truth <- rbinom(2000, 1L, 0.4)
  pred  <- ifelse(runif(2000) < 0.18, 1L - truth, truth)
  lab   <- sample(2000, 200)
  cal <- agent_calibrate(pred, gold = list(gold = truth[lab], pred_on_gold = pred[lab]),
                         method = "ppi", estimand = "proportion")
  d <- diagnostics(cal)
  expect_s3_class(d, "tbl_df")
  expect_identical(nrow(d), 1L)
  expect_true(all(c("method", "estimand", "n_labeled", "n_total",
                    "naive_bias", "ci_width", "accuracy", "alpha") %in% names(d)))
  expect_true(abs(d$naive_bias) > 0.01)
})

test_that("the LLMRcontent bridge frame is shaped and aligned", {
  fr <- as_llmrcontent_validation(
    predictions = c("pos", "neg", "pos"),
    gold        = c("pos", "neg", "neg"),
    id          = c("a", "b", "c"))
  expect_s3_class(fr, "tbl_df")
  expect_identical(names(fr), c("id", "prediction", "gold"))
  expect_identical(fr$prediction, c("pos", "neg", "pos"))
  expect_identical(fr$gold, c("pos", "neg", "neg"))
  expect_error(as_llmrcontent_validation(1:3, 1:2))   # length mismatch
})

test_that("PPI-OLS point estimate beats naive OLS on a slope coefficient", {
  set.seed(110)
  N <- 3000L; n <- 300L
  x1 <- rnorm(N); x2 <- rnorm(N); x3 <- rnorm(N)
  beta_true <- c(intercept = 1.0, x1 = 2.0, x2 = -1.5, x3 = 0.5)
  y_gold <- beta_true["intercept"] + beta_true["x1"] * x1 +
            beta_true["x2"] * x2 + beta_true["x3"] * x3 + rnorm(N, sd = 1)
  # the model's predicted outcome: gold plus a small, covariate-correlated bias
  # plus noise, so the naive (predicted-only) OLS slopes are pulled off
  pred <- y_gold + 0.4 + 0.6 * x1 + rnorm(N, sd = 0.8)

  dat <- tibble::tibble(x1 = x1, x2 = x2, x3 = x3)
  labeled <- rep(FALSE, N); labeled[sample(N, n)] <- TRUE

  cal <- agent_calibrate(
    predictions = pred,
    gold = y_gold[labeled],
    method = "ppi", estimand = "ols",
    formula = ~ x1 + x2 + x3, data = dat, id = labeled)

  expect_s3_class(cal, "agent_calibration")
  expect_identical(nrow(cal$estimate), 4L)   # intercept + 3 slopes

  # focus on x1, the coefficient the prediction bias corrupts
  ppi_x1   <- cal$estimate$estimate[cal$estimate$term == "x1"]
  naive_x1 <- cal$naive$estimate[cal$naive$term == "x1"]
  expect_lt(abs(ppi_x1 - beta_true["x1"]), abs(naive_x1 - beta_true["x1"]))
})

test_that("print.agent_calibration runs without error", {
  set.seed(110)
  truth <- rbinom(400, 1L, 0.5)
  pred  <- ifelse(runif(400) < 0.2, 1L - truth, truth)
  lab   <- sample(400, 80)
  cal <- agent_calibrate(pred, gold = list(gold = truth[lab], pred_on_gold = pred[lab]),
                         method = "ppi", estimand = "proportion")
  expect_output(print(cal), "agent_calibration")
})
