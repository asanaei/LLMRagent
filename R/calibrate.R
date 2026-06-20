# calibrate.R -----------------------------------------------------------------
# The calibration bridge. An LLM (or an agent) is a cheap, abundant, but biased
# coder; a small human-labeled GOLD sample is scarce but trusted. Plugging the
# model's predicted labels straight into a downstream estimator (a mean, a
# proportion, a regression) is biased even when the model agrees with humans
# 80-90% of the time, because the residual disagreement is rarely orthogonal to
# the quantity of interest. The fix, from the design-based and
# prediction-powered inference literatures, is to combine the abundant cheap
# labels with the small gold sample and rectify: estimate the model's bias on
# the labeled units and subtract it off, propagating the extra uncertainty into
# the standard error. This file implements a thin, correct local estimator for
# the mean / proportion and for OLS, and a frame that bridges out to LLMRcontent
# for heavier validators. The math is base R + stats; nothing here calls a
# model.
#
# References:
#   Angelopoulos, A. N., Bates, S., Fannjiang, C., Jordan, M. I., & Zrnic, T.
#     (2023). Prediction-powered inference. Science, 382(6671), 669-674.
#   Egami, N., Hinck, M., Stewart, B. M., & Wei, H. (2023). Using imperfect
#     surrogates for downstream inference: Design-based supervised learning for
#     social science. NeurIPS 36.

#' @importFrom tibble tibble as_tibble
NULL

# ---- input coercion ---------------------------------------------------------

# Pull a plain prediction vector out of whatever `predictions` is: a bare
# vector (the common path), a tibble (then `label` names the column), or
# something run-able (an Agent / classed result -> as_agent_run -> the
# call-level response_text). Documented on agent_calibrate().
#' @keywords internal
#' @noRd
.cal_predictions <- function(predictions, label = NULL) {
  if (is.null(predictions)) {
    stop("`predictions` is required.", call. = FALSE)
  }
  # a tidy frame: take the named column, or the single non-id column
  if (is.data.frame(predictions)) {
    if (!is.null(label)) {
      if (!label %in% names(predictions)) {
        stop(sprintf("`label` = \"%s\" is not a column of `predictions`.", label),
             call. = FALSE)
      }
      return(predictions[[label]])
    }
    if (ncol(predictions) == 1L) return(predictions[[1L]])
    stop("`predictions` is a data frame with several columns; name one via `label`.",
         call. = FALSE)
  }
  # a bare atomic vector: the common path
  if (is.atomic(predictions)) return(predictions)
  # something run-able: extract the call-level response text
  run <- tryCatch(as_agent_run(predictions), error = function(e) NULL)
  if (!is.null(run)) {
    calls <- tryCatch(tibble::as_tibble(as_tibble(run, "call")), error = function(e) NULL)
    if (!is.null(calls) && "response_text" %in% names(calls)) {
      return(calls$response_text)
    }
  }
  stop(paste0("Could not extract a prediction vector from `predictions`. Pass a ",
              "numeric/character/logical vector, a tibble with a `label` column, ",
              "or an agent_run/Agent."), call. = FALSE)
}

# Coerce a label vector to numeric for the mean/proportion math. Logicals and
# 0/1-ish characters become 0/1; already-numeric passes through. Used for both
# predictions and gold so a proportion of TRUE/"yes" works.
#' @keywords internal
#' @noRd
.cal_numeric <- function(x, what = "values") {
  if (is.numeric(x)) return(as.numeric(x))
  if (is.logical(x)) return(as.numeric(x))
  if (is.factor(x)) x <- as.character(x)
  if (is.character(x)) {
    num <- suppressWarnings(as.numeric(x))
    if (!any(is.na(num) & !is.na(x))) return(num)
    # try a yes/true-style mapping to 0/1
    lo <- tolower(trimws(x))
    yes <- lo %in% c("1", "yes", "true", "t", "y", "positive", "pos")
    no  <- lo %in% c("0", "no", "false", "f", "n", "negative", "neg")
    if (all(yes | no | is.na(x))) {
      out <- rep(NA_real_, length(x)); out[yes] <- 1; out[no] <- 0; return(out)
    }
  }
  stop(sprintf(paste0("Cannot coerce `%s` to numeric for a mean/proportion. Pass ",
                      "a numeric or 0/1 (logical / yes-no) vector, or use ",
                      "estimand = \"ols\" with your own outcome."), what),
       call. = FALSE)
}

# Resolve the gold contract into a list(gold, pred_on_gold): the true labels on
# the labeled subset and the model's predictions on exactly those units. The
# accepted forms (permissive but documented on agent_calibrate()):
#   * gold = list/tibble with $gold and $pred_on_gold  -> used as-is
#   * gold = a named vector or 2-col (id, gold) tibble + `id` on predictions ->
#       align f_all to gold by id, take f_lab = f_all[match]
#   * gold = a plain vector AND length(gold) == length(predictions-on-labeled):
#       interpreted as the labeled subset in order, with pred_on_gold supplied
#       separately is required -> error asking for the contract.
#' @keywords internal
#' @noRd
.cal_gold <- function(gold, f_all, id = NULL, gold_id_col = NULL) {
  # form 1: an explicit list/frame carrying both sides
  if (is.list(gold) && !is.null(gold[["gold"]]) && !is.null(gold[["pred_on_gold"]])) {
    g <- gold[["gold"]]; p <- gold[["pred_on_gold"]]
    if (length(g) != length(p)) {
      stop("`gold$gold` and `gold$pred_on_gold` must have the same length.",
           call. = FALSE)
    }
    return(list(gold = g, pred_on_gold = p))
  }
  # form 2: id-aligned. gold is a 2-col (id, value) tibble or a named vector;
  # `id` is the id vector for predictions (f_all).
  if (!is.null(id)) {
    if (is.data.frame(gold)) {
      idc <- gold_id_col %||% names(gold)[1L]
      valc <- setdiff(names(gold), idc)[1L]
      gid <- gold[[idc]]; gval <- gold[[valc]]
    } else if (!is.null(names(gold))) {
      gid <- names(gold); gval <- unname(gold)
    } else {
      stop(paste0("With `id` given, `gold` must be a 2-column (id, value) tibble ",
                  "or a named vector."), call. = FALSE)
    }
    if (length(id) != length(f_all)) {
      stop("`id` must be the id vector for ALL units, so length(id) == ",
           "length(predictions) (here ", length(f_all), "). The gold ids are ",
           "matched into it to find the labeled subset. You passed length(id) = ",
           length(id), ".", call. = FALSE)
    }
    pos <- match(gid, id)
    if (anyNA(pos)) {
      stop("Some gold ids are not present in the all-units `id`.", call. = FALSE)
    }
    return(list(gold = gval, pred_on_gold = f_all[pos]))
  }
  # form 3: a bare vector with no id and no pred_on_gold -> ambiguous
  stop(paste0("Ambiguous `gold`. Pass one of: (a) a list/tibble with `$gold` and ",
              "`$pred_on_gold`; or (b) a named vector / 2-col (id, value) tibble ",
              "together with `id` (the id vector aligned to all-units ",
              "`predictions`)."), call. = FALSE)
}

# The normal-approximation half width for a two-sided interval at `level`.
#' @keywords internal
#' @noRd
.cal_zwidth <- function(se, level) {
  stats::qnorm(1 - (1 - level) / 2) * se
}

# ---- the mean / proportion estimators ---------------------------------------

# Rectified mean (PPI; and DSL under random sampling). f_all over N units, gold
# Y over n labeled units, f_lab the predictions on those same n units.
#' @keywords internal
#' @noRd
.cal_mean_rectified <- function(f_all, gold, pred_on_gold, level) {
  N <- length(f_all); n <- length(gold)
  if (n < 2L) stop("Need at least 2 gold-labeled units.", call. = FALSE)
  rect_terms <- gold - pred_on_gold           # per-unit bias correction
  theta <- mean(f_all) + mean(rect_terms)
  v <- stats::var(f_all) / N + stats::var(rect_terms) / n
  se <- sqrt(v)
  hw <- .cal_zwidth(se, level)
  list(estimate = theta, std_error = se,
       conf_low = theta - hw, conf_high = theta + hw,
       rectifier = list(mean_correction = mean(rect_terms),
                        f_all_mean = mean(f_all),
                        var_f_all = stats::var(f_all),
                        var_rect = stats::var(rect_terms)))
}

# The biased plug-in mean: ignore the gold, average the model's labels.
#' @keywords internal
#' @noRd
.cal_mean_naive <- function(f_all, level) {
  N <- length(f_all)
  theta <- mean(f_all)
  se <- sqrt(stats::var(f_all) / N)
  hw <- .cal_zwidth(se, level)
  list(estimate = theta, std_error = se,
       conf_low = theta - hw, conf_high = theta + hw)
}

# ---- the OLS estimators -----------------------------------------------------

# Design matrix from the COVARIATES (right-hand side) of a formula over `data`.
# The response is deleted from the terms first, so `data` need only carry the
# covariates -- the outcome comes from `gold`/`predictions`, not `data`. Returns
# list(X, y_name, terms); factors expand consistently via the model frame.
#' @keywords internal
#' @noRd
.cal_model_matrix <- function(formula, data) {
  tt <- stats::delete.response(stats::terms(formula))  # covariates only
  mf <- stats::model.frame(tt, data, na.action = stats::na.pass)
  X <- stats::model.matrix(tt, mf)
  y_name <- if (attr(stats::terms(formula), "response") > 0L) all.vars(formula)[1L] else NA_character_
  list(X = X, y_name = y_name, terms = colnames(X))
}

# Solve (X'X) beta = X'y via the cross-products, robust to rank with a small
# ridge fallback only if singular.
#' @keywords internal
#' @noRd
.cal_ols_solve <- function(X, y) {
  XtX <- crossprod(X)
  Xty <- crossprod(X, y)
  beta <- tryCatch(solve(XtX, Xty),
                   error = function(e) solve(XtX + diag(1e-8, ncol(XtX)), Xty))
  drop(beta)
}

# One-step debiased OLS (PPI for OLS, classic form). f_all = predicted outcome
# on ALL units (numeric); the labeled subset additionally has the gold outcome.
# beta_f is OLS of f_all on X over all units; the rectifier is the OLS of the
# gold residual (Y_lab - f_lab) on X over the labeled units; theta = beta_f +
# rectifier. Sandwich SEs: the variance of beta_f over all units plus the
# variance of the rectifier over the labeled units, both via the standard
# (X'X)^-1 X' diag(e^2) X (X'X)^-1 meat.
#' @keywords internal
#' @noRd
.cal_ols_ppi <- function(formula, data, f_all, gold, gold_rows, level) {
  mm <- .cal_model_matrix(formula, data)
  X <- mm$X
  terms <- mm$terms
  N <- nrow(X); p <- ncol(X)

  if (length(f_all) != N) {
    stop("`f_all` (predicted outcome) must have one value per row of `data`.",
         call. = FALSE)
  }
  Xl <- X[gold_rows, , drop = FALSE]
  fl <- f_all[gold_rows]
  if (length(gold) != length(gold_rows)) {
    stop("`gold` must have one value per labeled row.", call. = FALSE)
  }
  n <- nrow(Xl)
  if (n <= p) stop("Need more labeled units than coefficients for OLS.", call. = FALSE)

  beta_f <- .cal_ols_solve(X, f_all)
  resid_gold <- gold - fl
  rect <- .cal_ols_solve(Xl, resid_gold)
  theta <- beta_f + rect

  # sandwich pieces
  bread_all <- solve(crossprod(X))
  e_all <- as.numeric(f_all - X %*% beta_f)
  meat_all <- crossprod(X * e_all)
  V_f <- bread_all %*% meat_all %*% bread_all

  bread_lab <- solve(crossprod(Xl))
  e_lab <- as.numeric(resid_gold - Xl %*% rect)
  meat_lab <- crossprod(Xl * e_lab)
  V_rect <- bread_lab %*% meat_lab %*% bread_lab

  V <- V_f + V_rect
  se <- sqrt(pmax(diag(V), 0))
  hw <- .cal_zwidth(se, level)
  list(estimate = theta, std_error = se,
       conf_low = theta - hw, conf_high = theta + hw,
       terms = terms,
       rectifier = list(beta_f = beta_f, correction = rect))
}

# Naive OLS: regress the predicted outcome on X over all units, plug-in SEs.
#' @keywords internal
#' @noRd
.cal_ols_naive <- function(formula, data, f_all, level) {
  mm <- .cal_model_matrix(formula, data)
  X <- mm$X
  N <- nrow(X); p <- ncol(X)
  beta_f <- .cal_ols_solve(X, f_all)
  bread <- solve(crossprod(X))
  e <- as.numeric(f_all - X %*% beta_f)
  meat <- crossprod(X * e)
  V <- bread %*% meat %*% bread
  se <- sqrt(pmax(diag(V), 0))
  hw <- .cal_zwidth(se, level)
  list(estimate = beta_f, std_error = se,
       conf_low = beta_f - hw, conf_high = beta_f + hw,
       terms = mm$terms)
}

# ---- the agreement block ----------------------------------------------------

# Lay predictions and gold side by side on the labeled set and read reliability
# through LLMR::llm_agreement(). Accuracy is the share of exact matches (after
# the same normalization llm_agreement uses); alpha is nominal Krippendorff.
#' @keywords internal
#' @noRd
.cal_agreement <- function(pred_on_gold, gold) {
  df <- tibble::tibble(pred = as.character(pred_on_gold),
                       gold = as.character(gold))
  ag <- tryCatch(LLMR::llm_agreement(df, cols = c("pred", "gold")),
                 error = function(e) NULL)
  if (is.null(ag)) {
    acc <- mean(.norm_chr(pred_on_gold) == .norm_chr(gold), na.rm = TRUE)
    return(tibble::tibble(accuracy = acc, alpha = NA_real_,
                          mean_pairwise_agreement = acc, n_units = length(gold)))
  }
  s <- ag$summary
  acc <- mean(.norm_chr(pred_on_gold) == .norm_chr(gold), na.rm = TRUE)
  tibble::tibble(
    accuracy = acc,
    alpha = s$krippendorff_alpha %||% NA_real_,
    mean_pairwise_agreement = s$mean_pairwise_agreement %||% NA_real_,
    n_units = s$n_units %||% length(gold))
}

# trim + lowercase, matching llm_agreement(normalize = TRUE), for the accuracy
# share so it agrees with the pairwise number for two columns.
#' @keywords internal
#' @noRd
.norm_chr <- function(x) tolower(trimws(as.character(x)))

# ---- the public estimator ---------------------------------------------------

#' Calibrate LLM/agent labels for valid downstream inference
#'
#' A calibration helper: combine plentiful low-cost LLM (or agent) labels with a
#' small human-labeled GOLD sample to produce a design-based / prediction-powered
#' estimate of a mean, proportion, or OLS coefficient that is valid even when the
#' model agrees with humans only 80-90% of the time. Plugging predicted labels
#' straight into an estimator is biased; this function estimates the model's bias
#' on the labeled units, subtracts it off (the *rectifier*), and propagates the
#' extra uncertainty into the standard error.
#'
#' @section Estimators:
#' For `estimand = "mean"` or `"proportion"`:
#'
#' * `method = "ppi"`: the prediction-powered rectified mean of Angelopoulos
#'   et al. (2023). With `f_all` the predictions on all `N` units and `Y`,
#'   `f_lab` the gold and the predictions on the `n` labeled units, the estimate
#'   is `mean(f_all) + mean(Y - f_lab)` with variance
#'   `var(f_all)/N + var(Y - f_lab)/n`.
#' * `method = "dsl"`: design-based supervised learning (Egami et al. 2023).
#'   Under a simple random gold sample the point estimate coincides with PPI's
#'   rectified mean, so it is implemented as PPI here. Two caveats: the variance
#'   reported is PPI's (a superpopulation form that treats the prediction frame
#'   as random), not the finite-population design variance, so for a fixed
#'   prediction frame it is mildly conservative; and DSL's generalization to
#'   non-random or weighted gold samples (inverse-probability weighting, which
#'   also corrects the point estimate) is **not** implemented in this thin local
#'   estimator. For weighted or non-random sampling, use a full implementation
#'   (e.g. via the `LLMRcontent` bridge, see [as_llmrcontent_validation()]).
#' * `method = "naive"`: the biased plug-in `mean(f_all)` with variance
#'   `var(f_all)/N`. Provided for comparison only; do not report it.
#'
#' For `estimand = "ols"`:
#'
#' * `method = "ppi"`: the one-step debiased OLS. With `f_all` the predicted
#'   outcome on all units and `Y` the gold outcome on the labeled subset, fit
#'   `beta_f` (OLS of `f_all` on the covariates over all units) and the rectifier
#'   `(X_lab'X_lab)^-1 X_lab'(Y_lab - f_lab)` over the labeled units; the estimate
#'   is `beta_f + rectifier`. Standard errors add an HC0 sandwich for `beta_f`
#'   over all units to an HC0 sandwich for the rectifier over the labeled units.
#'   This is a deliberately simple inference: it omits the cross-covariance from
#'   the labeled rows being a subset of the all-units frame, and uses no
#'   finite-sample or leverage correction, so it can **under-cover with a small
#'   gold sample or high-leverage designs**. Treat the OLS intervals as
#'   approximate; for careful inference use a full PPI/DSL implementation.
#' * `method = "naive"`: OLS of the predicted outcome on the covariates,
#'   sandwich SEs, for comparison.
#'
#' @param predictions The model's predictions on **all** units. The common path
#'   is a bare numeric / logical / character vector. Also accepted: a tibble
#'   (then `label` names the column) or an [Agent] / agent run (then the
#'   call-level `response_text` is used).
#' @param gold The held-out human labels on the labeled subset, with the model's
#'   predictions on exactly those units. Two forms: (a) a list or tibble with
#'   `$gold` (the true labels) and `$pred_on_gold` (the predictions on the same
#'   units); or (b) a named vector or 2-column `(id, value)` tibble together with
#'   `id`, the id vector aligned to all-units `predictions`. For `estimand =
#'   "ols"`, `gold` is the gold outcome on the labeled rows (a vector), and the
#'   labeled rows are identified by `id` (a logical/integer index into `data`).
#' @param ... Unused; reserved.
#' @param method One of `"dsl"`, `"ppi"`, or `"naive"`. For the mean/proportion,
#'   `"dsl"` and `"ppi"` coincide under random sampling.
#' @param estimand One of `"mean"`, `"proportion"`, or `"ols"`. `"mean"` and
#'   `"proportion"` share the rectified-mean machinery (a proportion is the mean
#'   of a 0/1 label).
#' @param formula For `estimand = "ols"`, the model formula. Write it with the
#'   response on the left (e.g. `y ~ x1 + x2`); the response column does **not**
#'   need to be in `data` (the gold outcome on the labeled rows is supplied via
#'   `gold`, and the predicted outcome on all units via `predictions`). Only the
#'   covariates must exist in `data` for every unit. A one-sided formula
#'   (`~ x1 + x2`) is also accepted.
#' @param data For `estimand = "ols"`, a data frame of the **covariates** for
#'   **all** units. The outcome columns are not read from `data`: the predicted
#'   outcome comes from `predictions` and the gold outcome on the labeled rows
#'   from `gold`.
#' @param label When `predictions` is a tibble, the name of the prediction
#'   column.
#' @param id For the mean/proportion id-aligned form, the id vector for all
#'   units. For OLS, a logical or integer index marking the labeled rows of
#'   `data` (aligned to `gold`).
#' @param level Confidence level for the interval (default `0.95`).
#' @param attach_to Optionally an agent run; when supplied, the returned object
#'   carries an `attached_to_run_id` attribute. Attaching to the run itself is a
#'   separate, explicit step via [attach_calibration()].
#' @return An object of class `agent_calibration`: a list with `estimate` (a
#'   tibble of `term`, `estimate`, `std_error`, `conf_low`, `conf_high`,
#'   `method`, `estimand`), `naive` (the same shape for the plug-in), `agreement`
#'   (accuracy and Krippendorff alpha on the labeled set), `n_labeled`,
#'   `n_total`, `rectifier`, `calibrated = TRUE`, `method`, `estimand`, and a
#'   `manifest_patch` to fold into a run's design via [attach_calibration()].
#' @references
#' Angelopoulos, A. N., Bates, S., Fannjiang, C., Jordan, M. I., & Zrnic, T.
#' (2023). Prediction-powered inference. Science, 382(6671), 669-674.
#'
#' Egami, N., Hinck, M., Stewart, B. M., & Wei, H. (2023). Using imperfect
#' surrogates for downstream inference: Design-based supervised learning for
#' social science. NeurIPS 36.
#' @seealso [attach_calibration()], [as_llmrcontent_validation()],
#'   [LLMR::llm_agreement()]
#' @examples
#' # The clearest contract: hand over both sides explicitly. `gold` is the human
#' # labels on the subset; `pred_on_gold` is the model's labels on those same
#' # units. No id matching to get wrong.
#' set.seed(110)
#' truth <- rbinom(2000, 1, 0.4)
#' pred  <- ifelse(runif(2000) < 0.15, 1 - truth, truth)   # ~85% accurate
#' lab   <- sample(2000, 200)
#' cal <- agent_calibrate(
#'   predictions = pred,
#'   gold = list(gold = truth[lab], pred_on_gold = pred[lab]),
#'   method = "ppi", estimand = "proportion")
#' cal$estimate
#'
#' # The id-aligned contract: `id` is the id of EVERY unit (so length(id) ==
#' # length(predictions)); the ids in `gold` are matched into it to find the
#' # labeled rows. A frequent mistake is to pass only the subset's ids as `id`.
#' preds <- c(1, 0, 1, 0, 1, 0, 1, 1, 1, 0, 1, 0)            # model: all 12 units
#' gold  <- tibble::tibble(id = 1:6, value = c(1, 0, 1, 0, 1, 0))  # human: first 6
#' agent_calibrate(preds, gold = gold, id = 1:12,            # id = ALL 12 unit ids
#'                 method = "ppi", estimand = "proportion")$estimate
#'
#' \dontrun{
#' # The common path: an agent labels every unit, humans label a subset.
#' preds <- vapply(texts, function(t) a$chat(t), character(1))
#' cal <- agent_calibrate(preds, gold = list(gold = human, pred_on_gold = preds[idx]),
#'                        method = "dsl", estimand = "proportion")
#' }
#' @export
agent_calibrate <- function(predictions, gold, ...,
                            method = c("dsl", "ppi", "naive"),
                            estimand = c("mean", "proportion", "ols"),
                            formula = NULL, data = NULL, label = NULL,
                            id = NULL, level = 0.95, attach_to = NULL) {
  method <- match.arg(method)
  estimand <- match.arg(estimand)
  if (!is.numeric(level) || length(level) != 1L || level <= 0 || level >= 1) {
    stop("`level` must be a single number in (0, 1).", call. = FALSE)
  }

  if (estimand == "ols") {
    out <- .cal_run_ols(gold = gold, method = method, formula = formula,
                        data = data, predictions = predictions, label = label,
                        id = id, level = level)
  } else {
    out <- .cal_run_mean(predictions = predictions, gold = gold, method = method,
                         estimand = estimand, label = label, id = id, level = level)
  }

  if (!is.null(attach_to)) {
    rid <- tryCatch(as_agent_run(attach_to)$run_id, error = function(e) NA_character_)
    attr(out, "attached_to_run_id") <- rid
  }
  out
}

# mean / proportion driver: coerce, estimate (both the chosen method and the
# naive plug-in for comparison), build the agreement block, assemble the result.
#' @keywords internal
#' @noRd
.cal_run_mean <- function(predictions, gold, method, estimand, label, id, level) {
  f_raw <- .cal_predictions(predictions, label = label)
  f_all <- .cal_numeric(f_raw, "predictions")

  g <- .cal_gold(gold, f_all = f_all, id = id)
  gold_v <- .cal_numeric(g$gold, "gold")
  flab_v <- .cal_numeric(g$pred_on_gold, "pred_on_gold")

  naive <- .cal_mean_naive(f_all, level)
  fit <- if (method == "naive") naive else .cal_mean_rectified(f_all, gold_v, flab_v, level)

  est_tbl <- .cal_estimate_tbl("(mean)", fit, method, estimand)
  naive_tbl <- .cal_estimate_tbl("(mean)", naive, "naive", estimand)
  agr <- .cal_agreement(g$pred_on_gold, g$gold)

  .new_agent_calibration(
    estimate = est_tbl, naive = naive_tbl, agreement = agr,
    n_labeled = length(gold_v), n_total = length(f_all),
    rectifier = fit$rectifier %||% list(), method = method, estimand = estimand)
}

# OLS driver: split the labeled rows out of `data`, estimate (PPI one-step and
# the naive predicted-only OLS), build the agreement block on the labeled rows.
#' @keywords internal
#' @noRd
.cal_run_ols <- function(gold, method, formula, data, predictions, label, id, level) {
  if (is.null(formula) || is.null(data)) {
    stop("`estimand = \"ols\"` requires both `formula` and `data`.", call. = FALSE)
  }
  if (!inherits(formula, "formula")) stop("`formula` must be a formula.", call. = FALSE)
  if (!is.data.frame(data)) stop("`data` must be a data frame.", call. = FALSE)

  f_all <- .cal_numeric(.cal_predictions(predictions, label = label), "predictions")
  if (length(f_all) != nrow(data)) {
    stop("`predictions` must have one value per row of `data`.", call. = FALSE)
  }

  if (is.null(id)) {
    stop(paste0("`estimand = \"ols\"` needs `id`: a logical or integer index of ",
                "the labeled rows of `data`, aligned to `gold`."), call. = FALSE)
  }
  gold_rows <- if (is.logical(id)) which(id) else as.integer(id)
  gold_v <- .cal_numeric(gold, "gold")
  if (length(gold_v) != length(gold_rows)) {
    stop("`gold` must have one value per labeled row identified by `id`.",
         call. = FALSE)
  }

  naive <- .cal_ols_naive(formula, data, f_all, level)
  fit <- if (method == "naive") naive else
    .cal_ols_ppi(formula, data, f_all, gold_v, gold_rows, level)

  est_tbl <- .cal_estimate_tbl(fit$terms, fit, method, "ols")
  naive_tbl <- .cal_estimate_tbl(naive$terms, naive, "naive", "ols")
  agr <- .cal_agreement(f_all[gold_rows], gold_v)

  .new_agent_calibration(
    estimate = est_tbl, naive = naive_tbl, agreement = agr,
    n_labeled = length(gold_v), n_total = nrow(data),
    rectifier = fit$rectifier %||% list(), method = method, estimand = "ols")
}

# A tidy estimate tibble (broom-shaped) from a fit list. `term` is the row
# label(s); the fit carries vectors of the same length.
#' @keywords internal
#' @noRd
.cal_estimate_tbl <- function(term, fit, method, estimand) {
  tibble::tibble(
    term = as.character(term),
    estimate = as.numeric(fit$estimate),
    std_error = as.numeric(fit$std_error),
    conf_low = as.numeric(fit$conf_low),
    conf_high = as.numeric(fit$conf_high),
    method = method,
    estimand = estimand)
}

# Construct the agent_calibration object with its manifest patch.
#' @keywords internal
#' @noRd
.new_agent_calibration <- function(estimate, naive, agreement, n_labeled, n_total,
                                   rectifier, method, estimand) {
  structure(list(
    estimate = estimate,
    naive = naive,
    agreement = agreement,
    n_labeled = as.integer(n_labeled),
    n_total = as.integer(n_total),
    rectifier = rectifier,
    calibrated = TRUE,
    method = method,
    estimand = estimand,
    manifest_patch = list(
      calibration_method = method,
      estimand = estimand,
      n_labeled = as.integer(n_labeled),
      n_total = as.integer(n_total))
  ), class = "agent_calibration")
}

# ---- attach -----------------------------------------------------------------

#' Attach a calibration to an agent run
#'
#' Records a calibration on a run and folds its `manifest_patch` into the run's
#' design, so the run now carries its validated inference and its study
#' [agent_manifest()] hash changes (the apparatus is no longer a bare
#' model-conditioned run; it is a calibrated estimate). The run's `claim_type`
#' is set to `"calibrated_inference"`, which suppresses the model-conditioned
#' caveat in [report()].
#'
#' @param run An object accepted by [as_agent_run()].
#' @param cal An `agent_calibration` from [agent_calibrate()].
#' @return The modified `agent_run`.
#' @seealso [agent_calibrate()], [agent_manifest()]
#' @examples
#' \dontrun{
#' run <- as_agent_run(a)
#' cal <- agent_calibrate(preds, gold = g, method = "ppi", estimand = "proportion")
#' run <- attach_calibration(run, cal)
#' }
#' @export
attach_calibration <- function(run, cal) {
  if (!inherits(cal, "agent_calibration")) {
    stop("`cal` must be an agent_calibration from agent_calibrate().", call. = FALSE)
  }
  run <- as_agent_run(run)
  run$calibration <- cal
  run$design$calibration <- cal$manifest_patch
  run$claim_type <- "calibrated_inference"
  run
}

# ---- the LLMRcontent bridge frame -------------------------------------------

#' Build a validation frame for LLMRcontent
#'
#' Returns a tidy frame shaped for an external validator (the `LLMRcontent`
#' package's content-validation entry point): one row per labeled unit with the
#' aligned `id`, `prediction`, and `gold`. This is the hand-off frame; it does
#' not call `LLMRcontent`. Pass the result to `LLMRcontent`'s validator (for
#' example `LLMRcontent::validate_against_gold(frame)`) for the heavier
#' diagnostics (confusion matrix, per-class precision/recall, calibration
#' curves) that are out of scope for this thin local estimator.
#'
#' @param predictions The model's predictions on the labeled units (a vector),
#'   or a tibble with a single prediction column.
#' @param gold The human labels on the same units, aligned to `predictions`.
#' @param id Optional ids for the units; defaults to a 1..n sequence.
#' @return A tibble with columns `id`, `prediction`, `gold`.
#' @seealso [agent_calibrate()]
#' @examples
#' frame <- as_llmrcontent_validation(
#'   predictions = c("pos", "neg", "pos"),
#'   gold        = c("pos", "neg", "neg"))
#' frame
#' @export
as_llmrcontent_validation <- function(predictions, gold, id = NULL) {
  pred <- if (is.data.frame(predictions)) {
    if (ncol(predictions) != 1L) {
      stop("`predictions` tibble must have exactly one column here.", call. = FALSE)
    }
    predictions[[1L]]
  } else predictions
  if (length(pred) != length(gold)) {
    stop("`predictions` and `gold` must have the same length (the labeled units).",
         call. = FALSE)
  }
  if (is.null(id)) id <- seq_along(pred)
  if (length(id) != length(pred)) {
    stop("`id` must be the same length as `predictions`/`gold`.", call. = FALSE)
  }
  tibble::tibble(id = id, prediction = pred, gold = gold)
}

# ---- diagnostics + print ----------------------------------------------------

#' Machine-readable diagnostics for a calibration
#'
#' One-row diagnostic summary of an [agent_calibrate()] result: the method and
#' estimand, the gold and total sample sizes, the naive bias (the plug-in
#' estimate minus the corrected estimate, for the first/headline term), the
#' corrected interval width, and the labeled-set accuracy and Krippendorff alpha.
#' A `naive_bias` far from zero is the signal that plugging predicted labels in
#' directly would have been misleading.
#'
#' @param x An `agent_calibration`.
#' @param ... Unused.
#' @return A one-row tibble with `method`, `estimand`, `n_labeled`, `n_total`,
#'   `naive_bias`, `ci_width`, `accuracy`, `alpha`.
#' @seealso [agent_calibrate()], [LLMR::diagnostics()]
#' @importFrom LLMR diagnostics
#' @exportS3Method LLMR::diagnostics agent_calibration
diagnostics.agent_calibration <- function(x, ...) {
  e <- x$estimate; nv <- x$naive
  naive_bias <- nv$estimate[1L] - e$estimate[1L]
  ci_width <- e$conf_high[1L] - e$conf_low[1L]
  tibble::tibble(
    method = x$method,
    estimand = x$estimand,
    n_labeled = x$n_labeled,
    n_total = x$n_total,
    naive_bias = naive_bias,
    ci_width = ci_width,
    accuracy = x$agreement$accuracy %||% NA_real_,
    alpha = x$agreement$alpha %||% NA_real_)
}

#' @export
print.agent_calibration <- function(x, ...) {
  cat(sprintf("<agent_calibration | method=%s | estimand=%s | n_gold=%d / N=%d>\n",
              x$method, x$estimand, x$n_labeled, x$n_total))
  acc <- x$agreement$accuracy %||% NA_real_
  alpha <- x$agreement$alpha %||% NA_real_
  cat(sprintf("  labeled-set agreement: accuracy = %.3f, alpha = %.3f\n", acc, alpha))
  e <- x$estimate; nv <- x$naive
  fmt <- function(tb, tag) {
    for (i in seq_len(nrow(tb))) {
      cat(sprintf("  %-9s %-14s estimate = %.4f  [%.4f, %.4f]\n",
                  tag, tb$term[i], tb$estimate[i], tb$conf_low[i], tb$conf_high[i]))
    }
  }
  fmt(e, x$method)
  fmt(nv, "naive")
  bias <- nv$estimate[1L] - e$estimate[1L]
  cat(sprintf("  naive bias (plug-in - corrected, headline term): %.4f\n", bias))
  invisible(x)
}
