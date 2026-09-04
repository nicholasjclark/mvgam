# The marginaleffects and insight surface, and the extractors that
# sit beside it.
#
# Lives in tests/local because the round trip through
# marginaleffects -> insight -> get_predict -> posterior_epred is
# fixture-driven and too heavy for CI. Covers the user-facing entry
# points -- predictions, avg_predictions, avg_slopes,
# avg_comparisons -- together with the group-level extractors and
# the smooth surfaces, which have no executable coverage elsewhere.
#
# marginaleffects forbids any column literally named 'group' in the
# fit's training data, since it reserves that name for its own
# output. Fixtures built by build_fixtures.R use 'grp' for the
# random-effects grouping variable to avoid the collision. The
# orphan ar1_t2_noint fixture still carries 'group' and the affected
# test below strips it before dispatching.
#
# Every claim here is about mvgam's own output. An earlier version
# asked instead whether a matched brms fit produced the same shape,
# which needed a second fit for each of twenty-two fixtures and
# could not distinguish a table keyed correctly from one keyed by
# the wrong column. Where a brms comparison stood, the direct claim
# that replaced it is the stronger one: ranef is keyed by the
# frame's own factor levels, VarCorr's three blocks agree with each
# other, and loo_epred parts from loo_linpred exactly where the link
# is not the identity.

source("setup_tests_local.R")
source("concordance_helpers.R")

library(marginaleffects)
options("marginaleffects_model_classes" = "mvgam")


# Coverage scope for the marginaleffects entry-point smoke loop.
# Each fixture exercises a distinct observation family.
me_safe_fixtures <- c(
  "beta_ar1",
  "binom_ar1",
  "cumulative_fx",
  "hurdle_poisson_ar1",
  "zero_inflated_poisson_ar1"
)


# -- Insight surface ----------------------------------------------------

test_that("insight::get_data returns the training data", {
  require_fixtures("val_mvgam_beta_ar1.rds")
  mv <- load_mvgam("beta_ar1")
  out <- insight::get_data(mv)
  testthat::expect_s3_class(out, "data.frame")
  testthat::expect_identical(out, mv$data)
})

test_that("insight::find_response returns the response name(s)", {
  require_fixtures("val_mvgam_beta_ar1.rds")
  mv <- load_mvgam("beta_ar1")
  testthat::expect_equal(insight::find_response(mv), "y")
})

test_that("insight::find_predictors picks up obs + trend + meta vars", {
  require_fixtures("val_mvgam_ar1_fx_trend.rds")
  mv <- load_mvgam("ar1_fx_trend")
  preds <- insight::find_predictors(mv)
  testthat::expect_type(preds, "list")
  testthat::expect_true("x" %in% preds$conditional)
  testthat::expect_true("time" %in% preds$conditional)
})

test_that("find_predictors reports a series column that varies", {
  require_fixtures("val_mvgam_var_cor.rds")
  mv <- load_mvgam("var_cor")
  preds <- insight::find_predictors(mv)
  testthat::expect_true("series" %in% preds$conditional)
  testthat::expect_true("time" %in% preds$conditional)
})

test_that("find_predictors drops a meta variable holding one value", {
  require_fixtures("val_mvgam_ar1_fx_trend.rds")
  mv <- load_mvgam("ar1_fx_trend")
  # This fit carries a single series, so 'series' supports neither a
  # slope nor a contrast and must not be offered as a predictor.
  testthat::expect_equal(length(unique(mv$data$series)), 1L)
  testthat::expect_false("series" %in% insight::find_predictors(mv)$conditional)
})

test_that("avg_slopes runs on a single-series fit", {
  require_fixtures("val_mvgam_beta_ar1.rds")
  mv <- load_mvgam("beta_ar1")
  out <- SW(marginaleffects::avg_slopes(mv))
  testthat::expect_s3_class(out, "data.frame")
  testthat::expect_gt(nrow(out), 0L)
})

test_that("model.frame.mvgam returns predictors + response only", {
  require_fixtures("val_mvgam_beta_ar1.rds")
  mv <- load_mvgam("beta_ar1")
  mf <- model.frame(mv)
  testthat::expect_s3_class(mf, "data.frame")
  testthat::expect_true("y" %in% names(mf))
  testthat::expect_true("x" %in% names(mf))
})


# -- get_predict per type ----------------------------------------------

test_that("get_predict.mvgam: response type returns rowid/group/estimate", {
  require_fixtures("val_mvgam_beta_ar1.rds")
  mv <- load_mvgam("beta_ar1")
  out <- get_predict(mv, newdata = mv$data, type = "response")
  testthat::expect_s3_class(out, "data.frame")
  testthat::expect_true(all(c("rowid", "group", "estimate") %in% names(out)))
  testthat::expect_equal(nrow(out), nrow(mv$data))
  draws <- attr(out, "posterior_draws")
  testthat::expect_true(is.matrix(draws))
  # marginaleffects stores draws as [nobs x ndraws]
  testthat::expect_equal(nrow(draws), nrow(mv$data))
})

test_that("get_predict.mvgam: link type matches posterior_linpred", {
  require_fixtures("val_mvgam_beta_ar1.rds")
  mv <- load_mvgam("beta_ar1")
  out <- get_predict(mv, newdata = mv$data, type = "link")
  testthat::expect_s3_class(out, "data.frame")
  draws <- attr(out, "posterior_draws")
  testthat::expect_true(is.matrix(draws))
})

test_that("get_predict.mvgam: 3D ordinal epred flattens to tall frame", {
  require_fixtures("val_mvgam_cumulative_fx.rds")
  mv <- load_mvgam("cumulative_fx")
  # type = "expected" routes through posterior_epred, which returns
  # [ndraws x nobs x ncat] for ordinal families. The 3D branch in
  # get_predict.mvgam flattens this to one row per (obs, category).
  out <- get_predict(mv, newdata = mv$data, type = "expected")
  testthat::expect_equal(nrow(out), nrow(mv$data) * length(unique(out$group)))
  testthat::expect_true(length(unique(out$group)) > 1L)
})


# -- End-to-end marginaleffects entry points ---------------------------

run_me_smoke <- function(mv) {
  list(
    pred      = predictions(mv),
    avg_pred  = avg_predictions(mv),
    avg_slope = if (any(insight::find_predictors(mv)$conditional == "x")) {
      avg_slopes(mv, variables = "x")
    } else {
      NULL
    },
    avg_comp  = if (any(insight::find_predictors(mv)$conditional == "x")) {
      avg_comparisons(mv, variables = "x")
    } else {
      NULL
    }
  )
}

test_that("marginaleffects entry points run on supported families", {
  for (fix in me_safe_fixtures) {
    require_fixtures(paste0("val_mvgam_", fix, ".rds"))
    mv <- load_mvgam(fix)
    out <- suppressWarnings(run_me_smoke(mv))
    testthat::expect_s3_class(out$pred, "predictions")
    testthat::expect_s3_class(out$avg_pred, "predictions")
    if (!is.null(out$avg_slope)) {
      testthat::expect_true(nrow(out$avg_slope) >= 1L)
    }
    if (!is.null(out$avg_comp)) {
      testthat::expect_true(nrow(out$avg_comp) >= 1L)
    }
  }
})


# -- Response-scale claims on a bounded family -------------------------

test_that("predictions on a Beta fit stay on the Beta scale", {
  require_fixtures("val_mvgam_beta_ar1.rds")
  mv <- load_mvgam("beta_ar1")
  nd <- mv$data
  p <- suppressWarnings(predictions(mv, newdata = nd))
  # A Beta mean is a proportion, and so are its interval bounds. A
  # prediction that escaped to the link scale keeps every dimension
  # and fails here, which is the shape finding 28 takes on the
  # forecast arm of the softmax families.
  expect_identical(nrow(p), nrow(nd))
  expect_true(all(p$estimate > 0 & p$estimate < 1))
  expect_true(all(p$conf.low > 0 & p$conf.high < 1))
  expect_true(all(p$conf.low <= p$estimate))
  expect_true(all(p$estimate <= p$conf.high))
  # One row of the frame is one row of the output, in order, so a
  # prediction placed by position rather than by content fails.
  expect_equal(as.numeric(p$z), as.numeric(nd$z))
})


# -- Unsupported / error paths -----------------------------------------

test_that("get_predict rejects multivariate without resp", {
  require_fixtures("val_mvgam_mv_gauss.rds")
  mv <- load_mvgam("mv_gauss")
  testthat::expect_error(
    get_predict(mv, newdata = mv$data, type = "response"),
    regexp = "requires"
  )
})

test_that("get_predict rejects an invalid type", {
  require_fixtures("val_mvgam_beta_ar1.rds")
  mv <- load_mvgam("beta_ar1")
  testthat::expect_error(
    get_predict(mv, newdata = mv$data, type = "bogus")
  )
})


# -- type vocabulary & process_error toggle ----------------------------

test_that("type='expected' matches median(posterior_epred) exactly", {
  require_fixtures("val_mvgam_beta_ar1.rds")
  mv <- load_mvgam("beta_ar1")
  p <- suppressWarnings(predictions(mv, type = "expected", process_error = FALSE))
  ep <- posterior_epred(mv, newdata = mv$data, process_error = FALSE)
  manual <- unname(apply(ep, 2L, stats::median))
  testthat::expect_equal(unname(p$estimate), manual, tolerance = 1e-10)
})

test_that("type='response' on Poisson returns non-negative count samples", {
  require_fixtures("val_mvgam_ar1_int.rds")
  mv <- load_mvgam("ar1_int")
  p <- suppressWarnings(predictions(mv, type = "response", process_error = FALSE))
  # posterior_predict draws are integer; the per-obs MEDIAN of an
  # even number of draws can fall on a half-integer (e.g. 7.5), so
  # test the underlying draws rather than the reported point estimate.
  # marginaleffects strips the `posterior_draws` attribute from the
  # data.frame and stores it on an internal slot, so it is read
  # back with marginaleffects::posterior_draws().
  draws <- marginaleffects::posterior_draws(p, shape = "DxP")
  testthat::expect_true(all(draws == round(draws)))
  testthat::expect_true(all(p$estimate >= 0))
})

test_that("type='link' returns unbounded linpred-scale draws", {
  require_fixtures("val_mvgam_beta_ar1.rds")
  mv <- load_mvgam("beta_ar1")
  p_link <- suppressWarnings(predictions(mv, type = "link", process_error = FALSE))
  p_resp <- suppressWarnings(predictions(mv, type = "expected", process_error = FALSE))
  testthat::expect_true(any(p_link$estimate < 0))
  testthat::expect_equal(plogis(p_link$estimate), p_resp$estimate,
                         tolerance = 1e-6)
})

test_that("process_error TRUE vs FALSE produces a measurable shift", {
  require_fixtures("val_mvgam_beta_ar1.rds")
  mv <- load_mvgam("beta_ar1")
  set.seed(1)
  p_F <- suppressWarnings(predictions(mv, type = "expected", process_error = FALSE))
  set.seed(1)
  p_T <- suppressWarnings(predictions(mv, type = "expected", process_error = TRUE))
  testthat::expect_gt(max(abs(p_F$estimate - p_T$estimate)), 1e-6)
})


test_that("closure-unit type='latent_state' round-trips the marginaleffects pipeline", {
  # marginaleffects ships the mvgam latent-state token as `latent_N`;
  # mvgam's user-facing token is `latent_state`. conditional_effects
  # translates one to the other so the upstream sanitize_type() gate
  # passes without patching the marginaleffects namespace. get_predict
  # accepts both tokens and maps them to predict(type='latent_state').
  require_fixtures("val_occ_mvgam.rds")
  occ_fit <- readRDS(file.path(local_fixture_dir(), "val_occ_mvgam.rds"))

  ce <- suppressWarnings(
    conditional_effects(occ_fit, type = "latent_state")
  )
  testthat::expect_s3_class(ce, "mvgam_conditional_effects")
  testthat::expect_true(length(ce) >= 1L)
  testthat::expect_s3_class(ce[[1L]], "ggplot")

  gp_state <- get_predict(occ_fit, newdata = occ_fit$data,
                          type = "latent_state")
  gp_n <- get_predict(occ_fit, newdata = occ_fit$data,
                      type = "latent_N")
  testthat::expect_equal(gp_state$estimate, gp_n$estimate)
})


# -- get_group_names for ordinal ---------------------------------------

test_that("get_group_names.mvgam returns factor levels for ordinal", {
  require_fixtures("val_mvgam_cumulative_fx.rds")
  mv <- load_mvgam("cumulative_fx")
  expected <- levels(mv$data[[mv$response_names[1L]]])
  testthat::expect_equal(get_group_names(mv), expected)
})

test_that("get_group_names.mvgam returns default for non-ordinal", {
  require_fixtures("val_mvgam_beta_ar1.rds")
  mv <- load_mvgam("beta_ar1")
  testthat::expect_equal(get_group_names(mv), "main_marginaleffect")
})


# -- Edge: unseen times silently fall back to per-series marginal mean -

test_that("predictions at unseen times use marginal mean (no error)", {
  require_fixtures("val_mvgam_ar1_int.rds")
  mv <- load_mvgam("ar1_int")
  fit_max_time <- max(mv$data$time)
  nd_oos <- data.frame(
    time = (fit_max_time + 1L):(fit_max_time + 3L),
    series = mv$data$series[1L],
    y = NA_integer_
  )
  # The primitive emits an info on first call; tests/local always
  # passes through, so just verify it returns finite values.
  ep <- posterior_epred(mv, newdata = nd_oos, process_error = FALSE)
  testthat::expect_equal(ncol(ep), 3L)
  testthat::expect_true(all(is.finite(ep)))
})


# -- conditional_effects.mvgam ----------------------------------------

test_that("conditional_effects(mv) detects formula terms automatically", {
  require_fixtures("val_mvgam_ar1_gp.rds")
  mv <- load_mvgam("ar1_gp")
  ce <- suppressWarnings(conditional_effects(mv))
  testthat::expect_s3_class(ce, "mvgam_conditional_effects")
  testthat::expect_true("z" %in% names(ce))
  testthat::expect_s3_class(ce[[1L]], "ggplot")
})

test_that("conditional_effects(mv, type = link) routes through plot_predictions", {
  require_fixtures("val_mvgam_ar1_gp.rds")
  mv <- load_mvgam("ar1_gp")
  ce <- suppressWarnings(conditional_effects(mv, type = "link"))
  testthat::expect_s3_class(ce, "mvgam_conditional_effects")
  testthat::expect_true(length(ce) >= 1L)
})

test_that("conditional_effects detects tensor-product interactions", {
  require_fixtures("val_mvgam_ar1_t2_noint.rds")
  mv <- load_mvgam("ar1_t2_noint")
  # ar1_t2_noint is an orphan fixture built outside build_fixtures.R
  # and still carries a `group` column that marginaleffects forbids.
  mv$data$group <- NULL
  ce <- suppressWarnings(conditional_effects(mv))
  testthat::expect_s3_class(ce, "mvgam_conditional_effects")
  testthat::expect_true(any(grepl(":", names(ce))))
})

test_that("conditional_effects honours user-supplied `effects`", {
  require_fixtures("val_mvgam_ar1_fx.rds")
  mv <- load_mvgam("ar1_fx")
  ce <- suppressWarnings(conditional_effects(mv, effects = "x"))
  testthat::expect_equal(length(ce), 1L)
  testthat::expect_equal(names(ce), "x")
})

test_that("plot.mvgam_conditional_effects returns the list invisibly", {
  require_fixtures("val_mvgam_ar1_gp.rds")
  mv <- load_mvgam("ar1_gp")
  ce <- suppressWarnings(conditional_effects(mv))
  out <- plot(ce, plot = FALSE)
  testthat::expect_identical(out, ce)
})


# -- series argument (state-space deviation from brms) ----------------
# `series = NULL` brms-parity behaviour is already covered by the
# default-call tests above.

test_that("conditional_effects(series = 'all') appends series to condition", {
  require_fixtures("val_mvgam_ar1_gp.rds")
  mv <- load_mvgam("ar1_gp")
  ce <- suppressWarnings(conditional_effects(mv, series = "all"))
  testthat::expect_s3_class(ce, "mvgam_conditional_effects")
  testthat::expect_s3_class(ce[[1L]], "ggplot")
})

test_that("conditional_effects(series = <chr>) filters to one series", {
  require_fixtures("val_mvgam_ar1_gp.rds")
  mv <- load_mvgam("ar1_gp")
  one_level <- levels(mv$data$series)[1L]
  ce <- suppressWarnings(
    conditional_effects(mv, series = one_level)
  )
  testthat::expect_s3_class(ce, "mvgam_conditional_effects")
  testthat::expect_s3_class(ce[[1L]], "ggplot")
})

test_that("conditional_effects(series = <int>) resolves to factor level", {
  require_fixtures("val_mvgam_ar1_gp.rds")
  mv <- load_mvgam("ar1_gp")
  ce <- suppressWarnings(conditional_effects(mv, series = 1L))
  testthat::expect_s3_class(ce, "mvgam_conditional_effects")
})

test_that("conditional_effects rejects an unknown series level", {
  require_fixtures("val_mvgam_ar1_gp.rds")
  mv <- load_mvgam("ar1_gp")
  testthat::expect_error(
    conditional_effects(mv, series = "not_a_series"),
    regexp = "not one of the model's series levels"
  )
})

test_that("conditional_effects rejects an out-of-range series index", {
  require_fixtures("val_mvgam_ar1_gp.rds")
  mv <- load_mvgam("ar1_gp")
  n_levels <- length(levels(mv$data$series))
  testthat::expect_error(
    conditional_effects(mv, series = n_levels + 1L),
    regexp = "upper"
  )
})

test_that("conditional_effects rejects malformed series arg", {
  require_fixtures("val_mvgam_ar1_gp.rds")
  mv <- load_mvgam("ar1_gp")
  testthat::expect_error(
    conditional_effects(mv, series = c("a", "b")),
    regexp = "NULL, 'all', a series name"
  )
})


# -- Binomial trials propagate through datagrid -----------------------

test_that("binomial trials carry through datagrid + predictions", {
  require_fixtures("val_mvgam_binom_ar1.rds")
  mv <- load_mvgam("binom_ar1")
  grid <- datagrid(x = 0, trials = c(10, 50, 100), model = mv)
  testthat::expect_true("trials" %in% names(grid))
  p <- suppressWarnings(predictions(mv, newdata = grid, type = "expected",
                                     process_error = FALSE))
  # epred for binomial = p * trials. At fixed x, estimates should
  # be approximately proportional to trials.
  testthat::expect_lt(
    abs(p$estimate[2L] / p$estimate[1L] - 5),
    0.05
  )
  testthat::expect_lt(
    abs(p$estimate[3L] / p$estimate[1L] - 10),
    0.05
  )
})


# ====================================================================
# vcov, loo_R2, loo_predict and loo_model_weights
# ====================================================================
# Each is held to its own definition on a fixed-effects fixture: a
# covariance against its own correlation, a summary against the
# draws it summarised. They live in `local/` because they run on a
# full posterior, which is too heavy for CI.


test_that("vcov carries the model's own fixed effects", {
  require_fixtures("val_mvgam_ar1_fx.rds")
  mv <- load_mvgam("ar1_fx")
  vm <- vcov(mv)
  # The columns are this model's terms, taken from its own formula
  # rather than from a second fit that happens to agree. `b[k]` is
  # aliased to `b_<term>`, so a failure of that aliasing shows here.
  expect_setequal(colnames(vm), c("Intercept", "x"))
  expect_identical(colnames(vm), rownames(vm))
  # Symmetric and positive definite, which a covariance must be.
  expect_equal(vm, t(vm), tolerance = 1e-12)
  expect_true(all(diag(vm) > 0))
  expect_true(all(eigen(vm, only.values = TRUE)$values > 0))
  # correlation = TRUE rescales the same matrix, so the diagonal
  # goes to one and the off-diagonal stays inside [-1, 1].
  cm <- vcov(mv, correlation = TRUE)
  expect_equal(unname(diag(cm)), c(1, 1), tolerance = 1e-12)
  expect_true(all(abs(cm[upper.tri(cm)]) <= 1))
  expect_equal(cm[1L, 2L],
               vm[1L, 2L] / sqrt(vm[1L, 1L] * vm[2L, 2L]),
               tolerance = 1e-10)
})


test_that("loo_R2 summarises the draws it computed", {
  require_fixtures("val_mvgam_ar1_fx.rds")
  mv <- load_mvgam("ar1_fx")
  set.seed(1L)
  r2 <- suppressWarnings(loo_R2(mv))
  expect_setequal(colnames(r2),
                  c("Estimate", "Est.Error", "Q2.5", "Q97.5"))
  # A proportion of variance explained, clamped above at one.
  expect_true(is.finite(r2[, "Estimate"]))
  expect_lte(r2[, "Estimate"], 1)
  expect_true(r2[, "Q2.5"] <= r2[, "Estimate"])
  expect_true(r2[, "Estimate"] <= r2[, "Q97.5"])

  # The summary has to be the summary of its own draws. Comparing a
  # point estimate against a second package's tells you the two
  # agree; comparing it against the vector it came from is what
  # catches a summary taken over the wrong margin.
  set.seed(1L)
  draws <- suppressWarnings(loo_R2(mv, summary = FALSE))
  expect_equal(unname(r2[, "Estimate"]), mean(draws[, 1L]),
               tolerance = 1e-8)
  expect_equal(unname(r2[, "Est.Error"]), stats::sd(draws[, 1L]),
               tolerance = 1e-8)
  expect_equal(
    unname(r2[, c("Q2.5", "Q97.5")]),
    unname(stats::quantile(draws[, 1L], c(0.025, 0.975))),
    tolerance = 1e-8
  )
})


test_that("loo_R2(mvgam, summary = FALSE) returns per-draw vector", {
  require_fixtures("val_mvgam_ar1_fx.rds")
  mv <- load_mvgam("ar1_fx")
  set.seed(1L)
  r2 <- suppressWarnings(loo_R2(mv, summary = FALSE))
  expect_true(is.matrix(r2))
  expect_identical(ncol(r2), 1L)
  # Clamped to [-1, 1].
  expect_true(all(r2 >= -1 & r2 <= 1))
})


test_that("loo_predict(mvgam, type = 'mean') returns finite length-N vector", {
  require_fixtures("val_mvgam_ar1_fx.rds")
  mv <- load_mvgam("ar1_fx")
  lp <- suppressMessages(suppressWarnings(
    loo_predict(mv, type = "mean")
  ))
  expect_true(is.numeric(lp))
  expect_identical(length(lp), nrow(mv$data))
  expect_true(all(is.finite(lp)))
})


test_that("loo_model_weights(mvgam, mvgam) returns named stacking weights", {
  require_fixtures("val_mvgam_ar1_fx.rds", "val_mvgam_ar1_re.rds")
  m1 <- load_mvgam("ar1_fx")
  m2 <- load_mvgam("ar1_re")
  w <- suppressWarnings(
    loo_model_weights(m1, m2, model_names = c("fx", "re"))
  )
  # loo_model_weights returns a named "stacking_weights" object;
  # treat it as a numeric vector for these assertions.
  expect_identical(length(as.numeric(w)), 2L)
  labels <- rownames(w)
  if (is.null(labels)) labels <- names(w)
  expect_setequal(labels, c("fx", "re"))
  # Weights are non-negative and sum to 1.
  expect_true(all(as.numeric(w) >= 0))
  expect_equal(sum(as.numeric(w)), 1, tolerance = 1e-6)
})


test_that("add_criterion(mvgam) populates $criteria with loo + bayes_R2", {
  require_fixtures("val_mvgam_ar1_fx.rds")
  mv <- load_mvgam("ar1_fx")
  set.seed(1L)
  mv2 <- suppressWarnings(add_criterion(mv, c("loo", "bayes_R2")))
  expect_setequal(names(mv2$criteria), c("loo", "bayes_R2"))
  expect_s3_class(mv2$criteria$loo, "psis_loo")
  expect_true(is.matrix(mv2$criteria$bayes_R2))
})


test_that("LOO and WAIC alias the lowercase functions", {
  require_fixtures("val_mvgam_ar1_fx.rds")
  mv <- load_mvgam("ar1_fx")
  set.seed(1L)
  l_lo <- suppressWarnings(loo(mv))
  set.seed(1L)
  l_hi <- suppressWarnings(LOO(mv))
  expect_equal(l_lo$estimates, l_hi$estimates)
  set.seed(1L)
  w_lo <- suppressWarnings(waic(mv))
  set.seed(1L)
  w_hi <- suppressWarnings(WAIC(mv))
  expect_equal(w_lo$estimates, w_hi$estimates)
})


test_that("loo_moment_match.mvgam errors informatively", {
  require_fixtures("val_mvgam_ar1_fx.rds")
  mv <- load_mvgam("ar1_fx")
  expect_error(loo_moment_match(mv),
                "not currently supported")
})


test_that("loo_subsample.mvgam errors informatively", {
  require_fixtures("val_mvgam_ar1_fx.rds")
  mv <- load_mvgam("ar1_fx")
  expect_error(loo_subsample(mv),
                "not currently supported")
})


# ====================================================================
# update.mvgam
# ====================================================================
# Numerical refit checks vs the cached fit. These live in `local/`
# because they instantiate full MCMC fits via cmdstanr. The backend
# stancode cache means most calls skip the actual Stan compile.


test_that("update(mvgam) reuses inherited slots and refits cleanly", {
  require_fixtures("val_mvgam_ar1_fx.rds")
  mv <- load_mvgam("ar1_fx")
  refit <- suppressWarnings(suppressMessages(
    update(mv, iter = 200, warmup = 100, chains = 1,
            silent = 2, refresh = 0)
  ))
  expect_s3_class(refit, "mvgam")
  # New sampler dimensions land on the fit.
  expect_identical(
    posterior::nchains(posterior::as_draws_array(refit$fit)), 1L
  )
  # Inherited slots are preserved.
  expect_identical(refit$family$family, mv$family$family)
  expect_identical(refit$backend, mv$backend)
  # Formula round-trips.
  expect_identical(deparse(refit$formula), deparse(mv$formula))
})


test_that("update(mvgam, newdata = subset) refits on the new data", {
  require_fixtures("val_mvgam_ar1_fx.rds")
  mv <- load_mvgam("ar1_fx")
  nd <- mv$data[1:20, ]
  refit <- suppressWarnings(suppressMessages(
    update(mv, newdata = nd, iter = 200, warmup = 100, chains = 1,
            silent = 2, refresh = 0)
  ))
  expect_s3_class(refit, "mvgam")
  expect_identical(nrow(refit$data), 20L)
})


test_that("update(mvgam, formula. = ~ . + 1) is a no-op formula change", {
  require_fixtures("val_mvgam_ar1_fx.rds")
  mv <- load_mvgam("ar1_fx")
  refit <- suppressWarnings(suppressMessages(
    update(mv, formula. = ~ . + 1, iter = 200, warmup = 100,
            chains = 1, silent = 2, refresh = 0)
  ))
  expect_s3_class(refit, "mvgam")
  # Same fixed-effect set after the no-op update.
  expect_setequal(rownames(fixef(refit)), rownames(fixef(mv)))
})


test_that("update(recompile = FALSE) succeeds when stancode is unchanged", {
  require_fixtures("val_mvgam_ar1_fx.rds")
  mv <- load_mvgam("ar1_fx")
  refit <- suppressWarnings(suppressMessages(
    update(mv, recompile = FALSE, iter = 200, warmup = 100,
            chains = 1, silent = 2, refresh = 0)
  ))
  expect_s3_class(refit, "mvgam")
})


test_that("update(recompile = FALSE) accepts data-only refits", {
  require_fixtures("val_mvgam_ar1_fx.rds")
  mv <- load_mvgam("ar1_fx")
  # brms parametrises b[Kc] with Kc as a data variable, so adding
  # a new fixed-effect term grows the X design matrix without
  # mutating the stancode body. recompile = FALSE should pass.
  refit <- suppressWarnings(suppressMessages(
    update(mv, formula. = ~ . + I(x^2), recompile = FALSE,
            iter = 200, warmup = 100, chains = 1,
            silent = 2, refresh = 0)
  ))
  expect_s3_class(refit, "mvgam")
})


test_that("update(recompile = FALSE, family = new_family) errors", {
  require_fixtures("val_mvgam_ar1_fx.rds")
  mv <- load_mvgam("ar1_fx")
  expect_error(
    update(mv, recompile = FALSE, family = gaussian()),
    "incompatible with the requested"
  )
})


# ---------------------------------------------------------------------
# ranef and VarCorr
# ---------------------------------------------------------------------
#
# Three random-effect fixtures: intercept only, intercept beside a
# smooth, and a correlated slope. Each table is held to the levels of
# the grouping factor in the frame that built it, and to its own
# internal agreement, rather than to what another package returns.
# One helper for the claim all three random-effect fixtures make.
# A group-level table is keyed by the levels of the grouping factor
# in the frame's own order, and asking a second package whether it
# produced the same dimnames cannot catch a table keyed correctly but
# by the wrong column.
expect_ranef_keyed <- function(mv, coefs) {
  re <- ranef(mv)
  lev <- levels(mv$data$grp)
  expect_named(re, "grp")
  expect_identical(dim(re$grp),
                   c(length(lev), 4L, length(coefs)))
  expect_identical(dimnames(re$grp)[[1L]], lev)
  expect_identical(dimnames(re$grp)[[2L]],
                   c("Estimate", "Est.Error", "Q2.5", "Q97.5"))
  expect_identical(dimnames(re$grp)[[3L]], coefs)
  for (cf in coefs) {
    est <- re$grp[, "Estimate", cf]
    expect_true(all(is.finite(est)))
    # Group-level effects are deviations from the population term,
    # so they are centred and they vary. A block of zeros, or one
    # constant repeated down the levels, satisfies every shape
    # claim above and means no level was distinguished.
    expect_lt(abs(mean(est)), 0.5)
    expect_gt(stats::sd(est), 1e-6)
    expect_true(all(re$grp[, "Est.Error", cf] > 0))
    expect_true(all(re$grp[, "Q2.5", cf] <= est))
    expect_true(all(est <= re$grp[, "Q97.5", cf]))
  }
  invisible(re)
}


test_that("ranef is keyed by the grouping factor's own levels", {
  require_fixtures("val_mvgam_ar1_re.rds")
  expect_ranef_keyed(load_mvgam("ar1_re"), "Intercept")
})


test_that("ranef keeps that keying beside a smooth", {
  require_fixtures("val_mvgam_ar1_re_smooth.rds")
  expect_ranef_keyed(load_mvgam("ar1_re_smooth"), "Intercept")
})


test_that("ranef carries both coefficients when the slope varies", {
  require_fixtures("val_mvgam_ar1_cor_re.rds")
  mv <- load_mvgam("ar1_cor_re")
  re <- expect_ranef_keyed(mv, c("Intercept", "x"))
  # The two coefficients have to be told apart. A correlated slope
  # model that returned the intercept deviations twice would pass
  # every claim in the helper.
  expect_false(isTRUE(all.equal(re$grp[, "Estimate", "Intercept"],
                                re$grp[, "Estimate", "x"],
                                check.attributes = FALSE)))
})


test_that("ranef(summary = FALSE) returns a draws-shaped 3D array", {
  require_fixtures("val_mvgam_ar1_cor_re.rds")
  mv <- load_mvgam("ar1_cor_re")
  raw <- ranef(mv, summary = FALSE)
  # [n_draws, n_levels, n_coefs]
  expect_identical(length(dim(raw$grp)), 3L)
  expect_identical(dim(raw$grp)[2L], length(levels(mv$data$grp)))
  expect_identical(dim(raw$grp)[3L], 2L)
  expect_true(!is.null(attr(raw$grp, "nchains")))
})


test_that("VarCorr(mvgam) returns sd-only structure for M = 1", {
  require_fixtures("val_mvgam_ar1_re.rds")
  mv <- load_mvgam("ar1_re")
  vc <- VarCorr(mv)
  expect_named(vc, "grp")
  expect_named(vc$grp, "sd")
  expect_identical(dim(vc$grp$sd), c(1L, 4L))
  expect_identical(dimnames(vc$grp$sd)[[1L]], "Intercept")
  expect_identical(
    dimnames(vc$grp$sd)[[2L]],
    c("Estimate", "Est.Error", "Q2.5", "Q97.5")
  )
})


test_that("VarCorr's three blocks describe one covariance", {
  require_fixtures("val_mvgam_ar1_cor_re.rds")
  mv <- load_mvgam("ar1_cor_re")
  vc <- VarCorr(mv)
  coefs <- c("Intercept", "x")
  expect_named(vc, "grp")
  expect_true(all(c("sd", "cor", "cov") %in% names(vc$grp)))
  expect_identical(dimnames(vc$grp$sd)[[1L]], coefs)
  expect_identical(dim(vc$grp$cor)[1L], 2L)
  expect_identical(dim(vc$grp$cov)[1L], 2L)

  # `sd`, `cor` and `cov` are three views of one matrix, so within a
  # draw the covariance diagonal is the squared scale and the
  # correlation is the covariance rescaled by it. The identity holds
  # per draw and not on the summaries, because a posterior mean of a
  # square is not the square of a posterior mean: measured here,
  # E[cov_11] is 0.774 against (E[sd_1])^2 of 0.525, and the gap is
  # Var(sd_1) to three decimals. So the claim is made where it is
  # true, and exactly rather than under a tolerance.
  raw <- VarCorr(mv, summary = FALSE)
  sd_d <- raw$grp$sd
  cov_d <- raw$grp$cov
  cor_d <- raw$grp$cor
  expect_identical(dim(sd_d), c(as.integer(ndraws(mv)), 2L))
  expect_identical(dim(cov_d), c(as.integer(ndraws(mv)), 2L, 2L))
  expect_equal(as.numeric(cbind(cov_d[, 1L, 1L], cov_d[, 2L, 2L])),
               as.numeric(sd_d^2))
  expect_equal(as.numeric(cor_d[, 1L, 2L]),
               as.numeric(cov_d[, 1L, 2L] /
                            (sd_d[, 1L] * sd_d[, 2L])))
  expect_equal(as.numeric(cor_d[, 1L, 1L]), rep(1, nrow(cor_d)))
  expect_true(all(sd_d > 0))
  expect_true(all(abs(cor_d[, 1L, 2L]) <= 1))

  # And the summary is the summary of those draws, which ties the
  # two levels together: the gap the means show is the variance the
  # draws carry, so a summary taken over the wrong margin fails.
  expect_equal(vc$grp$sd[, "Estimate"], colMeans(sd_d),
               tolerance = 1e-8, ignore_attr = TRUE)
  expect_equal(
    unname(vc$grp$cov[1L, "Estimate", 1L] -
             vc$grp$sd[1L, "Estimate"]^2),
    stats::var(sd_d[, 1L]) * (nrow(sd_d) - 1L) / nrow(sd_d),
    tolerance = 1e-6
  )
})


test_that("VarCorr(summary = FALSE) returns per-draw arrays", {
  require_fixtures("val_mvgam_ar1_cor_re.rds")
  mv <- load_mvgam("ar1_cor_re")
  raw <- VarCorr(mv, summary = FALSE)
  expect_identical(length(dim(raw$grp$sd)), 2L)
  expect_identical(length(dim(raw$grp$cor)), 3L)
  expect_identical(length(dim(raw$grp$cov)), 3L)
  # cor must be exactly 1 on the diagonal across every draw.
  expect_true(all(raw$grp$cor[, 1L, 1L] == 1))
  expect_true(all(raw$grp$cor[, 2L, 2L] == 1))
})


# ---------------------------------------------------------------------
# Tier-5 supporting checks: variable aliasing through update()
# ---------------------------------------------------------------------

test_that("update(mvgam) round-trips RE structure cleanly", {
  require_fixtures("val_mvgam_ar1_cor_re.rds")
  mv <- load_mvgam("ar1_cor_re")
  # Update with tighter sampler dimensions to confirm the refit
  # path preserves the brms-native RE parameter names without
  # special-casing.
  refit <- suppressWarnings(suppressMessages(
    update(mv, iter = 200, warmup = 100, chains = 1,
            silent = 2, refresh = 0)
  ))
  expect_s3_class(refit, "mvgam")
  vars <- variables(refit)
  expect_true(any(grepl("^r_grp\\[", vars)))
  expect_true(any(grepl("^sd_grp__", vars)))
  expect_true(any(grepl("^cor_grp__", vars)))
})


# ---------------------------------------------------------------------
# posterior_smooths and conditional_smooths
# ---------------------------------------------------------------------
#
# Four smooth shapes: a plain s(z), a trend-side smooth, a by-factor
# expansion and a two-dimensional tensor. Each is asked whether it
# was evaluated over the covariates it names, which is what
# separates a tensor from a curve and a by-factor from one shared
# curve.
test_that("smooths names the terms this model's own formula holds", {
  require_fixtures("val_mvgam_ar1_re_smooth.rds")
  mv <- load_mvgam("ar1_re_smooth")
  # The expected list comes from the fit's own formula rather than
  # from a second fit that was given the same one. Parsing the
  # formula answers what the model was asked for; `smooths()` has to
  # answer the same thing.
  expected <- attr(
    terms(brms::brmsterms(mv$formula)$dpars$mu$sm), "term.labels"
  )
  expect_identical(smooths(mv), expected)
  expect_identical(smooths(mv), "s(z)")
})


test_that("posterior_smooths returns one column per row it was given", {
  require_fixtures("val_mvgam_ar1_re_smooth.rds")
  mv <- load_mvgam("ar1_re_smooth")
  eta <- posterior_smooths(mv, smooth = "s(z)")
  expect_identical(dim(eta),
                   c(as.integer(ndraws(mv)), nrow(mv$data)))
  expect_true(all(is.finite(eta)))

  # A smooth is centred and it bends. A basis that failed to reach
  # the draws returns zeros, and a linear term returns something
  # that a straight line fits exactly; both keep the dimensions
  # above, and asking a second package for its dimensions catches
  # neither.
  med <- apply(eta, 2L, stats::median)
  expect_gt(stats::sd(med), 1e-6)
  expect_lt(abs(mean(med)), 1)
  fit_lin <- stats::lm(med ~ mv$data$z)
  expect_gt(summary(fit_lin)$sigma, 1e-6)

  # The columns follow the frame's own row order, which a shuffled
  # frame is what tests. A smooth evaluated by position rather than
  # by content answers the same numbers in the original order and
  # fails here while keeping every dimension.
  perm <- c(seq(2L, nrow(mv$data)), 1L)
  shuffled <- mv$data[perm, , drop = FALSE]
  eta_s <- posterior_smooths(mv, smooth = "s(z)", newdata = shuffled)
  expect_identical(dim(eta_s), dim(eta))
  expect_equal(apply(eta_s, 2L, stats::median), med[perm],
               tolerance = 1e-8)
})


test_that("posterior_smooths(mvgam) respects ndraws and draw_ids", {
  require_fixtures("val_mvgam_ar1_re_smooth.rds")
  mv <- load_mvgam("ar1_re_smooth")
  full <- posterior_smooths(mv, smooth = "s(z)")
  expect_identical(
    nrow(posterior_smooths(mv, smooth = "s(z)", ndraws = 50L)),
    50L
  )
  ids <- c(1L, 5L, 10L)
  picked <- posterior_smooths(mv, smooth = "s(z)", draw_ids = ids)
  expect_identical(nrow(picked), length(ids))
  expect_equal(picked, full[ids, , drop = FALSE])
})


test_that("posterior_smooths(mvgam) accepts user-supplied newdata", {
  require_fixtures("val_mvgam_ar1_re_smooth.rds")
  mv <- load_mvgam("ar1_re_smooth")
  nd <- data.frame(
    z = seq(min(mv$data$z), max(mv$data$z), length.out = 25L),
    x = 0,
    grp = factor("a", levels = levels(mv$data$grp))
  )
  out <- posterior_smooths(mv, smooth = "s(z)", newdata = nd)
  expect_identical(ncol(out), 25L)
})


test_that("conditional_smooths returns an evaluated curve", {
  require_fixtures("val_mvgam_ar1_re_smooth.rds")
  mv <- load_mvgam("ar1_re_smooth")
  cs <- conditional_smooths(mv)
  # mvgam returns its own class so `plot()` picks up the house
  # theme. What downstream code reads is the frame inside it, and
  # these are the columns brms's own renderer requires.
  expect_s3_class(cs, "mvgam_conditional_smooths")
  expect_length(cs, 1L)
  d <- cs[[1L]]
  expect_true(all(c("z", "effect1__", "estimate__", "se__",
                    "lower__", "upper__") %in% colnames(d)))
  expect_identical(nrow(d), 100L)

  # The curve has to be a curve. Finding 34 is what happens when
  # this frame is right and nothing checks that it was evaluated:
  # a constant estimate with a zero-width band satisfies the column
  # list and the row count.
  expect_gt(stats::sd(d$estimate__), 1e-6)
  expect_true(all(d$se__ > 0))
  expect_true(all(d$lower__ <= d$estimate__))
  expect_true(all(d$estimate__ <= d$upper__))
  expect_gt(mean(d$upper__ - d$lower__), 0)
  # The grid spans the covariate it was built from and is ordered.
  expect_identical(d$z, sort(d$z))
  expect_gte(min(d$z), min(mv$data$z))
  expect_lte(max(d$z), max(mv$data$z))
})


test_that("conditional_smooths(mvgam) plot dispatch works", {
  require_fixtures("val_mvgam_ar1_re_smooth.rds")
  mv <- load_mvgam("ar1_re_smooth")
  cs <- conditional_smooths(mv)
  # brms inherits the plot method; check it returns a list of
  # ggplots without error.
  p <- suppressWarnings(plot(cs, plot = FALSE))
  expect_type(p, "list")
  expect_true(length(p) >= 1L)
  expect_s3_class(p[[1L]], "ggplot")
})


test_that("posterior_smooths(mvgam) works on trend-side smooths", {
  require_fixtures("val_mvgam_ar1_re_smooth_trend.rds")
  mv <- load_mvgam("ar1_re_smooth_trend")
  expect_true("s(z)" %in% smooths(mv))
  eta <- posterior_smooths(mv, smooth = "s(z)")
  expect_identical(length(dim(eta)), 2L)
  # Trend-side smooth has 30 grid points (matches training data).
  expect_identical(ncol(eta), 30L)
})


test_that("s(z, by = grp) gives every level its own curve", {
  require_fixtures("val_mvgam_ar1_s_by.rds")
  mv <- load_mvgam("ar1_s_by")
  lev <- levels(mv$data$grp)
  eta <- posterior_smooths(mv, smooth = "s(z, by = grp)")
  expect_identical(dim(eta),
                   c(as.integer(ndraws(mv)), nrow(mv$data)))

  # A by-factor smooth emits one basis block per level, so the
  # conditional grid carries a curve for each. Comparing the eta
  # matrix's dimensions against another package's says nothing
  # about whether the levels were separated at all: a model that
  # fitted one shared curve returns exactly this shape.
  cs <- conditional_smooths(mv)[[1L]]
  expect_true("grp" %in% colnames(cs))
  expect_setequal(as.character(unique(cs$grp)), lev)
  expect_identical(nrow(cs), 100L * length(lev))
  curves <- split(cs$estimate__, cs$grp)
  expect_length(curves, length(lev))
  for (k in seq_along(curves)) {
    expect_gt(stats::sd(curves[[k]]), 1e-6)
  }
  # The levels have to differ from one another, which is the whole
  # point of writing `by =`.
  expect_false(isTRUE(all.equal(curves[[1L]], curves[[2L]])))
})


test_that("conditional_smooths(mvgam) facets by-factor on real fit", {
  require_fixtures("val_mvgam_ar1_s_by.rds")
  mv <- load_mvgam("ar1_s_by")
  cs <- conditional_smooths(mv)
  expect_s3_class(cs, "mvgam_conditional_smooths")
  expect_true(length(cs) >= 1L)
  # By-factor smooth: cond__ column should carry per-level facet
  # labels.
  expect_true(length(unique(cs[[1L]]$cond__)) >= 3L ||
    !is.null(attr(cs[[1L]], "effects")))
})


test_that("t2(z, w) is evaluated over two covariates, not one", {
  require_fixtures("val_mvgam_ar1_t2.rds")
  mv <- load_mvgam("ar1_t2")
  eta <- posterior_smooths(mv, smooth = "t2(z, w)")
  expect_identical(dim(eta),
                   c(as.integer(ndraws(mv)), nrow(mv$data)))
  expect_true(all(is.finite(eta)))

  # A tensor smooth is a surface, so its conditional grid is a grid:
  # both margins appear, and the row count is their product rather
  # than either margin alone. A tensor collapsed to one covariate
  # returns the same eta dimensions as this and a grid of 100.
  cs <- conditional_smooths(mv)[[1L]]
  expect_true(all(c("z", "w", "effect1__", "effect2__") %in%
                    colnames(cs)))
  expect_identical(nrow(cs), 100L * 100L)
  expect_identical(length(unique(cs$z)), 100L)
  expect_identical(length(unique(cs$w)), 100L)

  # The surface has to vary along both margins. One that moved with
  # `z` alone would be a `s(z)` wearing a tensor's shape.
  along_w <- vapply(split(cs$estimate__, cs$z),
                    stats::sd, numeric(1L))
  along_z <- vapply(split(cs$estimate__, cs$w),
                    stats::sd, numeric(1L))
  expect_gt(mean(along_w), 1e-6)
  expect_gt(mean(along_z), 1e-6)
})


test_that("conditional_smooths(mvgam) 2D surface=TRUE returns full grid", {
  require_fixtures("val_mvgam_ar1_t2.rds")
  mv <- load_mvgam("ar1_t2")
  cs <- conditional_smooths(mv, resolution = 20L)
  # 20 x 20 = 400 grid points by default for a 2D surface smooth.
  expect_identical(nrow(cs[[1L]]), 400L)
  expect_true(attr(cs[[1L]], "surface"))
  expect_identical(attr(cs[[1L]], "effects"), c("z", "w"))
})


test_that("conditional_smooths(mvgam) 2D surface=FALSE facets the second covariate", {
  require_fixtures("val_mvgam_ar1_t2.rds")
  mv <- load_mvgam("ar1_t2")
  cs <- conditional_smooths(mv, surface = FALSE,
                              resolution = 20L, facets = 3L)
  # 20 (focal) x 3 (facets) = 60 rows.
  expect_identical(nrow(cs[[1L]]), 60L)
  expect_false(attr(cs[[1L]], "surface"))
})


test_that("conditional_smooths(mvgam) facets argument controls facet count", {
  require_fixtures("val_mvgam_ar1_t2.rds")
  mv <- load_mvgam("ar1_t2")
  cs5 <- conditional_smooths(mv, surface = FALSE,
                              resolution = 10L, facets = 5L)
  cs2 <- conditional_smooths(mv, surface = FALSE,
                              resolution = 10L, facets = 2L)
  expect_identical(nrow(cs5[[1L]]), 50L)
  expect_identical(nrow(cs2[[1L]]), 20L)
})


test_that("conditional_smooths(mvgam) int_conditions overrides covariate values", {
  require_fixtures("val_mvgam_ar1_re_smooth.rds")
  mv <- load_mvgam("ar1_re_smooth")
  cs <- conditional_smooths(mv,
                              int_conditions = list(z = c(-1, 0, 1)))
  # int_conditions on the focal covariate overrides the full grid.
  expect_identical(nrow(cs[[1L]]), 3L)
})


test_that("conditional_smooths(mvgam) spaghetti returns draws overlay", {
  require_fixtures("val_mvgam_ar1_re_smooth.rds")
  mv <- load_mvgam("ar1_re_smooth")
  cs <- conditional_smooths(mv, spaghetti = TRUE, ndraws = 20L)
  spa <- attr(cs[[1L]], "spaghetti")
  expect_s3_class(spa, "data.frame")
  expect_true("estimate__" %in% colnames(spa))
  expect_true("sample__" %in% colnames(spa))
  # 100 grid pts x 20 draws.
  expect_identical(nrow(spa), 2000L)
})


test_that("conditional_smooths(mvgam) restricts via smooths argument", {
  require_fixtures("val_mvgam_ar1_re_smooth.rds")
  mv <- load_mvgam("ar1_re_smooth")
  # Subset to s(z); the formula only has s(z) so output is length 1
  # either way, but this exercises the smooths-arg filter path.
  cs <- conditional_smooths(mv, smooths = "s(z)")
  expect_identical(length(cs), 1L)
  # Unknown smooth errors with the hint to use smooths(x).
  expect_error(
    conditional_smooths(mv, smooths = "s(nonexistent)"),
    "smooths"
  )
})


# ---------------------------------------------------------------------
# posterior_interval, predictive_interval, ngrps, predictive_error
# and the deprecated aliases
# ---------------------------------------------------------------------


test_that("posterior_interval quantiles the draws it names", {
  require_fixtures("val_mvgam_ar1_re_smooth.rds")
  mv <- load_mvgam("ar1_re_smooth")
  pi <- posterior_interval(mv)
  expect_identical(ncol(pi), 2L)
  expect_identical(colnames(pi), c("2.5%", "97.5%"))
  expect_true(all(pi[, 1L] <= pi[, 2L]))

  # The interval has to be the interval of this fit's own draws.
  # Matching a second package's column names says nothing about
  # which parameter each row describes.
  dm <- as_draws_matrix(mv)
  shared <- intersect(rownames(pi), colnames(dm))
  expect_gt(length(shared), 0L)
  for (v in utils::head(shared, 5L)) {
    expect_equal(
      unname(pi[v, ]),
      unname(stats::quantile(as.numeric(dm[, v]), c(0.025, 0.975))),
      tolerance = 1e-8
    )
  }
  # A different probability moves the bounds the right way.
  narrow <- posterior_interval(mv, prob = 0.5)
  expect_true(all(narrow[shared, 1L] >= pi[shared, 1L]))
  expect_true(all(narrow[shared, 2L] <= pi[shared, 2L]))
})


test_that("predictive_interval(mvgam) returns [nobs x 2] with brms cols", {
  require_fixtures("val_mvgam_ar1_re_smooth.rds")
  mv <- load_mvgam("ar1_re_smooth")
  pi <- predictive_interval(mv)
  expect_identical(ncol(pi), 2L)
  expect_identical(nrow(pi), nrow(mv$data))
  expect_identical(colnames(pi), c("5%", "95%"))
})


test_that("ngrps counts the levels the frame actually holds", {
  require_fixtures("val_mvgam_ar1_re_smooth.rds")
  mv <- load_mvgam("ar1_re_smooth")
  # The frame is the authority on how many groups there are, so
  # that is what this is held to. Two packages agreeing on a count
  # taken from the same design cannot catch a count taken from the
  # wrong column.
  ng <- ngrps(mv)
  expect_named(ng, "grp")
  expect_identical(as.integer(ng$grp), nlevels(mv$data$grp))
  # And it agrees with the table keyed by those same levels.
  expect_identical(as.integer(ng$grp),
                   dim(ranef(mv)$grp)[1L])
})


test_that("predictive_error(mvgam) returns [ndraws x nobs] error matrix", {
  require_fixtures("val_mvgam_ar1_re_smooth.rds")
  mv <- load_mvgam("ar1_re_smooth")
  err <- predictive_error(mv, ndraws = 50L)
  expect_identical(nrow(err), 50L)
  expect_identical(ncol(err), nrow(mv$data))
  # epred branch returns a different matrix; sanity-check the
  # branch toggle works without erroring.
  err_e <- predictive_error(
    mv, method = "posterior_epred", ndraws = 20L
  )
  expect_identical(dim(err_e), c(20L, nrow(mv$data)))
})


test_that("deprecated brms aliases dispatch to current methods", {
  require_fixtures("val_mvgam_ar1_re_smooth.rds")
  mv <- load_mvgam("ar1_re_smooth")
  # parnames -> variables; nsamples -> ndraws; both warn via the
  # brms generic.
  expect_identical(suppressWarnings(parnames(mv)), variables(mv))
  expect_identical(
    suppressWarnings(nsamples(mv)),
    posterior::ndraws(posterior::as_draws(mv$fit))
  )
})


# ------------------------------------------------------------------
# PSIS-weighted prediction and model averaging: loo_epred,
# loo_linpred, loo_predictive_interval, posterior_average and
# pp_average
# ------------------------------------------------------------------


# The leave-one-out surfaces are driven on two fixtures chosen for
# PSIS stability: binom_ar1, a Binomial AR(1) over 30 observations
# whose largest Pareto-k is 0.68, and gauss_ar1_n150, a Gaussian
# AR(1) over 150 with a smooth posterior. The two carry different
# links, which is what lets loo_epred and loo_linpred be told
# apart.









test_that("posterior_average.mvgam returns shape + attrs (brms-parity)", {
  require_fixtures("val_mvgam_ar1_fx.rds", "val_mvgam_ar1_int.rds")
  mv_a <- load_mvgam("ar1_fx")
  mv_b <- load_mvgam("ar1_int")
  # Equal weights bypass the stacking solver to keep this test
  # cheap and deterministic.
  out <- posterior_average(
    mv_a, mv_b, weights = c(0.5, 0.5),
    ndraws = 200L, seed = 1L
  )
  expect_s3_class(out, "data.frame")
  expect_identical(nrow(out), 200L)
  expect_true("b_Intercept" %in% colnames(out))
  w <- attr(out, "weights")
  expect_equal(unname(w), c(0.5, 0.5))
  nd <- attr(out, "ndraws")
  expect_equal(sum(nd), 200)
})


test_that("pp_average.mvgam summary shape matches posterior_summary", {
  require_fixtures("val_mvgam_ar1_fx.rds", "val_mvgam_ar1_int.rds")
  mv_a <- load_mvgam("ar1_fx")
  mv_b <- load_mvgam("ar1_int")
  out <- pp_average(
    mv_a, mv_b, weights = c(0.5, 0.5),
    method = "posterior_epred",
    ndraws = 200L, summary = TRUE, seed = 1L
  )
  expect_true(is.matrix(out))
  expect_identical(nrow(out), NROW(mv_a$data))
  expect_identical(ncol(out), 4L)
  expect_equal(unname(attr(out, "weights")), c(0.5, 0.5))
})


test_that("pp_average.mvgam draws=FALSE returns the averaged draw matrix", {
  require_fixtures("val_mvgam_ar1_fx.rds", "val_mvgam_ar1_int.rds")
  mv_a <- load_mvgam("ar1_fx")
  mv_b <- load_mvgam("ar1_int")
  out <- pp_average(
    mv_a, mv_b, weights = c(0.5, 0.5),
    ndraws = 200L, summary = FALSE, seed = 1L
  )
  expect_true(is.matrix(out))
  expect_identical(nrow(out), 200L)
  expect_identical(ncol(out), NROW(mv_a$data))
})


test_that("pp_average.mvgam errors on mismatched response variables", {
  require_fixtures("val_mvgam_ar1_fx.rds", "val_mvgam_beta_ar1.rds")
  mv_g <- load_mvgam("ar1_fx")
  mv_b <- load_mvgam("beta_ar1")
  expect_error(
    pp_average(mv_g, mv_b, weights = c(0.5, 0.5)),
    "same response"
  )
})


test_that("loo_epred and loo_linpred separate exactly at the link", {
  # Two fixtures whose links differ. The gaussian carries the
  # identity link, so its expectation and its linear predictor are
  # the same numbers and agreeing is the contract; the binomial
  # carries a logit, so they must not agree. Holding both at once
  # is what a comparison against another package's dimensions
  # cannot do: a method that skipped the inverse link entirely
  # passes on the gaussian and returns the wrong scale here.
  scales <- list(
    binom_ar1 = list(identity_link = FALSE, lower = 0),
    gauss_ar1_n150 = list(identity_link = TRUE, lower = -Inf)
  )
  for (name in names(scales)) {
    require_fixtures(paste0("val_mvgam_", name, ".rds"))
    mv <- load_mvgam(name)
    spec <- scales[[name]]
    e <- suppressMessages(suppressWarnings(loo_epred(mv, type = "mean")))
    l <- suppressMessages(suppressWarnings(loo_linpred(mv, type = "mean")))
    expect_identical(dim(e), c(nrow(mv$data), 1L))
    expect_identical(dim(l), dim(e))
    expect_true(all(is.finite(e)))
    expect_true(all(is.finite(l)))
    expect_true(all(e >= spec$lower))
    if (isTRUE(spec$identity_link)) {
      expect_equal(as.numeric(e), as.numeric(l), tolerance = 1e-8)
    } else {
      expect_false(isTRUE(all.equal(as.numeric(e), as.numeric(l))))
    }

    # A leave-one-out expectation is a reweighting of the posterior
    # one, so it tracks it without reproducing it. Equality would
    # mean no weights were applied.
    ep <- colMeans(posterior_epred(mv))
    expect_gt(stats::cor(as.numeric(e), ep), 0.5)
    expect_false(isTRUE(all.equal(as.numeric(e), unname(ep))))
  }
})


test_that("loo_predictive_interval brackets the data it left out", {
  # An interval is read for its coverage, so that is what it is
  # held to here rather than to another package's column names. The
  # nominal 90 per cent has to be approached from a small sample of
  # observations without collapsing or spanning everything.
  for (name in c("gauss_ar1_n150", "binom_ar1")) {
    require_fixtures(paste0("val_mvgam_", name, ".rds"))
    mv <- load_mvgam(name)
    pi <- suppressMessages(suppressWarnings(
      loo_predictive_interval(mv, prob = 0.9)
    ))
    expect_identical(dim(pi), c(nrow(mv$data), 2L))
    expect_identical(colnames(pi), c("q5", "q95"))
    expect_true(all(pi[, 2L] >= pi[, 1L]))
    expect_true(all(is.finite(pi)))

    covered <- mean(mv$data$y >= pi[, 1L] & mv$data$y <= pi[, 2L])
    expect_gt(covered, 0.75)
    expect_lte(covered, 1)
    # A band wide enough to hold everything would also satisfy the
    # coverage floor, so the width is bounded against the spread of
    # the response it is predicting.
    expect_lt(mean(pi[, 2L] - pi[, 1L]),
              8 * stats::sd(as.numeric(mv$data$y)))

    # A narrower request has to nest inside the wider one.
    tight <- suppressMessages(suppressWarnings(
      loo_predictive_interval(mv, prob = 0.5)
    ))
    expect_true(all(tight[, 1L] >= pi[, 1L]))
    expect_true(all(tight[, 2L] <= pi[, 2L]))
  }
})
