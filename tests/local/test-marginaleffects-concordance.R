# marginaleffects integration tests for mvgam.
#
# Lives in tests/local because the round-trip through
# marginaleffects -> insight -> get_predict -> posterior_epred is
# fixture-driven and too heavy for CI. Covers the full user surface:
# predictions / avg_predictions / avg_slopes / avg_comparisons.
#
# marginaleffects forbids any column literally named 'group' in the
# fit's training data (it reserves that name for its own output).
# Fixtures built by build_fixtures.R use 'grp' for the random-effects
# grouping variable to avoid that collision. The orphan ar1_t2_noint
# fixture (built outside build_fixtures.R) still carries 'group' and
# the affected test below strips it before dispatching.
#
# Concordance bar: predictions(mvgam_fit) and predictions(brms_fit)
# at observed time points should agree on the deterministic submodel
# (obs + trend covariates, no AR contribution) within a tolerance
# that captures the state-space / residual-AR architectural gap.

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
  testthat::expect_true("series" %in% preds$conditional)
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


# -- brms-concordance on the deterministic submodel --------------------

test_that("predictions(mvgam) tracks predictions(brms) on Beta AR(1)", {
  require_fixtures("val_mvgam_beta_ar1.rds", "val_brms_beta_ar1.rds")
  mv <- load_mvgam("beta_ar1")
  bf <- load_brms("beta_ar1")
  # Restrict comparison to a fresh datagrid so brms's AR-residual
  # draws don't dominate.
  nd <- mv$data
  mv_p  <- suppressWarnings(predictions(mv, newdata = nd))
  bf_p  <- suppressWarnings(predictions(bf, newdata = nd))
  # Both estimates are on the response scale and bounded (0, 1)
  testthat::expect_true(all(mv_p$estimate > 0 & mv_p$estimate < 1))
  testthat::expect_true(all(bf_p$estimate > 0 & bf_p$estimate < 1))
  # Loose cor bound: state-space vs residual-AR diverge per-obs but
  # share the X * beta deterministic signal.
  comp <- compare_vectors(bf_p$estimate, mv_p$estimate)
  testthat::expect_gte(comp$cor, 0.40)
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
  # data.frame and stores it on an internal slot — use
  # marginaleffects::posterior_draws() to retrieve it.
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
# Tier-3 brms-parity batch: vcov, loo_R2, loo_predict, loo_model_weights
# ====================================================================
# Numerical sanity vs brms on a shared fixed-effects fixture. These
# tests live in `local/` because they instantiate full MCMC fits via
# the fixture pair and run on the full posterior — too heavy for CI.


test_that("vcov(mvgam) shares fixed effects with vcov(brms)", {
  require_fixtures("val_mvgam_ar1_fx.rds", "val_brms_ar1_fx.rds")
  mv <- load_mvgam("ar1_fx")
  bm <- load_brms("ar1_fx")
  vm <- vcov(mv)
  vb <- vcov(bm)
  # mvgam now aliases b[k] to b_<term>; both should carry x + Intercept.
  expect_setequal(colnames(vm), c("Intercept", "x"))
  expect_setequal(colnames(vb), c("Intercept", "x"))
  # Symmetric and PSD (variances on diag > 0).
  expect_equal(vm, t(vm), tolerance = 1e-12)
  expect_true(all(diag(vm) > 0))
  # correlation = TRUE produces unit diagonal.
  cm <- vcov(mv, correlation = TRUE)
  expect_equal(unname(diag(cm)), c(1, 1), tolerance = 1e-12)
})


test_that("loo_R2(mvgam) returns finite R^2 with overlapping CI vs brms", {
  require_fixtures("val_mvgam_ar1_fx.rds", "val_brms_ar1_fx.rds")
  mv <- load_mvgam("ar1_fx")
  bm <- load_brms("ar1_fx")
  set.seed(1L)
  r2_mv <- suppressWarnings(loo_R2(mv))
  set.seed(1L)
  r2_bm <- suppressWarnings(loo_R2(bm))
  # brms-shape summary: Estimate, Est.Error, Q2.5, Q97.5.
  expect_setequal(colnames(r2_mv),
                   c("Estimate", "Est.Error", "Q2.5", "Q97.5"))
  # Estimate finite + in (-1, 1] (clamped per Gelman 2019).
  expect_true(is.finite(r2_mv[, "Estimate"]))
  expect_lte(r2_mv[, "Estimate"], 1)
  # Credible intervals overlap (different latent structures give
  # different point estimates but the intervals should agree).
  expect_true(
    r2_mv[, "Q2.5"] <= r2_bm[, "Q97.5"] &&
      r2_bm[, "Q2.5"] <= r2_mv[, "Q97.5"]
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
# Tier-4 brms-parity batch: update.mvgam
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
# Tier-5 brms-parity batch: ranef + VarCorr
# ---------------------------------------------------------------------
#
# Concordance against brms's own random-effect accessors on the three
# RE fixtures (intercept-only, intercept+smooth, correlated slope).
# Numerical tolerance reflects mvgam's state-space architecture: the
# AR(1) trend lives in the linear predictor rather than as residual
# autocorrelation, so per-level posterior medians shift relative to
# brms. Shape parity is exact; per-level Estimate / SD comparisons are
# loose enough to absorb the architectural difference but tight
# enough to catch a structural bug.


re_concordance_threshold_sd <- 0.5
re_concordance_threshold_estimate <- 1.0


test_that("ranef(mvgam) matches brms shape on val_mvgam_ar1_re", {
  require_fixtures("val_mvgam_ar1_re.rds", "val_brms_ar1_re.rds")
  mv <- load_mvgam("ar1_re")
  br <- load_brms("ar1_re")
  mv_re <- ranef(mv)
  br_re <- brms::ranef(br)
  expect_named(mv_re, names(br_re))
  expect_identical(dim(mv_re$grp), dim(br_re$grp))
  expect_identical(dimnames(mv_re$grp), dimnames(br_re$grp))
  expect_lt(
    max(abs(mv_re$grp[, "Estimate", "Intercept"] -
              br_re$grp[, "Estimate", "Intercept"])),
    re_concordance_threshold_estimate
  )
})


test_that("ranef(mvgam) matches brms shape on val_mvgam_ar1_re_smooth", {
  require_fixtures(
    "val_mvgam_ar1_re_smooth.rds", "val_brms_ar1_re_smooth.rds"
  )
  mv <- load_mvgam("ar1_re_smooth")
  br <- load_brms("ar1_re_smooth")
  mv_re <- ranef(mv)
  br_re <- brms::ranef(br)
  expect_named(mv_re, names(br_re))
  expect_identical(dim(mv_re$grp), dim(br_re$grp))
  expect_identical(dimnames(mv_re$grp), dimnames(br_re$grp))
})


test_that("ranef(mvgam) matches brms shape on val_mvgam_ar1_cor_re", {
  require_fixtures(
    "val_mvgam_ar1_cor_re.rds", "val_brms_ar1_cor_re.rds"
  )
  mv <- load_mvgam("ar1_cor_re")
  br <- load_brms("ar1_cor_re")
  mv_re <- ranef(mv)
  br_re <- brms::ranef(br)
  expect_named(mv_re, names(br_re))
  expect_identical(dim(mv_re$grp), dim(br_re$grp))
  expect_identical(dimnames(mv_re$grp), dimnames(br_re$grp))
  # Correlated slope: both Intercept and x dimensions should be
  # populated with sensible numerical agreement.
  expect_lt(
    max(abs(mv_re$grp[, "Estimate", "Intercept"] -
              br_re$grp[, "Estimate", "Intercept"])),
    re_concordance_threshold_estimate
  )
  expect_lt(
    max(abs(mv_re$grp[, "Estimate", "x"] -
              br_re$grp[, "Estimate", "x"])),
    re_concordance_threshold_estimate
  )
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


test_that("VarCorr(mvgam) matches brms shape on val_mvgam_ar1_cor_re", {
  require_fixtures(
    "val_mvgam_ar1_cor_re.rds", "val_brms_ar1_cor_re.rds"
  )
  mv <- load_mvgam("ar1_cor_re")
  br <- load_brms("ar1_cor_re")
  mv_vc <- VarCorr(mv)
  br_vc <- brms::VarCorr(br)
  expect_named(mv_vc, names(br_vc))
  expect_named(mv_vc$grp, names(br_vc$grp))
  for (slot in c("sd", "cor", "cov")) {
    expect_identical(dim(mv_vc$grp[[slot]]), dim(br_vc$grp[[slot]]))
    expect_identical(
      dimnames(mv_vc$grp[[slot]]), dimnames(br_vc$grp[[slot]])
    )
  }
  # SD estimates per coef should agree within a state-space
  # tolerance — the AR(1) absorbs some shrinkage, but the
  # group-level SD is largely structural and should not diverge
  # dramatically.
  expect_lt(
    max(abs(mv_vc$grp$sd[, "Estimate"] - br_vc$grp$sd[, "Estimate"])),
    re_concordance_threshold_sd
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
# Tier-6 brms-parity batch: posterior_smooths + conditional_smooths
# ---------------------------------------------------------------------
#
# Shape concordance against brms's own smooth methods on the
# ar1_re_smooth fixture (single 1D smooth) and ar1_re_smooth_trend
# (trend-side smooth). The numerical tolerance reflects mvgam's
# state-space architecture (AR(1) trend in the linear predictor vs
# brms residual-AR) but is still tight enough to catch a structural
# bug in the basis-matrix / coefficient pairing.


smooths_concordance_threshold <- 0.5


test_that("smooths(mvgam) enumerates the same s() terms as brms", {
  require_fixtures(
    "val_mvgam_ar1_re_smooth.rds", "val_brms_ar1_re_smooth.rds"
  )
  mv <- load_mvgam("ar1_re_smooth")
  br <- load_brms("ar1_re_smooth")
  mv_terms <- smooths(mv)
  br_terms <- attr(
    terms(brms::brmsterms(br$formula)$dpars$mu$sm),
    "term.labels"
  )
  expect_identical(mv_terms, br_terms)
})


test_that("posterior_smooths(mvgam) matches brms shape on ar1_re_smooth", {
  require_fixtures(
    "val_mvgam_ar1_re_smooth.rds", "val_brms_ar1_re_smooth.rds"
  )
  mv <- load_mvgam("ar1_re_smooth")
  br <- load_brms("ar1_re_smooth")
  mv_eta <- posterior_smooths(mv, smooth = "s(z)")
  br_eta <- brms::posterior_smooths(br, smooth = "s(z)")
  expect_identical(dim(mv_eta), dim(br_eta))
  # Per-grid-point posterior median should track brms within the
  # state-space tolerance — this catches a structural index or
  # basis-matrix mismatch even when the AR architectural gap
  # shifts the absolute values.
  expect_lt(
    max(abs(apply(mv_eta, 2, median) - apply(br_eta, 2, median))),
    smooths_concordance_threshold
  )
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


test_that("conditional_smooths(mvgam) returns brms_conditional_effects shape", {
  require_fixtures(
    "val_mvgam_ar1_re_smooth.rds", "val_brms_ar1_re_smooth.rds"
  )
  mv <- load_mvgam("ar1_re_smooth")
  br <- load_brms("ar1_re_smooth")
  mv_cs <- conditional_smooths(mv)
  br_cs <- brms::conditional_smooths(br)
  expect_s3_class(mv_cs, "brms_conditional_effects")
  expect_identical(length(mv_cs), length(br_cs))
  expect_identical(
    sort(colnames(mv_cs[[1L]])), sort(colnames(br_cs[[1L]]))
  )
  expect_identical(nrow(mv_cs[[1L]]), nrow(br_cs[[1L]]))
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


test_that("posterior_smooths(mvgam) handles s(z, by = grp) factor expansion", {
  require_fixtures(
    "val_mvgam_ar1_s_by.rds", "val_brms_ar1_s_by.rds"
  )
  mv <- load_mvgam("ar1_s_by")
  br <- load_brms("ar1_s_by")
  # By-factor expansion: brms enumerates one user-facing smooth
  # but emits per-level basis blocks; the eta matrix concatenates
  # all by-levels into the grid.
  mv_eta <- posterior_smooths(mv, smooth = "s(z, by = grp)")
  br_eta <- brms::posterior_smooths(br, smooth = "s(z, by = grp)")
  expect_identical(dim(mv_eta), dim(br_eta))
})


test_that("conditional_smooths(mvgam) facets by-factor on real fit", {
  require_fixtures("val_mvgam_ar1_s_by.rds")
  mv <- load_mvgam("ar1_s_by")
  cs <- conditional_smooths(mv)
  expect_s3_class(cs, "brms_conditional_effects")
  expect_true(length(cs) >= 1L)
  # By-factor smooth: cond__ column should carry per-level facet
  # labels.
  expect_true(length(unique(cs[[1L]]$cond__)) >= 3L ||
    !is.null(attr(cs[[1L]], "effects")))
})


test_that("posterior_smooths(mvgam) handles 2D t2(z, w) tensor smooth", {
  require_fixtures("val_mvgam_ar1_t2.rds", "val_brms_ar1_t2.rds")
  mv <- load_mvgam("ar1_t2")
  br <- load_brms("ar1_t2")
  mv_eta <- posterior_smooths(mv, smooth = "t2(z, w)")
  br_eta <- brms::posterior_smooths(br, smooth = "t2(z, w)")
  expect_identical(dim(mv_eta), dim(br_eta))
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
# Tier-7 brms-parity batch: posterior_interval + predictive_interval
# + ngrps + predictive_error + deprecated aliases
# ---------------------------------------------------------------------


test_that("posterior_interval(mvgam) matches brms shape on a real fit", {
  require_fixtures(
    "val_mvgam_ar1_re_smooth.rds", "val_brms_ar1_re_smooth.rds"
  )
  mv <- load_mvgam("ar1_re_smooth")
  br <- load_brms("ar1_re_smooth")
  mv_pi <- posterior_interval(mv)
  br_pi <- posterior_interval(br)
  expect_identical(ncol(mv_pi), ncol(br_pi))
  expect_identical(colnames(mv_pi), colnames(br_pi))
})


test_that("predictive_interval(mvgam) returns [nobs x 2] with brms cols", {
  require_fixtures("val_mvgam_ar1_re_smooth.rds")
  mv <- load_mvgam("ar1_re_smooth")
  pi <- predictive_interval(mv)
  expect_identical(ncol(pi), 2L)
  expect_identical(nrow(pi), nrow(mv$data))
  expect_identical(colnames(pi), c("5%", "95%"))
})


test_that("ngrps(mvgam) matches brms on a real RE fit", {
  require_fixtures(
    "val_mvgam_ar1_re_smooth.rds", "val_brms_ar1_re_smooth.rds"
  )
  mv <- load_mvgam("ar1_re_smooth")
  br <- load_brms("ar1_re_smooth")
  expect_identical(ngrps(mv), ngrps(br))
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
