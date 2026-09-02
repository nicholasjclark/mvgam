# End-to-end tests for the broom S3 trio --
# `tidy.mvgam` / `augment.mvgam` / `glance.mvgam` -- exercised
# against real cached fixtures, since each reads fields (MCMC
# summaries, obs_data) that only exist on an actual fit.
#
# Fixtures are intentionally varied across response families so
# the spec-table dispatch in `tidy.mvgam` is hit across
# `observation_family_extra_param` (e.g. gaussian sigma),
# `observation_beta` (intercepts + slopes), `trend_model_param`
# (AR coefficients, sigma_trend), `trend_beta` (trend formula
# fixed effects), and the random-effect blocks.

source("setup_tests_local.R")

CACHE_DIR <- "fixtures"


load_fixture <- function(name) {
  readRDS(file.path(CACHE_DIR, paste0(name, ".rds")))
}


# ---- tidy.mvgam: broom column conventions ------------------------

test_that("tidy.mvgam returns broom-standard columns", {
  fit <- load_fixture("val_mvgam_ar1_int")
  t <- tidy(fit)
  expect_s3_class(t, "tbl_df")
  expect_true(all(c("term", "type", "estimate", "std.error",
                     "conf.low", "conf.high") %in% colnames(t)))
  expect_true(all(is.finite(t$estimate)))
  expect_true(all(is.finite(t$std.error)))
  expect_true(all(t$std.error >= 0))
  expect_true(all(t$conf.low <= t$estimate))
  expect_true(all(t$conf.high >= t$estimate))
})


test_that("tidy.mvgam(conf.int = FALSE) drops CI cols", {
  fit <- load_fixture("val_mvgam_ar1_int")
  t <- tidy(fit, conf.int = FALSE)
  expect_false("conf.low" %in% colnames(t))
  expect_false("conf.high" %in% colnames(t))
})


test_that("tidy.mvgam(rhat = TRUE, ess = TRUE) adds diagnostic cols", {
  fit <- load_fixture("val_mvgam_ar1_int")
  t <- tidy(fit, rhat = TRUE, ess = TRUE)
  expect_true("rhat" %in% colnames(t))
  expect_true("ess_bulk" %in% colnames(t))
  # ESS should always be positive
  expect_true(all(t$ess_bulk > 0))
})


test_that("tidy.mvgam(robust = TRUE) uses median/mad", {
  fit <- load_fixture("val_mvgam_ar1_int")
  t_mean <- tidy(fit, robust = FALSE)
  t_med  <- tidy(fit, robust = TRUE)
  # mean vs median should differ for at least one parameter
  expect_false(isTRUE(all.equal(t_mean$estimate, t_med$estimate)))
})


test_that("tidy.mvgam(conf.level = 0.5) tightens the CI", {
  fit <- load_fixture("val_mvgam_ar1_int")
  t_wide   <- tidy(fit, conf.level = 0.95)
  t_narrow <- tidy(fit, conf.level = 0.5)
  # Narrower level → narrower interval
  expect_true(all(
    (t_narrow$conf.high - t_narrow$conf.low) <=
      (t_wide$conf.high - t_wide$conf.low) + 1e-8
  ))
})


# ---- tidy.mvgam: effects = filter --------------------------------

test_that("tidy.mvgam(effects = 'fixed') drops trend/family params", {
  fit <- load_fixture("val_mvgam_ar1_fx")
  t_all   <- tidy(fit, effects = "all")
  t_fixed <- tidy(fit, effects = "fixed")
  expect_true(nrow(t_fixed) <= nrow(t_all))
  # All rows should be `observation_beta` or `trend_beta`
  expect_true(all(t_fixed$type %in%
                    c("observation_beta", "trend_beta")))
})


# ---- tidy.mvgam: trend_dynamic dispatch covers AR/VAR/PW --------

test_that("tidy.mvgam picks up trend dynamics for AR / VAR / PW", {
  ar_fit  <- load_fixture("val_mvgam_ar1_int")
  var_fit <- load_fixture("val_mvgam_var_cor")
  ar_t  <- tidy(ar_fit)
  var_t <- tidy(var_fit)
  expect_true(any(grepl("ar1_trend|sigma_trend",
                          ar_t$term[ar_t$type == "trend_model_param"])))
  expect_true(any(grepl("^A\\[|alpha_cor|Sigma",
                          var_t$term[var_t$type == "trend_model_param"])))
})


# ---- tidy.mvgam: smoothpars + effects = ran_pars / ran_vals -----

test_that("tidy.mvgam surfaces obs Intercept + smooth coefs + penalty", {
  # Use an obs-side smooth fit; this fixture has `y ~ s(x)` plus
  # an AR(1) trend, so we expect:
  #   * `Intercept` / `b_Intercept` rows as `observation_beta`,
  #   * `sds_*` rows as `observation_smooth_param` (ran_pars),
  #   * `zs_*` / `s_*` rows as `observation_smooth_coef` (ran_vals).
  # `head_betas()` must not silently drop the smooth block, leaving
  # `tidy()` to return only the 2-row trend_model_param block.
  fit <- load_fixture("val_mvgam_ar1_re_smooth")
  td <- tidy(fit)
  expect_true("observation_beta" %in% td$type)
  expect_true("observation_smooth_param" %in% td$type)
  expect_true("observation_smooth_coef" %in% td$type)
  # Smooth penalties (`sds_*`) are positive
  sds_rows <- td[td$type == "observation_smooth_param", ]
  expect_true(all(sds_rows$estimate > 0))
  # Per-basis values should include both `s_*` and `zs_*` aliases
  coef_terms <- td$term[td$type == "observation_smooth_coef"]
  expect_true(any(grepl("^s_", coef_terms)))
  expect_true(any(grepl("^zs_", coef_terms)))
})


test_that("tidy.mvgam surfaces trend-side zs_*_trend smooth coefs", {
  # The `trend_smoothpars` categorize pattern must match
  # `zs_*_trend` / `sdgp_*_trend` / `lscale_*_trend` / `zgp_*_trend`
  # in addition to `^(sds_.*_trend|s_.*_trend)`. Anything it misses
  # leaks into `trend_pars` and is filtered out of tidy() entirely
  # for not matching the trend-dynamic regex.
  fit <- load_fixture("val_mvgam_ar1_re_smooth_trend")
  td <- tidy(fit)
  zs_trend <- td[grepl("^zs_.*_trend", td$term), ]
  expect_gt(nrow(zs_trend), 0L)
  expect_true(all(zs_trend$type == "trend_smooth_coef"))
})


test_that("tidy.mvgam(effects = ran_pars) returns only variance components", {
  fit <- load_fixture("val_mvgam_ar1_re_smooth")
  rp <- tidy(fit, effects = "ran_pars")
  expect_gt(nrow(rp), 0L)
  expect_true(all(rp$type %in% c(
    "observation_family_extra_param",
    "observation_smooth_param",
    "random_effect_group_level",
    "trend_model_param",
    "trend_smooth_param",
    "trend_random_effect_group_level"
  )))
  # `s_*` per-basis coefficients are ran_vals; they must NOT
  # appear in the ran_pars filter.
  expect_false(any(grepl("^s_[0-9]", rp$term)))
})


test_that("tidy.mvgam(effects = ran_vals) returns only level deviations", {
  fit <- load_fixture("val_mvgam_ar1_re_smooth")
  rv <- tidy(fit, effects = "ran_vals")
  expect_gt(nrow(rv), 0L)
  expect_true(all(rv$type %in% c(
    "random_effect_beta",
    "observation_smooth_coef",
    "trend_random_effect_beta",
    "trend_smooth_coef"
  )))
})


# ---- categorize_mvgam_parameters: bucket coverage ----------------

test_that("categorize_mvgam_parameters buckets every drawn parameter", {
  # Architectural guard for the lazy-categorization invariant:
  # every parameter the user can see via `variables(mod)` should
  # land in exactly one of the eight `obj_vars` buckets (plus
  # `trends` for the state arrays). Anything that drops on the
  # floor is a categorize regex gap.
  fit <- load_fixture("val_mvgam_ar1_re_smooth_trend")
  obj_vars <- mvgam:::categorize_mvgam_parameters(fit)
  bucketed <- unique(c(
    obj_vars$observation_pars$orig_name,
    obj_vars$observation_betas$orig_name,
    obj_vars$observation_smoothpars$orig_name,
    obj_vars$observation_re_params$orig_name,
    obj_vars$trend_pars$orig_name,
    obj_vars$trend_betas$orig_name,
    obj_vars$trend_smoothpars$orig_name,
    obj_vars$trend_re_params$orig_name,
    obj_vars$trends$orig_name
  ))
  all_pars <- variables(posterior::as_draws(fit$fit))
  all_pars <- setdiff(all_pars, fit$exclude %||% character(0L))
  # `b_Intercept_trend` is the brms uncentred generated quantity
  # and is excluded by design (see categorize() comment).
  all_pars <- setdiff(all_pars, "b_Intercept_trend")
  uncategorized <- setdiff(all_pars, bucketed)
  expect_equal(
    length(uncategorized), 0L,
    label = paste0(
      "uncategorized parameters: ",
      paste(uncategorized, collapse = ", ")
    )
  )
})


# ---- pairs.mvgam: default variable selection ---------------------

test_that("pairs.mvgam default selection covers canonical params", {
  # The default returns a list of regex patterns (brms-style) that
  # are matched against `variables(fit)` inside bayesplot. Each
  # fixture should pick up the right scalar hyperparameters: the
  # parametric coefficients, family extras, trend dynamics, and
  # smoothness penalties / RE variance components.
  patterns_match_any <- function(patts, vars) {
    any(vapply(patts, function(p) any(grepl(p, vars)), logical(1)))
  }

  ar_fit <- load_fixture("val_mvgam_ar1_re_smooth_trend")
  patts <- mvgam:::default_pairs_variables(ar_fit)
  vars <- variables(ar_fit)
  matched <- unique(unlist(lapply(
    patts, function(p) grep(p, vars, value = TRUE)
  )))
  expect_gt(length(matched), 3L)
  # Trend-side dynamics, intercepts and variance components are in.
  expect_true(any(grepl("^sigma_trend", matched)))
  expect_true(any(grepl("^ar1_trend", matched)))
  expect_true(any(grepl("^b_Intercept", matched)))
  expect_true(any(grepl("^sds_.*_trend", matched)))
  expect_true(any(grepl("^sd_.*_trend", matched)))
  # Per-basis smooth coefficients and per-level RE draws stay out.
  expect_false(any(grepl("^(s_|zs_|zgp_|r_|z_)", matched)))

  # Gaussian fit: family-specific dpar `sigma` must be picked up
  # via the family-aware dpars block.
  gauss_fit <- load_fixture("val_mvgam_gauss_ar1_n150")
  vars_g <- variables(gauss_fit)
  patts_g <- mvgam:::default_pairs_variables(gauss_fit)
  matched_g <- unique(unlist(lapply(
    patts_g, function(p) grep(p, vars_g, value = TRUE)
  )))
  expect_true("sigma" %in% matched_g)

  # Hurdle negbinomial: two dpars (`shape`, `hu`) both surface.
  hurdle_fit <- load_fixture("val_mvgam_hurdle_negbinomial_ar1")
  vars_h <- variables(hurdle_fit)
  patts_h <- mvgam:::default_pairs_variables(hurdle_fit)
  matched_h <- unique(unlist(lapply(
    patts_h, function(p) grep(p, vars_h, value = TRUE)
  )))
  expect_true("shape" %in% matched_h)
  expect_true("hu" %in% matched_h)
})


test_that("pairs.mvgam runs end-to-end on a smooth + RE fit", {
  fit <- load_fixture("val_mvgam_ar1_re_smooth_trend")
  res <- suppressWarnings(pairs(fit))
  expect_s3_class(res, "bayesplot_grid")
})


# ---- augment.mvgam: broom-standard columns -----------------------

test_that("augment.mvgam returns broom-standard fit / resid cols", {
  fit <- load_fixture("val_mvgam_ar1_int")
  a <- augment(fit)
  expect_s3_class(a, "tbl_df")
  expect_true(all(c(".observed", ".fitted", ".se.fit",
                     ".lower", ".upper",
                     ".resid", ".resid.se",
                     ".resid.lower", ".resid.upper") %in%
                    colnames(a)))
  # One row per training observation
  expect_identical(nrow(a), nrow(as.data.frame(fit$data)))
  expect_true(all(a$.se.fit >= 0))
})


test_that("augment.mvgam(conf.int = FALSE) drops interval cols", {
  fit <- load_fixture("val_mvgam_ar1_int")
  a <- augment(fit, conf.int = FALSE)
  expect_false(".lower" %in% colnames(a))
  expect_false(".upper" %in% colnames(a))
  expect_false(".resid.lower" %in% colnames(a))
  expect_false(".resid.upper" %in% colnames(a))
  expect_true(".fitted" %in% colnames(a))
  expect_true(".resid" %in% colnames(a))
})


# ---- glance.mvgam: 1-row tibble ---------------------------------

test_that("glance.mvgam returns a one-row tibble with expected cols", {
  fit <- load_fixture("val_mvgam_ar1_int")
  g <- glance(fit)
  expect_s3_class(g, "tbl_df")
  expect_identical(nrow(g), 1L)
  expect_true(all(c("algorithm", "pss", "nobs", "nseries",
                     "family", "link") %in% colnames(g)))
  expect_true(g$pss >= 1L)
  expect_true(g$nobs >= 1L)
  expect_true(g$nseries >= 1L)
  expect_identical(g$family, "poisson")
  expect_identical(g$link, "log")
})


test_that("glance.mvgam(looic = TRUE) adds elpd_loo / looic", {
  fit <- load_fixture("val_mvgam_ar1_int")
  g <- glance(fit, looic = TRUE)
  expect_true(all(c("elpd_loo", "se_elpd_loo",
                     "p_loo", "looic") %in% colnames(g)))
  expect_true(is.finite(g$elpd_loo))
  expect_true(is.finite(g$looic))
})
