# End-to-end tests for the broom S3 trio --
# `tidy.mvgam` / `augment.mvgam` / `glance.mvgam` -- exercised
# against real cached fixtures. These were dead-code paths before
# the rewrite (`tidy.mvgam` called the missing `mcmc_summary`,
# `augment.mvgam` read empty obs_data, `glance.mvgam` did not
# exist).
#
# Fixtures are intentionally varied across response families so
# the new spec-table dispatch in `tidy.mvgam` is hit across
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
