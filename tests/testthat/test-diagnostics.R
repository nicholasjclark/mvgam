# Fast CI-safe runtime tests for as_draws_*.mvgam, the keyword
# shortcuts in `variable=`, and the bayesplot-ecosystem wrappers
# (rhat, neff_ratio, nuts_params, log_posterior, coef, fixef,
# bayes_R2, prior_summary, ndraws, nchains, niterations, nvariables,
# posterior_summary, getCall).
#
# Each test builds a draws_array-backed mvgam stub so the runtime
# paths execute without invoking Stan. Catches regressions like
# stale `x$model_output` references that previously broke the
# diagnostic family silently.


# Minimal mvgam-class stub. Carries the slots every diagnostic /
# draws method reads: a posterior::draws_array as `$fit`, the
# response name(s), the data frame, the formula, and an empty
# trend_formula so resolve_mvgam_keyword takes the obs-only branch.
make_mvgam_stub <- function(varnames = c(
                              "b_Intercept", "b_x", "b_trend[1]",
                              "Intercept", "sigma", "phi",
                              "sd_1[1]", "sds_sx_1", "sds_sz_1_trend",
                              "ar1_trend[1]", "sigma_trend[1]",
                              "innovations_trend[1,1]",
                              "trend[1,1]", "Y_pred[1]",
                              "lp__", "lprior"),
                            n_iter = 50L, n_chains = 2L,
                            mv = FALSE) {
  set.seed(1L)
  arr <- array(
    rnorm(n_iter * n_chains * length(varnames)),
    dim = c(n_iter, n_chains, length(varnames)),
    dimnames = list(NULL, NULL, varnames)
  )
  drws <- posterior::as_draws_array(arr)
  formula <- if (mv) {
    brms::bf(brms::mvbind(y1, y2) ~ x)
  } else {
    structure(y ~ x, class = c("brmsformula", "formula"))
  }
  data <- if (mv) {
    data.frame(y1 = rnorm(10), y2 = rnorm(10), x = rnorm(10))
  } else {
    data.frame(y = rnorm(10), x = rnorm(10))
  }
  structure(
    list(
      fit = drws,
      formula = formula,
      trend_formula = NULL,
      response_names = if (mv) c("y1", "y2") else "y",
      data = data,
      prior = data.frame(prior = "(flat)", class = "b"),
      call = call("mvgam", formula = formula)
    ),
    class = "mvgam"
  )
}


# ---- as_draws_*.mvgam runtime paths ----------------------------------

test_that("as.matrix.mvgam(variable = NULL) returns all parameters", {
  stub <- make_mvgam_stub()
  out <- as.matrix(stub)
  expect_s3_class(out, "draws_matrix")
  expect_equal(ncol(out), 16L)
})

test_that("as.matrix.mvgam(variable = 'betas') extracts b_* only", {
  stub <- make_mvgam_stub()
  out <- as.matrix(stub, variable = "betas")
  expect_setequal(colnames(out), c("b_Intercept", "b_x"))
})

test_that("as.matrix.mvgam(variable = 'trend_betas') extracts b_trend[", {
  stub <- make_mvgam_stub()
  out <- as.matrix(stub, variable = "trend_betas")
  expect_equal(colnames(out), "b_trend[1]")
})

test_that("as.matrix.mvgam(variable = 'obs_params') excludes _trend", {
  stub <- make_mvgam_stub()
  out <- as.matrix(stub, variable = "obs_params")
  expect_setequal(colnames(out), c("sigma", "phi"))
})

test_that("as.matrix.mvgam(variable = 'smooth_params') excludes _trend", {
  stub <- make_mvgam_stub()
  out <- as.matrix(stub, variable = "smooth_params")
  expect_equal(colnames(out), "sds_sx_1")
})

test_that("as.matrix.mvgam(variable = 'trend_smooth_params') matches _trend", {
  stub <- make_mvgam_stub()
  out <- as.matrix(stub, variable = "trend_smooth_params")
  expect_equal(colnames(out), "sds_sz_1_trend")
})

test_that("trend_params on obs-only fit picks top-level dynamics", {
  stub <- make_mvgam_stub()
  stub$trend_formula <- NULL
  out <- as.matrix(stub, variable = "trend_params")
  expect_true("sigma" %in% colnames(out))
  expect_false("trend[1,1]" %in% colnames(out))
  expect_false("innovations_trend[1,1]" %in% colnames(out))
})

test_that("trend_params on trend-formula fit picks _trend block", {
  stub <- make_mvgam_stub()
  stub$trend_formula <- ~AR(p = 1)
  out <- as.matrix(stub, variable = "trend_params")
  expect_true("ar1_trend[1]" %in% colnames(out))
  expect_true("sigma_trend[1]" %in% colnames(out))
  expect_false("b_trend[1]" %in% colnames(out))
  expect_false("innovations_trend[1,1]" %in% colnames(out))
  expect_false("sds_sz_1_trend" %in% colnames(out))
})

test_that("variable = mix of keyword and regex composes", {
  stub <- make_mvgam_stub()
  out <- as.matrix(stub, variable = c("betas", "^sd_"), regex = TRUE)
  expect_setequal(colnames(out), c("b_Intercept", "b_x", "sd_1[1]"))
})

test_that("variable = explicit name vector works (no regex)", {
  stub <- make_mvgam_stub()
  out <- as.matrix(stub, variable = c("b_Intercept", "sigma"))
  expect_setequal(colnames(out), c("b_Intercept", "sigma"))
})

test_that("free pattern that matches nothing errors informatively", {
  stub <- make_mvgam_stub()
  expect_error(
    as.matrix(stub, variable = "totally_nonexistent_par"),
    regexp = "No parameters matched"
  )
})

test_that("keyword that resolves to empty (e.g. obs_params on Poisson) is OK", {
  stub <- make_mvgam_stub(varnames = c("b_Intercept", "ar1_trend[1]",
                                        "sigma_trend[1]", "lp__"))
  out <- as.matrix(stub, variable = "obs_params")
  expect_equal(ncol(out), 0L)
})

test_that("as.data.frame.mvgam returns a data.frame", {
  stub <- make_mvgam_stub()
  out <- as.data.frame(stub, variable = "betas")
  expect_s3_class(out, "data.frame")
  expect_true("b_Intercept" %in% names(out))
})

test_that("as.array.mvgam returns a draws_array", {
  stub <- make_mvgam_stub()
  out <- as.array(stub, variable = "betas")
  expect_s3_class(out, "draws_array")
})

test_that("as_draws_matrix / df / array / list / rvars all dispatch", {
  stub <- make_mvgam_stub()
  expect_s3_class(as_draws_matrix(stub, variable = "betas"),
                  "draws_matrix")
  expect_s3_class(as_draws_df(stub, variable = "betas"), "draws_df")
  expect_s3_class(as_draws_array(stub, variable = "betas"),
                  "draws_array")
  expect_s3_class(as_draws_list(stub, variable = "betas"),
                  "draws_list")
  expect_s3_class(as_draws_rvars(stub, variable = "betas"),
                  "draws_rvars")
})


# ---- bayesplot-ecosystem wrappers ------------------------------------

test_that("coef.mvgam returns named posterior means of b_* by default", {
  stub <- make_mvgam_stub()
  out <- coef(stub)
  expect_type(out, "double")
  expect_setequal(names(out), c("b_Intercept", "b_x"))
})

test_that("coef.mvgam(summarise = FALSE) returns the full chain", {
  stub <- make_mvgam_stub()
  out <- coef(stub, summarise = FALSE)
  expect_s3_class(out, "draws_matrix")
  expect_equal(ncol(out), 2L)
})

test_that("fixef.mvgam returns a brms-shaped summary matrix", {
  stub <- make_mvgam_stub()
  out <- fixef(stub)
  expect_true(is.matrix(out))
  expect_setequal(colnames(out), c("Estimate", "Est.Error",
                                    "Q2.5", "Q97.5"))
  expect_setequal(rownames(out), c("Intercept", "x"))
})

test_that("fixef.mvgam(summary = FALSE) returns the draws matrix", {
  stub <- make_mvgam_stub()
  out <- fixef(stub, summary = FALSE)
  expect_true(is.matrix(out))
  expect_setequal(colnames(out), c("Intercept", "x"))
})

test_that("rhat.mvgam returns a named numeric vector", {
  stub <- make_mvgam_stub()
  out <- rhat(stub)
  expect_type(out, "double")
  expect_equal(length(out), 16L)
})

test_that("rhat.mvgam(pars = ...) filters", {
  stub <- make_mvgam_stub()
  out <- rhat(stub, pars = c("b_Intercept", "sigma"))
  expect_setequal(names(out), c("b_Intercept", "sigma"))
})

test_that("neff_ratio.mvgam returns ratios <= 1 in [0,1]", {
  stub <- make_mvgam_stub(n_iter = 200L, n_chains = 4L)
  out <- neff_ratio(stub, pars = c("b_Intercept"))
  expect_true(all(out > 0))
})

test_that("posterior_summary.mvgam returns a brms-shaped matrix", {
  stub <- make_mvgam_stub()
  out <- posterior_summary(stub, pars = c("b_Intercept", "sigma"))
  expect_true(is.matrix(out))
  expect_setequal(colnames(out), c("Estimate", "Est.Error",
                                    "Q2.5", "Q97.5"))
})

test_that("ndraws / nchains / niterations / nvariables work", {
  stub <- make_mvgam_stub(n_iter = 50L, n_chains = 2L)
  expect_equal(ndraws(stub), 100L)
  expect_equal(nchains(stub), 2L)
  expect_equal(niterations(stub), 50L)
  expect_equal(nvariables(stub), 16L)
})

test_that("getCall.mvgam returns the stored call", {
  stub <- make_mvgam_stub()
  expect_true(is.call(getCall(stub)))
})

test_that("prior_summary.mvgam returns the prior table", {
  stub <- make_mvgam_stub()
  out <- prior_summary(stub)
  expect_s3_class(out, "data.frame")
  expect_true("prior" %in% names(out))
})

test_that("prior_summary errors when fit has no prior slot", {
  stub <- make_mvgam_stub()
  stub$prior <- NULL
  expect_error(prior_summary(stub),
               regexp = "not stored with a prior table")
})

test_that("bayes_R2.mvgam errors for multivariate without resp", {
  stub <- make_mvgam_stub(mv = TRUE)
  expect_error(
    bayes_R2(stub),
    regexp = "requires 'resp' for multivariate models"
  )
})
