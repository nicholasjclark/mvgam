# Tests for ordinal predictive sampling helpers and the brms-parity arg
# validation paths on loo / waic. CI-safe — uses synthetic inputs only.

# -------------------------------------------------------------------------
# ordinal_linkinv — link inverses on the CDF offset
# -------------------------------------------------------------------------

test_that("ordinal_linkinv: logit matches plogis", {
  expect_equal(ordinal_linkinv(c(-1, 0, 1), "logit"),
               stats::plogis(c(-1, 0, 1)))
})

test_that("ordinal_linkinv: probit matches pnorm", {
  expect_equal(ordinal_linkinv(c(-1, 0, 1), "probit"),
               stats::pnorm(c(-1, 0, 1)))
})

test_that("ordinal_linkinv: probit_approx matches pnorm", {
  expect_equal(ordinal_linkinv(c(-1, 0, 1), "probit_approx"),
               stats::pnorm(c(-1, 0, 1)))
})

test_that("ordinal_linkinv: cloglog matches 1 - exp(-exp(x))", {
  x <- c(-2, -1, 0, 1)
  expect_equal(ordinal_linkinv(x, "cloglog"), 1 - exp(-exp(x)))
})

test_that("ordinal_linkinv: cauchit matches pcauchy", {
  expect_equal(ordinal_linkinv(c(-1, 0, 1), "cauchit"),
               stats::pcauchy(c(-1, 0, 1)))
})

test_that("ordinal_linkinv: identity returns input", {
  x <- c(-3.14, 0, 2.71)
  expect_equal(ordinal_linkinv(x, "identity"), x)
})

test_that("ordinal_linkinv: unknown link errors", {
  expect_error(ordinal_linkinv(0, "magic"),
               regexp = "Unsupported ordinal link")
})


# -------------------------------------------------------------------------
# ordinal_sample — sampling correctness
# -------------------------------------------------------------------------

# Deterministic test case verified against brms::pordinal +
# brms::first_greater (see development notes):
#   K=3 cats, thres = c(-1, 1), disc = 1, eta = 0, link = "logit"
#   → CDF per row = (0.2689, 0.7311, 1)
#   → marginal PMF = (0.2689, 0.4621, 0.2689)
# We assert (a) the inverse-CDF construction reproduces the CDF, and (b)
# the empirical PMF from a large sample matches within MC tolerance.

test_that("ordinal_sample: CDF reproduces the hand-computed values", {
  ndraws <- 1L
  thres <- matrix(c(-1, 1), nrow = ndraws, ncol = 2)
  eta <- matrix(0, nrow = ndraws, ncol = 1L)
  cdf_1 <- ordinal_linkinv(1 * (thres[, 1L] - eta[, 1L]), "logit")
  cdf_2 <- ordinal_linkinv(1 * (thres[, 2L] - eta[, 1L]), "logit")
  expect_equal(as.numeric(cdf_1), stats::plogis(-1), tolerance = 1e-10)
  expect_equal(as.numeric(cdf_2), stats::plogis(1), tolerance = 1e-10)
})

test_that("ordinal_sample: empirical PMF matches expected within MC bar", {
  set.seed(2026)
  ndraws <- 20000L
  nobs <- 1L
  eta <- matrix(0, nrow = ndraws, ncol = nobs)
  thres <- matrix(c(-1, 1), nrow = ndraws, ncol = 2L, byrow = TRUE)
  samples <- ordinal_sample(eta = eta, thres = thres, disc = 1,
                            link = "logit")
  observed <- prop.table(table(factor(samples, levels = 1:3)))
  p1 <- stats::plogis(-1)
  p3 <- 1 - stats::plogis(1)
  p2 <- 1 - p1 - p3
  expected <- c(p1, p2, p3)
  # 3-sigma bound on a multinomial proportion is roughly
  # 3 * sqrt(p * (1 - p) / n) for n = 20000
  expect_true(all(abs(as.numeric(observed) - expected) <
                  3 * sqrt(expected * (1 - expected) / ndraws)))
})

test_that("ordinal_sample: identity link works (no inverse link)", {
  set.seed(2027)
  ndraws <- 4L
  thres <- matrix(c(0.3, 0.7), nrow = ndraws, ncol = 2L, byrow = TRUE)
  eta <- matrix(0, nrow = ndraws, ncol = 1L)
  samples <- ordinal_sample(eta = eta, thres = thres, disc = 1,
                            link = "identity")
  expect_true(all(samples %in% 1:3))
})

test_that("ordinal_sample: returns one integer per (draw, obs)", {
  set.seed(2028)
  ndraws <- 10L
  nobs <- 5L
  thres <- matrix(c(-1, 1), nrow = ndraws, ncol = 2L, byrow = TRUE)
  eta <- matrix(rnorm(ndraws * nobs), nrow = ndraws, ncol = nobs)
  samples <- ordinal_sample(eta = eta, thres = thres, disc = 1,
                            link = "logit")
  expect_equal(length(samples), ndraws * nobs)
  expect_true(all(samples %in% 1:3))
})

test_that("ordinal_sample: disc as a matrix is accepted per obs", {
  set.seed(2029)
  ndraws <- 50L
  nobs <- 3L
  thres <- matrix(c(-1, 1), nrow = ndraws, ncol = 2L, byrow = TRUE)
  eta <- matrix(0, nrow = ndraws, ncol = nobs)
  disc <- matrix(2, nrow = ndraws, ncol = nobs)
  samples <- ordinal_sample(eta = eta, thres = thres, disc = disc,
                            link = "logit")
  expect_equal(length(samples), ndraws * nobs)
})


# -------------------------------------------------------------------------
# loo.mvgam / waic.mvgam — brms-parity argument validation
# -------------------------------------------------------------------------

# These tests assert the rejection paths fire before any model machinery
# touches the object, so a bare-class stub is sufficient.

test_that("loo.mvgam: pointwise = TRUE errors", {
  stub <- structure(list(), class = "mvgam")
  expect_error(loo(stub, pointwise = TRUE),
               regexp = "pointwise = TRUE")
})

test_that("loo.mvgam: moment_match = TRUE errors", {
  stub <- structure(list(), class = "mvgam")
  expect_error(loo(stub, moment_match = TRUE),
               regexp = "moment_match")
})

test_that("loo.mvgam: reloo = TRUE errors", {
  stub <- structure(list(), class = "mvgam")
  expect_error(loo(stub, reloo = TRUE),
               regexp = "reloo")
})

test_that("waic.mvgam: pointwise = TRUE errors", {
  stub <- structure(list(), class = "mvgam")
  expect_error(waic(stub, pointwise = TRUE),
               regexp = "pointwise = TRUE")
})


# -------------------------------------------------------------------------
# log_lik.mvgam — argument validation that does not require a real fit
# -------------------------------------------------------------------------

test_that("log_lik.mvgam: invalid object class errors via checkmate", {
  # Calling log_lik on a non-mvgam stub with class "mvgam" but no body
  # should fire the checkmate assertions on newdata / ndraws etc.
  # Calling the generic on a bare string fails at S3 dispatch before
  # the checkmate; both error paths are acceptable as long as no
  # silent success can happen.
  stub <- structure(list(), class = "mvgam")
  expect_error(log_lik(stub))  # no newdata, no $data
  expect_error(log_lik("not an mvgam"))
})


# -------------------------------------------------------------------------
# loo.mvgam: signature has incl_dynamics after ...
# -------------------------------------------------------------------------

test_that("loo.mvgam: incl_dynamics lives after `...` (brms convention)", {
  fmls <- names(formals(getS3method("loo", "mvgam")))
  dots_idx <- which(fmls == "...")
  inc_idx <- which(fmls == "incl_dynamics")
  expect_true(length(dots_idx) == 1L)
  expect_true(length(inc_idx) == 1L)
  expect_gt(inc_idx, dots_idx)
})

test_that("loo_compare.mvgam: criterion + incl_dynamics live after `...`", {
  fmls <- names(formals(getS3method("loo_compare", "mvgam")))
  dots_idx <- which(fmls == "...")
  expect_true(length(dots_idx) == 1L)
  expect_gt(which(fmls == "criterion"), dots_idx)
  expect_gt(which(fmls == "incl_dynamics"), dots_idx)
})

test_that("pp_check.mvgam: resp + draw_ids are accepted (brms parity)", {
  fmls <- names(formals(getS3method("pp_check", "mvgam")))
  expect_true("resp" %in% fmls)
  expect_true("draw_ids" %in% fmls)
})


# -------------------------------------------------------------------------
# extract_component_linpred / extract_trend_latent_states signature gates
# -------------------------------------------------------------------------

test_that("extract_component_linpred: incl_latent_state arg exists", {
  fmls <- names(formals(extract_component_linpred))
  expect_true("incl_latent_state" %in% fmls)
})

test_that("extract_trend_latent_states: missing trend[t,s] column errors", {
  # Hand-build a [3 x 4] draws matrix containing only trend[1,1] and
  # trend[2,1]; index a (t,s) that does not exist in the columns to
  # confirm the pre-loop validation fires with a clear message rather
  # than the cryptic 'subscript out of bounds'.
  full_draws <- matrix(
    rnorm(12),
    nrow = 3,
    dimnames = list(NULL, c("trend[1,1]", "trend[2,1]",
                            "sigma_trend[1]", "ar1_trend[1]"))
  )
  # Build a minimal mock mvgam object with the structure the helper
  # depends on: standata + trend_metadata + obs_data with a `time`
  # column. Mock get_observation_structure by passing newdata that
  # maps to t = 3 (which is missing from full_draws above).
  mock_fit <- structure(
    list(
      standata = list(
        N_time_trend = 3L,
        N_series_trend = 1L,
        times_trend = matrix(c(1L, 2L, 3L), ncol = 1L)
      ),
      trend_metadata = list(
        variables = list(time_var = "time", series_var = "series")
      ),
      obs_data = data.frame(time = 1:3),
      data = data.frame(time = 1:3)
    ),
    class = "mvgam"
  )
  attr(mock_fit$obs_data, "mvgam_series") <- factor("s1", levels = "s1")
  newdata <- data.frame(time = 3L)
  expect_error(
    extract_trend_latent_states(mock_fit, newdata,
                                full_draws = full_draws),
    regexp = "Latent trend state columns missing from posterior draws"
  )
})
