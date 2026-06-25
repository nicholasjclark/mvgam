# Tests for the pre-fit `mvgam_data()` data inspector. Covers the
# happy path, the closure-unit dispatch, and the response-vs-family
# / structural errors users hit before paying for a Stan compile.


# ---- Happy path --------------------------------------------------

test_that("mvgam_data returns an mvgam_data list on a clean Poisson sim", {
  set.seed(1L)
  simdat <- sim_mvgam(family = poisson(), n_series = 3L,
                       n_timepoints = 20L)
  res <- suppressMessages(
    mvgam_data(simdat$data_train, y = "y", family = poisson(),
                plot = FALSE)
  )
  expect_s3_class(res, "mvgam_data")
  expect_identical(res$n_series, 3L)
  expect_identical(res$series_levels, levels(simdat$data_train$series))
  expect_equal(res$time_range,
               range(simdat$data_train$time, na.rm = TRUE))
})


test_that("mvgam_data validates closure-unit data via the shared validator", {
  cu <- sim_closure_unit_data(family = occ(), type = 1L)
  res <- suppressMessages(
    mvgam_data(cu$data_train, y = "y", family = occ(), plot = FALSE)
  )
  expect_s3_class(res, "mvgam_data")
})


test_that("mvgam_data synthesises a series factor when missing", {
  set.seed(2L)
  df <- data.frame(time = seq_len(20L),
                   y    = rpois(20L, lambda = 3))
  res <- suppressMessages(
    mvgam_data(df, y = "y", family = poisson(), plot = FALSE)
  )
  expect_identical(res$n_series, 1L)
})


# ---- Response-vs-family hard errors ------------------------------

test_that("mvgam_data errors on negative y with Poisson", {
  set.seed(3L)
  simdat <- sim_mvgam(family = poisson(), n_series = 2L,
                       n_timepoints = 15L)
  bad <- simdat$data_train
  bad$y[1L] <- -1L
  expect_error(
    suppressMessages(
      mvgam_data(bad, y = "y", family = poisson(), plot = FALSE)
    ),
    "negative values"
  )
})


test_that("mvgam_data errors on non-binary y with Bernoulli", {
  set.seed(4L)
  df <- data.frame(time   = seq_len(12L),
                   series = factor(rep("s1", 12L)),
                   y      = c(rep(0L, 6L), rep(2L, 6L)))
  expect_error(
    suppressMessages(
      mvgam_data(df, y = "y", family = bernoulli(), plot = FALSE)
    ),
    "0/1"
  )
})


test_that("mvgam_data errors on non-positive y with Gamma", {
  df <- data.frame(time   = seq_len(10L),
                   series = factor(rep("s1", 10L)),
                   y      = c(0, runif(9L)))
  expect_error(
    suppressMessages(
      mvgam_data(df, y = "y", family = Gamma(link = "log"),
                  plot = FALSE)
    ),
    "strictly positive"
  )
})


# ---- Structural errors -------------------------------------------

test_that("mvgam_data refuses multi-response families", {
  set.seed(5L)
  simdat <- sim_mvgam(family = poisson(), n_series = 2L,
                       n_timepoints = 10L)
  expect_error(
    suppressMessages(
      mvgam_data(simdat$data_train, y = "y", family = mvn(),
                  plot = FALSE)
    ),
    "Multi-response families"
  )
})


test_that("mvgam_data errors when 'series' is not a factor", {
  set.seed(6L)
  df <- data.frame(time   = seq_len(10L),
                   series = rep("s1", 10L),
                   y      = rpois(10L, lambda = 2))
  expect_error(
    suppressMessages(
      mvgam_data(df, y = "y", family = poisson(), plot = FALSE)
    ),
    "factor"
  )
})


# ---- Time regularity / CAR dispensation --------------------------

test_that("mvgam_data enforces regular time intervals by default", {
  set.seed(7L)
  df <- data.frame(time   = c(1L, 2L, 4L, 7L),
                   series = factor(rep("s1", 4L)),
                   y      = rpois(4L, lambda = 3))
  expect_error(
    suppressMessages(
      mvgam_data(df, y = "y", family = poisson(), plot = FALSE)
    ),
    "regular|Irregular"
  )
})


test_that("mvgam_data skips time-regularity check under CAR()", {
  set.seed(8L)
  df <- data.frame(time   = c(1L, 2L, 4L, 7L),
                   series = factor(rep("s1", 4L)),
                   y      = rpois(4L, lambda = 3))
  expect_no_error(
    suppressMessages(
      mvgam_data(df, y = "y", family = poisson(),
                  trend_model = CAR(), plot = FALSE)
    )
  )
})


# ---- check_mvgam_data() alias ------------------------------------

test_that("check_mvgam_data() is exported and dispatches to mvgam_data()", {
  expect_true(exists("check_mvgam_data", mode = "function",
                      envir = asNamespace("mvgam")))
  set.seed(1L)
  simdat <- sim_mvgam(family = poisson(), n_series = 2L,
                       n_timepoints = 16L)
  out_check <- suppressMessages(
    check_mvgam_data(simdat$data_train, family = poisson(),
                      plot = FALSE)
  )
  out_orig <- suppressMessages(
    mvgam_data(simdat$data_train, family = poisson(),
                plot = FALSE)
  )
  # The two entry points return objects with identical structure
  # (sans the call attribute, which we don't track).
  expect_s3_class(out_check, "mvgam_data")
  expect_identical(out_check$n_series, out_orig$n_series)
  expect_identical(out_check$series_levels, out_orig$series_levels)
  expect_identical(out_check$time_range, out_orig$time_range)
})


test_that("check_mvgam_data() forwards errors from mvgam_data()", {
  # rnorm() produces non-integer floats, which trips the
  # validate_response_for_family() non-integer branch under
  # family = poisson(); the negative-values branch is exercised
  # in the earlier "errors on negative y with Poisson" test.
  bad <- data.frame(
    y      = rnorm(10L),
    time   = seq_len(10L),
    series = factor("s1", levels = "s1")
  )
  expect_error(
    suppressMessages(
      check_mvgam_data(bad, y = "y", family = poisson(),
                        plot = FALSE)
    ),
    regexp = "Poisson|integer|non-negative"
  )
})


# ---- Covariate-NA guard (validate_no_covariate_nas) --------------

test_that("validate_no_covariate_nas() catches NAs in obs-formula covariates", {
  set.seed(1L)
  simdat <- sim_mvgam(family = poisson(), n_series = 2L,
                       n_timepoints = 16L)
  dat <- simdat$data_train
  dat$temp <- rnorm(nrow(dat))
  dat$temp[3L] <- NA
  # Without a formula, no covariate is referenced; should pass.
  expect_s3_class(
    suppressMessages(mvgam_data(dat, family = poisson(), plot = FALSE)),
    "mvgam_data"
  )
  # With a formula referencing the NA-bearing column, error.
  expect_error(
    suppressMessages(
      mvgam_data(dat, formula = y ~ temp, family = poisson(),
                  plot = FALSE)
    ),
    regexp = "Covariates referenced.*missing values.*'temp': 1 NA"
  )
})


test_that("validate_no_covariate_nas() checks trend-formula covariates", {
  set.seed(1L)
  simdat <- sim_mvgam(family = poisson(), n_series = 2L,
                       n_timepoints = 16L)
  dat <- simdat$data_train
  dat$env <- rnorm(nrow(dat))
  dat$env[c(2L, 7L)] <- NA
  expect_error(
    suppressMessages(
      mvgam_data(dat, trend_formula = ~ s(env) + AR(),
                  family = poisson(), plot = FALSE)
    ),
    regexp = "Covariates referenced.*'env': 2 NAs"
  )
})


test_that("validate_no_covariate_nas() ignores response + unreferenced cols", {
  set.seed(1L)
  simdat <- sim_mvgam(family = poisson(), n_series = 2L,
                       n_timepoints = 16L,
                       prop_missing = 0.2)
  dat <- simdat$data_train
  # Unreferenced NA column: should NOT trigger an error.
  dat$junk <- NA_real_
  expect_s3_class(
    suppressMessages(
      mvgam_data(dat, formula = y ~ 1, family = poisson(),
                  plot = FALSE)
    ),
    "mvgam_data"
  )
  # NAs in y (the response) are also allowed by the validator.
  expect_true(any(is.na(dat$y)))
})


test_that("validate_no_covariate_nas() catches NAs reached via gp() / RE group", {
  set.seed(1L)
  simdat <- sim_mvgam(family = poisson(), n_series = 2L,
                       n_timepoints = 16L)
  dat <- simdat$data_train
  # gp() reference: NAs in the GP input column must error.
  dat$loc <- rnorm(nrow(dat))
  dat$loc[2L] <- NA
  expect_error(
    suppressMessages(
      mvgam_data(dat, formula = y ~ gp(loc),
                  family = poisson(), plot = FALSE)
    ),
    regexp = "'loc': 1 NA"
  )
  # Random-effect grouping factor: NA in the grouping column.
  dat$loc <- rnorm(nrow(dat))  # clean again
  dat$grp <- factor(rep(c("a", "b"), length.out = nrow(dat)))
  dat$grp[3L] <- NA
  expect_error(
    suppressMessages(
      mvgam_data(dat, formula = y ~ (1 | grp),
                  family = poisson(), plot = FALSE)
    ),
    regexp = "'grp': 1 NA"
  )
})


test_that("validate_no_covariate_nas() counts NAs in matrix-column predictors", {
  # Matrix-column predictors (distributed-lag style) are
  # carried as list entries rather than data.frame columns
  # because as.data.frame() would flatten them. Exercise the
  # validator helper directly to confirm it counts every NA
  # cell in a 16 x 3 matrix predictor.
  dat <- list(
    y      = rpois(16L, 1),
    time   = 1:16,
    series = factor(rep("s1", 16L), levels = "s1"),
    Z      = matrix(rnorm(48L), ncol = 3L)
  )
  dat$Z[c(2L, 7L), 2L] <- NA
  expect_error(
    validate_no_covariate_nas(dat, formulas = list(y ~ Z),
                                response_vars = "y"),
    regexp = "'Z': 2 NAs"
  )
})


test_that("extract_response_vars() handles formula / brmsformula / mvbrmsformula", {
  expect_identical(extract_response_vars(NULL),    character(0L))
  expect_identical(extract_response_vars(~ x),     character(0L))
  expect_identical(extract_response_vars(y ~ x),   "y")
  expect_setequal(extract_response_vars(cbind(y, trials) ~ x),
                  c("y", "trials"))
  # brmsformula: response on $formula slot.
  expect_identical(extract_response_vars(brms::bf(y ~ x)), "y")
  # Two-arm bf(): still only the top response, not the dpar arm.
  expect_identical(extract_response_vars(brms::bf(y ~ env, p ~ tod)),
                   "y")
})


test_that("extract_predictor_vars() handles formula / brmsformula / bf arms", {
  expect_identical(extract_predictor_vars(NULL),     character(0L))
  expect_identical(extract_predictor_vars(y ~ 1),    character(0L))
  expect_setequal(extract_predictor_vars(y ~ x + z), c("x", "z"))
  expect_setequal(extract_predictor_vars(y ~ s(x, by = grp)),
                  c("x", "grp"))
  # Trend constructor bare names should be picked up.
  expect_setequal(extract_predictor_vars(~ AR(time = week, series = sp)),
                  c("week", "sp"))
  # Two-arm bf() (closure-unit detection sub-formula).
  bf_two <- brms::bf(y ~ env, p ~ tod)
  expect_setequal(extract_predictor_vars(bf_two), c("env", "tod"))
  # List of formulas: union the predictors, drop duplicates.
  expect_setequal(
    extract_predictor_vars(list(y ~ x, ~ s(z, by = grp))),
    c("x", "z", "grp")
  )
})
