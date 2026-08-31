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
    "outside the support"
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
    "outside the support"
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
    "outside the support"
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


# ---- Response support (validate_response_for_family) -------------

test_that("response support matches brms for every shared family", {
  # brms holds each family's support in `family_info(family,
  # "ybounds")` / `"closed"`, which is unexported and so closed to
  # a CRAN package. mvgam carries the same values in
  # `mvgam_response_support`; this drives brms through its public
  # `make_standata()` and asserts the two refuse the same vectors,
  # so the copied values cannot drift from their source.
  shared <- c(
    "poisson", "negbinomial", "geometric", "bernoulli", "gamma",
    "lognormal", "weibull", "exponential", "frechet",
    "inverse.gaussian", "beta", "zero_inflated_beta",
    "zero_one_inflated_beta", "von_mises", "hurdle_gamma",
    "hurdle_lognormal", "hurdle_poisson", "zero_inflated_poisson"
  )
  probes <- list(
    negative = c(-1, 0.5, 2),
    zero = c(0, 0.5, 2),
    fractional = c(1, 2.5, 4),
    unit_low = c(0.1, 0.4, 0.9),
    above_one = c(0.1, 0.4, 1.4),
    exactly_one = c(0.1, 0.4, 1),
    large = c(1, 2, 9)
  )
  # `gamma` and `beta` resolve to base R functions, so each family
  # is named by its constructor rather than looked up by string.
  ctors <- list(
    poisson = poisson, negbinomial = brms::negbinomial,
    geometric = brms::geometric, bernoulli = brms::bernoulli,
    gamma = function() Gamma(), lognormal = brms::lognormal,
    weibull = brms::weibull, exponential = brms::exponential,
    frechet = brms::frechet,
    inverse.gaussian = function() inverse.gaussian(),
    beta = function() brms::brmsfamily("beta"),
    zero_inflated_beta = brms::zero_inflated_beta,
    zero_one_inflated_beta = brms::zero_one_inflated_beta,
    von_mises = brms::von_mises, hurdle_gamma = brms::hurdle_gamma,
    hurdle_lognormal = brms::hurdle_lognormal,
    hurdle_poisson = brms::hurdle_poisson,
    zero_inflated_poisson = brms::zero_inflated_poisson
  )
  for (fam_name in shared) {
    fam <- ctors[[fam_name]]()
    for (pname in names(probes)) {
      y <- probes[[pname]]
      dat <- data.frame(y = y, x = seq_along(y))
      brms_ok <- !inherits(try(
        brms::make_standata(y ~ 1, data = dat, family = fam),
        silent = TRUE
      ), "try-error")
      mvgam_ok <- !inherits(try(
        validate_response_for_family(y, fam, "y"),
        silent = TRUE
      ), "try-error")
      # Named so a failure says which family and which probe.
      expect_equal(
        mvgam_ok, brms_ok,
        label = paste0(fam_name, " / ", pname, ": mvgam accepts")
      )
    }
  }
})


test_that("mvgam() reports the response support itself, not brms", {
  # The message has to name the column and the values that broke
  # it; brms' own names neither and states only the first bound it
  # reaches.
  bad <- data.frame(
    y = c(0.2, 0.5, 1.4, 0.3),
    time = 1:4,
    series = factor(rep("s1", 4L))
  )
  err <- expect_error(
    mvgam(y ~ 1, data = bad, family = Beta(), run_model = FALSE)
  )
  # insight wraps the message, so a phrase can straddle a newline.
  msg <- gsub("[[:space:]]+", " ", conditionMessage(err))
  expect_match(msg, "'y'", fixed = TRUE)
  expect_match(msg, "0 < 'y' < 1", fixed = TRUE)
  expect_match(msg, "1.4", fixed = TRUE)
  expect_match(msg, "first at row 3", fixed = TRUE)
  # brms' phrasing must not be what surfaces.
  expect_false(grepl("requires response smaller than", msg))
})


test_that("every family the support table names is refused by mvgam()", {
  # A family reaching brms means the table has no entry for it, and
  # the user gets a message naming neither their column nor their
  # data.
  cases <- list(
    list(geometric(), c(1, 2, -3)),
    list(weibull(), c(1, 2, -3)),
    list(exponential(), c(1, 2, -3)),
    list(frechet(), c(1, 2, -3)),
    list(inverse.gaussian(), c(1, 2, -3)),
    list(zero_inflated_beta(), c(0.2, 0.5, 1.4)),
    list(zero_one_inflated_beta(), c(0.2, 0.5, 1.4)),
    list(von_mises(), c(0, 1, 9)),
    list(tweedie(), c(1, 2, -3))
  )
  for (cs in cases) {
    y <- cs[[2]]
    dat <- data.frame(
      y = y, time = seq_along(y),
      series = factor(rep("s1", length(y)))
    )
    err <- expect_error(
      mvgam(y ~ 1, data = dat, family = cs[[1]], run_model = FALSE)
    )
    expect_match(conditionMessage(err), "outside the support")
  }
})


test_that("a factor response is left to the family that takes one", {
  # Comparing a factor to floor() errors, so the check has to stand
  # aside rather than break a legitimate bernoulli fit.
  y <- factor(c("a", "b", "a", "b"))
  expect_silent(validate_response_for_family(y, bernoulli(), "y"))
  expect_silent(validate_response_for_family(y, cumulative(), "y"))
})


test_that("a response inside its support passes every family", {
  expect_silent(validate_response_for_family(c(0, 1, 5), poisson(), "y"))
  expect_silent(validate_response_for_family(c(0.1, 0.9), Beta(), "y"))
  expect_silent(validate_response_for_family(c(0, 0.9),
                                              zero_inflated_beta(), "y"))
  expect_silent(validate_response_for_family(c(0, 1),
                                              zero_one_inflated_beta(), "y"))
  expect_silent(validate_response_for_family(c(0.5, 2), Gamma(), "y"))
  expect_silent(validate_response_for_family(c(0, 2.5), tweedie(), "y"))
  # All-NA and empty responses have nothing to check.
  expect_silent(validate_response_for_family(c(NA_real_, NA_real_),
                                              poisson(), "y"))
})


test_that("newdata is held to the same support as the training data", {
  set.seed(1L)
  sd <- sim_mvgam(family = poisson(), n_series = 2L,
                   n_timepoints = 30L)
  tr <- sd$data_train
  # A forecast frame carries an all-NA response and must pass.
  # Driven through the validator rather than mvgam(), because a
  # call that clears validation runs on into the model build.
  fc <- sd$data_test
  fc$y <- NA_real_
  expect_silent(validate_response_shapes(fc, "y", poisson()))
  expect_silent(validate_response_shapes(tr, "y", poisson()))
  # A value it does carry is held to the family.
  bad <- sd$data_test
  bad$y[2L] <- -5
  err <- expect_error(
    mvgam(y ~ 1, data = tr, newdata = bad, family = poisson(),
          run_model = FALSE)
  )
  expect_match(conditionMessage(err), "outside the support")
})
