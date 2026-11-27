# The tests of test-mvgam-data.R that assemble a model or run for
# 0.2 s or more. CRAN skips this file. CI and local runs take it.
skip_on_cran()

test_that("a trend requires a series column", {
  set.seed(2L)
  df <- data.frame(time = seq_len(20L),
                   y    = rpois(20L, lambda = 3))
  # A model without a trend has no series axis to fill.
  res <- suppressMessages(
    mvgam_data(df, y = "y", family = poisson(), plot = TRUE)
  )
  expect_null(res$series_levels)
  expect_identical(res$n_series, 0L)
  expect_s3_class(res$plot, "ggplot")
  # A trend needs each row's series. mvgam_data() and stancode()
  # raise the same refusal, which gives the one-line remedy for a
  # single series.
  err <- expect_error(
    mvgam_data(df, y = "y", family = poisson(),
               trend_formula = ~ AR(), plot = FALSE)
  )
  fit_err <- expect_error(
    stancode(mvgam_formula(y ~ 1, trend_formula = ~ AR()),
             data = df, family = poisson())
  )
  expect_identical(conditionMessage(err), conditionMessage(fit_err))
  expect_match(conditionMessage(err), "Column 'series' is absent")
  expect_match(conditionMessage(err),
               'data$series <- factor("series_1")', fixed = TRUE)
  df$series <- factor("series_1")
  res <- suppressMessages(
    mvgam_data(df, y = "y", family = poisson(),
               trend_formula = ~ AR(), plot = FALSE)
  )
  expect_identical(res$n_series, 1L)
  # A held-out frame is named in the refusal and its remedy
  expect_error(
    suppressMessages(mvgam_data(df, family = poisson(),
                                trend_formula = ~ AR(),
                                newdata = df[, c("y", "series")])),
    "Add 'time' to 'newdata'"
  )
})

test_that("the response comes from the formula", {
  set.seed(7L)
  df <- data.frame(time = seq_len(12L), x = rnorm(12L),
                   count = c(NA, rpois(11L, 3)))
  res <- suppressMessages(
    mvgam_data(as.list(df), formula = count ~ x, family = poisson(),
               newdata = as.list(df), series = 1L)
  )
  expect_identical(res$n_missing, 1L)
  expect_s3_class(res$plot, "patchwork")
  expect_error(
    mvgam_data(df, y = "x", formula = count ~ x, plot = FALSE),
    "'y' names 'x' but the formula's response is 'count'"
  )
  df$z <- rnorm(12L)
  expect_error(
    mvgam_data(df, formula = bf(count ~ 1) + bf(z ~ 1) + set_rescor(FALSE),
               plot = FALSE),
    "checks one response at a time"
  )
})

test_that("the trend link warning reaches a prefit", {
  # The helper alone would pass with the call site mis-wired, so the
  # warning is driven through `mvgam()` itself. `run_model = FALSE`
  # keeps the check free of sampling.
  set.seed(1)
  df <- data.frame(
    y = rgamma(20L, shape = 2, rate = 1),
    time = seq_len(20L),
    series = factor(rep("s1", 20L))
  )
  expect_warning(
    mvgam(y ~ 1, trend_formula = ~ AR(p = 1), data = df,
          family = Gamma(), run_model = FALSE, silent = 2, validate = FALSE),
    "positive linear\\s+predictor"
  )
})

test_that("the trend link warning reaches mvgam_multiple", {
  # `mvgam_multiple()` replays the call through `mvgam()`, which raises
  # the notice once for every imputation.
  set.seed(2)
  mk <- function() {
    data.frame(
      y = rgamma(20L, shape = 2, rate = 1),
      time = seq_len(20L),
      series = factor(rep("s1", 20L))
    )
  }
  expect_warning(
    mvgam_multiple(y ~ 1, trend_formula = ~ AR(p = 1),
                   data_list = list(mk(), mk()), family = Gamma(),
                   run_model = FALSE, silent = 2, validate = FALSE),
    "positive linear\\s+predictor"
  )
})

test_that("columns the formulas use must be in the data", {
  set.seed(1L)
  dat <- sim_mvgam(family = poisson(), n_series = 2L,
                   n_timepoints = 16L)$data_train
  dat$n <- dat$y + 2L
  dat$x <- rnorm(nrow(dat))
  # Addition terms, offsets and random-effect groups are all columns,
  # and brms would report them only after the trend was parsed.
  expect_error(
    suppressMessages(mvgam_data(
      dat, formula = y | trials(n) ~ x + offset(log(area)) + (1 | site),
      family = binomial(), trend_formula = ~ s(env) + AR(), plot = FALSE
    )),
    "Absent: 'site', 'area', 'env'"
  )
  # A name in 'data2' counts as present, as brms counts it.
  expect_silent(validate_model_columns(
    dat, y ~ s(lagm, k = 4), poisson(),
    data2 = list(lagm = matrix(0, nrow(dat), 2L))
  ))
  # A prediction frame may omit its response but not its trials.
  expect_silent(validate_model_columns(
    dat[, c("time", "series", "n", "x")], y | trials(n) ~ x, binomial(),
    optional = "y", context = "newdata"
  ))
  expect_error(
    validate_model_columns(dat[, c("time", "series", "x")],
                           y | trials(n) ~ x, binomial(),
                           optional = "y", context = "newdata"),
    "missing from 'newdata'"
  )
  # A count may not exceed its denominator.
  dat$n[5L] <- dat$y[5L] - 1L
  expect_error(
    validate_response_shapes(dat, y | trials(n) ~ x, binomial()),
    "first at row 5"
  )
})

# ---- Response support (validate_response_for_family) -------------

# brms holds each family's support in `family_info(family, "ybounds")`
# and `"closed"`, which is unexported and closed to a CRAN package.
# mvgam carries the same values in `mvgam_response_support`. The check
# drives brms through its public `make_standata()` and lists the
# family and probe vector where the two disagree on refusal.
response_support_mismatches <- function(families) {
  probes <- list(
    negative = c(-1, 0.5, 2),
    zero = c(0, 0.5, 2),
    fractional = c(1, 2.5, 4),
    unit_low = c(0.1, 0.4, 0.9),
    above_one = c(0.1, 0.4, 1.4),
    exactly_one = c(0.1, 0.4, 1),
    large = c(1, 2, 9)
  )
  # `gamma` and `beta` resolve to base R functions. The table names
  # each family by its constructor.
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
  mismatches <- character(0)
  for (fam_name in families) {
    fam <- ctors[[fam_name]]()
    for (pname in names(probes)) {
      y <- probes[[pname]]
      dat <- data.frame(y = y, x = seq_along(y))
      brms_ok <- is.null(caught_error(
        brms::make_standata(y ~ 1, data = dat, family = fam)
      ))
      mvgam_ok <- is.null(caught_error(
        validate_response_for_family(y, fam, "y")
      ))
      if (!identical(mvgam_ok, brms_ok)) {
        mismatches <- c(mismatches, paste(fam_name, pname))
      }
    }
  }
  mismatches
}

test_that("response support matches brms for each kind of bound", {
  # Counts, binary, positive, the open and the closed unit interval,
  # and the circle
  expect_identical(response_support_mismatches(c(
    "poisson", "bernoulli", "gamma", "beta", "zero_one_inflated_beta",
    "von_mises"
  )), character(0))
})

test_that("response support matches brms for every other shared family", {
  expect_identical(response_support_mismatches(c(
    "negbinomial", "geometric", "lognormal", "weibull", "exponential",
    "frechet", "inverse.gaussian", "zero_inflated_beta", "hurdle_gamma",
    "hurdle_lognormal", "hurdle_poisson", "zero_inflated_poisson"
  )), character(0))
})

test_that("time regularity is gated on the trend's own rules", {
  # Each trend's `validation_rules` decide whether times must be
  # evenly spaced. `ZMVN()` indexes its covariance by series alone and
  # accepts gaps.
  set.seed(2)
  irregular <- sort(sample(seq_len(40), 25))
  dat <- data.frame(
    y = rpois(50, 4),
    time = rep(irregular, 2),
    series = factor(rep(c("a", "b"), each = 25))
  )
  expect_silent(suppressMessages(
    mvgam_data(dat, trend_formula = ~ ZMVN(), family = poisson())
  ))
  expect_silent(suppressMessages(
    mvgam_data(dat, trend_formula = ~ CAR(), family = poisson())
  ))
  expect_error(
    suppressMessages(
      mvgam_data(dat, trend_formula = ~ AR(p = 1), family = poisson())
    ),
    "Gaps between times range from 1 to 3"
  )
  # A model without a trend has no dynamics to space evenly
  expect_silent(suppressMessages(mvgam_data(dat, family = poisson())))
})
