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

test_that("the trend constructor names the axis columns", {
  set.seed(3L)
  df <- data.frame(week = rep(seq_len(10L), 2L),
                   species = factor(rep(c("a", "b"), each = 10L)),
                   y = rpois(20L, lambda = 3))
  res <- suppressMessages(
    mvgam_data(df, family = poisson(),
               trend_formula = ~ AR(time = week, series = species),
               plot = FALSE)
  )
  expect_identical(res$series_levels, c("a", "b"))
  expect_identical(res$time_range, c(1L, 10L))
})

test_that("mvgam_data refuses a mixture family as mvgam() does", {
  set.seed(4L)
  df <- data.frame(time = seq_len(10L),
                   series = factor(rep("s1", 10L)),
                   y = rnorm(10L))
  mix <- suppressMessages(brms::mixture(gaussian, gaussian))
  expect_error(
    mvgam_data(df, family = mix, plot = FALSE),
    "Mixture families are not supported"
  )
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

# ---- Structural errors -------------------------------------------

test_that("a positive mean under a zero-admitting link warns with a trend", {
  # `Gamma()` supplies the inverse link, whose mean is positive only
  # where the linear predictor is. A latent trend takes negative
  # values, where the likelihood is undefined.
  # `insight` wraps at the console width, so the pattern tolerates a
  # line break where the message happens to fold.
  expect_warning(
    warn_positive_mean_link_with_trend(y ~ 1, Gamma(), ~ AR(p = 1)),
    "positive linear\\s+predictor"
  )
  # `inverse.gaussian()` arrives with the `1/mu^2` link.
  expect_warning(
    warn_positive_mean_link_with_trend(
      y ~ 1, stats::inverse.gaussian(), ~ AR(p = 1)
    ),
    "inverse.gaussian"
  )

  # The log link keeps the mean positive for any predictor.
  expect_silent(
    warn_positive_mean_link_with_trend(y ~ 1, Gamma(link = "log"), ~ AR(p = 1))
  )
  # A trendless GLM keeps the inverse link without trouble, which is
  # why the trend is part of the condition.
  expect_silent(warn_positive_mean_link_with_trend(y ~ 1, Gamma(), NULL))
  # `lognormal()` carries `mu` as a log-scale location taking any
  # sign, and identity is its right link. A check built from
  # `mvgam_response_support` would group it with Gamma and warn on
  # every lognormal trend fit.
  expect_silent(
    warn_positive_mean_link_with_trend(y ~ 1, brms::lognormal(), ~ AR(p = 1))
  )
  expect_silent(
    warn_positive_mean_link_with_trend(y ~ 1, poisson(), ~ AR(p = 1))
  )
  # A family given by name is checked as the object it names. By name
  # brms gives Gamma the log link, and the identity link is named.
  expect_silent(
    warn_positive_mean_link_with_trend(y ~ 1, "Gamma", ~ AR(p = 1))
  )
  expect_warning(
    warn_positive_mean_link_with_trend(
      y ~ 1, c("Gamma", "identity"), ~ AR(p = 1)
    ),
    "link = \"identity\""
  )
  # A response's own family is checked where `family` is harmless.
  mixed <- bf(y1 ~ 1, family = Gamma()) + bf(y2 ~ 1) + set_rescor(FALSE)
  expect_warning(
    warn_positive_mean_link_with_trend(mixed, poisson(), ~ AR(p = 1)),
    "Family 'gamma'"
  )
})

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

# ---- Model columns (validate_model_columns) ---------------------

test_that("NAs in observation covariates are refused", {
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
    regexp = "hold missing values.*'temp': 1 NA, first at row 3"
  )
})

test_that("NAs in trend covariates are refused", {
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
    regexp = "'env': 2 NAs"
  )
})

test_that("NAs in the response and unused columns pass", {
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
})

test_that("an addition term is complete on the rows its response was seen", {
  # brms drops a row whose addition term is missing. The trend mapping
  # keeps every row with an observed response, and a missing weight
  # would leave the likelihood and the trend on different rows.
  dat <- data.frame(y = rpois(12L, 3), x = rnorm(12L),
                    w = runif(12L, 0.2, 1.5))
  dat$w[4L] <- NA
  expect_error(
    validate_model_columns(dat, y | weights(w) ~ x, poisson()),
    regexp = "'w': 1 NA"
  )
  # A row whose response is also missing leaves the likelihood, and
  # its addition term is not checked.
  dat$y[4L] <- NA
  expect_silent(validate_model_columns(dat, y | weights(w) ~ x, poisson()))
})

test_that("gp() and RE group columns are checked for NAs", {
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

test_that("NAs in matrix-column predictors are counted", {
  # A matrix predictor (distributed-lag style) is carried as a list
  # entry, since as.data.frame() would flatten it. A row counts once
  # however many of its cells are missing.
  dat <- list(
    y      = rpois(16L, 1),
    time   = 1:16,
    series = factor(rep("s1", 16L), levels = "s1"),
    Z      = matrix(rnorm(48L), ncol = 3L)
  )
  dat$Z[c(2L, 7L), 2L] <- NA
  dat$Z[2L, 3L] <- NA
  expect_error(
    validate_model_columns(dat, y ~ Z, gaussian()),
    regexp = "'Z': 2 NAs"
  )
})

test_that("lhs_columns() handles formula / brmsformula / mvbrmsformula", {
  expect_identical(lhs_columns(NULL),    character(0L))
  expect_identical(lhs_columns(~ x),     character(0L))
  expect_identical(lhs_columns(y ~ x),   "y")
  expect_identical(lhs_columns(cbind(y, trials) ~ x),
                   c("y", "trials"))
  # brmsformula: response on $formula slot.
  expect_identical(lhs_columns(brms::bf(y ~ x)), "y")
  # A bf() with a detection formula: the main response alone.
  expect_identical(lhs_columns(brms::bf(y ~ env, p ~ tod)),
                   "y")
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
    mvgam(y ~ 1, data = bad, family = Beta(), run_model = FALSE,
          validate = FALSE)
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
  # An all-NA response has nothing to check.
  expect_silent(validate_response_for_family(c(NA_real_, NA_real_),
                                              poisson(), "y"))
  # tweedie() is mvgam's own family, absent from the brms comparison
  expect_error(validate_response_for_family(c(1, 2, -3), tweedie(), "y"),
               "outside the support")
})

test_that("only a response is held to its family, and to its own", {
  dat <- data.frame(count = rpois(10L, 3), mass = rnorm(10L),
                    x = rnorm(10L), w = runif(10L, 0.2, 1.5))
  # A fractional weight is not an observation of a count family.
  expect_silent(
    validate_response_shapes(dat, count | weights(w) ~ x, poisson())
  )
  # Each arm answers to the family its own `bf()` names; the family
  # given to `mvgam()` covers only an arm that names none.
  arms <- brms::bf(count ~ x, family = poisson()) +
    brms::bf(mass ~ x, family = gaussian())
  expect_silent(validate_response_shapes(dat, arms, poisson()))
  dat$count[2L] <- 2.5
  expect_error(validate_response_shapes(dat, arms, gaussian()),
               regexp = "'count'")
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
  expect_silent(validate_response_shapes(fc, y ~ 1, poisson()))
  expect_silent(validate_response_shapes(tr, y ~ 1, poisson()))
  # A value it does carry is held to the family.
  bad <- sd$data_test
  bad$y[2L] <- -5
  err <- expect_error(
    mvgam(y ~ 1, data = tr, newdata = bad, family = poisson(),
          run_model = FALSE, validate = FALSE)
  )
  expect_match(conditionMessage(err), "outside the support")
})
