# Tests for the mvgam_formula constructor, its validation and its
# get_prior() method

test_that("mvgam_formula keeps the formulas it is given", {
  mf <- mvgam_formula(y ~ x, trend_formula = ~ x - 1 + AR())
  expect_identical(class(mf), c("mvgam_formula", "formula"))
  expect_named(mf, c("formula", "trend_formula"))
  expect_identical(mf$formula, y ~ x)
  expect_identical(deparse(mf$trend_formula), "~x - 1 + AR()")
  expect_null(mvgam_formula(y ~ x)$trend_formula)

  bf1 <- brms::bf(y ~ x)
  mf_bf <- mvgam_formula(bf1)
  expect_identical(class(mf_bf), c("mvgam_formula", "brmsformula"))
  expect_identical(mf_bf$formula, bf1)

  # A dot would sweep the time and series columns into the model.
  expect_error(mvgam_formula(y ~ .), "not supported")
  expect_error(mvgam_formula(brms::bf(y ~ x + .)), "not supported")
})

test_that("mvgam_formula takes formulas alone", {
  for (bad in list("not a formula", NULL, 123, list(y ~ x))) {
    expect_error(mvgam_formula(bad), "Assertion on 'formula' failed")
  }
  for (bad in list("not a formula", 123, list(~ 1))) {
    expect_error(mvgam_formula(y ~ x, trend_formula = bad),
                 "Assertion on 'trend_formula' failed")
  }
})

test_that("print.mvgam_formula shows both formulas", {
  out <- capture.output(print(mvgam_formula(y ~ x)))
  expect_match(out[1], "mvgam_formula object")
  expect_true(any(grepl("NULL \\(no trend component\\)", out)))
  mf <- mvgam_formula(y ~ x, trend_formula = ~ s(time))
  capture.output(returned <- expect_invisible(print(mf)))
  expect_identical(returned, mf)
  out <- capture.output(print(mf))
  expect_true(any(grepl("Trend formula:", out)))
  expect_true(any(grepl("s\\(time\\)", out)))
})

test_that("get_prior on a trendless mvgam_formula is the brms table", {
  test_data <- data.frame(
    y = rnorm(20), x = rnorm(20), count = rpois(20, 3),
    binary = rbinom(20, 1, 0.5), group = factor(rep(1:4, 5))
  )
  cases <- list(
    gaussian = list(f = y ~ x, family = gaussian()),
    poisson = list(f = count ~ x, family = poisson()),
    binomial = list(f = binary ~ x, family = binomial()),
    random = list(f = y ~ x + s(x) + (1 | group), family = gaussian()),
    bf = list(f = brms::bf(y ~ x), family = gaussian()),
    # A family embedded in bf() needs no family argument
    embedded = list(f = brms::bf(y ~ x, family = gaussian()), family = NULL)
  )
  for (nm in names(cases)) {
    cs <- cases[[nm]]
    args <- list(data = test_data)
    if (!is.null(cs$family)) args$family <- cs$family
    mvgam_result <- do.call(get_prior, c(list(mvgam_formula(cs$f)), args))
    brms_result <- do.call(brms::get_prior, c(list(cs$f), args))
    expect_identical(mvgam_result[, names(brms_result)], brms_result,
                     label = nm)
  }
  mv <- get_prior(
    mvgam_formula(brms::bf(mvbind(y, x) ~ 1) + brms::set_rescor(FALSE)),
    data = test_data
  )
  expect_false(any(grepl("_trend$", mv$class)))
})

test_that("brms's `+` combines user priors with an mvgam prior table", {
  d <- data.frame(y = rnorm(20), x = rnorm(20), time = 1:20,
                  series = factor(rep("A", 20)))
  tab <- get_prior(mvgam_formula(y ~ x, trend_formula = ~ AR()),
                   family = gaussian(), data = d)
  combined <- brms::prior("normal(0, 0.5)", class = "ar1_trend") +
    brms::set_prior("normal(0, 2)", class = "Intercept") + tab
  expect_true(all(c("normal(0, 0.5)", "normal(0, 2)") %in% combined$prior))
})

test_that("a multi-response formula routes its trend_formula to mvgam", {
  set.seed(4)
  d <- data.frame(
    time = rep(1:10, each = 2L), series = factor(rep(c("a", "b"), 10L)),
    count = rpois(20L, 5), biomass = rnorm(20L)
  )
  f <- brms::bf(count ~ 1, family = poisson()) +
    brms::bf(biomass ~ 1, family = gaussian()) + brms::set_rescor(FALSE)
  # Without a method for the mvbrmsformula class the call reached brms,
  # which dropped the trend
  expect_identical(
    as.character(stancode(f, trend_formula = ~ RW(), data = d,
                          validate = FALSE)),
    as.character(stancode(mvgam_formula(f, trend_formula = ~ RW()),
                          data = d, validate = FALSE))
  )
  expect_identical(
    standata(f, trend_formula = ~ RW(), data = d)$N_series_trend, 2L
  )
})

test_that("error handling for invalid get_prior calls", {
  expect_error(
    get_prior(mvgam_formula(y ~ x), data = NULL),
    "Assertion on 'data' failed"
  )
  # Missing response in formula, refused by the one reader of a
  # model's responses
  expect_error(
    get_prior(mvgam_formula(~ x), data = data.frame(x = 1:5)),
    "The observation formula needs a response"
  )
  # brms refuses an unknown family name and lists the names it accepts
  test_data <- data.frame(y = rnorm(20), x = rnorm(20))
  expect_error(
    get_prior(mvgam_formula(y ~ x), family = "not a family",
              data = test_data),
    "supported family"
  )
})

test_that("trend_formula refuses brms addition and autocorrelation terms", {
  # The trend dynamics replace brms autocorrelation
  err <- expect_error(
    mvgam_formula(y ~ x, trend_formula = ~ autocor(M = ~ 1) + weights(w))
  )
  expect_match(conditionMessage(err),
               "brms autocorrelation terms are not supported")
  expect_match(conditionMessage(err), "conflict with the trend dynamics")

  # A term is found however deeply it is nested
  refused <- list(
    ~ subset(idx),
    ~ cov_ranef(M = ~ 1),
    ~ s(time) + (1 | group) + weights(w),
    ~ (1 | group) + I(weights(w) * 2),
    ~ ((time + weights(w)) * factor),
    ~ x - 1 + weights(w) + AR()
  )
  for (tf in refused) {
    expect_error(
      mvgam_formula(y ~ x, trend_formula = tf),
      "brms addition terms are not supported",
      label = deparse(tf)
    )
  }

  # The observation formula takes them. A covariate named like a term
  # is a variable.
  expect_no_error(
    mvgam_formula(y ~ x + s(time) + weights(w) + (1 | group),
                  trend_formula = ~ weights + s(season))
  )
})

test_that("exact and approximate GP terms are accepted on either side", {
  # Exact GPs fit through brms with a full covariance kernel. Their
  # notice covers the prediction-at-newdata gap and is quiet under
  # testthat.
  expect_no_error(mvgam_formula(y ~ gp(x), trend_formula = ~ 1))
  expect_no_error(
    mvgam_formula(y ~ x, trend_formula = ~ s(time) + gp(season) + AR())
  )
  expect_no_error(
    mvgam_formula(y ~ gp(temperature, k = 5, cov = "matern32"),
                  trend_formula = ~ gp(time, k = 15))
  )
  expect_no_error(
    mvgam_formula(y ~ gp(x, k = 5) + gp(z), trend_formula = ~ 1)
  )
})

test_that("the exact GP notice shows once per term under 'silent' < 2", {
  # The notice is a message, quiet under testthat and at `silent = 2`.
  # A fresh term keeps the once-per-session count of earlier tests
  # away from this one.
  withr::local_envvar(TESTTHAT = "")
  expect_message(
    mvgam_formula(y ~ gp(z), trend_formula = ~ 1),
    "Exact GP term"
  )
  expect_no_message(mvgam_formula(y ~ gp(z), trend_formula = ~ 1))
  withr::local_options(mvgam.silent = 2L)
  expect_no_message(mvgam_formula(y ~ gp(z2), trend_formula = ~ 1))
})

test_that("bf() in trend_formula errors with hint", {
  # A non-linear trend formula was named as a distributional one.
  expect_error(
    mvgam_formula(y ~ 1, trend_formula = bf(~ a * x, a ~ 1, nl = TRUE)),
    "Found 'bf\\(\\)' sub-formulas for: 'a'"
  )

  err <- conditionMessage(expect_error(
    mvgam_formula(
      y ~ 1,
      trend_formula = bf(~ AR(p = 1), sigma ~ z)
    )
  ))
  expect_match(err, "'sigma'")
  expect_match(err, "obs", ignore.case = TRUE)
})
