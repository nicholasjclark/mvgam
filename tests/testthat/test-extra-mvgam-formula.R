# The tests of test-mvgam-formula.R that assemble a model or run for
# 0.2 s or more. CRAN skips this file. CI and local runs take it.
skip_on_cran()

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
