# The tests of test-marginaleffects-insight.R that assemble a model or run for
# 0.2 s or more. CRAN skips this file. CI and local runs take it.
skip_on_cran()

test_that("model_info describes each response of a multivariate fit", {
  # insight returns one description per response for a brms fit. A
  # lookup of `$family` alone gave every response the last family.
  d <- data.frame(time = 1:20, count = rpois(20, 4), seen = rbinom(20, 1, 0.5))
  pf <- mvgam(
    brms::bf(count ~ 1, family = poisson()) +
      brms::bf(seen ~ 1, family = bernoulli()) + brms::set_rescor(FALSE),
    trend_formula = ~ AR(p = 1), data = d, run_model = FALSE, validate = FALSE
  )
  info <- insight::model_info(pf)
  expect_named(info, c("count", "seen"))
  expect_true(info$count$is_count)
  expect_false(info$count$is_binomial)
  expect_true(info$seen$is_binomial)
  expect_false(info$seen$is_count)
  expect_identical(insight::model_info(pf, response = "seen"), info$seen)
  expect_error(insight::model_info(pf, response = "mass"),
               "must be a response of this model")
})

test_that("the term list sorts each column to the readers that need it", {
  # One parse serves `find_predictors()`, `find_random()`,
  # `find_variables()`, `terms()` and the prediction grid. A grouping,
  # a binomial denominator and an offset each name a column the model
  # uses. Nobody takes a slope over them. Each must still appear in the
  # prediction grid.
  set.seed(1)
  d <- data.frame(
    time = rep(1:10, 2),
    series = factor(rep(c("a", "b"), each = 10L)),
    y = rbinom(20L, 10L, 0.5), n = 10L,
    x = rnorm(20L), e = runif(20L, 1, 2),
    g = factor(rep(c("g1", "g2"), 10L))
  )
  pf <- mvgam(
    brms::bf(y | trials(n) ~ x + offset(log(e)) + (1 | g)),
    trend_formula = ~ AR(p = 1), data = d, family = binomial(),
    run_model = FALSE, validate = FALSE
  )
  preds <- insight::find_predictors(pf, effects = "all")
  expect_identical(preds$conditional, "x")
  expect_identical(preds$random, "g")
  expect_true(all(c("n", "e", "time", "series") %in% preds$grid))
  expect_identical(insight::find_random(pf)$random, "g")
  # A prediction grid is built from `find_variables()`
  expect_true(all(c("y", "x", "g", "n", "e", "time", "series") %in%
                    insight::find_variables(pf, flatten = TRUE)))
  # `terms()` holds the terms alone. `model.frame()` holds every column
  # the model uses.
  expect_identical(attr(terms(pf), "term.labels"), "x")
  expect_true(all(c("y", "x", "g", "n", "e") %in%
                    names(model.frame(pf))))
})

test_that("a distributional sub-formula's covariate is a term", {
  # `w` enters the model through `sigma` alone. A grid built without it
  # holds `sigma` at one value.
  set.seed(2)
  d <- data.frame(
    time = rep(1:10, 2),
    series = factor(rep(c("a", "b"), each = 10L)),
    y = rnorm(20L), x = rnorm(20L), w = rnorm(20L)
  )
  pf <- mvgam(
    brms::bf(y ~ x, sigma ~ w), trend_formula = ~ AR(p = 1),
    data = d, family = gaussian(), run_model = FALSE, validate = FALSE
  )
  expect_setequal(insight::find_predictors(pf)$conditional, c("x", "w"))
})

test_that("each response offers its own effects and the trend's", {
  # A panel over a covariate that a response's formula lacks draws a
  # flat line at that response's mean
  stub <- structure(
    list(
      formula = brms::bf(count ~ s(month), family = poisson()) +
        brms::bf(camera ~ deploy, family = bernoulli()),
      trend_formula = ~ temp
    ),
    class = "mvgam"
  )
  expect_equal(mvgam:::detect_conditional_effects(stub, resp = "count"),
               list("month", "temp"))
  expect_equal(mvgam:::detect_conditional_effects(stub, resp = "camera"),
               list("deploy", "temp"))
  expect_equal(mvgam:::detect_conditional_effects(stub),
               list("month", "deploy", "temp"))
})
