# CI tests for the insight and marginaleffects methods. The tests use
# stubs and unfitted models. None of them compiles Stan.

test_that("every S3 method mvgam registers is reachable", {
  # Each row names one (generic, class) pair
  registered <- list(
    c("find_formula", "mvgam"), c("find_response", "mvgam"),
    c("find_predictors", "mvgam"), c("get_data", "mvgam"),
    c("model_info", "mvgam"),
    c("get_predict", "mvgam"), c("get_coef", "mvgam"),
    c("get_vcov", "mvgam"), c("set_coef", "mvgam"),
    c("conditional_effects", "mvgam"),
    c("plot", "mvgam_conditional_effects"),
    c("print", "mvgam_conditional_effects")
  )
  for (pair in registered) {
    expect_false(
      is.null(getS3method(pair[1L], pair[2L], optional = TRUE)),
      label = paste0(pair[1L], ".", pair[2L], " registered")
    )
  }
})

test_that("mvgam re-exports the marginaleffects entry points", {
  exports <- getNamespaceExports("mvgam")
  for (nm in c("conditional_effects",
               "predictions", "avg_predictions", "slopes", "avg_slopes",
               "comparisons", "avg_comparisons", "datagrid",
               "hypotheses", "plot_predictions",
               "plot_slopes", "plot_comparisons")) {
    expect_true(nm %in% exports, label = paste0("mvgam re-exports ", nm))
  }
})

test_that("attaching mvgam names its class to marginaleffects", {
  withr::local_options(marginaleffects_model_classes = NULL)
  expect_message(mvgam:::.onAttach(), "Loading 'mvgam'")
  expect_identical(getOption("marginaleffects_model_classes"), "mvgam")
})

test_that("model_info describes each response of a multivariate fit", {
  # insight returns one description per response for a brms fit. A
  # lookup of `$family` alone gave every response the last family.
  d <- data.frame(time = 1:20, count = rpois(20, 4), seen = rbinom(20, 1, 0.5))
  pf <- mvgam(
    brms::bf(count ~ 1, family = poisson()) +
      brms::bf(seen ~ 1, family = bernoulli()) + brms::set_rescor(FALSE),
    trend_formula = ~ AR(p = 1), data = d, run_model = FALSE
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

test_that("get_predict, get_coef and set_coef check their inputs", {
  stub <- structure(list(marker = 42L, trend_formula = NULL),
                    class = "mvgam")
  expect_error(
    marginaleffects::get_predict(stub, newdata = data.frame(x = 1),
                                 type = "bogus"),
    "Must be element of set"
  )
  expect_error(
    marginaleffects::get_coef(stub, trend_effects = TRUE),
    "needs a model fitted with a 'trend_formula'"
  )
  # The draws carry the uncertainty. A coefficient vector from
  # marginaleffects leaves the model unchanged.
  expect_identical(marginaleffects::set_coef(stub, coefs = c(a = 1)), stub)
})

test_that("get_vcov warns only when given a vcov estimator", {
  stub <- structure(list(), class = "mvgam")
  # marginaleffects passes `vcov = TRUE` on every call. A warning there
  # would reach every user of `predictions()`.
  expect_null(expect_silent(marginaleffects::get_vcov(stub)))
  expect_silent(marginaleffects::get_vcov(stub, vcov = TRUE))
  expect_silent(marginaleffects::get_vcov(stub, vcov = FALSE))
  # A named estimator warns on every call. The second call in a
  # session warns again.
  for (i in 1:2) {
    expect_warning(marginaleffects::get_vcov(stub, vcov = "HC3"),
                   "posterior draws")
  }
})

test_that("find_predictors takes terms from both submodels", {
  # A covariate of either submodel is a term. Nobody takes a slope over
  # an occasion number or a series index. Listing the axis columns as
  # terms put a grouping into `avg_slopes()`.
  stub <- structure(
    list(
      formula = y ~ x1,
      trend_formula = trend_y ~ x2 - 1,
      trend_metadata = list(axes = list(vars = list(
        time_var = "time", series_var = "series"
      )))
    ),
    class = "mvgam"
  )
  expect_setequal(insight::find_predictors(stub)$conditional, c("x1", "x2"))
})

test_that("non-linear sub-formulas give terms and a grouping", {
  # The top-level right-hand side of a non-linear formula names the
  # parameters `a` and `b`. Their sub-formulas hold the covariates.
  stub <- structure(
    list(
      formula = brms::bf(
        y ~ a + b * env,
        a ~ trait1 + (1 | species),
        b ~ trait1 + (1 | species),
        nl = TRUE
      ),
      trend_formula = NULL, trend_metadata = NULL
    ),
    class = "mvgam"
  )
  expect_setequal(insight::find_predictors(stub, flatten = TRUE),
                  c("env", "trait1"))
  # `species` groups the varying terms. Its levels are exchangeable
  # draws. `find_random()` reports it as a grouping.
  expect_identical(insight::find_random(stub)$random, "species")
  # Neither parameter name splits off as a panel variable
  expect_equal(mvgam:::detect_conditional_effects(stub),
               list("env", "trait1"))
})

test_that("find_predictors lists a jsdgam fit's unit and species", {
  # A grid builder needs the unit and species columns jsdgam() recorded
  stub <- structure(
    list(
      formula = y ~ env, trend_formula = NULL, trend_metadata = NULL,
      data = data.frame(y = 1:3, env = 1:3, species = letters[1:3],
                        site = 1:3),
      jsdgam_args = list(unit = "site", species = "species")
    ),
    class = "mvgam"
  )
  expect_true(all(c("env", "species", "site") %in%
                    insight::find_predictors(stub, flatten = TRUE)))
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
    run_model = FALSE
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
    data = d, family = gaussian(), run_model = FALSE
  )
  expect_setequal(insight::find_predictors(pf)$conditional, c("x", "w"))
})

test_that("an offset is never offered as a conditional effect", {
  # The formulas pair the offset with a linear term, a smooth and a
  # trend-side term in turn
  stubs <- list(
    y ~ env + offset(log(n)),
    y ~ s(env) + offset(log(n))
  )
  for (f in stubs) {
    stub <- structure(list(formula = f, trend_formula = NULL),
                      class = "mvgam")
    expect_equal(mvgam:::detect_conditional_effects(stub), list("env"))
  }
  trend_stub <- structure(
    list(formula = y ~ env, trend_formula = ~ z + offset(log(n))),
    class = "mvgam"
  )
  expect_equal(mvgam:::detect_conditional_effects(trend_stub),
               list("env", "z"))
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

test_that("a smooth's covariates come from its call", {
  # A smooth names its covariates as unnamed arguments and its grouping
  # as `by`. The other arguments are settings. Parsing the call without
  # evaluating it gives `by = interaction(a, b)` its two columns. A
  # three-covariate smooth gives its three pairwise margins.
  stub <- structure(
    list(
      formula = y ~ s(x, k = 5) + t2(x, z, w) +
        s(x, by = interaction(a, b)),
      trend_formula = NULL
    ),
    class = "mvgam"
  )
  expect_equal(
    mvgam:::detect_conditional_effects(stub),
    list("x", c("x", "z"), c("x", "w"), c("z", "w"), c("x", "a", "b"))
  )
})

test_that("an effect varying by series puts the series second", {
  # The first variable of a grouping is the x axis of its panel. A
  # slope written as `series:x` drew the series on the x axis.
  stub <- structure(
    list(formula = y ~ series + series:x + s(z, by = series),
         trend_formula = NULL),
    class = "mvgam"
  )
  detected <- mvgam:::detect_conditional_effects(stub)
  expect_equal(detected, list("series", c("x", "series"), c("z", "series")))
  # A named effect takes the series grouping the model detects. Without
  # it, `effects = "x"` drew the slope at the reference series.
  expect_identical(mvgam:::with_series_grouping("x", detected, "series"),
                   c("x", "series"))
  expect_identical(mvgam:::with_series_grouping("w", detected, "series"),
                   "w")
})

# A stub carrying the axis record that `resolve_series_arg()` uses. A
# hierarchical fit records its series on the axis and holds no `series`
# column, which `with_series_col = FALSE` reproduces.
series_stub <- function(levels = c("s1", "s2", "s3"),
                        with_series_col = TRUE) {
  dat <- data.frame(y = seq_along(levels))
  if (with_series_col) {
    dat$series <- factor(levels, levels = levels)
  }
  structure(
    list(
      data = dat,
      trend_metadata = list(
        axes = list(
          series = list(levels = levels, source = "explicit",
                        n = length(levels), groups = NULL),
          time = NULL
        ),
        variables = list(series_var = "series")
      )
    ),
    class = c("mvgam", "brmsfit")
  )
}

test_that("resolve_series_arg resolves each accepted form", {
  resolved <- function(series, object = series_stub()) {
    out <- mvgam:::resolve_series_arg(series, object)
    c(out$kind, out$level)
  }
  expect_identical(resolved(NULL), c("none", NA))
  expect_identical(resolved("all"), c("all", NA))
  expect_identical(resolved("s2"), c("one", "s2"))
  expect_identical(resolved(3L), c("one", "s3"))
  # The levels come from the axis record. A frame without a series
  # column resolves the same level.
  expect_identical(resolved("s2", series_stub(with_series_col = FALSE)),
                   c("one", "s2"))
})

test_that("resolve_series_arg refuses what names no single series", {
  stub <- series_stub()
  expect_error(mvgam:::resolve_series_arg("not_a_series", stub),
               "not one of the model's series levels")
  expect_error(mvgam:::resolve_series_arg(99L, stub),
               "'series' index is out of range")
  expect_error(mvgam:::resolve_series_arg(c("s1", "s2"), stub),
               "NULL, 'all', a series name")
  empty <- structure(
    list(
      data = data.frame(y = 1:3),
      trend_metadata = list(axes = list(series = NULL, time = NULL))
    ),
    class = c("mvgam", "brmsfit")
  )
  expect_error(mvgam:::resolve_series_arg("s1", empty),
               "empty series axis")
})

test_that("conditional_effects refuses `...` that sets its own arguments", {
  # The guard runs before any prediction. A stub is enough to reach it.
  stub <- structure(
    list(
      formula = y ~ x,
      data = data.frame(y = 1, x = 1, series = factor("s1"))
    ),
    class = "mvgam"
  )
  for (kw in c("condition", "draw", "newdata")) {
    expect_error(
      do.call(conditional_effects, c(list(stub, effects = "x"),
                                      setNames(list("hijack"), kw))),
      paste0("Cannot pass '", kw, "'")
    )
  }
})

test_that("as.data.frame stacks one and several responses' panels", {
  # Each panel is a ggplot whose `$data` holds the marginaleffects grid
  fake_ggplot <- function(effect_var, n = 3L) {
    d <- data.frame(
      rowid     = seq_len(n),
      estimate  = seq(0, 1, length.out = n),
      conf.low  = seq(-1, 0, length.out = n),
      conf.high = seq(1, 2, length.out = n),
      grid_col  = seq(-2, 2, length.out = n)
    )
    d[[effect_var]] <- seq_len(n)
    structure(list(data = d), class = c("ggplot", "list"))
  }
  uni <- structure(
    list(env = fake_ggplot("env"), rain = fake_ggplot("rain")),
    class = "mvgam_conditional_effects"
  )
  df_u <- as.data.frame(uni)
  expect_true(all(c("effect", "estimate__", "lower__", "upper__")
                    %in% colnames(df_u)))
  expect_false("rowid" %in% colnames(df_u))
  expect_identical(df_u$effect, rep(c("env", "rain"), each = 3L))
  expect_identical(df_u$estimate__, rep(c(0, 0.5, 1), 2L))

  # A multivariate fit wraps one such list per response
  mv <- structure(
    list(count = uni, pa = uni),
    class = "mvgam_conditional_effects",
    mv_wrapper = TRUE
  )
  df_m <- as.data.frame(mv)
  expect_identical(df_m$resp, rep(c("count", "pa"), each = 6L))
  expect_identical(df_m$effect, rep(df_u$effect, 2L))
})
