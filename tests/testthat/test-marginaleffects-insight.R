# Fast CI-safe tests for the insight + marginaleffects S3 surface.
# Signature parity, NAMESPACE wiring, validation paths — no fitted
# objects required.

test_that("insight S3 methods are registered on mvgam", {
  for (g in c("find_formula", "find_response", "find_predictors",
              "get_data", "model_info")) {
    expect_true(
      !is.null(getS3method(g, "mvgam", optional = TRUE)),
      label = paste("insight::", g, ".mvgam registered")
    )
  }
})

test_that("model_info describes each response of a multivariate fit", {
  # insight answers once per response for a brms fit. Reading
  # `$family` alone described every arm as the last one, and three
  # caught errors turned a failed lookup into a blank family.
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
               "not a response of this model")
})

test_that("marginaleffects S3 methods are registered on mvgam", {
  for (g in c("get_predict", "get_coef", "get_vcov", "set_coef")) {
    expect_true(
      !is.null(getS3method(g, "mvgam", optional = TRUE)),
      label = paste("marginaleffects::", g, ".mvgam registered")
    )
  }
})

test_that(".onAttach sets marginaleffects_model_classes", {
  # `loadNamespace` does not always trigger .onAttach, and the installed
  # package ships no R source, so deparse the function body directly.
  # This does not depend on load order or on source files being present.
  body_src <- deparse(mvgam:::.onAttach)
  expect_true(any(grepl("marginaleffects_model_classes", body_src)))
})

test_that("model.frame.mvgam signature has trend_effects after formula", {
  fmls <- names(formals(getS3method("model.frame", "mvgam")))
  expect_equal(fmls[1L], "formula")
  expect_true("trend_effects" %in% fmls)
})

test_that("get_predict.mvgam validates type via checkmate::assert_choice", {
  stub <- structure(list(), class = "mvgam")
  # Invalid type triggers checkmate before any model machinery runs.
  expect_error(
    marginaleffects::get_predict(
      stub,
      newdata = data.frame(x = 1),
      type = "bogus"
    ),
    regexp = "type"
  )
})

test_that("get_coef.mvgam errors on trend_effects when no trend formula", {
  stub <- structure(
    list(trend_formula = NULL),
    class = "mvgam"
  )
  expect_error(
    marginaleffects::get_coef(stub, trend_effects = TRUE),
    regexp = "trend"
  )
})

test_that("get_vcov.mvgam returns NULL", {
  stub <- structure(list(), class = "mvgam")
  expect_null(marginaleffects::get_vcov(stub))
})

test_that("get_vcov.mvgam speaks only for a vcov estimator", {
  stub <- structure(list(), class = "mvgam")
  # marginaleffects passes `vcov = TRUE` on every call it makes, so
  # a notice there would reach every user of `predictions()`.
  expect_silent(marginaleffects::get_vcov(stub, vcov = TRUE))
  expect_silent(marginaleffects::get_vcov(stub, vcov = FALSE))
  # A named estimator is a request mvgam cannot honour, and it is
  # raised per call rather than once per session, so a second call
  # in the same session still reports it.
  expect_warning(
    marginaleffects::get_vcov(stub, vcov = "HC3"),
    regexp = "posterior draws"
  )
  expect_warning(
    marginaleffects::get_vcov(stub, vcov = "HC3"),
    regexp = "posterior draws"
  )
})

test_that("set_coef.mvgam is a no-op pass-through", {
  stub <- structure(list(marker = 42L), class = "mvgam")
  out <- marginaleffects::set_coef(stub, coefs = c(a = 1))
  expect_identical(out$marker, 42L)
})

test_that("find_predictors reads both submodels and leaves the axis out", {
  # A covariate of either submodel is a term. The axis columns are
  # not: nothing takes a slope over an occasion number or a series
  # index, and offering them is what put a grouping into
  # `avg_slopes()`. They stay in the variable list, which is what a
  # prediction grid is built from.
  stub <- structure(
    list(
      formula = y ~ x1,
      trend_formula = trend_y ~ x2 - 1,
      trend_metadata = list(variables = list(time_var = "time",
                                              series_var = "series",
                                              gr_var = NA_character_,
                                              subgr_var = NA_character_))
    ),
    class = "mvgam"
  )
  preds <- insight::find_predictors(stub)$conditional
  expect_true(all(c("x1", "x2") %in% preds))
  expect_false(any(c("time", "series") %in% preds))
})

test_that("find_predictors.mvgam walks nl sub-formulas and jsdgam aliases", {
  # nl: trait1 in a sub-formula must surface alongside env (top-level).
  obs_nl <- brms::bf(
    y  ~ a + b * env,
    a  ~ trait1 + (1 | species),
    b  ~ trait1 + (1 | species),
    nl = TRUE
  )
  stub_nl <- structure(
    list(
      formula = obs_nl, trend_formula = NULL,
      trend_metadata = NULL, model_data = NULL
    ),
    class = "mvgam"
  )
  preds <- insight::find_predictors(stub_nl, flatten = TRUE)
  expect_true("env" %in% preds)
  expect_true("trait1" %in% preds)
  expect_false("a" %in% preds)
  expect_false("b" %in% preds)
  # `species` groups the sub-formulas' varying terms. It is the
  # grouping, not a term a slope can be taken over, so it answers
  # under `random`.
  expect_false("species" %in% preds)
  expect_identical(insight::find_random(stub_nl)$random, "species")

  # jsdgam: the user's species / unit column names persist via
  # attr(model_data, "prepped_trend_model"). find_predictors must
  # surface them so downstream tools building newdata grids do
  # not have to know the aliasing.
  stub_jsdgam <- structure(
    list(
      formula = y ~ env, trend_formula = NULL,
      trend_metadata = NULL,
      model_data = structure(
        data.frame(y = 1:3, env = 1:3, species = letters[1:3],
                    site = 1:3),
        prepped_trend_model = list(unit = "site", species = "species")
      )
    ),
    class = "mvgam"
  )
  preds_j <- insight::find_predictors(stub_jsdgam, flatten = TRUE)
  expect_true("species" %in% preds_j)
  expect_true("site" %in% preds_j)
})

test_that("the term list splits one formula the way its readers ask", {
  # One parse answers `find_predictors()`, `find_random()`,
  # `find_variables()`, `terms()` and the prediction grid. The split
  # decides whether a consumer offers a slope over a column: a
  # grouping, a binomial denominator and an offset each name a column
  # the model reads without anyone taking a slope over it, and each
  # has to stay addressable in the grid all the same.
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
  # The denominator, the offset and the axis are addressable and are
  # not terms.
  expect_true(all(c("n", "e", "time", "series") %in% preds$grid))
  expect_false(any(c("n", "e", "g", "time", "series") %in%
                     preds$conditional))
  expect_identical(insight::find_random(pf)$random, "g")

  # `find_variables()` is what a prediction grid is built from, so
  # every column above has to survive into it.
  vars <- insight::find_variables(pf, flatten = TRUE)
  expect_true(all(c("y", "x", "g", "n", "e", "time", "series") %in%
                    vars))

  # `terms()` carries the terms and none of the rest, and
  # `model.frame()` carries every column the model reads.
  expect_identical(attr(terms(pf), "term.labels"), "x")
  expect_true(all(c("y", "x", "g", "n", "e") %in%
                    names(model.frame(pf))))
})

test_that("a distributional sub-formula's covariate is a term", {
  # A covariate reaching the linear predictor only through a `dpar`
  # appears in no top-level right-hand side, and a grid built without
  # it holds the parameter it governs at one value.
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

test_that("detect_conditional_effects recurses into nl sub-formulas", {
  # Reason: bf(..., nl = TRUE) hides the user-relevant covariates
  # inside per-nlpar pforms; the top-level RHS only enumerates the
  # nlpar names themselves (`a + b * env`). Naive parsing of the
  # top-level formula would return c("a", "b", "env") and split
  # the `b * env` interaction into a nonsense (b, env) grouping.
  # The recursion pulls trait1 etc. out of each pform and filters
  # nlpar tokens from the split.
  obs_nl <- brms::bf(
    y  ~ a + b * env,
    a  ~ trait1 + (1 | species),
    b  ~ trait1 + (1 | species),
    nl = TRUE
  )
  stub <- structure(
    list(formula = obs_nl, trend_formula = NULL),
    class = "mvgam"
  )
  cond <- mvgam:::detect_conditional_effects(stub)
  flat <- unlist(cond, use.names = FALSE)
  # env comes off the top-level right-hand side and trait1 off a
  # sub-formula, so both are terms a reader can take a slope over.
  expect_true("env" %in% flat)
  expect_true("trait1" %in% flat)
  expect_false("a" %in% flat)
  expect_false("b" %in% flat)
  # `species` groups the sub-formulas' varying terms. Its levels are
  # exchangeable draws from a distribution whose scale the model
  # estimates, so a panel over them draws shrunk deviations as though
  # they were a population contrast. It answers under `find_random()`.
  expect_false("species" %in% flat)
  expect_identical(insight::find_random(stub)$random, "species")
  # No grouping should contain a nlpar after filtering.
  for (g in cond) {
    expect_false(any(g %in% c("a", "b")))
  }
})

test_that("detect_conditional_effects leaves linear formulas alone", {
  # Regression guard: the nl branch must not leak into the plain
  # linear path. A bare y ~ env model should produce just c("env").
  stub <- structure(
    list(formula = y ~ env, trend_formula = NULL),
    class = "mvgam"
  )
  cond <- mvgam:::detect_conditional_effects(stub)
  expect_equal(cond, list("env"))
})

test_that("a smooth's covariates are read off its call", {
  # A smooth names its covariates as unnamed arguments and its
  # grouping as `by`; every other argument is a setting. The
  # covariates and the `by` are read off the call rather than by
  # evaluating it, so `by = interaction(a, b)` answers with the two
  # columns it names instead of a column named after the call, and a
  # three-covariate smooth answers with its three pairwise margins.
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

test_that("conditional_effects.mvgam is registered and re-exports the generic", {
  expect_true(
    !is.null(getS3method("conditional_effects", "mvgam", optional = TRUE))
  )
  exports <- getNamespaceExports("mvgam")
  expect_true("conditional_effects" %in% exports)
})

test_that("plot/print methods on mvgam_conditional_effects are registered", {
  expect_true(
    !is.null(getS3method("plot", "mvgam_conditional_effects",
                         optional = TRUE))
  )
  expect_true(
    !is.null(getS3method("print", "mvgam_conditional_effects",
                         optional = TRUE))
  )
})

test_that("conditional_effects.mvgam signature has expected args", {
  fmls <- names(formals(getS3method("conditional_effects", "mvgam")))
  expect_true("x" %in% fmls)
  expect_true("effects" %in% fmls)
  expect_true("type" %in% fmls)
  expect_true("process_error" %in% fmls)
  expect_true("series" %in% fmls)
})

# Build a minimal mvgam-class stub carrying just the slots
# `resolve_series_arg` reads. Keeps these tests fast (~1 ms each).
series_stub <- function(levels = c("s1", "s2", "s3"),
                        with_series_col = TRUE) {
  dat <- data.frame(y = seq_along(levels))
  if (with_series_col) {
    dat$series <- factor(levels, levels = levels)
  }
  structure(list(data = dat), class = "mvgam")
}

test_that("resolve_series_arg(NULL) returns kind = 'none'", {
  out <- mvgam:::resolve_series_arg(NULL, series_stub())
  expect_equal(out$kind, "none")
  expect_true(is.na(out$level))
})

test_that("resolve_series_arg('all') returns kind = 'all'", {
  out <- mvgam:::resolve_series_arg("all", series_stub())
  expect_equal(out$kind, "all")
  expect_true(is.na(out$level))
})

test_that("resolve_series_arg(<chr>) resolves to a single level", {
  out <- mvgam:::resolve_series_arg("s2", series_stub())
  expect_equal(out$kind, "one")
  expect_equal(out$level, "s2")
})

test_that("resolve_series_arg(<int>) indexes into levels()", {
  out <- mvgam:::resolve_series_arg(3L, series_stub())
  expect_equal(out$kind, "one")
  expect_equal(out$level, "s3")
})

test_that("resolve_series_arg errors on unknown level", {
  expect_error(
    mvgam:::resolve_series_arg("not_a_series", series_stub()),
    regexp = "not one of the model's series levels"
  )
})

test_that("resolve_series_arg errors on out-of-range index", {
  # The handler wraps the bound check in `insight::format_error` so
  # the user sees the requested index, the valid range, and the
  # available level names. Match the stable leading line.
  expect_error(
    mvgam:::resolve_series_arg(99L, series_stub()),
    regexp = "'series' index is out of range"
  )
})

test_that("resolve_series_arg errors on length-N vector", {
  expect_error(
    mvgam:::resolve_series_arg(c("s1", "s2"), series_stub()),
    regexp = "NULL, 'all', a series name"
  )
})

test_that("resolve_series_arg errors when data has no series column", {
  expect_error(
    mvgam:::resolve_series_arg(
      "s1",
      series_stub(with_series_col = FALSE)
    ),
    regexp = "no 'series' column"
  )
})

test_that("conditional_effects rejects clashes between `...` and reserved args", {
  # A stub is enough — the collision guard fires before any
  # plot_predictions / posterior_epred machinery runs.
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
      regexp = "Cannot pass"
    )
  }
})

test_that("as.data.frame.mvgam_conditional_effects handles both shapes", {
  # Univariate shape: list of ggplots keyed by effect. Simulate the
  # marginaleffects-style ggplot payload with a $data slot.
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
  expect_true(is.data.frame(df_u))
  expect_true(all(c("effect", "estimate__", "lower__", "upper__")
                    %in% colnames(df_u)))
  expect_false("rowid" %in% colnames(df_u))
  expect_setequal(unique(df_u$effect), c("env", "rain"))

  # Multivariate shape: wrapper tagged mv_wrapper, elements are
  # themselves mvgam_conditional_effects lists.
  mv <- structure(
    list(count = uni, pa = uni),
    class = "mvgam_conditional_effects"
  )
  attr(mv, "mv_wrapper") <- TRUE
  df_m <- as.data.frame(mv)
  expect_true("resp" %in% colnames(df_m))
  expect_setequal(unique(df_m$resp), c("count", "pa"))
  expect_setequal(unique(df_m$effect), c("env", "rain"))
  # Row count: two effects x n=3 grid x two arms = 12.
  expect_equal(nrow(df_m), 12L)
})


test_that("re-exports of marginaleffects entry points are wired", {
  exports <- getNamespaceExports("mvgam")
  for (nm in c("predictions", "avg_predictions", "slopes", "avg_slopes",
               "comparisons", "avg_comparisons", "datagrid",
               "hypotheses", "plot_predictions",
               "plot_slopes", "plot_comparisons")) {
    expect_true(
      nm %in% exports,
      label = paste0("mvgam re-exports ", nm)
    )
  }
})


test_that("an offset is not offered as a conditional effect", {
  # `stats::terms()` files an offset under the "offset" attribute
  # rather than in "term.labels", so it never reaches the effect
  # list. This guards that the answer stays right even if `terms()`
  # ever starts putting offsets in "term.labels" instead.
  stub <- structure(
    list(formula = y ~ env + offset(log(n)), trend_formula = NULL),
    class = "mvgam"
  )
  expect_equal(mvgam:::detect_conditional_effects(stub), list("env"))

  # brms reaches the same set from the same formula.
  expect_equal(
    unlist(brms:::get_all_effects(
      brms::brmsterms(brms::bf(y ~ env + offset(log(n))))
    )),
    "env"
  )

  # An offset alongside a smooth, and an offset on the trend side.
  smooth_stub <- structure(
    list(formula = y ~ s(env) + offset(log(n)), trend_formula = NULL),
    class = "mvgam"
  )
  expect_equal(mvgam:::detect_conditional_effects(smooth_stub),
               list("env"))
  trend_stub <- structure(
    list(formula = y ~ env, trend_formula = ~ z + offset(log(n))),
    class = "mvgam"
  )
  expect_equal(mvgam:::detect_conditional_effects(trend_stub),
               list("env", "z"))
})
