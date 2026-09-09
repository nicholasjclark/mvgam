# An observation formula that declares no terms at all.
#
# `y ~ -1` says the observation side carries nothing and every
# covariate belongs to the latent process. It is the idiom the VAR
# article is written in, and nothing else in this directory fits it:
# every other fixture puts at least an intercept on the observation
# side, so the branch that handles an empty design is never reached.
#
# mvgam cannot hand brms a design with no columns, so it substitutes
# a placeholder named `.mvgam_empty_obs`. Two things about it are
# visible to a user. What the column contributes to the linear
# predictor decides whether the formula was honoured, and the name
# itself reaches the frame, the term list and the printed summary
# unless every one of those filters it.
#
# The prefit settles all of the structure. `standata()` and
# `stancode()` need no posterior, and the questions here are about
# what the model was given rather than what the sampler found, so
# most of this file runs without draws.
#
# Two fits follow, and the pair is the point:
#
#   1. `trend_formula = ~ elev + AR(p = 1)`. The trend carries a
#      covariate and no intercept, so the placeholder is the only
#      constant in the model and is identified.
#   2. `trend_formula = ~ series + AR(p = 1)`. The trend carries a
#      per-series intercept, so its span already contains the
#      constant the placeholder adds. This is the VAR article's
#      shape, and whether the two are separately identified is what
#      the second fit answers.
#
# Run with:
#   TESTTHAT_MAX_FAILS=1000 Rscript -e "devtools::load_all(); \
#     testthat::test_file('tests/local/test-obs-empty-formula.R')"

suppressMessages({
  devtools::load_all(".", quiet = TRUE)
  library(posterior)
  library(testthat)
})

# Resolved from where this file is running rather than from what is
# already on disk. testthat sets the working directory to the test
# file's own, so asking whether `fixtures` exists picks the wrong
# branch on a clean tree and writes tests/local/tests/local/fixtures.
cache_path <- function(name) {
  dir <- if (dir.exists(file.path("tests", "local"))) {
    file.path("tests", "local", "fixtures")
  } else {
    "fixtures"
  }
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  file.path(dir, name)
}

set.seed(31L)

n_time <- 40L
series_names <- c("north", "east", "west")
n_series <- length(series_names)

# Declared out of alphabetical order, so a summary or a term list
# that sorts the axis is visible rather than coincidentally right.
dat <- data.frame(
  time = rep(seq_len(n_time), times = n_series),
  series = factor(rep(series_names, each = n_time),
                  levels = series_names),
  elev = round(stats::rnorm(n_time * n_series), 2L)
)
# Truth: a per-series level, a covariate slope on the trend side and
# a modest observation SD. The intercept the user did not ask for has
# nothing to explain that the series levels do not already.
series_level <- c(north = 1.0, east = 2.0, west = -0.5)
beta_elev <- 0.6
dat$y <- series_level[as.character(dat$series)] +
  beta_elev * dat$elev +
  stats::rnorm(n_time * n_series, 0, 0.5)

sim_truth <- list(series_level = series_level, beta_elev = beta_elev)


# -- What the prefit carries ------------------------------------------
#
# No sampling is involved in any of the blocks below, so each states a
# fact about the model that was built rather than about a posterior.

prefit <- mvgam(
  y ~ -1, trend_formula = ~ elev + AR(p = 1),
  data = dat, family = gaussian(), run_model = FALSE
)


test_that("the empty observation design is one substituted column", {
  # The substitution itself, stated plainly. mvgam cannot pass brms a
  # design with no columns, so it makes one, and the whole of what
  # follows turns on what that column carries.
  sd <- standata(prefit)
  expect_identical(as.integer(sd$K), 1L)
  expect_identical(dim(sd$X), c(nrow(dat), 1L))
  expect_identical(colnames(sd$X), ".mvgam_empty_obs")
})


test_that("a formula that declines an intercept is fitted without one", {
  # `y ~ -1` is the user saying the observation side contributes no
  # term, so the column mvgam substitutes has to contribute nothing.
  # It does so twice over: the column is zero, and the coefficient is
  # held at zero by a constant prior, so no free parameter enters the
  # program.
  sd <- standata(prefit)
  expect_identical(unique(as.numeric(sd$X)), 0)
  code <- stancode(prefit)
  lines <- strsplit(code, "\n")[[1L]]
  par_start <- grep("^parameters \\{", lines)
  par_end <- grep("^transformed parameters \\{", lines)
  expect_length(par_start, 1L)
  expect_length(par_end, 1L)
  par_block <- lines[seq(par_start, par_end)]
  expect_false(any(grepl("vector[K] b;", par_block, fixed = TRUE)))
  expect_true(any(grepl("b[1] = 0;", lines, fixed = TRUE)))
})


test_that("the covariate reached the side it was written on", {
  # The premise the rest of the file rests on. `elev` belongs to the
  # trend formula, so it has to appear in the trend design and not in
  # the observation one.
  sd <- standata(prefit)
  expect_identical(as.integer(sd$K_trend), 1L)
  expect_identical(colnames(sd$X_trend), "elev")
  expect_false("elev" %in% colnames(sd$X))
  # And the trend design carries no intercept of its own here, which
  # is what makes the placeholder the only constant in this model.
  expect_false(any(colnames(sd$X_trend) %in% c("Intercept", "b_Intercept")))
})


test_that("the axis is the one the frame declared", {
  # Declared `north, east, west`, which is not the sorted order, so a
  # resolver that rebuilt the axis with `sort(unique(...))` answers
  # differently here.
  ax <- mvgam_axes(prefit)
  expect_identical(as.character(ax$series$levels), series_names)
  expect_identical(as.integer(ax$series$n), n_series)
  expect_identical(as.numeric(ax$time$values), as.numeric(seq_len(n_time)))
})


test_that("the placeholder is not offered as a predictor", {
  # `find_predictors()` is what marginaleffects and the
  # rest of the easystats surface read to decide what a user may take
  # a slope or a contrast over. `.mvgam_empty_obs` is constant, so no
  # slope over it exists, and it is an internal name besides.
  preds <- unlist(insight::find_predictors(prefit, effects = "all"))
  expect_false(any(grepl("mvgam_empty_obs", preds, fixed = TRUE)))
  # The real predictor is still offered, so this is not asking for an
  # empty list.
  expect_true("elev" %in% preds)
})


test_that("the frame handed back is the frame that was given", {
  # The placeholder is written into the frame brms is given, so a
  # user reading `insight::get_data()` or `fit$data` must not find a
  # column they did not supply sitting beside their own.
  expect_setequal(names(prefit$data), names(dat))
})


# The stacked design, and why it belongs on a prefit
#
# The linear predictor is `X %*% b` plus the trend, and the trend's
# own mean is `X_trend %*% b_trend` read at that row's cell. So the
# matrix deciding whether the two sides are separately identified is
# the pair stacked side by side, and it is fully determined before
# any sampling.

# The mapping is checked against the frame below rather than being
# taken on trust, and the package's own resolver is what is driven, so
# a second derivation cannot drift from the one that runs.
stacked_design <- function(fit) {
  mvgam:::stacked_obs_trend_design(
    standata(fit), mvgam:::MVGAM_EMPTY_OBS_PLACEHOLDER
  )
}


test_that("the mapping onto the trend grid is the one Stan uses", {
  # The block below is only worth anything if this index is right, so
  # it is checked against the frame on a fit where the same covariate
  # sits on both sides: read through the map, the trend column has to
  # come back as the frame's own values. Putting `elev` on both sides
  # is what makes that comparison possible, and it is also the
  # pairing the likelihood cannot separate, so the notice is expected
  # here rather than silenced.
  expect_warning(
    pre_both <- mvgam(
      y ~ elev, trend_formula = ~ elev + AR(p = 1),
      data = dat, family = gaussian(), run_model = FALSE
    ),
    "not separately identified"
  )
  sd <- standata(pre_both)
  idx <- mvgam:::obs_rows_to_trend_rows(sd)
  expect_equal(as.numeric(sd$X_trend[idx, "elev"]), dat$elev)
  expect_equal(as.numeric(sd$X[, "elev"]), dat$elev)
})


# Which of the seven pairings is identified, and what each one is
# told. A column pinned at a constant carries no free parameter, so
# the placeholder is dropped before the rank is taken: what is asked
# is whether the free coefficients are separately identified.
#
# The three deficient rows are not refused. Fitting `y ~ 1` against
# `~ series + AR(p = 1)` three ways settles why: under the default
# priors each part carries a posterior SD of 4.26 while their sums
# hold at 0.11 to 0.36; under `std_normal()` on the trend
# coefficients the fit is proper at R-hat 1.02 and the parts are
# still displaced by the intercept; and `y ~ -1` recovers the levels.
# The sums, the fitted values and the forecasts are identified
# throughout, so a refusal would reject a model that samples and
# predicts. The notice names the columns instead.
identification_cases <- list(
  list(obs = y ~ -1, trend = ~ elev + AR(p = 1), deficient = FALSE),
  list(obs = y ~ -1, trend = ~ series + AR(p = 1), deficient = FALSE),
  list(obs = y ~ 1, trend = ~ elev + AR(p = 1), deficient = FALSE),
  list(obs = y ~ 1, trend = ~ series + AR(p = 1), deficient = TRUE),
  list(obs = y ~ 1, trend = ~ 1 + AR(p = 1), deficient = FALSE),
  list(obs = y ~ elev, trend = ~ series + AR(p = 1), deficient = TRUE),
  list(obs = y ~ elev, trend = ~ elev + AR(p = 1), deficient = TRUE)
)


test_that("a confounded pairing is named where it is built", {
  for (case in identification_cases) {
    build <- function() {
      mvgam(
        case$obs, trend_formula = case$trend, data = dat,
        family = gaussian(), run_model = FALSE
      )
    }
    if (case$deficient) {
      expect_warning(pre_i <- build(), "not separately identified")
    } else {
      pre_i <- build()
    }
    M <- stacked_design(pre_i)
    # `~ 1 + AR` carries no trend design at all, so there is nothing
    # to stack and nothing that could be confounded.
    if (is.null(M)) {
      expect_false(case$deficient)
      next
    }
    expect_identical(qr(M)$rank < ncol(M), case$deficient)
  }
})


# -- Fits -------------------------------------------------------------
#
# Two, and the pair is what separates a cosmetic name from a
# modelling consequence. The first puts no constant on the trend
# side; the second gives the trend a per-series intercept, whose span
# would contain any constant the observation side added. That second
# shape is the one the VAR article is written in, and it is where a
# free placeholder would have cost the fit its identification.

fit_cached <- function(name, trend) {
  path <- cache_path(paste0("val_mvgam_empty_obs_", name, ".rds"))
  if (file.exists(path)) {
    cat("[cache] Loading empty-obs fit:", name, "\n")
    return(readRDS(path))
  }
  cat("[fit  ] mvgam(y ~ -1, trend_formula =", deparse(trend), ")\n")
  fit <- mvgam(
    y ~ -1, trend_formula = trend, data = dat, family = gaussian(),
    chains = 2L, iter = 1000L, warmup = 500L,
    silent = 2, backend = "cmdstanr"
  )
  part <- paste0(path, ".part")
  saveRDS(fit, part)
  file.rename(part, path)
  fit
}

fit_free <- fit_cached("free", ~ elev + AR(p = 1))
fit_conf <- fit_cached("conf", ~ series + AR(p = 1))


test_that("the summary shows no name the user cannot look up", {
  # `summary()` used to print the placeholder as the sole
  # Population-Level Effect, with an estimate and an interval, while
  # neither `variables()` nor `tidy()` listed it. The pin is what
  # settles it: a coefficient held at a constant has no draws to
  # summarise.
  printed <- capture.output(summary(fit_free))
  expect_false(any(grepl(".mvgam_empty_obs", printed, fixed = TRUE)))
})


test_that("what the summary names, the accessors carry", {
  # The weaker half of the claim above, stated so that a fix which
  # merely renames the placeholder still has to make it reachable.
  printed <- capture.output(summary(fit_free))
  start <- grep("Population-Level Effects", printed)
  expect_length(start, 1L)
  rest <- printed[seq(start + 2L, length(printed))]
  block <- rest[seq_len(which(!nzchar(trimws(rest)))[1L] - 1L)]
  shown <- trimws(sub("^\\s*(\\S+).*$", "\\1", block))
  shown <- shown[nzchar(shown)]
  expect_gt(length(shown), 0L)
  v <- variables(fit_free)
  for (nm in shown) {
    expect_true(any(grepl(nm, v, fixed = TRUE)))
  }
})


test_that("the trend recovers what the frame was built from", {
  # The fit has to be worth asserting on before its output is worth
  # complaining about. `elev` sits on the trend side with a true
  # slope of 0.6.
  dm <- posterior::as_draws_matrix(fit_free)
  b <- grep("elev", colnames(dm), value = TRUE)
  expect_length(b, 1L)
  draws <- as.numeric(dm[, b])
  expect_gt(stats::quantile(draws, 0.975), sim_truth$beta_elev)
  expect_lt(stats::quantile(draws, 0.025), sim_truth$beta_elev)
})


test_that("prediction and forecasting run on an empty design", {
  # Nothing here is subtle. The point is that the placeholder does
  # not break the methods that read the observation design.
  ep <- posterior_epred(fit_free, ndraws = 20L)
  expect_identical(ncol(ep), nrow(dat))
  expect_true(all(is.finite(ep)))

  nd <- data.frame(
    time = rep(n_time + seq_len(4L), times = n_series),
    series = factor(rep(series_names, each = 4L), levels = series_names),
    elev = 0, y = NA_real_
  )
  fc <- forecast(fit_free, newdata = nd)
  expect_s3_class(fc, "mvgam_forecast")
  expect_length(fc$forecasts, n_series)
  expect_identical(names(fc$forecasts), series_names)
})


test_that("the constant mvgam adds is identified against the trend", {
  # The block the file exists for. `~ series + AR(p = 1)` gives the
  # trend one intercept per series, whose span already contains any
  # constant. A free placeholder beside them was an exact ridge: it
  # correlated at -1.000 with every trend intercept, each carried a
  # posterior SD near 233 against sums near 0.13, and the sampler
  # returned R-hat 2.14 at a bulk ESS of 2.63 from 1000 draws, with
  # the series levels reported at about 110 against a truth of 1.0,
  # 2.0 and -0.5.
  #
  # The pin removes the direction rather than shrinking it, so what
  # is asserted is its absence: no such coefficient is sampled, and
  # the trend intercepts are the only levels in the model.
  dm <- posterior::as_draws_matrix(fit_conf)
  expect_length(grep("empty_obs", colnames(dm), value = TRUE), 0L)
  tr <- grep("^b_series.*_trend$", colnames(dm), value = TRUE)
  expect_length(tr, n_series)
  # On a ridge each part wanders far beyond the spread of their
  # sums. With nothing to trade against, each level stands alone.
  for (nm in tr) {
    expect_lt(stats::sd(as.numeric(dm[, nm])), 1)
  }
})


test_that("a model mvgam agrees to build converges", {
  # The consequence of the block above, stated where a user would
  # meet it. A fit whose R-hat is 2.14 has told us nothing, and the
  # only sign of that was a column of the summary table.
  rh <- rhat(fit_conf)
  rh <- rh[is.finite(rh)]
  expect_lt(max(rh), 1.05)
})


test_that("the series levels are recovered, not their ridge", {
  # The same claim read on the quantity a user came for. Truth is
  # 1.0, 2.0 and -0.5. Differences between series stay identified
  # even on a ridge, so this asks for the levels themselves, which
  # are what the summary reports and what a reader would quote.
  dm <- posterior::as_draws_matrix(fit_conf)
  for (i in seq_along(series_names)) {
    nm <- paste0("b_series", series_names[i], "_trend")
    draws <- as.numeric(dm[, nm])
    truth <- sim_truth$series_level[[series_names[i]]]
    expect_lt(stats::quantile(draws, 0.025), truth)
    expect_gt(stats::quantile(draws, 0.975), truth)
    # And the interval has to be narrow enough to mean something.
    expect_lt(diff(stats::quantile(draws, c(0.025, 0.975))), 5)
  }
})


test_that("an argument nothing reads is refused on this fit too", {
  for (m in c("posterior_epred", "residuals", "predict", "summary")) {
    expect_error(do.call(m, list(fit_free, zzz_unknown = 1)), "zzz_unknown")
  }
})


test_that("a refit runs on a design mvgam wrote a column for", {
  # `mvgam()` strips the placeholder from the frame it stores, and
  # stamps it back wherever a frame reaches brms. A refit is such a
  # place: `kfold()` and `lfo_cv()` both hold their fold out by
  # refitting through `update()`. Without the stamp brms refuses the
  # refit for a variable the user never wrote and cannot supply, so
  # both methods are unavailable on any fit with an empty observation
  # formula.
  #
  # Both fits are driven because they differ in what the trend side
  # carries, and the placeholder belongs to the observation side.
  for (fit in list(fit_free, fit_conf)) {
    refit <- suppressWarnings(update(
      fit, newdata = mvgam:::mvgam_training_data(fit),
      chains = 1L, iter = 2L, silent = 2L, refresh = 0
    ))
    expect_s3_class(refit, "mvgam")
    # The refit is the same program, not merely a program.
    expect_identical(
      mvgam:::mvgam_normalise_stancode(stancode(refit)),
      mvgam:::mvgam_normalise_stancode(stancode(fit))
    )
    # Named separately because it is the half a whole-program check
    # would stop reporting if it were ever loosened, and it is the
    # half that decides whether the model is identified. `mvgam()`
    # filters the pin out of the table it stores, so a refit
    # inheriting that table samples free what the fit held at zero,
    # and against a trend intercept the two lie on an exact ridge.
    for (code in list(stancode(fit), stancode(refit))) {
      expect_match(as.character(code), "b[1] = 0;", fixed = TRUE)
    }
    # The column mvgam adds stays out of the frame the refit hands
    # back, so a user reading it still sees only their own columns.
    expect_false(
      mvgam:::MVGAM_EMPTY_OBS_PLACEHOLDER %in% names(refit$data)
    )
  }
})


test_that("cross-validation reaches a fit with no observation terms", {
  # `update()` above is the mechanism; this is what a user does with
  # it. Both methods hold a fold out by refitting, so neither was
  # available on any fit written `y ~ -1` -- which is the idiom the
  # VAR article uses. `lfo_cv()` takes the same route, so one of the
  # two is driven here and the other is left to the fixtures that
  # exercise it on a longer grid.
  kf <- suppressWarnings(kfold(fit_free, K = 2L, silent = 2L))
  expect_true(is.finite(kf$estimates["elpd_kfold", "Estimate"]))
})
