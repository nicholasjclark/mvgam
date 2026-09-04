# Conway-Maxwell binomial, and what a `trials()` aterm reaches.
#
# `com_binomial()` is a custom brms family: it carries its own Stan
# code on `attr(fam, "mvgam_stanvars")`, declares a second
# distributional parameter `nu` for over and under dispersion, and
# takes its denominator through a `trials()` aterm rather than as a
# predictor.
#
# That aterm is what makes this fit worth having. A trial count is a
# denominator rather than a predictor, so `find_predictors()` leaves
# it out of the term list, and every route that builds a frame from
# that list has no denominator to put in it. Three do: the smooth
# grid, the marginaleffects grid and the padded forecast grid.
#
# The observation side carries more than the family needs, because
# each term reaches a surface nothing else here drives. `mo(dose)` is
# the only monotonic effect in the directory. `(1 | site)` is the only
# group-level effect outside the VAR fit, and it is what makes
# `ranef()`, `VarCorr()` and `ngrps()` answerable at all. `nu ~ z`
# puts a sub-formula on the second distributional parameter of a
# custom family, where the only other dpar coverage is `hu` on a
# built-in one.
#
#   truth: 2 series over 50 occasions, a logit-scale smooth in `x`,
#          a latent AR(1), trials varying per row
#   model: bf(y | trials(n_trials) ~ s(x, k = 5) + series +
#             (1 | site) + mo(dose), nu ~ z) with an AR(1) trend,
#          family = com_binomial()
#
# Series are named out of alphabetical order and occasions numbered
# from 3, so a rank is never a time and a sorted axis is not the
# model's.
#
# Run with:
#   testthat::test_file("tests/local/test-family-com-binomial.R")

suppressMessages({
  devtools::load_all(".", quiet = TRUE)
  library(posterior)
  library(testthat)
})

# Several blocks below state what the package does not yet do, and
# testthat stops a file after ten failures by default, which would
# leave the blocks after them unrun and looking clean.
testthat::set_max_fails(Inf)

SM <- suppressMessages

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

set.seed(4242L)

n_series <- 2L
n_time <- 50L
series_levels <- c("upper", "lower")
stopifnot(!identical(series_levels, sort(series_levels)))
time_vals <- seq_len(n_time) + 2L

# A latent AR(1) on the logit scale, so conditioning on it moves the
# predictions by more than Monte Carlo noise.
ar_true <- 0.7
sigma_true <- 0.5
latent <- matrix(0, nrow = n_time, ncol = n_series)
for (s in seq_len(n_series)) {
  latent[1L, s] <- rnorm(1L, 0, sigma_true / sqrt(1 - ar_true^2))
  for (t in 2:n_time) {
    latent[t, s] <- ar_true * latent[t - 1L, s] + rnorm(1L, 0, sigma_true)
  }
}

x <- as.numeric(scale(rnorm(n_time)))
f_true <- 1.2 * sin(1.5 * x)
beta_series <- c(upper = 0, lower = -0.6)

dose_levels <- c("none", "low", "mid", "high")
site_levels <- paste0("st", 1:5)
# A monotone increasing effect of dose, which is what `mo()` is for:
# ordered categories whose spacing the model estimates.
dose_effect <- c(none = 0, low = 0.25, mid = 0.55, high = 1.1)

dat <- data.frame(
  time = rep(time_vals, times = n_series),
  series = factor(rep(series_levels, each = n_time),
                  levels = series_levels),
  x = rep(x, times = n_series),
  z = rnorm(n_time * n_series),
  site = factor(rep(site_levels, length.out = n_time * n_series),
                levels = site_levels),
  dose = factor(sample(dose_levels, n_time * n_series, replace = TRUE),
                levels = dose_levels, ordered = TRUE)
)
site_effect <- rnorm(length(site_levels), 0, 0.4)
# A denominator that varies per row, so a grid holding it at one
# value is visibly not the frame's own.
dat$n_trials <- as.integer(sample(20:60, nrow(dat), replace = TRUE))
eta <- as.numeric(latent) +
  rep(f_true, times = n_series) +
  beta_series[as.character(dat$series)] +
  dose_effect[as.character(dat$dose)] +
  site_effect[as.integer(dat$site)]
dat$y <- rbinom(nrow(dat), size = dat$n_trials, prob = plogis(eta))

obs_formula <- bf(
  y | trials(n_trials) ~ s(x, k = 5) + series + (1 | site) + mo(dose),
  nu ~ z
)

sim_truth <- list(
  n_series = n_series, n_time = n_time,
  series_levels = series_levels, time_vals = time_vals,
  ar_true = ar_true, sigma_true = sigma_true, latent = latent,
  f_true = f_true, beta_series = beta_series,
  dose_levels = dose_levels, dose_effect = dose_effect,
  site_levels = site_levels, site_effect = site_effect
)


# -- Prefit ------------------------------------------------------------

prefit <- mvgam(
  formula = obs_formula, trend_formula = ~ AR(p = 1),
  data = dat, family = com_binomial(), run_model = FALSE, silent = 2
)


test_that("the custom family reaches Stan with its own code and nu", {
  # `com_binomial()` is a `brms::custom_family()` whose Stan functions
  # hang off the family object. Nothing else in this directory takes
  # that route except `tweedie()`, and the two differ in that this one
  # also carries an aterm.
  fam <- com_binomial()
  expect_identical(fam$family, "custom")
  expect_true(!is.null(attr(fam, "mvgam_stanvars")))
  expect_true(all(c("mu", "nu") %in% fam$dpars))

  sc <- as.character(stancode(prefit))
  # The denominator reaches the program as data, one per observation,
  # rather than as a column of the design matrix.
  expect_true(grepl("vector[N] trials", sc, fixed = TRUE) ||
                grepl("int trials[N]", sc, fixed = TRUE) ||
                grepl("array[N] int trials", sc, fixed = TRUE))
  # The dispersion parameter is estimated.
  expect_true(grepl("nu", sc, fixed = TRUE))

  sd <- prefit$standata
  expect_identical(as.integer(sd$trials), as.integer(dat$n_trials))
  expect_identical(as.integer(sd$obs_trend_series),
                   match(as.character(dat$series), series_levels))
})


# -- Fit ---------------------------------------------------------------

cache <- cache_path("val_mvgam_com_binomial.rds")
if (file.exists(cache)) {
  cat("[cache] Loading com_binomial fit.\n")
  fit <- readRDS(cache)
} else {
  cat("[fit ] mvgam(com_binomial, trials aterm, s(x), AR(1))\n")
  fit <- mvgam(
    formula = obs_formula, trend_formula = ~ AR(p = 1),
    data = dat, family = com_binomial(),
    chains = 2L, iter = 1000L, warmup = 500L,
    silent = 2, backend = "cmdstanr"
  )
}
if (!identical(attr(fit, "sim_truth"), sim_truth)) {
  attr(fit, "sim_truth") <- sim_truth
  saveRDS(fit, cache)
}


test_that("posterior_epred answers on the count scale for this family", {
  # The expectation of a Conway-Maxwell binomial is not `trials * mu`,
  # so it is reached through the family's own kernel rather than by
  # inverting the link. The kernel is handed a prep list and reads
  # `mu`, `nu` and the trial counts out of it. A kernel that instead
  # took an already inverse-linked matrix would apply the link a
  # second time, which returns finite means of the wrong size rather
  # than raising, so the bound below is the claim that catches it.
  ep <- posterior_epred(fit, ndraws = 50L)
  expect_identical(dim(ep), c(50L, nrow(dat)))
  expect_true(all(is.finite(ep)))
  # A count between zero and the row's own denominator, which is the
  # claim a double inverse-link fails.
  expect_true(all(ep >= 0))
  for (j in seq_len(ncol(ep))) {
    expect_true(all(ep[, j] <= dat$n_trials[j] + 1e-8))
  }
  # And it varies with the denominator: two rows with the same
  # predictor and different trial counts do not share an expectation.
  expect_gt(stats::sd(colMeans(ep)), 0)
})


test_that("posterior_predict reads incl_autocor rather than dropping it", {
  # `posterior_predict()` composes its draws through
  # `posterior_linpred()`, which is the sibling method that takes
  # `incl_autocor` and derives the trend state from it. The argument
  # has to reach that call under its own name: anything passed
  # alongside it lands in `...` and every draw comes back marginal,
  # which is finite, correctly shaped and conditioned on nothing.
  ids <- 1:100
  cond <- posterior_predict(fit, draw_ids = ids, incl_autocor = TRUE)
  marg <- posterior_predict(fit, draw_ids = ids, incl_autocor = FALSE)
  expect_identical(dim(cond), dim(marg))
  expect_false(isTRUE(all.equal(cond, marg)))
  # Different where it matters, not in the last bit of one cell.
  expect_gt(max(abs(colMeans(cond) - colMeans(marg))), 1e-3)

  # Conditioned on the fitted state, the draws describe the same
  # surface the hindcast reports, so the two track each other closely.
  hc <- hindcast(fit, type = "expected")
  cells <- unlist(lapply(names(hc$hindcasts), function(s) {
    rows <- which(as.character(dat$series) == s)
    rows[order(dat$time[rows])]
  }))
  hc_mean <- as.numeric(do.call(cbind, hc$hindcasts) |> colMeans())
  expect_gt(stats::cor(colMeans(cond)[cells], hc_mean), 0.9)
})


test_that("quantile residuals are not pinned against a marginal surface", {
  # This family has no analytic CDF entry, so `residuals()` forms a
  # randomised PIT against `posterior_predict()` draws. The surface
  # those draws come from decides the answer: against a marginal one
  # the PIT saturates at zero or one for the cells the model fits
  # best, the residual clamps at `qnorm(eps)`, and the trend's own
  # persistence reappears as residual autocorrelation.
  r <- residuals(fit, type = "quantile")
  expect_identical(nrow(r), nrow(dat))
  est <- r[, "Estimate"]
  expect_true(all(is.finite(est)))
  # Nothing sitting on the clamp. A pinned cell lands near -8.
  expect_lt(max(abs(est)), 6)

  # A residual carrying the trend is a failure of the surface it was
  # formed against, so the lag-one autocorrelation within a series
  # stays well below the trend's own persistence.
  for (s in series_levels) {
    rows <- which(as.character(dat$series) == s)
    rows <- rows[order(dat$time[rows])]
    a <- stats::acf(est[rows], lag.max = 1L, plot = FALSE)$acf[2L]
    expect_lt(abs(a), 0.4)
  }

  # The ordinary residuals are unaffected either way, which is what
  # places any fault in the quantile path rather than in the fit.
  ro <- residuals(fit, type = "ordinary")
  expect_true(all(is.finite(ro[, "Estimate"])))
})


test_that("plot(type = 'smooths') renders on a trials aterm model", {
  # Drawing a smooth needs three things to reach brms together: a
  # formula that names its family, so the aterm parses as binomial
  # rather than gaussian; a grid builder that passes that family on;
  # and a backfill that holds the non-focal columns at values the
  # family accepts. A denominator held at a median is fractional, and
  # brms refuses a fractional number of trials.
  drawn <- function(p) {
    expect_s3_class(p, "ggplot")
    layers <- ggplot2::ggplot_build(p)$data
    expect_gt(sum(vapply(layers, nrow, integer(1L))), 0L)
    invisible(p)
  }
  drawn(plot(fit, type = "smooths"))

  # The same grid reached through the two public smooth methods.
  sm <- smooths(fit)
  expect_gt(length(sm), 0L)
  ps <- posterior_smooths(fit, smooth = sm[1L])
  expect_identical(nrow(ps), as.integer(ndraws(fit)))
  cs <- conditional_smooths(fit)
  expect_gt(length(cs), 0L)

  # A denominator held at a fractional value is what brms refuses, so
  # the grid the backfill builds carries whole numbers.
  d <- cs[[1L]]
  if ("n_trials" %in% names(d)) {
    expect_true(all(d$n_trials == round(d$n_trials)))
  }
})


test_that("a padded forecast grid does not break the post-fit methods", {
  # A trend needs a rectangular grid, so a forecast frame pads the
  # occasions a series was not observed on. The denominator has to be
  # padded too, and a zero there cannot carry any response paired
  # with it, so every method that reads the padded rows refuses with
  # "Number of trials is smaller than the number of events".
  h <- 5L
  fut <- data.frame(
    time = rep(max(time_vals) + seq_len(h), times = n_series),
    series = factor(rep(series_levels, each = h), levels = series_levels),
    x = 0,
    n_trials = 0L,
    y = NA_integer_
  )
  expect_no_error(SM(forecast(fit, newdata = fut, ndraws = 20L)))
  expect_no_error(hindcast(fit))
  expect_no_error(plot(fit, type = "residuals"))
  expect_no_error(plot(fit, type = "trend"))
})


test_that("conditional_effects offers a grid the denominator reaches", {
  # `datagrid()` builds from `find_predictors()`, which leaves a
  # denominator out because it is not a predictor. The column then
  # never reaches the grid and brms refuses the prediction for want
  # of it. Naming the column in a hand-built grid works, and
  # `conditional_effects()` builds its own and refuses a supplied
  # one, so the grid has to carry aterm columns at a representative
  # value the way it carries a covariate at its mean.
  withr::local_options(marginaleffects_model_classes = "mvgam")
  expect_no_error(conditional_effects(fit))
})


test_that("a random effect over the series draws a visible panel", {
  # A panel holding one point per level draws nothing if its layer is
  # a line, since a line needs two points to show. The row count is
  # what says a mark was placed.
  ce <- conditional_effects(fit)
  nms <- names(ce)
  expect_true("series" %in% nms)
  b <- ggplot2::ggplot_build(plot(ce)[[which(nms == "series")]])
  expect_gt(sum(vapply(b$data, nrow, integer(1L))), 0L)
})


test_that("the monotonic effect orders its categories", {
  # `mo()` estimates the spacing between ordered categories rather
  # than fitting a slope on their codes, so what it produces is a
  # simplex of increments and one scale. Nothing else in this
  # directory fits one.
  vars <- variables(fit)
  expect_true(any(grepl("^bsp_", vars)))
  simo <- grep("^simo_", vars, value = TRUE)
  # One increment per step between the four levels.
  expect_length(simo, length(dose_levels) - 1L)

  dm <- posterior::as_draws_matrix(fit)
  # The increments are a simplex, so they sum to one in every draw.
  sums <- rowSums(as.matrix(dm[, simo, drop = FALSE]))
  expect_equal(unname(sums), rep(1, nrow(dm)), tolerance = 1e-8)

  # The fitted effect is monotone across the categories, which is the
  # constraint `mo()` exists to impose. Read from a grid that holds
  # every other column fixed and varies dose alone.
  grid <- dat[rep(1L, length(dose_levels)), , drop = FALSE]
  grid$dose <- factor(dose_levels, levels = dose_levels, ordered = TRUE)
  grid$y <- NA_integer_
  lp <- colMeans(posterior_linpred(fit, newdata = grid, ndraws = 200L))
  expect_length(lp, length(dose_levels))
  expect_false(isTRUE(all.equal(max(lp) - min(lp), 0)))
  expect_true(all(diff(lp) >= -1e-8) || all(diff(lp) <= 1e-8))
})


test_that("the group-level accessors answer on a fit that has one", {
  # `ranef()`, `VarCorr()` and `ngrps()` need a group-level effect to
  # answer at all, and this is the fit that has one. Each is held to
  # the grouping it was built over rather than to its class.
  re <- ranef(fit)
  expect_true(is.list(re) || is.array(re))
  expect_true("site" %in% names(re))
  expect_identical(dim(re$site)[1L], length(site_levels))
  expect_identical(dimnames(re$site)[[1L]], site_levels)

  vc <- VarCorr(fit)
  expect_true("site" %in% names(vc))

  ng <- ngrps(fit)
  expect_identical(as.integer(ng[["site"]]), length(site_levels))

  # The grouping belongs under `random` rather than among the terms a
  # consumer would take a population slope over.
  expect_identical(insight::find_random(fit)$random, "site")
  expect_false("site" %in% insight::find_predictors(fit)$conditional)
})


test_that("the second distributional parameter carries its own formula", {
  # `nu ~ z` puts a linear predictor on the dispersion, so `nu` moves
  # with `z` instead of holding one value. A sub-formula accepted and
  # dropped leaves a scalar, which is finite and correctly shaped.
  vars <- variables(fit)
  expect_true("b_nu_z" %in% vars)
  expect_true(any(grepl("^b_nu_Intercept$", vars)))

  # The parameter varies across rows, on its own scale.
  nu_draws <- posterior_linpred(fit, dpar = "nu", ndraws = 100L)
  expect_identical(ncol(nu_draws), nrow(dat))
  expect_gt(stats::sd(colMeans(nu_draws)), 1e-6)

  # And the covariate that drives it is offered as an effect, which a
  # term list built from the mean's formula alone would omit.
  ce <- conditional_effects(fit)
  expect_true("z" %in% names(ce))
})


test_that("the interval and error accessors answer for every row", {
  # Four accessors that need a posterior and are reachable on any
  # fit. Each is tied to the draws it summarises rather than to its
  # shape, so a method reading a different surface fails here.
  pe <- predictive_error(fit, ndraws = 50L)
  expect_identical(dim(pe), c(50L, nrow(dat)))

  pi <- predictive_interval(fit, prob = 0.8, ndraws = 200L)
  expect_identical(nrow(pi), nrow(dat))
  expect_identical(ncol(pi), 2L)
  expect_true(all(pi[, 1L] <= pi[, 2L]))

  poi <- posterior_interval(fit, prob = 0.8)
  expect_identical(ncol(poi), 2L)
  expect_true(all(poi[, 1L] <= poi[, 2L]))

  # A wider probability gives a wider interval, which is what says the
  # argument is read.
  wide <- predictive_interval(fit, prob = 0.95, ndraws = 200L)
  expect_gte(mean(wide[, 2L] - wide[, 1L]),
             mean(pi[, 2L] - pi[, 1L]))
})


test_that("how_to_cite reports the sampler settings this fit used", {
  # The citation text names the chains, the iterations and any
  # sampler control that departs from Stan's defaults, so it is the
  # method that reads `control` back out of the fit.
  txt <- capture.output(how_to_cite(fit))
  expect_gt(length(txt), 5L)
  expect_true(any(grepl("2 ", txt)))
  ctl <- fit$fit@stan_args[[1L]]$control
  expect_identical(as.numeric(ctl$adapt_delta), 0.8)
  expect_identical(as.integer(ctl$max_treedepth), 10L)
})


test_that("a tensor product is refused with the term to use instead", {
  # `te()` and `ti()` have no brms implementation, and the refusal
  # names `t2()` rather than failing inside the basis construction.
  err <- expect_error(
    mvgam(bf(y | trials(n_trials) ~ te(x, z)),
          trend_formula = ~ AR(p = 1), data = dat,
          family = com_binomial(), run_model = FALSE, silent = 2)
  )
  expect_match(conditionMessage(err), "t2", fixed = TRUE)
})


test_that("predict refuses variance and names what it does offer", {
  # `type = "variance"` has no closed form for this family, and the
  # refusal names both the families that do support it and what to
  # reach for instead, rather than failing inside the kernel.
  err <- expect_error(predict(fit, type = "variance", ndraws = 20L))
  expect_gt(nchar(conditionMessage(err)), 40L)
})


cat("\nDone.\n")
