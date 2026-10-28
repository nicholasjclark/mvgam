# Conway-Maxwell binomial with a `trials()` denominator.
#
# `com_binomial()` is a `brms::custom_family()`. The family object
# holds its Stan functions on `attr(fam, "mvgam_stanvars")`. Its second
# distributional parameter `nu` sets over or under dispersion. The
# denominator enters through a `trials()` aterm.
#
# `find_predictors()` leaves a denominator out of the term list. Three
# routes build a frame from that list and must still supply the
# denominator: the smooth grid, the marginaleffects grid and the padded
# forecast grid.
#
# The observation formula carries terms the family itself does not
# need. `mo(dose)` exercises a monotonic effect. `(1 | site)` gives
# `ranef()`, `VarCorr()` and `ngrps()` a grouping to report. `nu ~ z`
# puts a sub-formula on the second parameter of a custom family.
#
#   truth: 2 series over 50 occasions, a logit-scale smooth in `x`,
#          a latent AR(1), trials varying per row
#   model: bf(y | trials(n_trials) ~ s(x, k = 5) + series +
#             (1 | site) + mo(dose), nu ~ z) with an AR(1) trend,
#          family = com_binomial()
#
# The series levels run against alphabetical order and the occasions
# start at 3. A method that sorts the series or uses a rank as a time
# then fails here.
#
# Run with:
#   testthat::test_file("tests/local/test-family-com-binomial.R")

suppressMessages({
  devtools::load_all(".", quiet = TRUE)
  library(posterior)
  library(testthat)
})

SM <- suppressMessages


set.seed(4242L)

n_series <- 2L
n_time <- 50L
series_levels <- c("upper", "lower")
stopifnot(!identical(series_levels, sort(series_levels)))
time_vals <- seq_len(n_time) + 2L

# A latent AR(1) on the logit scale. Conditioning on it moves the
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
# A monotone increasing effect over ordered categories. `mo()`
# estimates the spacing between them.
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
# The denominator varies per row. A grid holding it at one value then
# differs from the frame.
dat$n_trials <- as.integer(sample(20:60, nrow(dat), replace = TRUE))
eta <- as.numeric(latent) +
  rep(f_true, times = n_series) +
  beta_series[as.character(dat$series)] +
  dose_effect[as.character(dat$dose)] +
  site_effect[as.integer(dat$site)]
dat$y <- rbinom(nrow(dat), size = dat$n_trials, prob = plogis(eta))
# Missing responses in both series, one of them a run of two. The fit
# drops them from the likelihood and the trend carries their states.
# Every post-fit method then meets rows holding a denominator and no
# count. Each model built from this frame draws brms's notice that
# the rows were excluded. This file asserts the notice wherever it
# arises.
na_notice <- "Rows containing NAs"
na_rows <- c(7L, 8L, 23L, 61L, 88L)
dat$y[na_rows] <- NA_integer_
observed <- !is.na(dat$y)

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
  site_levels = site_levels, site_effect = site_effect,
  na_rows = na_rows
)


# -- Prefit ------------------------------------------------------------

expect_warning(
  prefit <- mvgam(
    formula = obs_formula, trend_formula = ~ AR(p = 1),
    data = dat, family = com_binomial(), run_model = FALSE, silent = 2
  ),
  na_notice
)


test_that("the custom family reaches Stan with its own code and nu", {
  # The denominator reaches the program as integer data, one per
  # observation. The likelihood pairs each count with its own
  # denominator and its own `nu`. The `nu ~ z` predictor builds `nu`.
  sc <- as.character(stancode(prefit))
  expect_true(grepl("array[N] int trials;", sc, fixed = TRUE))
  expect_true(grepl("nu += Intercept_nu + Xc_nu * b_nu;", sc, fixed = TRUE))
  expect_true(grepl(
    "com_binomial_lpmf(Y[n] | mu[n], nu[n], trials[n], lfact_com_binomial)",
    sc, fixed = TRUE
  ))

  # The likelihood keeps the observed rows, each with its own
  # denominator and series.
  sd <- prefit$standata
  expect_identical(as.integer(sd$trials),
                   as.integer(dat$n_trials[observed]))
  expect_identical(as.integer(sd$obs_trend_series),
                   match(as.character(dat$series[observed]), series_levels))
})


# -- Fit ---------------------------------------------------------------

fit <- cached_fit(
  "val_mvgam_com_binomial.rds",
  function() {
    expect_warning(
      built <- mvgam(
        formula = obs_formula, trend_formula = ~ AR(p = 1),
        data = dat, family = com_binomial(),
        chains = 2L, iter = 1000L, warmup = 500L,
        silent = 2, backend = "cmdstanr"
      ),
      na_notice
    )
    built
  },
  key = sim_truth
)


test_that("posterior_epred gives the mean over each row's support", {
  # The mean of a Conway-Maxwell binomial differs from `trials * mu`.
  # The family's kernel takes `mu`, `nu` and the trial counts from a
  # prep list. A kernel handed an inverse-linked matrix applies the
  # link twice and returns finite means of the wrong size. The exact
  # recomputation below catches that.
  ids <- 1:3
  ep <- posterior_epred(fit, draw_ids = ids)
  expect_identical(dim(ep), c(length(ids), nrow(dat)))
  lp <- posterior_linpred(fit, draw_ids = ids, transform = FALSE)
  nu <- posterior_linpred(fit, draw_ids = ids, dpar = "nu")
  manual <- ep
  for (i in seq_along(ids)) {
    for (j in seq_len(ncol(ep))) {
      size <- dat$n_trials[j]
      pmf <- cmb_pmf(stats::plogis(lp[i, j]), nu[i, j], size)
      manual[i, j] <- sum(seq.int(0L, size) * pmf)
    }
  }
  expect_equal(ep, manual, tolerance = 1e-10)
})


test_that("posterior_predict conditions on the trend under incl_autocor", {
  # `posterior_predict()` passes `incl_autocor` to `posterior_linpred()`
  # by name. Lost in `...`, it leaves every draw marginal: finite,
  # correctly shaped and conditioned on nothing.
  ids <- 1:100
  cond <- posterior_predict(fit, draw_ids = ids, incl_autocor = TRUE)
  marg <- posterior_predict(fit, draw_ids = ids, incl_autocor = FALSE)
  expect_identical(dim(cond), dim(marg))
  # Every draw is a count within its own row's denominator, a missing
  # response's row included.
  trial_mat <- data2draws(dat$n_trials, dim(cond))
  expect_true(all(cond >= 0 & cond <= trial_mat))
  expect_gt(max(abs(colMeans(cond) - colMeans(marg))), 1e-3)

  # The conditioned draws describe the surface the hindcast reports
  hc <- hindcast(fit, type = "expected")
  cells <- unlist(lapply(names(hc$hindcasts), function(s) {
    rows <- which(as.character(dat$series) == s)
    rows[order(dat$time[rows])]
  }))
  hc_mean <- as.numeric(do.call(cbind, hc$hindcasts) |> colMeans())
  expect_gt(stats::cor(colMeans(cond)[cells], hc_mean), 0.9)
})


test_that("quantile residuals use the surface conditioned on the trend", {
  # `residuals()` evaluates the COM-binomial distribution function at
  # each draw's parameters under the fitted trend state. A marginal
  # surface saturates the PIT at zero or one for the best-fitted cells.
  # The residual then clamps at `qnorm(eps)`. The trend's persistence
  # also reappears as residual autocorrelation.
  r <- residuals(fit, type = "quantile")
  expect_identical(nrow(r), nrow(dat))
  est <- r[, "Estimate"]
  # A missing response has no residual. One missing response once
  # stopped the distribution function and every residual with it.
  expect_identical(which(is.na(est)), na_rows)
  expect_true(all(is.finite(est[observed])))
  # A clamped cell lands near -8
  expect_lt(max(abs(est[observed])), 6)

  # The lag-one autocorrelation within a series stays well below the
  # trend's own persistence of 0.7
  for (s in series_levels) {
    rows <- which(as.character(dat$series) == s)
    rows <- rows[order(dat$time[rows])]
    a <- stats::acf(est[rows], lag.max = 1L, plot = FALSE,
                    na.action = stats::na.pass)$acf[2L]
    expect_lt(abs(a), 0.4)
  }

  # The ordinary residuals need no distribution function. They leave
  # the same rows missing and every observed row finite.
  ro <- residuals(fit, type = "ordinary")[, "Estimate"]
  expect_identical(which(is.na(ro)), na_rows)
  expect_true(all(is.finite(ro[observed])))
})


test_that("a residual histogram shows eight draws of a stable PIT", {
  # The histogram draws one panel per draw. Drawing all 500 draws
  # rendered an unreadable grid.
  p <- pp_check(fit, type = "resid_hist")
  panels <- unique(ggplot2::ggplot_build(p)$data[[1L]]$PANEL)
  expect_length(panels, 8L)
  # This family's residuals come from its distribution function. A
  # small draw count then raises no empirical-PIT advice.
  withr::local_envvar(TESTTHAT = "")
  got <- with_warnings(pp_check(fit, type = "resid_hist", ndraws = 10L))
  expect_false(any(grepl("fewer than 50 draws", got$warnings)))
})


test_that("plot(type = 'smooths') renders on a trials aterm model", {
  expect_drawn(plot(fit, type = "smooths"))

  # The same grid reached through the two public smooth methods
  sm <- smooths(fit)
  expect_length(sm, 1L)
  ps <- posterior_smooths(fit, smooth = sm[1L])
  expect_identical(dim(ps), c(as.integer(ndraws(fit)), nrow(dat)))
  expect_length(conditional_smooths(fit), 1L)
})


test_that("post-fit methods accept a forecast frame with zero trials", {
  # A future occasion has no count yet. Its denominator of zero pairs
  # with a missing response. brms once refused such rows with "Number
  # of trials is smaller than the number of events".
  h <- 5L
  fut <- data.frame(
    time = rep(max(time_vals) + seq_len(h), times = n_series),
    series = factor(rep(series_levels, each = h), levels = series_levels),
    x = 0,
    z = 0,
    site = factor(site_levels[1L], levels = site_levels),
    dose = factor(dose_levels[1L], levels = dose_levels, ordered = TRUE),
    n_trials = 0L,
    y = NA_integer_
  )
  fc <- SM(forecast(fit, newdata = fut, ndraws = 20L))
  expect_identical(names(fc$forecasts), series_levels)
  expect_identical(dim(fc$forecasts[[1L]]), c(20L, h))
  expect_identical(names(hindcast(fit)$hindcasts), series_levels)
  # The residual panel omits the missing responses and says so once
  withr::local_envvar(TESTTHAT = "")
  rlang::reset_message_verbosity("mvgam_pp_check_missing_response")
  expect_message(plot(fit, type = "residuals"),
                 "missing response are omitted")
  expect_drawn(plot(fit, type = "trend"))
})


test_that("conditional_effects builds a grid holding the denominator", {
  # `datagrid()` builds from `find_predictors()`, which omits the
  # denominator. brms refuses a prediction whose grid lacks it.
  # `conditional_effects()` sets each aterm column at a representative
  # value, as it sets a covariate at its mean.
  ce <- conditional_effects(fit)
  # `z` drives `nu` alone. A term list built from the mean's formula
  # omits it.
  expect_true(all(c("series", "z") %in% names(ce)))
  # The series panel holds one point per level. A line layer needs two
  # points and draws nothing. A positive row count shows a mark.
  b <- ggplot2::ggplot_build(plot(ce)[["series"]])
  expect_gt(sum(vapply(b$data, nrow, integer(1L))), 0L)
})


test_that("the monotonic effect orders its categories", {
  # `mo()` estimates a simplex of increments between ordered categories
  # and one scale `bsp`
  vars <- variables(fit)
  expect_true("bsp_modose" %in% vars)
  simo <- paste0("simo_modose1[", seq_len(length(dose_levels) - 1L), "]")
  expect_true(all(simo %in% vars))

  dm <- posterior::as_draws_matrix(fit)
  sums <- rowSums(as.matrix(dm[, simo, drop = FALSE]))
  expect_equal(unname(sums), rep(1, nrow(dm)), tolerance = 1e-8)

  # A grid that varies dose alone. The fitted effect climbs across the
  # categories, as the simulated effect does.
  grid <- dat[rep(1L, length(dose_levels)), , drop = FALSE]
  grid$dose <- factor(dose_levels, levels = dose_levels, ordered = TRUE)
  grid$y <- NA_integer_
  lp <- colMeans(posterior_linpred(fit, newdata = grid, ndraws = 200L))
  expect_true(all(diff(lp) > 0))
  expect_gt(lp[4L] - lp[1L], 0.3)
})


test_that("the group-level accessors report the site grouping", {
  re <- ranef(fit)
  expect_named(re, "site")
  expect_identical(dimnames(re$site)[[1L]], site_levels)
  expect_true("site" %in% names(VarCorr(fit)))
  expect_identical(as.integer(ngrps(fit)[["site"]]), length(site_levels))

  # The grouping is reported under `random`. A consumer takes no
  # population slope over it.
  expect_identical(insight::find_random(fit)$random, "site")
  expect_false("site" %in% insight::find_predictors(fit)$conditional)
})


test_that("nu varies with its own covariate", {
  # `nu ~ z` puts a linear predictor on the dispersion. A sub-formula
  # accepted and dropped leaves a finite, correctly shaped scalar.
  vars <- variables(fit)
  expect_true(all(c("b_nu_Intercept", "b_nu_z") %in% vars))
  nu_draws <- posterior_linpred(fit, dpar = "nu", ndraws = 100L)
  expect_identical(ncol(nu_draws), nrow(dat))
  expect_gt(stats::sd(colMeans(nu_draws)), 1e-6)
})


test_that("the interval and error accessors cover every row", {
  pe <- predictive_error(fit, ndraws = 50L)
  expect_identical(dim(pe), c(50L, nrow(dat)))

  pi <- predictive_interval(fit, prob = 0.8, ndraws = 200L)
  expect_identical(dim(pi), c(nrow(dat), 2L))
  expect_true(all(pi[, 1L] <= pi[, 2L]))

  poi <- posterior_interval(fit, prob = 0.8)
  expect_identical(ncol(poi), 2L)
  expect_true(all(poi[, 1L] <= poi[, 2L]))

  # A wider probability gives a wider interval
  wide <- predictive_interval(fit, prob = 0.95, ndraws = 200L)
  expect_gt(mean(wide[, 2L] - wide[, 1L]), mean(pi[, 2L] - pi[, 1L]))
})


test_that("how_to_cite reports the sampler settings this fit used", {
  # This fit ran 2 chains of 500 warmup and 500 sampling iterations
  txt <- paste(capture.output(how_to_cite(fit)), collapse = " ")
  txt <- gsub("\\s+", " ", txt)
  expect_match(txt, paste("We ran 2 Hamiltonian Monte Carlo chains for",
                          "500 warmup iterations and 500 sampling",
                          "iterations."), fixed = TRUE)
  # The family's own reference
  expect_match(txt, "Shmueli et al. 2005", fixed = TRUE)
})


test_that("a user prior reaches the program on every class it fits", {
  # mvgam injects its own defaults for classes brms would get wrong. A
  # merge that dropped or reordered the user's rows would leave
  # `prior_summary()` and the program disagreeing, both well formed.
  cases <- list(
    list(prior(normal(0, 0.123), class = "b"), "0.123"),
    list(prior(normal(3, 0.456), class = "Intercept"), "0.456"),
    list(prior(exponential(3.21), class = "sd"), "3.21"),
    list(prior(exponential(4.32), class = "sds"), "4.32"),
    list(prior(exponential(5.43), class = "sigma_trend"), "5.43"),
    list(prior(normal(0, 0.321), class = "ar1_trend"), "0.321")
  )
  for (cs in cases) {
    expect_warning(
      built <- SM(mvgam(
        obs_formula, trend_formula = ~ AR(p = 1), data = dat,
        family = com_binomial(), prior = cs[[1L]],
        run_model = FALSE, silent = 2
      )),
      na_notice
    )
    code <- paste(as.character(stancode(built)), collapse = "\n")
    expect_true(grepl(cs[[2L]], code, fixed = TRUE))
  }

  # `get_prior()` lists the trend's classes among those a user can set
  expect_warning(
    gp <- as.data.frame(get_prior(
      mvgam_formula(obs_formula, trend_formula = ~ AR(p = 1)),
      data = dat, family = com_binomial()
    )),
    na_notice
  )
  expect_true(all(c("sigma_trend", "ar1_trend") %in% gp$class))
  # brms gives a parameter named `nu` a positive-only default. The
  # modelled `nu` intercept takes the prior the program uses.
  nu_row <- gp[gp$class == "Intercept" &
                 !is.na(gp$dpar) & gp$dpar == "nu", "prior"]
  expect_identical(as.character(nu_row), "normal(1, 1)")
})


test_that("no method warns about a prior this model does not use", {
  # Methods that delegate to the stored brms model re-derived brms's
  # default priors and warned about the positive-only `nu` prior mvgam
  # replaced
  for (call in list(
    function() variables(fit),
    function() capture.output(summary(fit)),
    function() ranef(fit),
    function() VarCorr(fit),
    function() ngrps(fit)
  )) {
    w <- with_warnings(call())$warnings
    expect_identical(grep("lower bounded prior", w, value = TRUE),
                     character(0))
  }
})


test_that("get_coef reports every population coefficient fixef reports", {
  # Third-party packages learn the coefficients from `get_coef()`.
  # brms writes the population block as an indexed array. Without the
  # aliasing pass the hook found `b_Intercept` and nothing else.
  gc <- mvgam:::get_coef.mvgam(fit)
  expect_identical(mvgam:::fixef_name(names(gc)), rownames(fixef(fit)))
})


test_that("a tensor product is refused with the term to use instead", {
  # brms implements neither `te()` nor `ti()`. The refusal names `t2()`.
  expect_error(
    mvgam(bf(y | trials(n_trials) ~ te(x, z)),
          trend_formula = ~ AR(p = 1), data = dat,
          family = com_binomial(), run_model = FALSE, silent = 2),
    "t2", fixed = TRUE
  )
})


test_that("predict refuses variance and names what it does offer", {
  # `type = "variance"` has no closed form for this family
  err <- expect_error(predict(fit, type = "variance", ndraws = 20L))
  expect_match(conditionMessage(err), "family 'com_binomial'",
               fixed = TRUE)
  expect_match(conditionMessage(err), "type = \"response\"",
               fixed = TRUE)
})


cat("\nDone.\n")
