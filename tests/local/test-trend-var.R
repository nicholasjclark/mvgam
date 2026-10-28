# Post-fit coverage for `VAR()`, fitted in this file.
#
# A VAR lets one series' past drive another's future through the
# off-diagonal entries of the transition matrix `A`. The checks here
# hold `A` to its index order, its stationarity and its use in every
# post-fit surface: forecasts, impulse responses, variance
# decompositions, stability metrics and residual correlations.
#
#   truth: 3 series, 60 occasions, gaussian, known stationary A with
#          deliberate cross-series terms, Sigma with correlated noise
#   model: y ~ elev * region + (1 | block),
#          trend_formula = ~ VAR(cor = TRUE)
#
# The observation side carries a continuous-by-factor interaction and
# a random intercept beyond the trend. Series are named out of
# alphabetical order. Occasions are numbered from 3. A rank then
# differs from every value it indexes.
#
# Cached at tests/local/fixtures/val_mvgam_var_trend.rds. Delete to
# refit.
#
# Run with:
#   testthat::test_file("tests/local/test-trend-var.R")

suppressMessages({
  devtools::load_all(".", quiet = TRUE)
  library(posterior)
  library(testthat)
})

# This file fits its own model and caches it in the fixtures folder.

SM <- suppressMessages

# testthat stops a file after ten failures by default. The reporter
# takes the limit from the environment when it starts:
#   TESTTHAT_MAX_FAILS=1000 Rscript -e "..."

set.seed(911L)

n_series <- 3L
n_time <- 60L
series_levels <- c("willow", "ash", "rowan")
stopifnot(!identical(series_levels, sort(series_levels)))

time_vals <- seq_len(n_time) + 2L

# A stationary, asymmetric transition matrix with cross-series terms.
# Its transpose is a different model.
A_true <- matrix(
  c(0.55, 0.30, 0.00,
    0.00, 0.45, 0.35,
    0.20, 0.00, 0.40),
  nrow = n_series, byrow = TRUE
)
stopifnot(max(Mod(eigen(A_true)$values)) < 1)

sigma_true <- c(0.35, 0.30, 0.40)
R_true <- matrix(c(1, 0.4, -0.2,
                   0.4, 1, 0.3,
                   -0.2, 0.3, 1), nrow = n_series)
Sigma_true <- diag(sigma_true) %*% R_true %*% diag(sigma_true)
L_true <- chol(Sigma_true)

latent <- matrix(0, nrow = n_time, ncol = n_series)
for (t in 2:n_time) {
  innov <- as.numeric(crossprod(L_true, rnorm(n_series)))
  latent[t, ] <- as.numeric(A_true %*% latent[t - 1L, ]) + innov
}

# Observation-side structure. `region` is a three-level factor
# declared out of alphabetical order. `elev` is continuous. The model
# interacts the two. `block` groups the occasions into six blocks that
# cross the series, which identifies a random intercept with the
# trend.
region_levels <- c("upper", "lower", "mid")
n_block <- 6L
block_sd <- 0.4

elev <- as.numeric(scale(rnorm(n_time)))
region <- factor(region_levels[(seq_len(n_time) %% 3L) + 1L],
                 levels = region_levels)
block <- factor(rep(seq_len(n_block), each = n_time / n_block))
block_effect <- rnorm(n_block, 0, block_sd)

# Slopes on `elev` differ by region
beta_elev <- c(upper = 0.25, lower = -0.30, mid = 0.05)
beta_region <- c(upper = 0, lower = 0.35, mid = -0.20)

dat <- data.frame(
  time = rep(time_vals, times = n_series),
  series = factor(rep(series_levels, each = n_time),
                  levels = series_levels),
  elev = rep(elev, times = n_series),
  region = rep(region, times = n_series),
  block = rep(block, times = n_series)
)
dat$y <- as.numeric(latent) +
  beta_region[as.character(dat$region)] +
  beta_elev[as.character(dat$region)] * dat$elev +
  block_effect[as.integer(dat$block)] +
  rnorm(nrow(dat), 0, 0.2)

obs_formula <- y ~ elev * region + (1 | block)

# A future frame holding the axis columns and every covariate the
# observation model uses
make_future <- function(h) {
  ft <- max(time_vals) + seq_len(h)
  data.frame(
    time = rep(ft, times = n_series),
    series = factor(rep(series_levels, each = h),
                    levels = series_levels),
    elev = rep(rep(0, h), times = n_series),
    region = factor(rep(rep(region_levels[1L], h), times = n_series),
                    levels = region_levels),
    block = factor(rep(rep(levels(block)[1L], h), times = n_series),
                   levels = levels(block)),
    y = NA_real_
  )
}

sim_truth <- list(
  n_series = n_series, n_time = n_time,
  series_levels = series_levels, time_vals = time_vals,
  A_true = A_true, Sigma_true = Sigma_true,
  sigma_true = sigma_true, latent = latent,
  region_levels = region_levels, beta_elev = beta_elev,
  beta_region = beta_region, block_effect = block_effect
)


# -- Prefit: the shape before any sampling ---------------------------

prefit <- mvgam(
  formula = obs_formula, trend_formula = ~ VAR(cor = TRUE),
  data = dat, family = gaussian(), run_model = FALSE, silent = 2
)


test_that("the prefit records the series axis and the VAR grain", {
  ax <- mvgam:::mvgam_axes(prefit)
  expect_identical(as.character(ax$series$levels), series_levels)
  expect_identical(as.integer(ax$time$values), time_vals)
  expect_identical(ax$grain, "series")
  sd <- prefit$standata
  expect_identical(as.integer(sd$obs_trend_series),
                   match(as.character(dat$series), series_levels))
})


test_that("the random intercept indexes each row's own block", {
  # A grouping resolved against a reordered frame keeps its count and
  # its levels and pairs rows with the wrong blocks.
  expect_identical(as.integer(prefit$standata$J_1), as.integer(dat$block))
})


# -- Fit --------------------------------------------------------------

fit <- cached_fit(
  "val_mvgam_var_trend.rds",
  function() {
    mvgam(
      formula = obs_formula, trend_formula = ~ VAR(cor = TRUE),
      data = dat, family = gaussian(),
      chains = 2L, iter = 1000L, warmup = 500L,
      silent = 2, backend = "cmdstanr"
    )
  },
  key = sim_truth
)

dm_all <- posterior::as_draws_matrix(fit$fit)

# One draw's square trend matrix, `nm` a pattern over row and column
draw_matrix <- function(k, nm) {
  matrix(as.numeric(dm_all[k, sprintf(nm, rep(1:n_series, n_series),
                                       rep(1:n_series, each = n_series))]),
         n_series)
}

# One draw's stationary covariance, which solves `G = A G A' + Sigma`
stationary_cov <- function(k) {
  A <- draw_matrix(k, "Phi_trend[1,%d,%d]")
  matrix(solve(diag(n_series^2) - kronecker(A, A),
               as.vector(draw_matrix(k, "Sigma_trend[%d,%d]"))), n_series)
}

# The posterior mean transition matrix, indexed as the draws name it,
# `Phi_trend[lag, row, column]`
A_hat <- Reduce(`+`, lapply(seq_len(nrow(dm_all)), draw_matrix,
                            nm = "Phi_trend[1,%d,%d]")) / nrow(dm_all)



test_that("the chain count comes from the slot holding finished chains", {
  # The CI stub is a draws array with no `stan_args` slot. A fitted
  # model is where the slot branch runs.
  expect_true(isS4(fit$fit))
  expect_true("stan_args" %in% methods::slotNames(fit$fit))
  expect_identical(realised_chain_count(fit$fit), 2L)

  # The slot and the draws agree on a cmdstanr fit
  expect_identical(
    realised_chain_count(fit$fit),
    as.integer(posterior::nchains(posterior::as_draws(fit$fit)))
  )

  # A request above what finished names both counts.
  expect_warning(
    check_chains_finished(fit$fit, requested = 4L, algorithm = "sampling"),
    "Asked for 4 chains and 2 finished"
  )
  expect_silent(check_chains_finished(fit$fit, 2L, "sampling"))
})


test_that("Phi_trend is indexed [lag, row, column]", {
  # The simulated `A` is asymmetric. A reader that swapped the row
  # and column indices returns its transpose. The posterior mean is
  # closer to the simulated matrix than to that transpose.
  expect_lt(sum((A_hat - A_true)^2), sum((A_hat - t(A_true))^2))
})


# The spectral radius of `A` in every draw
radii_of_A <- function() {
  vapply(seq_len(nrow(dm_all)), function(k) {
    max(Mod(eigen(draw_matrix(k, "Phi_trend[1,%d,%d]"),
                  only.values = TRUE)$values))
  }, numeric(1))
}


test_that("every draw of A is stationary and starts at its own law", {
  # The map of Heaps (2023) keeps every draw of `A` stationary
  expect_true(all(radii_of_A() < 1))
  # The states before the first occasion take the stationary
  # covariance. The program places it from the autocovariances the
  # coefficient map returns. The Lyapunov solve in R is the
  # definition.
  worst <- max(vapply(round(seq(1, nrow(dm_all), length.out = 50L)),
                      function(k) {
    max(abs(stationary_cov(k) - draw_matrix(k, "Omega_trend[%d,%d]")))
  }, numeric(1L)))
  expect_lt(worst, 1e-6)
})


test_that("residual_cor reports the stationary or the innovation covariance", {
  # By default `residual_cor()` reports the posterior mean of the
  # stationary covariance. `covariance = "innovation"` reports that of
  # the innovation covariance `Sigma`, a different matrix.
  ks <- seq_len(nrow(dm_all))
  G <- Reduce(`+`, lapply(ks, stationary_cov)) / length(ks)
  S <- Reduce(`+`, lapply(ks, draw_matrix, nm = "Sigma_trend[%d,%d]")) /
    length(ks)
  rc <- residual_cor(fit)
  expect_equal(unname(rc$cov), G, tolerance = 1e-6)
  expect_gt(max(abs(unname(rc$cov) - S)), 0.05)
  shocks <- residual_cor(fit, covariance = "innovation")
  expect_equal(unname(shocks$cov), S, tolerance = 1e-6)
  # `draw_ids` keeps the named draws, in the order named
  sub <- residual_cor(fit, covariance = "innovation", summary = FALSE,
                      draw_ids = c(7L, 2L))$cov_draws
  expect_equal(unname(sub[1L, , ]), draw_matrix(7L, "Sigma_trend[%d,%d]"))
  expect_equal(unname(sub[2L, , ]), draw_matrix(2L, "Sigma_trend[%d,%d]"))
})


test_that("the observation structure keys each row to its own cell", {
  os <- mvgam:::get_observation_structure(fit, newdata = dat)
  expect_identical(as.character(os$series), as.character(dat$series))
  expect_identical(os$series_levels, series_levels)
  expect_identical(as.integer(os$time), match(dat$time, time_vals))
})


test_that("each row takes the latent cell the sampler drew for it", {
  t_rec <- as.integer(fit$standata$obs_trend_time)
  s_rec <- as.integer(fit$standata$obs_trend_series)
  want <- vapply(paste0("trend[", t_rec, ",", s_rec, "]"),
                 function(k) mean(dm_all[, k]), numeric(1))
  got <- colMeans(
    mvgam:::extract_trend_latent_states(fit, newdata = dat,
                                        full_draws = dm_all)
  )
  expect_equal(unname(got), unname(want))
})


test_that("the one-step forecast is A times the last state", {
  # In each draw the first step is `Phi %*% trend[T, ]` plus a
  # mean-zero innovation. Over every draw the forecast mean equals the
  # mean of that product within Monte Carlo error. Series 1's step
  # depends on series 2's last state through `A[1, 2]`. A recursion
  # that applied `A` transposed, or per series independently, lands
  # elsewhere.
  ids <- seq_len(nrow(dm_all))
  set.seed(5L)
  fc <- forecast(fit, newdata = make_future(1L), draw_ids = ids,
                 type = "trend")
  last <- function(k) {
    as.numeric(dm_all[k, sprintf("trend[%d,%d]", n_time, 1:n_series)])
  }
  step <- t(vapply(ids, function(k) {
    as.numeric(draw_matrix(k, "Phi_trend[1,%d,%d]") %*% last(k))
  }, numeric(n_series)))
  innov_sd <- sqrt(diag(Reduce(`+`, lapply(
    ids, draw_matrix, nm = "Sigma_trend[%d,%d]"
  )) / length(ids)))
  for (k in seq_len(n_series)) {
    got <- mean(fc$forecasts[[series_levels[k]]][, 1L])
    expect_lt(abs(got - mean(step[, k])),
              4 * innov_sd[k] / sqrt(length(ids)))
  }
  # The off-diagonal coefficients move the step away from the
  # diagonal-only one. An independent AR uses only the diagonal and
  # fails the check above.
  last_mean <- colMeans(t(vapply(ids, last, numeric(n_series))))
  expect_gt(max(abs(colMeans(step) - diag(A_hat) * last_mean)), 0.05)
})


test_that("an identity link gives one forecast on both scales", {
  # A gaussian expectation is the linear predictor itself. Under one
  # seed the two scales return the same numbers. A scale that applied
  # a link it does not have, or dropped the observation model's terms,
  # differs here.
  set.seed(9L)
  lk <- forecast(fit, newdata = make_future(4L), ndraws = 20L,
                 type = "link")
  set.seed(9L)
  ex <- forecast(fit, newdata = make_future(4L), ndraws = 20L,
                 type = "expected")
  expect_equal(ex$forecasts, lk$forecasts)
})


test_that("the forecast object is keyed, ordered and in user units", {
  # The occasions a forecast covers, their order and the series of
  # each element are recorded on the object
  h <- 4L
  future_times <- max(time_vals) + seq_len(h)
  fc <- forecast(fit, newdata = make_future(h), ndraws = 20L,
                 type = "link")

  # The elements follow the series axis
  expect_identical(names(fc$forecasts), series_levels)
  expect_identical(as.character(fc$series_names), series_levels)

  for (s in series_levels) {
    # The horizon holds the occasions the caller asked for, ascending,
    # in the user's numbering
    expect_identical(as.integer(fc$test_times[[s]]), future_times)
    # The training tail is this fit's own grid, also in user units.
    expect_identical(as.integer(fc$train_times[[s]]), time_vals)
    # Each element's training observations are that series' own, in
    # time order
    rows <- which(as.character(dat$series) == s)
    rows <- rows[order(dat$time[rows])]
    expect_equal(as.numeric(fc$train_observations[[s]]),
                 as.numeric(dat$y[rows]))
  }
})


test_that("fitted summarises the same draws epred returns", {
  # `fitted()` summarises the columns `posterior_epred()` returns. A
  # re-sorted output pairs values with the wrong rows.
  ep <- posterior_epred(fit, ndraws = NULL, incl_autocor = TRUE)
  ft <- fitted(fit)
  expect_equal(as.numeric(ft[, "Estimate"]), unname(colMeans(ep)))
})


test_that("the hindcast matches the conditional epred cell by cell", {
  # `hindcast()` composes per series and `posterior_epred()` across
  # the whole frame. The two agree when both resolve the same state for
  # each cell.
  hc <- hindcast(fit, type = "expected")
  blocks <- hc$hindcasts
  expect_identical(names(blocks), series_levels)
  cells <- unlist(lapply(names(blocks), function(s) {
    rows <- which(as.character(dat$series) == s)
    rows[order(dat$time[rows])]
  }))
  ep <- posterior_epred(fit, incl_autocor = TRUE)
  expect_equal(unname(ep[, cells, drop = FALSE]),
               unname(do.call(cbind, blocks)))
})


test_that("the VAR methods gate on the recorded trend type", {
  # `irf()`, `fevd()` and `posterior_transition_matrix()` take the
  # trend type from `trend_metadata$trend_type`
  expect_identical(get_trend_type(fit), "VAR")
  blanked <- fit
  blanked$trend_metadata$trend_type <- NULL
  expect_identical(get_trend_type(blanked), "None")
  for (meth in list(function(x) irf(x, h = 2L),
                    function(x) fevd(x, h = 2L),
                    posterior_transition_matrix)) {
    expect_error(meth(blanked), "requires a VAR latent trend")
  }
})


test_that("the decomposition is orthogonalised in the series order", {
  fe <- fevd(fit, h = 6L)

  # The Cholesky factor follows the series order. At the first horizon
  # series k takes shares from shocks 1 to k alone. The table is then
  # upper triangular when the shock axis is the series axis.
  h1 <- fe[fe$horizon == 1L, ]
  sides <- strsplit(h1$shock, " -> ", fixed = TRUE)
  from <- vapply(sides, `[`, "", 1L)
  to <- vapply(sides, `[`, "", 2L)
  # Keyed in the order the table emits
  keys <- unique(from)
  M <- matrix(0, n_series, n_series, dimnames = list(keys, keys))
  for (i in seq_len(nrow(h1))) {
    M[from[i], to[i]] <- h1$fevdQ50[i]
  }
  expect_equal(sum(abs(M[lower.tri(M)])), 0)
  expect_gt(sum(abs(M[upper.tri(M)])), 0.05)

  # Every other order of the axis breaks the triangle
  for (perm in list(c(2L, 1L, 3L), c(1L, 3L, 2L), c(3L, 2L, 1L))) {
    expect_gt(sum(abs(M[perm, perm][lower.tri(M)])), 1e-3)
  }
})


test_that("posterior_transition_matrix returns this fit's own Phi", {
  # The accessor and the draws agree cell for cell. A transposed
  # matrix, or the lag index taken as a row, puts the right numbers in
  # the wrong cells.
  ptm <- posterior_transition_matrix(fit)
  expect_equal(unname(ptm$A), unname(A_hat), tolerance = 1e-8)

  # The two tail probabilities partition the draws. `prob_nonzero` is
  # the larger of the two.
  expect_equal(unname(ptm$prob_positive + ptm$prob_negative),
               matrix(1, n_series, n_series))
  expect_equal(unname(ptm$prob_nonzero),
               unname(pmax(ptm$prob_positive, ptm$prob_negative)))

  # The rows and columns are named by series
  expect_identical(as.character(ptm$series_names), series_levels)
  expect_identical(rownames(ptm$A), series_levels)
  expect_identical(colnames(ptm$A), series_levels)
})


test_that("the prediction methods refuse an unknown argument", {
  # A misspelled argument in `...` would leave the default in place
  expect_error(posterior_epred(fit, zzz_unknown = 1))
  expect_error(posterior_predict(fit, zzz_unknown = 1))
  expect_error(predict(fit, zzz_unknown = 1))
  expect_error(residuals(fit, zzz_unknown = 1))
})


test_that("irf and fevd name each process by its series", {
  ir <- irf(fit, h = 3L)
  labels <- unique(unlist(strsplit(unique(ir$shock), " -> ",
                                   fixed = TRUE)))
  expect_setequal(labels, series_levels)
})


test_that("the residual correlation is labelled by the series axis", {
  rc <- residual_cor(fit)
  expect_identical(rownames(rc$cor), series_levels)
  expect_identical(colnames(rc$cor), series_levels)
})


test_that("the variance surface is the gaussian variance", {
  # A gaussian predictive variance is `sigma^2` on every row. A
  # surface returning the standard deviation, or the variance of the
  # linear predictor, fails here.
  v <- predict(fit, type = "variance", ndraws = 300L)
  sigma_draws <- as.numeric(posterior::as_draws_matrix(fit)[, "sigma"])
  expect_equal(mean(v[, 1L]), mean(sigma_draws^2), tolerance = 0.05)
  # Constant across rows
  expect_lt(stats::sd(v[, 1L]) / mean(v[, 1L]), 0.05)
})


test_that("the factor summaries refuse a VAR over the series", {
  # A VAR gives every series its own latent dimension
  expect_error(active_factors(fit), "requires a latent factor model")
  expect_error(shared_variation(fit), "requires a latent factor model")
  expect_error(ordinate(fit), "requires a latent factor model")
})


# -- The newdata battery ----------------------------------------------

ref_epred <- posterior_epred(fit, newdata = dat, draw_ids = 1:10,
                             incl_autocor = TRUE)

test_that("the reference the battery compares against actually varies", {
  # Every check below compares a rearranged or cut frame with columns
  # of `ref_epred`. A predictor returning one constant would pass them
  # all. These checks bite only when the reference varies between and
  # within series.
  colm <- colMeans(ref_epred)
  expect_gt(stats::sd(colm), 0)

  # Between series
  by_series <- tapply(colm, as.character(dat$series), mean)
  expect_gt(stats::sd(as.numeric(by_series)), 0)

  # Within each series over time
  for (s in series_levels) {
    rows <- which(as.character(dat$series) == s)
    expect_gt(stats::sd(colm[rows]), 0)
  }
})


test_that("a shuffled newdata gives the same predictions in the new order", {
  set.seed(37L)
  perm <- sample(nrow(dat))
  expect_equal(
    unname(posterior_epred(fit, newdata = dat[perm, , drop = FALSE],
                           draw_ids = 1:10, incl_autocor = TRUE)),
    unname(ref_epred[, perm, drop = FALSE])
  )
})


test_that("a newdata holding one series takes that series' state", {
  for (s in series_levels) {
    rows <- which(as.character(dat$series) == s)
    sub <- dat[rows, , drop = FALSE]
    sub$series <- droplevels(sub$series)
    expect_identical(levels(sub$series), s)
    expect_equal(
      unname(posterior_epred(fit, newdata = sub, draw_ids = 1:10,
                             incl_autocor = TRUE)),
      unname(ref_epred[, rows, drop = FALSE])
    )
  }
})


test_that("a newdata holding a pair of series takes both states", {
  for (subset in list(series_levels[c(1L, 3L)],
                      series_levels[c(3L, 2L)])) {
    rows <- which(as.character(dat$series) %in% subset)
    sub <- dat[rows, , drop = FALSE]
    sub$series <- factor(as.character(sub$series), levels = subset)
    expect_equal(
      unname(posterior_epred(fit, newdata = sub, draw_ids = 1:10,
                             incl_autocor = TRUE)),
      unname(ref_epred[, rows, drop = FALSE])
    )
  }
})


test_that("relabelling or retyping the series leaves predictions unchanged", {
  rev_nd <- dat
  rev_nd$series <- factor(as.character(rev_nd$series),
                          levels = rev(series_levels))
  expect_equal(
    unname(posterior_epred(fit, newdata = rev_nd, draw_ids = 1:10,
                           incl_autocor = TRUE)),
    unname(ref_epred)
  )
  chr_nd <- dat
  chr_nd$series <- as.character(chr_nd$series)
  expect_equal(
    unname(posterior_epred(fit, newdata = chr_nd, draw_ids = 1:10,
                           incl_autocor = TRUE)),
    unname(ref_epred)
  )
  extra_nd <- dat
  extra_nd$series <- factor(as.character(extra_nd$series),
                            levels = c(series_levels, "unobserved"))
  expect_identical(sum(extra_nd$series == "unobserved"), 0L)
  expect_equal(
    unname(posterior_epred(fit, newdata = extra_nd, draw_ids = 1:10,
                           incl_autocor = TRUE)),
    unname(ref_epred)
  )
})


test_that("single, repeated and single-occasion frames predict their cells", {
  for (s in series_levels) {
    j <- which(as.character(dat$series) == s)[1L]
    one <- posterior_epred(fit, newdata = dat[j, , drop = FALSE],
                           draw_ids = 1:10, incl_autocor = TRUE)
    expect_equal(unname(one), unname(ref_epred[, j, drop = FALSE]))
  }
  j <- which(as.character(dat$series) == series_levels[2L])[1L]
  rep_nd <- dat[c(j, j), , drop = FALSE]
  got <- posterior_epred(fit, newdata = rep_nd, draw_ids = 1:10,
                         incl_autocor = TRUE)
  expect_equal(unname(got[, 1L]), unname(got[, 2L]))

  for (tv in time_vals[c(1L, 25L, n_time)]) {
    rows <- which(dat$time == tv)
    expect_equal(
      unname(posterior_epred(fit, newdata = dat[rows, , drop = FALSE],
                             draw_ids = 1:10, incl_autocor = TRUE)),
      unname(ref_epred[, rows, drop = FALSE])
    )
  }
})


test_that("an unknown series is refused by name", {
  nd <- dat
  nd$series <- factor(
    ifelse(seq_len(nrow(nd)) == 1L, "hazel", as.character(nd$series)),
    levels = c(series_levels, "hazel")
  )
  err <- expect_error(
    posterior_epred(fit, newdata = nd, draw_ids = 1:5),
    "Series in 'newdata' has levels absent from the training data"
  )
  expect_match(conditionMessage(err), "hazel", fixed = TRUE)
  for (s in series_levels) {
    expect_match(conditionMessage(err), s, fixed = TRUE)
  }
})


test_that("a ragged panel is refused with counts and a remedy", {
  ragged <- dat[!(as.character(dat$series) == series_levels[2L] &
                    dat$time == time_vals[10L]), , drop = FALSE]
  err <- expect_error(
    mvgam(
      formula = obs_formula, trend_formula = ~ VAR(cor = TRUE),
      data = ragged, family = gaussian(), run_model = FALSE,
      silent = 2
    ),
    "must share one time grid"
  )
  msg <- conditionMessage(err)
  expect_match(msg, as.character(n_time), fixed = TRUE)
  expect_match(msg, as.character(n_series), fixed = TRUE)
  expect_match(msg, "NA", fixed = TRUE)
})


test_that("summary, tidiers and criticism run on a VAR fit", {
  txt <- capture.output(summary(fit))
  expect_true(any(grepl(paste0("Series:\\s*", n_series), txt)))
  # `loo()` warns when a Pareto-k exceeds its threshold. The warning
  # is captured and checked against the diagnostics it reports on.
  caught <- with_warnings(loo(fit))
  ic <- caught$value
  loo_warnings <- caught$warnings
  expect_true(is.finite(ic$estimates["elpd_loo", "Estimate"]))
  pareto_k <- ic$diagnostics$pareto_k
  expect_true(all(is.finite(pareto_k)))
  # Dropping an observation moves the state it is scored against. A
  # state-space fit shows high Pareto-k values for that reason. The
  # warning arrives exactly when a k exceeds 0.7.
  expect_identical(
    any(pareto_k > 0.7),
    any(grepl("Pareto k", loo_warnings))
  )
  # Every warning raised is the Pareto-k notice
  expect_true(all(grepl("Pareto k", loo_warnings)))

  aug <- augment(fit)
  expect_equal(as.numeric(aug$time), as.numeric(dat$time))
  expect_equal(as.numeric(aug$.observed), as.numeric(dat$y))
  expect_identical(as.character(aug$series), as.character(dat$series))
  # `variables()`, `posterior_summary()` and `tidy()` report the same
  # transition matrix and innovation scales.
  expect_true(any(grepl("^Phi_trend\\[", variables(fit))))
  n_A_vars <- sum(grepl("^Phi_trend\\[", variables(fit)))
  n_A_summ <- sum(grepl("^Phi_trend\\[", rownames(posterior_summary(fit))))
  expect_identical(n_A_summ, n_A_vars)
  td <- tidy(fit, effects = "all")
  expect_identical(sum(grepl("^Phi_trend\\[", td$term)), n_A_vars)
  expect_identical(sum(grepl("^sigma_trend\\[", td$term)),
                   sum(grepl("^sigma_trend\\[", variables(fit))))

  # `tidy()` reports user-facing names. `b[1]` is the program's name
  # for `b_elev`. `z_1` is the non-centred helper of the group
  # deviations.
  expect_identical(grep("^b\\[", td$term, value = TRUE), character(0))
  expect_identical(grep("^z_1\\[", td$term, value = TRUE), character(0))
  # One intercept, under one name.
  expect_lte(sum(td$term %in% c("Intercept", "b_Intercept")), 1L)
})


test_that("conditional_effects cuts the interaction by region", {
  # The slope on `elev` differs by region. A design that dropped the
  # interaction returns one slope for every region. The grid holds
  # `elev` at two values in each region.
  withr::local_options(marginaleffects_model_classes = "mvgam")
  grid <- expand.grid(
    elev = c(-1, 1),
    region = factor(region_levels, levels = region_levels),
    stringsAsFactors = FALSE
  )
  grid$series <- factor(series_levels[1L], levels = series_levels)
  grid$time <- time_vals[1L]
  grid$block <- dat$block[1L]
  grid$y <- NA_real_
  ep <- posterior_epred(fit, newdata = grid, ndraws = 200L)
  est <- colMeans(ep)
  slopes <- vapply(region_levels, function(r) {
    idx <- which(as.character(grid$region) == r)
    est[idx[2L]] - est[idx[1L]]
  }, numeric(1))
  # Three distinct slopes
  expect_gt(stats::sd(slopes), 1e-6)
})


test_that("a missing covariate value is refused by name", {
  # A missing covariate value would pass through the design as an
  # `NA` prediction. The refusal names the column.
  nd <- dat
  nd$elev[1L] <- NA_real_
  err <- expect_error(
    posterior_epred(fit, newdata = nd, draw_ids = 1:5)
  )
  msg <- conditionMessage(err)
  expect_match(msg, "elev", fixed = TRUE)
  expect_false(grepl("eta", msg, fixed = TRUE))
})


test_that("conditional_effects leaves out the random-effect grouping", {
  # `block` is the random-effect grouping. Its levels are exchangeable
  # draws from a distribution. An effects panel over them would
  # present shrunk deviations as a population contrast.
  ce <- conditional_effects(fit)
  expect_false("block" %in% names(ce))
})


test_that("the random intercept is estimated over the blocks", {
  # The observation-side parameters carry brms names, taken from the
  # fit itself.
  r_cols <- grep("^r_block\\[", variables(fit), value = TRUE)

  dm_brms <- posterior::as_draws_matrix(fit)
  # Each block's mean fitted value moves with that block's own
  # deviation. A grouping resolved against the wrong column leaves the
  # two uncorrelated.
  r_hat <- vapply(sort(r_cols), function(k) mean(dm_brms[, k]),
                  numeric(1))
  ep <- colMeans(posterior_epred(fit, incl_autocor = FALSE,
                                 ndraws = 200L))
  by_block <- tapply(ep, dat$block, mean)
  expect_gt(stats::cor(unname(r_hat), as.numeric(by_block)), 0.9)
})


test_that("a decomposition's shares sum to one, draw by draw", {
  # A response's shares add to one at every horizon in every draw. A
  # normalisation of the median alone keeps the summary in [0, 1] and
  # fails here.
  fv <- fevd(fit, h = 8L, summary = FALSE)
  row_sums <- unlist(lapply(fv, function(draw) {
    vapply(draw, function(mat) rowSums(mat), numeric(8L))
  }))
  expect_true(all(abs(row_sums - 1) < 1e-10))
})


test_that("orthogonal and generalized responses are different objects", {
  # The two identifications differ when `Sigma` has correlations,
  # which `cor = TRUE` estimates.
  ir_gen <- irf(fit, h = 4L, orthogonal = FALSE, summary = FALSE)
  ir_orth <- irf(fit, h = 4L, orthogonal = TRUE, summary = FALSE)
  expect_identical(attr(ir_gen, "irf_type"), "Generalized")
  expect_identical(attr(ir_orth, "irf_type"), "Orthogonalized")
  expect_false(isTRUE(all.equal(ir_gen[[1L]], ir_orth[[1L]])))
})


test_that("the cumulative response is the running sum of the other", {
  # A cumulative response is the running sum over horizons of the
  # per-horizon response, exactly and in every draw.
  ids <- 1:10
  step <- irf(fit, h = 5L, cumulative = FALSE, draw_ids = ids,
              summary = FALSE)
  cum <- irf(fit, h = 5L, cumulative = TRUE, draw_ids = ids,
             summary = FALSE)
  expect_false(isTRUE(all.equal(step, cum)))
  for (d in seq_along(ids)) {
    for (r in seq_len(n_series)) {
      expect_equal(cum[[d]][[r]], apply(step[[d]][[r]], 2L, cumsum))
    }
  }

  # The two agree at the first horizon and separate after it
  expect_equal(cum[[1L]][[1L]][1L, ], step[[1L]][[1L]][1L, ])
  expect_gt(max(abs(cum[[1L]][[1L]][5L, ] - step[[1L]][[1L]][5L, ])),
            1e-6)
})


test_that("irf and fevd use the draws they were given", {
  # A response takes its coefficients and its innovation covariance
  # from one draw. A selected draw gives the same response alone or
  # among others.
  total <- ndraws(fit)
  ids <- c(2L, 7L, 15L)
  ir <- irf(fit, h = 4L, draw_ids = ids, summary = FALSE)
  # Draw 7 alone gives the response the second of the three gave
  expect_equal(irf(fit, h = 4L, draw_ids = 7L, summary = FALSE)[[1L]],
               ir[[2L]])
  expect_length(fevd(fit, h = 4L, ndraws = 5L, summary = FALSE), 5L)
  expect_error(irf(fit, h = 4L, ndraws = total + 1L),
               "more draws than the posterior has")
  expect_error(plot(irf(fit, h = 5L), shocks = "nonexistent"),
               "Unknown shock-response")
})


test_that("stability summarises each metric over the whole posterior", {
  st <- stability(fit)
  draws <- stability(fit, summary = FALSE)
  expect_equal(summary(draws)$Estimate, st$Estimate, tolerance = 1e-10)

  # The `future = TRUE` path returns the same draws in the same order
  expect_equal(stability(fit, future = TRUE, summary = FALSE), draws)

  metrics <- c(
    "prop_cov_offdiag", "prop_cov_diag", "prop_int", "prop_int_adj",
    "prop_int_offdiag", "prop_int_diag", "reactivity",
    "mean_return_rate", "var_return_rate"
  )
  expect_setequal(as.character(st$metric), metrics)
  # The mean return rate is the spectral radius of `A`, draw for
  # draw.
  expect_equal(draws$mean_return_rate, radii_of_A())

  # The interaction metrics, recomputed from one draw's `A`. Every
  # off-diagonal sensitivity counts. Averaging the lower triangle
  # alone missed half of an asymmetric `A`.
  for (k in c(1L, 250L)) {
    A <- draw_matrix(k, "Phi_trend[1,%d,%d]")
    sens <- abs(2 * det(A) * t(solve(A)))
    off <- mean(sens[row(A) != col(A)])
    expect_equal(draws$prop_int[k], det(A)^2)
    expect_equal(draws$prop_int_offdiag[k], off / (off + mean(diag(sens))))
    expect_equal(draws$reactivity[k], log(max(svd(A)$d)))
  }

  # The summary records each metric's binned posterior, which `plot()`
  # draws as the histogram of the draws.
  bins <- attr(st, "bin_counts")
  ref <- graphics::hist(
    draws$reactivity,
    breaks = seq(min(draws$reactivity), max(draws$reactivity),
                 length.out = 31L),
    plot = FALSE
  )
  expect_identical(bins$reactivity$counts, as.integer(ref$counts))

  expect_error(stability(fit, ndraws = ndraws(fit) + 1L),
               "more draws than the posterior has")
  # `probs` in either order, as `residual_cor()` takes them
  expect_equal(stability(fit, probs = c(0.9, 0.1)),
               stability(fit, probs = c(0.1, 0.9)))
  expect_identical(nrow(stability(fit, ndraws = 5L, summary = FALSE)), 5L)
})


test_that("incl_autocor switches conditional and marginal predictions", {
  # The argument chooses between the fitted latent state and the
  # trend's deterministic submodel, which differ on a VAR fit.
  ids <- 1:20
  cond_ep <- posterior_epred(fit, draw_ids = ids, incl_autocor = TRUE)
  marg_ep <- posterior_epred(fit, draw_ids = ids, incl_autocor = FALSE)
  # The two differ by more than rounding
  expect_gt(max(abs(colMeans(cond_ep) - colMeans(marg_ep))), 1e-3)

  cond_pp <- posterior_predict(fit, draw_ids = ids, incl_autocor = TRUE)
  marg_pp <- posterior_predict(fit, draw_ids = ids, incl_autocor = FALSE)
  expect_false(isTRUE(all.equal(cond_pp, marg_pp)))
})


test_that("the summary prints every coefficient the model estimated", {
  # Every coefficient the sampler estimated appears in the summary.
  # The design has an intercept, a slope, two region contrasts and two
  # interaction columns.
  smry <- summary(fit)
  blocks <- grep("^(fixed|trend_fixed)$", names(smry),
                 value = TRUE)
  expect_gt(length(blocks), 0L)
  shown <- unlist(lapply(blocks, function(k) rownames(smry[[k]])))

  coefs <- sub("^b_", "", grep("^b_", variables(fit), value = TRUE))
  coefs <- coefs[!grepl("_trend$", coefs)]
  expect_gte(length(coefs), 6L)
  expect_identical(paste(setdiff(coefs, shown), collapse = ", "), "")
})


test_that("pp_check draws one panel per group", {
  # `group` names a column of the data. The plot has one panel per
  # level of it.
  for (ty in c("dens_overlay_grouped", "stat_grouped")) {
    p <- pp_check(fit, type = ty, ndraws = 20L, group = "region")
    # The panels, taken from the plot's own facet spec
    built <- ggplot2::ggplot_build(p)
    expect_setequal(
      as.character(built$layout$layout$group),
      region_levels
    )
  }
  # `intervals` and `ribbon`, the types taking `x`, stay untested
  # here. They reach `bayesplot::ppc_intervals()`, which raises a
  # ggplot2 deprecation warning.
})


test_that("the plotting methods render for a VAR fit", {
  expect_drawn(pp_check(fit, ndraws = 20L))
  for (ty in c("residuals", "trend", "series", "re")) {
    expect_drawn(plot(fit, type = ty))
  }
  panel <- as.character(mcmc_plot(fit)$data$parameter)
  # The default panel shows the interpretable parameters under the
  # names `variables()` lists.
  sigma_1 <- paste0("sigma_trend[", fitted_series_levels(fit)[1L], "]")
  expect_true(all(
    c("b_elev", "sd_block__Intercept", sigma_1) %in% panel
  ))
  # The per-level deviations stay out of the default panel.
  # `variable` selects them.
  expect_false(any(grepl("^r_block", panel)))
  expect_s3_class(
    pairs(fit, variable = c("sigma", sigma_1)),
    "bayesplot_grid"
  )
  expect_drawn(plot(conditional_effects(fit))[[1L]])

  # Each plot type this fit lacks the structure for refuses it by
  # naming that structure
  expect_error(plot(fit, type = "smooths"), "require a smooth term")
  expect_error(plot(fit, type = "factors"), "requires a latent factor model")
  expect_error(plot(fit, type = "latent_state"), "has no closure units")
})

test_that("find_predictors keeps the series axis among the variables", {
  # `series` indexes the trend axis. It is a grid variable and is kept
  # out of the predictors a slope or a contrast is taken over.
  library(marginaleffects)
  options("marginaleffects_model_classes" = "mvgam")
  preds <- insight::find_predictors(fit)$conditional
  expect_false("series" %in% preds)
  expect_true("series" %in% insight::find_variables(fit, flatten = TRUE))
  # The observation formula's terms are predictors
  expect_true(all(c("elev", "region") %in% preds))
  # The random-effect grouping is listed under `random`. A
  # conditional `block` would be offered to `avg_slopes()` as a term
  # to contrast over.
  expect_false("block" %in% preds)
  all_eff <- insight::find_predictors(fit, effects = "all")
  expect_true("block" %in% all_eff$random)
  expect_identical(insight::find_random(fit)$random, "block")
})


test_that("lfo_cv scores this fit on its own time axis", {
  # `lfo_cv()` refits at a window and steps forward on importance
  # ratios. Its evaluated occasions are in the user's numbering, which
  # starts at 3 here.
  lfo <- SM(lfo_cv(fit, min_t = 55L, fc_horizon = 1L, silent = 2))

  expect_identical(as.integer(lfo$eval_timepoints), 56:62)
  expect_true(all(is.finite(lfo$elpds)))
  # Refits happen at occasions of the training grid
  expect_true(all(lfo$refits_at %in% time_vals))

  # The refit threshold is the adaptive one,
  # `min(1 - 1/log10(S), 0.7)`, 2/3 at 1000 draws
  expect_equal(lfo$pareto_k_threshold,
               mvgam:::mvgam_ps_khat_threshold(
                 as.integer(posterior::ndraws(fit))
               ))
  expect_true(lfo$pareto_k_threshold_adaptive)
})


test_that("lfo_cv can report a forecast-based score", {
  # A proper score needs a forecast from each window
  lfo <- SM(lfo_cv(fit, min_t = 55L, fc_horizon = 1L, score = "crps",
                   silent = 2))
  expect_identical(as.integer(lfo$eval_timepoints), 56:62)
  expect_identical(length(lfo$scores$crps), length(lfo$eval_timepoints))
  expect_true(all(is.finite(lfo$scores$crps) & lfo$scores$crps >= 0))
})


test_that("a refit resolves the same axis the first fit recorded", {
  # `update()` resolves the axes a second time from the stored call
  upd <- SM(update(
    fit, prior = prior(exponential(3), class = sigma_trend),
    chains = 2L, iter = 600L, warmup = 300L, silent = 2
  ))
  ax0 <- mvgam:::mvgam_axes(fit)
  ax1 <- mvgam:::mvgam_axes(upd)
  expect_identical(as.character(ax1$series$levels),
                   as.character(ax0$series$levels))
  expect_identical(as.integer(ax1$time$values),
                   as.integer(ax0$time$values))
  expect_identical(ax1$grain, ax0$grain)
  # Each row maps to the same trend cell
  expect_identical(as.integer(upd$standata$obs_trend_series),
                   as.integer(fit$standata$obs_trend_series))
  expect_identical(as.integer(upd$standata$obs_trend_time),
                   as.integer(fit$standata$obs_trend_time))
})


test_that("partial residual correlations invert the stationary covariance", {
  # A VAR gives each series its own dimension. Its stationary
  # covariance is invertible. Each draw's partial correlations are the
  # scaled inverse. The summary averages them on the Fisher-z scale.
  ks <- seq_len(nrow(dm_all))
  z <- Reduce(`+`, lapply(ks, function(k) {
    prec <- solve(stationary_cov(k))
    p_k <- -prec / sqrt(outer(diag(prec), diag(prec)))
    diag(p_k) <- 0
    atanh(p_k)
  })) / length(ks)
  P <- tanh(z)
  diag(P) <- 1
  rc <- residual_cor(fit, partial = TRUE)
  expect_identical(rownames(rc$prec), series_levels)
  expect_equal(unname(rc$prec), P, tolerance = 1e-6)
})


test_that("hypothesis reaches every parameter the fit reports", {
  # A series-indexed parameter is named by its series, in level order
  expect_hypothesis_reaches(fit, c("b_Intercept", "sigma",
                                   "Phi_trend[1,willow,ash]", "b_elev",
                                   "sd_block__Intercept"))
})


test_that("a forecast over observed occasions is refused", {
  # A frame naming no occasion beyond the training grid has nothing to
  # forecast. The refusal names the times it was given.
  h <- 3L
  inside <- make_future(h)
  inside$time <- rep(time_vals[seq_len(h)], times = n_series)
  err <- expect_error(
    forecast(fit, newdata = inside, ndraws = 20L, type = "trend"),
    "ends at or before the last training time"
  )
  expect_match(conditionMessage(err),
               paste(time_vals[seq_len(h)], collapse = ", "), fixed = TRUE)
})


test_that("the fevd plot takes a vector of series", {
  # `plot(fevds, series = 1:3, contributing = 1:3)` keeps a wide panel
  # readable. `?plot.mvgam_fevd` offers the vector form.
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  fv <- fevd(fit, h = 4L)

  # A scalar and the documented vector form are both accepted
  expect_drawn(plot(fv, series = 1L))
  expect_drawn(plot(fv, series = 1:2))
})


test_that("the irf plot takes a vector of series", {
  # `irf()` and `fevd()` summaries share one plot method
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  ir <- irf(fit, h = 4L)
  # The panels are the shocks to the processes named, in the order the
  # VAR numbers its processes. A character facet sorted them
  # alphabetically.
  labs <- attr(ir, "process_labels")
  drawn <- ggplot2::ggplot_build(plot(ir, series = 1:2))$plot$data$shock
  expect_identical(levels(drawn),
                   c(paste0(labs[1L], " -> ", labs),
                     paste0(labs[2L], " -> ", labs)))
})


test_that("series selects the end of the pair its help names", {
  # `?plot.mvgam_fevd` documents `series` as choosing the target
  # processes drawn as facets.
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  fv <- fevd(fit, h = 4L)
  drawn <- ggplot2::ggplot_build(plot(fv, series = 1L))$plot$data
  # Selecting one target leaves one panel
  expect_identical(levels(droplevels(drawn$target)),
                   attr(fv, "process_labels")[1L])
})


test_that("contributing changes what the fevd plot draws", {
  # `contributing` selects the source processes a panel draws
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  fv <- fevd(fit, h = 4L)
  one_source <- ggplot2::ggplot_build(
    plot(fv, contributing = 1L)
  )$plot$data
  expect_identical(unique(as.character(one_source$Series)),
                   attr(fv, "process_labels")[1L])
  # The one source drawn takes the whole of each bar
  expect_equal(unique(one_source$share), 1)
})


# -- Grouped VAR(2) --------------------------------------------------------
# Each group maps its lags to coefficients jointly. Only the joint
# mapping guarantees a stationary VAR(2).
set.seed(12L)
gr_lv_true <- local({
  Phi1 <- matrix(c(0.5, 0.1, 0.2, 0.3), 2L, 2L)
  Phi2 <- matrix(c(0.2, 0, -0.1, 0.1), 2L, 2L)
  one_region <- function() {
    x <- matrix(0, 130L, 2L)
    for (t in 3:130) {
      x[t, ] <- Phi1 %*% x[t - 1L, ] + Phi2 %*% x[t - 2L, ] + rnorm(2L, 0, 0.4)
    }
    x[-(1:50), ]
  }
  cbind(one_region(), one_region())
})
gr_dat <- expand.grid(time = 1:80, sp = c("a", "b"), region = c("r1", "r2"))
gr_dat$y <- rpois(nrow(gr_dat), exp(1 + as.vector(gr_lv_true)))
gr_fit <- cached_fit(
  "val_mvgam_var_grouped.rds",
  function() {
    mvgam(y ~ 1, trend_formula = ~ VAR(p = 2, gr = region, subgr = sp),
          data = gr_dat, family = poisson(), chains = 2L, iter = 1000L,
          warmup = 500L, silent = 2, backend = "cmdstanr")
  },
  key = gr_lv_true
)

test_that("a grouped VAR(2) is stationary, block by block", {
  dm <- posterior::as_draws_matrix(gr_fit$fit)
  phi <- function(r, lag) {
    matrix(as.numeric(dm[r, sprintf("Phi_trend[%d,%d,%d]", lag,
                                     rep(1:4, 4), rep(1:4, each = 4))]), 4L)
  }
  modulus <- vapply(seq_len(nrow(dm)), function(r) {
    comp <- rbind(cbind(phi(r, 1L), phi(r, 2L)), cbind(diag(4), diag(0, 4)))
    max(Mod(eigen(comp, only.values = TRUE)$values))
  }, numeric(1L))
  expect_lt(max(modulus), 1)
  # The groups do not interact
  cross <- vapply(seq_len(nrow(dm)), function(r) {
    max(abs(c(phi(r, 1L)[1:2, 3:4], phi(r, 1L)[3:4, 1:2],
              phi(r, 2L)[1:2, 3:4], phi(r, 2L)[3:4, 1:2])))
  }, numeric(1L))
  expect_identical(max(cross), 0)
  # The start covariance is the stationary covariance of the companion
  # form. The program places each group's autocovariances. The
  # Lyapunov equation solved in R over all four series is the
  # definition.
  worst <- max(vapply(round(seq(1, nrow(dm), length.out = 50L)),
                      function(r) {
    comp <- rbind(cbind(phi(r, 1L), phi(r, 2L)),
                  cbind(diag(4), diag(0, 4)))
    S <- matrix(as.numeric(dm[r, sprintf("Sigma_trend[%d,%d]", rep(1:4, 4),
                                         rep(1:4, each = 4))]), 4L)
    Q <- diag(0, 8)
    Q[1:4, 1:4] <- S
    Omega <- matrix(solve(diag(64) - kronecker(comp, comp), as.vector(Q)),
                    8L)
    stored <- matrix(as.numeric(dm[r, sprintf(
      "Omega_trend[%d,%d]", rep(1:8, 8), rep(1:8, each = 8)
    )]), 8L)
    max(abs(Omega - stored))
  }, numeric(1L)))
  expect_lt(worst, 1e-6)
  # Each group's matrix is its own block of Phi_trend. The default is
  # the mean of the groups' matrices in every draw.
  r1 <- posterior_transition_matrix(gr_fit, groups = "r1", lag = 2L,
                                    summary = FALSE)
  r2 <- posterior_transition_matrix(gr_fit, groups = "r2", lag = 2L,
                                    summary = FALSE)
  pooled <- posterior_transition_matrix(gr_fit, lag = 2L, summary = FALSE)
  expect_equal(as.numeric(r2[7L, , ]), as.numeric(phi(7L, 2L)[3:4, 3:4]))
  expect_equal(as.numeric(pooled), as.numeric((r1 + r2) / 2))
  expect_identical(attr(pooled, "group_label"), "mean over groups")
})


cat("\nDone.\n")
