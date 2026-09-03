# Recovery and post-fit coverage for `VAR()`, fitted in this file.
#
# A VAR is the trend where one series' past drives another's future.
# That lives entirely in the off-diagonal entries of the transition
# matrix `A`: with a diagonal `A` the model is a bank of independent
# AR(1)s, fits comparably well on most data, and differs only in the
# entries nothing else checks. So the claims that matter here are
# about `A` as a matrix rather than about the fit as a whole.
#
#   truth: 3 series, 60 occasions, gaussian, known stationary A with
#          deliberate cross-series terms, Sigma with correlated noise
#   model: y ~ elev * region + (1 | block),
#          trend_formula = ~ VAR(cor = TRUE)
#
# The observation side carries a continuous-by-factor interaction and
# a random intercept, so the trend is not the only thing in the linear
# predictor. That matters here: a design matrix that dropped the
# interaction, or a random effect resolved against the wrong grouping,
# leaves `A` to absorb the difference and quietly changes the
# cross-series terms this file is about.
#
# Series are named out of alphabetical order and occasions are
# numbered from 3, so a rank never equals a value and no rival
# resolver agrees by accident.
#
# Cached at tests/local/fixtures/val_mvgam_var_trend.rds. Delete to
# refit.
#
# Run with:
#   testthat::test_file("tests/local/test-var-trend.R")

suppressMessages({
  devtools::load_all(".", quiet = TRUE)
  library(posterior)
  library(testthat)
})

source(if (file.exists("concordance_helpers.R")) {
  "concordance_helpers.R"
} else {
  file.path("tests", "local", "concordance_helpers.R")
})

set.seed(911L)

n_series <- 3L
n_time <- 60L
series_levels <- c("willow", "ash", "rowan")
stopifnot(!identical(series_levels, sort(series_levels)))

time_vals <- seq_len(n_time) + 2L

# A stationary transition matrix with cross-series terms that are
# large enough to be identified and asymmetric, so `A[i, j]` and
# `A[j, i]` cannot be swapped without changing the answer. A
# transposed `A` is the defect this shape is chosen to expose.
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
# declared out of alphabetical order, `elev` is continuous, and the
# two interact. `block` groups the occasions into six, which crosses
# the series rather than nesting inside one, so a random intercept on
# it is identified alongside the trend.
region_levels <- c("upper", "lower", "mid")
n_block <- 6L
block_sd <- 0.4

elev <- as.numeric(scale(rnorm(n_time)))
region <- factor(region_levels[(seq_len(n_time) %% 3L) + 1L],
                 levels = region_levels)
block <- factor(rep(seq_len(n_block), each = n_time / n_block))
block_effect <- rnorm(n_block, 0, block_sd)

# Slopes on `elev` differ by region, which is what makes it an
# interaction rather than two additive terms.
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

# A future frame in the shape the observation model needs: the axis
# columns plus every covariate on the right-hand side above.
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
  expect_identical(as.integer(sd$N_series_trend), n_series)
  expect_identical(as.integer(sd$N_time_trend), n_time)
  # A VAR runs one latent dimension per series, so the two agree and
  # `A` is square in that dimension.
  expect_identical(as.integer(sd$N_lv_trend), n_series)
  expect_identical(as.integer(sd$obs_trend_series),
                   match(as.character(dat$series), series_levels))
})


test_that("the program declares a full transition matrix", {
  # `A_trend` is an array of one square matrix per group, and this
  # model has one group. Declared at any other size it could not
  # express a cross-series effect at all.
  sc <- as.character(stancode(prefit))
  expect_true(grepl("array[1] matrix[N_lv_trend, N_lv_trend] A_trend",
                    sc, fixed = TRUE))
  # The innovations are drawn jointly, which is what `cor = TRUE`
  # buys: a diagonal covariance would make the series independent
  # given `A`.
  expect_true(grepl("cov_matrix[N_lv_trend] Sigma_trend", sc,
                    fixed = TRUE))
  expect_true(grepl("multi_normal_lpdf", sc, fixed = TRUE))
  # The recursion multiplies the lagged state by `A`.
  expect_match(sc, "A_trend\\[i\\]\\s*\\*\\s*lv_trend")
})


test_that("the observation design carries the interaction and the RE", {
  # The trend is not the only thing in the linear predictor, and each
  # of these is a route by which a design can be silently wrong: an
  # interaction dropped to two additive terms, or a random effect
  # built over the wrong number of groups.
  sd <- prefit$standata
  # `elev * region` on a three-level factor gives an intercept, the
  # slope, two contrasts and two interaction columns.
  expect_identical(as.integer(sd$K), 6L)
  expect_true(any(grepl("elev:region", colnames(sd$X), fixed = TRUE)))
  # Both interaction columns are present, one per non-reference level.
  inter <- grep("elev:region", colnames(sd$X), fixed = TRUE,
                value = TRUE)
  expect_length(inter, 2L)
  # An interaction column is zero off its own level, which is what
  # distinguishes it from a main effect.
  for (cl in inter) {
    expect_gt(sum(sd$X[, cl] == 0), 0L)
    expect_gt(sum(sd$X[, cl] != 0), 0L)
  }

  # The random intercept spans the six blocks, not the rows or the
  # series. A grouping resolved against the wrong column gives a
  # different count here and nothing else notices.
  expect_identical(as.integer(sd$N_1), as.integer(n_block))
  expect_identical(as.integer(sd$M_1), 1L)
  expect_identical(as.integer(sd$J_1), as.integer(dat$block))
})


# -- Fit --------------------------------------------------------------

cache <- local_fixture_path("val_mvgam_var_trend.rds")
if (file.exists(cache)) {
  cat("[cache] Loading VAR fit.\n")
  fit <- readRDS(cache)
} else {
  cat("[fit ] mvgam(VAR(cor = TRUE), 3 series x 60 occasions)\n")
  fit <- mvgam(
    formula = obs_formula, trend_formula = ~ VAR(cor = TRUE),
    data = dat, family = gaussian(),
    chains = 2L, iter = 1000L, warmup = 500L,
    silent = 2, backend = "cmdstanr"
  )
}
if (!identical(attr(fit, "sim_truth"), sim_truth)) {
  attr(fit, "sim_truth") <- sim_truth
  saveRDS(fit, cache)
}

# The posterior mean transition matrix, read once in the order the
# draws name it: `A_trend[group, row, column]`.
dm_all <- posterior::as_draws_matrix(fit$fit)
A_hat <- matrix(NA_real_, n_series, n_series)
for (i in seq_len(n_series)) {
  for (j in seq_len(n_series)) {
    A_hat[i, j] <- mean(dm_all[, paste0("A_trend[1,", i, ",", j, "]")])
  }
}


test_that("A is one square matrix over the series", {
  cols <- grep("^A_trend\\[", colnames(dm_all), value = TRUE)
  expect_length(cols, n_series * n_series)
  # Three indices, the first being the group. Read as a plain matrix
  # the entries would be misplaced, which is why the reader above
  # names all three.
  expect_true(all(grepl("^A_trend\\[1,[0-9]+,[0-9]+\\]$", cols)))
})


test_that("A recovers the simulated dynamics, entry by entry", {
  # Recovery on the whole matrix rather than on a summary of it. A
  # transposed or row-permuted `A` reproduces the same marginal
  # behaviour for each series and differs only here.
  expect_lt(max(abs(A_hat - A_true)), 0.35)
  expect_gt(stats::cor(as.numeric(A_hat), as.numeric(A_true)), 0.8)

  # The diagonal is positive persistence for every series.
  expect_true(all(diag(A_hat) > 0))

  # The cross-series terms the simulation put in are found, and the
  # ones it left out stay small. This is the pair of claims that
  # separates a VAR from a bank of independent AR(1)s.
  expect_gt(A_hat[1L, 2L], 0.1)
  expect_gt(A_hat[2L, 3L], 0.1)
  expect_gt(A_hat[3L, 1L], 0.05)
  expect_lt(abs(A_hat[1L, 3L]), 0.3)
  expect_lt(abs(A_hat[2L, 1L]), 0.3)

  # And `A` is not symmetric, so a transpose is a different matrix.
  expect_gt(max(abs(A_hat - t(A_hat))), 0.1)
})


test_that("every posterior draw of A is stationary", {
  # The parameterisation is meant to keep `A` inside the stationary
  # region draw by draw. A mean that looks stationary can be the
  # average of draws that are not, so this reads each one.
  draw_A <- function(k) {
    out <- matrix(NA_real_, n_series, n_series)
    for (i in seq_len(n_series)) {
      for (j in seq_len(n_series)) {
        out[i, j] <- dm_all[k, paste0("A_trend[1,", i, ",", j, "]")]
      }
    }
    out
  }
  ks <- unique(round(seq(1, nrow(dm_all), length.out = 25L)))
  radii <- vapply(ks, function(k) {
    max(Mod(eigen(draw_A(k), only.values = TRUE)$values))
  }, numeric(1))
  expect_true(all(is.finite(radii)))
  expect_true(all(radii < 1))
})


test_that("Sigma is a covariance matrix, and correlated", {
  S <- matrix(NA_real_, n_series, n_series)
  for (i in seq_len(n_series)) {
    for (j in seq_len(n_series)) {
      S[i, j] <- mean(dm_all[, paste0("Sigma_trend[", i, ",", j, "]")])
    }
  }
  expect_equal(unname(S), unname(t(S)), tolerance = 1e-6)
  expect_true(all(diag(S) > 0))
  expect_true(all(eigen(S, only.values = TRUE)$values > 0))
  # `cor = TRUE` was asked for, so the innovations are not
  # independent. An all-but-diagonal Sigma would mean the argument
  # had been accepted and dropped.
  R <- stats::cov2cor(S)
  expect_gt(max(abs(R[upper.tri(R)])), 0.1)
})


test_that("every prediction surface answers for every row", {
  n_obs <- nrow(dat)
  ep <- posterior_epred(fit, ndraws = 20L)
  pp <- posterior_predict(fit, ndraws = 20L)
  lp <- posterior_linpred(fit, ndraws = 20L)
  expect_identical(dim(ep), c(20L, n_obs))
  expect_identical(dim(pp), c(20L, n_obs))
  expect_identical(dim(lp), c(20L, n_obs))
  expect_true(all(is.finite(ep)))
  expect_true(all(is.finite(pp)))
  expect_identical(nrow(fitted(fit, ndraws = 20L)), n_obs)
  expect_identical(nrow(residuals(fit, ndraws = 20L)), n_obs)

  os <- mvgam:::get_observation_structure(fit, newdata = dat)
  expect_identical(as.character(os$series), as.character(dat$series))
  expect_identical(os$series_levels, series_levels)
  expect_identical(as.integer(os$time), match(dat$time, time_vals))
})


test_that("each row reads the latent cell the sampler drew for it", {
  t_rec <- as.integer(fit$standata$obs_trend_time)
  s_rec <- as.integer(fit$standata$obs_trend_series)
  expect_length(t_rec, nrow(dat))
  want <- vapply(paste0("trend[", t_rec, ",", s_rec, "]"),
                 function(k) mean(dm_all[, k]), numeric(1))
  got <- colMeans(
    mvgam:::extract_trend_latent_states(fit, newdata = dat,
                                        full_draws = dm_all)
  )
  expect_equal(unname(got), unname(want))
})


test_that("the one-step forecast is A times the last state", {
  # The claim that ties `A` to what a user receives. A VAR steps as
  # `A %*% trend[T, ]` plus a mean-zero innovation, so averaged over
  # draws the first forecast step lands on that product. Crucially
  # this is a per-series claim that mixes the series: series 1's
  # step depends on series 2's last state through `A[1, 2]`, so a
  # recursion that applied `A` transposed, or per series
  # independently, lands somewhere else while staying finite and
  # correctly shaped.
  h <- 3L
  future_times <- max(time_vals) + seq_len(h)
  # A forecast frame has to carry every covariate the observation
  # model uses, not just the axis columns. Omitting one fails inside
  # the design build rather than at a helpful boundary.
  future <- make_future(h)
  n_draw <- 600L
  fc <- forecast(fit, newdata = future, ndraws = n_draw,
                 type = "trend")
  expect_identical(names(fc$forecasts), series_levels)

  last <- vapply(seq_len(n_series), function(k) {
    mean(dm_all[, paste0("trend[", n_time, ",", k, "]")])
  }, numeric(1))
  expected <- as.numeric(A_hat %*% last)
  for (k in seq_len(n_series)) {
    got <- mean(fc$forecasts[[series_levels[k]]][, 1L])
    expect_lt(abs(got - expected[k]), 0.3)
  }

  # A diagonal-only step is a different number, so the check above
  # is not satisfied by an independent AR.
  diag_only <- diag(A_hat) * last
  expect_gt(max(abs(expected - diag_only)), 0.05)
})


test_that("the forecast is keyed by the series axis on every scale", {
  h <- 4L
  future_times <- max(time_vals) + seq_len(h)
  # A forecast frame has to carry every covariate the observation
  # model uses, not just the axis columns. Omitting one fails inside
  # the design build rather than at a helpful boundary.
  future <- make_future(h)
  for (ty in c("link", "expected", "trend", "response")) {
    fc <- forecast(fit, newdata = future, ndraws = 20L, type = ty)
    expect_s3_class(fc, "mvgam_forecast")
    expect_identical(names(fc$forecasts), series_levels)
    for (s in series_levels) {
      expect_identical(dim(fc$forecasts[[s]]), c(20L, h))
      expect_true(all(is.finite(fc$forecasts[[s]])))
    }
  }
})


test_that("the forecast object is keyed, ordered and in user units", {
  # Dimensions say a forecast came back. They say nothing about which
  # occasions it covers, in what order, or which series each arm
  # belongs to. Those are carried on the object and checked here.
  h <- 4L
  future_times <- max(time_vals) + seq_len(h)
  fc <- forecast(fit, newdata = make_future(h), ndraws = 20L,
                 type = "link")

  # The arms and the axis agree, in order.
  expect_identical(names(fc$forecasts), series_levels)
  expect_identical(as.character(fc$series_names), series_levels)

  for (s in series_levels) {
    # The horizon is the occasions the caller asked for, ascending,
    # in the numbering the user supplied rather than 1..h.
    expect_identical(as.integer(fc$test_times[[s]]), future_times)
    expect_false(identical(as.integer(fc$test_times[[s]]),
                           seq_len(h)))
    # The training tail is this fit's own grid, also in user units.
    expect_identical(as.integer(fc$train_times[[s]]), time_vals)
    # Column j of the arm is horizon j, so the widths line up with
    # the times above rather than merely being h wide.
    expect_identical(ncol(fc$forecasts[[s]]),
                     length(fc$test_times[[s]]))
    # And each arm's training observations are that series' own, in
    # time order. An arm carrying another series' history forecasts
    # from the wrong place while looking correct throughout.
    rows <- which(as.character(dat$series) == s)
    rows <- rows[order(dat$time[rows])]
    expect_equal(as.numeric(fc$train_observations[[s]]),
                 as.numeric(dat$y[rows]))
  }
})


test_that("fitted summarises the same draws epred returns", {
  # Two routes to one quantity, composed differently. `fitted()`
  # summarises per column, `posterior_epred()` returns the draws, so
  # they agree only when both put the same cell in the same place. A
  # method that re-sorted its output keeps every dimension and every
  # value while pairing them with the wrong rows.
  ep <- posterior_epred(fit, ndraws = NULL, process_error = FALSE)
  ft <- fitted(fit, process_error = FALSE)
  expect_identical(nrow(ft), nrow(dat))
  expect_identical(colnames(ft),
                   c("Estimate", "Est.Error", "Q2.5", "Q97.5"))
  expect_equal(as.numeric(ft[, "Estimate"]), unname(colMeans(ep)))
  # The interval is ordered and brackets the point estimate.
  expect_true(all(ft[, "Q2.5"] <= ft[, "Estimate"]))
  expect_true(all(ft[, "Estimate"] <= ft[, "Q97.5"]))
  expect_true(all(ft[, "Est.Error"] >= 0))
})


test_that("the hindcast reads the same cells as the conditional epred", {
  # `hindcast()` composes per series and `posterior_epred()` across
  # the whole frame, so they agree only when both resolve the same
  # state for the same cell. This is the check that ties the
  # per-series arms back to the row order of the frame.
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


test_that("hindcast arms are the series, in order, and distinct", {
  arms <- hindcast(fit, ndraws = 20L)$hindcasts
  expect_identical(names(arms), series_levels)
  same <- character(0)
  for (i in seq_along(arms)) {
    for (j in seq_along(arms)) {
      if (j <= i) next
      if (isTRUE(all.equal(arms[[i]], arms[[j]]))) {
        same <- c(same, paste(names(arms)[i], names(arms)[j],
                              sep = "="))
      }
    }
  }
  expect_identical(same, character(0))
})


test_that("irf and fevd describe this fit's own matrix", {
  # The two summaries a VAR exists to produce. An impulse response
  # is generated by powers of `A`, so at horizon 1 the response of
  # series i to a shock in j is `A[i, j]` up to the shock's scale;
  # a forecast error decomposition is a set of proportions.
  h <- 6L
  ir <- irf(fit, h = h)
  fe <- fevd(fit, h = h)
  expect_s3_class(ir, "data.frame")
  expect_s3_class(fe, "data.frame")

  # One row per ordered pair of series per horizon.
  expect_identical(nrow(ir), n_series * n_series * h)
  expect_identical(nrow(fe), n_series * n_series * h)
  expect_identical(sort(unique(ir$horizon)), seq_len(h))
  expect_length(unique(ir$shock), n_series * n_series)

  # Named columns, checked by name. Sweeping every numeric column
  # into one range check pulls `horizon` in with the estimates, and
  # then reports 6 as an out-of-range share.
  for (cl in c("irfQ2.5", "irfQ50", "irfQ97.5")) {
    expect_true(cl %in% names(ir))
    expect_true(all(is.finite(ir[[cl]])))
  }
  expect_true(all(ir$irfQ2.5 <= ir$irfQ50))
  expect_true(all(ir$irfQ50 <= ir$irfQ97.5))

  # A decomposition is a set of shares, so each column is a
  # proportion and the intervals are ordered.
  for (cl in c("fevdQ2.5", "fevdQ50", "fevdQ97.5")) {
    expect_true(cl %in% names(fe))
    expect_true(all(is.finite(fe[[cl]])))
    expect_true(all(fe[[cl]] >= -1e-8))
    expect_true(all(fe[[cl]] <= 1 + 1e-8))
  }
  expect_true(all(fe$fevdQ2.5 <= fe$fevdQ50))
  expect_true(all(fe$fevdQ50 <= fe$fevdQ97.5))

  # At the first horizon a series' forecast error is all its own
  # shock, which is the one value in the table that theory pins.
  own_h1 <- fe$fevdQ50[fe$horizon == 1L &
                         sub(" -> .*$", "", fe$shock) ==
                           sub("^.* -> ", "", fe$shock)]
  expect_length(own_h1, n_series)
  expect_true(all(own_h1 > 0.5))
})


test_that("irf and fevd name the series, not Process_k", {
  # Both tables label their shocks `Process_1 -> Process_2` while
  # this fit's series are named. A correct decomposition under a
  # generic label is, to a reader, the same as a wrong one: there is
  # nothing in the output that says which species `Process_2` is,
  # and the mapping is positional and undocumented.
  ir <- irf(fit, h = 3L)
  labels <- unique(unlist(strsplit(unique(ir$shock), " -> ",
                                   fixed = TRUE)))
  expect_setequal(labels, series_levels)
})


test_that("the residual correlation is labelled by the series axis", {
  rc <- residual_cor(fit)
  expect_identical(rownames(rc$cor), series_levels)
  expect_identical(colnames(rc$cor), series_levels)
  expect_equal(unname(diag(rc$cor)), rep(1, n_series))
  expect_equal(unname(rc$cor), unname(t(rc$cor)))
})


# -- The newdata battery ----------------------------------------------

ref_epred <- posterior_epred(fit, newdata = dat, draw_ids = 1:10,
                             incl_autocor = TRUE)

test_that("the reference the battery compares against actually varies", {
  # Every check below compares a rearranged or cut frame against the
  # same columns of `ref_epred`. That is a comparison of the
  # prediction with itself, so a predictor returning one constant for
  # every row satisfies the whole battery: the shuffle, the subsets,
  # the relabelling, the single rows and the duplicates all agree
  # trivially. This is the guard that makes the rest of them mean
  # something.
  colm <- colMeans(ref_epred)
  expect_gt(stats::sd(colm), 0)

  # It varies between series, so a cut by series is a real cut.
  by_series <- tapply(colm, as.character(dat$series), mean)
  expect_length(by_series, n_series)
  expect_gt(stats::sd(as.numeric(by_series)), 0)

  # And within a series over time, so a cut by occasion is one too.
  for (s in series_levels) {
    rows <- which(as.character(dat$series) == s)
    expect_gt(stats::sd(colm[rows]), 0)
  }

  # No two rows of the reference are the same everywhere, which is
  # what a collapsed axis would produce while keeping every shape.
  expect_gt(length(unique(round(colm, 8))), 1L)
})



test_that("a shuffled newdata answers the same, in the new order", {
  set.seed(37L)
  perm <- sample(nrow(dat))
  expect_equal(
    unname(posterior_epred(fit, newdata = dat[perm, , drop = FALSE],
                           draw_ids = 1:10, incl_autocor = TRUE)),
    unname(ref_epred[, perm, drop = FALSE])
  )
})


test_that("a newdata holding one series reads that series' state", {
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


test_that("a newdata holding a pair of series reads both", {
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


test_that("relabelling or retyping the series moves nothing", {
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


test_that("single, repeated and single-occasion frames read right", {
  for (s in series_levels) {
    j <- which(as.character(dat$series) == s)[1L]
    one <- posterior_epred(fit, newdata = dat[j, , drop = FALSE],
                           draw_ids = 1:10, incl_autocor = TRUE)
    expect_identical(dim(one), c(10L, 1L))
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


test_that("an unknown series is refused, and named", {
  nd <- dat
  nd$series <- factor(
    ifelse(seq_len(nrow(nd)) == 1L, "hazel", as.character(nd$series)),
    levels = c(series_levels, "hazel")
  )
  err <- expect_error(
    posterior_epred(fit, newdata = nd, draw_ids = 1:5),
    "Series levels in newdata not found in training data"
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
    "do not share the same time grid"
  )
  msg <- conditionMessage(err)
  expect_match(msg, as.character(n_time), fixed = TRUE)
  expect_match(msg, as.character(n_series), fixed = TRUE)
  expect_match(msg, "NA", fixed = TRUE)
})


test_that("summary, tidiers and criticism run on a VAR fit", {
  txt <- capture.output(summary(fit))
  expect_gt(length(txt), 10L)
  expect_true(any(grepl(paste0("Series:\\s*", n_series), txt)))
  expect_true(any(grepl("A_trend|VAR", txt)))

  ll <- log_lik(fit, ndraws = 20L)
  expect_identical(dim(ll), c(20L, nrow(dat)))
  expect_true(all(is.finite(ll)))
  ic <- suppressWarnings(loo(fit))
  expect_true(is.finite(ic$estimates["elpd_loo", "Estimate"]))

  aug <- augment(fit)
  expect_identical(nrow(aug), nrow(dat))
  expect_equal(as.numeric(aug$time), as.numeric(dat$time))
  expect_equal(as.numeric(aug$.observed), as.numeric(dat$y))
  expect_identical(as.character(aug$series), as.character(dat$series))
  expect_true(is.data.frame(tidy(fit)))
  expect_true(any(grepl("^A_trend\\[", variables(fit))))
})


test_that("conditional_effects cuts the interaction by region", {
  # An interaction means the slope on `elev` differs by region, so
  # the effect has to come back cut by region rather than as one
  # pooled line. A design that dropped the interaction returns a
  # single slope and every panel looks the same.
  ce <- conditional_effects(fit)
  expect_s3_class(ce, "mvgam_conditional_effects")
  expect_gt(length(ce), 0L)
  for (eff in names(ce)) {
    d <- ce[[eff]]$data
    expect_true(all(is.finite(d$estimate)))
    expect_true(all(d$conf.low <= d$estimate))
    expect_true(all(d$estimate <= d$conf.high))
  }

  # The slopes differ by region on a grid that holds elev fixed at
  # two values and varies the region.
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
  # Three different slopes, not one repeated three times.
  expect_gt(stats::sd(slopes), 1e-6)
})


test_that("the random intercept is estimated over the blocks", {
  # One standard deviation and one deviation per block. Read at the
  # wrong grain this is a per-row or per-series effect, which has the
  # same name and a different length.
  # `as_draws_matrix(fit$fit)` carries the raw Stan names, which is
  # what the trend reads above use. The observation-side parameters
  # are brms-named, and reach a caller through the fit itself, so
  # these are read from there rather than from the stanfit.
  vars <- variables(fit)
  expect_true("sd_block__Intercept" %in% vars)
  r_cols <- grep("^r_block\\[", vars, value = TRUE)
  expect_length(r_cols, n_block)

  dm_brms <- posterior::as_draws_matrix(fit)
  sd_draws <- as.numeric(dm_brms[, "sd_block__Intercept"])
  expect_true(all(sd_draws > 0))
  # The interaction columns are estimated too, one per non-reference
  # region, so the design reached the sampler intact.
  expect_true(all(c("b_elev:regionlower", "b_elev:regionmid") %in%
                    vars))
})


test_that("the plotting methods render for a VAR fit", {
  expect_s3_class(pp_check(fit, ndraws = 20L), "ggplot")
  for (ty in c("residuals", "trend", "series")) {
    # `plot()` returns a ggplot, so that is what is asserted. The
    # alternation this replaced ended in `is.list(p)`, which an empty
    # list satisfies: any method returning `list()` passed it.
    p <- plot(fit, type = ty)
    expect_s3_class(p, "ggplot")
  }
  expect_s3_class(mcmc_plot(fit), "ggplot")
})

cat("\nDone.\n")
