# Recovery and post-fit coverage for a sparse multi-lag `AR()`,
# fitted in this file.
#
# `AR(p = c(1, 3, 12))` asks for three specific lags rather than the
# first twelve. That distinction is the whole point of passing a
# vector, and it is invisible to any check on shape: a model that
# quietly read the vector's maximum would emit twelve coefficients,
# fit at least as well, and differ only in which lags exist.
#
# The observation side carries a `gp()` term, which no other local
# file exercises. A Gaussian process on a covariate reaches Stan as an
# approximate basis with its own eigenvalues, so it has structure of
# its own to get wrong.
#
#   truth: 2 series, 96 occasions, gaussian, latent AR with real
#          weight at lags 1, 3 and 12 and none in between
#   model: y ~ gp(x, k = 10), trend_formula = ~ AR(p = c(1, 3, 12))
#
# Series named out of alphabetical order, occasions numbered from 3.
#
# Cached at tests/local/fixtures/val_mvgam_ar_multilag.rds.
#
# Run with:
#   testthat::test_file("tests/local/test-ar-multilag.R")

suppressMessages({
  devtools::load_all(".", quiet = TRUE)
  library(posterior)
  library(testthat)
})

# This file fits its own model and caches it beside itself, so it
# depends on no shared fixture and no build step.
cache_path <- function(name) {
  dir <- if (dir.exists("fixtures")) {
    "fixtures"
  } else {
    file.path("tests", "local", "fixtures")
  }
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  file.path(dir, name)
}

set.seed(2207L)

n_series <- 2L
n_time <- 96L
series_levels <- c("gamma", "alpha")
stopifnot(!identical(series_levels, sort(series_levels)))

time_vals <- seq_len(n_time) + 2L

# The lags the model is told to use, and the ones it must not invent.
lags <- c(1L, 3L, 12L)
absent_lags <- setdiff(seq_len(max(lags)), lags)
stopifnot(length(absent_lags) == 9L)

# Weight at exactly those lags. The lag-12 term is the seasonal one,
# which is why a vector of lags is worth having at all.
phi_true <- c(0.35, 0.25, 0.30)

latent <- matrix(0, nrow = n_time, ncol = n_series)
for (s in seq_len(n_series)) {
  e <- rnorm(n_time, 0, 0.3)
  for (t in seq_len(n_time)) {
    val <- e[t]
    for (li in seq_along(lags)) {
      if (t > lags[li]) {
        val <- val + phi_true[li] * latent[t - lags[li], s]
      }
    }
    latent[t, s] <- val
  }
}

# A smooth, non-linear function of `x` for the gp term to find.
x <- as.numeric(scale(seq_len(n_time)))
gp_true <- 1.2 * sin(x * 2) - 0.5 * x

# A covariate on the *trend* side. This is a different code path from
# an observation-side term: it is built on the trend grid, carried by
# a separate brmsfit, and read again whenever a forecast has to
# evaluate the trend beyond the training grid. No other local file
# pairs one with a dynamic trend.
temp <- as.numeric(scale(cos(seq_len(n_time) / 7)))
temp_effect <- 0.8 * sin(temp * 1.6)

dat <- data.frame(
  time = rep(time_vals, times = n_series),
  series = factor(rep(series_levels, each = n_time),
                  levels = series_levels),
  x = rep(x, times = n_series),
  temp = rep(temp, times = n_series)
)
dat$y <- as.numeric(latent) + rep(gp_true, times = n_series) +
  rep(temp_effect, times = n_series) +
  rnorm(nrow(dat), 0, 0.25)

obs_formula <- y ~ gp(x, k = 10)
trend_spec <- ~ s(temp, k = 6) + AR(p = c(1, 3, 12))

sim_truth <- list(
  n_series = n_series, n_time = n_time,
  series_levels = series_levels, time_vals = time_vals,
  lags = lags, phi_true = phi_true, latent = latent,
  gp_true = gp_true, x = x, temp = temp,
  temp_effect = temp_effect
)

# A future frame carrying both the observation covariate and the
# trend covariate. `temp_future` is what the trend smooth is
# evaluated at beyond the training grid.
make_future <- function(h, temp_future = 0) {
  ft <- max(time_vals) + seq_len(h)
  data.frame(
    time = rep(ft, times = n_series),
    series = factor(rep(series_levels, each = h),
                    levels = series_levels),
    x = rep(rep(0, h), times = n_series),
    temp = rep(rep(temp_future, length.out = h), times = n_series),
    y = NA_real_
  )
}


# -- Prefit: which lags exist, and the gp basis ----------------------

prefit <- mvgam(
  formula = obs_formula, trend_formula = trend_spec,
  data = dat, family = gaussian(), run_model = FALSE, silent = 2
)

ar_params <- function(object) {
  sc <- as.character(stancode(object))
  toks <- strsplit(sc, "[^A-Za-z0-9_]")[[1]]
  sort(unique(grep("^ar[0-9]+_trend$", toks, value = TRUE)))
}


test_that("a lag vector emits those lags and no others", {
  # The claim that makes `p = c(1, 3, 12)` different from `p = 12`.
  # A model that took the maximum would carry nine more coefficients,
  # every one of them free, and no shape check would see it.
  found <- ar_params(prefit)
  expect_identical(found, sort(paste0("ar", lags, "_trend")))
  expect_length(found, length(lags))
  for (l in absent_lags) {
    expect_false(paste0("ar", l, "_trend") %in% found)
  }
})


test_that("a dense p emits every lag up to it, which is the contrast", {
  # The same model written the other way. Without this the check
  # above could pass on a build that emits nothing at all.
  dense <- mvgam(
    formula = obs_formula, trend_formula = ~ AR(p = 12),
    data = dat, family = gaussian(), run_model = FALSE, silent = 2
  )
  found <- ar_params(dense)
  expect_length(found, 12L)
  expect_identical(found, sort(paste0("ar", seq_len(12L), "_trend")))
  # And it is a strict superset of the sparse set, so the two specs
  # really are the same family read two ways.
  expect_true(all(ar_params(prefit) %in% found))
})


test_that("the trend axis and grain are the series", {
  sd <- prefit$standata
  ax <- mvgam:::mvgam_axes(prefit)
  expect_identical(as.character(ax$series$levels), series_levels)
  expect_identical(as.integer(ax$time$values), time_vals)
  expect_identical(ax$grain, "series")
  expect_identical(as.integer(sd$N_time_trend), n_time)
  expect_identical(as.integer(sd$N_series_trend), n_series)
  expect_identical(as.integer(sd$obs_trend_series),
                   match(as.character(dat$series), series_levels))
  # The grid has to be long enough for the longest lag to be
  # identified at all.
  expect_gt(n_time, max(lags) * 2L)
})


test_that("the gp term reaches Stan as a one-dimensional basis", {
  # A `gp()` is approximated by a basis with its own eigenvalues, so
  # it carries structure beyond a column of the design matrix. The
  # dimension is the one that matters here: `gp(x)` is over a single
  # covariate, and a two-dimensional basis would be a different
  # model with the same term in the formula.
  sd <- prefit$standata
  expect_identical(as.integer(sd$Dgp_1), 1L)
  # One gp block, so no second process is hiding behind the first.
  expect_length(grep("^Dgp_[0-9]+$", names(sd), value = TRUE), 1L)

  # The basis is evaluated once per distinct covariate value, and
  # each row is pointed at its own value by `Jgp_1`. That index is
  # the whole of the term's row-level correctness, and a permutation
  # of it gives every observation another's position on the curve
  # while keeping every dimension.
  n_unique <- length(unique(dat$x))
  expect_identical(as.integer(sd$Nsubgp_1), n_unique)
  expect_length(as.integer(sd$Jgp_1), nrow(dat))
  expect_identical(sort(unique(as.integer(sd$Jgp_1))),
                   seq_len(n_unique))
  # The round trip. brms rescales a gp covariate onto its own
  # bounded support before building the basis, so the stored values
  # are an increasing affine image of `x` rather than `x` itself.
  # What has to hold regardless of that scaling is the map: rows
  # sharing a covariate value share an index, rows differing in it
  # differ, and the mapped values rise with the covariate. A
  # permuted index breaks the last of these while keeping the first
  # two and every dimension.
  mapped <- as.numeric(sd$Xgp_prior_1[, 1L])[as.integer(sd$Jgp_1)]
  expect_length(mapped, nrow(dat))
  expect_equal(stats::cor(mapped, as.numeric(dat$x)), 1)
  expect_identical(as.integer(sd$Jgp_1),
                   match(as.numeric(dat$x),
                         sort(unique(as.numeric(dat$x)))))

  # `k` basis functions, and the basis matrix is one row per distinct
  # value and one column per function.
  expect_identical(as.integer(sd$NBgp_1), 10L)
  expect_identical(dim(sd$Xgp_1),
                   c(n_unique, as.integer(sd$NBgp_1)))
  # One eigenvalue per basis function, all positive.
  expect_length(as.numeric(sd$slambda_1), as.integer(sd$NBgp_1))
  expect_true(all(as.numeric(sd$slambda_1) > 0))
})


test_that("a trend covariate is built on the trend grid", {
  # The trend side has its own design, at its own grain: one row per
  # (occasion, series), not one per observation and not one per
  # occasion. `temp` varies by occasion only, so every series has to
  # receive the same value at a given occasion, and different
  # occasions have to receive different ones.
  sd <- prefit$standata
  expect_identical(as.integer(sd$N_trend), n_time * n_series)
  expect_identical(nrow(sd$Xs_trend), n_time * n_series)
  expect_identical(nrow(sd$Zs_1_1_trend), n_time * n_series)

  # Read through `times_trend`, which is the map from a cell to its
  # design row. This is the check a transposed or renumbered map
  # fails: the covariate would be read at the wrong occasion while
  # every dimension stayed correct.
  tt <- sd$times_trend
  expect_identical(dim(tt), c(n_time, n_series))
  design_col <- as.numeric(sd$Xs_trend[, 1L])
  for (t in c(1L, 2L, 37L, n_time)) {
    rows <- as.integer(tt[t, ])
    expect_length(rows, n_series)
    # One value per occasion, shared across the series.
    expect_length(unique(round(design_col[rows], 10L)), 1L)
  }
  # Distinct occasions do not collapse onto one design value.
  per_time <- vapply(seq_len(n_time), function(t) {
    design_col[as.integer(tt[t, 1L])]
  }, numeric(1))
  expect_gt(length(unique(round(per_time, 10L))), n_time / 2)
  # And the design column tracks the covariate the user supplied,
  # occasion for occasion, up to the basis rescaling a smooth
  # applies.
  expect_equal(abs(stats::cor(per_time, temp)), 1)

  # The trend model carries its own frame at that grain, holding the
  # covariate and nothing from the observation side.
  td <- prefit$trend_model$data
  expect_identical(nrow(td), n_time * n_series)
  expect_true("temp" %in% names(td))
  expect_false("x" %in% names(td))

  # The smooth is named on the trend side, and the observation-side
  # gp is not confused with it.
  sm <- smooths(prefit)
  expect_true(any(grepl("temp", sm, fixed = TRUE)))
})


# -- Fit --------------------------------------------------------------

cache <- cache_path("val_mvgam_ar_multilag.rds")
if (file.exists(cache)) {
  cat("[cache] Loading multi-lag AR fit.\n")
  fit <- readRDS(cache)
} else {
  cat("[fit ] mvgam(AR(p = c(1, 3, 12)), gp(x), 2 x 96)\n")
  fit <- mvgam(
    formula = obs_formula, trend_formula = trend_spec,
    data = dat, family = gaussian(),
    chains = 2L, iter = 1000L, warmup = 500L,
    silent = 2, backend = "cmdstanr"
  )
}
if (!identical(attr(fit, "sim_truth"), sim_truth)) {
  attr(fit, "sim_truth") <- sim_truth
  saveRDS(fit, cache)
}

dm <- posterior::as_draws_matrix(fit$fit)


test_that("the posterior carries exactly the three lag coefficients", {
  for (l in lags) {
    cols <- grep(paste0("^ar", l, "_trend\\["), colnames(dm),
                 value = TRUE)
    expect_length(cols, n_series)
  }
  for (l in absent_lags) {
    expect_length(
      grep(paste0("^ar", l, "_trend\\["), colnames(dm), value = TRUE),
      0L
    )
  }
})


test_that("the lag coefficients recover the simulated weights", {
  # Each lag carries real weight in the simulation, so none should
  # come back at zero, and the ordering of the three is roughly the
  # ordering that was put in.
  est <- vapply(lags, function(l) {
    mean(vapply(seq_len(n_series), function(s) {
      mean(dm[, paste0("ar", l, "_trend[", s, "]")])
    }, numeric(1)))
  }, numeric(1))
  names(est) <- paste0("lag", lags)
  expect_true(all(is.finite(est)))
  # Positive dependence at every lag the simulation used.
  expect_true(all(est > 0))
  expect_lt(max(abs(est - phi_true)), 0.3)
  # The seasonal lag is real, which is the reason for the vector.
  expect_gt(est[["lag12"]], 0.1)
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
                 function(k) mean(dm[, k]), numeric(1))
  got <- colMeans(
    mvgam:::extract_trend_latent_states(fit, newdata = dat,
                                        full_draws = dm)
  )
  expect_equal(unname(got), unname(want))
})


test_that("fitted summarises the same draws epred returns", {
  ep <- posterior_epred(fit, ndraws = NULL, process_error = FALSE)
  ft <- fitted(fit, process_error = FALSE)
  expect_identical(nrow(ft), nrow(dat))
  expect_identical(colnames(ft),
                   c("Estimate", "Est.Error", "Q2.5", "Q97.5"))
  expect_equal(as.numeric(ft[, "Estimate"]), unname(colMeans(ep)))
  expect_true(all(ft[, "Q2.5"] <= ft[, "Estimate"]))
  expect_true(all(ft[, "Estimate"] <= ft[, "Q97.5"]))
})


test_that("the one-step forecast weights all three lags", {
  # An AR with lags 1, 3 and 12 carries the state forward as a
  # weighted sum at those three offsets. The recursion runs on
  # `lv_trend`, and the returned trend adds the trend-design term
  # `mu_trend` evaluated at the covariate the future frame supplies.
  #
  # That term is unknown here: `temp_future` is 0 and no draw names
  # the smooth at that value. It is, however, the same number for
  # both series, because `temp` varies by occasion and not by series.
  # So the AR part is pinned by subtracting it out: whatever the
  # forecast owes to the covariate cancels, and what is left has to
  # agree across the two series.
  #
  # A recursion that used only the first lag, or read a lag at the
  # wrong offset, leaves a residual built from the omitted terms,
  # which carry each series' own coefficients and own states. Those
  # do not agree, so the check bites.
  h <- 1L
  fc <- forecast(fit, newdata = make_future(h), ndraws = NULL,
                 type = "trend")
  resid <- vapply(seq_len(n_series), function(s) {
    contrib <- 0
    for (l in lags) {
      phi <- as.numeric(dm[, paste0("ar", l, "_trend[", s, "]")])
      st <- as.numeric(
        dm[, paste0("lv_trend[", n_time - l + 1L, ",", s, "]")]
      )
      contrib <- contrib + mean(phi * st)
    }
    mean(fc$forecasts[[series_levels[s]]][, 1L]) - contrib
  }, numeric(1))
  expect_lt(abs(diff(resid)), 0.15)

  # And the cancelled term is the trend design, so the residual has
  # to sit inside the range that design took over training. A
  # forecast that dropped `mu_trend` altogether would leave a
  # residual near zero for both series and pass the agreement check
  # above on its own.
  mu_cols <- grep("^mu_trend\\[", colnames(dm), value = TRUE)
  mu_rng <- range(colMeans(dm[, mu_cols, drop = FALSE]))
  expect_gt(mean(resid), mu_rng[1L] - 0.1)
  expect_lt(mean(resid), mu_rng[2L] + 0.1)
})


test_that("a forecast responds to the trend covariate it is given", {
  # The claim that makes a trend covariate worth having, and the one
  # place its newdata handling is load-free of the fitted states: past
  # the training grid there is no latent state to read, so the trend
  # smooth has to be evaluated at the covariate the caller supplied.
  # A forecast that ignored `temp` in newdata, or read it from the
  # training frame instead, returns a finite trajectory of the right
  # width and the same numbers whatever is asked for.
  h <- 4L
  lo <- forecast(fit, newdata = make_future(h, temp_future = -1.5),
                 ndraws = 300L, type = "trend")
  hi <- forecast(fit, newdata = make_future(h, temp_future = 1.5),
                 ndraws = 300L, type = "trend")
  for (s in series_levels) {
    expect_identical(dim(lo$forecasts[[s]]), c(300L, h))
    expect_identical(dim(hi$forecasts[[s]]), c(300L, h))
    expect_true(all(is.finite(lo$forecasts[[s]])))
    expect_true(all(is.finite(hi$forecasts[[s]])))
  }
  # The two differ, which is the whole point.
  diffs <- vapply(series_levels, function(s) {
    abs(mean(lo$forecasts[[s]]) - mean(hi$forecasts[[s]]))
  }, numeric(1))
  expect_gt(max(diffs), 0)

  # A frame with no `temp` column cannot be forecast from, and the
  # refusal names the column rather than failing on a dimension.
  bare <- make_future(h)
  bare$temp <- NULL
  err <- expect_error(
    forecast(fit, newdata = bare, ndraws = 20L, type = "trend")
  )
  expect_match(conditionMessage(err), "temp", fixed = TRUE)
})


test_that("the trend smooth is recovered on the trend side", {
  # `s(temp)` sits on the trend, so its shape is read through the
  # trend component rather than through the observation linpred.
  # Reading it at the wrong grain returns one value per observation
  # instead of one per trend cell.
  lp <- extract_component_linpred(
    mvgam_fit = fit, newdata = dat, component = "trend",
    draw_ids = 1:20, incl_latent_state = FALSE
  )
  expect_identical(dim(lp), c(20L, nrow(dat)))
  expect_true(all(is.finite(lp)))
  # The deterministic trend submodel is the temp smooth, so it tracks
  # the simulated effect and is not flat.
  det <- colMeans(lp)
  expect_gt(stats::sd(det), 0)
  first <- which(as.character(dat$series) == series_levels[1L])
  first <- first[order(dat$time[first])]
  expect_gt(abs(stats::cor(det[first], temp_effect)), 0.5)
})


test_that("the forecast object is keyed, ordered and in user units", {
  h <- 6L
  future_times <- max(time_vals) + seq_len(h)
  fc <- forecast(fit, newdata = make_future(h), ndraws = 20L,
                 type = "link")
  expect_identical(names(fc$forecasts), series_levels)
  expect_identical(as.character(fc$series_names), series_levels)
  for (s in series_levels) {
    expect_identical(as.integer(fc$test_times[[s]]), future_times)
    expect_identical(as.integer(fc$train_times[[s]]), time_vals)
    expect_identical(ncol(fc$forecasts[[s]]),
                     length(fc$test_times[[s]]))
    rows <- which(as.character(dat$series) == s)
    rows <- rows[order(dat$time[rows])]
    expect_equal(as.numeric(fc$train_observations[[s]]),
                 as.numeric(dat$y[rows]))
  }
})


test_that("the hindcast reads the same cells as the conditional epred", {
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


test_that("the gp effect is recovered and drawn over its covariate", {
  # The gp is the only observation-side term, so the marginal effect
  # of `x` is the shape it found. It has to track the simulated
  # function rather than merely existing.
  grid <- data.frame(
    x = seq(min(dat$x), max(dat$x), length.out = 40L),
    series = factor(series_levels[1L], levels = series_levels),
    time = time_vals[1L],
    temp = temp[1L],
    y = NA_real_
  )
  ep <- colMeans(posterior_epred(fit, newdata = grid, ndraws = 200L))
  truth <- 1.2 * sin(grid$x * 2) - 0.5 * grid$x
  expect_gt(stats::cor(ep, truth), 0.7)
  # The effect is not flat, which a gp that collapsed to a constant
  # would be while still returning finite values everywhere.
  expect_gt(stats::sd(ep), 0.1)

  # Both sides carry a smooth term, so both have to appear, keyed by
  # the covariate each was written over and in the order the two
  # formulas name them. A method that swept only the observation
  # formula returns one panel; one that keyed them positionally
  # returns two panels a reader cannot attribute.
  ce <- conditional_effects(fit)
  expect_s3_class(ce, "mvgam_conditional_effects")
  expect_identical(names(ce), c("x", "temp"))

  # And each panel has to carry its own function rather than the
  # other's. Two panels of the right names holding the same curve is
  # the failure a shape check cannot see.
  truths <- list(
    x = function(v) 1.2 * sin(v * 2) - 0.5 * v,
    temp = function(v) 0.8 * sin(v * 1.6)
  )
  for (nm in names(ce)) {
    d <- ce[[nm]]$data
    expect_true(all(is.finite(d$estimate)))
    expect_true(all(d$conf.low <= d$estimate))
    expect_true(all(d$estimate <= d$conf.high))
    expect_gt(stats::sd(d$estimate), 0.05)
    expect_gt(stats::cor(d$estimate, truths[[nm]](d[[nm]])), 0.7)
    # The panel is drawn over its own covariate, not over a rank or
    # over the other term's range.
    expect_equal(range(d[[nm]]), range(dat[[nm]]), tolerance = 1e-6)
  }
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



test_that("rearranged and cut newdata frames read the same cells", {
  set.seed(88L)
  perm <- sample(nrow(dat))
  expect_equal(
    unname(posterior_epred(fit, newdata = dat[perm, , drop = FALSE],
                           draw_ids = 1:10, incl_autocor = TRUE)),
    unname(ref_epred[, perm, drop = FALSE])
  )

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

  for (tv in time_vals[c(1L, 40L, n_time)]) {
    rows <- which(dat$time == tv)
    expect_equal(
      unname(posterior_epred(fit, newdata = dat[rows, , drop = FALSE],
                             draw_ids = 1:10, incl_autocor = TRUE)),
      unname(ref_epred[, rows, drop = FALSE])
    )
  }

  rev_nd <- dat
  rev_nd$series <- factor(as.character(rev_nd$series),
                          levels = rev(series_levels))
  expect_equal(
    unname(posterior_epred(fit, newdata = rev_nd, draw_ids = 1:10,
                           incl_autocor = TRUE)),
    unname(ref_epred)
  )
})


test_that("an unknown series is refused, and named", {
  nd <- dat
  nd$series <- factor(
    ifelse(seq_len(nrow(nd)) == 1L, "omega", as.character(nd$series)),
    levels = c(series_levels, "omega")
  )
  err <- expect_error(
    posterior_epred(fit, newdata = nd, draw_ids = 1:5),
    "Series levels in newdata not found in training data"
  )
  expect_match(conditionMessage(err), "omega", fixed = TRUE)
  for (s in series_levels) {
    expect_match(conditionMessage(err), s, fixed = TRUE)
  }
})


test_that("a newdata missing the gp covariate is refused by name", {
  nd <- dat
  nd$x <- NULL
  err <- expect_error(
    posterior_epred(fit, newdata = nd, draw_ids = 1:5)
  )
  expect_match(conditionMessage(err), "x", fixed = TRUE)
})


test_that("summary, tidiers and criticism run on a multi-lag AR", {
  txt <- capture.output(summary(fit))
  expect_gt(length(txt), 10L)
  expect_true(any(grepl(paste0("Series:\\s*", n_series), txt)))

  ll <- log_lik(fit, ndraws = 20L)
  expect_identical(dim(ll), c(20L, nrow(dat)))
  expect_true(all(is.finite(ll)))
  # `loo()` warns when a Pareto-k exceeds its threshold, which is a
  # statement about this fit rather than noise. Suppressing it throws
  # away the one diagnostic that says whether the approximation can
  # be trusted, so it is captured and turned into claims: the
  # estimate is finite, no k reaches the point where the
  # approximation breaks, and the warning that arrived, if any, is
  # the k notice those numbers already account for rather than
  # something else that slipped through.
  loo_warnings <- character(0)
  ic <- withCallingHandlers(
    loo(fit),
    warning = function(w) {
      loo_warnings <<- c(loo_warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_true(is.finite(ic$estimates["elpd_loo", "Estimate"]))
  pareto_k <- ic$diagnostics$pareto_k
  expect_true(all(is.finite(pareto_k)))
  # A high Pareto-k is what a latent state-space fit is expected to
  # produce: dropping an observation moves the very state it is being
  # scored against, which is the reason `lfo_cv()` exists. So the
  # claim is not that the diagnostic is good, it is that the user is
  # told the truth about it. The notice has to arrive exactly when
  # there is something to report, which fails both on a `loo()` gone
  # silent over bad draws and on one that cries out over good ones.
  expect_identical(
    any(pareto_k > 0.7),
    any(grepl("Pareto k", loo_warnings))
  )
  # Whatever was raised is that notice and nothing else, so an
  # unrelated warning cannot hide among the expected ones.
  expect_true(all(grepl("Pareto k", loo_warnings)))

  aug <- augment(fit)
  expect_identical(nrow(aug), nrow(dat))
  expect_equal(as.numeric(aug$time), as.numeric(dat$time))
  expect_equal(as.numeric(aug$.observed), as.numeric(dat$y))
  expect_identical(as.character(aug$series), as.character(dat$series))

  # The tidier names every lag the model has, and no others.
  vars <- variables(fit)
  for (l in lags) {
    expect_true(any(grepl(paste0("^ar", l, "_trend\\["), vars)))
  }
  for (l in absent_lags) {
    expect_false(any(grepl(paste0("^ar", l, "_trend\\["), vars)))
  }
})


test_that("pp_check and the plotting methods render", {
  expect_s3_class(pp_check(fit, ndraws = 20L), "ggplot")
  for (ty in c("residuals", "trend", "series")) {
    p <- plot(fit, type = ty)
    expect_s3_class(p, "ggplot")
  }
  expect_s3_class(mcmc_plot(fit), "ggplot")
})

test_that("irf, fevd and stability refuse a trend that has no A", {
  # All three read a transition matrix, and an AR has none: its
  # persistence is a vector of lag coefficients, so there are no
  # off-diagonal entries to carry a shock from one series into
  # another. Answering anyway would mean inventing the cross terms,
  # and the tables give a reader no way to tell an invented one from
  # an estimated one.
  expect_error(irf(fit), "VAR\\(1\\) latent trend")
  expect_error(fevd(fit), "VAR\\(1\\) latent trend")
  expect_error(stability(fit), "VAR\\(1\\) latent trend")
})


# ----- conditional_effects, and the `series` argument ----------------
#
# Two series is what makes the `series` argument testable: on one
# series every filtering claim below passes whether or not the filter
# does anything.

test_that("conditional_effects finds the model's own smooth term", {
  # Each element is a built panel, and the frame behind it is what
  # carries the effect. A panel that was never evaluated is a
  # constant with a zero-width band and satisfies any class check.
  ce <- suppressWarnings(conditional_effects(fit))
  expect_s3_class(ce, "mvgam_conditional_effects")
  expect_true("x" %in% names(ce))
  d <- ce[["x"]]$data
  expect_true(all(c("estimate", "conf.low", "conf.high") %in%
                    colnames(d)))
  expect_gt(stats::sd(d$estimate), 1e-8)
  expect_true(all(d$conf.low <= d$estimate))
  expect_true(all(d$estimate <= d$conf.high))
  expect_gt(mean(d$conf.high - d$conf.low), 0)
})


test_that("conditional_effects type = link is the link scale", {
  ce_resp <- suppressWarnings(conditional_effects(fit))
  ce_link <- suppressWarnings(conditional_effects(fit, type = "link"))
  expect_setequal(names(ce_link), names(ce_resp))
  # This fit is gaussian, so its link is the identity and the two
  # scales are the same numbers. Agreeing is the contract rather than
  # a coincidence, and it has to hold on both panels: a `type` that
  # applied some inverse link anyway would move them. The
  # complementary claim, where the two must not agree, is made on the
  # log-linked fits in test-draw-alignment.R and on the bernoulli arm
  # of test-mvbf-wide.R.
  for (nm in names(ce_resp)) {
    expect_equal(ce_link[[nm]]$data$estimate,
                 ce_resp[[nm]]$data$estimate, tolerance = 1e-10)
    expect_equal(ce_link[[nm]]$data$conf.low,
                 ce_resp[[nm]]$data$conf.low, tolerance = 1e-10)
  }
  # And the panels are not constant, so the agreement above is
  # between two things that vary rather than between two flat lines.
  expect_gt(stats::sd(ce_resp[["x"]]$data$estimate), 0.05)
})


test_that("conditional_effects honours a user-supplied effects list", {
  ce <- suppressWarnings(conditional_effects(fit, effects = "x"))
  expect_length(ce, 1L)
  expect_identical(names(ce), "x")
})


test_that("plot returns the effects list invisibly and draws it", {
  ce <- suppressWarnings(conditional_effects(fit))
  out <- plot(ce, plot = FALSE)
  expect_identical(out, ce)
  # Returning its input unchanged is half the contract; the panels
  # it would have drawn have to hold data.
  p <- plot(ce)[[1L]]
  layers <- ggplot2::ggplot_build(p)$data
  expect_gt(sum(vapply(layers, nrow, integer(1L))), 0L)
})


test_that("the series argument selects among this fit's own levels", {
  # The levels are the fit's own, in its own order, which is the
  # alphabetical reverse of what a rebuilt axis would give.
  lev <- levels(fit$data$series)
  expect_identical(lev, series_levels)
  all_ce <- suppressWarnings(conditional_effects(fit, series = "all"))
  d_all <- all_ce[[1L]]$data
  expect_true("series" %in% colnames(d_all))
  expect_setequal(as.character(unique(d_all$series)), lev)

  # Naming one series filters to it. On a single-series fit this is
  # satisfied by a filter that does nothing, which is why it is
  # asserted here instead.
  one <- suppressWarnings(conditional_effects(fit, series = lev[1L]))
  d_one <- one[[1L]]$data
  expect_lt(nrow(d_one), nrow(d_all))
  expect_setequal(as.character(unique(d_one$series)), lev[1L])

  # An index resolves to the level at that position, so it and the
  # name give the same answer. Position, not alphabetical rank: this
  # fit's first level is `gamma`, which sorts second.
  by_int <- suppressWarnings(conditional_effects(fit, series = 1L))
  expect_equal(by_int[[1L]]$data$estimate, d_one$estimate)
})


test_that("the series argument refuses what it cannot resolve", {
  n_lev <- nlevels(fit$data$series)
  expect_identical(n_lev, n_series)
  expect_error(conditional_effects(fit, series = "not_a_series"),
               "not one of the model's series levels")
  expect_error(conditional_effects(fit, series = n_lev + 1L), "upper")
  expect_error(conditional_effects(fit, series = c("a", "b")),
               "NULL, 'all', a series name")
})


cat("\nDone.\n")
