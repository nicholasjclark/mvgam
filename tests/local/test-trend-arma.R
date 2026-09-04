# Recovery and post-fit coverage for an ARMA trend, fitted here.
#
# There is no `ARMA()` constructor. A moving-average term is asked for
# with `ma = TRUE` on `AR()` or `VAR()`, which is worth pinning
# because it is the sort of argument that can be accepted and then
# dropped: the model still fits, the AR coefficients still come back,
# and the only evidence is a parameter that is missing from the
# program.
#
# So this file's central claim is a contrast. The same model with and
# without `ma = TRUE` must differ, and differ by the moving-average
# machinery specifically rather than by anything else.
#
# The observation side carries a two-dimensional `gp()`, which is a
# single basis over a pair of covariates rather than two bases. No
# other local file has one.
#
#   truth: 2 series, 80 occasions, gaussian, ARMA(1, 1) latent
#   model: y ~ gp(x1, x2, k = 8), trend_formula = ~ AR(p = 1,
#          ma = TRUE)
#
# Series named out of alphabetical order, occasions numbered from 3.
#
# Cached at tests/local/fixtures/val_mvgam_arma_trend.rds.
#
# Run with:
#   testthat::test_file("tests/local/test-trend-arma.R")

suppressMessages({
  devtools::load_all(".", quiet = TRUE)
  library(posterior)
  library(testthat)
})

# Several blocks below state what the package does not yet do, and
# testthat stops a file after ten failures by default, which would
# leave the blocks after them unrun and looking clean. The limit is
# read when the reporter is built, before this file is sourced, so it
# has to come from the environment:
#   TESTTHAT_MAX_FAILS=1000 Rscript -e "..."

# This file fits its own model and caches it beside itself, so it
# depends on no shared fixture and no build step.
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

set.seed(3109L)

n_series <- 2L
n_time <- 80L
series_levels <- c("kappa", "beta")
stopifnot(!identical(series_levels, sort(series_levels)))

time_vals <- seq_len(n_time) + 2L

# ARMA(1, 1): the state depends on its own past and on the previous
# innovation. The MA weight is large enough to be identified.
phi_true <- 0.55
theta_true <- 0.45
sigma_true <- 0.3

latent <- matrix(0, nrow = n_time, ncol = n_series)
for (s in seq_len(n_series)) {
  e <- rnorm(n_time, 0, sigma_true)
  for (t in 2:n_time) {
    latent[t, s] <- phi_true * latent[t - 1L, s] +
      e[t] + theta_true * e[t - 1L]
  }
}

# Two covariates and a surface that needs both, so a
# two-dimensional basis has something a pair of one-dimensional ones
# could not represent.
x1 <- as.numeric(scale(rnorm(n_time)))
x2 <- as.numeric(scale(rnorm(n_time)))
gp_true <- 0.9 * sin(x1 * 1.4) * cos(x2 * 1.4)

dat <- data.frame(
  time = rep(time_vals, times = n_series),
  series = factor(rep(series_levels, each = n_time),
                  levels = series_levels),
  x1 = rep(x1, times = n_series),
  x2 = rep(x2, times = n_series)
)
dat$y <- as.numeric(latent) + rep(gp_true, times = n_series) +
  rnorm(nrow(dat), 0, 0.2)

obs_formula <- y ~ gp(x1, x2, k = 8)

sim_truth <- list(
  n_series = n_series, n_time = n_time,
  series_levels = series_levels, time_vals = time_vals,
  phi_true = phi_true, theta_true = theta_true,
  sigma_true = sigma_true, latent = latent, gp_true = gp_true,
  x1 = x1, x2 = x2
)

make_future <- function(h) {
  ft <- max(time_vals) + seq_len(h)
  data.frame(
    time = rep(ft, times = n_series),
    series = factor(rep(series_levels, each = h),
                    levels = series_levels),
    x1 = 0, x2 = 0,
    y = NA_real_
  )
}


# -- Prefit: does `ma = TRUE` change the program? --------------------

prefit <- mvgam(
  formula = obs_formula, trend_formula = ~ AR(p = 1, ma = TRUE),
  data = dat, family = gaussian(), run_model = FALSE, silent = 2
)
prefit_noma <- mvgam(
  formula = obs_formula, trend_formula = ~ AR(p = 1),
  data = dat, family = gaussian(), run_model = FALSE, silent = 2
)


test_that("ma = TRUE adds a moving-average term and its innovations", {
  # The contrast the file exists for. An argument accepted and
  # dropped leaves these two programs identical, and every other
  # check in this file would still pass.
  sc_ma <- as.character(stancode(prefit))
  sc_plain <- as.character(stancode(prefit_noma))
  expect_false(identical(sc_ma, sc_plain))

  # The MA coefficient is indexed by lag, as the AR ones are.
  expect_true(grepl("theta1_trend", sc_ma, fixed = TRUE))
  expect_false(grepl("theta1_trend", sc_plain, fixed = TRUE))

  # A moving average is a weighted sum of past innovations, so the
  # program has to keep them. Without this the coefficient exists and
  # multiplies nothing.
  expect_true(grepl("ma_innovations_trend", sc_ma, fixed = TRUE))
  expect_false(grepl("ma_innovations_trend", sc_plain, fixed = TRUE))

  # The autoregressive half is unchanged, so `ma = TRUE` adds rather
  # than replaces.
  expect_true(grepl("ar1_trend", sc_ma, fixed = TRUE))
  expect_true(grepl("ar1_trend", sc_plain, fixed = TRUE))

  # The data handed to Stan is the same either way: the difference is
  # entirely in the parameters and the recursion.
  expect_identical(names(prefit$standata), names(prefit_noma$standata))
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
})


test_that("the gp is one basis over two covariates", {
  # `gp(x1, x2)` is a single two-dimensional process, not two
  # one-dimensional ones. The dimension is what separates them, and
  # a design that emitted two bases has the same term in the formula
  # and a different model underneath.
  sd <- prefit$standata
  expect_identical(as.integer(sd$Dgp_1), 2L)
  # Only one gp block, so there is no second process hiding.
  expect_length(grep("^Dgp_[0-9]+$", names(sd), value = TRUE), 1L)

  # The covariates are stored once per distinct (x1, x2) pair, two
  # columns wide, and each row is pointed at its pair by `Jgp_1`.
  # The round trip is what pins the row order: looking each row up
  # through the index has to return both of its own covariates.
  pairs <- unique(dat[, c("x1", "x2")])
  expect_identical(as.integer(sd$Nsubgp_1), nrow(pairs))
  expect_identical(ncol(sd$Xgp_prior_1), 2L)
  expect_length(as.integer(sd$Jgp_1), nrow(dat))
  expect_identical(sort(unique(as.integer(sd$Jgp_1))),
                   seq_len(nrow(pairs)))
  # brms rescales gp covariates onto a bounded support before
  # building the basis, so the stored pairs are an increasing affine
  # image of the originals rather than the originals. The map is
  # what has to be right regardless: each margin, looked up through
  # the index, rises with the covariate the frame states.
  j <- as.integer(sd$Jgp_1)
  expect_equal(
    stats::cor(as.numeric(sd$Xgp_prior_1[, 1L])[j],
               as.numeric(dat$x1)), 1
  )
  expect_equal(
    stats::cor(as.numeric(sd$Xgp_prior_1[, 2L])[j],
               as.numeric(dat$x2)), 1
  )
  # Rows sharing a pair share an index, and rows differing in either
  # margin do not.
  key <- paste(dat$x1, dat$x2)
  expect_identical(length(unique(j)), length(unique(key)))
  expect_true(all(tapply(j, key, function(v) length(unique(v))) == 1L))

  # One basis matrix over those pairs, and one eigenvalue per basis
  # function per dimension.
  expect_identical(nrow(sd$Xgp_1), nrow(pairs))
  expect_true(all(as.numeric(sd$slambda_1) > 0))
})


# -- Fit --------------------------------------------------------------

cache <- cache_path("val_mvgam_arma_trend.rds")
if (file.exists(cache)) {
  cat("[cache] Loading ARMA fit.\n")
  fit <- readRDS(cache)
} else {
  cat("[fit ] mvgam(AR(p = 1, ma = TRUE), gp(x1, x2), 2 x 80)\n")
  fit <- mvgam(
    formula = obs_formula, trend_formula = ~ AR(p = 1, ma = TRUE),
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


test_that("both halves of the ARMA are estimated, one per series", {
  ar_cols <- grep("^ar1_trend\\[", colnames(dm), value = TRUE)
  ma_cols <- grep("^theta1_trend\\[", colnames(dm), value = TRUE)
  expect_length(ar_cols, n_series)
  expect_length(ma_cols, n_series)

  ar_hat <- vapply(ar_cols, function(k) mean(dm[, k]), numeric(1))
  ma_hat <- vapply(ma_cols, function(k) mean(dm[, k]), numeric(1))
  expect_true(all(is.finite(ar_hat)))
  expect_true(all(is.finite(ma_hat)))

  # Both weights are real in the simulation, so neither should come
  # back at zero: an MA coefficient pinned to zero is the signature
  # of a term that is declared but never enters the recursion.
  expect_true(all(abs(ma_hat) > 0.05))
  # Stationary and invertible, which is what the parameterisation is
  # meant to guarantee.
  expect_true(all(abs(ar_hat) < 1))
  expect_true(all(abs(ma_hat) < 1))

  # Recovery, loosely: an ARMA(1,1) trades phi against theta, so the
  # tolerance is wide and the sign is the informative part.
  expect_true(all(ar_hat > 0))
  expect_true(all(ma_hat > 0))
})


test_that("every prediction surface answers for every row", {
  n_obs <- nrow(dat)
  ep <- posterior_epred(fit, ndraws = 20L)
  pp <- posterior_predict(fit, ndraws = 20L)
  expect_identical(dim(ep), c(20L, n_obs))
  expect_identical(dim(pp), c(20L, n_obs))
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


test_that("the forecast object is keyed, ordered and in user units", {
  h <- 5L
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
  # Two series with their own coefficients do not forecast alike.
  expect_false(isTRUE(all.equal(fc$forecasts[[1L]],
                                fc$forecasts[[2L]])))
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


test_that("the two-dimensional gp effect varies in both margins", {
  # A surface, not a pair of lines. Holding one covariate and moving
  # the other has to give a different profile depending on where the
  # held one sits, which is what an interaction between the margins
  # means and what two additive one-dimensional terms cannot do.
  xs <- seq(min(dat$x1), max(dat$x1), length.out = 25L)
  profile_at <- function(x2_val) {
    grid <- data.frame(
      x1 = xs, x2 = x2_val,
      series = factor(series_levels[1L], levels = series_levels),
      time = time_vals[1L], y = NA_real_
    )
    colMeans(posterior_epred(fit, newdata = grid, ndraws = 200L))
  }
  lo <- profile_at(unname(stats::quantile(dat$x2, 0.15)))
  hi <- profile_at(unname(stats::quantile(dat$x2, 0.85)))
  expect_true(all(is.finite(lo)))
  expect_true(all(is.finite(hi)))
  # Each profile varies along x1 ...
  expect_gt(stats::sd(lo), 0.05)
  expect_gt(stats::sd(hi), 0.05)
  # ... and the two profiles are not the same curve shifted, which is
  # all an additive model could produce.
  expect_gt(stats::sd(hi - lo), 0.02)
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
  set.seed(64L)
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

  for (tv in time_vals[c(1L, 33L, n_time)]) {
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
    ifelse(seq_len(nrow(nd)) == 1L, "sigma", as.character(nd$series)),
    levels = c(series_levels, "sigma")
  )
  err <- expect_error(
    posterior_epred(fit, newdata = nd, draw_ids = 1:5),
    "Series levels in newdata not found in training data"
  )
  expect_match(conditionMessage(err), "sigma", fixed = TRUE)
  for (s in series_levels) {
    expect_match(conditionMessage(err), s, fixed = TRUE)
  }
})


test_that("a newdata missing either gp covariate is refused by name", {
  # Both margins are needed, so dropping either one has to be
  # refused, and the message has to say which is gone.
  for (cl in c("x1", "x2")) {
    nd <- dat
    nd[[cl]] <- NULL
    err <- expect_error(
      posterior_epred(fit, newdata = nd, draw_ids = 1:5)
    )
    expect_match(conditionMessage(err), cl, fixed = TRUE)
  }
})


test_that("summary, tidiers and criticism run on an ARMA fit", {
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

  # Both halves are visible to a user reading the parameter names.
  vars <- variables(fit)
  expect_true(any(grepl("^ar1_trend\\[", vars)))
  expect_true(any(grepl("^theta1_trend\\[", vars)))
})


test_that("pp_check and the plotting methods draw something", {
  # A ggplot comes back whether or not a layer received data, so the
  # class alone passes on an empty panel. Building it forces the
  # layers to resolve and the row count says something was drawn.
  drawn <- function(p) {
    expect_s3_class(p, "ggplot")
    layers <- ggplot2::ggplot_build(p)$data
    expect_gt(sum(vapply(layers, nrow, integer(1L))), 0L)
    invisible(p)
  }
  drawn(pp_check(fit, ndraws = 20L))
  for (ty in c("residuals", "trend", "series")) {
    drawn(plot(fit, type = ty))
  }
  drawn(mcmc_plot(fit))
})


test_that("every per-series plot panels in the model's own order", {
  # The series are declared out of alphabetical order, so a panel
  # order taken from a sort differs from the one the model holds.
  # `plot(type = "series")` and the hindcast arms use the model's
  # order; `plot(type = "trend")` sorts, so the two pictures a reader
  # is most likely to compare put a different series first while
  # labelling every panel correctly.
  panel_order <- function(ty) {
    b <- ggplot2::ggplot_build(plot(fit, type = ty))
    lay <- b$layout$layout
    fc <- setdiff(names(lay),
                  c("PANEL", "ROW", "COL", "SCALE_X", "SCALE_Y"))
    if (!length(fc)) return(character(0))
    as.character(lay[[fc[1L]]])
  }
  expect_identical(panel_order("series"), series_levels)
  expect_identical(panel_order("trend"), series_levels)
  expect_identical(names(hindcast(fit)$hindcasts), series_levels)
})


test_that("print names the trend the fit was given", {
  # The first thing a user calls. This file exists to tell an ARMA
  # from an AR, and `print()` reports both as `AR`: the lag order and
  # the moving-average term are dropped, so the two models are
  # indistinguishable in the output. `summary()` carries
  # `theta1_trend`, so the information is there to report.
  out <- capture.output(print(fit))
  trend_line <- out[which(grepl("^Trend model", out)) + 1L]
  expect_match(trend_line, "ma", ignore.case = TRUE)

  # And it prints the formula environments, two lines of pointer that
  # change between sessions and describe nothing about the model.
  expect_identical(grep("<environment: 0x", out, value = TRUE),
                   character(0))
})


test_that("forecast with no newdata says what it needs", {
  # The default call. It returns an `mvgam_forecast` carrying two
  # hindcast arms, a type, and an empty `forecasts` list, so an
  # object that looks complete holds no forecast at all. Either a
  # horizon is forecast or the requirement is named.
  fc <- forecast(fit)
  expect_length(fc$forecasts, n_series)
})

cat("\nDone.\n")
