# End-to-end forecast tests on latent-factor fits with
# posterior-sampled Z (free-loadings branch of
# `resolve_Z_loadings()`). The fixed-Z branch is exercised in
# `test-trend-map-fit.R`. Together they cover both sources of Z
# that `forecast.mvgam()` reads in factor mode.
#
# `val_mvgam_lv_factor.rds` is a 4-series, 2-factor AR(1) fit
# where the loadings matrix Z is sampled unconstrained
# (`trend_map = NULL`, n_lv = 2), so the resolver takes the
# `Z[s, k]` posterior branch. Every draw carries its own Z
# slice, and the projected forecast trajectory rides that draw's
# latent path.


CACHE_DIR <- "fixtures"
fit <- readRDS(file.path(CACHE_DIR, "val_mvgam_lv_factor.rds"))


# One forecast frame, built once. Five copies of these ten lines stood
# in this file, and each took its series from `levels(fit$data$series)`
# -- a second reading of the fact the fit already records, and the
# reading a permutation between the two would leave standing.
future_frame <- function(object, h) {
  levs <- as.character(mvgam_axes(object)$series$levels)
  n_time <- object$standata$N_time_trend
  out <- data.frame(
    time = rep((n_time + 1L):(n_time + h), length(levs)),
    series = factor(rep(levs, each = h), levels = levs)
  )
  # Match the fitted response's type so newdata assembly does not
  # coerce the column into a non-NA sentinel.
  out[[object$response_names[1L] %||% "y"]] <- NA_real_
  out
}


# A forecast is keyed by the axis and in the axis's own order. Reading
# an arm by name, which is what every forecast test here did, passes
# on a list holding every arm under the wrong key, and that
# permutation is the defect this file exists to catch.
expect_axis_keyed <- function(fc, object, ndraws, h) {
  levs <- as.character(mvgam_axes(object)$series$levels)
  expect_identical(names(fc$forecasts), levs)
  for (s in levs) {
    expect_equal(dim(fc$forecasts[[s]]), c(ndraws, h))
  }
}


test_that("fixture is a free-Z factor fit (sanity checks)", {
  # Guard against fixture drift: the tests below rely on the
  # posterior sampling Z, not fixing it.
  expect_equal(fit$standata$N_lv_trend, 2L)
  expect_equal(fit$standata$N_series_trend, 4L)
  expect_true(is.null(fit$mv_spec$trend_specs$fixed_Z))
  expect_true(any(grepl("^Z(_tilde)?\\[", variables(fit))))
})


test_that("forecast on free-Z factor fit is keyed by the axis", {
  h <- 8L
  fc <- forecast(fit, newdata = future_frame(fit, h), ndraws = 30L)
  expect_s3_class(fc, "mvgam_forecast")
  expect_axis_keyed(fc, fit, ndraws = 30L, h = h)
})


test_that("forecast on free-Z factor fit gives finite draws", {
  # Every posterior draw should produce a finite [h, n_series]
  # trajectory once Z projection applies; NAs would indicate
  # either a missing Z column at some draw or a downstream
  # kernel producing non-finite values.
  h <- 6L
  fc <- forecast(fit, newdata = future_frame(fit, h), ndraws = 30L,
                  type = "link")
  expect_axis_keyed(fc, fit, ndraws = 30L, h = h)
  for (draws in fc$forecasts) {
    expect_true(all(is.finite(draws)))
  }
})


test_that("free-Z factor forecast draws respect Z per draw", {
  # For a free-Z fit, per-draw variability in the loadings
  # `Z[s, k]` should show up in the forecast: two series with
  # non-collinear rows of Z should NOT produce identical
  # forecast draws (unlike the fixed-Z case where identical
  # rows collapse to identical draws). This catches a
  # regression where the projection accidentally applies the
  # same Z row across all series -- a silent bug that would
  # only surface here.
  h <- 6L
  fc <- forecast(fit, newdata = future_frame(fit, h), ndraws = 30L,
                  type = "link")
  expect_axis_keyed(fc, fit, ndraws = 30L, h = h)

  # Every pair of series must differ, not merely one pair somewhere.
  # Asking whether any pair differs passes on an axis that collapsed
  # three of the four series onto one latent column, which is the
  # shape this test was written to refuse. The Heaps identification
  # pins the upper-triangular block of Z, and no two rows of a free Z
  # are equal, so the arms are distinct pair by pair.
  arms <- fc$forecasts
  pairs_same <- character(0)
  for (i in seq_along(arms)) {
    for (j in seq_along(arms)) {
      if (j <= i) next
      if (isTRUE(all.equal(arms[[i]], arms[[j]]))) {
        pairs_same <- c(pairs_same,
                        paste(names(arms)[i], names(arms)[j], sep = "="))
      }
    }
  }
  expect_identical(pairs_same, character(0))
})


# ---- ZMVN factor forecast on a jsdgam fit ----------------------
# `val_jsdgam_trait.rds` is a jsdgam fit with n_lv = 2, n_series
# = 8, ZMVN trend under the Heaps QR identification. The latent
# state is stored as `lv_trend_tilde[t, k]` and the loadings as
# `Z_tilde[s, k]` (no `sigma_trend` / `L_Omega_trend` because
# under the identification the LVs are standard normals and all
# scale / correlation lives on Z_tilde). Forecast propagation
# reduces to fresh N(0, I) draws per horizon step, projected
# through Z_tilde.

jsdgam_fit <- readRDS(file.path(CACHE_DIR, "val_jsdgam_trait.rds"))


test_that("jsdgam ZMVN factor fit is a sane fixture", {
  expect_s3_class(jsdgam_fit, "jsdgam")
  expect_equal(jsdgam_fit$trend_metadata$trend_type, "ZMVN")
  expect_lt(
    jsdgam_fit$standata$N_lv_trend,
    jsdgam_fit$standata$N_series_trend
  )
  # Under Heaps identification: no raw sigma_trend /
  # L_Omega_trend, but Z_tilde and lv_trend_tilde exist.
  vars <- variables(jsdgam_fit)
  expect_false(any(grepl("^sigma_trend\\[", vars)))
  expect_false(any(grepl("^L_Omega_trend\\[", vars)))
  expect_true(any(grepl("^Z_tilde\\[", vars)))
  expect_true(any(grepl("^lv_trend_tilde\\[", vars)))
})


test_that("jsdgam ZMVN factor forecast returns finite draws", {
  n_series <- jsdgam_fit$standata$N_series_trend
  n_time <- jsdgam_fit$standata$N_time_trend
  h <- 5L
  train <- jsdgam_fit$obs_data
  # The axis as the fit recorded it, not as the training column
  # happens to spell it.
  series_levels <- as.character(mvgam_axes(jsdgam_fit)$series$levels)
  trait_map <- unique(train[, c("species", "trait1")])
  newdat <- expand.grid(
    time = (n_time + 1L):(n_time + h),
    species = trait_map$species,
    stringsAsFactors = FALSE
  )
  newdat$trait1 <- trait_map$trait1[
    match(newdat$species, trait_map$species)
  ]
  newdat$site <- rep(seq_len(h), n_series)
  newdat$env <- rnorm(nrow(newdat))
  newdat$series <- factor(newdat$species, levels = series_levels)
  newdat$y <- NA_real_

  fc <- forecast(jsdgam_fit, newdata = newdat, ndraws = 30L)
  expect_s3_class(fc, "mvgam_forecast")
  expect_identical(names(fc$forecasts), series_levels)
  for (s in series_levels) {
    fm <- fc$forecasts[[s]]
    expect_equal(dim(fm), c(30L, h))
    expect_true(all(is.finite(fm)))
  }

  # No two species share a row of Z_tilde, so no two arms are the
  # same draws. A spread in the per-series means, which is what stood
  # here, is satisfied by an axis that gave several species one
  # column while leaving the rest apart.
  arms <- fc$forecasts
  collapsed <- character(0)
  for (i in seq_along(arms)) {
    for (j in seq_along(arms)) {
      if (j <= i) next
      if (isTRUE(all.equal(arms[[i]], arms[[j]]))) {
        collapsed <- c(collapsed,
                       paste(names(arms)[i], names(arms)[j], sep = "="))
      }
    }
  }
  expect_identical(collapsed, character(0))
})


# ---- VAR factor forecast on a small inline fit -----------------
# No cached fixture exists for a VAR-factor fit, and building
# one is cheap enough to happen inside a local test. The fit is
# tiny (4 series, 2 factors, 60 timepoints) but the trend_map
# forces shared latent paths on {s1, s2} and {s3, s4}, so the
# forecast projection has a testable invariant: on the trend
# scale, two series pointing at the same factor must have
# byte-identical forecast draws.

test_that("VAR factor forecast preserves shared-latent invariant", {
  set.seed(7)
  n_time <- 60L
  n_series <- 4L
  n_lv <- 2L
  # Simulate two independent VAR(1) latent processes then
  # project through a binary Z (s1, s2 -> trend 1; s3, s4 ->
  # trend 2) so the fit has real signal to lock onto.
  A <- matrix(c(0.6, 0.1, -0.05, 0.5), nrow = n_lv, ncol = n_lv)
  lv <- matrix(0, nrow = n_time, ncol = n_lv)
  for (t in 2:n_time) {
    lv[t, ] <- A %*% lv[t - 1L, ] + rnorm(n_lv, 0, 0.4)
  }
  Z_true <- matrix(c(1, 1, 0, 0, 0, 0, 1, 1), nrow = n_series)
  mu <- lv %*% t(Z_true)
  y_mat <- mu + matrix(rnorm(n_time * n_series, 0, 0.2),
                        n_time, n_series)
  dat <- data.frame(
    time = rep(seq_len(n_time), n_series),
    series = factor(
      rep(paste0("s", 1:n_series), each = n_time),
      levels = paste0("s", 1:n_series)
    ),
    y = as.numeric(y_mat)
  )
  tm <- data.frame(
    series = paste0("s", 1:n_series),
    trend = c(1L, 1L, 2L, 2L)
  )
  mod <- mvgam(
    y ~ 1, trend_formula = ~ VAR(p = 1L), trend_map = tm,
    data = dat, family = gaussian(),
    chains = 2L, iter = 1000L, warmup = 500L,
    silent = 2L, refresh = 0
  )
  expect_equal(mod$standata$N_lv_trend, n_lv)
  expect_equal(mod$standata$N_series_trend, n_series)

  h <- 5L
  newdat <- data.frame(
    time = rep((n_time + 1L):(n_time + h), n_series),
    series = factor(
      rep(paste0("s", 1:n_series), each = h),
      levels = paste0("s", 1:n_series)
    ),
    y = NA_real_
  )
  fc <- forecast(mod, newdata = newdat, ndraws = 30L,
                  type = "link")
  # Series sharing a trend must produce byte-identical forecast
  # draws on the link scale. The observation noise is not added
  # for type = "link"; any regression where the LV recursion
  # diverges across shared series, or where Z is applied
  # inconsistently, would break these equalities immediately.
  expect_equal(fc$forecasts[["s1"]], fc$forecasts[["s2"]])
  expect_equal(fc$forecasts[["s3"]], fc$forecasts[["s4"]])
  # Series on different trends must differ (the VAR has real
  # dynamics, not a degenerate zero draw).
  expect_false(
    isTRUE(all.equal(fc$forecasts[["s1"]], fc$forecasts[["s3"]]))
  )
  for (s in levels(dat$series)) {
    expect_true(all(is.finite(fc$forecasts[[s]])))
  }
})


# ---- a factor fit at the truncation ceiling --------------------
# `val_mvgam_mgp_ceiling.rds` is a 3-series AR(1) fit with
# `n_lv = 3` under MGP column shrinkage, which is what admits
# `n_lv = n_series`. Both Stan trend dimensions are 3 there, and a
# non-factor fit carries that same pair with an identity Z, so a
# post-fit surface that compares them reads this fit as
# series-grain: no Z projection happens and `ar1_trend[k]`, indexed
# by latent column, reaches the recursion for series k with lengths
# that match and nothing to complain. Only the requested `n_lv`
# separates the two, which is what `detect_factor_n_lv()` reads.
#
# The fixture samples with a few divergences and a low E-BFMI on
# one chain. That is the geometry the ceiling has, and why the
# ceiling exists only under shrinkage; the fit is here to exercise
# a code path rather than to recover the simulated loadings, so the
# tests below assert grain and never parameter accuracy.

mgp_fit <- readRDS(file.path(CACHE_DIR, "val_mvgam_mgp_ceiling.rds"))


test_that("the ceiling fixture really is a free-Z fit at n_lv = n_series", {
  expect_equal(mgp_fit$standata$N_lv_trend, 3L)
  expect_equal(mgp_fit$standata$N_series_trend, 3L)
  expect_true(any(grepl("^Z(_tilde)?\\[", variables(mgp_fit))))
  expect_null(mgp_fit$mv_spec$trend_specs$fixed_Z)
  # The two Stan dimensions agree, so they cannot be the source.
  expect_identical(detect_factor_n_lv(mgp_fit), 3L)
})


test_that("last-state extraction at the ceiling reads the latent grain", {
  draws_mat <- posterior::as_draws_matrix(mgp_fit$fit)
  st <- extract_last_state(mgp_fit, draw_id = 1L, draws_mat = draws_mat)
  # Set only in factor mode, and the flag the caller projects on.
  expect_identical(st$n_lv_active, 3L)

  n_time <- mgp_fit$standata$N_time_trend
  k_seq <- seq_len(3L)
  lv_last <- vapply(k_seq, function(k) {
    draws_mat[1L, paste0("lv_trend[", n_time, ",", k, "]")]
  }, numeric(1L))
  series_last <- vapply(k_seq, function(s) {
    draws_mat[1L, paste0("trend[", n_time, ",", s, "]")]
  }, numeric(1L))
  got <- as.numeric(st$last_state$trends[nrow(st$last_state$trends), ])

  expect_equal(got, unname(lv_last))
  # Z is not the identity here, so reading the wrong grain is
  # visible rather than a distinction without a difference.
  expect_false(isTRUE(all.equal(unname(lv_last), unname(series_last))))
})


# Shape and finiteness through the factor path. The grain itself
# is pinned by the state test above, which is where reading the
# wrong one shows up numerically; at the ceiling both readings
# produce an `[h, n_series]` block of finite draws, so this test
# covers the path rather than discriminating the defect.
test_that("forecast at the ceiling returns a finite series block", {
  h <- 5L
  fc <- forecast(mgp_fit, newdata = future_frame(mgp_fit, h),
                  ndraws = 20L, type = "link")
  expect_axis_keyed(fc, mgp_fit, ndraws = 20L, h = h)
  for (arm in fc$forecasts) {
    expect_true(all(is.finite(arm)))
  }
  expect_false(isTRUE(all.equal(fc$forecasts[[1L]], fc$forecasts[[2L]])))
})
