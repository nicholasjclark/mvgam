# End-to-end fit-time tests for `trend_map` (fixed-Z factor
# models). Reads the cached `val_mvgam_trend_map_fx.rds` fit
# produced by tests/local/build_fixtures.R. The cached fit is a
# 4-series, 2-factor AR(1) model with a dense user-supplied Z.
#
# Covers Stan emission and standata round-trip contract checks,
# plus the downstream resolver (`resolve_factor_loadings`) that
# lets posterior_predict / residuals / residual_cor /
# extract_factors run on a fixed-Z fit.

CACHE_DIR <- "fixtures"
fit <- readRDS(file.path(CACHE_DIR, "val_mvgam_trend_map_fx.rds"))
Z_true <- attr(fit$data, "Z_true") %||% attr(fit$obs_data, "Z_true")


test_that("standata carries the fixed Z matrix verbatim", {
  sd <- fit$standata
  expect_true("Z" %in% names(sd))
  expect_equal(dim(sd$Z), dim(Z_true))
  expect_equal(unname(sd$Z), unname(Z_true))
})


test_that("each row of Z loads the series that occupies its column", {
  # Comparing `Z` against `Z_true` says the matrix arrived intact in
  # the order the user wrote it. It does not say row `k` belongs to
  # the series Stan puts in trend column `k`, and a permutation
  # between those two orders is invisible to a value-for-value
  # check: every loading is present, every dimension agrees, and
  # each series simply loads on another's factors.
  #
  # The occupant of each column is read from `obs_trend_series`,
  # which is what the sampler indexed, and the row of `Z_true` is
  # found by the series' own label. Neither side comes from the
  # normaliser being checked.
  sd <- fit$standata
  labels <- as.character(mvgam:::training_series_labels(fit))
  s_idx <- as.integer(sd$obs_trend_series)
  n_series <- as.integer(sd$N_series_trend)

  occupant <- vapply(seq_len(n_series), function(k) {
    held <- unique(labels[s_idx == k])
    if (length(held) == 1L) held else NA_character_
  }, character(1))
  expect_false(anyNA(occupant))

  # `Z_true` was built row-per-series in the order the frame
  # declares its series, which is the user's own statement of which
  # loadings belong to whom.
  declared <- levels(droplevels(as.factor(fit$data$series)))
  expect_setequal(occupant, declared)
  for (k in seq_len(n_series)) {
    expect_equal(
      unname(sd$Z[k, ]),
      unname(Z_true[match(occupant[k], declared), ]),
      label = paste("column", k, "carries", occupant[k], "loadings")
    )
  }
})


test_that("each series reads the latent cell the sampler gave it", {
  # The claim every other assertion here rests on, checked against
  # the posterior itself rather than against another derivation:
  # the state post-fit resolves for a row is the `trend[t, s]` the
  # sampler drew for that row's recorded cell. A wrong axis reads a
  # real state belonging to another series, so the values are
  # finite, the shapes agree, and only this comparison notices.
  d <- as.data.frame(fit$data)
  recorded_s <- as.integer(fit$standata$obs_trend_series)
  recorded_t <- as.integer(fit$standata$obs_trend_time)
  expect_length(recorded_s, nrow(d))

  dm <- posterior::as_draws_matrix(fit$fit)
  want <- vapply(seq_len(nrow(d)), function(j) {
    mean(dm[, paste0("trend[", recorded_t[j], ",", recorded_s[j], "]")])
  }, numeric(1))
  got <- colMeans(
    mvgam:::extract_trend_latent_states(fit, newdata = d, full_draws = dm)
  )
  expect_equal(unname(got), unname(want))

  # And the series are not all reading one column: a fixed `Z` with
  # distinct rows gives distinct states, so identical arms would
  # mean the axis collapsed.
  by_series <- split(want, recorded_s)
  expect_gt(length(by_series), 1L)
  expect_false(isTRUE(all.equal(by_series[[1L]], by_series[[2L]])))
})


test_that("Stan code declares Z in data; no Z_raw / no prior", {
  code_txt <- as.character(fit$stancode)
  expect_true(grepl(
    "matrix[N_series_trend, N_lv_trend] Z;",
    code_txt, fixed = TRUE
  ))
  expect_false(grepl("Z_raw", code_txt, fixed = TRUE))
})


test_that("fit object structure is intact after fixed-Z path", {
  expect_s3_class(fit, "mvgam")
  expect_true(is.list(fit$trend_metadata))
  expect_true("Z" %in% names(fit$standata))
  expect_equal(fit$standata$N_lv_trend, 2L)
  expect_equal(fit$standata$N_series_trend, 4L)
})


test_that("Z does NOT appear as a posterior parameter", {
  # Sampled-Z fits store Z_raw / Z draws in $fit; fixed-Z fits
  # must NOT — the matrix is data, not a parameter. This catches
  # any future regression where Z slips back into the parameter
  # block.
  param_names <- fit$fit@sim$pars_oi %||% character(0)
  expect_false(any(grepl("Z_raw", param_names)))
})


test_that("trend_metadata persists the fixed Z matrix", {
  expect_false(is.null(fit$trend_metadata$fixed_Z))
  expect_equal(unname(fit$trend_metadata$fixed_Z), unname(Z_true))
  expect_equal(fit$trend_metadata$n_lv, ncol(Z_true))
})


# ---- Downstream methods that route through resolve_factor_loadings ----

test_that("posterior_predict returns the right shape on fixed-Z fit", {
  pp <- posterior_predict(fit, ndraws = 30L)
  expect_true(is.matrix(pp))
  expect_equal(nrow(pp), 30L)
  expect_equal(ncol(pp), nrow(fit$data))
})


test_that("posterior_epred returns the right shape on fixed-Z fit", {
  ep <- posterior_epred(fit, ndraws = 30L)
  expect_true(is.matrix(ep))
  expect_equal(nrow(ep), 30L)
  expect_equal(ncol(ep), nrow(fit$data))
  expect_true(all(is.finite(ep)))
  expect_true(all(ep > 0))
})


test_that("residuals return one column per observation", {
  r <- residuals(fit, summary = FALSE, ndraws = 30L)
  expect_true(is.matrix(r))
  expect_equal(nrow(r), 30L)
  expect_equal(ncol(r), nrow(fit$data))
})


test_that("forecast on fixed-Z factor fit returns the right shape", {
  # `val_mvgam_trend_map_fx.rds` is a 4-series, 2-factor AR(1)
  # with a dense user-supplied Z. Since this test's fit has no
  # persisted test_data, build newdata explicitly so the
  # horizon is fixed for the shape assertion below.
  n_series <- fit$standata$N_series_trend
  n_time <- fit$standata$N_time_trend
  h <- 8L
  series_levels <- levels(fit$data$series)
  newdat <- data.frame(
    time = rep((n_time + 1L):(n_time + h), n_series),
    series = factor(
      rep(series_levels, each = h),
      levels = series_levels
    )
  )
  newdat$count <- NA_integer_
  fc <- forecast(fit, newdata = newdat, ndraws = 30L)
  expect_s3_class(fc, "mvgam_forecast")
  for (s in series_levels) {
    expect_equal(dim(fc$forecasts[[s]]), c(30L, h))
  }
})


test_that("forecast on fixed-Z factor gives finite link-scale draws", {
  # Every posterior draw should produce a finite [h, n_series]
  # trajectory once the LV-space AR(1) recursion has been
  # projected via the fixed Z matrix. NAs would indicate
  # either a missing lv_trend column or an infinite draw
  # slipping past the projection.
  n_series <- fit$standata$N_series_trend
  n_time <- fit$standata$N_time_trend
  h <- 6L
  series_levels <- levels(fit$data$series)
  newdat <- data.frame(
    time = rep((n_time + 1L):(n_time + h), n_series),
    series = factor(rep(series_levels, each = h),
                     levels = series_levels)
  )
  newdat$count <- NA_integer_
  fc <- forecast(fit, newdata = newdat, ndraws = 30L,
                  type = "link")
  for (s in series_levels) {
    expect_true(all(is.finite(fc$forecasts[[s]])))
  }
})


test_that("residual_cor returns a residcor object", {
  rc <- residual_cor(fit, ndraws = 30L)
  expect_s3_class(rc, "mvgam_residcor")
})


test_that("posterior recovers different per-series means", {
  # What separates the series here is the latent state each one loads
  # on, so the question is only answerable on the surface that carries
  # it. The observation formula is `y ~ 1`, and the default surface
  # leaves the trend at its deterministic submodel, which for this fit
  # is zero: every row then returns `exp(Intercept)` and the series
  # agree exactly, whatever `Z` holds. That is the documented default,
  # not a collapse.
  series_vec <- fit$data$series
  series_mean <- function(m) {
    vapply(levels(series_vec), function(s) mean(m[, series_vec == s]),
           numeric(1L))
  }

  ep <- posterior_epred(fit, ndraws = 30L, incl_autocor = TRUE)
  series_means <- series_mean(ep)
  expect_true(all(is.finite(series_means)))
  expect_true(all(series_means > 0))
  # Dense Z mixes 2 factors with different per-series weights,
  # so expected counts should not collapse to one value.
  expect_gt(stats::sd(series_means), 1e-6)

  # And the marginal surface is flat here for the reason above, which
  # is worth pinning so the two are not confused again.
  marg <- series_mean(posterior_epred(fit, ndraws = 30L))
  expect_equal(stats::sd(marg), 0)
})
