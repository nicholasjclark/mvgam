# Unit tests for `forecast.mvgam()` in `R/forecast.mvgam.R`. The
# function orchestrates a per-draw kernel loop, an obs-side
# linpred combination, and an optional family sampling pass. The
# heavy primitives (extract_component_linpred, propagate_trend,
# posterior_predict / posterior_epred, get_observation_structure)
# are exercised by their own test files, so these tests stub
# them via `local_mocked_bindings()` and assert on:
#   * mvgam_forecast class shape (10 fields, list-of-matrices
#     for hindcasts / forecasts).
#   * uncertainty-toggle semantics (coef_uncertainty,
#     trend_uncertainty, obs_uncertainty).
#   * newdata-handling edge cases (NULL, no-novel-times,
#     unseen-series error).
#   * type dispatch (response / link / expected / trend) and the
#     family-pars slot for type = "link".


# A minimal `mvgam`-class object that satisfies just enough of
# the fit interface for `forecast.mvgam()` to traverse. The
# tests below mock the primitives that would otherwise read
# real Stan output / brms machinery.
make_mock_mvgam <- function(series_levels = "s1", n_time = 10L,
                              trend_type = "AR",
                              ar_lags = 1L, ma_lags = integer(0),
                              max_lag = 1L, n_lv = NULL) {
  d <- data.frame(
    time = rep(seq_len(n_time), length(series_levels)),
    series = factor(rep(series_levels, each = n_time),
                    levels = series_levels),
    y = rep(0L, n_time * length(series_levels))
  )
  spec <- list(trend = trend_type, p = 1, ma = FALSE,
               cor = FALSE, drift = FALSE)
  class(spec) <- "mvgam_trend"

  fit <- list(
    fit = structure(list(), class = "fake_stanfit"),
    data = d,
    obs_data = d,
    formula = stats::as.formula("y ~ 1"),
    call = match.call(),
    trend_call = stats::as.formula("~ AR(p = 1)"),
    family = poisson(),
    trend_model = list(),
    mv_spec = list(
      response_names = "y",
      trend_specs = spec
    ),
    series_info = list(series_levels = series_levels),
    trend_metadata = list(
      trend_type = trend_type,
      ar_lags = ar_lags,
      ma_lags = ma_lags,
      max_lag = max_lag,
      has_cor = FALSE,
      variables = list(time_var = "time", series_var = "series"),
      dimensions = list(n_series = length(series_levels))
    ),
    standata = list(
      N_series_trend = length(series_levels),
      N_lv_trend = n_lv %||% length(series_levels),
      N_time_trend = n_time
    ),
    backend = "rstan"
  )
  class(fit) <- "mvgam"
  fit
}


# A frame reaching one occasion past the mock fit's grid, for the
# calls whose subject is something other than the horizon. Built
# from the fit's own data so the series levels and the columns
# match whatever `make_mock_mvgam()` was given.
mock_future_data <- function(fit, h = 1L) {
  d <- fit$data
  levs <- levels(d$series)
  nxt <- max(d$time) + seq_len(h)
  data.frame(
    time = rep(nxt, times = length(levs)),
    series = factor(rep(levs, each = h), levels = levs),
    y = NA_integer_
  )
}


# Stub `posterior::as_draws_matrix` so `forecast.mvgam` sees a
# named numeric matrix without touching the fake stanfit.
make_draws_mat <- function(ndraws = 5L,
                            cols = c("Intercept", "sigma_trend[1]",
                                       "ar1_trend[1]")) {
  m <- matrix(stats::rnorm(ndraws * length(cols)),
              nrow = ndraws, dimnames = list(NULL, cols))
  posterior::as_draws_matrix(m)
}


# Build a fake obs_struct that mirrors the real fields. Raw
# times live on `names(time)` per `ensure_mvgam_variables`.
# The grid a `slice_per_series()` caller supplies, carrying the
# occasions as numbers rather than as labels: that is what the
# function reads, because a label round trip moved an occasion by
# 1.4e-14 and lost a whole horizon of a continuous grid.
make_fc_grid_for_slice <- function(times, fc_times) {
  list(
    times = fc_times,
    data = data.frame(time = times),
    time_var = "time"
  )
}

make_obs_struct_for_grid <- function(times, series_int,
                                       series_levels) {
  unique_times <- sort(unique(times))
  time_pos <- match(times, unique_times)
  names(time_pos) <- as.character(times)
  list(
    time = time_pos,
    series = factor(series_levels[series_int],
                    levels = series_levels),
    series_int = as.integer(series_int),
    series_levels = series_levels,
    n_obs = length(times),
    n_times = length(unique_times),
    n_series = length(series_levels),
    unique_times = seq_along(unique_times)
  )
}


# ----- Class shape + type dispatch --------------------------------

test_that("Returns mvgam_forecast with 10 contractual fields", {
  fit <- make_mock_mvgam()
  draws <- make_draws_mat(ndraws = 3L)
  newdata <- data.frame(time = 11:12,
                         series = factor("s1", levels = "s1"),
                         y = NA_integer_)

  testthat::local_mocked_bindings(
    `as_draws_matrix` = function(...) draws,
    .package = "posterior"
  )
  testthat::local_mocked_bindings(
    extract_component_linpred = function(mvgam_fit, newdata,
                                            component, ...) {
      matrix(0.1, nrow = 3L, ncol = nrow(newdata))
    },
    get_observation_structure = function(object, newdata, ...) {
      make_obs_struct_for_grid(
        as.integer(newdata$time),
        rep(1L, nrow(newdata)),
        levels(newdata$series)
      )
    },
    propagate_trend = function(...) {
      matrix(c(0.5, 0.6), nrow = 2L, ncol = 1L)
    },
    extract_last_state = function(...) {
      list(
        params = list(sigma = 0.1, ar = matrix(0.5, 1L, 1L)),
        last_state = list(
          trends = matrix(0, 1L, 1L),
          errors = NULL
        )
      )
    },
    posterior_predict = function(...) {
      matrix(1L, nrow = 3L, ncol = 10L)
    },
    posterior_epred = function(...) {
      matrix(1, nrow = 3L, ncol = 10L)
    }
  )

  # `type = "expected"` avoids the family sampling path, which
  # would require a real stanfit object for dpar extraction; the
  # class-shape contract is the same for every type.
  fc <- forecast(fit, newdata = newdata, type = "expected")
  expect_s3_class(fc, "mvgam_forecast")
  required <- c("family", "family_pars", "type", "series_names",
                "train_observations", "train_times",
                "test_observations", "test_times",
                "hindcasts", "forecasts")
  expect_equal(sort(names(fc)), sort(required))
  dead <- c("call", "trend_call", "trend_model", "drift",
            "use_lv", "fit_engine")
  expect_false(any(dead %in% names(fc)))
  expect_identical(fc$type, "expected")
  expect_true(is.list(fc$forecasts))
  expect_true(is.list(fc$hindcasts))
})


test_that("newdata = NULL returns hindcasts only, no forecasts", {
  fit <- make_mock_mvgam()
  draws <- make_draws_mat(ndraws = 3L)
  testthat::local_mocked_bindings(
    `as_draws_matrix` = function(...) draws,
    .package = "posterior"
  )
  # Standard families: hindcast pulls per-draw trend[t, s] from the
  # stanfit via extract_trend_latent_states, composes with the
  # per-draw obs-side linpred, then samples the response via
  # predict_single_response.
  testthat::local_mocked_bindings(
    extract_trend_latent_states = function(mvgam_fit, newdata,
                                             full_draws) {
      matrix(0, nrow = 3L, ncol = nrow(newdata))
    },
    extract_component_linpred = function(mvgam_fit, newdata,
                                           component, ...) {
      matrix(0, nrow = 3L, ncol = nrow(newdata))
    },
    predict_single_response = function(object, linpred_resp, resp,
                                         draw_ids, ndraws, newdata,
                                         is_multivariate) {
      matrix(2L, nrow = length(draw_ids), ncol = nrow(newdata))
    }
  )
  # Without `newdata` there are no occasions to forecast at, and a
  # fit cannot invent them. This returned an `mvgam_forecast`
  # carrying hindcasts, a type and an empty `forecasts` list, which
  # reads as though it forecast something; `hindcast()` is the call
  # that answers at the training occasions.
  expect_error(
    forecast(fit, newdata = NULL, type = "response"),
    "'newdata' is required to forecast"
  )
  expect_identical(dim(hindcast(fit)$hindcasts[["s1"]]), c(3L, 10L))
})


test_that("type = 'link' populates family_pars", {
  fit <- make_mock_mvgam()
  draws <- make_draws_mat(
    ndraws = 4L,
    cols = c("Intercept", "sigma_trend[1]",
              "ar1_trend[1]", "shape")
  )
  # Negative-binomial family so `shape` is a real dpar.
  fit$family <- brms::negbinomial()
  newdata <- data.frame(time = 11:12,
                         series = factor("s1", levels = "s1"),
                         y = NA_integer_)
  testthat::local_mocked_bindings(
    `as_draws_matrix` = function(...) draws,
    .package = "posterior"
  )
  testthat::local_mocked_bindings(
    extract_component_linpred = function(mvgam_fit, newdata,
                                            component, ...) {
      matrix(0.1, nrow = 4L, ncol = nrow(newdata))
    },
    get_observation_structure = function(object, newdata, ...) {
      make_obs_struct_for_grid(
        as.integer(newdata$time),
        rep(1L, nrow(newdata)),
        levels(newdata$series)
      )
    },
    propagate_trend = function(...) {
      matrix(c(0.5, 0.6), nrow = 2L, ncol = 1L)
    },
    extract_last_state = function(...) {
      list(
        params = list(sigma = 0.1, ar = matrix(0.5, 1L, 1L)),
        last_state = list(
          trends = matrix(0, 1L, 1L), errors = NULL
        )
      )
    },
    posterior_predict = function(...) matrix(1L, 4L, 10L),
    posterior_epred = function(...) matrix(1, 4L, 10L)
  )
  fc <- forecast(fit, newdata = newdata, type = "link",
                  ndraws = 4L)
  expect_true("shape" %in% names(fc$family_pars))
  expect_identical(dim(fc$family_pars$shape), c(4L, 1L))
})


# ----- Validation errors ------------------------------------------

test_that("ndraws beyond available draws errors informatively", {
  fit <- make_mock_mvgam()
  draws <- make_draws_mat(ndraws = 3L)
  testthat::local_mocked_bindings(
    `as_draws_matrix` = function(...) draws,
    .package = "posterior"
  )
  # `ndraws` is still checked against the posterior, on a call that
  # supplies the occasions to forecast at.
  expect_error(
    forecast(fit, newdata = mock_future_data(fit), type = "response",
              ndraws = 50L),
    "more draws than the posterior holds"
  )
})


test_that("All multivariate / PW trend types flow through dispatch", {
  # Every supported trend type should reach build_hindcast_arms.
  # Stubs cover the Stan-direct hindcast pipeline (extract trend +
  # obs linpred + family RNG). Only an invented "BOGUS" type should
  # still fail before dispatch.
  draws <- make_draws_mat(ndraws = 2L)
  testthat::local_mocked_bindings(
    `as_draws_matrix` = function(...) draws,
    .package = "posterior"
  )
  testthat::local_mocked_bindings(
    extract_trend_latent_states = function(mvgam_fit, newdata,
                                             full_draws) {
      matrix(0, nrow = 2L, ncol = nrow(newdata))
    },
    extract_component_linpred = function(mvgam_fit, newdata,
                                           component, ...) {
      matrix(0, nrow = 2L, ncol = nrow(newdata))
    },
    predict_single_response = function(object, linpred_resp, resp,
                                         draw_ids, ndraws, newdata,
                                         is_multivariate) {
      matrix(1L, nrow = length(draw_ids), ncol = nrow(newdata))
    }
  )
  # Asked of `hindcast()`, which is the surface this block's stubs
  # cover and the one its claim names. It used to ask `forecast()`
  # with no `newdata`, which returned before reaching any trend
  # dispatch at all, so every type passed without being tried.
  for (tt in c("VAR", "CAR", "PW")) {
    fit_tt <- make_mock_mvgam(trend_type = tt)
    fit_tt$mv_spec$trend_specs$trend <- tt
    expect_no_error(hindcast(fit_tt, ndraws = 2L))
  }
})


# ----- compute_car_forecast_time ----------------------------------

test_that("compute_car_forecast_time builds the right gap vector", {
  # Fake fit with two series, last training times = c(10, 10).
  fit <- make_mock_mvgam(series_levels = c("a", "b"))
  testthat::local_mocked_bindings(
    extract_last_observed_times = function(fit, n_series) {
      c(10, 10)
    }
  )
  fc_grid <- list(times = list(a = c(11, 14, 16),
                                  b = c(11, 14, 16)))
  out <- compute_car_forecast_time(fit, fc_grid,
                                     series_levels = c("a", "b"))
  expect_equal(out, c(1, 3, 2))
})


test_that("compute_car_forecast_time errors on per-series time mismatch", {
  fit <- make_mock_mvgam(series_levels = c("a", "b"))
  testthat::local_mocked_bindings(
    extract_last_observed_times = function(fit, n_series) {
      c(10, 10)
    }
  )
  # Series 'a' has gaps c(1, 1); series 'b' has gaps c(1, 2).
  fc_grid <- list(times = list(a = c(11, 12),
                                  b = c(11, 13)))
  expect_error(
    compute_car_forecast_time(fit, fc_grid,
                                series_levels = c("a", "b")),
    "share the same forecast"
  )
})


test_that("compute_car_forecast_time ignores series with no forecast rows", {
  fit <- make_mock_mvgam(series_levels = c("a", "b"))
  testthat::local_mocked_bindings(
    extract_last_observed_times = function(fit, n_series) {
      c(10, 10)
    }
  )
  fc_grid <- list(times = list(a = c(11, 14, 16),
                                  b = integer(0L)))
  out <- compute_car_forecast_time(fit, fc_grid,
                                     series_levels = c("a", "b"))
  expect_equal(out, c(1, 3, 2))
})


test_that("Trendless fit rejects type = 'trend' but accepts others", {
  # Trendless forecasting projects the obs-side linear predictor
  # onto newdata; there is no latent trend trajectory to extract,
  # so only `type = "trend"` is undefined. The other types
  # (`"link"`, `"expected"`, `"response"`) route through the
  # `build_trendless_forecast_arms()` helper. End-to-end recovery
  # is covered by the live-fit fixtures.
  fit <- make_mock_mvgam()
  fit$mv_spec$trend_specs <- NULL
  draws <- make_draws_mat(ndraws = 2L)
  testthat::local_mocked_bindings(
    `as_draws_matrix` = function(...) draws,
    .package = "posterior"
  )
  newdata <- data.frame(
    time = 11:12,
    series = factor("s1", levels = "s1"),
    y = NA_real_
  )
  expect_error(
    forecast(fit, newdata = newdata, type = "trend"),
    "not defined for trendless fits"
  )
})


test_that("Newdata with unseen series levels errors crisply", {
  fit <- make_mock_mvgam()
  draws <- make_draws_mat(ndraws = 2L)
  newdata <- data.frame(time = 11:12,
                         series = factor("ghost", levels = "ghost"),
                         y = NA_integer_)
  testthat::local_mocked_bindings(
    `as_draws_matrix` = function(...) draws,
    .package = "posterior"
  )
  testthat::local_mocked_bindings(
    posterior_predict = function(...) matrix(1L, 2L, 10L)
  )
  expect_error(
    forecast(fit, newdata = newdata, type = "response"),
    "series levels not seen"
  )
})


test_that("Newdata that names no time or no series errors", {
  # Two conditions, refused separately. The time is read here, so a
  # frame without one cannot be placed at all. Which columns name
  # the series is the record's question, and a frame carrying
  # neither a series column nor a grouping answers it with nothing:
  # asking for a named column instead turned away hierarchical fits
  # handed the very frames they were fitted on.
  fit <- make_mock_mvgam()
  draws <- make_draws_mat(ndraws = 2L)
  testthat::local_mocked_bindings(
    `as_draws_matrix` = function(...) draws,
    .package = "posterior"
  )
  testthat::local_mocked_bindings(
    posterior_predict = function(...) matrix(1L, 2L, 10L)
  )
  expect_error(
    forecast(
      fit,
      newdata = data.frame(notime = 11:12, y = NA_integer_),
      type = "response"
    ),
    "must contain the time column"
  )
  expect_error(
    forecast(
      fit,
      newdata = data.frame(time = 11:12, y = NA_integer_),
      type = "response"
    ),
    "names no series"
  )
})


# ----- pad_or_trim_rows -------------------------------------------

test_that("pad_or_trim_rows pads short grids by repeating final row", {
  g <- matrix(c(1, 2, 3, 4), nrow = 2L, ncol = 2L)
  out <- pad_or_trim_rows(g, target_rows = 4L)
  expect_identical(dim(out), c(4L, 2L))
  expect_equal(out[1, ], c(1, 3))
  expect_equal(out[2, ], c(2, 4))
  # Rows 3 and 4 repeat the final source row.
  expect_equal(out[3, ], c(2, 4))
  expect_equal(out[4, ], c(2, 4))
})


test_that("pad_or_trim_rows trims long grids from the head", {
  g <- matrix(seq_len(6L), nrow = 6L, ncol = 1L)
  out <- pad_or_trim_rows(g, target_rows = 3L)
  expect_identical(dim(out), c(3L, 1L))
  # Trimming from the head keeps the trailing 3 rows.
  expect_equal(as.numeric(out), c(4, 5, 6))
})


test_that("pad_or_trim_rows preserves an exact-fit grid", {
  g <- matrix(c(1, 2), nrow = 2L, ncol = 1L)
  expect_identical(pad_or_trim_rows(g, 2L), g)
})


test_that("pad_or_trim_rows handles zero-row input as a zero pad", {
  g <- matrix(0, nrow = 0L, ncol = 2L)
  out <- pad_or_trim_rows(g, target_rows = 3L)
  expect_identical(dim(out), c(3L, 2L))
  expect_true(all(out == 0))
})


# ----- trend_linpred_grid -----------------------------------------

# A small obs_struct standing in for `get_observation_structure()`:
# 3 times x 2 series, stored in (time, series) row order.
lp_obs_struct <- function() {
  list(
    unique_times = c(41, 42, 43),
    time = c(41, 41, 42, 42, 43, 43),
    series_int = c(1L, 2L, 1L, 2L, 1L, 2L),
    n_series = 2L
  )
}


test_that("trend_linpred_grid reshapes one draw to series scale", {
  lp <- matrix(seq_len(12), nrow = 2L, byrow = TRUE)
  out <- trend_linpred_grid(lp, lp_obs_struct(), draw_row = 2L,
                              n_rows = 3L, n_series = 2L)
  expect_identical(dim(out), c(3L, 2L))
  # Row 2 of `lp` is 7:12, laid out as (t, s) pairs.
  expect_equal(out[1, ], c(7, 8))
  expect_equal(out[2, ], c(9, 10))
  expect_equal(out[3, ], c(11, 12))
})


test_that("trend_linpred_grid is zero without a trend formula", {
  out <- trend_linpred_grid(NULL, NULL, draw_row = 1L,
                              n_rows = 4L, n_series = 3L)
  expect_identical(dim(out), c(4L, 3L))
  expect_true(all(out == 0))
})


test_that("trend_linpred_grid returns an empty grid for zero rows", {
  out <- trend_linpred_grid(NULL, NULL, draw_row = 1L,
                              n_rows = 0L, n_series = 2L)
  expect_identical(dim(out), c(0L, 2L))
})


test_that("trend_linpred_grid keeps series scale, not draw width", {
  # The grid is indexed by series even when the caller propagates
  # at the latent-variable grain, so its width tracks `n_series`.
  lp <- matrix(seq_len(6), nrow = 1L)
  out <- trend_linpred_grid(lp, lp_obs_struct(), draw_row = 1L,
                              n_rows = 2L, n_series = 2L)
  expect_identical(ncol(out), 2L)
  # Trimming from the head keeps the two most recent times.
  expect_equal(out[2, ], c(5, 6))
})


# ----- the zero-mean propagation convention ------------------------

test_that("centred propagation reproduces the kernel's mean form", {
  # `trend_arma_recursC()` can carry a time-varying mean itself,
  # via `trend[t] = lp[t] + A (trend[t - 1] - lp[t - 1]) + e[t]`.
  # mvgam instead advances the zero-mean latent state and adds
  # `mu_trend` at series scale, which is the only form that also
  # covers a factor model. The two agree exactly, and that
  # equivalence is what lets one convention serve every trend.
  set.seed(404)
  h <- 8L
  n_series <- 2L
  max_lag <- 1L
  total <- h + max_lag
  A <- array(diag(c(0.6, 0.35)), dim = c(2L, 2L, 1L))
  B <- array(0, dim = c(2L, 2L, 0L))
  innov <- matrix(rnorm(total * n_series, sd = 0.3),
                    nrow = total, ncol = n_series)
  lp <- matrix(rnorm(total * n_series, sd = 1.2),
                 nrow = total, ncol = n_series)
  state <- matrix(c(1.1, -0.4), nrow = 1L)

  with_mean <- trend_arma_recursC(
    ar_lags = 1L, ma_lags = integer(0), drift = c(0, 0),
    A = A, B = B, innovations = innov, linpreds = lp,
    last_trends = state, h = h
  )
  zero_mean <- trend_arma_recursC(
    ar_lags = 1L, ma_lags = integer(0), drift = c(0, 0),
    A = A, B = B, innovations = innov,
    linpreds = matrix(0, nrow = total, ncol = n_series),
    last_trends = state - lp[seq_len(max_lag), , drop = FALSE],
    h = h
  )
  reconstructed <- zero_mean +
    lp[seq.int(max_lag + 1L, total), , drop = FALSE]
  expect_equal(reconstructed, with_mean, tolerance = 1e-12)
})


# ----- the mean enters once, at series scale -----------------------

test_that("the forecast centres the state and re-adds the mean", {
  # Pins both halves of the convention separately by giving the
  # training tail and the forecast window different trend linear
  # predictors. With a propagator that echoes the state it was
  # handed, the answer is (state - lp_tail) + lp_forecast.
  # Dropping the centring gives 3.5, dropping the re-add gives
  # 1.5, and dropping both gives 2, so no single omission
  # survives.
  fit <- make_mock_mvgam()
  draws <- make_draws_mat(ndraws = 3L)
  newdata <- data.frame(time = 11:12,
                          series = factor("s1", levels = "s1"),
                          y = NA_integer_)

  testthat::local_mocked_bindings(
    `as_draws_matrix` = function(...) draws,
    .package = "posterior"
  )
  testthat::local_mocked_bindings(
    extract_component_linpred = function(mvgam_fit, newdata,
                                            component, ...) {
      if (!identical(component, "trend")) {
        return(matrix(0, nrow = 3L, ncol = nrow(newdata)))
      }
      # The tail carries one row per series, the forecast grid two.
      value <- if (nrow(newdata) == 1L) 0.5 else 1.5
      matrix(value, nrow = 3L, ncol = nrow(newdata))
    },
    get_observation_structure = function(object, newdata, ...) {
      make_obs_struct_for_grid(
        as.integer(newdata$time),
        rep(1L, nrow(newdata)),
        levels(newdata$series)
      )
    },
    propagate_trend = function(trend_model, params, h, n_series,
                                 last_state = NULL, ...) {
      matrix(rep(as.numeric(last_state$trends), h),
               nrow = h, ncol = n_series, byrow = TRUE)
    },
    extract_last_state = function(...) {
      list(
        params = list(sigma = 0.1, ar = matrix(0.5, 1L, 1L)),
        last_state = list(
          trends = matrix(2, 1L, 1L),
          errors = NULL
        )
      )
    }
  )

  fc <- forecast(fit, newdata = newdata, type = "trend")
  expect_equal(unname(as.numeric(fc$forecasts[["s1"]])),
                 rep(2 - 0.5 + 1.5, 6L))
})


# ----- one basis for state and loadings ---------------------------

test_that("a factor forecast projects with the sampled loadings", {
  # A QR-identified fit stores both `Z` with `lv_trend` and
  # `Z_tilde` with `lv_trend_tilde`, and only a matching pair
  # rebuilds the trend. The propagated state is the raw
  # `lv_trend`, so the projection has to use the raw `Z`. Here
  # the two loadings matrices differ, so pairing the state with
  # `Z_tilde` would give 2 rather than 1 in every cell.
  fit <- make_mock_mvgam(series_levels = c("s1", "s2"),
                           n_lv = 1L, trend_type = "ZMVN",
                           ar_lags = integer(0), max_lag = 0L)
  fit$mv_spec$trend_specs$trend <- "ZMVN"
  draws <- posterior::as_draws_matrix(matrix(
    c(1, 1, 2, 2), nrow = 1L,
    dimnames = list(NULL, c("Z[1,1]", "Z[2,1]",
                              "Z_tilde[1,1]", "Z_tilde[2,1]"))
  ))
  newdata <- data.frame(
    time = c(11L, 11L, 12L, 12L),
    series = factor(c("s1", "s2", "s1", "s2"),
                      levels = c("s1", "s2")),
    y = NA_integer_
  )

  testthat::local_mocked_bindings(
    `as_draws_matrix` = function(...) draws,
    .package = "posterior"
  )
  testthat::local_mocked_bindings(
    extract_component_linpred = function(mvgam_fit, newdata,
                                            component, ...) {
      matrix(0, nrow = 1L, ncol = nrow(newdata))
    },
    get_observation_structure = function(object, newdata, ...) {
      make_obs_struct_for_grid(
        as.integer(newdata$time),
        as.integer(newdata$series),
        levels(newdata$series)
      )
    },
    propagate_trend = function(trend_model, params, h, n_series,
                                 ...) {
      matrix(1, nrow = h, ncol = n_series)
    },
    extract_last_state = function(...) {
      list(
        params = list(Sigma = diag(1)),
        last_state = list(
          trends = matrix(0, nrow = 0L, ncol = 1L),
          errors = NULL
        ),
        n_lv_active = 1L
      )
    }
  )

  fc <- forecast(fit, newdata = newdata, type = "trend")
  expect_equal(unname(as.numeric(fc$forecasts[["s1"]])), c(1, 1))
  expect_equal(unname(as.numeric(fc$forecasts[["s2"]])), c(1, 1))
})


# ----- slice_per_series -------------------------------------------

test_that("slice_per_series picks the right cells per raw time", {
  # 2 series, 3 forecast times each, interleaved in obs_struct
  # row order (t, s) = (41,a), (41,b), (42,a), (42,b), (43,a),
  # (43,b). Mat values encode (time*10 + series_int).
  obs_struct <- make_obs_struct_for_grid(
    times = c(41, 41, 42, 42, 43, 43),
    series_int = c(1, 2, 1, 2, 1, 2),
    series_levels = c("a", "b")
  )
  mat <- matrix(
    c(411, 412, 421, 422, 431, 432),
    nrow = 1L
  )
  fc_grid <- make_fc_grid_for_slice(
    times = c(41, 41, 42, 42, 43, 43),
    fc_times = list(a = c(41, 42, 43), b = c(41, 42, 43))
  )
  out <- slice_per_series(mat, fc_grid, obs_struct,
                            ndraws_use = 1L,
                            series_levels = c("a", "b"))
  expect_equal(as.numeric(out[["a"]]), c(411, 421, 431))
  expect_equal(as.numeric(out[["b"]]), c(412, 422, 432))
})


test_that("slice_per_series handles per-series horizon differences", {
  # Series 'a' has 3 forecast times, 'b' has 2.
  obs_struct <- make_obs_struct_for_grid(
    times = c(41, 41, 42, 42, 43),
    series_int = c(1, 2, 1, 2, 1),
    series_levels = c("a", "b")
  )
  mat <- matrix(c(411, 412, 421, 422, 431), nrow = 1L)
  fc_grid <- make_fc_grid_for_slice(
    times = c(41, 41, 42, 42, 43),
    fc_times = list(a = c(41, 42, 43), b = c(41, 42))
  )
  out <- slice_per_series(mat, fc_grid, obs_struct,
                            ndraws_use = 1L,
                            series_levels = c("a", "b"))
  expect_identical(dim(out[["a"]]), c(1L, 3L))
  expect_identical(dim(out[["b"]]), c(1L, 2L))
  expect_equal(as.numeric(out[["a"]]), c(411, 421, 431))
  expect_equal(as.numeric(out[["b"]]), c(412, 422))
})


test_that("the forecast grid does not depend on newdata row order", {
  # `fc_times` is sorted; the truths and the rows the linear
  # predictor is built from were taken in arrival order. Passing
  # the same future rows shuffled therefore permuted the
  # observations and the forecast columns independently, and
  # `score()` paired each truth with the wrong horizon without
  # complaining.
  training <- data.frame(
    time = rep(1:10, 2),
    series = factor(rep(c("a", "b"), each = 10)),
    y = rnorm(20)
  )
  future <- data.frame(
    time = rep(11:13, 2),
    series = factor(rep(c("a", "b"), each = 3), levels = c("a", "b")),
    y = c(101, 102, 103, 201, 202, 203)
  )
  train_info <- list(
    times = list(a = 1:10, b = 1:10),
    observations = list(a = training$y[1:10], b = training$y[11:20]),
    data = training, series_var = "series", time_var = "time",
    resp = "y"
  )
  ordered_grid <- resolve_forecast_grid(
    object = NULL, newdata = future, training = train_info,
    series_levels = c("a", "b")
  )
  shuffled_grid <- resolve_forecast_grid(
    object = NULL, newdata = future[c(5, 2, 6, 1, 4, 3), ],
    training = train_info, series_levels = c("a", "b")
  )

  # Truths follow the sorted times, not the order they arrived in.
  expect_equal(ordered_grid$observations, shuffled_grid$observations)
  expect_equal(shuffled_grid$observations$a, c(101, 102, 103))
  expect_equal(shuffled_grid$observations$b, c(201, 202, 203))

  # And so do the rows the linear predictor is built from.
  expect_equal(shuffled_grid$data$time, ordered_grid$data$time)
  expect_equal(
    as.character(shuffled_grid$data$series),
    as.character(ordered_grid$data$series)
  )
})


test_that("an argument neither method reads is refused, not swallowed", {
  # Every argument after `...` is matched by name, so a misspelling
  # falls into `...` and the call proceeds on the default it meant to
  # override. Nothing warns, and the returned object has the right
  # class and shape, which is what made the same shape of defect hard
  # to see on `posterior_predict()`.
  fit <- make_mock_mvgam()
  expect_error(hindcast(fit, incl_autcor = TRUE), "must be empty")
  expect_error(hindcast(fit, ndraw = 5L), "must be empty")
  expect_error(forecast(fit, newdata = fit$data, ndraw = 5L),
               "must be empty")
  # The offending name is reported, so the caller can see which one.
  expect_error(hindcast(fit, incl_autcor = TRUE), "incl_autcor")
})
