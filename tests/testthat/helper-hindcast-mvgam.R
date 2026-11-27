# Helpers shared by test-hindcast-mvgam.R and test-extra-hindcast-mvgam.R.
# CI tests for `hindcast.mvgam()` and the `mvgam_forecast` methods on
# mock fits: the object's slots, the dispatch by `type`, the
# `obs_uncertainty` toggle, draw selection and multivariate forecasts.


# A mock fit holding the slots `hindcast()` uses
make_hindcast_mock <- function(series_levels = "s1",
                               n_time = 8L,
                               trend_type = "AR") {
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
    formula = stats::as.formula("y ~ 1"),
    call = match.call(),
    trend_call = stats::as.formula("~ AR(p = 1)"),
    family = poisson(),
    trend_model = list(),
    mv_spec = list(
      trend_specs = spec
    ),
    trend_metadata = c(mock_axis_record(d), list(
      trend_type = trend_type,
      ar_lags = 1L,
      ma_lags = integer(0L),
      max_lag = 1L,
      has_cor = FALSE
    )),
    standata = list(
      N_series_trend = length(series_levels),
      N_lv_trend = length(series_levels),
      N_time_trend = n_time
    ),
    backend = "rstan"
  )
  class(fit) <- "mvgam"
  fit
}

make_hindcast_draws <- function(ndraws = 3L,
                                cols = c("Intercept",
                                         "sigma_trend[1]",
                                         "ar1_trend[1]")) {
  m <- matrix(stats::rnorm(ndraws * length(cols)),
              nrow = ndraws, dimnames = list(NULL, cols))
  posterior::as_draws_matrix(m)
}
