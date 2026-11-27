# Helpers shared by test-plot-mvgam-forecast.R and test-extra-plot-mvgam-forecast.R.
# plot.mvgam_forecast tests using hand-built minimal forecast
# objects — no Stan fit dependency.

# Build a tiny mvgam_forecast with controllable hindcast / forecast
# arms and an arbitrary number of series. Cells default to draws
# of N(series_offset, 1) so series can be distinguished visually
# in the local renderer.
.make_fc <- function(
  series = c("a", "b"),
  n_train = 10L,
  n_test = 5L,
  ndraws = 40L,
  include_forecast = TRUE,
  include_train_obs = TRUE,
  include_test_obs = TRUE
) {
  set.seed(1L)
  train_times <- stats::setNames(
    lapply(series, function(.) seq_len(n_train)),
    series
  )
  test_times <- stats::setNames(
    lapply(series, function(.) n_train + seq_len(n_test)),
    series
  )
  hindcasts <- stats::setNames(
    lapply(seq_along(series), function(i) {
      matrix(
        rnorm(ndraws * n_train, mean = i),
        nrow = ndraws, ncol = n_train
      )
    }),
    series
  )
  forecasts <- stats::setNames(
    lapply(seq_along(series), function(i) {
      matrix(
        rnorm(ndraws * n_test, mean = i),
        nrow = ndraws, ncol = n_test
      )
    }),
    series
  )
  train_obs <- stats::setNames(
    lapply(seq_along(series), function(i) {
      rnorm(n_train, mean = i)
    }),
    series
  )
  test_obs <- stats::setNames(
    lapply(seq_along(series), function(i) {
      rnorm(n_test, mean = i)
    }),
    series
  )
  structure(
    list(
      call = NULL,
      family = "gaussian",
      type = "response",
      series_names = factor(series, levels = series),
      train_observations = if (include_train_obs) train_obs else NULL,
      train_times = train_times,
      test_observations = if (include_test_obs &&
                                include_forecast) {
        test_obs
      } else {
        NULL
      },
      test_times = if (include_forecast) test_times else NULL,
      hindcasts = hindcasts,
      forecasts = if (include_forecast) forecasts else NULL
    ),
    class = "mvgam_forecast"
  )
}
