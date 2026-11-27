# Helpers shared by test-compare-scores.R and test-extra-compare-scores.R.
# `compare_scores()`, `compare_elpds()`, `ensemble()` and
# `print.mvgam_forecast()`, on small hand-built `mvgam_forecast` and
# `mvgam_lfo` objects.


# ---- Mock helpers ------------------------------------------------

make_mock_forecast <- function(n_series = 2L, h = 5L,
                                 ndraws = 30L, type = "response",
                                 family = "gaussian",
                                 obs_offset = 0,
                                 seed = 1L) {
  set.seed(seed)
  series_names <- factor(
    paste0("s", seq_len(n_series)),
    levels = paste0("s", seq_len(n_series))
  )
  forecasts <- lapply(seq_len(n_series), function(s) {
    matrix(stats::rnorm(ndraws * h, mean = s, sd = 1),
            nrow = ndraws, ncol = h)
  })
  names(forecasts) <- levels(series_names)
  test_observations <- lapply(
    seq_len(n_series), function(s) rep(s + obs_offset, h)
  )
  names(test_observations) <- levels(series_names)
  structure(
    list(
      type = type,
      series_names = series_names,
      forecasts = forecasts,
      hindcasts = list(),
      test_observations = test_observations,
      test_times = NULL,
      train_observations = list(),
      train_times = list(),
      family = family
    ),
    class = "mvgam_forecast"
  )
}


make_mock_lfo <- function(elpds, eval_timepoints = NULL,
                            fc_horizon = 1L, pareto_ks = NULL,
                            refit_triggered = NULL) {
  n <- length(elpds)
  if (is.null(eval_timepoints)) eval_timepoints <- seq_len(n) + 30L
  if (is.null(pareto_ks)) pareto_ks <- rep(0.3, n)
  if (is.null(refit_triggered)) refit_triggered <- rep(FALSE, n)
  structure(
    list(
      elpds = elpds,
      eval_timepoints = eval_timepoints,
      fc_horizon = fc_horizon,
      pareto_ks = pareto_ks,
      refit_triggered = refit_triggered,
      refits_at = integer(0),
      pareto_k_threshold = 0.7
    ),
    class = "mvgam_lfo"
  )
}
