# Helpers shared by test-lfo-cv-mvgam.R and test-extra-lfo-cv-mvgam.R.


# ---- loo_compare.mvgam_lfo ---------------------------------------

mk_mvgam_lfo <- function(elpds, eval_timepoints = NULL,
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
