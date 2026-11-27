# Helpers shared by test-plot-residcor.R and test-extra-plot-residcor.R.
# `plot.mvgam_residcor()` and its heatmap helpers, on hand-built
# `mvgam_residcor` objects.

.make_residcor <- function(cor_mat) {
  structure(
    list(
      cor = cor_mat,
      sig_cor = cor_mat,
      prob_nonzero = cor_mat * 0 + 0.6,
      prob_threshold = 0.95,
      series_names = colnames(cor_mat) %||%
        paste0("s", seq_len(nrow(cor_mat))),
      n_series = nrow(cor_mat)
    ),
    class = "mvgam_residcor"
  )
}
