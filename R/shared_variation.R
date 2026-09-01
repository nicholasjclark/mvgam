#' Posterior summary of the factor-implied shared variation matrix
#'
#' For a latent-factor `mvgam` fit, returns the posterior
#' distribution of the shared variation matrix
#' \eqn{\Delta = Z Z^\top}, where `Z` is the `n_series x n_lv`
#' loadings matrix. `Delta` is the cross-series covariance
#' pattern that the latent factors induce on the trends; its
#' diagonal entries `Delta_ii = sum(Z[i, ]^2)` describe how
#' strongly each series is captured by the factor structure,
#' and the off-diagonal entries describe pairwise factor-driven
#' coupling between series.
#'
#' `Delta` is rotation-invariant: any orthogonal rotation
#' `Z* = Z Q` leaves `Z* Z*' = Z Q Q' Z' = Z Z'` unchanged.
#' This means the posterior summary returned by
#' `shared_variation()` is the same whether computed from the
#' raw sampled `Z` or from the QR-identified `Z_tilde` (Heaps &
#' Jermyn 2024). For fully-fixed-Z fits (`trend_map` with no
#' NAs) the function returns a degenerate summary built from the
#' user-supplied loadings (zero posterior spread).
#'
#' @section Key inferential property:
#' Heaps & Jermyn (2024) Sect. 3 show that under the structured
#' matrix-normal prior `Z | Phi, Psi ~ MN(0, Phi, Psi)`,
#'   `E(Delta) = tr(Psi^2) * Phi`.
#' The among-row scale matrix `Phi` is therefore proportional
#' to the **prior expectation** of `Delta` (an observable
#' quantity), not the prior expectation of `Z` (a
#' rotation-arbitrary latent matrix). Encoding domain knowledge
#' into `Phi` via `loadings_prior` shapes the prior on the
#' observable pattern. Inspecting `shared_variation(fit)` after
#' fitting is the natural way to read off the
#' factor-implied covariance posterior.
#'
#' @param object A fitted `mvgam` object whose trend model
#'   includes latent factors (`n_lv < n_series`).
#' @param probs Numeric vector of length 2 giving the quantile
#'   probabilities used for the credible bounds. Defaults to
#'   `c(0.025, 0.975)` (95% intervals).
#' @param robust Logical. When `TRUE`, point estimates use the
#'   posterior median and spread uses the median absolute
#'   deviation; when `FALSE` (default) the posterior mean and
#'   standard deviation are used.
#' @param ... Additional arguments for S3 dispatch (currently
#'   unused).
#'
#' @return An object of class `mvgam_shared_variation`, a list
#'   with:
#'   \itemize{
#'     \item `delta`: posterior point estimate of the
#'       `n_series x n_series` shared variation matrix.
#'     \item `delta_se`: posterior spread per entry.
#'     \item `delta_lower`, `delta_upper`: per-entry credible
#'       bounds at `probs`.
#'     \item `delta_ess`: per-entry bulk effective sample size.
#'     \item `n_series`, `series_names`, `n_lv`: model
#'       dimensions and series labels.
#'     \item `probs`: quantile probabilities used.
#'   }
#'
#' @references
#' Heaps, S. E. and Jermyn, I. H. (2024). Structured prior
#' distributions for the covariance matrix in latent factor
#' models. \emph{Statistics and Computing}, 34:143.
#' \doi{10.1007/s11222-024-10454-0}
#'
#' @seealso \code{\link{residual_cor}} for the implied
#'   correlation matrix `cov2cor(Delta)`, \code{\link{mvgam}}
#'   for the `loadings_prior` argument that lets `Phi` shape
#'   `E(Delta)`.
#'
#' @examples
#' \dontrun{
#' set.seed(1)
#' simdat <- sim_closure_unit_data(
#'   family    = occ(),
#'   n_species = 4L,
#'   n_sites   = 50L,
#'   n_visits  = 4L,
#'   type      = 2L
#' )
#' mod <- jsdgam(
#'   formula        = bf(y ~ env, p ~ tod_c),
#'   factor_formula = ~ -1,
#'   data           = simdat$data_train,
#'   family         = occ(),
#'   n_lv           = 2L,
#'   chains         = 2,
#'   silent         = 2
#' )
#'
#' # Posterior summary of the Z Z' shared-variation matrix
#' # (the implied species-by-species residual covariance).
#' shared_variation(mod)
#' }
#'
#' @author Nicholas J Clark
#' @export
shared_variation <- function(object, ...) {
  UseMethod("shared_variation")
}


#' @rdname shared_variation
#' @export
shared_variation.mvgam <- function(object,
                                   probs = c(0.025, 0.975),
                                   robust = FALSE, ...) {
  checkmate::assert_class(object, "mvgam")
  checkmate::assert_numeric(
    probs, len = 2L, lower = 0, upper = 1,
    any.missing = FALSE, sorted = TRUE
  )
  checkmate::assert_flag(robust)

  n_lv <- detect_factor_n_lv(object)
  if (is.null(n_lv)) {
    stop(insight::format_error(c(
      "shared_variation() requires a latent-factor fit.",
      x = "The fitted trend model has no `n_lv` (no factors).",
      i = paste0(
        "Refit with a factor constructor (e.g. ",
        "`trend_formula = ~ AR(p = 1, n_lv = 2)`)."
      )
    )))
  }

  series_names <- resolve_series_info(object)$series_levels
  # Delegate the per-draw Z Z' computation to the existing
  # `extract_cov_draws_factor()` helper, which already routes
  # through `resolve_factor_loadings()` for the fixed-vs-
  # sampled-Z decision.
  delta_draws <- extract_cov_draws_factor(
    object, n_lv = n_lv, n_series = length(series_names)
  )
  stats <- summarise_unconstrained_array(
    delta_draws, robust = robust, probs = probs,
    series_names = series_names
  )
  structure(
    list(
      delta = stats$point,
      delta_se = stats$se,
      delta_lower = stats$lower,
      delta_upper = stats$upper,
      delta_ess = stats$ess,
      n_series = length(series_names),
      series_names = series_names,
      n_lv = n_lv,
      probs = probs,
      robust = robust
    ),
    class = "mvgam_shared_variation"
  )
}


#' @export
print.mvgam_shared_variation <- function(x, digits = 3L, ...) {
  cat("Factor-implied shared variation matrix (Delta = Z Z')\n")
  cat(
    "  ", x$n_series, " series x ", x$n_lv,
    " latent factors", sep = ""
  )
  if (isTRUE(x$robust)) {
    cat(" | point = posterior median, spread = MAD\n")
  } else {
    cat(" | point = posterior mean, spread = SD\n")
  }
  cat("  Quantile bounds:",
      paste0("[", x$probs[1L], ", ", x$probs[2L], "]"), "\n\n")
  cat("Diagonal (captured variance per series):\n")
  diag_df <- data.frame(
    Estimate = diag(x$delta),
    lower = diag(x$delta_lower),
    upper = diag(x$delta_upper),
    row.names = x$series_names
  )
  colnames(diag_df) <- c(
    "Estimate",
    paste0("Q", format(100 * x$probs[1L], trim = TRUE)),
    paste0("Q", format(100 * x$probs[2L], trim = TRUE))
  )
  print(round(diag_df, digits))
  cat("\nUse `plot(x)` for the full posterior mean heatmap.\n")
  invisible(x)
}


#' Plot the factor-implied shared variation matrix
#'
#' Heatmap of the posterior point estimate of
#' \eqn{\Delta = Z Z^\top}. Diagonal cells show the per-series
#' captured variance (always positive); off-diagonal cells show
#' factor-driven cross-series covariance, which can be positive
#' or negative. The diverging fill scale is symmetric around
#' zero so positive and negative couplings read on equivalent
#' colour intensities.
#'
#' @param x A `mvgam_shared_variation` object returned by
#'   [shared_variation()].
#' @param type One of `"point"` (default) showing the posterior
#'   point estimate, or `"uncertainty"` showing the per-entry
#'   posterior spread (`delta_se`).
#' @param ... Ignored.
#'
#' @return A `ggplot` object.
#'
#' @seealso [shared_variation()],
#'   [plot.mvgam_residcor()] for the implied correlation
#'   structure as a residual-correlation heatmap.
#' @author Nicholas J Clark
#' @method plot mvgam_shared_variation
#' @export
plot.mvgam_shared_variation <- function(x,
                                        type = c("point", "uncertainty"),
                                        ...) {
  checkmate::assert_class(x, "mvgam_shared_variation")
  type <- match.arg(type)
  mat <- switch(type, point = x$delta, uncertainty = x$delta_se)
  rng <- max(abs(range(mat, na.rm = TRUE, finite = TRUE)))
  if (type == "point") {
    limits <- c(-rng, rng)
    scale_name <- "Posterior\nDelta"
  } else {
    limits <- c(0, rng)
    scale_name <- "Posterior\nspread"
  }
  long <- gather_matrix(mat, drop_diag = FALSE, drop_upper = FALSE)
  p <- ggplot2::ggplot(
    data = long,
    mapping = ggplot2::aes(x = Var1, y = Var2, fill = value)
  ) +
    ggplot2::geom_tile(colour = "grey50")
  if (type == "point") {
    p <- p + mvgam_diverging_scale(name = scale_name, limits = limits)
  } else {
    warm <- mvgam_palette("red")[5L]
    p <- p + ggplot2::scale_fill_gradient(
      name = scale_name,
      low = "grey95", high = warm,
      limits = limits, na.value = "grey30"
    )
  }
  p +
    ggplot2::labs(x = "", y = "") +
    ggplot2::scale_x_discrete(guide = ggplot2::guide_axis(angle = 45)) +
    ggplot2::scale_y_discrete(limits = rev) +
    mvgam_theme()
}
