#' Posterior summary of the factor-implied shared variation matrix
#'
#' For a latent-factor `mvgam` fit, returns the posterior of the shared
#' variation matrix \eqn{\Delta = Z \Sigma Z^\top}. `Z` is the
#' `n_series x n_lv` loadings matrix and \eqn{\Sigma} is the covariance
#' of the latent states. `Delta` is the covariance the factors induce
#' between the series' trends. Its diagonal gives the variance of each
#' series that the factors capture. Its off-diagonal entries give the
#' covariance between pairs of series.
#'
#' A factor column enters the trend as `sigma_trend[k] * Z[, k]`. `Z Z'`
#' alone would weight every column equally, while the model scales each
#' column by its latent standard deviation. Under multiplicative gamma
#' process shrinkage those scales hold the whole of the shrinkage. For
#' an autoregressive trend \eqn{\Sigma} is the stationary covariance of
#' the latent states, as in [residual_cor()].
#'
#' `Delta` is invariant to rotation. An orthogonal `Z* = Z Q` with
#' `Sigma* = Q' Sigma Q` leaves `Z* Sigma* Z*'` unchanged. The summary
#' takes the same value for the sampled `Z` and for the QR-identified
#' `Z_tilde` of Heaps and Jermyn (2024). It uses the sampled `Z`, the
#' basis \eqn{\Sigma} is stated in. A `trend_map` without `NA` entries
#' fixes `Z`. The posterior spread of `Delta` then comes from
#' \eqn{\Sigma} alone.
#'
#' @section Relation to `loadings_prior`:
#' Under the matrix-normal prior `Z | Phi, Psi ~ MN(0, Phi, Psi)`, the
#' prior expectation of `Z Z'` is `tr(Psi) Phi` (Heaps and Jermyn 2024,
#' section 3). `Phi` sets the prior expectation of the coupling pattern
#' between series, while `Z` itself is identified only up to rotation.
#' A `loadings_prior` in [mvgam()] encodes domain knowledge about that
#' pattern through `Phi`.
#'
#' `Delta` also carries the latent covariance. Its scale includes the
#' variance of the latent states. Compare its pattern with `Phi`.
#'
#' @inheritParams predict.mvgam
#' @param object A fitted `mvgam` object whose trend model
#'   includes latent factors (`n_lv < n_series`).
#' @param ... Unused. Anything passed here is refused.
#'
#' @return An object of class `mvgam_shared_variation`, a list
#'   with:
#'   \itemize{
#'     \item `delta`: posterior point estimate of the
#'       `n_series x n_series` shared variation matrix.
#'     \item `delta_se`: posterior spread per entry.
#'     \item `delta_lower`, `delta_upper`: per-entry credible
#'       bounds at `probs`.
#'     \item `delta_ess`: per-entry effective sample size, from
#'       [posterior::ess_basic()].
#'     \item `n_series`, `series_names`, `n_lv`: model
#'       dimensions and series labels.
#'     \item `probs`, `robust`: the summary settings.
#'   }
#'
#' @references
#' Heaps, S. E. and Jermyn, I. H. (2024). Structured prior
#' distributions for the covariance matrix in latent factor
#' models. \emph{Statistics and Computing}, 34:143.
#' \doi{10.1007/s11222-024-10454-0}
#'
#' @seealso [residual_cor()] for the implied correlation matrix,
#'   `cov2cor(Delta)`. Under [mvn()] and [mvt()] it adds each series'
#'   residual variance to the diagonal of `Delta` first. The
#'   `r mvgam_article("jsdgam", "joint species distribution article")`
#'   interprets both for a community of species.
#'
#' @examples
#' \dontrun{
#' # Detections of 6 species whose occupancy shares two latent factors
#' simdat <- sim_closure_unit_data(
#'   family    = occ(),
#'   n_species = 6L,
#'   n_sites   = 80L,
#'   n_visits  = 4L,
#'   n_lv      = 2L,
#'   seed      = 1L
#' )
#' mod <- jsdgam(
#'   formula        = bf(y ~ series * env, p ~ series + tod_c),
#'   factor_formula = ~ -1,
#'   data           = simdat$data_train,
#'   family         = occ(),
#'   n_lv           = 2L,
#'   chains         = 2,
#'   silent         = 2
#' )
#'
#' # The species-by-species covariance the two factors induce
#' sv <- shared_variation(mod)
#' sv
#' plot(sv)
#'
#' # Next: residual_cor() rescales Delta to correlations
#' }
#'
#' @author Nicholas J Clark
#' @family latent dynamics
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
  rlang::check_dots_empty()

  n_lv <- require_factor_n_lv(object, "shared_variation()")

  series_names <- fitted_series_levels(object)
  # The per-draw `Z Sigma Z'`, shared with `residual_cor()`
  delta_draws <- factor_implied_cov_draws(
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
  cat("Factor-implied shared variation matrix (Delta = Z Sigma Z')\n")
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
  cat("\nplot() draws the full matrix as a heatmap.\n")
  invisible(x)
}


#' Plot the factor-implied shared variation matrix
#'
#' Heatmap of the posterior point estimate of
#' \eqn{\Delta = Z \Sigma Z^\top}. Diagonal cells show the variance
#' the factors capture in each series. Off-diagonal cells show the
#' covariance between series, positive or negative. The fill scale is
#' symmetric around zero. A positive and a negative covariance of equal
#' size take colours of equal intensity.
#'
#' @param x A `mvgam_shared_variation` object returned by
#'   [shared_variation()].
#' @param type One of `"point"` (default) showing the posterior
#'   point estimate, or `"uncertainty"` showing the per-entry
#'   posterior spread (`delta_se`).
#' @param ... Unused. Anything passed here is refused.
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
  rlang::check_dots_empty()
  mat <- switch(type, point = x$delta, uncertainty = x$delta_se)
  rng <- max(abs(range(mat, na.rm = TRUE, finite = TRUE)))
  fill_scale <- if (type == "point") {
    mvgam_diverging_scale(name = "Posterior\nDelta", limits = c(-rng, rng))
  } else {
    ggplot2::scale_fill_gradient(
      name = "Posterior\nspread",
      low = "grey95", high = mvgam_palette("red")[5L],
      limits = c(0, rng), na.value = "grey30"
    )
  }
  matrix_heatmap(
    matrix_panel(mat),
    fill_scale
  )
}
