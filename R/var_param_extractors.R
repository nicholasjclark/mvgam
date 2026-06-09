#' Detect whether a fitted mvgam carries a VAR(1) latent trend
#'
#' Inspects `object$trend_components$types` (the canonical trend-type
#' surface populated at fit time) and returns the matching trend-type
#' string for VAR variants, or `NULL` otherwise. Used by
#' `irf.mvgam()`, `fevd.mvgam()`, and `stability.mvgam()` to gate
#' VAR-only downstream calculations behind a single shared check.
#'
#' @param object A fitted `mvgam` object.
#' @return Character string ("VAR", "VAR1", "VARcor", "VAR1cor") if
#'   the fit's first trend component is a VAR; `NULL` otherwise.
#' @noRd
detect_var_trend <- function(object) {
  trend_type <- object$trend_components$types[1L]
  if (is.null(trend_type)) {
    return(NULL)
  }
  if (!trend_type %in% c("VAR", "VAR1", "VARcor", "VAR1cor")) {
    return(NULL)
  }
  trend_type
}

#' Assert that a fitted mvgam is a VAR(1) and return the trend type
#'
#' Thin wrapper around `detect_var_trend()` that errors with a
#' consistent message when the gate fails. The `surface` argument
#' lets the caller name the function the user invoked
#' (`"irf()"`, `"fevd()"`, `"stability()"`) so the error points the
#' right place.
#'
#' @param object A fitted `mvgam` object.
#' @param surface Character; the user-facing function name to cite
#'   in the error message.
#' @return The trend-type string on success; stops on failure.
#' @noRd
assert_var_trend <- function(object, surface) {
  checkmate::assert_class(object, "mvgam")
  checkmate::assert_string(surface, min.chars = 1L)
  trend_type <- detect_var_trend(object)
  if (is.null(trend_type)) {
    stop(insight::format_error(c(
      paste0("'", surface, "' requires a VAR(1) latent trend."),
      x = paste0(
        "This fit's trend type is '",
        object$trend_components$types[1L] %||% "<none>",
        "'."
      ),
      i = paste0(
        "Refit with 'trend_formula = ~ VAR(p = 1)' to use ",
        "'", surface, "'."
      )
    )))
  }
  trend_type
}

#' Extract VAR(1) posterior draws of (A, Sigma) from a fitted mvgam
#'
#' Pulls the per-draw coefficient matrix `A_trend[1, , ]` and
#' innovation covariance `Sigma_trend` from `object$fit` and returns
#' them as 3-D arrays sized `[ndraws, K, K]`. The `[1]` index on
#' `A_trend` selects the (currently only) supported VAR lag (`p = 1`);
#' higher-lag VAR is rejected at the constructor.
#'
#' `K` is the number of latent series in the VAR. For both factor
#' and non-factor VARs the Stan-side dimension is `N_lv_trend` (for
#' a non-factor VAR over the raw series, the compiler sets
#' `N_lv_trend = N_series_trend`). We read this directly from the
#' standata cached on the fit so the value is always what the Stan
#' model actually compiled with.
#'
#' @param object A fitted `mvgam` object whose trend is a VAR(1).
#' @return A list with elements:
#'   * `A`: `[ndraws, K, K]` array of VAR coefficient draws.
#'   * `Sigma`: `[ndraws, K, K]` array of innovation-covariance
#'     draws.
#'   * `K`: integer, the VAR dimension.
#'   * `ndraws`: integer, the number of posterior draws.
#' @noRd
extract_var_posterior <- function(object) {
  checkmate::assert_class(object, "mvgam")
  draws_mat <- posterior::as_draws_matrix(object$fit)
  # Reads the raw `A_trend`. For factor VAR with Heaps QR
  # identification the generated-quantities block also emits
  # `A_trend_tilde = Q A_trend Q'`; callers that want the
  # QR-identified surface should read `A_trend_tilde` and rotate
  # `Sigma_trend` by the same Q.
  # `N_lv_trend` is the Stan-side VAR dimension for every code
  # path: factor VARs set it to the number of factors, non-factor
  # VARs set it equal to `N_series_trend`. Reading from standata
  # guarantees the value matches what the compiled model uses.
  K <- object$standata$N_lv_trend
  if (is.null(K)) {
    stop(insight::format_error(c(
      "Cannot determine VAR dimension from fit.",
      x = "'standata$N_lv_trend' is missing.",
      i = "Did the fit complete fully? Try refitting."
    )))
  }
  K <- as.integer(K)
  checkmate::assert_int(K, lower = 1L)

  ndraws <- nrow(draws_mat)
  all_cols <- colnames(draws_mat)

  # `A_trend` is declared as `array[size(A_trend)] matrix[K, K]` in
  # Stan, so posterior column names take the form `A_trend[lag,i,j]`.
  # Restrict to the single supported lag (lag = 1) up front.
  A <- array(0, c(ndraws, K, K))
  for (j in seq_len(K)) {
    for (i in seq_len(K)) {
      col_name <- sprintf("A_trend[1,%d,%d]", i, j)
      if (!col_name %in% all_cols) {
        stop(insight::format_error(c(
          paste0("Posterior parameter '", col_name, "' not found."),
          i = "Required for VAR(1) coefficient extraction."
        )))
      }
      A[, i, j] <- as.numeric(draws_mat[, col_name])
    }
  }

  Sigma <- extract_indexed_array_2d(
    draws_mat, "Sigma_trend", K, K,
    required_for = "Sigma_trend (VAR innovation covariance)"
  )

  list(A = A, Sigma = Sigma, K = K, ndraws = ndraws)
}
