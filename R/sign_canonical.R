# Sign-canonical orientation for latent-factor mvgam fits.
#
# Mutates the saved per-chain draws of `Z[i, k]` and
# `lv_trend[t, k]` so every saved draw has `Z[k, k] >= 0`. The
# `trend[t, s] = Z %*% lv_trend` product is invariant under
# simultaneous column-sign flip of Z and lv_trend, so likelihood
# / prior / saved `trend` draws are unchanged; only Z and
# lv_trend draws on disk become canonical.
#
# This removes the `2^n_lv` sign-mode equivalence class that the
# lower-triangular Z constraint alone leaves identifiable up to.
# Without the fix, chains can drift between sign-flipped modes
# within a single run, inflating Rhat / ESS, polluting trace
# plots, and pulling posterior medians of Z artificially toward
# zero (the median of a bimodal symmetric posterior). With the
# fix, all of those diagnostics behave correctly.
#
# Skipped automatically when:
#   - the fit has no latent factors (detect_factor_n_lv returns
#     NULL), or
#   - the loadings are fixed via `trend_map` (no sign-mode
#     exists when Z is data, not a parameter).
#
# Called exactly once inside `create_mvgam_from_combined_fit()`
# right before the final `mvgam_object` is returned to the user.

#' Sign-canonicalise factor draws on a fitted mvgam.
#'
#' @param object A fitted `mvgam` object (or any `brmsfit`-shaped
#'   object with a `$fit` slot containing a `stanfit`).
#'
#' @return The same `object` with `@sim$samples` mutated in place
#'   and a `$trend_metadata$sign_canonicalised = TRUE` flag.
#'
#' @noRd
sign_canonicalise_factors <- function(object) {
  checkmate::assert_class(object, "mvgam")
  n_lv <- detect_factor_n_lv(object)
  if (is.null(n_lv) || n_lv < 1L) return(object)
  if (!is.null(object$trend_metadata$fixed_Z)) return(object)

  stanfit <- object$fit
  if (is.null(stanfit) || !isS4(stanfit) || is.null(stanfit@sim)) {
    return(object)
  }
  samples_list <- stanfit@sim$samples
  if (is.null(samples_list) || length(samples_list) == 0L) {
    return(object)
  }

  n_series <- object$series_info$n_series %||%
    object$trend_components$n_trends
  if (is.null(n_series) || n_series < 1L) return(object)

  for (chain in seq_along(samples_list)) {
    samples <- samples_list[[chain]]
    n_iter <- nrow(samples)
    if (is.null(n_iter) || n_iter == 0L) next

    flip_mask <- matrix(FALSE, nrow = n_iter, ncol = n_lv)
    diag_present <- logical(n_lv)
    for (j in seq_len(n_lv)) {
      key <- sprintf("Z[%d,%d]", j, j)
      if (key %in% names(samples)) {
        flip_mask[, j] <- samples[[key]] < 0
        diag_present[j] <- TRUE
      }
    }
    # If no diagonal Z columns are saved (shouldn't happen for
    # standard factor fits, but guards against pruned posteriors
    # or jsdgam-style Theta naming until those are unified) leave
    # this chain alone.
    if (!any(diag_present)) next

    for (j in seq_len(n_lv)) {
      if (!diag_present[j]) next
      flips <- flip_mask[, j]
      if (!any(flips)) next

      for (i in seq_len(n_series)) {
        key <- sprintf("Z[%d,%d]", i, j)
        if (key %in% names(samples)) {
          samples[[key]][flips] <- -samples[[key]][flips]
        }
      }

      lv_pattern <- sprintf("^lv_trend\\[\\d+,%d\\]$", j)
      lv_keys <- grep(lv_pattern, names(samples), value = TRUE)
      for (key in lv_keys) {
        samples[[key]][flips] <- -samples[[key]][flips]
      }
    }

    samples_list[[chain]] <- samples
  }
  stanfit@sim$samples <- samples_list
  object$fit <- stanfit
  object$trend_metadata$sign_canonicalised <- TRUE
  object
}
