# Sign-canonical orientation for latent-factor mvgam fits.
#
# Mutates the saved per-chain draws of `Z[i, k]` and
# `lv_trend[t, k]` so every saved draw has `Z[k, k] >= 0`. The
# `trend[t, s] = Z %*% lv_trend` product is invariant under
# simultaneous column-sign flip of Z and lv_trend, so likelihood
# / prior / saved `trend` draws are unchanged; only Z and
# lv_trend draws on disk become canonical.
#
# Factor models that sample Z freely save identified `Z_tilde`
# and `lv_trend_tilde` with a positive diagonal by construction
# via Stan's `qr_thin_R`. The sign-mode equivalence class is
# removed at the QR step, so this function is a no-op whenever
# `Z_tilde` columns are present in the posterior; it is kept
# active for partial-Z fits where the saved `Z` includes free
# entries that can still drift between sign modes.
#
# Skipped automatically when:
#   - the fit has no latent factors (detect_factor_n_lv returns
#     NULL),
#   - the loadings are fixed via `trend_map` (no sign-mode
#     exists when Z is data, not a parameter), or
#   - the posterior already carries `Z_tilde` columns (QR
#     identification handled at Stan level).
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
#' @references
#' Heaps, S. E. and Jermyn, I. H. (2024). Structured prior
#' distributions for the covariance matrix in latent factor
#' models. \emph{Statistics and Computing}, 34:143.
#' \doi{10.1007/s11222-024-10454-0}
#'
#' @noRd
sign_canonicalise_factors <- function(object) {
  checkmate::assert_class(object, "mvgam")
  n_lv <- detect_factor_n_lv(object)
  if (is.null(n_lv) || n_lv < 1L) return(object)
  # Skip canonicalisation when Z's sign is effectively constrained:
  #   * fixed_Z (user-pinned partial or full Z): every column touched
  #     by the constraint has its sign pinned by the user; a flip
  #     would corrupt the encoded structure.
  #   * has_by_lv (per-factor smooth fits): the cached
  #     `trend[t, s] = sum_k Z[s, k] * (lv_trend[t, k] + mu_factor[t, k])`
  #     uses `mu_factor` baked in by Stan (not stored as a flippable
  #     parameter). Flipping `(Z[, k], lv_trend[, k])` leaves the
  #     `Z @ lv_trend` term invariant but flips `Z @ mu_factor`,
  #     corrupting the cached trend used by
  #     `compose_by_lv_trend_linpred()`. The rotation invariance
  #     Heaps and Jermyn (2024) addresses is already neutralised by
  #     the per-factor smooth's own constraints (the AST detector +
  #     Stan emission auto-skip QR for has_by_lv too).
  if (!is.null(object$trend_metadata$fixed_Z) ||
      isTRUE(object$trend_metadata$has_by_lv)) {
    return(object)
  }

  stanfit <- object$fit
  if (is.null(stanfit) || !isS4(stanfit) || is.null(stanfit@sim)) {
    return(object)
  }
  samples_list <- stanfit@sim$samples
  if (is.null(samples_list) || length(samples_list) == 0L) {
    return(object)
  }

  # Sampled-Z factor fits save a positive-diagonal Z_tilde via
  # Stan's qr_thin_R; the sign-mode equivalence is already
  # resolved at the QR step, so the chain-level mutation below is
  # unnecessary. Detect Z_tilde columns once and skip if present.
  first_chain <- samples_list[[1L]]
  if (!is.null(first_chain) &&
      has_identified_loadings(names(first_chain))) {
    object$trend_metadata$sign_canonicalised <- TRUE
    return(object)
  }

  # The axis's own count. `series_info$n_series` is empty whenever
  # the frame names no series column, which is every hierarchical
  # and every response-keyed fit, and the fallback beside it is
  # empty on most of those too. The function then returned the
  # object untouched and the stored posterior kept its unrotated
  # loadings, with nothing said.
  n_series <- mvgam_axes(object)$series$n
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
    # or jsdgam-style Theta naming until those are reconciled) leave
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
