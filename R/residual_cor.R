#' Extract residual correlations from a fitted mvgam model
#'
#' Compute residual (cross-series) correlation, covariance and partial
#' correlation summaries from a fitted [mvgam][mvgam::mvgam] model. The
#' three supported sources for the per-series covariance are:
#'
#' \itemize{
#'   \item Latent-factor trends (`n_lv` on the trend constructor). The
#'     implied per-series covariance is `Sigma = Z Z^T` per draw,
#'     using the posterior loadings `Z` for the default sampled-Z
#'     factor model, or the user-supplied loadings broadcast across
#'     draws when `trend_map = ...` fixes Z (see [mvgam()] /
#'     [AR()] / [VAR()] / [RW()] / [ZMVN()]).
#'   \item Correlated process-error trends (`cor = TRUE` on `RW()` /
#'     `AR()` / `ZMVN()`, or any `VAR()` / `VARMA()` trend).
#'   \item Hierarchical correlation structures (grouping variable on
#'     the trend via `gr = ...`).
#' }
#'
#' @param object A fitted [mvgam][mvgam::mvgam] object.
#' @param groups Logical. Only relevant for hierarchical trends
#'   (`gr = ...` supplied to the trend constructor). When `FALSE`
#'   (default) the population-level correlation
#'   (`tcrossprod(L_Omega_global)`) is returned. When `TRUE`, a named
#'   list of `mvgam_residcor` objects is returned, one per group; each
#'   group's correlation is reconstructed as
#'   `alpha * tcrossprod(L_global) + (1 - alpha) * tcrossprod(L_dev[g])`.
#' @param partial Logical. When `TRUE`, also compute the residual
#'   partial correlation matrix per draw via the inverse-correlation
#'   identity `P_ij = -inv(R)_ij / sqrt(inv(R)_ii * inv(R)_jj)`. This
#'   is `O(p^3)` per draw and is omitted by default. Required if you
#'   want the `prec*` fields populated.
#' @param summary Logical. When `TRUE` (default) per-element summaries
#'   (point estimate, posterior SD, quantile CI, posterior probability
#'   mass) are returned. When `FALSE` the full per-draw arrays are
#'   returned for downstream analysis.
#' @param robust Logical. When `FALSE` (default), the posterior mean
#'   is the point estimate; when `TRUE`, the posterior median is used.
#'   Only relevant when `summary = TRUE`.
#' @param probs Length-2 numeric vector giving the lower and upper
#'   probabilities for the quantile-based credible interval. Default
#'   `c(0.025, 0.975)`.
#' @param ... Currently ignored.
#'
#' @return An [mvgam_residcor-class] object when `summary = TRUE` and
#'   `groups = FALSE`. When `groups = TRUE` on a hierarchical trend, a
#'   named list of `mvgam_residcor` objects (one per group plus the
#'   population-level matrix under the name `"_global"`). When
#'   `summary = FALSE`, a list of per-draw arrays.
#'
#' @details
#' For non-hierarchical correlated trends the per-draw correlation
#' matrix is reconstructed from the trend's posterior covariance:
#' \itemize{
#'   \item Cholesky-scaled trends (`RW(cor = TRUE)`, `AR(cor = TRUE)`,
#'     `ZMVN(cor = TRUE)`) reconstruct
#'     `Sigma = diag(sigma) %*% L_Omega %*% t(L_Omega) %*% diag(sigma)`
#'     per draw.
#'   \item Full-covariance trends (`VAR`, `VARMA`) use `Sigma_trend`
#'     directly.
#' }
#' For hierarchical trends, per-group correlations combine the global
#' Cholesky factor and per-group deviations via
#' `alpha * tcrossprod(L_global) + (1 - alpha) * tcrossprod(L_dev[g])`.
#'
#' Credible intervals and posterior SDs for correlations are computed
#' on the Fisher z-transformed scale (`atanh(r)`) and back-transformed
#' for display, which keeps the intervals well-behaved for
#' correlations near `+/- 1`. Posterior probabilities
#' (`prob_positive`, `prob_negative`) are computed on the native
#' scale. `prob_nonzero = pmax(prob_positive, prob_negative)`
#' summarises one-sided evidence; `sig_cor` is the correlation matrix
#' with elements set to zero where `prob_nonzero <= 0.95` (the
#' threshold is exposed via the returned object's
#' `prob_threshold` metadata field).
#'
#' Joint species distribution models (`jsdgam` fits) use a different
#' latent-factor parameterisation; this function will be wired up to
#' that case when the jsdgam port lands.
#'
#' @seealso [mvgam_residcor-class], [summary.mvgam_residcor()],
#'   [print.mvgam_residcor()].
#'
#' @references Hui, F. K. C. (2016). boral - Bayesian Ordination and
#'   Regression Analysis of Multivariate Abundance Data in R.
#'   \emph{Methods in Ecology and Evolution}, 7(6), 744-750.
#'   \doi{10.1111/2041-210X.12514}
#'
#' @export
residual_cor <- function(object, ...) {
  UseMethod("residual_cor", object)
}


#' @rdname residual_cor
#' @method residual_cor mvgam
#' @export
residual_cor.mvgam <- function(object,
                               groups = FALSE,
                               partial = FALSE,
                               summary = TRUE,
                               robust = FALSE,
                               probs = c(0.025, 0.975),
                               ...) {
  checkmate::assert_class(object, "mvgam")
  checkmate::assert_flag(groups)
  checkmate::assert_flag(partial)
  checkmate::assert_flag(summary)
  checkmate::assert_flag(robust)
  checkmate::assert_numeric(probs, len = 2L, lower = 0, upper = 1,
                            any.missing = FALSE, unique = TRUE)
  probs <- sort(probs)

  compute_residual_cor(
    object = object,
    groups = groups,
    partial = partial,
    summary = summary,
    robust = robust,
    probs = probs
  )
}


#' @rdname residual_cor
#' @method residual_cor jsdgam
#' @export
residual_cor.jsdgam <- function(object, ...) {
  stop(insight::format_error(c(
    "residual_cor() for jsdgam fits is not yet implemented.",
    i = paste0(
      "The jsdgam fitting entry point is being ported; this method ",
      "will be wired up once that lands."
    )
  )))
}


# ---------------------------------------------------------------------------
# Internal: reconstruct per-draw correlation matrices and summarise.
# ---------------------------------------------------------------------------

#' Compute residual correlation, covariance and partial-correlation
#' draws from an mvgam fit and (optionally) summarise them.
#'
#' Dispatches in priority order:
#'   1. Factor model (`n_lv > 0` on the trend spec): `Sigma = Z Z^T`
#'      per draw from the posterior loadings matrix `Z`.
#'   2. `get_trend_covariance_structure()$pattern`:
#'      - `"full_covariance"` (VAR, VARMA): `Sigma_trend[d, , ]` direct.
#'      - `"cholesky_scaled"` with correlations: reconstruct as
#'        `diag(sigma) %*% L_Omega %*% t(L_Omega) %*% diag(sigma)`.
#'      - `"hierarchical_cholesky"`: population-level `tcrossprod(L_global)`
#'        by default, per-group mixed when `groups = TRUE`.
#'
#' All branches end at the same `finalise_residcor()` summariser to
#' keep the cor / cov / partial / probability machinery in one place.
#'
#' @noRd
compute_residual_cor <- function(object, groups, partial, summary,
                                  robust, probs) {
  # Design note: factor covariance (n_lv > 0) is checked before the
  # trend-pattern dispatch because latent factors induce their own
  # Sigma = Z Z^T regardless of whether the trend itself emits a
  # cross-series covariance. Adding a future "factor-with-trend"
  # interaction would require either extending detect_factor_n_lv()
  # or surfacing a "factor_loadings" pattern from
  # get_trend_covariance_structure() and unifying the dispatch.
  n_lv <- detect_factor_n_lv(object)
  if (!is.null(n_lv)) {
    series_names <- resolve_series_info(object)$series_levels
    cov_draws <- extract_cov_draws_factor(object, n_lv,
                                          length(series_names))
    return(finalise_residcor(
      cov_draws = cov_draws,
      series_names = series_names,
      partial = partial,
      summary = summary,
      robust = robust,
      probs = probs,
      pattern = "factor_loadings",
      hierarchical = FALSE,
      group_label = NULL
    ))
  }

  cov_struct <- get_trend_covariance_structure(object)
  series_names <- get_residcor_series_names(object, cov_struct)

  if (cov_struct$pattern == "none") {
    stop(insight::format_error(c(
      "Cannot compute residual correlations: trend has no covariance.",
      x = "The fitted trend type does not emit a process-error matrix.",
      i = paste0(
        "Use a trend that supports correlated innovations, e.g. ",
        "VAR(), VARMA(), RW(cor = TRUE) / AR(cor = TRUE) / ",
        "ZMVN(cor = TRUE), or any trend with n_lv > 0."
      )
    )))
  }

  if (cov_struct$pattern == "diagonal" ||
      (cov_struct$pattern == "cholesky_scaled" &&
       !cov_struct$has_correlations)) {
    stop(insight::format_error(c(
      "Cannot compute residual correlations: trend innovations are independent.",
      x = "The fitted trend has no cross-series covariance structure.",
      i = paste0(
        "Refit with cor = TRUE on the trend constructor (e.g. ",
        "RW(cor = TRUE), AR(cor = TRUE), ZMVN(cor = TRUE)), use ",
        "VAR() / VARMA(), or add n_lv = k for a latent-factor fit."
      )
    )))
  }

  # Hierarchical path: optionally return per-group list.
  if (isTRUE(cov_struct$hierarchical)) {
    return(compute_residcor_hierarchical(
      object = object,
      cov_struct = cov_struct,
      groups = groups,
      partial = partial,
      summary = summary,
      robust = robust,
      probs = probs,
      series_names = series_names
    ))
  }

  cov_draws <- extract_cov_draws_flat(cov_struct)
  finalise_residcor(
    cov_draws = cov_draws,
    series_names = series_names$global,
    partial = partial,
    summary = summary,
    robust = robust,
    probs = probs,
    pattern = cov_struct$pattern,
    hierarchical = FALSE,
    group_label = NULL
  )
}


#' Detect the number of latent factors on a fitted mvgam trend.
#'
#' Returns `NULL` if the trend is not a factor model. For multivariate
#' trend specs (one per response in an mvbind fit) the first spec is
#' used; factor models with per-response `n_lv` heterogeneity are
#' currently out of scope.
#'
#' @noRd
detect_factor_n_lv <- function(object) {
  spec <- trend_spec_for_residcor(object)
  if (is.null(spec)) return(NULL)
  n_lv <- spec$n_lv
  if (is.null(n_lv) || !is.numeric(n_lv) || n_lv < 1L) {
    return(NULL)
  }
  as.integer(n_lv)
}


#' Per-draw factor-implied covariance `Sigma = Z Z^T`.
#'
#' Delegates to `resolve_factor_loadings()` so the sampled-vs-
#' fixed Z decision lives in one place. Returns the implied
#' `[ndraws, n_series, n_series]` covariance array.
#'
#' @noRd
extract_cov_draws_factor <- function(object, n_lv, n_series) {
  Z_arr <- resolve_factor_loadings(
    object = object, n_lv = n_lv, n_series = n_series
  )
  ndraws <- dim(Z_arr)[1L]
  cov_draws <- array(0, dim = c(ndraws, n_series, n_series))
  for (d in seq_len(ndraws)) {
    cov_draws[d, , ] <- tcrossprod(Z_arr[d, , ])
  }
  cov_draws
}




#' Per-draw covariance array from a non-hierarchical trend.
#'
#' Returns a `[ndraws, p, p]` numeric array of posterior covariance
#' matrices reconstructed from either the cholesky-scaled or
#' full-covariance pattern.
#'
#' @noRd
extract_cov_draws_flat <- function(cov_struct) {
  pattern <- cov_struct$pattern
  ndraws <- cov_struct$ndraws
  p <- cov_struct$n_series
  params <- cov_struct$params

  out <- array(0, dim = c(ndraws, p, p))

  if (pattern == "full_covariance") {
    # `Sigma_trend` is already a [ndraws, p, p] array.
    Sigma_arr <- params$Sigma_trend
    checkmate::assert_array(Sigma_arr, d = 3L, any.missing = FALSE)
    if (!identical(dim(Sigma_arr), as.integer(c(ndraws, p, p)))) {
      stop(insight::format_error(c(
        "Internal: 'Sigma_trend' dimensions do not match.",
        x = paste0(
          "Got ", paste(dim(Sigma_arr), collapse = "x"),
          "; expected ", ndraws, "x", p, "x", p, "."
        )
      )))
    }
    return(Sigma_arr)
  }

  if (pattern == "cholesky_scaled") {
    # L_Omega_trend is [ndraws, p, p]; sigma_trend is [ndraws, p].
    L_arr <- params$L_Omega_trend
    sigma_mat <- params$sigma_trend
    checkmate::assert_array(L_arr, d = 3L, any.missing = FALSE)
    checkmate::assert_matrix(sigma_mat, any.missing = FALSE)
    for (d in seq_len(ndraws)) {
      L <- L_arr[d, , ]
      s <- sigma_mat[d, ]
      # Sigma = diag(s) %*% L %*% t(L) %*% diag(s)
      L_scaled <- L * s
      out[d, , ] <- tcrossprod(L_scaled)
    }
    return(out)
  }

  stop(insight::format_error(c(
    paste0("Unsupported covariance pattern for residual_cor: '",
           pattern, "'."),
    i = "Supported patterns: full_covariance, cholesky_scaled."
  )))
}


#' Hierarchical-trend residual correlation summary.
#'
#' When `groups = FALSE` (default) returns the population-level
#' `tcrossprod(L_global)` summary. When `groups = TRUE` returns a
#' named list with element `"_global"` plus one per group, each
#' summarised independently.
#'
#' @noRd
compute_residcor_hierarchical <- function(object, cov_struct, groups,
                                          partial, summary, robust,
                                          probs, series_names) {
  ndraws <- cov_struct$ndraws
  n_sub <- as.integer(cov_struct$group_info$n_subgroups)
  n_groups <- as.integer(cov_struct$group_info$n_groups)
  # Prefer real factor levels resolved upstream from object$data via
  # the trend spec's `gr` variable; fall back to generic indices if
  # the resolver could not recover them.
  group_labels <- series_names$group_labels %||%
    cov_struct$group_info$group_labels %||%
    paste0("group_", seq_len(n_groups))
  alpha <- cov_struct$params$alpha_cor_trend
  L_glob_arr <- cov_struct$params$L_Omega_global_trend
  L_dev_arr <- cov_struct$params$L_deviation_group_trend

  # Population-level correlation matrix per draw.
  glob_cov <- array(0, dim = c(ndraws, n_sub, n_sub))
  for (d in seq_len(ndraws)) {
    glob_cov[d, , ] <- tcrossprod(L_glob_arr[d, , ])
  }
  global_names <- series_names$global
  global_out <- finalise_residcor(
    cov_draws = glob_cov,
    series_names = global_names,
    partial = partial,
    summary = summary,
    robust = robust,
    probs = probs,
    pattern = "hierarchical_cholesky",
    hierarchical = TRUE,
    group_label = "_global"
  )

  if (!groups) return(global_out)

  per_group <- vector("list", length = n_groups)
  names(per_group) <- group_labels
  for (g in seq_len(n_groups)) {
    grp_cov <- array(0, dim = c(ndraws, n_sub, n_sub))
    for (d in seq_len(ndraws)) {
      glob_d <- tcrossprod(L_glob_arr[d, , ])
      dev_d <- tcrossprod(L_dev_arr[d, g, , ])
      grp_cov[d, , ] <- alpha[d] * glob_d + (1 - alpha[d]) * dev_d
    }
    # Each group's per-series labels reuse the subgroup names since
    # subgroup defines the cor matrix dimension; group identity is
    # carried by the entry name in the returned list.
    grp_names <- global_names
    per_group[[g]] <- finalise_residcor(
      cov_draws = grp_cov,
      series_names = grp_names,
      partial = partial,
      summary = summary,
      robust = robust,
      probs = probs,
      pattern = "hierarchical_cholesky",
      hierarchical = TRUE,
      group_label = group_labels[g]
    )
  }

  c(list("_global" = global_out), per_group)
}


#' Convert per-draw covariance arrays into the public mvgam_residcor
#' return object, applying Fisher-z-based summaries for correlations.
#'
#' @noRd
finalise_residcor <- function(cov_draws, series_names, partial,
                              summary, robust, probs, pattern,
                              hierarchical, group_label) {
  checkmate::assert_array(cov_draws, d = 3L, any.missing = FALSE)
  ndraws <- dim(cov_draws)[1L]
  p <- dim(cov_draws)[2L]

  cor_draws <- array(0, dim = dim(cov_draws))
  for (d in seq_len(ndraws)) {
    cor_draws[d, , ] <- cov2cor(cov_draws[d, , ])
  }

  prec_draws <- NULL
  if (partial) {
    prec_draws <- array(0, dim = dim(cov_draws))
    for (d in seq_len(ndraws)) {
      prec_draws[d, , ] <- partial_cor_from_cor(cor_draws[d, , ])
    }
  }

  if (!summary) {
    out <- list(
      cor_draws = cor_draws,
      cov_draws = cov_draws,
      prec_draws = prec_draws,
      n_series = p,
      series_names = series_names,
      pattern = pattern,
      hierarchical = hierarchical,
      group_label = group_label
    )
    return(structure(out, class = "mvgam_residcor"))
  }

  cor_stats <- summarise_correlation_array(cor_draws, robust, probs,
                                            series_names)
  cov_stats <- summarise_unconstrained_array(cov_draws, robust, probs,
                                              series_names)

  out <- list(
    cor = cor_stats$point,
    cor_se = cor_stats$se,
    cor_lower = cor_stats$lower,
    cor_upper = cor_stats$upper,
    prob_positive = cor_stats$prob_positive,
    prob_negative = cor_stats$prob_negative,
    prob_nonzero = cor_stats$prob_nonzero,
    sig_cor = cor_stats$sig,
    cov = cov_stats$point,
    cov_se = cov_stats$se,
    cov_lower = cov_stats$lower,
    cov_upper = cov_stats$upper,
    mean_abs_offdiag = mean_abs_offdiag_summary(cor_draws, robust, probs),
    n_series = p,
    series_names = series_names,
    pattern = pattern,
    hierarchical = hierarchical,
    group_label = group_label,
    probs = probs,
    prob_threshold = 0.95
  )

  if (partial) {
    prec_stats <- summarise_unconstrained_array(prec_draws, robust, probs,
                                                 series_names)
    out$prec <- prec_stats$point
    out$prec_se <- prec_stats$se
    out$prec_lower <- prec_stats$lower
    out$prec_upper <- prec_stats$upper
  }

  structure(out, class = "mvgam_residcor")
}


#' Partial correlation matrix from a correlation matrix
#'
#' Standard inverse-correlation identity:
#' `P_ij = -inv(R)_ij / sqrt(inv(R)_ii * inv(R)_jj)` with
#' `diag(P) = 1`. Requires `R` to be positive definite.
#'
#' @noRd
partial_cor_from_cor <- function(R) {
  inv_R <- solve(R)
  d <- sqrt(diag(inv_R))
  P <- -inv_R / outer(d, d)
  diag(P) <- 1
  P
}


#' Per-element summary of a correlation array via Fisher z.
#'
#' For each `(i, j)` element of the `[ndraws, p, p]` correlation
#' array, compute point estimate, posterior SD, and quantile CIs on
#' the Fisher-z scale and back-transform to the correlation scale.
#' Off-scale draws (`r == 1`) are clipped before `atanh` to avoid
#' infinities.
#'
#' @noRd
summarise_correlation_array <- function(arr, robust, probs, series_names) {
  p <- dim(arr)[2L]
  point <- matrix(0, p, p)
  se    <- matrix(0, p, p)
  lower <- matrix(0, p, p)
  upper <- matrix(0, p, p)
  prob_pos <- matrix(0, p, p)
  prob_neg <- matrix(0, p, p)

  clip <- function(r) {
    # eps = 1e-7 keeps atanh finite (atanh(±1) = ±Inf) while staying
    # within roundoff-magnitude distance of the true correlation
    # boundary, so the Fisher-z back-transform recovers values
    # indistinguishable from r at standard print precision.
    eps <- 1e-7
    pmin(pmax(r, -1 + eps), 1 - eps)
  }

  for (i in seq_len(p)) {
    for (j in seq_len(p)) {
      r <- arr[, i, j]
      z <- atanh(clip(r))
      z_centre <- if (robust) median(z) else mean(z)
      z_q <- stats::quantile(z, probs = probs, na.rm = TRUE)
      point[i, j] <- tanh(z_centre)
      se[i, j]    <- tanh(sd(z))
      lower[i, j] <- tanh(z_q[1L])
      upper[i, j] <- tanh(z_q[2L])
      prob_pos[i, j] <- mean(r > 0)
      prob_neg[i, j] <- mean(r < 0)
    }
  }

  # Correlation matrix diagonal is definitionally 1; the Fisher-z
  # clip + back-transform introduces a tiny bias on the diagonal that
  # we hard-restore here so output is exact for diag and SE / CI bounds
  # collapse cleanly.
  diag_idx <- cbind(seq_len(p), seq_len(p))
  point[diag_idx] <- 1
  se[diag_idx]    <- 0
  lower[diag_idx] <- 1
  upper[diag_idx] <- 1

  rownames(point) <- colnames(point) <- series_names
  rownames(se) <- colnames(se) <- series_names
  rownames(lower) <- colnames(lower) <- series_names
  rownames(upper) <- colnames(upper) <- series_names
  rownames(prob_pos) <- colnames(prob_pos) <- series_names
  rownames(prob_neg) <- colnames(prob_neg) <- series_names

  diag(prob_pos) <- 1
  diag(prob_neg) <- 0

  prob_nz <- pmax(prob_pos, prob_neg)
  sig <- point
  sig[prob_nz <= 0.95] <- 0
  rownames(sig) <- colnames(sig) <- series_names
  rownames(prob_nz) <- colnames(prob_nz) <- series_names

  list(
    point = point, se = se, lower = lower, upper = upper,
    prob_positive = prob_pos, prob_negative = prob_neg,
    prob_nonzero = prob_nz, sig = sig
  )
}


#' Per-element summary of an unconstrained array (covariance,
#' partial correlation). Uses native-scale quantiles and SD.
#'
#' @noRd
summarise_unconstrained_array <- function(arr, robust, probs,
                                           series_names) {
  p <- dim(arr)[2L]
  point <- matrix(0, p, p)
  se    <- matrix(0, p, p)
  lower <- matrix(0, p, p)
  upper <- matrix(0, p, p)

  for (i in seq_len(p)) {
    for (j in seq_len(p)) {
      x <- arr[, i, j]
      point[i, j] <- if (robust) median(x) else mean(x)
      se[i, j]    <- stats::sd(x)
      qs <- stats::quantile(x, probs = probs, na.rm = TRUE)
      lower[i, j] <- qs[1L]
      upper[i, j] <- qs[2L]
    }
  }

  rownames(point) <- colnames(point) <- series_names
  rownames(se) <- colnames(se) <- series_names
  rownames(lower) <- colnames(lower) <- series_names
  rownames(upper) <- colnames(upper) <- series_names

  list(point = point, se = se, lower = lower, upper = upper)
}


#' Scalar summary of off-diagonal correlation magnitude.
#'
#' Returns a list with the point estimate and quantile CI of
#' `mean(|r_ij|)` over `i != j` per draw, interpretable as the
#' overall residual co-variation magnitude.
#'
#' @noRd
mean_abs_offdiag_summary <- function(cor_draws, robust, probs) {
  ndraws <- dim(cor_draws)[1L]
  p <- dim(cor_draws)[2L]
  if (p <= 1L) {
    return(list(point = NA_real_, lower = NA_real_, upper = NA_real_))
  }
  off_diag_mask <- !diag(TRUE, nrow = p)
  per_draw <- vapply(seq_len(ndraws), function(d) {
    mean(abs(cor_draws[d, , ][off_diag_mask]))
  }, numeric(1L))
  point <- if (robust) median(per_draw) else mean(per_draw)
  qs <- stats::quantile(per_draw, probs = probs, na.rm = TRUE)
  list(point = point, lower = qs[[1L]], upper = qs[[2L]])
}


#' Resolve series names for labelling correlation matrix rows / cols.
#'
#' Returns a list with `global` (character vector of length p for
#' non-hierarchical or global-level) and `group_labels` (character
#' vector of length n_groups, hierarchical only). Non-hierarchical
#' names come from `resolve_series_info()`, the canonical
#' series-identity resolver used by forecast / hindcast. Hierarchical
#' labels come from the trend spec's `gr` / `subgr` variable names
#' looked up in `object$data`; falls back to `group_<i>` /
#' `subgroup_<i>` only when the factor levels cannot be recovered.
#'
#' @noRd
get_residcor_series_names <- function(object, cov_struct) {
  fallback <- paste0("series_", seq_len(cov_struct$n_series))

  if (!isTRUE(cov_struct$hierarchical)) {
    nm <- resolve_series_info(object)$series_levels %||% fallback
    if (length(nm) != cov_struct$n_series) nm <- fallback
    return(list(global = nm))
  }

  group_info <- cov_struct$group_info
  spec <- trend_spec_for_residcor(object)
  data <- object$data

  group_labels <- lookup_factor_levels(data, spec$gr,
                                       group_info$n_groups,
                                       prefix = "group")
  sub_labels <- lookup_factor_levels(data, spec$subgr,
                                     group_info$n_subgroups,
                                     prefix = "subgroup")

  list(global = sub_labels, group_labels = group_labels)
}


#' Return the active trend spec for an mvgam fit (multivariate
#' fits use the first spec — per-response hierarchical heterogeneity
#' is out of scope for residual_cor).
#'
#' @noRd
trend_spec_for_residcor <- function(object) {
  ts <- object$mv_spec$trend_specs
  if (inherits(ts, "mvgam_trend")) ts else ts[[1L]]
}


#' Look up `levels(data[[var_name]])` with a fallback if the column
#' is missing or the level count differs from the expected size.
#' Variable name comes from `as.character()` of the trend spec slot.
#'
#' @noRd
lookup_factor_levels <- function(data, var_name, expected_n, prefix) {
  fallback <- paste0(prefix, "_", seq_len(expected_n))
  if (is.null(data) || is.null(var_name)) return(fallback)
  nm <- as.character(var_name)
  if (length(nm) != 1L || is.na(nm) || nm == "NA" ||
      !nm %in% names(data)) {
    return(fallback)
  }
  lvls <- levels(as.factor(data[[nm]]))
  if (length(lvls) != expected_n) fallback else lvls
}


# ---------------------------------------------------------------------------
# S3 methods on mvgam_residcor
# ---------------------------------------------------------------------------

#' Tidy summary of an `mvgam_residcor` object
#'
#' Returns a tibble with one row per unique upper-triangle pair of
#' series, sorted by `prob_nonzero` descending (so the most credibly
#' non-zero pairs appear first).
#'
#' @param object An `mvgam_residcor` object returned by
#'   [residual_cor()] with `summary = TRUE`.
#' @param ... Currently ignored.
#'
#' @return A `tibble::tibble` with columns `series_1`, `series_2`,
#'   `Estimate`, `Est.Error`, `Q_lower`, `Q_upper`, `prob_positive`,
#'   `prob_negative`, `prob_nonzero`, `sig` (whether
#'   `prob_nonzero > prob_threshold`).
#'
#' @method summary mvgam_residcor
#' @export
summary.mvgam_residcor <- function(object, ...) {
  checkmate::assert_class(object, "mvgam_residcor")

  # Use [[ for exact name lookup; $ does partial matching and would
  # silently return cor_draws when cor is absent (summary = FALSE).
  cor_mat <- object[["cor"]]
  if (is.null(cor_mat)) {
    stop(insight::format_error(c(
      "Cannot summarise: object was built with summary = FALSE.",
      i = "Call residual_cor(..., summary = TRUE) to get a summary."
    )))
  }

  p <- object[["n_series"]]
  series_names <- object[["series_names"]] %||%
    rownames(cor_mat) %||%
    paste0("series_", seq_len(p))
  if (is.list(series_names)) series_names <- series_names$global

  if (p < 2L) {
    return(tibble::tibble(
      series_1 = character(0), series_2 = character(0),
      Estimate = numeric(0), Est.Error = numeric(0),
      Q_lower = numeric(0), Q_upper = numeric(0),
      prob_positive = numeric(0), prob_negative = numeric(0),
      prob_nonzero = numeric(0), sig = logical(0)
    ))
  }

  pairs <- which(upper.tri(cor_mat), arr.ind = TRUE)
  rows <- pairs[, 1L]
  cols <- pairs[, 2L]
  threshold <- object[["prob_threshold"]] %||% 0.95

  out <- tibble::tibble(
    series_1 = series_names[rows],
    series_2 = series_names[cols],
    Estimate = cor_mat[pairs],
    Est.Error = object[["cor_se"]][pairs],
    Q_lower = object[["cor_lower"]][pairs],
    Q_upper = object[["cor_upper"]][pairs],
    prob_positive = object[["prob_positive"]][pairs],
    prob_negative = object[["prob_negative"]][pairs],
    prob_nonzero = object[["prob_nonzero"]][pairs],
    sig = object[["prob_nonzero"]][pairs] > threshold
  )

  # Sort by prob_nonzero desc, then by |Estimate| desc as tiebreaker.
  ord <- order(-out$prob_nonzero, -abs(out$Estimate))
  out[ord, ]
}


#' Print a concise overview of an `mvgam_residcor` object
#'
#' @param x An `mvgam_residcor` object.
#' @param digits Integer; number of significant digits for printed
#'   numbers. Default `2`.
#' @param ... Currently ignored.
#'
#' @method print mvgam_residcor
#' @export
print.mvgam_residcor <- function(x, digits = 2L, ...) {
  checkmate::assert_class(x, "mvgam_residcor")
  checkmate::assert_int(digits, lower = 0L)

  cor_mat <- x[["cor"]]
  cat("Residual correlations from an mvgam fit\n")
  cat("  Pattern    : ", x[["pattern"]] %||% "unknown", "\n", sep = "")
  cat("  Series     : ", x[["n_series"]] %||% NA_integer_, "\n", sep = "")
  if (isTRUE(x[["hierarchical"]])) {
    cat("  Group      : ", x[["group_label"]] %||% "(unlabelled)",
        "\n", sep = "")
  }
  if (!is.null(x[["mean_abs_offdiag"]])) {
    mad_summary <- x[["mean_abs_offdiag"]]
    cat(sprintf(
      "  Mean |r_ij| (i != j): %s [%s, %s]\n",
      format(round(mad_summary$point, digits), nsmall = digits),
      format(round(mad_summary$lower, digits), nsmall = digits),
      format(round(mad_summary$upper, digits), nsmall = digits)
    ))
  }
  if (!is.null(cor_mat)) {
    threshold <- x[["prob_threshold"]] %||% 0.95
    prob_nz <- x[["prob_nonzero"]]
    n_sig <- sum(prob_nz > threshold & upper.tri(cor_mat))
    n_pairs <- sum(upper.tri(cor_mat))
    cat(sprintf(
      "  Pairs with prob_nonzero > %.2f: %d / %d\n",
      threshold, n_sig, n_pairs
    ))
  } else {
    cat("  (raw draws only; call with summary = TRUE for headlines)\n")
  }
  invisible(x)
}
