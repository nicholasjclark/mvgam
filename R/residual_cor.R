#' Residual correlations between the series of a fitted model
#'
#' @description
#' Posterior correlations, covariances and optional partial correlations
#' between the series' latent trends. Residual here means the
#' co-variation of the trends left once the covariate effects are
#' accounted for. [residuals.mvgam()] returns the observation
#' residuals. Three kinds of trend estimate these correlations:
#' \itemize{
#'   \item A factor trend (`n_lv` on the trend constructor). Each draw
#'     gives the series covariance `Z Omega Z'`, with `Z` the loadings
#'     and `Omega` the covariance of the latent states. [mvn()] and
#'     [mvt()] add each series' residual variance to the diagonal:
#'     `Psi^2`, or `Psi^2 * nu / (nu - 2)` under [mvt()].
#'   \item A trend with correlated innovations: `RW()`, `AR()`, `CAR()`
#'     or `ZMVN()` with `cor = TRUE`, or `VAR()`.
#'   \item A hierarchical trend (`gr` on the trend constructor).
#' }
#'
#' @inheritParams predict.mvgam
#' @param probs Two probabilities for the lower and upper bounds of the
#'   credible interval. Default `c(0.025, 0.975)`, a 95% interval.
#' @param object A fitted `mvgam` object.
#' @param covariance Which covariance to summarise. `"stationary"` (the
#'   default) gives the covariance of the latent states over the whole
#'   series. `"innovation"` gives the covariance of one time step's
#'   innovations. The two coincide for a trend whose states keep no memory
#'   between times.
#' @param by_group Logical, for a hierarchical trend. `FALSE` (the
#'   default) returns the population correlation of the innovations,
#'   `tcrossprod(L_Omega_global)`, under either `covariance`. `TRUE`
#'   also returns each group's correlation,
#'   `alpha * tcrossprod(L_global) + (1 - alpha) * tcrossprod(L_dev[g])`.
#' @param partial Logical. `TRUE` also computes partial correlations,
#'   `P_ij = -inv(R)_ij / sqrt(inv(R)_ii * inv(R)_jj)`. A partial
#'   correlation measures the association of two series once every
#'   other series is held fixed. The result stores them in the fields
#'   whose names start with `prec`. A factor fit with fewer factors
#'   than series has a singular covariance of rank `n_lv` and is
#'   refused. [shared_variation()]
#'   reports the variation its series share. The residual variances of
#'   [mvn()] and [mvt()] make their covariance full rank.
#' @param summary Logical. `TRUE` (the default) summarises each entry:
#'   the point estimate, spread, credible interval and posterior sign
#'   probabilities. `FALSE` returns the arrays of draws.
#' @param ... Unused. Anything passed here is refused.
#'
#' @return An [mvgam_residcor-class] object. Under `by_group = TRUE` on a
#'   hierarchical trend, an `mvgam_residcor_list` of them: the population
#'   matrix as `"_global"` and one per group.
#'   [plot.mvgam_residcor_list()] draws them as faceted heatmaps.
#'
#' @details
#' A correlated-innovation trend gives each draw the covariance
#' `diag(sigma) L_Omega L_Omega' diag(sigma)`. A `VAR()` trend takes the
#' leading block of `Omega_trend`, the stationary covariance the Stan
#' model derives from `Phi_trend`, `Sigma_trend` and, under `ma = TRUE`,
#' `Theta_trend`.
#'
#' The stationary covariance describes the states over the whole series.
#' An `AR(cor = TRUE)` trend reports the stationary covariance of its
#' states, including any moving-average term. With one lag it is
#' `Sigma[a, b] / (1 - ar_a * ar_b)`. A `CAR()`
#' trend holds it at every time of an irregular grid. A draw whose
#' autoregression is explosive has no stationary covariance and keeps
#' its innovation covariance. A warning gives the count once per session.
#' `ZMVN()` keeps no memory between times and `RW()` has no stationary
#' distribution. Both report their innovation covariance. A hierarchical
#' `AR()` converts each group's scales to stationary scales in the same
#' way, before its global and group correlations are mixed.
#'
#' Under `covariance = "innovation"` a `VAR()` fit reports the
#' contemporaneous shocks. Its transition matrix spreads each shock
#' across series at every step. The two correlations can differ
#' substantially.
#'
#' Intervals and spreads of correlations are computed on the Fisher-z
#' scale, `atanh(r)`, then back-transformed. The intervals stay inside
#' `(-1, 1)`. The reported matrix summarises each entry
#' separately. It need not be positive semi-definite, most visibly when
#' a few factors drive many series. A computation that factorises the
#' matrix, such as [chol()], takes `summary = FALSE`, whose per-draw
#' matrices are valid correlation matrices. `prob_positive` and
#' `prob_negative` are posterior sign probabilities. `prob_nonzero` is
#' the larger of the two. `sig_cor` keeps the correlations whose
#' `prob_nonzero` exceeds `prob_threshold`, fixed at 0.95. Its other
#' entries are zero.
#'
#' [jsdgam()] fits the same factor model. Its species covariance follows
#' the factor rule above.
#'
#' @seealso [plot.mvgam_residcor()] draws the matrix as a heatmap.
#'   [summary.mvgam_residcor()] lists the pairs. [ordinate()] draws the
#'   loadings as a biplot. The example of [trend_constructors] takes
#'   the correlations of a `CAR(cor = TRUE)` trend. The
#'   `r mvgam_article("jsdgam", "joint species distribution article")`
#'   interprets species correlations from a factor model.
#'
#' @references Hui, F. K. C. (2016). boral - Bayesian Ordination and
#'   Regression Analysis of Multivariate Abundance Data in R.
#'   \emph{Methods in Ecology and Evolution}, 7(6), 744-750.
#'   \doi{10.1111/2041-210X.12514}
#'
#' @examples
#' \dontrun{
#' # Detections of 6 species whose occupancy shares two latent factors
#' simdat <- sim_closure_unit_data(family = occ(), n_species = 6L,
#'                                 n_sites = 80L, n_visits = 4L,
#'                                 n_lv = 2L, seed = 1L)
#' mod <- jsdgam(formula = bf(y ~ series * env, p ~ series + tod_c),
#'               factor_formula = ~ -1,
#'               data = simdat$data_train, family = occ(),
#'               n_lv = 2L, chains = 2, silent = 2)
#'
#' # Species correlations after the fixed effects
#' rc <- residual_cor(mod)
#' round(rc$cor, 2)
#' plot(rc)
#'
#' # Next: summary(rc) lists the pairs by posterior sign probability
#' }
#'
#' @family latent dynamics
#' @export
residual_cor <- function(object, ...) {
  UseMethod("residual_cor", object)
}


#' @rdname residual_cor
#' @method residual_cor mvgam
#' @export
residual_cor.mvgam <- function(object,
                               covariance = c("stationary", "innovation"),
                               by_group = FALSE,
                               partial = FALSE,
                               summary = TRUE,
                               robust = FALSE,
                               probs = c(0.025, 0.975),
                               ndraws = NULL,
                               draw_ids = NULL,
                               ...) {
  checkmate::assert_class(object, "mvgam")
  covariance <- match.arg(covariance)
  checkmate::assert_flag(by_group)
  checkmate::assert_flag(partial)
  checkmate::assert_flag(summary)
  checkmate::assert_flag(robust)
  checkmate::assert_numeric(probs, len = 2L, lower = 0, upper = 1,
                            any.missing = FALSE, unique = TRUE)
  validate_draw_selectors(ndraws, draw_ids)
  rlang::check_dots_empty()
  probs <- sort(probs)

  compute_residual_cor(
    object = object,
    covariance = covariance,
    by_group = by_group,
    partial = partial,
    summary = summary,
    robust = robust,
    probs = probs,
    draw_ids = resolve_draw_ids(object, ndraws, draw_ids)
  )
}


# ---------------------------------------------------------------------------
# Per-draw correlation matrices and their summaries
# ---------------------------------------------------------------------------

#' Residual correlation, covariance and partial-correlation draws
#'
#' Dispatches in priority order:
#'   1. Factor model (`n_lv > 0` on the trend spec): `Z Omega Z^T` per
#'      draw, from `factor_implied_cov_draws()`.
#'   2. `get_trend_covariance_structure()$pattern`:
#'      - `"full_covariance"` (VAR, VARMA): the covariance the
#'        structure carries as `Sigma_trend`.
#'      - `"cholesky_scaled"` with correlations:
#'        `diag(sigma) %*% L_Omega %*% t(L_Omega) %*% diag(sigma)`.
#'      - `"hierarchical_cholesky"`: population-level
#'        `tcrossprod(L_global)` by default, per-group mixed when
#'        `by_group = TRUE`.
#'
#' Every branch ends at `finalise_residcor()`, which computes the
#' correlation, covariance, partial-correlation and probability
#' summaries.
#'
#' @noRd
compute_residual_cor <- function(object, covariance, by_group, partial,
                                  summary, robust, probs,
                                  draw_ids = NULL) {
  stationary <- identical(covariance, "stationary")
  # The factor check comes first. Loadings that share each latent
  # column between series give a factor fit a cross-series covariance
  # with independent innovations too. The guard below refuses only a
  # trend without that sharing.
  n_lv <- detect_factor_n_lv(object)
  if (!is.null(n_lv)) {
    series_names <- fitted_series_levels(object)
    # A residual variance per series makes the covariance full rank
    residual <- family_has_component_scale(object$family)
    if (!residual) {
      refuse_partial_on_rank_deficient(partial, n_lv,
                                       length(series_names))
    }
    cov_draws <- factor_implied_cov_draws(object, n_lv,
                                          length(series_names),
                                          stationary = stationary,
                                          draw_ids = draw_ids,
                                          residual = residual)
    return(finalise_residcor(
      cov_draws = cov_draws,
      series_names = series_names,
      partial = partial,
      summary = summary,
      robust = robust,
      probs = probs,
      pattern = "factor_loadings",
      covariance = covariance,
      hierarchical = FALSE,
      group_label = NULL
    ))
  }

  cov_struct <- get_trend_covariance_structure(object,
                                               draw_ids = draw_ids,
                                               stationary = stationary)
  series_names <- get_residcor_series_names(object, cov_struct)

  if (effective_covariance_pattern(cov_struct$pattern,
                                   cov_struct$has_correlations) %in%
        c("none", "diagonal")) {
    stop(insight::format_error(c(
      "The fitted trend has no correlation between series.",
      i = paste0(
        "Correlated trends: 'VAR()', a constructor with 'cor = TRUE' and ",
        "a factor model with 'n_lv'."
      )
    )), call. = FALSE)
  }

  # Hierarchical path: optionally return per-group list.
  if (isTRUE(cov_struct$hierarchical)) {
    return(compute_residcor_hierarchical(
      object = object,
      cov_struct = cov_struct,
      covariance = covariance,
      by_group = by_group,
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
    covariance = covariance,
    hierarchical = FALSE,
    group_label = NULL
  )
}


#' The cross-series covariance a factor model induces, per draw
#'
#' `residual_cor()` and `shared_variation()` both call this function.
#' It takes the latent covariance from the extractor the non-factor
#' trends use, then projects it onto the series through the loadings.
#' `mvn()` and `mvt()` add a residual variance per series, which
#' `residual = TRUE` adds to the diagonal. The shared part alone is
#' rank-deficient whenever there are fewer factors than series.
#'
#' @param object A fitted `mvgam` with a factor trend.
#' @param n_lv Number of latent factors.
#' @param n_series Number of observed series.
#' @param stationary Passed to `get_trend_covariance_structure()`.
#' @param draw_ids Posterior draws to use, or `NULL` for every draw.
#'   The loadings come from the same draws as the latent covariance.
#' @param residual Logical. `TRUE` adds each series' residual variance
#'   for a family that estimates one.
#' @return `[ndraws, n_series, n_series]` covariance draws.
#' @noRd
factor_implied_cov_draws <- function(object, n_lv, n_series,
                                     stationary = TRUE, draw_ids = NULL,
                                     residual = FALSE) {
  checkmate::assert_flag(residual)
  parts <- factor_model_draws(object, n_lv, n_series,
                              stationary = stationary, draw_ids = draw_ids)
  out <- project_cov_through_loadings(parts$latent, parts$Z_arr)
  if (residual && family_has_component_scale(object$family)) {
    variance <- component_scale_draws(
      parts$draws_mat, n_series, object$family
    )$variance
    for (d in seq_len(dim(out)[1L])) {
      out[d, , ] <- out[d, , ] + diag(variance[d, ], nrow = n_series)
    }
  }
  out
}


#' The loadings and latent covariance of a factor model, per draw
#'
#' Both come from the same draws. The latent covariance is stated in
#' the basis of the loadings the model sampled.
#'
#' @inheritParams factor_implied_cov_draws
#' @return A list:
#'   \itemize{
#'     \item `latent`: `[ndraws, n_lv, n_lv]` latent covariance draws.
#'     \item `Z_arr`: `[ndraws, n_series, n_lv]` loadings draws.
#'     \item `draws_mat`: the posterior draws both came from.
#'   }
#' @noRd
factor_model_draws <- function(object, n_lv, n_series, stationary = TRUE,
                               draw_ids = NULL) {
  cov_struct <- get_trend_covariance_structure(object,
                                               draw_ids = draw_ids,
                                               stationary = stationary)
  list(
    latent = extract_cov_draws_flat(cov_struct),
    Z_arr = resolve_factor_loadings(
      object = object, draws_mat = cov_struct$draws_mat, n_lv = n_lv,
      n_series = n_series, basis = "model"
    ),
    draws_mat = cov_struct$draws_mat
  )
}


#' Refuse a partial correlation the covariance cannot supply
#'
#' A factor model writes each series' trend as a combination of `n_lv`
#' latent columns. The covariance it implies has rank `n_lv`. With
#' fewer factors than series that matrix is singular whatever the
#' data. A partial correlation needs its inverse. `solve()` reports
#' the failure as a LAPACK reciprocal condition number. The error
#' below names the rank instead.
#'
#' Conditioning on the other `n_series - 1` series then determines the
#' remaining one exactly. Every partial correlation would be `+/- 1`.
#' `shared_variation()` reports the variation the series share.
#'
#' @param partial Whether partial correlations were asked for.
#' @param n_lv Latent factors the fit carries.
#' @param n_series Series the loadings project onto.
#' @return `invisible(NULL)`, or an error.
#' @noRd
refuse_partial_on_rank_deficient <- function(partial, n_lv, n_series) {
  if (!isTRUE(partial) || n_lv >= n_series) {
    return(invisible(NULL))
  }
  stop(insight::format_error(c(
    "Partial correlations need a full-rank residual covariance.",
    x = paste0(
      "The fit projects ", n_series, " series onto ", n_lv,
      " latent factor", if (n_lv == 1L) "" else "s", "."
    ),
    x = paste0("The implied covariance has rank ", n_lv,
               " and is singular."),
    i = "Use 'shared_variation()' for the variation the factors share.",
    i = paste0("Refit with n_lv = ", n_series,
               " for partial correlations.")
  )), call. = FALSE)
}


#' Project a latent covariance onto the series through the loadings
#'
#' A factor model writes `trend[t, s] = Z[s, ] . lv[t, ]`. The
#' covariance it induces between series is `Z Omega Z'`, where
#' `Omega` is the covariance of the latent states. `Z Z'` alone drops
#' the scale of each latent column, which a model with fixed loadings
#' samples as `sigma_trend`. It also drops the stationary variance of
#' autoregressive factors.
#'
#' `Omega` is stated in the basis of the loadings the model sampled.
#' `Z` must be those loadings. The QR-identified `Z_tilde` is a
#' rotation of them. `Z Z'` is invariant to that rotation. The basis
#' matters only once `Omega` enters the product.
#'
#' @param latent `[ndraws, k, k]` latent covariance draws.
#' @param Z_arr `[ndraws, n_series, k]` loadings draws.
#' @return `[ndraws, n_series, n_series]` covariance draws.
#' @noRd
project_cov_through_loadings <- function(latent, Z_arr) {
  checkmate::assert_array(latent, d = 3L)
  checkmate::assert_array(Z_arr, d = 3L)
  ndraws <- dim(Z_arr)[1L]
  n_series <- dim(Z_arr)[2L]
  out <- array(0, dim = c(ndraws, n_series, n_series))
  for (d in seq_len(ndraws)) {
    Z <- matrix(Z_arr[d, , ], nrow = n_series)
    out[d, , ] <- Z %*% matrix(latent[d, , ], nrow = ncol(Z)) %*% t(Z)
  }
  out
}




#' Per-draw covariance array from a non-hierarchical trend.
#'
#' Returns a \[ndraws, p, p\] numeric array of posterior covariance
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
    return(assert_dims(Sigma_arr, "'Sigma_trend'", c(ndraws, p, p)))
  }

  if (identical(effective_covariance_pattern(
    pattern, cov_struct$has_correlations
  ), "diagonal")) {
    # Independent innovations sample no `L_Omega_trend`. The covariance
    # is the diagonal of squared scales. A factor fit still gains a
    # cross-series covariance from it, through loadings that share each
    # latent column between series.
    sigma_mat <- params$sigma_trend
    checkmate::assert_matrix(sigma_mat, any.missing = FALSE)
    for (d in seq_len(ndraws)) {
      out[d, , ] <- diag(as.numeric(sigma_mat[d, ])^2, nrow = p)
    }
    return(out)
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

  stop_mvgam_fault(
    paste0("Covariance pattern '", pattern, "' has no extractor."),
    "Extractors cover full_covariance, cholesky_scaled and diagonal."
  )
}


#' Hierarchical-trend residual correlation summary.
#'
#' When `by_group = FALSE` (default) returns the population-level
#' `tcrossprod(L_global)` summary. When `by_group = TRUE` returns a
#' named list with element `"_global"` plus one per group, each
#' summarised independently.
#'
#' @noRd
compute_residcor_hierarchical <- function(object, cov_struct, covariance,
                                          by_group, partial, summary,
                                          robust, probs, series_names) {
  ndraws <- cov_struct$ndraws
  n_sub <- as.integer(cov_struct$group_info$n_subgroups)
  n_groups <- as.integer(cov_struct$group_info$n_groups)
  group_labels <- series_names$group_labels
  L_glob_arr <- cov_struct$params$L_Omega_global_trend

  # Population-level correlation matrix per draw.
  glob_cov <- array(0, dim = c(ndraws, n_sub, n_sub))
  for (d in seq_len(ndraws)) {
    glob_cov[d, , ] <- tcrossprod(L_glob_arr[d, , ])
  }
  global_names <- series_names$global
  # The population correlation is a hyperparameter of the shocks. No
  # population transition matrix exists to lift it through. It is an
  # innovation correlation under either setting of `covariance`.
  global_out <- finalise_residcor(
    cov_draws = glob_cov,
    series_names = global_names,
    partial = partial,
    summary = summary,
    robust = robust,
    probs = probs,
    pattern = "hierarchical_cholesky",
    covariance = "innovation",
    hierarchical = TRUE,
    group_label = "_global"
  )

  if (!by_group) return(global_out)

  per_group <- vector("list", length = n_groups)
  names(per_group) <- group_labels
  for (g in seq_len(n_groups)) {
    # The group's own covariance per draw. The global entry above
    # stays the population correlation, a hyperparameter shared by
    # every group, which has no single autoregressive coefficient to
    # settle against.
    grp_cov <- array(0, dim = c(ndraws, n_sub, n_sub))
    for (d in seq_len(ndraws)) {
      grp_cov[d, , ] <- tcrossprod(
        group_trend_factor(cov_struct$params, d, g, n_sub)
      )
    }
    # The subgroups label each group's matrix. The list entry names the
    # group.
    per_group[[g]] <- finalise_residcor(
      cov_draws = grp_cov,
      series_names = global_names,
      partial = partial,
      summary = summary,
      robust = robust,
      probs = probs,
      pattern = "hierarchical_cholesky",
      covariance = covariance,
      hierarchical = TRUE,
      group_label = group_labels[g]
    )
  }

  structure(
    c(list("_global" = global_out), per_group),
    class = "mvgam_residcor_list"
  )
}


# The posterior mass on one sign above which a correlation enters
# `sig_cor`
residcor_prob_threshold <- 0.95


#' Convert per-draw covariance arrays into the public mvgam_residcor
#' return object, applying Fisher-z-based summaries for correlations.
#'
#' @noRd
finalise_residcor <- function(cov_draws, series_names, partial,
                              summary, robust, probs, pattern,
                              covariance, hierarchical, group_label) {
  checkmate::assert_array(cov_draws, d = 3L, any.missing = FALSE)
  checkmate::assert_choice(covariance, c("stationary", "innovation"))
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
      covariance = covariance,
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
    cor_ess = cor_stats$ess,
    prob_positive = cor_stats$prob_positive,
    prob_negative = cor_stats$prob_negative,
    prob_nonzero = cor_stats$prob_nonzero,
    sig_cor = cor_stats$sig,
    cov = cov_stats$point,
    cov_se = cov_stats$se,
    cov_lower = cov_stats$lower,
    cov_upper = cov_stats$upper,
    cov_ess = cov_stats$ess,
    mean_abs_offdiag = mean_abs_offdiag_summary(cor_draws, robust, probs),
    n_series = p,
    series_names = series_names,
    pattern = pattern,
    covariance = covariance,
    hierarchical = hierarchical,
    group_label = group_label,
    probs = probs,
    prob_threshold = residcor_prob_threshold
  )

  if (partial) {
    # Partial correlations are bounded like any correlation. They take
    # the Fisher-z summary of the correlations. That summary supplies
    # `sig_prec` for `plot(type = "precision")`.
    prec_stats <- summarise_correlation_array(prec_draws, robust, probs,
                                               series_names)
    out$prec <- prec_stats$point
    out$prec_se <- prec_stats$se
    out$prec_lower <- prec_stats$lower
    out$prec_upper <- prec_stats$upper
    out$prec_ess <- prec_stats$ess
    out$prec_prob_positive <- prec_stats$prob_positive
    out$prec_prob_negative <- prec_stats$prob_negative
    out$prec_prob_nonzero <- prec_stats$prob_nonzero
    out$sig_prec <- prec_stats$sig
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


#' Per-element summary of a correlation array on the Fisher-z scale
#'
#' Each cell is summarised on the Fisher-z scale and back-transformed
#' to the correlation scale. Draws at `|r| = 1` are clipped first. A
#' clip of `1e-7` keeps `atanh()` finite and the back-transform then
#' recovers `r` to print precision. The diagonal is 1 in every draw
#' and is set exactly, with zero SE.
#'
#' @inheritParams summarise_unconstrained_array
#' @return The list of `summarise_unconstrained_array()` plus the
#'   matrices of `sign_probabilities()` and `sig`, the point
#'   estimates with entries below `residcor_prob_threshold` set to 0
#' @noRd
summarise_correlation_array <- function(arr, robust, probs, series_names) {
  eps <- 1e-7
  z <- atanh(pmin(pmax(arr, -1 + eps), 1 - eps))
  stats <- summarise_unconstrained_array(z, robust, probs, series_names,
                                         back = tanh)
  diag(stats$point) <- 1
  diag(stats$se) <- 0
  diag(stats$lower) <- 1
  diag(stats$upper) <- 1
  diag(stats$ess) <- NA_real_
  signs <- sign_probabilities(arr, series_names)
  diag(signs$prob_positive) <- 1
  diag(signs$prob_negative) <- 0
  diag(signs$prob_nonzero) <- 1
  stats$sig <- stats$point
  stats$sig[signs$prob_nonzero <= residcor_prob_threshold] <- 0
  c(stats, signs)
}


#' The posterior mass above and below zero of every matrix entry
#'
#' @param arr `[ndraws, p, p]` array of draws
#' @param series_names Labels for both dimensions
#' @return List of `prob_positive`, `prob_negative` and `prob_nonzero`,
#'   the larger of the two, each a labelled `p` by `p` matrix
#' @noRd
sign_probabilities <- function(arr, series_names) {
  checkmate::assert_array(arr, d = 3L)
  prob_pos <- apply(arr > 0, c(2L, 3L), mean)
  prob_neg <- apply(arr < 0, c(2L, 3L), mean)
  name_series_matrices(list(
    prob_positive = prob_pos,
    prob_negative = prob_neg,
    prob_nonzero = pmax(prob_pos, prob_neg)
  ), series_names)
}


#' Per-element summary of an array of matrix draws
#'
#' Each cell takes the summary `mvgam_post_summary()` gives a column of
#' draws.
#'
#' @inheritParams mvgam_post_summary
#' @param arr `[ndraws, p, p]` array of draws
#' @param probs Length-2 interval probabilities
#' @param series_names Labels for both dimensions
#' @param back Function mapping the scale of `arr` to the reported
#'   scale. It is applied to the point estimate, spread and bounds.
#'   The ESS is that of `arr`
#' @return List of labelled `p` by `p` matrices `point`, `se`,
#'   `lower`, `upper` and `ess`. A constant cell has `NA` ESS
#' @noRd
summarise_unconstrained_array <- function(arr, robust, probs,
                                          series_names, back = identity) {
  p <- dim(arr)[2L]
  # Column `(j - 1) * p + i` holds cell `[i, j]`, the order `matrix()`
  # fills a `p` by `p` matrix in.
  cells <- matrix(arr, nrow = dim(arr)[1L])
  stats <- mvgam_post_summary(cells, robust = robust, probs = probs)
  as_cells <- function(x) back(matrix(x, p, p))
  ess <- without_ess_cap_notice(apply(cells, 2L, function(x) {
    if (stats::sd(x) > 0) posterior::ess_basic(x) else NA_real_
  }))
  name_series_matrices(list(
    point = as_cells(stats[, 1L]),
    se = as_cells(stats[, 2L]),
    lower = as_cells(stats[, 3L]),
    upper = as_cells(stats[, 4L]),
    ess = matrix(ess, p, p)
  ), series_names)
}


#' Label the rows and columns of series-by-series matrices
#'
#' @param mats Named list of square matrices
#' @param series_names Labels for both dimensions
#' @return `mats`, each with dimnames set
#' @noRd
name_series_matrices <- function(mats, series_names) {
  lapply(mats, function(m) {
    dimnames(m) <- list(series_names, series_names)
    m
  })
}


#' Mean absolute entry of a square matrix off its diagonal
#'
#' @param m Square numeric matrix
#' @return Numeric scalar
#' @noRd
mean_abs_offdiag <- function(m) {
  mean(abs(m[!diag(TRUE, nrow(m))]))
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
  per_draw <- vapply(seq_len(ndraws), function(d) {
    mean_abs_offdiag(cor_draws[d, , ])
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
#' names come from `fitted_series_levels()`. Hierarchical labels come
#' from `group_labels()` and `subgroup_labels()`.
#'
#' @noRd
get_residcor_series_names <- function(object, cov_struct) {
  fallback <- paste0("series_", seq_len(cov_struct$n_series))

  if (!isTRUE(cov_struct$hierarchical)) {
    nm <- fitted_series_levels(object) %||% fallback
    if (length(nm) != cov_struct$n_series) nm <- fallback
    return(list(global = nm))
  }

  group_info <- cov_struct$group_info
  list(
    global = subgroup_labels(object, group_info$n_subgroups),
    group_labels = group_labels(object, group_info$n_groups)
  )
}




#' Labels for the groups or subgroups of a hierarchical trend
#'
#' The levels the fit recorded label the correlation blocks when they
#' count one per block. Subgroups named apart in each group give more
#' levels than a group holds series. The labels are then positional.
#'
#' @param levels The recorded levels, or `NULL`
#' @param expected_n How many blocks the program declares
#' @param prefix Stem of the positional labels
#' @return Character vector of length `expected_n`
#' @noRd
lookup_factor_levels <- function(levels, expected_n, prefix) {
  if (length(levels) == expected_n) {
    return(as.character(levels))
  }
  paste0(prefix, "_", seq_len(expected_n))
}


#' Labels for the groups of a hierarchical trend
#'
#' @param object A fitted `mvgam` object
#' @param n_groups How many groups the program declares
#' @return Character vector of length `n_groups`
#' @noRd
group_labels <- function(object, n_groups) {
  lookup_factor_levels(mvgam_axes(object)$group_levels$gr, n_groups,
                       prefix = "group")
}


#' Labels for the subgroups within each group of a hierarchical trend
#'
#' @param object A fitted `mvgam` object
#' @param n_subgroups How many subgroups each group holds
#' @return Character vector of length `n_subgroups`
#' @noRd
subgroup_labels <- function(object, n_subgroups) {
  lookup_factor_levels(mvgam_axes(object)$group_levels$subgr,
                       n_subgroups, prefix = "subgroup")
}


# ---------------------------------------------------------------------------
# S3 methods on mvgam_residcor
# ---------------------------------------------------------------------------

#' Refuse the draws of `residual_cor(summary = FALSE)` where a summary
#' is needed
#'
#' The draws carry the class of the summary. `$` matches partially,
#' and `x$cor` on the draws returns the `cor_draws` array. The methods
#' take fields with `[[` and refuse the draws by name.
#'
#' @param x An `mvgam_residcor` object
#' @return `invisible(x)`, or an error
#' @noRd
assert_residcor_summarised <- function(x) {
  if (is.null(x[["cor"]])) {
    stop(insight::format_error(c(
      "The 'mvgam_residcor' object holds draws from 'summary = FALSE'.",
      i = "Call 'residual_cor()' with 'summary = TRUE' to summarise or plot."
    )), call. = FALSE)
  }
  invisible(x)
}


#' Tidy summary of an `mvgam_residcor` object
#'
#' Returns a tibble with one row per unique upper-triangle pair of
#' series, sorted by `prob_nonzero` descending (so the most credibly
#' non-zero pairs appear first).
#'
#' @param object An `mvgam_residcor` object returned by
#'   [residual_cor()] with `summary = TRUE`.
#' @param ... Unused. Anything passed here is refused.
#'
#' @return A `tibble::tibble` with columns `series_1`, `series_2`,
#'   `Estimate`, `Est.Error`, `Q_lower`, `Q_upper`, `ESS`,
#'   `prob_positive`, `prob_negative`, `prob_nonzero`, `sig` (whether
#'   `prob_nonzero > prob_threshold`). `ESS` is the per-entry
#'   effective sample size of the Fisher-z correlation draws. A low
#'   value marks a pair the chains sampled poorly, however tight its
#'   interval.
#'
#' @method summary mvgam_residcor
#' @export
summary.mvgam_residcor <- function(object, ...) {
  checkmate::assert_class(object, "mvgam_residcor")
  rlang::check_dots_empty()

  assert_residcor_summarised(object)
  cor_mat <- object[["cor"]]

  p <- object[["n_series"]]
  series_names <- object[["series_names"]]

  if (p < 2L) {
    return(tibble::tibble(
      series_1 = character(0), series_2 = character(0),
      Estimate = numeric(0), Est.Error = numeric(0),
      Q_lower = numeric(0), Q_upper = numeric(0),
      ESS = numeric(0),
      prob_positive = numeric(0), prob_negative = numeric(0),
      prob_nonzero = numeric(0), sig = logical(0)
    ))
  }

  pairs <- which(upper.tri(cor_mat), arr.ind = TRUE)
  rows <- pairs[, 1L]
  cols <- pairs[, 2L]
  threshold <- object[["prob_threshold"]]

  out <- tibble::tibble(
    series_1 = series_names[rows],
    series_2 = series_names[cols],
    Estimate = cor_mat[pairs],
    Est.Error = object[["cor_se"]][pairs],
    Q_lower = object[["cor_lower"]][pairs],
    Q_upper = object[["cor_upper"]][pairs],
    ESS = object[["cor_ess"]][pairs],
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
#' @param ... Unused.
#'
#' @return The `mvgam_residcor` object `x`, returned invisibly.
#'
#' @method print mvgam_residcor
#' @export
print.mvgam_residcor <- function(x, digits = 2L, ...) {
  checkmate::assert_class(x, "mvgam_residcor")
  checkmate::assert_int(digits, lower = 0L)

  cor_mat <- x[["cor"]]
  cat("Residual correlations from an mvgam fit\n")
  source <- c(factor_loadings = "factor loadings",
              full_covariance = "VAR innovation covariance",
              cholesky_scaled = "correlated innovations",
              hierarchical_cholesky = "hierarchical correlations")
  cat("  Source     : ", source[[x[["pattern"]]]], "\n", sep = "")
  cat("  Covariance : ", x[["covariance"]], "\n", sep = "")
  cat("  Series     : ", x[["n_series"]], "\n", sep = "")
  if (isTRUE(x[["hierarchical"]])) {
    cat("  Group      : ", x[["group_label"]], "\n", sep = "")
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
    threshold <- x[["prob_threshold"]]
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
