#' Extract group-level (random) effects from a fitted \pkg{mvgam}
#' model
#'
#' Pull posterior draws of the group-level (random) effects on a
#' fitted `mvgam` object and return them in the same shape as
#' [brms::ranef.brmsfit()] so brms-trained users can drop into the
#' same workflow. Names follow brms's `r_<group>[<level>, <coef>]`
#' convention; the underlying positional Stan parameters are
#' aliased transparently.
#'
#' @param object A fitted `mvgam` object.
#' @param summary Logical. If `TRUE` (the default), collapse each
#'   group's posterior draws to a `[n_levels x n_stats x n_coefs]`
#'   array of summary statistics
#'   (`Estimate`, `Est.Error`, and the lower / upper quantiles
#'   defined by `probs`). If `FALSE`, return the raw draws as a
#'   `[n_draws x n_levels x n_coefs]` array per group.
#' @param robust Logical. When `summary = TRUE`, use the median /
#'   MAD as the centre / spread instead of the mean / SD. Defaults
#'   to `FALSE`.
#' @param probs Numeric vector of length 2 with the quantiles to
#'   report when `summary = TRUE`. Defaults to
#'   `c(0.025, 0.975)`.
#' @param pars Optional character vector. When set, restrict the
#'   returned coefficients to those whose name matches one of the
#'   supplied entries (matched against the brms `coef` column).
#'   `NULL` (the default) keeps every coefficient.
#' @param groups Optional character vector. When set, restrict the
#'   returned grouping factors. `NULL` (the default) keeps every
#'   factor.
#' @param ... Unused; present for S3 / brms-parity.
#'
#' @return A named list with one entry per grouping factor. Each
#'   entry is a 3D `array`:
#'   * `summary = TRUE` (default) - dimensions
#'     `[n_levels, n_stats, n_coefs]` with dimnames
#'     `list(levels, c("Estimate", "Est.Error", "Q*", "Q*"), coefs)`.
#'   * `summary = FALSE` - dimensions
#'     `[n_draws, n_levels, n_coefs]` with dimnames
#'     `list(NULL, levels, coefs)` and an `nchains` attribute.
#'
#'   When the fit contains no group-level effects an error is
#'   raised; supply `groups = ...` for cases where only some
#'   factors are of interest.
#'
#' @details
#' Observation-side random effects only. Trend-side group-level
#' effects (i.e. `(1 | g)` inside `trend_formula`) remain
#' accessible via their positional Stan names; brms-parity
#' aliasing for trend REs is deferred.
#'
#' @author Nicholas J Clark
#'
#' @seealso [brms::ranef.brmsfit()], [VarCorr.mvgam()],
#'   [fixef.mvgam()], [variables.mvgam()].
#'
#' @examples
#' \donttest{
#' set.seed(14)
#' simdat <- sim_mvgam(family = poisson(), n_series = 4L,
#'                      n_timepoints = 40L, trend_model = AR())
#' mod <- mvgam(y ~ s(x) + (1 | series),
#'               trend_formula = ~ AR(p = 1),
#'               data    = simdat$data_train,
#'               family  = poisson(),
#'               chains  = 2, silent = 2)
#'
#' # `ranef()` returns a per-group nested list with one matrix per
#' # level: rows are levels, columns are `Estimate`, `Est.Error`,
#' # and the 95% credible bounds.
#' re <- ranef(mod)
#' str(re, max.level = 2)
#' re$series[, , "Intercept"]
#' }
#'
#' @method ranef mvgam
#' @export
ranef.mvgam <- function(object, summary = TRUE, robust = FALSE,
                         probs = c(0.025, 0.975), pars = NULL,
                         groups = NULL, ...) {
  checkmate::assert_class(object, "mvgam")
  checkmate::assert_logical(summary, len = 1L)
  checkmate::assert_logical(robust, len = 1L)
  checkmate::assert_numeric(
    probs, lower = 0, upper = 1, len = 2L, sorted = TRUE
  )
  if (!is.null(pars)) {
    checkmate::assert_character(pars, min.len = 1L)
  }
  if (!is.null(groups)) {
    checkmate::assert_character(groups, min.len = 1L)
  }
  meta <- mvgam_ranef_metadata(object)
  if (is.null(meta)) {
    stop(insight::format_error(c(
      paste0(
        "Cannot extract group-level effects: this 'mvgam' object ",
        "has no observation-side random effects."
      ),
      i = paste0(
        "Add group-level terms via '(1 | g)' / '(x | g)' / ",
        "'gr(g, ...)' on the observation formula."
      )
    )))
  }
  reframe <- meta$reframe
  group_levels <- meta$group_levels
  all_groups <- unique(reframe$group)
  if (!is.null(groups)) {
    all_groups <- intersect(all_groups, as.character(groups))
  }
  if (length(all_groups) == 0L) {
    return(stats::setNames(list(), character(0L)))
  }
  drws <- extract_mvgam_draws(object)
  n_chains <- posterior::nchains(drws)
  # Multi-response, nlpar, and dpar grouping rows live under
  # `<group>__<resp|nlpar|dpar>` in the aliased draws (see
  # `mvgam_ranef_aliases`). Reuse the same per-row prefix logic
  # here so the lookup matches the alias output exactly. Each
  # (group, prefix) pair becomes its own entry in the returned
  # list, keeping the brms-parity shape on univariate fits
  # (`prefix == ""`, key = bare group name).
  reframe$row_prefix <- make_row_prefix(
    reframe$nlpar, reframe$dpar, reframe$resp
  )
  alias_keys <- ifelse(
    nzchar(reframe$row_prefix),
    paste0(reframe$group, "__", reframe$row_prefix),
    reframe$group
  )
  reframe$alias_key <- alias_keys
  keys <- unique(reframe$alias_key)
  if (!is.null(groups)) {
    keys <- intersect(keys, as.character(groups))
  }
  out <- vector("list", length(keys))
  names(out) <- keys
  for (k in keys) {
    rows <- reframe[reframe$alias_key == k, , drop = FALSE]
    coefs <- as.character(rows$coef)
    if (!is.null(pars)) {
      coefs <- intersect(coefs, as.character(pars))
    }
    if (length(coefs) == 0L) next
    g_bare <- rows$group[1L]
    levels <- group_levels[[g_bare]]
    # outer() walks levels (rows) x coefs (cols); as.vector
    # flattens column-major to match the (level-minor, coef-major)
    # storage the alias map emits.
    rpars <- as.vector(outer(levels, coefs, function(l, c) {
      sprintf("r_%s[%s,%s]", k, l, c)
    }))
    mat <- posterior::as_draws_matrix(
      posterior::subset_draws(drws, variable = rpars)
    )
    arr <- array(
      as.numeric(mat),
      dim = c(nrow(mat), length(levels), length(coefs)),
      dimnames = list(NULL, levels, coefs)
    )
    if (summary) {
      arr <- brms::posterior_summary(
        arr, probs = probs, robust = robust
      )
    } else {
      attr(arr, "nchains") <- n_chains
    }
    out[[k]] <- arr
  }
  out[!vapply(out, is.null, logical(1L))]
}


#' @importFrom brms ranef
#' @export
brms::ranef


#' Extract variance and correlation components of group-level
#' (random) effects from a fitted \pkg{mvgam} model
#'
#' Pull posterior draws of the group-level standard deviations
#' and (when estimated) correlation / covariance matrices on a
#' fitted `mvgam` object, returned in the same shape as
#' [brms::VarCorr.brmsfit()] so brms-trained users can drop into
#' the same workflow.
#'
#' @param x A fitted `mvgam` object.
#' @param sigma Retained for brms-parity; currently unused on
#'   `mvgam` fits because the observation-family scale is exposed
#'   via [posterior_predict()] rather than the residual-sigma
#'   pathway brms uses. Must be a single positive number.
#' @param summary Logical. If `TRUE` (the default), collapse each
#'   group's posterior draws to summary statistics; if `FALSE`,
#'   return the raw draws.
#' @param robust Logical. When `summary = TRUE`, use the median /
#'   MAD as the centre / spread. Defaults to `FALSE`.
#' @param probs Numeric vector of length 2 with the quantiles to
#'   report when `summary = TRUE`.
#' @param ... Unused; present for S3 / brms-parity.
#'
#' @return A named list with one entry per grouping factor. Each
#'   entry is itself a list with up to three components:
#'   * `sd` - matrix of standard deviations per coefficient.
#'     With `summary = TRUE`, dimensions
#'     `[n_coefs, n_stats]`; with `summary = FALSE`,
#'     `[n_draws, n_coefs]`.
#'   * `cor` - correlation matrices per posterior draw. Only
#'     emitted when the group has two or more coefficients with
#'     an estimated correlation block. With `summary = TRUE`,
#'     dimensions `[n_coefs, n_stats, n_coefs]`; with
#'     `summary = FALSE`, `[n_draws, n_coefs, n_coefs]`.
#'   * `cov` - covariance matrices per posterior draw with the
#'     same dimensions as `cor`.
#'
#' @details
#' Observation-side group-level effects only. Trend-side
#' covariance components remain accessible via their positional
#' Stan names; brms-parity aliasing for the trend block is
#' deferred.
#'
#' @author Nicholas J Clark
#'
#' @seealso [brms::VarCorr.brmsfit()], [ranef.mvgam()],
#'   [fixef.mvgam()].
#'
#' @examples
#' \donttest{
#' set.seed(14)
#' simdat <- sim_mvgam(family = poisson(), n_series = 4L,
#'                      n_timepoints = 40L, trend_model = AR())
#' mod <- mvgam(y ~ s(x) + (1 | series),
#'               trend_formula = ~ AR(p = 1),
#'               data    = simdat$data_train,
#'               family  = poisson(),
#'               chains  = 2, silent = 2)
#'
#' # `VarCorr()` returns the per-grouping variance / covariance
#' # estimates. Each grouping has an `sd` matrix (rows = effects,
#' # columns = posterior summary) and, for multi-effect groupings,
#' # a `cor` matrix giving the implied correlation structure. Use
#' # `ranef()` for the level-specific BLUP-style estimates.
#' vc <- VarCorr(mod)
#' str(vc, max.level = 2)
#' vc$series$sd
#' }
#'
#' @method VarCorr mvgam
#' @export
VarCorr.mvgam <- function(x, sigma = 1, summary = TRUE,
                           robust = FALSE, probs = c(0.025, 0.975),
                           ...) {
  checkmate::assert_class(x, "mvgam")
  checkmate::assert_number(sigma, lower = 0)
  checkmate::assert_logical(summary, len = 1L)
  checkmate::assert_logical(robust, len = 1L)
  checkmate::assert_numeric(
    probs, lower = 0, upper = 1, len = 2L, sorted = TRUE
  )
  meta <- mvgam_ranef_metadata(x)
  if (is.null(meta)) {
    stop(insight::format_error(c(
      paste0(
        "Cannot extract variance components: this 'mvgam' object ",
        "has no observation-side random effects."
      ),
      i = paste0(
        "Add group-level terms via '(1 | g)' / '(x | g)' / ",
        "'gr(g, ...)' on the observation formula."
      )
    )))
  }
  reframe <- meta$reframe
  drws <- extract_mvgam_draws(x)
  # Mirror ranef.mvgam's per-row prefix logic so the lookup keys
  # match the alias map for multi-response, nlpar and dpar fits.
  # Univariate rows have an empty prefix and the key is the bare
  # group name, preserving the brms-parity list shape.
  reframe$row_prefix <- make_row_prefix(
    reframe$nlpar, reframe$dpar, reframe$resp
  )
  reframe$alias_key <- ifelse(
    nzchar(reframe$row_prefix),
    paste0(reframe$group, "__", reframe$row_prefix),
    reframe$group
  )
  # For sd the alias map writes `sd_<group>__<prefix>_<coef>`
  # (e.g. `sd_grp__y1_Intercept`) -- see `mvgam_ranef_aliases`.
  # cor uses the same prefixed coef token.
  reframe$coef_alias <- ifelse(
    nzchar(reframe$row_prefix),
    paste0(reframe$row_prefix, "_", reframe$coef),
    as.character(reframe$coef)
  )
  keys <- unique(reframe$alias_key)
  out <- vector("list", length(keys))
  names(out) <- keys
  for (k in keys) {
    rows <- reframe[reframe$alias_key == k, , drop = FALSE]
    coefs <- as.character(rows$coef)
    coef_aliases <- as.character(rows$coef_alias)
    g_bare <- rows$group[1L]
    n_coef <- length(coefs)
    sd_names <- sprintf("sd_%s__%s", g_bare, coef_aliases)
    sd_mat <- posterior::as_draws_matrix(
      posterior::subset_draws(drws, variable = sd_names)
    )
    colnames(sd_mat) <- coefs
    has_cor <- isTRUE(rows$cor[1L]) && n_coef > 1L
    group_out <- list(sd = unclass(sd_mat))
    if (has_cor) {
      # Walk pairs in brms's column-major upper-triangle order
      # (`cor[choose(k - 1, 2) + j] = Cor[j, k]`).
      ks <- rep(2:n_coef, times = seq_len(n_coef - 1L))
      js <- unlist(lapply(2:n_coef, function(k) seq_len(k - 1L)))
      cor_names <- sprintf("cor_%s__%s__%s", g_bare,
                            coef_aliases[js], coef_aliases[ks])
      cor_mat <- posterior::as_draws_matrix(
        posterior::subset_draws(drws, variable = cor_names)
      )
      cor_arr <- assemble_cor_array(cor_mat, n_coef, coefs)
      cov_arr <- assemble_cov_array(group_out$sd, cor_arr, coefs)
      group_out$cor <- cor_arr
      group_out$cov <- cov_arr
    }
    if (summary) {
      group_out$sd <- brms::posterior_summary(
        group_out$sd, probs = probs, robust = robust
      )
      if (has_cor) {
        group_out$cor <- brms::posterior_summary(
          group_out$cor, probs = probs, robust = robust
        )
        group_out$cov <- brms::posterior_summary(
          group_out$cov, probs = probs, robust = robust
        )
      }
    }
    out[[k]] <- group_out
  }
  out
}


#' @importFrom brms VarCorr
#' @export
brms::VarCorr


# Internal: build [n_draws x size x size] symmetric correlation
# arrays from the column-major upper-triangle flat-vector draws
# in `cor_mat` (shape [n_draws x NC] with NC = size*(size-1)/2).
# Diagonal is 1; off-diagonals follow brms's pack order via
# `out[,j,i] = out[,i,j] = cor_mat[,k]` for k = choose(i-1, 2) + j.
#'@noRd
assemble_cor_array <- function(cor_mat, size, coef_names) {
  n_draws <- nrow(cor_mat)
  out <- array(diag(1, size), dim = c(size, size, n_draws))
  out <- aperm(out, perm = c(3L, 1L, 2L))
  k <- 0L
  for (i in seq_len(size)[-1L]) {
    for (j in seq_len(i - 1L)) {
      k <- k + 1L
      out[, j, i] <- out[, i, j] <- cor_mat[, k]
    }
  }
  dimnames(out) <- list(NULL, coef_names, coef_names)
  out
}


# Internal: build [n_draws x size x size] covariance arrays from
# the per-draw `sd_mat` ([n_draws x size]) and correlation array
# `cor_arr` ([n_draws x size x size]) via `cov = D %*% R %*% D`
# where D = diag(sd). Implemented as an elementwise sweep so the
# helper avoids one matrix multiply per draw.
#'@noRd
assemble_cov_array <- function(sd_mat, cor_arr, coef_names) {
  size <- ncol(sd_mat)
  out <- cor_arr
  for (i in seq_len(size)) {
    for (j in seq_len(size)) {
      out[, i, j] <- cor_arr[, i, j] * sd_mat[, i] * sd_mat[, j]
    }
  }
  dimnames(out) <- list(NULL, coef_names, coef_names)
  out
}
