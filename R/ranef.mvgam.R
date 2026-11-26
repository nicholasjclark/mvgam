#' Group-level (random) effects of a fitted \pkg{mvgam} model
#'
#' Posterior draws of each level's deviation for every group-level term
#' of the observation formula, summarised or raw. Each element is named
#' `r_<group>[<level>,<coef>]`, as [variables.mvgam()] lists it.
#'
#' @inheritParams predict.mvgam
#' @param probs Two probabilities for the lower and upper bounds of the
#'   credible interval. Default `c(0.025, 0.975)`, a 95% interval.
#' @param object A fitted `mvgam` object.
#' @param summary Logical. `TRUE` (the default) summarises each
#'   coefficient's draws. `FALSE` returns the draws.
#' @param pars Optional character vector of coefficient names, such as
#'   `"Intercept"`. `NULL` (the default) keeps every coefficient.
#' @param groups Optional character vector of grouping factors. `NULL`
#'   (the default) keeps every factor.
#' @param ... Unused. Anything passed here is refused.
#'
#' @return A named list with one 3D array per grouping factor. With
#'   `summary = TRUE` the dimensions are levels, statistics and
#'   coefficients. The statistics are `Estimate`, `Est.Error` and one
#'   quantile per element of `probs`. With `summary = FALSE` the
#'   dimensions are draws, levels and coefficients, and the array
#'   carries an `nchains` attribute. A grouping factor of a
#'   distributional parameter or of one response of several is keyed
#'   `<group>__<parameter>`.
#'
#' @details
#' A group-level term in `trend_formula` is named with the `_trend`
#' suffix, as `r_g_trend[a,Intercept]`. [as_draws.mvgam()] returns its
#' draws.
#'
#' @author Nicholas J Clark
#'
#' @seealso [VarCorr.mvgam()] summarises the standard deviations and
#'   correlations. [fixef.mvgam()] summarises the population-level
#'   effects.
#'
#' @examples
#' \dontrun{
#' set.seed(14)
#' simdat <- sim_mvgam(family = poisson(), n_series = 4L,
#'                     n_timepoints = 120L, trend_model = AR())
#' mod <- mvgam(y ~ s(x, k = 5) + (1 | series),
#'              trend_formula = ~ AR(p = 1),
#'              data = simdat$data_train, family = poisson(),
#'              chains = 2, silent = 2)
#'
#' # Each series' intercept deviation, one row per series
#' ranef(mod)$series[, , "Intercept"]
#'
#' # Next: VarCorr() gives the standard deviation of these deviations
#' }
#'
#' @family posterior summaries
#' @method ranef mvgam
#' @export
ranef.mvgam <- function(object, summary = TRUE, robust = FALSE,
                         probs = c(0.025, 0.975), pars = NULL,
                         groups = NULL, ...) {
  checkmate::assert_class(object, "mvgam")
  checkmate::assert_flag(summary)
  checkmate::assert_flag(robust)
  checkmate::assert_numeric(
    probs, lower = 0, upper = 1, len = 2L, sorted = TRUE
  )
  checkmate::assert_character(pars, min.len = 1L, null.ok = TRUE)
  checkmate::assert_character(groups, min.len = 1L, null.ok = TRUE)
  rlang::check_dots_empty()
  meta <- require_ranef_metadata(object)
  reframe <- meta$reframe
  if (!is.null(groups)) {
    unknown <- setdiff(groups, reframe$group)
    if (length(unknown) > 0L) {
      stop(insight::format_error(c(
        "'groups' must name grouping factors of the model.",
        x = paste0("Unknown: ", paste0("'", unknown, "'", collapse = ", "),
                   "."),
        i = paste0("Grouping factors: ",
                   paste0("'", unique(reframe$group), "'", collapse = ", "),
                   ".")
      )), call. = FALSE)
    }
    reframe <- reframe[reframe$group %in% groups, , drop = FALSE]
  }
  drws <- extract_mvgam_draws(object)
  # Each (group, predictor) pair is its own entry, keyed as the aliased
  # draws name it: the bare group on a univariate fit and
  # `<group>__<prefix>` otherwise.
  out <- list()
  for (k in unique(reframe$alias_key)) {
    rows <- reframe[reframe$alias_key == k, , drop = FALSE]
    coefs <- as.character(rows$coef)
    if (!is.null(pars)) {
      coefs <- intersect(coefs, pars)
    }
    if (length(coefs) == 0L) next
    levels <- meta$group_levels[[rows$group[1L]]]
    # `outer()` runs over levels within coefficients, the order of the
    # aliased draws.
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
    out[[k]] <- if (summary) {
      brms::posterior_summary(arr, probs = probs, robust = robust)
    } else {
      structure(arr, nchains = posterior::nchains(drws))
    }
  }
  out
}


#' The group-level metadata of a fit's observation model
#'
#' @param object Fitted `mvgam` object
#' @return The list `mvgam_ranef_metadata()` returns. A fit without
#'   observation-side group-level effects is refused.
#' @noRd
require_ranef_metadata <- function(object) {
  meta <- mvgam_ranef_metadata(object)
  if (is.null(meta)) {
    stop(insight::format_error(c(
      "The observation model has no group-level (random) effects.",
      i = paste0("Add a term such as '(1 | g)' or '(x | g)' to the ",
                 "observation formula.")
    )), call. = FALSE)
  }
  meta
}


#' @importFrom brms ranef
#' @export
brms::ranef


#' Standard deviations and correlations of group-level effects
#'
#' Posterior draws of the standard deviation of each group-level term
#' of the observation formula, with the correlations and covariances of
#' terms that share a grouping factor.
#'
#' @param x A fitted `mvgam` object.
#' @param sigma A single non-negative number. It has no effect.
#'   [summary.mvgam()] reports the observation family's scale.
#' @param summary Logical. `TRUE` (the default) summarises the draws.
#'   `FALSE` returns them.
#' @param robust Logical. `TRUE` summarises with the median and MAD.
#'   Defaults to `FALSE`.
#' @param probs Numeric vector of the two quantiles to report.
#' @param ... Unused. Anything passed here is refused.
#'
#' @return A named list with one entry per grouping factor. Each entry
#'   is a list of these components:
#'   * `sd`: the standard deviation of each coefficient, a matrix of
#'     coefficients by statistics, or of draws by coefficients under
#'     `summary = FALSE`.
#'   * `cor`: correlation matrices, present when the factor carries two
#'     or more correlated coefficients. The array runs over
#'     coefficients, statistics and coefficients, or over draws and two
#'     coefficient axes under `summary = FALSE`.
#'   * `cov`: covariance matrices, shaped as `cor`.
#'
#' @details
#' A trend-side component is named with the `_trend` suffix, as
#' `sd_g__Intercept_trend`. [as_draws.mvgam()] returns its draws.
#'
#' @author Nicholas J Clark
#'
#' @seealso [ranef.mvgam()] gives each level's deviation.
#'   [fixef.mvgam()] summarises the population-level effects.
#'
#' @examples
#' \dontrun{
#' set.seed(14)
#' simdat <- sim_mvgam(family = poisson(), n_series = 4L,
#'                     n_timepoints = 120L, trend_model = AR())
#' mod <- mvgam(y ~ s(x, k = 5) + (1 | series),
#'              trend_formula = ~ AR(p = 1),
#'              data = simdat$data_train, family = poisson(),
#'              chains = 2, silent = 2)
#'
#' # The standard deviation of the series intercepts
#' VarCorr(mod)$series$sd
#'
#' # Next: ranef() gives each series' own deviation
#' }
#'
#' @family posterior summaries
#' @method VarCorr mvgam
#' @export
VarCorr.mvgam <- function(x, sigma = 1, summary = TRUE,
                           robust = FALSE, probs = c(0.025, 0.975),
                           ...) {
  checkmate::assert_class(x, "mvgam")
  checkmate::assert_number(sigma, lower = 0)
  checkmate::assert_flag(summary)
  checkmate::assert_flag(robust)
  checkmate::assert_numeric(
    probs, lower = 0, upper = 1, len = 2L, sorted = TRUE
  )
  rlang::check_dots_empty()
  meta <- require_ranef_metadata(x)
  reframe <- meta$reframe
  drws <- extract_mvgam_draws(x)
  keys <- unique(reframe$alias_key)
  out <- vector("list", length(keys))
  names(out) <- keys
  for (k in keys) {
    rows <- reframe[reframe$alias_key == k, , drop = FALSE]
    g_bare <- rows$group[1L]
    ids <- split(rows, factor(rows$id, levels = unique(rows$id)))
    coefs <- unlist(lapply(ids, ranef_rnames, col = "coef"),
                    use.names = FALSE)
    coef_aliases <- unlist(lapply(ids, ranef_rnames), use.names = FALSE)
    n_coef <- length(coefs)
    sd_mat <- posterior::as_draws_matrix(posterior::subset_draws(
      drws, variable = paste0("sd_", g_bare, "__", coef_aliases)
    ))
    colnames(sd_mat) <- coefs
    has_cor <- any(rows$cor) && n_coef > 1L
    group_out <- list(sd = unclass(sd_mat))
    if (has_cor) {
      # A pair brms estimates no correlation for, across two ids or two
      # levels of a `by` factor, is zero, as `brms::VarCorr()` fills it
      cor_names <- ranef_cor_names(g_bare, coef_aliases)
      cor_mat <- matrix(0, nrow = nrow(sd_mat), ncol = length(cor_names),
                        dimnames = list(NULL, cor_names))
      found <- intersect(cor_names, posterior::variables(drws))
      if (length(found)) {
        cor_mat[, found] <- as.matrix(posterior::as_draws_matrix(
          posterior::subset_draws(drws, variable = found)
        ))
      }
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


#' The names brms gives the coefficients of one group-level id
#'
#' Follows the internal brms function `get_rnames()`. A grouping with a
#' `by` factor, `gr(g, by = f)`, estimates each coefficient once per
#' level of `f`. Each cell is named `<coef>:<f><level>`, with the
#' coefficient varying fastest.
#'
#' @param rows Data frame of the rows of the `mvgam_ranef_metadata()`
#'   table for one id
#' @param col Character name of the column naming the coefficients,
#'   `"coef_alias"` for parameter names or `"coef"` for labels
#' @return Character vector with one name per cell
#' @noRd
ranef_rnames <- function(rows, col = "coef_alias") {
  checkmate::assert_data_frame(rows, min.rows = 1L)
  checkmate::assert_choice(col, c("coef_alias", "coef"))
  by <- rows$by[1L]
  coefs <- rows[[col]]
  if (length(by) == 0L || is.na(by) || !nzchar(by)) {
    return(coefs)
  }
  as.vector(outer(coefs, paste0(by, rows$bylevels[[1L]]), paste,
                  sep = ":"))
}


#' The names of a group's correlations in the order brms stores them
#'
#' The brms Stan code assigns `cor[choose(k - 1, 2) + j] = Cor[j, k]`
#' for `j < k`, filling the upper triangle column by column. Four
#' coefficients give the pairs (1,2), (1,3), (2,3), (1,4), (2,4) and
#' (3,4). `assemble_cor_array()` unpacks them in the same order.
#'
#' @param group Character name of the grouping factor
#' @param coef_alias Character vector of at least two coefficient names
#' @return Character vector of correlation parameter names
#' @noRd
ranef_cor_names <- function(group, coef_alias) {
  checkmate::assert_string(group)
  checkmate::assert_character(coef_alias, min.len = 2L)
  n <- length(coef_alias)
  ks <- rep(2:n, times = seq_len(n - 1L))
  js <- unlist(lapply(2:n, function(k) seq_len(k - 1L)))
  sprintf("cor_%s__%s__%s", group, coef_alias[js], coef_alias[ks])
}


#' Per-draw correlation matrices from their packed draws
#'
#' The diagonal is 1. Column `k = choose(i - 1, 2) + j` of `cor_mat`
#' fills cells `[j, i]` and `[i, j]`, following the brms packing order.
#'
#' @param cor_mat Numeric matrix of correlation draws with
#'   `size * (size - 1) / 2` columns
#' @param size Integer number of coefficients
#' @param coef_names Character vector of coefficient names
#' @return Numeric array with dimensions `[n_draws, size, size]`
#' @noRd
assemble_cor_array <- function(cor_mat, size, coef_names) {
  checkmate::assert_matrix(cor_mat)
  checkmate::assert_int(size, lower = 1L)
  checkmate::assert_character(coef_names, len = size)
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


#' Per-draw covariance matrices from SDs and correlations
#'
#' Computes `D R D` with `D = diag(sd)` as an elementwise product over
#' cells, avoiding one matrix product per draw.
#'
#' @param sd_mat Numeric matrix of SD draws with dimensions
#'   `[n_draws, size]`
#' @param cor_arr Numeric correlation array with dimensions
#'   `[n_draws, size, size]`
#' @param coef_names Character vector of coefficient names
#' @return Numeric array with dimensions `[n_draws, size, size]`
#' @noRd
assemble_cov_array <- function(sd_mat, cor_arr, coef_names) {
  checkmate::assert_matrix(sd_mat)
  checkmate::assert_array(cor_arr, d = 3L)
  checkmate::assert_character(coef_names, len = ncol(sd_mat))
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
