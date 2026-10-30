# Factor loadings: which posterior columns hold them, their draws
# for every factor model, whether sampled or fixed by a `trend_map`,
# and the projection of factor paths onto the series.


#' The parameter pattern of a factor model's loadings
#'
#' A fit identified by the QR rotation stores its reported loadings
#' and factor paths as `Z_tilde[i, j]` and `lv_trend_tilde[t, k]`.
#' Every other factor fit reports `Z[i, j]` and `lv_trend[t, k]`. A
#' fit whose factors take their own coefficients relabels them after
#' sampling. A `trend_map` fit stores them as declared.
#' `factor_identification()` decides which applies.
#' `extract_Z_loadings()`, `extract_lv_trend_matrices()`,
#' `match_z_loadings()` and `categorize_mvgam_parameters()` select
#' through these helpers.
#'
#' @param pars Parameter names from the fit.
#' @param basis `"identified"` for the loadings a report or plot
#'   shows. `"model"` for the `Z` the model sampled. A caller
#'   combining loadings with `sigma_trend`, `Sigma_trend` or
#'   `Omega_trend` needs `"model"`: the model estimated those
#'   parameters with the sampled `Z`. The two differ only in a
#'   rotated fit.
#' @return Regular expression matching the loading columns
#' @noRd
factor_loading_param_pattern <- function(pars,
                                         basis = c("identified",
                                                   "model")) {
  checkmate::assert_character(pars)
  basis <- match.arg(basis)
  if (identical(basis, "model")) return("^Z\\[")
  if (has_identified_loadings(pars)) "^Z_tilde\\[" else "^Z\\["
}


#' Does this fit store QR-rotated loadings?
#'
#' A fit identified by rotation stores `Z_tilde` as well as the `Z`
#' it sampled. Every caller asking whether a fit was rotated tests
#' for that parameter here.
#'
#' @param pars Character vector of parameter names
#' @return A single logical
#'
#' @noRd
has_identified_loadings <- function(pars) {
  checkmate::assert_character(pars)
  any(grepl("^Z_tilde\\[", pars))
}


#' Does this fit store QR-rotated factor paths?
#'
#' As `has_identified_loadings()`, for the factor paths the loadings
#' multiply.
#'
#' @param pars Character vector of parameter names
#' @return A single logical
#'
#' @noRd
has_identified_factor_states <- function(pars) {
  checkmate::assert_character(pars)
  any(grepl("^lv_trend_tilde\\[", pars))
}


#' The parameter pattern of a factor model's paths
#'
#' @param pars Character vector of parameter names
#' @return Regular expression matching the factor path columns
#' @noRd
factor_state_param_pattern <- function(pars) {
  checkmate::assert_character(pars)
  if (has_identified_factor_states(pars)) {
    "^lv_trend_tilde\\["
  } else {
    "^lv_trend\\["
  }
}


#' Factor-loading draws from the posterior
#'
#' `factor_loading_param_pattern()` selects `Z_tilde` or `Z`. The array
#' follows the column-major order Stan stores the loading matrix in.
#' `resolve_factor_loadings()` also covers a fully fixed `trend_map`
#' fit, whose loadings are data.
#'
#' @param draws_mat Numeric draws matrix with parameter column names
#' @param n_obs_series Integer number of observed series
#' @param n_lv Integer number of latent factors
#' @param basis Character loading basis, `"identified"` for the
#'   QR-identified loadings or `"model"` for the sampled ones
#' @return Numeric array with dimensions `[ndraws, n_obs_series, n_lv]`
#' @noRd
extract_Z_loadings <- function(draws_mat, n_obs_series, n_lv,
                               basis = c("identified", "model")) {
  checkmate::assert_matrix(draws_mat)
  checkmate::assert_int(n_obs_series, lower = 1L)
  checkmate::assert_int(n_lv, lower = 1L)
  basis <- match.arg(basis)
  ndraws <- nrow(draws_mat)
  pattern <- factor_loading_param_pattern(colnames(draws_mat), basis)
  param_name <- if (pattern == "^Z_tilde\\[") "Z_tilde" else "Z"
  cols <- grep(pattern, colnames(draws_mat), value = TRUE)
  expected_cols <- n_obs_series * n_lv
  if (length(cols) != expected_cols) {
    stop_mvgam_fault(
      paste0("Expected ", expected_cols, " ", param_name,
             " loading columns, found ", length(cols), "."),
      paste0("A factor model stores ", param_name, "[s, lv] for s in 1..",
             n_obs_series, " and lv in 1..", n_lv, ".")
    )
  }
  # Stan stores the loading matrix column-major:
  # [1,1], [2,1], ..., [N_series,1], [1,2], ...
  array(
    as.numeric(draws_mat[, cols, drop = FALSE]),
    dim = c(ndraws, n_obs_series, n_lv)
  )
}


#' A time-by-factor grid of every draw projected onto the series
#'
#' For draw `d`, time `t` and series `s` the output holds
#' `sum_lv Z[d, s, lv] * grid[d, t + (lv - 1) * n_times]` in column
#' `t + (s - 1) * n_times`. Time varies fastest within each factor on
#' input. That is the order of a Stan `matrix[N_time, N_lv]` and of
#' `transform_diagonal_innovations()`. Time varies fastest within each
#' series on output, the order of a `matrix[N_time, N_series]`. The
#' grid can hold innovations or a state path.
#'
#' @param grid Numeric matrix with dimensions `[ndraws, n_times * n_lv]`
#' @param Z Numeric loading array with dimensions
#'   `[ndraws, n_obs_series, n_lv]`
#' @param n_times Integer number of time points
#' @param n_lv Integer number of latent factors
#' @param n_obs_series Integer number of observed series
#' @return Numeric matrix with dimensions
#'   `[ndraws, n_times * n_obs_series]`
#' @noRd
project_lv_grid <- function(grid, Z, n_times, n_lv, n_obs_series) {
  checkmate::assert_int(n_times, lower = 1L)
  checkmate::assert_int(n_lv, lower = 1L)
  checkmate::assert_int(n_obs_series, lower = 1L)
  checkmate::assert_matrix(grid, ncols = n_times * n_lv)
  checkmate::assert_array(Z, d = 3L)
  ndraws <- nrow(grid)
  out <- matrix(0, ndraws, n_times * n_obs_series)
  for (d in seq_len(ndraws)) {
    # A single factor would drop `Z[d, , ]` to a vector. The reshape
    # keeps the `[n_obs_series, n_lv]` matrix.
    Z_d <- matrix(Z[d, , ], nrow = n_obs_series, ncol = n_lv)
    lv_d <- matrix(grid[d, ], nrow = n_times, ncol = n_lv)
    out[d, ] <- as.numeric(tcrossprod(lv_d, Z_d))
  }
  out
}


#' Resolve per-draw factor loadings for a fitted mvgam.
#'
#' Returns a `[ndraws, n_series, n_lv]` array of Z loadings whether
#' Z was sampled (the default factor model) or fixed by a
#' `trend_map`. Every method that needs Z calls this resolver, which
#' holds the choice between the fixed and sampled paths.
#'
#' Two calling styles:
#' - Object style: `resolve_factor_loadings(object = fit)`. The
#'   resolver takes `fixed_Z`, `n_lv` and `n_series` from the fit
#'   and extracts draws as needed.
#' - Explicit style: `resolve_factor_loadings(fixed_Z = ...,
#'   draws_mat = ..., n_series = ..., n_lv = ...)`. `n_series` and
#'   `n_lv` are required in this style. `draw_innovation_grid()`
#'   uses it with draws it has already extracted.
#'
#' A fully populated `fixed_Z` is repeated for every draw. Otherwise
#' `extract_Z_loadings()` extracts the draws. A fit identified by the
#' QR rotation stores `Z_tilde[i, j]`. Every other factor fit stores
#' `Z[i, j]`.
#'
#' @param object Fitted mvgam object (object-style entry).
#'   Required when callers omit `n_series`/`n_lv`.
#' @param draws_mat Optional pre-extracted posterior draws.
#'   Required when `object` is omitted and `fixed_Z` is NULL.
#' @param fixed_Z Optional fixed-loadings matrix. With `object` it
#'   defaults to `object$trend_metadata$fixed_Z`.
#' @param n_lv Integer factor dimension. Required in
#'   explicit-style; derived from `object` otherwise.
#' @param n_series Integer series count. Required in
#'   explicit-style; derived from `object` otherwise.
#' @param basis `"identified"` returns the loadings a report or plot
#'   shows. `"model"` returns the `Z` the model sampled. A caller
#'   combining loadings with `sigma_trend`, `Sigma_trend` or
#'   `Omega_trend` needs `"model"`: the model estimated those
#'   parameters with the sampled `Z`. The two differ only in a fit
#'   identified by the QR rotation.
#'
#' @return Numeric array of dimension `[ndraws, n_series, n_lv]`.
#' @noRd
resolve_factor_loadings <- function(object = NULL,
                                    draws_mat = NULL,
                                    fixed_Z = NULL,
                                    n_lv = NULL,
                                    n_series = NULL,
                                    basis = c("identified", "model")) {
  basis <- match.arg(basis)
  checkmate::assert_matrix(draws_mat, null.ok = TRUE)
  checkmate::assert_matrix(fixed_Z, null.ok = TRUE)
  if (!is.null(object)) {
    checkmate::assert_class(object, "mvgam")
    if (is.null(fixed_Z)) {
      fixed_Z <- object$trend_metadata$fixed_Z
    }
    if (is.null(n_lv)) {
      n_lv <- object$trend_metadata$n_lv %||%
        (if (!is.null(fixed_Z)) ncol(fixed_Z) else NULL)
    }
    n_series <- n_series %||% loading_series_count(object)
  }
  checkmate::assert_int(n_lv, lower = 1L)
  checkmate::assert_int(n_series, lower = 1L)

  # A partial Z marks its sampled entries with NA. The posterior
  # stores every entry as `Z[i, j]`. The draws parser below handles
  # that case.
  fully_fixed <- !is.null(fixed_Z) && !anyNA(fixed_Z)
  if (fully_fixed) {
    # A fixed-Z fit stores Z in the Stan data. The posterior has no
    # `Z[i, j]` columns. The matrix is repeated for every draw to
    # match the sampled-Z return shape.
    ndraws <- if (!is.null(draws_mat)) {
      nrow(draws_mat)
    } else if (!is.null(object)) {
      posterior::ndraws(object)
    } else {
      stop_mvgam_fault(
        "The fixed loadings need a draw count.",
        "The caller passed 'object = NULL' and 'draws_mat = NULL'."
      )
    }
    return(array(
      rep(as.numeric(unname(fixed_Z)), each = ndraws),
      dim = c(ndraws, n_series, n_lv)
    ))
  }

  if (is.null(draws_mat)) {
    if (is.null(object)) {
      stop_mvgam_fault(
        "The sampled loadings need draws.",
        "The caller passed 'object = NULL' and 'draws_mat = NULL'."
      )
    }
    draws_mat <- fit_draws(object$fit, c("Z", "Z_tilde"))
  }
  extract_Z_loadings(draws_mat, n_obs_series = n_series,
                     n_lv = n_lv, basis = basis)
}


#' Posterior medians of a factor model's loadings
#'
#' @param object Fitted `mvgam` factor model
#' @param n_lv Integer number of latent factors
#' @return Numeric matrix `[n_series, n_lv]` with the fitted series as
#'   row names
#' @noRd
factor_loading_medians <- function(object, n_lv) {
  med <- apply(resolve_factor_loadings(object, n_lv = n_lv), c(2L, 3L),
               stats::median)
  rownames(med) <- fitted_series_levels(object)
  med
}


#' Count the series a factor model's loadings span
#'
#' The axis record counts the series. A fit without one takes the
#' count of its fitted series levels.
#'
#' @param object An `mvgam` model object
#' @return Integer count of series
#'
#' @noRd
loading_series_count <- function(object) {
  # The rows of `Z` are the series axis. The axis record counts
  # them. A column's declared levels can include a level with zero
  # rows, which would size every loadings surface one series too
  # wide.
  n <- mvgam_axes(object)$series$n
  if (!is.null(n)) {
    return(as.integer(n))
  }
  as.integer(length(fitted_series_levels(object)))
}

