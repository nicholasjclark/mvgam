#' Compute Approximate Gaussian Process Contribution
#'
#' Computes GP contributions to linear predictor using Hilbert space
#' approximation. Uses spectral power density computation with 
#' kernel-specific dispatch for accurate brms compatibility.
#'
#' @param Xgp Matrix (N × k) of basis function evaluations at
#'   prediction points
#' @param slambda Array (k × dims) or (k × dims × 1) of eigenvalues
#'   for spectral basis functions
#' @param zgp Matrix (ndraws × k) of standard normal draws
#' @param sdgp Vector (ndraws) of marginal standard deviations
#' @param lscale Matrix (ndraws × dims) of length-scale parameters
#' @param kernel Character string specifying kernel type: "exp_quad", 
#'   "matern32", or "matern52"
#'
#' @return Matrix (ndraws × N) of GP contributions to add to linear
#'   predictor
#'
#' @details
#' Implements the brms Stan formula:
#'   (sqrt(spd_gp(slambda, sdgp, lscale, kernel)) * zgp) %*% t(Xgp)
#'
#' Computation steps:
#' 1. Compute spectral power density using kernel-specific function
#' 2. Take square root for direct multiplication with GP coefficients
#' 3. Element-wise multiply with standard normal draws
#' 4. Matrix multiply with transposed basis functions
#'
#' @noRd
approx_gp_pred <- function(Xgp, slambda, zgp, sdgp, lscale, kernel) {
  # Validate inputs
  checkmate::assert_matrix(Xgp, any.missing = FALSE, all.missing = FALSE)
  checkmate::assert_array(slambda, min.d = 2, max.d = 3, any.missing = FALSE)
  checkmate::assert_matrix(zgp, any.missing = FALSE, all.missing = FALSE)
  checkmate::assert_numeric(sdgp, any.missing = FALSE, min.len = 1)
  checkmate::assert_matrix(lscale, any.missing = FALSE, all.missing = FALSE)
  checkmate::assert_string(kernel, min.chars = 1)
  
  # Extract dimensions
  n_obs <- nrow(Xgp)
  n_basis <- ncol(Xgp)
  n_draws <- nrow(zgp)
  
  # Validate dimension consistency
  if (ncol(zgp) != n_basis) {
    stop(insight::format_error(
      cli::format_inline(
        "Basis function mismatch: {.field Xgp} has {n_basis} basis functions but {.field zgp} has {ncol(zgp)} coefficients."
      )
    ))
  }

  if (length(sdgp) != n_draws) {
    stop(insight::format_error(
      cli::format_inline(
        "Draw count mismatch: {.field zgp} has {n_draws} draws but {.field sdgp} has {length(sdgp)} elements."
      )
    ))
  }
  
  # Compute spectral power density (returns sqrt for direct use)
  spd_sqrt <- compute_spd_vectorized(slambda, sdgp, lscale, kernel)
  
  # Apply correct brms formula: (spd * zgp) %*% t(Xgp)  
  # spd_sqrt is [n_draws, n_basis], zgp is [n_draws, n_basis]
  spd_zgp <- spd_sqrt * zgp
  
  # Matrix multiply with transposed basis functions
  # spd_zgp %*% t(Xgp) gives [n_draws, n_obs]
  result <- spd_zgp %*% t(Xgp)
  
  result
}


#' Prepare SPD Inputs with Isotropic Detection
#'
#' Common helper for all GP spectral density functions. Validates inputs,
#' extracts first eigenvalue matrix slice from 3D arrays, and determines
#' if GP is isotropic or anisotropic. Follows brms pattern of checking
#' lscale column count (structure) rather than comparing values.
#'
#' @param slambda Array of eigenvalues; matrix \[n_basis, n_dims\] or
#'   3D array \[n_basis, n_dims, 1\]
#' @param sdgp Vector of marginal standard deviations \\[n_draws\\]
#' @param lscale Matrix of length scale parameters; \\[n_draws, 1\\] for
#'   isotropic GPs (brms default) or \\[n_draws, n_dims\\] for anisotropic
#'
#' @return List with validated/prepared components:
#'   - slambda: 2D matrix \[n_basis, n_dims\]
#'   - n_basis, n_dims, n_draws: dimension integers
#'   - is_isotropic: logical indicating single shared length scale
#'   - lscale_iso: vector \\[n_draws\\] if isotropic, NULL otherwise
#'   - lscale2: lscale^2 matrix for anisotropic computation
#'
#' @noRd
prepare_spd_inputs <- function(slambda, sdgp, lscale) {
  # Validate inputs
  checkmate::assert_array(
    slambda, min.d = 2, max.d = 3, any.missing = FALSE
  )
  checkmate::assert_numeric(sdgp, any.missing = FALSE, min.len = 1)
  checkmate::assert_matrix(
    lscale, any.missing = FALSE, all.missing = FALSE
  )

  # Handle 3D array - extract first eigenvalue matrix slice
  if (length(dim(slambda)) == 3) {
    slambda <- slambda[, , 1]
  }

  n_basis <- nrow(slambda)
  n_dims <- ncol(slambda)
  n_draws <- length(sdgp)
  n_lscale_dims <- ncol(lscale)

  # Validate row count matches draws
  if (nrow(lscale) != n_draws) {
    stop(insight::format_error(
      cli::format_inline(
        "Dimension mismatch: {.field lscale} has {nrow(lscale)} rows but {.field sdgp} has {n_draws} elements."
      )
    ))
  }

  # brms uses isotropic GPs by default (single shared length scale)
  # Check column count: 1 = isotropic, n_dims = anisotropic
  if (n_lscale_dims == 1L) {
    is_isotropic <- TRUE
    lscale_iso <- lscale[, 1]
    lscale2 <- lscale_iso^2
  } else if (n_lscale_dims == n_dims) {
    is_isotropic <- FALSE
    lscale_iso <- NULL
    lscale2 <- lscale^2
  } else {
    stop(insight::format_error(
      cli::format_inline(
        "Dimension mismatch: {.field lscale} has {n_lscale_dims} columns but expected 1 (isotropic) or {n_dims} (anisotropic)."
      )
    ))
  }

  list(
    slambda = slambda,
    lscale = lscale,
    n_basis = n_basis,
    n_dims = n_dims,
    n_draws = n_draws,
    is_isotropic = is_isotropic,
    lscale_iso = lscale_iso,
    lscale2 = lscale2
  )
}


#' Compute Spectral Power Density for Squared Exponential Kernel
#'
#' Computes spectral power density for approximate Gaussian processes
#' using the squared exponential (exp_quad) kernel. Follows brms
#' implementation exactly for consistency with Stan code generation.
#'
#' @param slambda Array of eigenvalues; matrix \[n_basis, n_dims\] or
#'   3D array \[n_basis, n_dims, 1\]
#' @param sdgp Vector of marginal standard deviations \\[n_draws\\]
#' @param lscale Matrix of length scale parameters; \\[n_draws, 1\\] for
#'   isotropic GPs (brms default) or \\[n_draws, n_dims\\] for anisotropic
#'
#' @return Matrix \\[n_draws, n_basis\\] of spectral power density values
#'
#' @details
#' Mathematical formula where D = n_dims:
#' sdgp^2 * sqrt(2*pi)^D * prod(lscale) * exp(-0.5 * sum(lscale^2 * slambda))
#'
#' @noRd
spd_gp_exp_quad <- function(slambda, sdgp, lscale) {
  p <- prepare_spd_inputs(slambda, sdgp, lscale)

  # Pre-compute constants
  constant_base <- sdgp^2 * sqrt(2 * pi)^p$n_dims

  # Pre-allocate result matrix
  out <- matrix(nrow = p$n_draws, ncol = p$n_basis)

  if (p$is_isotropic) {
    # Isotropic: single length scale for all dimensions (brms default)
    constant <- constant_base * p$lscale_iso^p$n_dims
    neg_half_lscale2 <- -0.5 * p$lscale2

    for (m in seq_len(p$n_basis)) {
      eigenval_sum <- sum(p$slambda[m, ]^2)
      out[, m] <- constant * exp(neg_half_lscale2 * eigenval_sum)
    }
  } else {
    # Anisotropic: different length scales per dimension
    constant <- constant_base * apply(p$lscale, 1, prod)
    neg_half_lscale2 <- -0.5 * p$lscale2

    for (m in seq_len(p$n_basis)) {
      slambda_expanded <- matrix(
        p$slambda[m, ]^2,
        nrow = p$n_draws,
        ncol = p$n_dims,
        byrow = TRUE
      )
      spd_term <- neg_half_lscale2 * slambda_expanded
      out[, m] <- constant * exp(rowSums(spd_term))
    }
  }

  out
}


#' Compute Spectral Power Density for Matern 3/2 Kernel
#'
#' Computes spectral power density for approximate Gaussian processes
#' using the Matern 3/2 kernel. Follows brms implementation exactly.
#'
#' @inheritParams spd_gp_exp_quad
#' @return Matrix \\[n_draws, n_basis\\] of spectral power density values
#'
#' @details
#' Mathematical formula where D = n_dims:
#' (3 + sum(lscale^2 * slambda))^(-(D+3)/2) with appropriate constants.
#'
#' @noRd
spd_gp_matern32 <- function(slambda, sdgp, lscale) {
  p <- prepare_spd_inputs(slambda, sdgp, lscale)

  # Pre-compute constants (following brms exactly)
  d <- p$n_dims
  gamma_term <- gamma((d + 3) / 2) * 3^(3 / 2)
  constant_base <- sdgp^2 * (2^d * pi^(d / 2) * gamma_term) / (0.5 * sqrt(pi))
  expo <- -(d + 3) / 2

  # Pre-allocate result
  out <- matrix(nrow = p$n_draws, ncol = p$n_basis)

  if (p$is_isotropic) {
    # Isotropic: single length scale for all dimensions
    constant <- constant_base * p$lscale_iso^d

    for (m in seq_len(p$n_basis)) {
      # One value per draw: each draw's length scale against the
      # basis function's squared eigenvalues.
      out[, m] <- constant * (3 + p$lscale2 * sum(p$slambda[m, ]^2))^expo
    }
  } else {
    # Anisotropic: different length scales per dimension
    constant <- constant_base * apply(p$lscale, 1, prod)

    for (m in seq_len(p$n_basis)) {
      slambda_expanded <- matrix(
        p$slambda[m, ]^2,
        nrow = p$n_draws,
        ncol = d,
        byrow = TRUE
      )
      eigenval_term <- rowSums(p$lscale2 * slambda_expanded)
      out[, m] <- constant * (3 + eigenval_term)^expo
    }
  }

  out
}


#' Compute Spectral Power Density for Matern 5/2 Kernel
#'
#' Computes spectral power density for approximate Gaussian processes
#' using the Matern 5/2 kernel. Follows brms implementation exactly.
#'
#' @inheritParams spd_gp_exp_quad
#' @return Matrix \\[n_draws, n_basis\\] of spectral power density values
#'
#' @details
#' Mathematical formula where D = n_dims:
#' (5 + sum(lscale^2 * slambda))^(-(D+5)/2) with appropriate constants.
#'
#' @noRd
spd_gp_matern52 <- function(slambda, sdgp, lscale) {
  p <- prepare_spd_inputs(slambda, sdgp, lscale)

  # Pre-compute constants (following brms exactly)
  d <- p$n_dims
  gamma_term <- gamma((d + 5) / 2) * 5^(5 / 2)
  constant_base <- sdgp^2 * (2^d * pi^(d / 2) * gamma_term) / (0.75 * sqrt(pi))
  expo <- -(d + 5) / 2

  # Pre-allocate result
  out <- matrix(nrow = p$n_draws, ncol = p$n_basis)

  if (p$is_isotropic) {
    # Isotropic: single length scale for all dimensions
    constant <- constant_base * p$lscale_iso^d

    for (m in seq_len(p$n_basis)) {
      # One value per draw, as for the Matern 3/2 kernel.
      out[, m] <- constant * (5 + p$lscale2 * sum(p$slambda[m, ]^2))^expo
    }
  } else {
    # Anisotropic: different length scales per dimension
    constant <- constant_base * apply(p$lscale, 1, prod)

    for (m in seq_len(p$n_basis)) {
      slambda_expanded <- matrix(
        p$slambda[m, ]^2,
        nrow = p$n_draws,
        ncol = d,
        byrow = TRUE
      )
      eigenval_term <- rowSums(p$lscale2 * slambda_expanded)
      out[, m] <- constant * (5 + eigenval_term)^expo
    }
  }

  out
}


#' Compute Spectral Power Density with Kernel Dispatch
#'
#' Kernel dispatcher function that computes spectral power density for approximate 
#' Gaussian processes by dispatching to appropriate kernel-specific
#' implementation. Returns sqrt(spd_result) for direct use in prediction.
#'
#' @param slambda Array of eigenvalues for spectral basis functions
#' @param sdgp Vector of marginal standard deviations across draws  
#' @param lscale Matrix of length scale parameters \[draws, dimensions\]
#' @param kernel Character string specifying kernel type: "exp_quad", 
#'   "matern32", or "matern52"
#'
#' @return Matrix \\[n_draws, n_basis\\] of sqrt(spectral_power_density)
#'
#' @noRd
compute_spd_vectorized <- function(slambda, sdgp, lscale, kernel) {
  # Validate kernel type
  checkmate::assert_choice(kernel, c("exp_quad", "matern32", "matern52"))
  
  # Dispatch to appropriate function
  spd_result <- switch(kernel,
    "exp_quad" = spd_gp_exp_quad(slambda, sdgp, lscale),
    "matern32" = spd_gp_matern32(slambda, sdgp, lscale), 
    "matern52" = spd_gp_matern52(slambda, sdgp, lscale),
    stop(insight::format_error(
      cli::format_inline(
        "Unsupported kernel type: {.field {kernel}}. Supported types: exp_quad, matern32, matern52."
      )
    ))
  )
  
  # Return sqrt for direct use in prediction formula
  sqrt(spd_result)
}


#' The ordered levels a monotonic term reads
#'
#' brms codes a monotonic variable with `D + 1` levels as `0..D`, its
#' first level `0` in every frame. A prediction frame holding only the
#' upper levels is therefore coded from `1` and is read as it stands:
#' shifting such a frame to start at `0` moved every row down a level.
#'
#' @param xmo_data The `Xmo_<i>` entry of the Stan data
#' @param xmo_name Its name, for the refusal
#' @param k_levels `D`, one less than the number of levels
#' @param n_obs Number of rows the predictor covers
#' @return Integer vector of levels in `0..D`
#'
#' @noRd
validate_monotonic_indices <- function(xmo_data, xmo_name, k_levels, n_obs) {
  checkmate::assert_integerish(xmo_data, any.missing = FALSE)
  checkmate::assert_count(k_levels, positive = TRUE)
  Xmo <- as.integer(xmo_data)
  if (length(Xmo) != n_obs) {
    stop_mvgam_fault(
      paste0("Monotonic variable '", xmo_name, "' does not cover the ",
             "prediction's rows."),
      paste0("It has ", length(Xmo), " values for ", n_obs, " rows.")
    )
  }
  if (any(Xmo < 0L | Xmo > k_levels)) {
    stop_mvgam_fault(
      paste0("Monotonic variable '", xmo_name, "' holds a level ",
             "outside its coding."),
      paste0("Levels run 0 to ", k_levels, "; found ", min(Xmo),
             " to ", max(Xmo), ".")
    )
  }
  Xmo
}


#' Remove the `_trend` the combined fit gives the trend's names
#'
#' The trend is a brms model of its own, whose data names are bare. The
#' combined program marks each of its parameters with `_trend` before
#' the index, as `b_trend[1]`, `r_1_1_trend[2]` or `sd_1_trend[1]`, or
#' at the end of an unindexed name.
#'
#' @param x Parameter names from the combined fit
#' @return `x` as the trend's brms model names them
#' @noRd
strip_trend_infix <- function(x) {
  sub("_trend(_|\\[|$)", "\\1", x)
}


#' The brms model one side of a fit is written as
#'
#' @param mvgam_fit A fitted `mvgam` object or a prefit
#' @param side `"obs"` or `"trend"`
#' @return A `brmsfit`, or `NULL` for a trend the fit does not have
#' @noRd
side_model <- function(mvgam_fit, side) {
  checkmate::assert_choice(side, c("obs", "trend"))
  if (identical(side, "trend")) mvgam_fit$trend_model else mvgam_fit$obs_model
}


#' The draws of one side's parameters, named as its brms model names them
#'
#' The observation model's parameters, a distributional parameter's
#' included, keep the names its brms model gives them. The trend's
#' carry `_trend`, which its own brms model does not write.
#'
#' @param mvgam_fit A fitted `mvgam` object
#' @param full_draws The fit's draws matrix, every parameter, at the
#'   chosen draws
#' @param side `"obs"` or `"trend"`
#' @return A draws matrix of that side's parameters
#' @noRd
side_draws <- function(mvgam_fit, full_draws, side) {
  checkmate::assert_class(full_draws, "draws_matrix")
  out <- full_draws[, side_parameters(mvgam_fit, side), drop = FALSE]
  if (identical(side, "trend")) {
    colnames(out) <- strip_trend_infix(colnames(out))
  }
  out
}


#' Linear predictor of one component of a fitted model
#'
#' The observation model's mean, one of its distributional parameters,
#' or the trend's mean, on the link scale. The observation model's
#' parameters keep the names brms gave them, which the composer reads
#' under each predictor's own suffix. The trend's are renamed to the
#' bare names its own brms model uses.
#'
#' @param mvgam_fit A fitted `mvgam` object
#' @param newdata Data frame to predict for
#' @param component `"obs"`, `"trend"` or the name of a distributional
#'   parameter that has a formula of its own
#' @param resp The response's key, or `NULL`
#' @param draw_ids Draw indices to use, or `NULL` for all
#' @param re_formula,allow_new_levels,sample_new_levels As
#'   `validate_group_level_args()` takes them
#' @return A `[ndraws x nobs]` matrix, or a list of them named by
#'   response
#' @noRd
extract_component_linpred <- function(mvgam_fit, newdata, component = "obs",
                                      resp = NULL, draw_ids = NULL,
                                      re_formula = NULL,
                                      allow_new_levels = FALSE,
                                      sample_new_levels = "uncertainty") {
  checkmate::assert_class(mvgam_fit, "mvgam")
  checkmate::assert_data_frame(newdata, min.rows = 1)
  checkmate::assert_string(component)
  checkmate::assert_string(resp, null.ok = TRUE)
  checkmate::assert_integerish(draw_ids, lower = 1, null.ok = TRUE,
                               any.missing = FALSE)
  # Stamp the empty-obs placeholder onto user-supplied newdata
  # when the fit needed one. brms's `validate_data()` would
  # otherwise reject the frame for missing the column.
  newdata <- ensure_obs_placeholder(newdata, mvgam_fit)
  if (!is.null(mvgam_fit$trend_metadata$levels)) {
    validate_prediction_factor_levels(newdata, mvgam_fit$trend_metadata)
  }

  is_trend <- identical(component, "trend")
  side <- if (is_trend) "trend" else "obs"
  brms_model <- side_model(mvgam_fit, side)
  if (is.null(brms_model)) {
    if (is_trend) {
      stop(insight::format_error(c(
        "This 'mvgam' fit has no latent trend to predict from.",
        i = paste0(
          "A trend is declared through 'trend_formula', for example ",
          "trend_formula = ~ AR()."
        )
      )), call. = FALSE)
    }
    stop_mvgam_fault(
      "This 'mvgam' fit carries no observation model.",
      "Every fit is built with one."
    )
  }

  # Draws arrive already chosen. The boundary a user called through
  # turned their count into indices, and this extraction reads the
  # same rows as every extraction it is combined with.
  full_draws <- subset_draws_rows(posterior::as_draws_matrix(mvgam_fit$fit),
                                  draw_ids = draw_ids)
  component_draws <- side_draws(mvgam_fit, full_draws, side)

  if (is_trend && isTRUE(mvgam_fit$trend_metadata$has_by_lv)) {
    return(compose_by_lv_trend_linpred(
      mvgam_fit = mvgam_fit,
      newdata = newdata,
      full_draws = full_draws,
      trend_draws = component_draws,
      re_formula = re_formula,
      allow_new_levels = allow_new_levels,
      sample_new_levels = sample_new_levels,
      resp = resp
    ))
  }

  # A trend shared by every response is a model with one response.
  use_resp <- if (!is_trend || brms::is.mvbrmsformula(brms_model$formula)) {
    resp
  }
  dpar <- if (!component %in% c("obs", "trend")) component
  prep <- prepare_linpred_data(
    draws = component_draws,
    brmsfit = brms_model,
    newdata = newdata,
    re_formula = re_formula,
    allow_new_levels = allow_new_levels,
    sample_new_levels = sample_new_levels
  )
  linpred <- extract_linpred_from_prep(prep, resp = use_resp, dpar = dpar)

  # A `draws_matrix` keeps its class through subsetting and
  # arithmetic, and a predictor composed from one would carry it or
  # not depending on the terms the model has. The class is dropped on
  # the way out, where the predictor stops being posterior draws and
  # becomes a value: an S4 slot declared to hold a plain matrix
  # rejects the classed one outright.
  if (is.list(linpred) && !is.matrix(linpred)) {
    return(lapply(linpred, as_plain_matrix))
  }
  as_plain_matrix(linpred)
}


#' The trend's mean on a fit whose trend runs at the factor grain
#'
#' Under `by = lv_axis()` the trend's brms model is written over
#' `(time, .trend)`, one row per latent factor, and Stan computes
#' `trend[t, s] = dot(Z[s, ], lv_trend[t, ] + mu_factor[t, ])`. This
#' composes the `dot(Z, mu_factor)` half: `mu_factor` is the trend
#' model's predictor on a grid holding each row of `newdata` once per
#' factor, and `Z` is the loadings drawn beside it. What comes back is
#' `linpred[d, i] = sum_k Z[d, series_i, k] * mu_factor[d, i, k]`,
#' mirroring `brms::posterior_linpred(incl_autocor = FALSE)`. The
#' `lv_trend` half is the latent state, which reaches a prediction as
#' the marginal envelope `sample_process_errors()` adds, and per draw
#' through `extract_trend_latent_states()` on the hindcast and forecast
#' paths.
#'
#' @param mvgam_fit A fitted `mvgam` object whose trend has `by =
#'   lv_axis()`
#' @param newdata Data frame to predict for, one row per `(time,
#'   series)`
#' @param full_draws The fit's draws, every parameter, at the chosen
#'   draws
#' @param trend_draws The same draws of the trend's parameters, named
#'   as the trend's brms model names them
#' @param re_formula,allow_new_levels,sample_new_levels As
#'   `validate_group_level_args()` takes them
#' @param resp The response's key on a model with several, or `NULL`
#' @return A `[ndraws x nrow(newdata)]` matrix
#' @noRd
compose_by_lv_trend_linpred <- function(mvgam_fit, newdata, full_draws,
                                        trend_draws, re_formula,
                                        allow_new_levels,
                                        sample_new_levels,
                                        resp = NULL) {
  checkmate::assert_class(mvgam_fit, "mvgam")
  checkmate::assert_data_frame(newdata, min.rows = 1L)
  checkmate::assert_class(full_draws, "draws_matrix")
  checkmate::assert_class(trend_draws, "draws_matrix")
  checkmate::assert_true(nrow(full_draws) == nrow(trend_draws))

  n_lv <- as.integer(mvgam_fit$trend_metadata$n_lv_for_grain)
  n_series <- mvgam_axes(mvgam_fit)$series$n
  if (length(n_lv) != 1L || length(n_series) != 1L) {
    stop_mvgam_fault(
      "A 'by = lv_axis()' fit does not record its factor and series counts.",
      "Both are needed to compose its trend prediction."
    )
  }

  obs_struct <- get_observation_structure(mvgam_fit, newdata = newdata,
                                          resp = resp)
  s_idx <- as.integer(obs_struct$series_int)
  # The frame's series were checked against the fit's before this, and
  # every row has a trend column.
  if (anyNA(s_idx) || any(s_idx < 1L | s_idx > n_series)) {
    stop_mvgam_fault(
      "A prediction row was placed on no trend column.",
      paste0("Series indices run 1 to ", n_series, ".")
    )
  }
  n_rows <- nrow(newdata)

  # Per-row by per-factor grid: each newdata row repeated for each
  # factor. A grid collapsed to one row per (time, .trend) would drop
  # the rows of a marginaleffects grid that share a time and differ
  # in a covariate. `expand_grid()` iterates `.trend` fastest: each
  # consecutive block of `n_lv` columns in `mu_factor_long` belongs to
  # one newdata row, which `byrow = TRUE` keeps in the reshape below.
  trend_vars <- mvgam_fit$trend_metadata$covariates %||% character(0)
  attach_cols <- intersect(unique(c(trend_vars, "time")),
                            colnames(newdata))
  lv_newdata <- tidyr::expand_grid(
    .row_id = seq_len(n_rows),
    .trend  = factor(seq_len(n_lv))
  )
  for (col in attach_cols) {
    lv_newdata[[col]] <- newdata[[col]][lv_newdata$.row_id]
  }

  prep <- prepare_linpred_data(
    draws = trend_draws, brmsfit = side_model(mvgam_fit, "trend"),
    newdata = lv_newdata, re_formula = re_formula,
    allow_new_levels = allow_new_levels,
    sample_new_levels = sample_new_levels
  )
  mu_factor_long <- extract_linpred_from_prep(prep, resp = NULL)
  if (is.list(mu_factor_long) && !is.matrix(mu_factor_long)) {
    stop(insight::format_error(
      "'by = lv_axis()' does not support a multivariate trend formula."
    ), call. = FALSE)
  }
  checkmate::assert_matrix(mu_factor_long, ncols = n_rows * n_lv)

  # `mu_factor` is the per-factor smooth over the latent axis, so
  # it belongs to the `Z` the model sampled. Stan folds it inside
  # the projection for these fits, and the basis is named here
  # rather than left to a default so the two halves cannot drift.
  Z_arr <- extract_Z_loadings(full_draws,
                              n_obs_series = n_series, n_lv = n_lv,
                              basis = "model")

  ndraws_used <- nrow(full_draws)
  linpred_mat <- matrix(NA_real_, nrow = ndraws_used, ncol = n_rows)
  for (d in seq_len(ndraws_used)) {
    mu_d <- matrix(mu_factor_long[d, ],
                   nrow = n_rows, ncol = n_lv, byrow = TRUE)
    Z_d <- matrix(Z_arr[d, , ], nrow = n_series, ncol = n_lv)
    # Per row i: dot(Z_d[series_i, :], mu_d[i, :])
    linpred_mat[d, ] <- rowSums(Z_d[s_idx, , drop = FALSE] * mu_d)
  }

  linpred_mat
}


#' Choose the prediction surface a diagnostic should read
#'
#' A residual or a posterior predictive check compares a prediction
#' against the observation that was actually recorded, so in sample it
#' reads the latent state the model inferred at that time rather than
#' a fresh draw of the trend. That is the state `hindcast()` returns,
#' and reading it keeps every diagnostic describing one series.
#'
#' Given `newdata` the fit never saw, there is no such state to read
#' and the marginal surface is what remains, carrying the trend's
#' process uncertainty as it should. The exception is a check weighted
#' by importance ratios: those are built from the likelihood, which is
#' conditional, so the draws they reweight have to be conditional too
#' whatever data they cover.
#'
#' The caller states whether the rows are the ones the fit saw, rather
#' than handing over a frame to be tested for emptiness. A caller that
#' has already substituted the training frame for a `NULL` `newdata`
#' would otherwise ask for the marginal surface while describing the
#' data the model was fitted to, which is how one panel came to plot a
#' conditional residual against a marginal fitted value.
#'
#' @param args Argument list destined for a `posterior_*` method
#' @param in_sample Whether the rows are the ones the fit was given
#' @param weighted Whether the result will be reweighted by importance
#'   ratios
#' @return `args`, with `incl_autocor` stamped on when the conditional
#'   surface applies and the caller has not named it already
#'
#' @noRd
diagnostic_surface_args <- function(args, in_sample, weighted = FALSE) {
  checkmate::assert_list(args)
  checkmate::assert_flag(in_sample)
  checkmate::assert_flag(weighted)
  if ("incl_autocor" %in% names(args)) {
    return(args)
  }
  if (in_sample || weighted) {
    args$incl_autocor <- TRUE
  }
  args
}


#' Which trend column each prediction row reads
#'
#' Ordinarily a row names its own series and the observation
#' structure numbers it. A wide `mvbf()` frame cannot: a row there is
#' one occasion carrying every response, so its series column is a
#' single constant and numbering it puts all sixty rows on series one.
#' The series is a property of the `(row, response)` pair, and the
#' axis record holds the response half of it, so the response the
#' caller was scoped to picks the column and the row supplies the
#' time.
#'
#' Refusing when no response is named is what keeps this safe to
#' extend: a new reader of the observation structure that forgets to
#' scope itself stops here, rather than reading the first response's
#' state for every response as three readers once did.
#'
#' @param object A fitted `mvgam` object
#' @param series_int The per-row series index the frame's own series
#'   values give
#' @param resp The response this prediction is scoped to, or `NULL`
#' @return An integer, one per prediction row, indexing the columns of
#'   `trend[t, s]`
#'
#' @noRd
trend_series_index <- function(object, series_int, resp) {
  if (!is_response_keyed(object)) {
    return(series_int)
  }
  resolve_resp(object, resp, required = TRUE)
  col <- response_series_index(mvgam_axes(object)$series$levels, resp)
  rep(col, length(series_int))
}


#' Extract per-observation latent trend state draws
#'
#' Pulls the `trend\[t, s\]` posterior draws from the stanfit and
#' aligns them to `newdata` rows via `(time, series)` mapping. Returns
#' a ``\\[ndraws x nobs\\]`` matrix or NULL if the fit has no latent trend
#' state.
#'
#' @noRd
extract_trend_latent_states <- function(mvgam_fit, newdata, full_draws,
                                        resp = NULL) {
  checkmate::assert_class(mvgam_fit, "mvgam")
  checkmate::assert_data_frame(newdata, min.rows = 1)
  checkmate::assert_matrix(full_draws, min.rows = 1)
  checkmate::assert_string(resp, null.ok = TRUE)

  par_names <- colnames(full_draws)
  trend_cols <- grep("^trend\\[", par_names, value = TRUE)
  if (length(trend_cols) == 0L) {
    return(NULL)
  }

  N_time_trend <- mvgam_fit$standata$N_time_trend
  N_series_trend <- mvgam_fit$standata$N_series_trend
  if (is.null(N_time_trend) || is.null(N_series_trend)) {
    stop(insight::format_error(c(
      "Cannot align latent trend state without N_time_trend / N_series_trend.",
      i = "This indicates a malformed mvgam fit."
    )))
  }

  # Map newdata rows to columns of the fitted trend matrix. The lookup
  # runs on the raw time values against the grid the model was fitted
  # on, because the observation structure renumbers time from one
  # within whatever frame it is handed: a frame holding only the later
  # half of a series would otherwise read the state of the earlier
  # half, silently and with the right shape. Working from the raw
  # values also makes a time the fit never saw fall out as `NA`, which
  # is what the marginal substitution below keys on.
  obs_struct <- get_observation_structure(mvgam_fit, newdata = newdata,
                                          resp = resp)
  s_idx <- obs_struct$series_int
  time_var <- mvgam_fit$trend_metadata$variables$time_var %||% "time"
  train_data <- mvgam_training_data(mvgam_fit)
  raw_t_idx <- if (time_var %in% names(newdata) &&
                     time_var %in% names(train_data)) {
    match(newdata[[time_var]], sort(unique(train_data[[time_var]])))
  } else {
    NULL
  }
  # A closure-unit family predicts at the unit grain rather than per
  # newdata row, so the raw lookup only applies when it covers the
  # same rows the observation structure does. Otherwise the position
  # that structure assigned is the only alignment available.
  t_idx <- if (!is.null(raw_t_idx) &&
                 length(raw_t_idx) == length(s_idx)) {
    raw_t_idx
  } else {
    match(obs_struct$time, obs_struct$unique_times)
  }

  if (any(s_idx < 1L | s_idx > N_series_trend)) {
    stop(insight::format_error(
      "newdata contains series indices outside the fitted model's range."
    ))
  }

  # Unseen times: substitute the per-series posterior mean of the
  # latent state (averaged across the training time grid) for any
  # newdata row whose time is outside the fitted grid. This is the
  # documented marginal-MC semantic of the posterior_*.mvgam
  # surfaces (see architecture-decisions.md): the prediction
  # primitives integrate over the trend dynamics and treat the
  # latent state as stationary at any prediction time, matching
  # the marginaleffects / brms::predict convention for models with
  # correlated residuals. For state-aware out-of-sample prediction
  # (latent state extrapolated forward via the trend kernel) use
  # `forecast.mvgam()` instead.
  has_unseen <- any(is.na(t_idx))

  series_marginal <- if (has_unseen) {
    out <- matrix(NA_real_, nrow = nrow(full_draws), ncol = N_series_trend)
    for (s in seq_len(N_series_trend)) {
      cols_s <- paste0("trend[", seq_len(N_time_trend), ",", s, "]")
      cols_s <- intersect(cols_s, par_names)
      if (length(cols_s) > 0L) {
        out[, s] <- rowMeans(full_draws[, cols_s, drop = FALSE])
      }
    }
    out
  } else {
    NULL
  }

  ndraws <- nrow(full_draws)
  nobs <- length(t_idx)
  # Every cell is named and located in one pass. Naming and locating
  # them one row at a time scanned the whole parameter vector per
  # observation, so the work grew with the product of the two rather
  # than with their sum.
  seen <- !is.na(t_idx)
  wanted <- paste0("trend[", t_idx, ",", s_idx, "]")
  col <- match(wanted, par_names)
  absent <- seen & is.na(col)
  if (any(absent)) {
    stop(insight::format_error(c(
      "Latent trend state column missing from posterior draws.",
      x = cli::format_inline(
        "Missing: {.val {wanted[which(absent)[1L]]}}."
      ),
      i = paste0(
        "Stan output should contain trend[t, s] for every (t, s) ",
        "pair covered by the fit."
      )
    )))
  }

  latent_mat <- matrix(NA_real_, nrow = ndraws, ncol = nobs)
  if (any(seen)) {
    latent_mat[, seen] <- full_draws[, col[seen], drop = FALSE]
  }
  if (any(!seen)) {
    latent_mat[, !seen] <- series_marginal[, s_idx[!seen], drop = FALSE]
  }
  latent_mat
}
