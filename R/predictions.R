#' Detect Gaussian Process Terms in Prep Object
#'
#' Searches a brmsprep object for Gaussian Process (GP) terms and
#'   validates that complete GP data structures exist. Only supports
#'   approximate GP using Hilbert space approximation (gp(x, k=5)
#'   syntax in brms).
#'
#' @param prep A brmsprep object from prepare_predictions()
#'
#' @return NULL if no GP terms found, otherwise a list with:
#'   - suffixes: character vector of GP term identifiers (e.g., "1",
#'     "trend_1")
#'   - type: always "approximate" (mvgam only supports approximate GP)
#'   - n_terms: integer count of valid GP terms
#'
#' @details
#' GP terms are identified by Xgp_* matrices in prep$sdata. Each valid
#'   GP term must have complete data structures:
#' - Xgp_<suffix>: basis function evaluations (N × k matrix)
#' - slambda_<suffix>: eigenvalues for spectral basis functions (k × dims array)
#' - NBgp_<suffix>: number of basis functions (scalar)
#'
#' And corresponding parameters in prep$draws:
#' - zgp_<suffix>: standard normal draws (ndraws × k matrix)
#' - sdgp_<suffix>: marginal standard deviations (ndraws × k matrix)  
#' - lscale_<suffix>: length-scale parameters (ndraws × k matrix)
#'
#' Full GP (gp(x) without k) is not supported and will not be detected.
#'
#' Terms with incomplete data structures are silently skipped. If all
#'   candidate GP terms are incomplete, returns NULL. This allows
#'   graceful handling of partially specified models.
#'
#' @noRd
detect_gp_terms <- function(prep) {
  checkmate::assert_class(prep, "brmsprep")
  checkmate::assert_list(prep$sdata, names = "named")

  # Validate dpars if present
  if ("dpars" %in% names(prep)) {
    checkmate::assert_list(prep$dpars, names = "named")
  }

  # Search for Xgp_* matrices indicating GP terms
  gp_candidates <- grep("^Xgp_", names(prep$sdata), value = TRUE)

  if (length(gp_candidates) == 0) {
    return(NULL)
  }

  # Extract suffixes (e.g., "1" from "Xgp_1", "trend_1" from "Xgp_trend_1")
  suffixes <- sub("^Xgp_", "", gp_candidates)

  # Validate each candidate has complete GP structure
  valid_suffixes <- character()

  for (suffix in suffixes) {
    # Check required sdata components
    required_sdata <- c(
      paste0("Xgp_", suffix),
      paste0("slambda_", suffix),
      paste0("NBgp_", suffix)
    )

    if (!all(required_sdata %in% names(prep$sdata))) {
      missing_sdata <- setdiff(required_sdata, names(prep$sdata))
      next
    }

    # Check required parameter components in draws matrix
    if (!"draws" %in% names(prep)) {
      next
    }

    draws_names <- colnames(prep$draws)
    
    # Check for bracket notation GP parameters
    has_zgp <- any(grepl(paste0("^zgp_", suffix, "\\["), draws_names))
    has_sdgp <- any(grepl(paste0("^sdgp_", suffix, "\\["), draws_names))
    
    # Check for lscale with multiple possible patterns
    has_lscale <- any(grepl(paste0("^lscale_", suffix, "\\["), draws_names))
    if (!has_lscale) {
      # Try alternative patterns for lscale
      alt_patterns <- c(
        paste0("^lsd_", suffix, "\\["),
        paste0("^lengthscale_", suffix, "\\["),
        paste0("^ls_", suffix, "\\[")
      )
      has_lscale <- any(sapply(alt_patterns, function(p) any(grepl(p, draws_names))))
    }
    
    if (!has_zgp || !has_sdgp || !has_lscale) {
      missing <- character()
      if (!has_zgp) missing <- c(missing, paste0("zgp_", suffix, "[*]"))
      if (!has_sdgp) missing <- c(missing, paste0("sdgp_", suffix, "[*]"))
      if (!has_lscale) missing <- c(missing, paste0("lscale_", suffix, "[*]"))
      
      # Add debug output to investigate missing lscale
      if (!has_lscale) {
        all_scale_params <- grep("scale|lscale|lsd|ls", draws_names, value = TRUE, ignore.case = TRUE)
      }
      
      next
    }

    # Valid complete GP term found
    valid_suffixes <- c(valid_suffixes, suffix)
  }

  if (length(valid_suffixes) == 0) {
    return(NULL)
  }

  list(
    suffixes = valid_suffixes,
    type = "approximate",
    n_terms = length(valid_suffixes)
  )
}


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
      "Basis function mismatch: {.field Xgp} has {n_basis} basis functions ",
      "but {.field zgp} has {ncol(zgp)} coefficients."
    ))
  }
  
  if (length(sdgp) != n_draws) {
    stop(insight::format_error(
      "Draw count mismatch: {.field zgp} has {n_draws} draws ",
      "but {.field sdgp} has {length(sdgp)} elements."
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
#' @param slambda Array of eigenvalues; matrix [n_basis, n_dims] or
#'   3D array [n_basis, n_dims, 1]
#' @param sdgp Vector of marginal standard deviations [n_draws]
#' @param lscale Matrix of length scale parameters; [n_draws, 1] for
#'   isotropic GPs (brms default) or [n_draws, n_dims] for anisotropic
#'
#' @return List with validated/prepared components:
#'   - slambda: 2D matrix [n_basis, n_dims]
#'   - n_basis, n_dims, n_draws: dimension integers
#'   - is_isotropic: logical indicating single shared length scale
#'   - lscale_iso: vector [n_draws] if isotropic, NULL otherwise
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
      "Dimension mismatch: {.field lscale} has {nrow(lscale)} rows ",
      "but {.field sdgp} has {n_draws} elements."
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
      "Dimension mismatch: {.field lscale} has {n_lscale_dims} ",
      "columns but expected 1 (isotropic) or {n_dims} (anisotropic)."
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
#' @param slambda Array of eigenvalues; matrix [n_basis, n_dims] or
#'   3D array [n_basis, n_dims, 1]
#' @param sdgp Vector of marginal standard deviations [n_draws]
#' @param lscale Matrix of length scale parameters; [n_draws, 1] for
#'   isotropic GPs (brms default) or [n_draws, n_dims] for anisotropic
#'
#' @return Matrix [n_draws, n_basis] of spectral power density values
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
#' @return Matrix [n_draws, n_basis] of spectral power density values
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
      eigenval_sum <- sum(p$lscale2 * p$slambda[m, ]^2)
      out[, m] <- constant * (3 + eigenval_sum)^expo
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
#' @return Matrix [n_draws, n_basis] of spectral power density values
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
      eigenval_sum <- sum(p$lscale2 * p$slambda[m, ]^2)
      out[, m] <- constant * (5 + eigenval_sum)^expo
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
#' @param lscale Matrix of length scale parameters [draws, dimensions]
#' @param kernel Character string specifying kernel type: "exp_quad", 
#'   "matern32", or "matern52"
#'
#' @return Matrix [n_draws, n_basis] of sqrt(spectral_power_density)
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
      "Unsupported kernel type: {.field {kernel}}. ",
      "Supported types: exp_quad, matern32, matern52."
    ))
  )
  
  # Return sqrt for direct use in prediction formula
  sqrt(spd_result)
}


#' Detect Kernel Type from brms Formula
#'
#' Extracts the kernel/covariance type from gp() terms in a brms formula.
#' Caches the result in the prep object to avoid repeated parsing.
#'
#' @param prep A brmsprep object from prepare_predictions() 
#' @param brmsfit A brmsfit object containing the original formula
#'
#' @return Character string: "exp_quad", "matern32", or "matern52"
#'
#' @details
#' Searches the formula for gp() terms and extracts the cov parameter.
#' Default is "exp_quad" if no cov parameter specified.
#' 
#' Pattern: gp(x1, x2, cov = "matern32") extracts "matern32"
#' Pattern: gp(x1, x2) defaults to "exp_quad"
#'
#' @noRd
detect_gp_kernel <- function(prep, brmsfit) {
  # Check if already cached
  if (!is.null(prep$gp_kernel)) {
    return(prep$gp_kernel)
  }
  
  # Extract formula string
  formula_str <- deparse(brmsfit$formula$formula, width.cutoff = 500L)
  formula_str <- paste(formula_str, collapse = " ")
  
  # Search for gp() terms with cov parameter
  # Pattern: cov\s*=\s*["']([^"']+)
  cov_match <- regmatches(
    formula_str, 
    regexec('cov\\s*=\\s*["\']([^"\']+)', formula_str)
  )
  
  if (length(cov_match[[1]]) > 1) {
    kernel <- cov_match[[1]][2]  # Extract captured group
    
    # Validate and normalize
    kernel <- switch(kernel,
      "exp_quad" = "exp_quad",
      "exponential_quadratic" = "exp_quad",
      "squared_exponential" = "exp_quad", 
      "rbf" = "exp_quad",
      "matern32" = "matern32",
      "matern_32" = "matern32",
      "matern3/2" = "matern32",
      "matern52" = "matern52", 
      "matern_52" = "matern52",
      "matern5/2" = "matern52",
      stop(insight::format_error(
        "Unsupported GP kernel: {.field {kernel}}. ",
        "Supported kernels: exp_quad, matern32, matern52."
      ))
    )
  } else {
    # Default kernel
    kernel <- "exp_quad"
  }
  
  # Cache in prep object
  prep$gp_kernel <- kernel
  
  kernel
}


#' Detect Nonlinear Formulas in Formula Object
#'
#' Checks if a brmsformula or mvgam model uses nonlinear formulas
#'   (`nl = TRUE`). Nonlinear formulas evaluate R expressions
#'   (e.g., `b1 * exp(b2 * x)`) rather than constructing linear
#'   predictors via matrix multiplication.
#'
#' @param object A brmsformula, mvbrmsformula, brmsfit, or mvgam
#'   object. For fitted objects, extracts formula automatically.
#'
#' @return Logical; TRUE if model uses nonlinear formulas, FALSE
#'   otherwise
#'
#' @details
#' brms handles nonlinear formulas fundamentally differently from
#'   linear predictors:
#' - Linear models: mu = X %*% beta (matrix multiplication)
#' - Nonlinear models: mu = eval(expression, parameters)
#'
#' The prediction system extracts and reconstructs linear predictors
#'   from components. For nonlinear models, prep$dpars$mu is already
#'   fully evaluated and cannot be reconstructed or subset.
#'
#' Detection pattern: Checks the `nl` attribute on formula$formula
#'   set by bf(..., nl = TRUE). This is brms's definitive indicator
#'   for nonlinear models. Additional validation ensures pforms
#'   (parameter formulas) exist, as nonlinear models must have
#'   parameter sub-formulas.
#'
#' For multivariate formulas, checks each response's formula
#'   separately and returns TRUE if ANY response uses nl = TRUE.
#'
#' @noRd
has_nlpars <- function(object) {
  # Validate input type
  checkmate::assert(
    checkmate::check_class(object, "mvgam"),
    checkmate::check_class(object, "brmsfit"),
    checkmate::check_class(object, "brmsformula"),
    checkmate::check_class(object, "mvbrmsformula"),
    combine = "or"
  )

  # Extract formula from fitted object if needed
  formula_obj <- if (inherits(object, c("mvgam", "brmsfit"))) {
    if (is.null(object$formula)) {
      stop(insight::format_error(
        "Object missing {.field formula} component."
      ))
    }
    object$formula
  } else {
    object
  }

  # Handle multivariate formulas (check each response)
  if (inherits(formula_obj, "mvbrmsformula")) {
    if (!is.null(formula_obj$forms)) {
      for (form in formula_obj$forms) {
        # Check nl attribute on each response formula
        nl_attr <- attr(form$formula, "nl")
        has_pforms <- !is.null(form$pforms) &&
                      length(form$pforms) > 0

        if (isTRUE(nl_attr) && has_pforms) {
          return(TRUE)
        }
      }
    }
    return(FALSE)
  }

  # Check nl attribute on formula (definitive indicator)
  nl_attr <- attr(formula_obj$formula, "nl")

  # Validate pforms exist (nonlinear models must have parameter
  # formulas)
  has_pforms <- !is.null(formula_obj$pforms) &&
                length(formula_obj$pforms) > 0

  return(isTRUE(nl_attr) && has_pforms)
}


#' Extract Linear Predictor for Nonlinear Formula Models
#'
#' Extracts pre-computed linear predictor (mu) from prep$dpars for
#'   models with nonlinear formulas (nl = TRUE). Unlike linear models
#'   where predictors are reconstructed from components, nonlinear
#'   models evaluate R expressions directly.
#'
#' @param prep A brmsprep object from prepare_predictions() with dpars
#'   component generated by compute_nonlinear_dpars()
#' @param resp Character string specifying response name for
#'   multivariate models. If NULL, returns all responses (univariate
#'   returns single matrix, multivariate returns named list).
#'
#' @return Matrix [ndraws × nobs] for univariate or single response.
#'   Named list of matrices for multivariate models when resp = NULL.
#'
#' @details
#' **IMPORTANT LIMITATION**: Parameter subsetting is not supported for
#'   nonlinear models. The returned mu reflects all parameters used
#'   during prep object creation, even when using mock_stanfit
#'   objects with parameter subsets.
#'
#' Nonlinear formulas evaluate arbitrary R expressions combining
#'   nonlinear parameters (nlpars) and covariates. Example:
#'   `y ~ b1 * exp(b2 * x)` where b1 and b2 are nlpars with their own
#'   linear predictors.
#'
#' Because formulas can be any R expression (exponentials, products,
#'   custom functions), results cannot be decomposed into additive
#'   components. Instead, the complete expression is evaluated during
#'   prep creation via compute_nonlinear_dpars(), and this function
#'   extracts the pre-computed result.
#'
#' @noRd
extract_linpred_nonlinear <- function(prep, resp = NULL) {
  # Validate inputs
  checkmate::assert_class(prep, "brmsprep")
  checkmate::assert_string(resp, null.ok = TRUE)

  if (!"dpars" %in% names(prep)) {
    stop(insight::format_error(
      "Nonlinear formula models require {.field dpars} component."
    ))
  }

  if (!"mu" %in% names(prep$dpars)) {
    stop(insight::format_error(
      "Nonlinear formula prep missing {.field mu} in dpars."
    ))
  }

  mu <- prep$dpars$mu

  # Validate mu is matrix with correct structure
  if (!is.matrix(mu)) {
    stop(insight::format_error(
      "{.field mu} must be a matrix [ndraws × nobs]."
    ))
  }

  if (ncol(mu) != prep$nobs) {
    stop(insight::format_error(
      "{.field mu} has {ncol(mu)} columns but expected ",
      "{prep$nobs} observations."
    ))
  }

  # Check for multivariate structure
  is_mv <- brms::is.mvbrmsformula(prep$formula)

  if (!is_mv) {
    # Univariate model
    if (!is.null(resp)) {
      stop(insight::format_error(
        "{.field resp} only for multivariate models."
      ))
    }
    return(mu)
  }

  # Multivariate model - split mu by response
  if (!"responses" %in% names(prep$formula)) {
    stop(insight::format_error(
      "Multivariate formula must contain {.field responses}."
    ))
  }

  response_names <- prep$formula$responses

  if (is.null(response_names) || length(response_names) == 0) {
    stop(insight::format_error(
      "Multivariate formula has no response names."
    ))
  }

  # Extract nobs per response using brms N_<response> pattern
  nobs_list <- lapply(response_names, function(r) {
    nobs_name <- paste0("N_", r)
    if (!nobs_name %in% names(prep$sdata)) {
      stop(insight::format_error(
        "Missing {.field {nobs_name}} in prep$sdata."
      ))
    }
    prep$sdata[[nobs_name]]
  })
  names(nobs_list) <- response_names

  # Split mu matrix by response (column-wise)
  mu_list <- list()
  col_start <- 1

  for (r in response_names) {
    n_r <- nobs_list[[r]]
    col_end <- col_start + n_r - 1
    mu_list[[r]] <- mu[, col_start:col_end, drop = FALSE]
    col_start <- col_end + 1
  }

  # Validate column count matches
  if (col_start - 1 != ncol(mu)) {
    stop(insight::format_error(
      "Column count mismatch: split {col_start - 1} columns but ",
      "{.field mu} has {ncol(mu)} columns."
    ))
  }

  # Return single response if specified
  if (!is.null(resp)) {
    if (!resp %in% response_names) {
      available <- paste(response_names, collapse = ", ")
      stop(insight::format_error(
        "Response {.field {resp}} not found. Available: {available}."
      ))
    }
    return(mu_list[[resp]])
  }

  mu_list
}


#' Extract and Unstandardize Smooth Coefficients
#'
#' Extracts smooth coefficients from posterior draws, handling both
#' standardized (zs_* with sds_*) and unstandardized (s_*) forms
#' following brms conventions.
#'
#' @param draws_mat Matrix of posterior draws [n_draws × n_params]
#' @param smooth_label Base name for smooth term
#'   (e.g., "1_1" from Zs_count_1_1)
#' @param resp Response name for multivariate models (NULL for univariate)
#' @param n_basis Expected number of basis functions
#'
#' @return Matrix [n_draws × n_basis] of smooth coefficients, or NULL if
#'   no matching coefficients found (allows silent skip for optional smooths)
#'
#' @details
#' Follows brms standardization pattern from extract_draws.R:
#'
#' - **Standardized smooths**: Uses zs_* parameters (standardized to
#'   N(0,1)) with sds_* scale parameter (single scalar per smooth).
#'   Unstandardization: sm_coef = zs * sds via R broadcasting.
#'
#' - **Unstandardized smooths**: Uses s_* parameters directly (no sds).
#'   Common when by-variable is a factor.
#'
#' The sds parameter is a SCALAR (one value per smooth term) that
#' implements hierarchical prior: β_j ~ N(0, σ) for all j basis
#' functions. R's recycling broadcasts sds across all coefficients.
#'
#' Validate Monotonic Effect Indices
#'
#' Validates that monotonic effect (mo()) indices are properly formatted
#' and within valid range for brms models.
#'
#' @param xmo_data Integer vector of ordinal level indices from prep$sdata
#' @param xmo_name Name of the monotonic design matrix (for error messages)
#' @param k_levels Number of ordinal levels in the monotonic effect
#' @param n_obs Expected number of observations
#'
#' @return Validated integer vector of monotonic indices
#'
#' @details
#' brms uses 0-based indexing for monotonic effects, so valid indices
#' range from 0 to k_levels-1.
#'
#' @noRd
validate_monotonic_indices <- function(xmo_data, xmo_name, k_levels, n_obs) {
  # Note: Check if indices are 0-based or 1-based
  checkmate::assert_integerish(xmo_data, any.missing = FALSE)

  Xmo <- as.integer(xmo_data)

  if (length(Xmo) != n_obs) {
    stop(insight::format_error(
      "Monotonic design matrix {.field {xmo_name}} has ",
      "{length(Xmo)} elements but expected {n_obs} observations."
    ))
  }

  # Detect if data is 0-based or 1-based and convert to 0-based for .mo function
  min_val <- min(Xmo)
  max_val <- max(Xmo)
  
  if (min_val == 0 && max_val <= k_levels - 1) {
    # Already 0-based indexing (0 to k_levels-1)
    return(Xmo)
  } else if (min_val == 1 && max_val <= k_levels) {
    # 1-based indexing (1 to k_levels), convert to 0-based
    return(Xmo - 1)
  } else {
    stop(insight::format_error(
      "Monotonic design matrix {.field {xmo_name}} contains ",
      "invalid index range. Expected 0-based [0, {k_levels - 1}] ",
      "or 1-based [1, {k_levels}]. Found range: [{min_val}, {max_val}]."
    ))
  }
}


#' Add All GP Contributions to Linear Predictor
#'
#' Universal function that detects, processes, and aggregates all 
#' Gaussian Process terms in a model. Handles both univariate and
#' multivariate contexts with response-specific filtering.
#'
#' @param eta Matrix [n_draws × n_obs] of current linear predictor values
#' @param prep A brmsprep object containing GP data structures
#' @param brmsfit A brmsfit object for kernel detection (optional)
#' @param resp Character string for response name in multivariate models.
#'   NULL for univariate models. When specified, only includes GPs that
#'   are response-specific ("resp_1") or shared (no prefix).
#'
#' @return Matrix [n_draws × n_obs] with GP contributions added
#'
#' @details
#' Processing steps:
#' 1. Detect all GP terms via Xgp_* matrices
#' 2. Filter by response context if multivariate
#' 3. Detect kernel type once and cache
#' 4. Loop through valid GP terms and aggregate contributions
#' 5. Return updated linear predictor
#'
#' Response filtering (multivariate only):
#' - Response-specific: "count_1", "biomass_2" (includes if matches resp)
#' - Shared terms: "1", "2" (includes always - no letter prefix)
#' - Other responses: "biomass_1" when resp="count" (excludes)
#'
#' @noRd
add_all_gp_contributions <- function(eta, prep, brmsfit = NULL, resp = NULL) {
  # Validate inputs
  checkmate::assert_matrix(eta, any.missing = FALSE)
  checkmate::assert_class(prep, "brmsprep") 
  checkmate::assert_string(resp, null.ok = TRUE)
  
  # Detect GP terms
  gp_info <- detect_gp_terms(prep)
  if (is.null(gp_info)) {
    return(eta)  # No GP terms found
  }
  
  # Detect kernel type (use cached if available, detect if brmsfit provided)
  kernel <- prep$gp_kernel
  if (is.null(kernel) && !is.null(brmsfit)) {
    kernel <- detect_gp_kernel(prep, brmsfit)
  }
  if (is.null(kernel)) {
    kernel <- "exp_quad"  # Default fallback
  }
  
  # Filter GP terms by response context (multivariate only)
  suffixes <- gp_info$suffixes
  if (!is.null(resp)) {
    # Multivariate: include response-specific + shared terms
    valid_suffixes <- character()
    for (suffix in suffixes) {
      is_resp_specific <- grepl(paste0("^", resp, "_"), suffix)
      is_shared <- !grepl("^[a-zA-Z]", suffix)
      
      if (is_resp_specific || is_shared) {
        valid_suffixes <- c(valid_suffixes, suffix)
      }
    }
    suffixes <- valid_suffixes
  }
  
  # Return early if no valid suffixes remain
  if (length(suffixes) == 0) {
    return(eta)
  }
  
  # Validate kernel before processing
  checkmate::assert_string(kernel, min.chars = 1)

  # Process each valid GP term
  for (suffix in suffixes) {
    # Extract GP components from sdata and draws
    Xgp <- prep$sdata[[paste0("Xgp_", suffix)]]
    slambda <- prep$sdata[[paste0("slambda_", suffix)]]

    # Skip if missing required data components
    if (is.null(Xgp) || is.null(slambda)) {
      next
    }

    # Extract GP parameters from draws matrix using bracket patterns
    draws_mat <- prep$draws
    draws_names <- colnames(draws_mat)
    n_draws <- nrow(draws_mat)

    # Count sdgp parameters to detect by-variable GP
    sdgp_names <- grep(
      paste0("^sdgp_", suffix, "\\["),
      draws_names,
      value = TRUE
    )
    n_levels <- length(sdgp_names)

    if (n_levels == 0) {
      next
    }

    # Check for Jgp (observation-to-covariate mapping)
    Jgp <- prep$sdata[[paste0("Jgp_", suffix)]]

    # Check for Cgp (continuous by-variable scaling)
    Cgp <- prep$sdata[[paste0("Cgp_", suffix)]]

    if (n_levels > 1) {
      # GP with categorical by-variable requires level-indexed parameters
      checkmate::assert_int(n_levels, lower = 2)

      # Initialize GP contribution matrix (rows=draws, cols=observations)
      gp_contrib <- matrix(0, nrow = n_draws, ncol = ncol(eta))

      for (g in seq_len(n_levels)) {
        # Extract level-specific sdgp
        sdgp_g <- draws_mat[, sdgp_names[g]]
        checkmate::assert_numeric(
          sdgp_g,
          any.missing = FALSE,
          len = n_draws
        )

        # Extract level-specific zgp: zgp_suffix[g,k] for k in 1:n_basis
        zgp_g_pattern <- paste0("^zgp_", suffix, "\\[", g, ",")
        zgp_g_names <- grep(zgp_g_pattern, draws_names, value = TRUE)
        if (length(zgp_g_names) == 0) {
          stop(insight::format_error(
            "No {.field zgp} parameters found for level {g} ",
            "in GP suffix {.field {suffix}}. ",
            "Check that model parameters match prediction data."
          ))
        }
        zgp_g <- draws_mat[, zgp_g_names, drop = FALSE]
        checkmate::assert_matrix(zgp_g, any.missing = FALSE, nrows = n_draws)

        # Extract level-specific lscale: lscale_suffix[g,d] for d dimensions
        lscale_g_pattern <- paste0("^lscale_", suffix, "\\[", g, ",")
        lscale_g_names <- grep(lscale_g_pattern, draws_names, value = TRUE)
        if (length(lscale_g_names) == 0) {
          stop(insight::format_error(
            "No {.field lscale} parameters found for level {g} ",
            "in GP suffix {.field {suffix}}. ",
            "Check that model parameters match prediction data."
          ))
        }
        lscale_g <- draws_mat[, lscale_g_names, drop = FALSE]
        checkmate::assert_matrix(lscale_g, any.missing = FALSE, nrows = n_draws)

        # Compute GP contribution for this level
        gp_g <- approx_gp_pred(Xgp, slambda, zgp_g, sdgp_g, lscale_g, kernel)

        # Find observations belonging to this level
        # Check for level-specific Jgp (e.g., Jgp_2_1, Jgp_2_2)
        Jgp_g_name <- paste0("Jgp_", suffix, "_", g)
        Jgp_g <- prep$sdata[[Jgp_g_name]]

        if (!is.null(Jgp_g)) {
          # Level-specific Jgp: maps level's obs to GP covariate indices
          checkmate::assert_integerish(Jgp_g, any.missing = FALSE, lower = 1)
          level_obs <- which(!is.na(Jgp_g) & Jgp_g > 0)
          if (length(level_obs) > 0) {
            gp_contrib[, level_obs] <- gp_g[, Jgp_g[level_obs], drop = FALSE]
          }
        } else if (!is.null(Jgp)) {
          # Single Jgp with level indicator: values indicate level membership
          level_obs <- which(Jgp == g)
          if (length(level_obs) > 0) {
            if (length(level_obs) != ncol(gp_g)) {
              stop(insight::format_error(
                "Dimension mismatch for GP level {g}: expected ",
                "{length(level_obs)} observations but GP contribution ",
                "has {ncol(gp_g)} columns."
              ))
            }
            gp_contrib[, level_obs] <- gp_g
          }
        } else {
          # No Jgp mapping available - cannot determine level membership
          stop(insight::format_error(
            "GP with by-variable requires {.field Jgp} mapping but none ",
            "found for suffix {.field {suffix}}. ",
            "Check that standata includes level membership indicators."
          ))
        }
      }

      # Apply continuous by-variable scaling if present
      if (!is.null(Cgp)) {
        checkmate::assert_numeric(Cgp, any.missing = FALSE, len = ncol(eta))
        Cgp_mat <- matrix(Cgp, nrow = n_draws, ncol = ncol(eta), byrow = TRUE)
        gp_contrib <- gp_contrib * Cgp_mat
      }

      eta <- eta + gp_contrib

    } else {
      # Single level GP (no categorical by-variable)
      sdgp <- draws_mat[, sdgp_names[1]]

      # Extract zgp parameters (multiple basis functions)
      zgp_names <- grep(
        paste0("^zgp_", suffix, "\\["),
        draws_names,
        value = TRUE
      )
      if (length(zgp_names) == 0) {
        stop(insight::format_error(
          "No {.field zgp} parameters found for suffix {.field {suffix}}."
        ))
      }
      zgp <- draws_mat[, zgp_names, drop = FALSE]

      # Extract lscale parameters
      lscale_names <- grep(
        paste0("^lscale_", suffix, "\\["),
        draws_names,
        value = TRUE
      )
      if (length(lscale_names) == 0) {
        # Try alternative patterns
        alt_patterns <- c(
          paste0("^lsd_", suffix, "\\["),
          paste0("^lengthscale_", suffix, "\\["),
          paste0("^ls_", suffix, "\\[")
        )
        for (pattern in alt_patterns) {
          lscale_names <- grep(pattern, draws_names, value = TRUE)
          if (length(lscale_names) > 0) break
        }
      }
      if (length(lscale_names) == 0) {
        stop(insight::format_error(
          "No {.field lscale} parameters found for suffix {.field {suffix}}. ",
          "Tried patterns: lscale_, lsd_, lengthscale_, ls_."
        ))
      }
      lscale <- draws_mat[, lscale_names, drop = FALSE]

      # Validate matrix dimensions and structure
      checkmate::assert_matrix(Xgp, any.missing = FALSE, all.missing = FALSE)
      checkmate::assert_array(
        slambda,
        min.d = 2,
        max.d = 3,
        any.missing = FALSE
      )
      checkmate::assert_matrix(zgp, any.missing = FALSE, all.missing = FALSE)
      checkmate::assert_numeric(sdgp, any.missing = FALSE, min.len = 1)
      checkmate::assert_matrix(lscale, any.missing = FALSE, all.missing = FALSE)

      # Compute GP contribution
      gp_contrib <- approx_gp_pred(Xgp, slambda, zgp, sdgp, lscale, kernel)

      # Apply Jgp expansion if present (maps obs to unique covariate positions)
      # Following brms pattern: eta <- eta[, Jgp, drop = FALSE]
      if (!is.null(Jgp)) {
        checkmate::assert_integerish(
          Jgp,
          any.missing = FALSE,
          lower = 1,
          upper = ncol(gp_contrib)
        )
        gp_contrib <- gp_contrib[, Jgp, drop = FALSE]
      }

      # Apply continuous by-variable scaling if present
      if (!is.null(Cgp)) {
        checkmate::assert_numeric(Cgp, any.missing = FALSE, len = ncol(eta))
        Cgp_mat <- matrix(Cgp, nrow = n_draws, ncol = ncol(eta), byrow = TRUE)
        gp_contrib <- gp_contrib * Cgp_mat
      }

      eta <- eta + gp_contrib
    }
  }
  
  eta
}


#' Extract Linear Predictor from Prep Object
#'
#' Computes linear predictors (on link scale) from a brmsprep object
#'   using fully vectorized matrix operations. Supports all brms
#'   formula features including fixed effects, smooths, random effects,
#'   Gaussian Processes, monotonic effects, offsets, and nonlinear
#'   formulas.
#'
#' @param prep A brmsprep object from prepare_predictions(). Typically
#'   created via prepare_predictions.mock_stanfit() method.
#' @param resp Optional response name for multivariate models. If NULL
#'   and model is multivariate, returns named list of matrices (one per
#'   response). If specified, returns matrix for that response only.
#'
#' @return For univariate models: Matrix [ndraws × nobs]
#'   For multivariate models with resp=NULL: Named list of matrices
#'   For multivariate models with resp specified: Matrix [ndraws × nobs]
#'
#' @details
#' The linear predictor (eta) is computed as:
#'   eta = Intercept + X * b + smooth_terms + random_effects +
#'         GP_terms + monotonic_effects + offset
#'
#' All computations use vectorized matrix operations. For multivariate
#'   models, loops over responses (not draws), with each response
#'   computation fully vectorized.
#'
#' **Link scale**: Results are on the link function scale (e.g., log
#'   scale for Poisson, logit scale for binomial). To get predictions
#'   on the response scale, apply the inverse link function.
#'
#' **GP filtering**: Response-specific GPs (e.g., Xgp_count_1) apply
#'   only to their response, while shared GPs (e.g., Xgp_1) apply to
#'   all responses.
#'
#' @noRd
extract_linpred_from_prep <- function(prep, resp = NULL) {
  # Validate prep object structure
  checkmate::assert_class(prep, "brmsprep")
  checkmate::assert_list(prep, names = "named")

  if (!"draws" %in% names(prep)) {
    stop(insight::format_error(
      "{.field prep} must contain a {.field draws} component."
    ))
  }

  if (!"sdata" %in% names(prep)) {
    stop(insight::format_error(
      "{.field prep} must contain an {.field sdata} component."
    ))
  }

  if (!"nobs" %in% names(prep)) {
    stop(insight::format_error(
      "{.field prep} must contain an {.field nobs} component."
    ))
  }

  if (!"formula" %in% names(prep)) {
    stop(insight::format_error(
      "{.field prep} must contain a {.field formula} component."
    ))
  }

  checkmate::assert_string(resp, null.ok = TRUE)
  checkmate::assert_integerish(prep$nobs, lower = 1, len = 1)

  # Detect multivariate model
  is_mv <- brms::is.mvbrmsformula(prep$formula)

  if (is_mv) {
    return(extract_linpred_multivariate(prep, resp))
  } else {
    if (!is.null(resp)) {
      stop(insight::format_error(
        "{.field resp} should only be specified for multivariate models."
      ))
    }
    return(extract_linpred_univariate(prep))
  }
}


#' Extract Random Effects Contribution Using Pre-computed Mapping
#'
#' Uses random effects mapping stored in prep object to extract correct
#' parameter contributions.
#'
#' @param prep A brmsprep object with re_mapping field
#' @param draws_mat Matrix of posterior draws
#' @param n_draws Number of draws
#' @param n_obs Number of observations
#'
#' @return Matrix [n_draws × n_obs] of random effects contributions
#'
#' @noRd
population_random_pred <- function(prep, draws_mat, n_draws, n_obs) {
  # Initialize contribution matrix
  re_contrib <- matrix(0, nrow = n_draws, ncol = n_obs)

  # Use pre-computed mapping from prep object
  re_mapping <- prep$re_mapping

  if (is.null(re_mapping) || length(re_mapping) == 0) {
    # No random effects mapping available
    return(re_contrib)
  }

  # Process each design matrix
  for (z_name in names(re_mapping)) {
    if (!z_name %in% names(prep$sdata)) {
      # Design matrix not present in this prediction context
      next
    }

    # Get design vector and indexing
    Z <- as.vector(prep$sdata[[z_name]])

    # Get corresponding J indexing (extract group number from Z_<group>_<term>)
    group_idx <- as.numeric(strsplit(z_name, "_")[[1]][2])
    J_name <- paste0("J_", group_idx)

    if (!J_name %in% names(prep$sdata)) {
      stop(insight::format_error(
        "Missing grouping index {.field {J_name}} for {.field {z_name}}."
      ))
    }

    J <- as.integer(prep$sdata[[J_name]])

    # Validate dimensions
    if (length(Z) != n_obs || length(J) != n_obs) {
      stop(insight::format_error(
        "Dimension mismatch: Z length={length(Z)}, J length={length(J)}, ",
        "expected n_obs={n_obs}."
      ))
    }

    # Get parameter names for this design matrix
    param_names <- re_mapping[[z_name]]

    # Extract parameters (these should exist in draws_mat)
    missing_params <- setdiff(param_names, colnames(draws_mat))
    if (length(missing_params) > 0) {
      stop(insight::format_error(
        "Missing random effects parameters: {paste(missing_params, collapse=', ')}"
      ))
    }

    r_draws <- draws_mat[, param_names, drop = FALSE]

    # Compute contribution: vectorized indexing
    # r_draws[, J] gives [n_draws × n_obs] via column indexing
    # Z broadcast to [n_draws × n_obs] for element-wise multiplication
    re_contrib <- re_contrib +
      r_draws[, J, drop = FALSE] *
      matrix(Z, nrow = n_draws, ncol = n_obs, byrow = TRUE)
  }

  return(re_contrib)
}

#' Add Smooth Fixed Effects (Xs * bs)
#'
#' Adds smooth fixed effects contribution to linear predictor using
#' basis matrix multiplication. Validates parameter dimensions and
#' provides informative error messages for mismatches.
#'
#' @param eta Current linear predictor matrix [n_draws × n_obs]
#' @param draws_mat Parameter draws matrix with bs[*] coefficients
#' @param prep Prepared prediction data containing sdata$Xs
#'
#' @return Updated eta matrix with smooth fixed effects added
#'
#' @details
#' Implements: eta += bs_draws %*% t(Xs)
#' where bs_draws are smooth fixed coefficients and Xs is the
#' smooth basis matrix for fixed effects.
#'
#' @noRd
smooth_fixed_pred <- function(eta, draws_mat, prep) {
  if (!"Xs" %in% names(prep$sdata) || ncol(prep$sdata$Xs) == 0) {
    return(eta)
  }

  Xs <- prep$sdata$Xs
  checkmate::assert_matrix(Xs)

  # Extract smooth fixed effect coefficients (bs[1], bs[2], etc.)
  bs_names <- grep("^bs\\[", colnames(draws_mat), value = TRUE)

  if (length(bs_names) == 0) {
    return(eta)
  }

  if (length(bs_names) != ncol(Xs)) {
    stop(insight::format_error(
      "Smooth parameter count mismatch: {length(bs_names)} ",
      "bs coefficient(s) but {ncol(Xs)} smooth predictor(s)."
    ))
  }

  bs_draws <- draws_mat[, bs_names, drop = FALSE]
  eta + bs_draws %*% t(Xs)
}

#' Add Monotonic Effects (bsp * mo(simo, Xmo))
#'
#' Adds monotonic effects contribution to linear predictor following
#' Stan formula: mu[n] += (bsp[1]) * mo(simo_1, Xmo_1[n])
#'
#' @param eta Current linear predictor matrix [n_draws × n_obs]
#' @param draws_mat Parameter draws matrix with bsp[*] and simo_*[*] coefficients
#' @param prep Prepared prediction data containing sdata components
#' @param suffix Monotonic term suffix (e.g., "1" for univariate, "count_1" for multivariate)
#' @param n_obs Number of observations for current response
#'
#' @return Updated eta matrix with monotonic effects added
#'
#' @details
#' Implements Stan formula: (bsp[id]) * mo(simo_id, Xmo_id[n])
#' where bsp are monotonic coefficients, simo are simplex parameters,
#' and Xmo are ordinal level indices.
#' 
#' Silent returns (unchanged eta) occur when monotonic components are
#' missing, allowing safe use in contexts where monotonic effects
#' may not be present.
#'
#' @noRd
monotonic_pred <- function(eta, draws_mat, prep, suffix, n_obs) {
  # Validate required parameters
  checkmate::assert_matrix(eta, any.missing = FALSE)
  checkmate::assert_matrix(draws_mat)
  checkmate::assert_list(prep, names = "named")
  checkmate::assert_string(suffix, na.ok = FALSE)
  checkmate::assert_count(n_obs, positive = TRUE)
  
  xmo_name <- paste0("Xmo_", suffix)
  
  if (!xmo_name %in% names(prep$sdata)) {
    return(eta)
  }
  
  # Get monotonic simplex parameters
  simo_name <- paste0("simo_", suffix)
  simo_names <- grep(
    paste0("^", simo_name, "\\["),
    colnames(draws_mat),
    value = TRUE
  )
  
  if (length(simo_names) == 0) {
    return(eta)
  }
  
  # Get monotonic coefficient (bsp) - exact match for suffix
  bsp_names <- grep(
    paste0("^bsp\\[", suffix, "\\]$|^bsp_", suffix, "$"),
    colnames(draws_mat),
    value = TRUE
  )
  
  if (length(bsp_names) == 0) {
    return(eta)
  }
  
  # Extract and validate simo parameters: [n_draws × k_levels]
  simo_draws <- draws_mat[, simo_names, drop = FALSE]
  checkmate::assert_matrix(
    simo_draws,
    any.missing = FALSE,
    all.missing = FALSE
  )
  k_levels <- ncol(simo_draws)
  
  # Extract and validate bsp coefficient: [n_draws × 1]
  bsp_draws <- draws_mat[, bsp_names[1], drop = FALSE]
  checkmate::assert_matrix(bsp_draws, ncols = 1)
  
  # Extract and validate ordinal level indices
  Xmo <- validate_monotonic_indices(
    prep$sdata[[xmo_name]],
    xmo_name,
    k_levels,
    n_obs
  )
  
  # Implement brms .mo function logic with vectorized operations
  # 1. Prepend column of zeros to simplex
  # 2. Compute cumulative sum across columns  
  # 3. Multiply by D (number of simplex dimensions)
  # 4. Index with Xmo + 1 (convert 0-based to 1-based)
  
  # D is the number of simplex dimensions
  D <- k_levels
  
  # Validate Xmo indices are within bounds before indexing
  max_index <- max(Xmo)
  if (max_index > k_levels) {
    stop(insight::format_error(
      "Monotonic indices exceed bounds: max index {max_index} ",
      "but only {k_levels} levels available."
    ))
  }
  
  # Prepend zeros and compute cumulative sum vectorized
  # simo_draws: [n_draws × k_levels] -> [n_draws × (k_levels + 1)]
  simplex_with_zero <- cbind(0, simo_draws)
  simplex_cumsum <- t(apply(simplex_with_zero, 1, cumsum))
  
  # Vectorized indexing: D * simplex_cumsum[, Xmo + 1]
  # Xmo contains 0-based indices, add 1 for R's 1-based indexing
  # Results in [n_draws × n_obs] matrix
  mo_values <- D * simplex_cumsum[, Xmo + 1, drop = FALSE]
  
  # Compute contribution: bsp coefficient times mo values
  mo_contrib <- as.vector(bsp_draws) * mo_values
  eta + mo_contrib
}

#' Add Smooth Terms Contributions Using Metadata-Driven Approach
#'
#' Process smooth terms by grouping components using brms metadata (nb_ fields).
#' Each nb_<id> field indicates the number of components for smooth term <id>.
#' This metadata-driven approach handles both regular smooths and tensor products.
#'
#' @param eta Current linear predictor matrix [n_draws × n_obs] to add to
#' @param draws_mat Parameter draws matrix with columns for coefficients
#' @param prep Prepared prediction data from brms
#' @param resp_prefix Response prefix for multivariate (e.g., "y1_") or "" for univariate
#' @return Updated eta matrix with smooth contributions added
#' @noRd
smooth_random_pred <- function(eta, draws_mat, prep, resp_prefix) {
  # Parameter validation
  checkmate::assert_matrix(eta)
  checkmate::assert_matrix(draws_mat)
  checkmate::assert_list(prep)
  checkmate::assert_string(resp_prefix)

  n_draws <- nrow(eta)
  n_obs <- ncol(eta)

  # Build pattern based on response prefix
  if (resp_prefix == "") {
    # Univariate: look for nb_<id> fields
    nb_pattern <- "^nb_"
    zs_prefix <- "Zs_"
    coef_prefix <- "s_"
  } else {
    # Multivariate: look for nb_<resp>_<id> fields
    resp_clean <- gsub("_$", "", resp_prefix)
    nb_pattern <- paste0("^nb_", resp_clean, "_")
    zs_prefix <- paste0("Zs_", resp_clean, "_")
    coef_prefix <- paste0("s_", resp_clean, "_")
  }

  nb_fields <- grep(nb_pattern, names(prep$sdata), value = TRUE)

  for (nb_field in nb_fields) {
    # Extract smooth term ID from nb_[resp_]<id> field name
    smooth_id <- sub(nb_pattern, "", nb_field)
    n_components <- prep$sdata[[nb_field]]

    # Validate metadata values
    checkmate::assert_integerish(n_components, lower = 1, len = 1)

    # Initialize total contribution for this smooth term
    total_smooth_contrib <- matrix(0, nrow = n_draws, ncol = n_obs)

    # Process all components for this smooth term
    for (component in seq_len(n_components)) {
      # Get basis matrix for this component
      zs_name <- paste0(zs_prefix, smooth_id, "_", component)

      if (zs_name %in% names(prep$sdata)) {
        Zs <- prep$sdata[[zs_name]]

        # Validate matrix structure
        if (!is.matrix(Zs)) {
          stop(insight::format_error(
            "Smooth basis {.field {zs_name}} must be a matrix."
          ))
        }

        if (nrow(Zs) != n_obs) {
          stop(insight::format_error(
            "Smooth basis {.field {zs_name}} has {nrow(Zs)} rows ",
            "but expected {n_obs} observations."
          ))
        }

        # Extract coefficients for this specific component
        component_pattern <- paste0("^", coef_prefix, smooth_id, "_", component, "\\[")
        component_params <- grep(
          component_pattern,
          colnames(draws_mat),
          value = TRUE
        )

        if (length(component_params) > 0) {
          # Get coefficients for this component
          sm_coef <- draws_mat[, component_params, drop = FALSE]

          # Add this component's contribution: [n_draws × n_obs]
          total_smooth_contrib <- total_smooth_contrib +
            sm_coef %*% t(Zs)
        }
      } else {
        # Component matrix missing - log warning but continue
        rlang::warn(
          paste0(
            "Expected smooth component matrix {.field ", zs_name, "} ",
            "not found in prep$sdata. Skipping this component."
          ),
          .frequency = "once"
        )
      }
    }

    # Add total smooth term contribution to linear predictor
    eta <- eta + total_smooth_contrib
  }

  return(eta)
}

#' Extract Linear Predictor for Univariate Models
#'
#' @param prep A brmsprep object from prepare_predictions()
#'
#' @return Matrix [ndraws × nobs]
#'
#' @noRd
extract_linpred_univariate <- function(prep) {
  # Check for nonlinear formula
  if (has_nlpars(prep$formula)) {
    return(extract_linpred_nonlinear(prep, resp = NULL))
  }

  draws_mat <- posterior::as_draws_matrix(prep$draws)
  n_draws <- nrow(draws_mat)
  n_obs <- prep$nobs

  # Initialize linear predictor
  eta <- matrix(0, nrow = n_draws, ncol = n_obs)

  # Add intercept parameter if present
  if ("b_Intercept" %in% colnames(draws_mat)) {
    intercept_draws <- draws_mat[, "b_Intercept"]
    eta <- eta + matrix(intercept_draws, nrow = n_draws, ncol = n_obs)
  }

  # Add fixed effects
  if ("X" %in% names(prep$sdata) && ncol(prep$sdata$X) > 0) {
    X <- prep$sdata$X
    checkmate::assert_matrix(X)

    # Remove intercept column if present (column of all 1s)
    if (ncol(X) > 0 && nrow(X) > 0 && all(X[, 1] == 1)) {
      if (ncol(X) > 1) {
        X <- X[, -1, drop = FALSE]
      } else {
        # Intercept-only model
        X <- NULL
      }
    }

    if (!is.null(X)) {
      # Extract non-intercept coefficients (univariate: b[1], b[2], etc.)
      b_names <- grep("^b\\[", colnames(draws_mat), value = TRUE)

      if (length(b_names) > 0) {
        if (length(b_names) != ncol(X)) {
          stop(insight::format_error(
            "Parameter count mismatch: {length(b_names)} ",
            "coefficient(s) but {ncol(X)} predictor(s)."
          ))
        }

        b_draws <- draws_mat[, b_names, drop = FALSE]
        eta <- eta + b_draws %*% t(X)
      }
    }
  }

  # Add smooth fixed effects (Xs * bs)
  eta <- smooth_fixed_pred(eta, draws_mat, prep)

  # Add smooth random effects
  eta <- smooth_random_pred(
    eta = eta,
    draws_mat = draws_mat,
    prep = prep,
    resp_prefix = ""
  )

  # Add random effects
  re_contrib <- population_random_pred(
    prep = prep,
    draws_mat = draws_mat,
    n_draws = n_draws,
    n_obs = n_obs
  )
  eta <- eta + re_contrib

  # Add GP terms
  eta <- add_all_gp_contributions(eta, prep, brmsfit = NULL, resp = NULL)

  # Add monotonic effects (mo() terms)
  xmo_names <- grep("^Xmo_", names(prep$sdata), value = TRUE)
  for (xmo_name in xmo_names) {
    # Extract monotonic term ID (univariate only: "1" from "Xmo_1")
    term_id <- sub("^Xmo_", "", xmo_name)
    eta <- monotonic_pred(eta, draws_mat, prep, term_id, n_obs)
  }

  # Add offset terms if present
  if ("offsets" %in% names(prep$sdata)) {
    offsets <- prep$sdata$offsets

    # Validate offset structure and finite values
    checkmate::assert_numeric(
      offsets,
      any.missing = FALSE,
      finite = TRUE,
      len = n_obs
    )

    # Broadcast offset across all draws on link scale
    # eta: [n_draws × n_obs], offsets: [n_obs] -> broadcast to [n_draws × n_obs]
    eta <- eta + matrix(offsets, nrow = n_draws, ncol = n_obs, byrow = TRUE)
  }

  return(eta)
}


#' Extract Linear Predictor for Multivariate Models
#'
#' @param prep A brmsprep object from prepare_predictions()
#' @param resp Optional response name. If NULL, returns list of matrices.
#'
#' @return If resp is NULL: Named list of matrices [ndraws × nobs]
#'   If resp specified: Single matrix [ndraws × nobs]
#'
#' @noRd
extract_linpred_multivariate <- function(prep, resp = NULL) {
  # Check for nonlinear formula
  if (has_nlpars(prep$formula)) {
    return(extract_linpred_nonlinear(prep, resp = resp))
  }

  # Extract response names
  if (!"responses" %in% names(prep$formula)) {
    stop(insight::format_error(
      "Multivariate formula must contain {.field responses} component."
    ))
  }

  response_names <- prep$formula$responses

  # Validate response_names is populated
  if (is.null(response_names) || length(response_names) == 0) {
    stop(insight::format_error(
      "Multivariate formula detected but no response names found."
    ))
  }

  # Validate resp if specified
  if (!is.null(resp)) {
    if (!resp %in% response_names) {
      stop(insight::format_error(
        "Response {.val {resp}} not found in model. ",
        "Available: {.val {response_names}}."
      ))
    }
    response_names <- resp
  }

  draws_mat <- posterior::as_draws_matrix(prep$draws)
  n_draws <- nrow(draws_mat)

  # Loop over responses (NOT draws) - each response fully vectorized
  result <- vector("list", length = length(response_names))
  names(result) <- response_names

  for (resp_name in response_names) {
    # Get response-specific nobs
    n_obs_name <- paste0("N_", resp_name)
    if (!n_obs_name %in% names(prep$sdata)) {
      available_n <- grep("^N_", names(prep$sdata), value = TRUE)
      stop(insight::format_error(
        "Cannot find {.field {n_obs_name}} in prep$sdata.",
        if (length(available_n) > 0) {
          paste("Available:", paste(available_n, collapse = ", "))
        } else {
          "No N_ fields found."
        }
      ))
    }
    n_obs <- prep$sdata[[n_obs_name]]

    # Initialize linear predictor
    eta <- matrix(0, nrow = n_draws, ncol = n_obs)

    # Add response-specific intercept if present
    intercept_name <- paste0("b_", resp_name, "_Intercept")
    if (intercept_name %in% colnames(draws_mat)) {
      intercept_draws <- draws_mat[, intercept_name]
      eta <- eta + matrix(intercept_draws, nrow = n_draws, ncol = n_obs)
    }

    # Add response-specific fixed effects
    X_name <- paste0("X_", resp_name)
    if (X_name %in% names(prep$sdata) && ncol(prep$sdata[[X_name]]) > 0) {
      X <- prep$sdata[[X_name]]
      checkmate::assert_matrix(X)

      # Remove intercept column if present (column of all 1s)
      if (ncol(X) > 0 && nrow(X) > 0 && all(X[, 1] == 1)) {
        if (ncol(X) > 1) {
          X <- X[, -1, drop = FALSE]
        } else {
          # Intercept-only model
          X <- NULL
        }
      }

      if (!is.null(X)) {
        # Extract response-specific coefficients (e.g., b_count[1], b_count[2])
        b_pattern <- paste0("^b_", resp_name, "\\[")
        b_names <- grep(b_pattern, colnames(draws_mat), value = TRUE)

        if (length(b_names) > 0) {
          if (length(b_names) != ncol(X)) {
            stop(insight::format_error(
              "Parameter mismatch for {.val {resp_name}}: ",
              "{length(b_names)} coefficient(s) but {ncol(X)} ",
              "predictor(s)."
            ))
          }

          b_draws <- draws_mat[, b_names, drop = FALSE]
          eta <- eta + b_draws %*% t(X)
        }
      }
    }

    # Add smooth fixed effects (Xs * bs) for this response
    eta <- smooth_fixed_pred(eta, draws_mat, prep)

    # Add smooth terms for this response using metadata-driven approach
    eta <- smooth_random_pred(
      eta = eta,
      draws_mat = draws_mat,
      prep = prep,
      resp_prefix = paste0(resp_name, "_")
    )

    # Add random effects for this response (vectorized)
    # Multivariate brms uses Z_<group>_<response>_<term> naming
    # Filter Z matrices for this response only
    resp_pattern <- paste0("_", resp_name, "_")
    re_vectors <- grep(resp_pattern, names(prep$sdata), value = TRUE)
    re_vectors <- grep("^Z_", re_vectors, value = TRUE)

    for (z_name in re_vectors) {
      # Parse Z_<group>_<response>_<term> pattern
      # Example: Z_1_y1_1 -> group=1, response=y1, term=1
      parts <- strsplit(z_name, "_")[[1]]
      if (length(parts) < 4) next

      group_id <- parts[2]
      # parts[3] is response name (already filtered above)
      term_id <- parts[4]

      # Get design vector (single column per term in brms)
      Z <- as.vector(prep$sdata[[z_name]])

      if (length(Z) != n_obs) {
        stop(insight::format_error(
          "Random effects design vector {.field {z_name}} has {length(Z)} ",
          "elements but expected {n_obs} observations for ",
          "response {.val {resp_name}}."
        ))
      }

      # Get grouping indices with response-specific naming
      # Pattern: J_<group>_<response>
      J_name <- paste0("J_", group_id, "_", resp_name)
      if (!J_name %in% names(prep$sdata)) {
        next
      }

      J <- as.integer(prep$sdata[[J_name]])
      if (length(J) != n_obs) {
        stop(insight::format_error(
          "Grouping indices {.field {J_name}} length {length(J)} ",
          "does not match {n_obs} observations for ",
          "response {.val {resp_name}}."
        ))
      }

      # Extract group-level parameters for this response and Z matrix
      # Multivariate pattern: r_<groupname>__<response>[level,termname]
      # Example: r_group__y1[1,Intercept], r_group__y1[1,x]
      #
      # Each Z matrix corresponds to one term. The term_id in Z name (e.g.,
      # Z_1_y1_2) corresponds to term order. Extract all term names and match
      # by position.

      r_pattern <- paste0("^r_.*__", resp_name, "\\[")
      r_names_all <- grep(r_pattern, colnames(draws_mat), value = TRUE)

      if (length(r_names_all) == 0) {
        next
      }

      # Extract unique term names from parameters (sorted alphabetically)
      # r_group__y1[1,Intercept] -> "Intercept"
      # r_group__y1[1,x] -> "x"
      term_names <- sort(unique(sub(".*,(.*)\\]$", "\\1", r_names_all)))

      # Map term_id to term_name (term_id=1 -> first term, etc.)
      term_idx <- as.integer(term_id)
      if (term_idx < 1 || term_idx > length(term_names)) {
        next
      }

      term_name <- term_names[term_idx]

      # Extract parameters for this specific term across all levels
      # Pattern: r_group__y1[<any_level>,<term_name>]
      term_pattern <- paste0(
        "^r_.*__", resp_name, "\\[\\d+,",
        gsub("([.()\\[\\]{}^$*+?|])", "\\\\\\1", term_name),  # Escape regex
        "\\]$"
      )
      r_names_term <- grep(term_pattern, colnames(draws_mat), value = TRUE)

      if (length(r_names_term) == 0) {
        next
      }

      # Extract r values: [n_draws × n_groups]
      r_draws_term <- draws_mat[, r_names_term, drop = FALSE]

      # Apply to linear predictor using vectorized indexing
      # r_draws_term[, J] gives [n_draws × n_obs] via column indexing
      # Z broadcast to [n_draws × n_obs] via matrix replication
      eta <- eta + r_draws_term[, J, drop = FALSE] *
        matrix(Z, nrow = n_draws, ncol = n_obs, byrow = TRUE)
    }

    # Add GP terms for this response
    eta <- add_all_gp_contributions(eta, prep, brmsfit = NULL, resp = resp_name)

    # Add monotonic effects for this response
    xmo_names <- grep("^Xmo_", names(prep$sdata), value = TRUE)
    for (xmo_name in xmo_names) {
      # Extract suffix and check if it belongs to current response
      suffix <- sub("^Xmo_", "", xmo_name)

      # Response-specific: "count_1" matches response "count"
      # Shared: "1" applies to all responses (no letter prefix)
      is_resp_specific <- grepl(paste0("^", resp_name, "_"), suffix)
      is_shared <- !grepl("^[a-zA-Z]", suffix)

      if (is_resp_specific || is_shared) {
        eta <- monotonic_pred(eta, draws_mat, prep, suffix, n_obs)
      }
    }

    # Add offset terms if present for this response
    if ("offsets" %in% names(prep$sdata)) {
      # In multivariate models, offsets may be response-specific or shared
      # Extract offsets for this response based on observation count

      # Calculate offset range for this response
      # Find observation start/end positions for this response
      resp_indices <- response_names[1:which(response_names == resp_name)]
      n_obs_before <- sum(sapply(resp_indices[-length(resp_indices)],
                                function(r) {
                                  if (length(resp_indices) == 1) return(0)
                                  prep$sdata[[paste0("N_", r)]]
                                }))

      offset_start <- n_obs_before + 1
      offset_end <- n_obs_before + n_obs

      # Extract response-specific offsets
      all_offsets <- prep$sdata$offsets

      # Validate total offset length matches total observations
      total_obs <- sum(sapply(response_names, function(r) {
        prep$sdata[[paste0("N_", r)]]
      }))

      if (length(all_offsets) != total_obs) {
        stop(insight::format_error(
          "Offset length {length(all_offsets)} does not match total ",
          "observations {total_obs} across all responses."
        ))
      }

      # Extract offsets for current response
      resp_offsets <- all_offsets[offset_start:offset_end]

      # Validate response-specific offset structure
      checkmate::assert_numeric(
        resp_offsets,
        any.missing = FALSE,
        finite = TRUE,
        len = n_obs
      )

      # Broadcast offset across all draws on link scale
      # eta: [n_draws × n_obs], resp_offsets: [n_obs] -> broadcast to [n_draws × n_obs]
      eta <- eta + matrix(resp_offsets, nrow = n_draws, ncol = n_obs, byrow = TRUE)
    }

    result[[resp_name]] <- eta
  }

  # Return single matrix if resp was specified, otherwise list
  if (!is.null(resp)) {
    return(result[[1]])
  } else {
    return(result)
  }
}


#' Extract Linear Predictor for Model Component
#'
#' Helper function that extracts linear predictors for specific model
#' components (observation, trend, or distributional parameters).
#'
#' @param mvgam_fit mvgam object from mvgam()
#' @param newdata data.frame with prediction covariates
#' @param component Character string: "obs", "trend", or distributional
#'   parameter name (e.g., "sigma", "zi", "hu")
#' @param resp Character string for multivariate models (NULL for univariate)
#' @param ndraws Integer number of posterior draws (NULL = all)
#' @param re_formula Formula for random effects (NULL = include all, NA = exclude all)
#' @param allow_new_levels Logical; allow new factor levels in random effects
#' @param sample_new_levels Character; how to sample new levels
#'   ("uncertainty" or "gaussian")
#'
#' @return Matrix [ndraws × nobs] of linear predictor values on link scale
#'
#' @details
#' **Component Routing**:
#' - "obs": Uses extract_obs_parameters() and mvgam_fit$obs_model
#' - "trend": Uses extract_trend_parameters() and mvgam_fit$trend_model,
#'   strips "_trend" suffix from parameter names
#' - Distributional parameters: Routes to appropriate distributional
#'   model component
#'
#' **Parameter Renaming**: Trend parameters have "_trend" suffix stripped
#' because the combined fit stores parameters as "b_trend[1]" but the
#' trend_model brmsfit expects "b[1]".
#'
#' @noRd
extract_component_linpred <- function(mvgam_fit, newdata, component = "obs",
                                     resp = NULL, ndraws = NULL,
                                     re_formula = NULL, allow_new_levels = FALSE,
                                     sample_new_levels = "uncertainty") {
  # Validate inputs
  checkmate::assert_class(mvgam_fit, "mvgam")
  checkmate::assert_data_frame(newdata, min.rows = 1)
  checkmate::assert_string(component)
  checkmate::assert_string(resp, null.ok = TRUE)
  checkmate::assert_int(ndraws, lower = 1, null.ok = TRUE)
  checkmate::assert_logical(allow_new_levels, len = 1)
  checkmate::assert_choice(sample_new_levels, c("uncertainty", "gaussian"))
  checkmate::assert(
    checkmate::check_null(re_formula),
    checkmate::check_class(re_formula, "formula"),
    checkmate::check_identical(re_formula, NA)
  )

  # Validate prediction data factor levels against training data
  if (!is.null(mvgam_fit$trend_metadata) &&
      !is.null(mvgam_fit$trend_metadata$levels)) {
    validate_prediction_factor_levels(newdata, mvgam_fit$trend_metadata)
  }

  # Extract parameters based on component
  if (component == "obs") {
    params <- extract_obs_parameters(mvgam_fit)
    brms_model <- mvgam_fit$obs_model
    strip_suffix <- FALSE
  } else if (component == "trend") {
    params <- extract_trend_parameters(mvgam_fit)
    brms_model <- mvgam_fit$trend_model
    strip_suffix <- TRUE
  } else {
    # Distributional parameter (sigma, zi, hu, etc.)
    params <- extract_obs_parameters(mvgam_fit)
    params <- grep(paste0("_", component), params, value = TRUE)
    
    if (length(params) == 0) {
      stop(insight::format_error(
        "No parameters found for component {.field {component}}."
      ))
    }
    
    brms_model <- mvgam_fit$obs_model
    strip_suffix <- FALSE
  }

  # Validate brms_model exists
  if (is.null(brms_model)) {
    stop(insight::format_error(
      "No brmsfit model found for component {.field {component}}."
    ))
  }

  # Extract parameter draws
  full_draws <- posterior::as_draws_matrix(mvgam_fit$fit)
  
  # Subset to requested draws if specified
  if (!is.null(ndraws)) {
    n_available <- nrow(full_draws)
    if (ndraws > n_available) {
      stop(insight::format_error(
        "Requested {ndraws} draws but only {n_available} available."
      ))
    }
    draw_indices <- sample(n_available, ndraws)
    full_draws <- full_draws[draw_indices, , drop = FALSE]
  }
  
  # Extract component-specific draws
  component_draws <- full_draws[, params, drop = FALSE]
  
  # Strip suffix for trend parameters
  if (strip_suffix) {
    colnames(component_draws) <- gsub("_trend", "", colnames(component_draws))
  }

  # Create mock stanfit object
  mock_fit <- create_mock_stanfit(component_draws)

  # Generate prep object
  prep <- prepare_predictions.mock_stanfit(
    object = mock_fit,
    brmsfit = brms_model,
    newdata = newdata,
    re_formula = re_formula,
    allow_new_levels = allow_new_levels,
    sample_new_levels = sample_new_levels
  )

  # Extract linear predictor
  extract_linpred_from_prep(prep, resp = resp)
}
