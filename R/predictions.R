#' Detect Gaussian Process Terms in Prep Object
#'
#' Searches a brmsprep object for Gaussian Process (GP) terms and
#'   groups level-specific basis matrices under a single term id when
#'   the GP carries a `by` variable. Only approximate (Hilbert-space)
#'   GPs are supported.
#'
#' @param prep A brmsprep object from prepare_predictions()
#'
#' @return NULL if no GP terms found, otherwise a list with:
#'   - terms: named list keyed by term id (e.g. `"1"`, `"trend_2"`).
#'     Each entry is `list(id, n_levels)`. `n_levels == 1` indicates a
#'     standard GP (single basis matrix `Xgp_<id>`); `n_levels > 1`
#'     indicates a by-factor GP with per-level basis matrices
#'     `Xgp_<id>_<g>` for g in 1..n_levels.
#'   - type: always "approximate"
#'   - n_terms: integer count of detected GP terms
#'
#' @details
#' Term ids are enumerated from `sdgp_<id>\[...\]` parameters in
#'   `prep$draws` rather than from `Xgp_*` names in `prep$sdata`, because
#'   `sdgp` is emitted consistently per term whereas `Xgp` carries a
#'   level suffix in the by-factor case (`Xgp_<id>_<g>`) and is
#'   indistinguishable by name from a non-by term whose id happens to
#'   contain an underscore.
#'
#' For each detected term, the function validates that the appropriate
#'   `Xgp_*` and `slambda_*` data structures exist:
#' - no-by GP: `Xgp_<id>`, `slambda_<id>`
#' - by-factor GP: `Xgp_<id>_<g>` and `slambda_<id>_<g>` for each level
#'
#' If a candidate term is missing required structures, the function
#'   errors with the unrecognised pattern (no silent skip, because a silent skip
#'   was the cause of bug #53 where by-factor GP contributions were
#'   never added to the linear predictor).
#'
#' Full GP (`gp(x)` without `k`) emits no `slambda_*` and is not
#'   detected.
#'
#' @noRd
detect_gp_terms <- function(prep) {
  checkmate::assert_class(prep, "brmsprep")
  checkmate::assert_list(prep$sdata, names = "named")
  if ("dpars" %in% names(prep)) {
    checkmate::assert_list(prep$dpars, names = "named")
  }
  if (!"draws" %in% names(prep)) {
    return(NULL)
  }

  draws_names <- colnames(prep$draws)
  sdata_names <- names(prep$sdata)

  # Enumerate term ids via sdgp_<id>[...]. The id is the substring
  # between "sdgp_" and the opening bracket.
  sdgp_flat <- grep("^sdgp_.+\\[[0-9]+\\]$", draws_names, value = TRUE)
  if (length(sdgp_flat) == 0L) {
    return(NULL)
  }
  ids <- unique(sub("^sdgp_(.+?)\\[[0-9]+\\]$", "\\1", sdgp_flat))

  terms <- list()
  for (id in ids) {
    sdgp_id <- grep(paste0("^sdgp_", id, "\\["), sdgp_flat, value = TRUE)
    n_levels <- length(sdgp_id)
    checkmate::assert_int(n_levels, lower = 1L)

    # Required Xgp / slambda entries differ by by-status. Surface the
    # missing names directly rather than skipping silently.
    if (n_levels == 1L) {
      required <- c(paste0("Xgp_", id), paste0("slambda_", id))
    } else {
      required <- c(
        paste0("Xgp_", id, "_", seq_len(n_levels)),
        paste0("slambda_", id, "_", seq_len(n_levels))
      )
    }
    missing_required <- setdiff(required, sdata_names)
    if (length(missing_required) > 0L) {
      stop(insight::format_error(c(
        cli::format_inline(
          "GP term {.field {id}} is missing expected standata entries."
        ),
        x = cli::format_inline(
          "Missing: {paste(missing_required, collapse = ', ')}"
        ),
        i = cli::format_inline(
          "n_levels detected from {.field sdgp_{id}[*]} = {n_levels}."
        )
      )))
    }

    # zgp parameter naming differs: bracket-indexed for no-by
    # (zgp_<id>[k]) and underscore-suffixed for by-factor
    # (zgp_<id>_<g>[k]).
    if (n_levels == 1L) {
      zgp_pat <- paste0("^zgp_", id, "\\[")
    } else {
      zgp_pat <- paste0("^zgp_", id, "_[0-9]+\\[")
    }
    if (!any(grepl(zgp_pat, draws_names))) {
      stop(insight::format_error(
        cli::format_inline(
          "No {.field zgp} parameters found for GP term {.field {id}}."
        )
      ))
    }

    # lscale is emitted by brms as a 2D matrix lscale_<id>[level, dim].
    # Some user-overrides rename to lsd_/lengthscale_/ls_; keep that
    # fallback intact.
    lscale_present <- any(grepl(
      paste0("^lscale_", id, "\\["), draws_names
    ))
    if (!lscale_present) {
      alt_pats <- paste0(
        "^", c("lsd_", "lengthscale_", "ls_"), id, "\\["
      )
      lscale_present <- any(vapply(
        alt_pats,
        function(p) any(grepl(p, draws_names)),
        logical(1L)
      ))
    }
    if (!lscale_present) {
      stop(insight::format_error(c(
        cli::format_inline(
          "No {.field lscale} parameters found for GP term {.field {id}}."
        ),
        i = "Tried prefixes: lscale_, lsd_, lengthscale_, ls_."
      )))
    }

    terms[[id]] <- list(id = id, n_levels = n_levels)
  }

  if (length(terms) == 0L) {
    return(NULL)
  }

  list(
    terms = terms,
    type = "approximate",
    n_terms = length(terms)
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
        cli::format_inline(
          "Unsupported GP kernel: {.field {kernel}}. Supported kernels: exp_quad, matern32, matern52."
        )
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
        cli::format_inline(
          "Object missing {.field formula} component."
        )
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
#' @return Matrix \\[ndraws × nobs\\] for univariate or single response.
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
#'   components. Instead, the whole expression is evaluated during
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
      cli::format_inline(
        "Nonlinear formula models require {.field dpars} component."
      )
    ))
  }

  if (!"mu" %in% names(prep$dpars)) {
    stop(insight::format_error(
      cli::format_inline(
        "Nonlinear formula prep missing {.field mu} in dpars."
      )
    ))
  }

  mu <- prep$dpars$mu

  # Validate mu is matrix with correct structure
  if (!is.matrix(mu)) {
    stop(insight::format_error(
      cli::format_inline("{.field mu} must be a matrix [ndraws \u00d7 nobs].")
    ))
  }

  if (ncol(mu) != prep$nobs) {
    stop(insight::format_error(
      cli::format_inline(
        "{.field mu} has {ncol(mu)} columns but expected {prep$nobs} observations."
      )
    ))
  }

  # Check for multivariate structure
  is_mv <- brms::is.mvbrmsformula(prep$formula)

  if (!is_mv) {
    # Univariate model
    if (!is.null(resp)) {
      stop(insight::format_error(
        cli::format_inline("{.field resp} only for multivariate models.")
      ))
    }
    return(mu)
  }

  # Multivariate model - split mu by response
  if (!"responses" %in% names(prep$formula)) {
    stop(insight::format_error(
      cli::format_inline(
        "Multivariate formula must contain {.field responses}."
      )
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
        cli::format_inline("Missing {.field {nobs_name}} in prep$sdata.")
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
      cli::format_inline(
        "Column count mismatch: split {col_start - 1} columns but {.field mu} has {ncol(mu)} columns."
      )
    ))
  }

  # Return single response if specified
  if (!is.null(resp)) {
    if (!resp %in% response_names) {
      available <- paste(response_names, collapse = ", ")
      stop(insight::format_error(
        cli::format_inline(
          "Response {.field {resp}} not found. Available: {available}."
        )
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
#' @param draws_mat Matrix of posterior draws \\[n_draws × n_params\\]
#' @param smooth_label Base name for smooth term
#'   (e.g., "1_1" from Zs_count_1_1)
#' @param resp Response name for multivariate models (NULL for univariate)
#' @param n_basis Expected number of basis functions
#'
#' @return Matrix \\[n_draws × n_basis\\] of smooth coefficients, or NULL if
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
  # `k_levels` is `ncol(simo_draws)` = D, the simplex dimension. The
  # brms convention is X_mo taking values 0..D inclusive (so D+1
  # distinct values) for 0-based indexing, or 1..D+1 for 1-based. The
  # check below mirrors that contract.
  checkmate::assert_integerish(xmo_data, any.missing = FALSE)

  Xmo <- as.integer(xmo_data)

  if (length(Xmo) != n_obs) {
    stop(insight::format_error(paste0(
      "Monotonic design matrix '", xmo_name, "' has ",
      length(Xmo), " elements but expected ", n_obs, " observations."
    )))
  }

  min_val <- min(Xmo)
  max_val <- max(Xmo)

  # Disambiguate 0-based vs 1-based by the presence of a zero:
  # data containing 0 must be 0-based; otherwise treat as
  # 1-based and shift to 0-based for `.mo()`. The original
  # validator additionally required `min_val == 0` or `1`
  # exactly, which broke partial-data calls -- e.g.
  # `forecast.mvgam`'s training-tail subset for `max_lag = 1`
  # passes a single row whose `Xmo` is a single value in the
  # interior of the valid range. Loosening to `min_val >= 0`
  # / `min_val >= 1` keeps the original semantics for full
  # data while accepting any in-range subset.
  if (min_val == 0L) {
    if (max_val > k_levels) {
      stop(insight::format_error(paste0(
        "Monotonic design matrix '", xmo_name, "' contains ",
        "invalid index range. Expected 0-based [0, ",
        k_levels, "] or 1-based [1, ", k_levels + 1L,
        "]. Found range: [", min_val, ", ", max_val, "]."
      )))
    }
    return(Xmo)
  }
  if (min_val >= 1L && max_val <= k_levels + 1L) {
    return(Xmo - 1L)
  }
  stop(insight::format_error(paste0(
    "Monotonic design matrix '", xmo_name, "' contains ",
    "invalid index range. Expected 0-based [0, ", k_levels,
    "] or 1-based [1, ", k_levels + 1L,
    "]. Found range: [", min_val, ", ", max_val, "]."
  )))
}


#' Add All GP Contributions to Linear Predictor
#'
#' Universal function that detects, processes, and aggregates all 
#' Gaussian Process terms in a model. Handles both univariate and
#' multivariate contexts with response-specific filtering.
#'
#' @param eta Matrix \\[n_draws × n_obs\\] of current linear predictor values
#' @param prep A brmsprep object containing GP data structures
#' @param brmsfit A brmsfit object for kernel detection (optional)
#' @param resp Character string for response name in multivariate models.
#'   NULL for univariate models. When specified, only includes GPs that
#'   are response-specific ("resp_1") or shared (no prefix).
#'
#' @return Matrix \\[n_draws × n_obs\\] with GP contributions added
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
  checkmate::assert_matrix(eta, any.missing = FALSE)
  checkmate::assert_class(prep, "brmsprep")
  checkmate::assert_string(resp, null.ok = TRUE)

  gp_info <- detect_gp_terms(prep)
  if (is.null(gp_info)) {
    return(eta)
  }

  kernel <- prep$gp_kernel
  if (is.null(kernel) && !is.null(brmsfit)) {
    kernel <- detect_gp_kernel(prep, brmsfit)
  }
  if (is.null(kernel)) {
    kernel <- "exp_quad"
  }
  checkmate::assert_string(kernel, min.chars = 1)

  term_ids <- names(gp_info$terms)
  if (!is.null(resp)) {
    # Multivariate filter: keep response-specific or shared term ids.
    # Response-specific term ids begin with the response name; shared
    # terms begin with a digit (purely numeric brms term id).
    keep <- vapply(term_ids, function(id) {
      grepl(paste0("^", resp, "_"), id) || !grepl("^[a-zA-Z]", id)
    }, logical(1L))
    term_ids <- term_ids[keep]
  }
  if (length(term_ids) == 0L) {
    return(eta)
  }

  draws_mat <- prep$draws
  draws_names <- colnames(draws_mat)
  n_draws <- nrow(draws_mat)

  for (id in term_ids) {
    info <- gp_info$terms[[id]]
    n_levels <- info$n_levels

    if (n_levels > 1L) {
      # By-factor GP. Each level has its own basis matrix
      # Xgp_<id>_<g>, eigenvalues slambda_<id>_<g>, observation
      # indices Igp_<id>_<g> (into the full eta column space) and
      # within-level covariate mapping Jgp_<id>_<g>. Parameters are
      # split across naming styles: sdgp_<id>[g] (bracket-indexed)
      # and zgp_<id>_<g>[k] (suffix-indexed). lscale_<id>[g, d] is
      # 2D bracket-indexed for level and covariate dimension.
      gp_contrib <- matrix(0, nrow = n_draws, ncol = ncol(eta))

      for (g in seq_len(n_levels)) {
        Xgp_g <- prep$sdata[[paste0("Xgp_", id, "_", g)]]
        slambda_g <- prep$sdata[[paste0("slambda_", id, "_", g)]]
        Igp_g <- prep$sdata[[paste0("Igp_", id, "_", g)]]
        Jgp_g <- prep$sdata[[paste0("Jgp_", id, "_", g)]]
        Cgp_g <- prep$sdata[[paste0("Cgp_", id, "_", g)]]

        # Igp_g + Jgp_g are required for by-factor placement.
        # detect_gp_terms already validated Xgp_g + slambda_g.
        if (is.null(Igp_g) || is.null(Jgp_g)) {
          stop(insight::format_error(c(
            cli::format_inline(
              "By-factor GP term {.field {id}} level {g} is missing observation-to-basis mappings."
            ),
            x = cli::format_inline(
              "Required: Igp_{id}_{g} and Jgp_{id}_{g} in standata."
            )
          )))
        }
        # newdata may not contain observations for every level of the
        # by-factor (e.g. a prediction grid filtered to a single
        # category). brms emits zero-row Xgp / empty Igp for absent
        # levels; nothing to scatter into eta for that level.
        if (length(Igp_g) == 0L) {
          next
        }
        checkmate::assert_integerish(Igp_g, any.missing = FALSE, lower = 1L)
        checkmate::assert_integerish(Jgp_g, any.missing = FALSE, lower = 1L)

        sdgp_g <- draws_mat[, paste0("sdgp_", id, "[", g, "]")]
        checkmate::assert_numeric(sdgp_g, any.missing = FALSE, len = n_draws)

        zgp_g_names <- grep(
          paste0("^zgp_", id, "_", g, "\\["),
          draws_names,
          value = TRUE
        )
        if (length(zgp_g_names) == 0L) {
          stop(insight::format_error(
            cli::format_inline(
              "No {.field zgp_{id}_{g}[*]} parameters in draws."
            )
          ))
        }
        zgp_g <- draws_mat[, zgp_g_names, drop = FALSE]

        lscale_g_names <- grep(
          paste0("^lscale_", id, "\\[", g, ","),
          draws_names,
          value = TRUE
        )
        if (length(lscale_g_names) == 0L) {
          stop(insight::format_error(
            cli::format_inline(
              "No {.field lscale_{id}[{g},*]} parameters in draws."
            )
          ))
        }
        lscale_g <- draws_mat[, lscale_g_names, drop = FALSE]

        gp_g <- approx_gp_pred(
          Xgp_g, slambda_g, zgp_g, sdgp_g, lscale_g, kernel
        )

        # Reorder level's basis-row predictions to observation order
        # using Jgp_g, then scatter into the full predictor at Igp_g.
        obs_contrib <- gp_g[, Jgp_g, drop = FALSE]

        # Cgp_g carries continuous by-variable scaling (1 for factor
        # by; covariate value for continuous by). Apply per-obs.
        if (!is.null(Cgp_g)) {
          checkmate::assert_numeric(
            Cgp_g, any.missing = FALSE, len = length(Igp_g)
          )
          Cgp_mat <- matrix(
            Cgp_g, nrow = n_draws, ncol = length(Igp_g), byrow = TRUE
          )
          obs_contrib <- obs_contrib * Cgp_mat
        }

        gp_contrib[, Igp_g] <- gp_contrib[, Igp_g] + obs_contrib
      }

      eta <- eta + gp_contrib
    } else {
      # No-by GP: a single basis matrix covers all observations.
      Xgp <- prep$sdata[[paste0("Xgp_", id)]]
      slambda <- prep$sdata[[paste0("slambda_", id)]]
      Jgp <- prep$sdata[[paste0("Jgp_", id)]]
      Cgp <- prep$sdata[[paste0("Cgp_", id)]]

      sdgp <- draws_mat[, paste0("sdgp_", id, "[1]")]

      zgp_names <- grep(
        paste0("^zgp_", id, "\\["), draws_names, value = TRUE
      )
      if (length(zgp_names) == 0L) {
        stop(insight::format_error(
          cli::format_inline(
            "No {.field zgp_{id}[*]} parameters in draws."
          )
        ))
      }
      zgp <- draws_mat[, zgp_names, drop = FALSE]

      lscale_names <- grep(
        paste0("^lscale_", id, "\\["), draws_names, value = TRUE
      )
      if (length(lscale_names) == 0L) {
        alt_patterns <- paste0(
          "^", c("lsd_", "lengthscale_", "ls_"), id, "\\["
        )
        for (pattern in alt_patterns) {
          lscale_names <- grep(pattern, draws_names, value = TRUE)
          if (length(lscale_names) > 0L) break
        }
      }
      if (length(lscale_names) == 0L) {
        stop(insight::format_error(c(
          cli::format_inline(
            "No {.field lscale} parameters in draws for term {.field {id}}."
          ),
          i = "Tried prefixes: lscale_, lsd_, lengthscale_, ls_."
        )))
      }
      lscale <- draws_mat[, lscale_names, drop = FALSE]

      checkmate::assert_matrix(Xgp, any.missing = FALSE, all.missing = FALSE)
      checkmate::assert_array(
        slambda, min.d = 2L, max.d = 3L, any.missing = FALSE
      )
      checkmate::assert_matrix(zgp, any.missing = FALSE, all.missing = FALSE)
      checkmate::assert_numeric(sdgp, any.missing = FALSE, min.len = 1L)
      checkmate::assert_matrix(
        lscale, any.missing = FALSE, all.missing = FALSE
      )

      gp_contrib <- approx_gp_pred(
        Xgp, slambda, zgp, sdgp, lscale, kernel
      )

      if (!is.null(Jgp)) {
        checkmate::assert_integerish(
          Jgp, any.missing = FALSE, lower = 1L, upper = ncol(gp_contrib)
        )
        gp_contrib <- gp_contrib[, Jgp, drop = FALSE]
      }

      if (!is.null(Cgp)) {
        checkmate::assert_numeric(Cgp, any.missing = FALSE, len = ncol(eta))
        Cgp_mat <- matrix(
          Cgp, nrow = n_draws, ncol = ncol(eta), byrow = TRUE
        )
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
#' @return For univariate models: Matrix \\[ndraws × nobs\\]
#'   For multivariate models with resp=NULL: Named list of matrices
#'   For multivariate models with resp specified: Matrix \\[ndraws × nobs\\]
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
      cli::format_inline(
        "{.field prep} must contain a {.field draws} component."
      )
    ))
  }

  if (!"sdata" %in% names(prep)) {
    stop(insight::format_error(
      cli::format_inline(
        "{.field prep} must contain an {.field sdata} component."
      )
    ))
  }

  if (!"nobs" %in% names(prep)) {
    stop(insight::format_error(
      cli::format_inline(
        "{.field prep} must contain an {.field nobs} component."
      )
    ))
  }

  if (!"formula" %in% names(prep)) {
    stop(insight::format_error(
      cli::format_inline(
        "{.field prep} must contain a {.field formula} component."
      )
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
        cli::format_inline(
          "{.field resp} should only be specified for multivariate models."
        )
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
#' @return Matrix \\[n_draws × n_obs\\] of random effects contributions
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
        cli::format_inline(
          "Missing grouping index {.field {J_name}} for {.field {z_name}}."
        )
      ))
    }

    J <- as.integer(prep$sdata[[J_name]])

    # Validate dimensions
    if (length(Z) != n_obs || length(J) != n_obs) {
      stop(insight::format_error(
        cli::format_inline(
          "Dimension mismatch: Z length={length(Z)}, J length={length(J)}, expected n_obs={n_obs}."
        )
      ))
    }

    # Get parameter names for this design matrix
    param_names <- re_mapping[[z_name]]

    # Extract parameters (these should exist in draws_mat)
    missing_params <- setdiff(param_names, colnames(draws_mat))
    if (length(missing_params) > 0) {
      stop(insight::format_error(
        cli::format_inline(
          "Missing random effects parameters: {paste(missing_params, collapse=', ')}"
        )
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
#' @param eta Current linear predictor matrix \\[n_draws × n_obs\\]
#' @param draws_mat Parameter draws matrix with bs\[*\] coefficients
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
      cli::format_inline(
        "Smooth parameter count mismatch: {length(bs_names)} bs coefficient(s) but {ncol(Xs)} smooth predictor(s)."
      )
    ))
  }

  bs_draws <- draws_mat[, bs_names, drop = FALSE]
  eta + bs_draws %*% t(Xs)
}

#' Add Monotonic Effects (bsp * mo(simo, Xmo))
#'
#' Adds monotonic effects contribution to linear predictor following
#' Stan formula: mu\[n\] += (bsp\[1\]) * mo(simo_1, Xmo_1\[n\])
#'
#' @param eta Current linear predictor matrix \\[n_draws × n_obs\\]
#' @param draws_mat Parameter draws matrix with bsp\[*\] and simo_*\[*\] coefficients
#' @param prep Prepared prediction data containing sdata components
#' @param suffix Monotonic term suffix (e.g., "1" for univariate, "count_1" for multivariate)
#' @param n_obs Number of observations for current response
#'
#' @return Updated eta matrix with monotonic effects added
#'
#' @details
#' Implements Stan formula: (bsp\[id\]) * mo(simo_id, Xmo_id\[n\])
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
  
  # Validate Xmo indices are within bounds before indexing.
  # Xmo is 0-based in 0..D after validate_monotonic_indices(); the
  # downstream lookup is simplex_cumsum[, Xmo + 1], which indexes a
  # matrix of width D + 1, so max(Xmo) <= D = k_levels.
  max_index <- max(Xmo)
  if (max_index > k_levels) {
    stop(insight::format_error(paste0(
      "Monotonic indices exceed bounds: max index ", max_index,
      " but only ", k_levels + 1L,
      " simplex entries available (0..", k_levels, ")."
    )))
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
#' @param eta Current linear predictor matrix \\[n_draws × n_obs\\] to add to
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
            cli::format_inline(
              "Smooth basis {.field {zs_name}} must be a matrix."
            )
          ))
        }

        if (nrow(Zs) != n_obs) {
          stop(insight::format_error(
            cli::format_inline(
              "Smooth basis {.field {zs_name}} has {nrow(Zs)} rows but expected {n_obs} observations."
            )
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
          cli::format_inline(
            "Expected smooth component matrix {.field {zs_name}} not found in prep$sdata. Skipping this component."
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
#' @return Matrix \\[ndraws × nobs\\]
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

    # Drop the first column only when it is brms's reserved intercept
    # column. We detect that by the presence of `b_Intercept` in
    # draws (handled above as a scalar offset). Formulas written as
    # `y ~ 0 + <term>` (including the `.mvgam_empty_obs` placeholder
    # injected for empty obs sub-formulas) have no `b_Intercept` and
    # their first column IS a real regressor with its own b[k].
    has_intercept_param <- "b_Intercept" %in% colnames(draws_mat)
    if (has_intercept_param && ncol(X) > 0 && nrow(X) > 0 &&
        all(X[, 1] == 1)) {
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
            cli::format_inline(
              "Parameter count mismatch: {length(b_names)} coefficient(s) but {ncol(X)} predictor(s)."
            )
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
#' @return If resp is NULL: Named list of matrices \\[ndraws × nobs\\]
#'   If resp specified: Single matrix \\[ndraws × nobs\\]
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
      cli::format_inline(
        "Multivariate formula must contain {.field responses} component."
      )
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
        cli::format_inline(
          "Response {.val {resp}} not found in model. Available: {.val {response_names}}."
        )
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
      stop(insight::format_error(c(
        cli::format_inline("Cannot find {.field {n_obs_name}} in prep$sdata."),
        i = if (length(available_n) > 0) {
          paste("Available:", paste(available_n, collapse = ", "))
        } else {
          "No N_ fields found."
        }
      )))
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
              cli::format_inline(
                "Parameter mismatch for {.val {resp_name}}: {length(b_names)} coefficient(s) but {ncol(X)} predictor(s)."
              )
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
          cli::format_inline(
            "Random effects design vector {.field {z_name}} has {length(Z)} elements but expected {n_obs} observations for response {.val {resp_name}}."
          )
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
          cli::format_inline(
            "Grouping indices {.field {J_name}} length {length(J)} does not match {n_obs} observations for response {.val {resp_name}}."
          )
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
          cli::format_inline(
            "Offset length {length(all_offsets)} does not match total observations {total_obs} across all responses."
          )
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
#' @return Matrix \\[ndraws × nobs\\] of linear predictor values on link scale
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
#' because the combined fit stores parameters as "b_trend\[1\]" but the
#' trend_model brmsfit expects "b\[1\]".
#'
#' @noRd
NULL

#' Detect dpar-suffixed parameter names
#'
#' Returns a logical vector indicating which names look like
#' brms's `<head>_<dpar>_<rest>` / `<head>_<dpar>[<idx>]` /
#' `<head>_<dpar>$` emission for a distributional parameter
#' (`p`, `mphi`, `mtheta`, etc.). Used by the dpar branch of
#' `extract_component_linpred()` to filter draws.
#' @noRd
is_dpar_param <- function(x, dpar) {
  pat <- paste0("(^|_)", dpar, "(_|\\[|$)")
  grepl(pat, x)
}

#' Filter and rename `prep$sdata` for a dpar linpred extraction
#'
#' For dpar components (e.g. `p` for nmix(), future `mphi` /
#' `mtheta` for tweedie distributional regression), the
#' obs_model's standata carries BOTH mu's design matrices (`X`,
#' `Xc`, `Xs`, `Zs_<j>_<m>`, `Z_<g>_<m>`, `Xgp_<id>`, etc.) and
#' the dpar's (suffixed with `_<dpar>`). If we feed this to
#' `extract_linpred_univariate()` unchanged, it reads mu's
#' entries and silently composes mu's linpred against the
#' dpar's draws.
#'
#' Two-pass rewrite:
#'  1. Keep ONLY dpar-suffixed design / basis entries plus the
#'     shared structural scalars (N, J_<g>, M_<g>, NC_<g>,
#'     nlevels, prior_only). Mu's bare-name design matrices
#'     are dropped because they would shadow the dpar's
#'     entries after rename.
#'  2. Strip the `_<dpar>` infix from the remaining names so
#'     the dpar's design matrices land under the bare names
#'     `extract_linpred_univariate()` consults.
#'
#' @noRd
strip_dpar_sdata <- function(sdata, dpar) {
  checkmate::assert_list(sdata, names = "named")
  checkmate::assert_string(dpar)
  # Shared structural entries that index into both mu and
  # dpars; keep regardless of `_<dpar>` membership. Row counts and
  # responses are per response, never per distributional parameter,
  # so `N_<resp>` and `Y_<resp>` are matched alongside the bare and
  # numbered forms: a multivariate fit names them after the response
  # and would otherwise lose the very count the predictor is sized by.
  shared_pat <- paste0(
    "^N($|_)|^J_|^M_[0-9]|^NC_[0-9]|^nlevels|",
    "^prior_only$|^offsets$|^Y($|_)"
  )
  has_dpar <- is_dpar_param(names(sdata), dpar)
  is_shared <- grepl(shared_pat, names(sdata))
  keep <- has_dpar | is_shared
  out <- sdata[keep]
  names(out) <- strip_dpar_infix(names(out), dpar)
  out
}

#' Strip the `_<dpar>` infix from brms-emitted parameter or
#' standata names
#'
#' Handles all three positions brms uses: end-of-name
#' (`Intercept_p` -> `Intercept`, `X_p` -> `X`), before an index
#' bracket (`b_p[1]` -> `b[1]`, `bs_p[1]` -> `bs[1]`), and as a
#' middle infix between segments (`b_p_Intercept` -> `b_Intercept`,
#' `r_1_p_1[5]` -> `r_1_1[5]`, `Zs_p_1_1` -> `Zs_1_1`).
#'
#' `_trend` follows the same convention: trend is always a
#' suffix, so passing `dpar = "trend"` gives the trend-side
#' rename used by `extract_component_linpred()`. The single
#' regex unifies the previously bespoke `gsub("_trend", "", ...)`
#' with the dpar path.
#'
#' @param x Character vector of brms parameter or standata names.
#' @param dpar Character scalar; the infix to strip (e.g.,
#'   `"p"`, `"trend"`, `"mphi"`).
#' @return Character vector with the infix removed.
#' @noRd
strip_dpar_infix <- function(x, dpar) {
  pat <- paste0("_", dpar, "(_|\\[|$)")
  sub(pat, "\\1", x)
}

extract_component_linpred <- function(mvgam_fit, newdata, component = "obs",
                                     resp = NULL, draw_ids = NULL,
                                     re_formula = NULL, allow_new_levels = FALSE,
                                     sample_new_levels = "uncertainty",
                                     incl_latent_state = TRUE) {
  # Validate inputs
  checkmate::assert_class(mvgam_fit, "mvgam")
  checkmate::assert_data_frame(newdata, min.rows = 1)
  # Stamp the empty-obs placeholder onto user-supplied newdata
  # when the fit needed one. brms's `validate_data()` would
  # otherwise reject the frame for missing the column.
  newdata <- ensure_obs_placeholder_in_newdata(newdata, mvgam_fit$data)
  checkmate::assert_string(component)
  checkmate::assert_integerish(draw_ids, lower = 1, null.ok = TRUE,
                                any.missing = FALSE)
  checkmate::assert_string(resp, null.ok = TRUE)
  checkmate::assert_logical(allow_new_levels, len = 1)
  checkmate::assert_logical(incl_latent_state, len = 1)
  checkmate::assert_choice(sample_new_levels, c("uncertainty", "gaussian"))
  checkmate::assert(
    checkmate::check_null(re_formula),
    checkmate::check_class(re_formula, "formula"),
    checkmate::check_scalar_na(re_formula)
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
    use_resp <- resp
  } else if (component == "trend") {
    params <- extract_trend_parameters(mvgam_fit)
    brms_model <- mvgam_fit$trend_model
    strip_suffix <- TRUE
    # Detect if trend model is multivariate or shared
    # Shared trends have univariate brmsfit structure and don't support resp
    is_mv_trend <- brms::is.mvbrmsformula(brms_model$formula)
    use_resp <- if (is_mv_trend) resp else NULL
  } else {
    # Distributional parameter (e.g. `p` for nmix(), future
    # `mphi` / `mtheta` for tweedie() distributional regression).
    # The dpar shares a brmsfit with the mu component, so
    # standata carries BOTH mu's design matrices and the dpar's
    # (suffixed by `_<dpar>`). The dpar branch below
    # strip-renames the `_<dpar>` infix on both the parameter
    # draws AND the sdata entries, then drops mu's shadowing
    # entries, so `extract_linpred_from_prep()` composes the
    # dpar linpred via the same parametric + smooth + RE + GP
    # machinery it uses for mu.
    params <- extract_obs_parameters(mvgam_fit)
    params <- params[is_dpar_param(params, component)]

    if (length(params) == 0) {
      stop(insight::format_error(
        cli::format_inline(
          "No parameters found for component {.field {component}}."
        )
      ))
    }

    brms_model <- mvgam_fit$obs_model
    strip_suffix <- TRUE
    use_resp <- resp
  }

  # A trend component is only fitted when the user asked for one, so
  # the common way to arrive here is a fit with no trend rather than a
  # fit whose component is missing.
  if (is.null(brms_model)) {
    if (identical(component, "trend")) {
      stop(insight::format_error(c(
        "This 'mvgam' fit has no latent trend to predict from.",
        i = paste0(
          "A trend is declared through 'trend_formula', for example ",
          "trend_formula = ~ AR()."
        )
      )))
    }
    stop(insight::format_error(
      cli::format_inline(
        "No brmsfit model found for component {.field {component}}."
      )
    ))
  }

  # by = lv_axis() trend predictions: the fitted trend brmsfit holds
  # data at (time, .trend) grain, so the user-supplied (time, series)
  # newdata cannot be passed to its prepare_predictions kernel
  # directly. Instead, build a (time, .trend) prediction grid, run the
  # brms kernel on that grid to recover the per-factor mu_factor, then
  # compose with the posterior Z draws to produce per-(time, series)
  # linpred. The has_by_lv trend computation in Stan emits
  # `trend[t, s] = dot(Z[s, :], lv_trend[t, :] + mu_factor[t, :])`,
  # so when `incl_latent_state = TRUE` the full per-(t, s) result is
  # already in the trend[t, s] draws; we delegate to the latent-state
  # path. When FALSE (the forecast.mvgam deterministic-submodel path),
  # we keep only the dot(Z, mu_factor) contribution here.
  has_by_lv_trend <- identical(component, "trend") &&
    isTRUE(mvgam_fit$trend_metadata$has_by_lv)
  if (has_by_lv_trend) {
    return(compose_by_lv_trend_linpred(
      mvgam_fit = mvgam_fit,
      newdata = newdata,
      draw_ids = draw_ids,
      re_formula = re_formula,
      allow_new_levels = allow_new_levels,
      sample_new_levels = sample_new_levels,
      incl_latent_state = incl_latent_state
    ))
  }

  # Extract parameter draws
  full_draws <- posterior::as_draws_matrix(mvgam_fit$fit)
  n_available <- nrow(full_draws)

  # Draws arrive already chosen: the count a user gave was turned into
  # indices at the boundary they called through, so this extraction
  # and every one it is combined with read the same rows.
  if (!is.null(draw_ids)) {
    if (max(draw_ids) > n_available) {
      stop(insight::format_error(c(
        "'draw_ids' exceeds the number of posterior draws.",
        x = paste0("Got max(draw_ids) = ", max(draw_ids),
                   ", total draws = ", n_available, ".")
      )))
    }
    full_draws <- full_draws[draw_ids, , drop = FALSE]
  }
  
  # Extract component-specific draws
  component_draws <- full_draws[, params, drop = FALSE]

  # Strip the component infix from parameter names so the shared
  # composer (`extract_linpred_univariate()`) reads them as
  # top-level mu parameters. For the trend the infix is always
  # at the end (`_trend`); for dpars it can sit at the end
  # (`Intercept_p`), before a bracket (`b_p[1]`), or as a middle
  # segment (`b_p_Intercept`, `r_1_p_1[5]`). `strip_dpar_infix()`
  # handles all three.
  if (strip_suffix) {
    colnames(component_draws) <- strip_dpar_infix(
      colnames(component_draws),
      dpar = component
    )
  }

  # Create mock stanfit object
  mock_fit <- create_mock_stanfit(component_draws)

  # Generate prep object. Trend submodels are linpred-only (the
  # latent state arrives via incl_latent_state below); their
  # gaussian family slot is a brms-side bookkeeping artefact and
  # the `sigma_trend[]` parameters are per-LV process noise, not
  # observation residual SDs. Pass `linpred_only = TRUE` so
  # prepare_predictions.mock_stanfit() skips the observation-dpar
  # extraction that would otherwise misinterpret `sigma_trend[1..n_lv]`
  # (renamed to `sigma[]` by the _trend infix stripper) as a per-
  # observation sigma dpar.
  linpred_only <- identical(component, "trend")
  prep <- prepare_predictions.mock_stanfit(
    x = mock_fit,
    brmsfit = brms_model,
    newdata = newdata,
    re_formula = re_formula,
    allow_new_levels = allow_new_levels,
    sample_new_levels = sample_new_levels,
    linpred_only = linpred_only
  )

  # For dpar components the obs_model's standata carries both
  # mu's design matrices and the dpar's (suffixed with `_<dpar>`).
  # Drop mu's shadowing entries and rename the dpar-suffixed ones
  # to bare names so the shared linpred composer reads the
  # dpar's basis. Trend components don't need this because the
  # trend has a separate brmsfit whose standata already uses
  # bare names.
  if (strip_suffix && !identical(component, "trend")) {
    prep$sdata <- strip_dpar_sdata(prep$sdata, dpar = component)
  }

  # Extract linear predictor (use_resp handles shared vs multivariate trends)
  linpred <- extract_linpred_from_prep(prep, resp = use_resp)

  # Reason: the predict_* family is time-agnostic and never reads the
  # per-draw conditional `trend[t, s]` from the stanfit; the latent
  # state's marginal-MC contribution is added by `get_combined_linpred`
  # via `sample_process_errors` when `process_error = TRUE`. The
  # `extract_trend_latent_states` helper stays available for the
  # hindcast and forecast paths, which explicitly want
  # the conditional state. `incl_latent_state` is preserved on the
  # signature for hindcast-internal callers that still want the
  # per-draw composition; in the predict_* path it is a no-op for
  # the standard trend kernel.

  # A `draws_matrix` keeps its class through subsetting and
  # arithmetic, so a predictor composed from one is a `draws_matrix`
  # too and a prediction's class ends up depending on whether the
  # model happened to have random effects. The class is dropped on the
  # way out, where the predictor stops being posterior draws and
  # becomes a value: an S4 slot declared to hold a plain matrix
  # rejects the classed one outright.
  if (is.list(linpred) && !is.matrix(linpred)) {
    return(lapply(linpred, as_plain_matrix))
  }
  as_plain_matrix(linpred)
}


#' Compose a per-(time, series) trend linpred for a by = lv_axis() fit.
#'
#' Builds a per-row by per-factor lv prediction grid from `newdata`,
#' runs the trend brmsfit on that grid via the brms mock-stanfit
#' machinery to recover `mu_factor[d, i, k]` (the deterministic
#' smooth contribution at newdata row i for factor k), and composes
#' with the posterior loadings `Z[d, s, k]` to produce a
#' `[draws x n_rows]` linpred matrix.
#'
#' Two return modes:
#'   * `incl_latent_state = FALSE` (deterministic submodel only):
#'     `linpred[d, i] = sum_k Z[d, series_i, k] * mu_factor[d, i, k]`.
#'     Mirrors `brms::posterior_linpred(incl_autocor = FALSE)`.
#'   * `incl_latent_state = TRUE` (latent state added on top): also
#'     pulls raw `lv_trend[t, k]` draws via the tilde-aware selector,
#'     looks up the in-grid `t` for each newdata row, and adds
#'     `sum_k Z[d, series_i, k] * lv_trend[d, t_i, k]`. Times outside
#'     the fit grid receive the per-factor marginal mean of
#'     `lv_trend` across training times, matching the marginal-MC
#'     semantic used by the standard non-by-lv path
#'     (`extract_trend_latent_states()`). For state-aware out-of-
#'     sample prediction, use `forecast.mvgam()`.
#'
#' Composes existing primitives rather than introducing new ones:
#'   * `get_observation_structure()`: newdata to (time, series_int).
#'   * `extract_Z_loadings()`: posterior Z draws as `[d, s, k]`.
#'   * `extract_lv_trend_array_from_draws()`: posterior lv_trend
#'     draws as `[d, t, k]` (tilde-aware, shares
#'     `collect_lv_trend_column_names()` with
#'     `extract_lv_trend_matrices()`).
#'   * `extract_trend_parameters()`, `create_mock_stanfit()`,
#'     `prepare_predictions.mock_stanfit()`,
#'     `extract_linpred_from_prep()`: brms-mocked deterministic
#'     submodel kernel, called with the per-row by .trend grid.
#'   * `strip_dpar_infix()`: strip the `_trend` infix on parameter
#'     draw column names before the mock-stanfit step.
#' @noRd
compose_by_lv_trend_linpred <- function(mvgam_fit, newdata,
                                         draw_ids, re_formula,
                                         allow_new_levels,
                                         sample_new_levels,
                                         incl_latent_state) {
  checkmate::assert_class(mvgam_fit, "mvgam")
  checkmate::assert_data_frame(newdata, min.rows = 1L)
  checkmate::assert_logical(incl_latent_state, len = 1L)

  full_draws <- posterior::as_draws_matrix(mvgam_fit$fit)
  n_available <- nrow(full_draws)
  # As above: the draws are already chosen, so the factor scores and
  # the loadings composed with them come from the same iterations.
  if (!is.null(draw_ids)) {
    if (max(draw_ids) > n_available) {
      stop(insight::format_error(c(
        "'draw_ids' exceeds the number of posterior draws.",
        x = paste0("Got max(draw_ids) = ", max(draw_ids),
                   ", total draws = ", n_available, ".")
      )))
    }
    full_draws <- full_draws[draw_ids, , drop = FALSE]
  }

  n_lv <- as.integer(mvgam_fit$trend_metadata$n_lv_for_grain)
  n_series <- as.integer(mvgam_fit$series_info$n_series %||%
                            mvgam_fit$trend_components$n_trends)
  if (is.null(n_lv) || is.null(n_series)) {
    stop(insight::format_error(c(
      "Cannot compose by = lv_axis() prediction without n_lv / n_series.",
      i = "This indicates a malformed mvgam fit."
    )))
  }
  brms_model <- mvgam_fit$trend_model
  if (is.null(brms_model)) {
    stop(insight::format_error(
      paste0("No trend brmsfit found on the mvgam object for ",
              "by = lv_axis() composition.")
    ))
  }

  obs_struct <- get_observation_structure(mvgam_fit, newdata = newdata)
  s_idx <- as.integer(obs_struct$series_int)
  if (any(s_idx < 1L | s_idx > n_series)) {
    stop(insight::format_error(
      "newdata contains series indices outside the fitted model's range."
    ))
  }
  n_rows <- nrow(newdata)

  # Per-row by per-factor grid: each newdata row repeated for each
  # factor. The previous (time, .trend)-grain build collapsed
  # newdata across series via `collapse_to_time_level()`, which
  # silently dropped per-row covariate variation (e.g. marginaleffects
  # grids with several covariate values at the same time mapped to
  # one row). expand_grid iterates `.trend` fastest, so consecutive
  # blocks of `n_lv` columns in `mu_factor_long` belong to one
  # newdata row; preserved via byrow = TRUE in the reshape below.
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
  if (!is.null(mvgam_fit$trend_metadata$levels)) {
    validate_prediction_factor_levels(lv_newdata,
                                       mvgam_fit$trend_metadata)
  }

  params <- extract_trend_parameters(mvgam_fit)
  component_draws <- full_draws[, params, drop = FALSE]
  colnames(component_draws) <- strip_dpar_infix(
    colnames(component_draws), dpar = "trend"
  )
  mock_fit <- create_mock_stanfit(component_draws)
  prep <- prepare_predictions.mock_stanfit(
    x = mock_fit, brmsfit = brms_model, newdata = lv_newdata,
    re_formula = re_formula, allow_new_levels = allow_new_levels,
    sample_new_levels = sample_new_levels
  )
  mu_factor_long <- extract_linpred_from_prep(prep, resp = NULL)
  if (is.list(mu_factor_long) && !is.matrix(mu_factor_long)) {
    stop(insight::format_error(
      "Multivariate trend formulas are not supported by 'by = lv_axis()' yet."
    ))
  }
  checkmate::assert_matrix(mu_factor_long, ncols = n_rows * n_lv)

  Z_arr <- extract_Z_loadings(full_draws,
                              n_obs_series = n_series, n_lv = n_lv)

  ndraws_used <- nrow(full_draws)
  linpred_mat <- matrix(NA_real_, nrow = ndraws_used, ncol = n_rows)
  for (d in seq_len(ndraws_used)) {
    mu_d <- matrix(mu_factor_long[d, ],
                   nrow = n_rows, ncol = n_lv, byrow = TRUE)
    Z_d <- matrix(Z_arr[d, , ], nrow = n_series, ncol = n_lv)
    # Per row i: dot(Z_d[series_i, :], mu_d[i, :])
    linpred_mat[d, ] <- rowSums(Z_d[s_idx, , drop = FALSE] * mu_d)
  }

  # The latent-state contribution at newdata cells is the marginal-MC
  # innovation envelope, added by `get_combined_linpred` via
  # `sample_process_errors` when `process_error = TRUE`. predict_*
  # does not extract per-draw conditional `trend[t, s]` for any
  # newdata cell; that lives on the hindcast / forecast surface.
  # `incl_latent_state` is preserved on the call signature for
  # hindcast-internal callers but is a no-op in the predict_* path.
  linpred_mat
}


# Internal: stack the tilde-aware `lv_trend[t, k]` draws into a
# `[draws, n_time, n_lv]` array, sourcing draws from a posterior
# matrix that has already been row-subsetted so the rows align with
# whatever Z extraction was done on the same matrix. Shares the
# column-name resolution with `extract_lv_trend_matrices()` (which
# returns a per-factor list of `[draws, n_time]` matrices for
# plotting) via `collect_lv_trend_column_names()`.
#'@noRd
extract_lv_trend_array_from_draws <- function(draws_mat, n_time, n_lv) {
  checkmate::assert_matrix(draws_mat)
  checkmate::assert_int(n_time, lower = 1L)
  checkmate::assert_int(n_lv, lower = 1L)
  meta <- collect_lv_trend_column_names(colnames(draws_mat), n_lv)
  if (nrow(meta$cols_by_tk) != n_time) {
    stop(insight::format_error(c(
      "Mismatch between expected and stored time count in lv_trend.",
      x = paste0(
        "Expected n_time = ", n_time,
        ", max time index in posterior = ",
        nrow(meta$cols_by_tk), "."
      )
    )))
  }
  ndraws <- nrow(draws_mat)
  lv_arr <- array(NA_real_, dim = c(ndraws, n_time, n_lv))
  for (k in seq_len(n_lv)) {
    lv_arr[, , k] <- draws_mat[, meta$cols_by_tk[, k], drop = FALSE]
  }
  lv_arr
}


#' Add latent state matrix to linpred (matrix or per-resp list)
#' @noRd
add_latent_to_linpred <- function(linpred, latent_mat) {
  if (is.list(linpred) && !is.matrix(linpred)) {
    return(lapply(linpred, function(m) {
      checkmate::assert_matrix(m,
        nrows = nrow(latent_mat),
        ncols = ncol(latent_mat)
      )
      m + latent_mat
    }))
  }
  checkmate::assert_matrix(linpred,
    nrows = nrow(latent_mat),
    ncols = ncol(latent_mat)
  )
  linpred + latent_mat
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
#' @param args Argument list destined for a `posterior_*` method
#' @param newdata The `newdata` the caller was given, or `NULL`
#' @param weighted Whether the result will be reweighted by importance
#'   ratios
#' @return `args`, with `latent_state` stamped on when the conditional
#'   surface applies and the caller has not named it already
#'
#' @noRd
diagnostic_surface_args <- function(args, newdata, weighted = FALSE) {
  checkmate::assert_list(args)
  checkmate::assert_logical(weighted, len = 1L)
  if ("latent_state" %in% names(args)) {
    return(args)
  }
  if (is.null(newdata) || isTRUE(weighted)) {
    args$latent_state <- "conditional"
  }
  args
}


#' Extract per-observation latent trend state draws
#'
#' Pulls the `trend\[t, s\]` posterior draws from the stanfit and
#' aligns them to `newdata` rows via `(time, series)` mapping. Returns
#' a ``\\[ndraws x nobs\\]`` matrix or NULL if the fit has no latent trend
#' state.
#'
#' @noRd
extract_trend_latent_states <- function(mvgam_fit, newdata, full_draws) {
  checkmate::assert_class(mvgam_fit, "mvgam")
  checkmate::assert_data_frame(newdata, min.rows = 1)
  checkmate::assert_matrix(full_draws, min.rows = 1)

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
  obs_struct <- get_observation_structure(mvgam_fit, newdata = newdata)
  s_idx <- obs_struct$series_int
  time_var <- mvgam_fit$trend_metadata$variables$time_var %||% "time"
  train_data <- mvgam_fit$obs_data %||% mvgam_fit$data
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
  latent_mat <- matrix(NA_real_, nrow = ndraws, ncol = nobs)
  for (j in seq_len(nobs)) {
    if (is.na(t_idx[j])) {
      latent_mat[, j] <- series_marginal[, s_idx[j]]
    } else {
      nm <- paste0("trend[", t_idx[j], ",", s_idx[j], "]")
      if (!nm %in% par_names) {
        stop(insight::format_error(c(
          "Latent trend state column missing from posterior draws.",
          x = cli::format_inline("Missing: {.val {nm}}."),
          i = "Stan output should contain trend[t, s] for every (t, s) pair covered by the fit."
        )))
      }
      latent_mat[, j] <- full_draws[, nm]
    }
  }
  latent_mat
}
