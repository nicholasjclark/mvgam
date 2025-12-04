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
#' - Mgp_<suffix>: spectral density transformation (k × k matrix)
#' - nb_gp_<suffix>: number of basis functions (scalar)
#'
#' And corresponding parameters in prep$dpars:
#' - zgp_<suffix>: standard normal draws (ndraws × k × k array)
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
      paste0("Mgp_", suffix),
      paste0("nb_gp_", suffix)
    )

    if (!all(required_sdata %in% names(prep$sdata))) {
      next
    }

    # Check required parameter components
    if (!"dpars" %in% names(prep)) {
      next
    }

    required_dpars <- c(
      paste0("zgp_", suffix),
      paste0("sdgp_", suffix),
      paste0("lscale_", suffix)
    )

    if (!all(required_dpars %in% names(prep$dpars))) {
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
#'   approximation following brms Stan code formula. Implements
#'   vectorized computation across all posterior draws.
#'
#' @param Xgp Matrix (N × k) of basis function evaluations at
#'   prediction points
#' @param Mgp Matrix (k × k) spectral density transformation matrix
#' @param zgp Array (ndraws × k × k) of standard normal draws forming
#'   Cholesky factors
#' @param sdgp Matrix (ndraws × k) of marginal standard deviations
#' @param lscale Matrix (ndraws × k) of length-scale parameters
#'
#' @return Matrix (ndraws × N) of GP contributions to add to linear
#'   predictor
#'
#' @details
#' Implements the brms Stan formula:
#'   mu += Xgp * (Mgp * (sdgp .* (zgp * lscale)))
#'
#' Step-by-step computation per draw:
#' 1. Transform latent factors: zgp[i,,] %*% lscale[i,]  -> (k)
#' 2. Scale by SD: sdgp[i,] * step1  -> (k)
#' 3. Apply spectral transform: Mgp %*% step2  -> (k)
#' 4. Project to observations: Xgp %*% step3  -> (N)
#'
#' Loop required for 3D array matrix multiplication per draw.
#'
#' @noRd
approx_gp_pred <- function(Xgp, Mgp, zgp, sdgp, lscale) {
  # Validate matrix dimensions and finite values
  checkmate::assert_matrix(Xgp, any.missing = FALSE, all.missing = FALSE)
  checkmate::assert_matrix(Mgp, any.missing = FALSE, all.missing = FALSE)
  checkmate::assert_array(zgp, d = 3, any.missing = FALSE)
  checkmate::assert_matrix(sdgp, any.missing = FALSE, all.missing = FALSE)
  checkmate::assert_matrix(
    lscale,
    any.missing = FALSE,
    all.missing = FALSE
  )

  # Extract dimensions
  n_obs <- nrow(Xgp)
  k <- ncol(Xgp)
  n_draws <- dim(zgp)[1]

  # Validate dimension consistency
  if (nrow(Mgp) != k || ncol(Mgp) != k) {
    stop(insight::format_error(
      "Dimension mismatch: {.field Mgp} has {nrow(Mgp)} rows and ",
      "{ncol(Mgp)} columns, but expected {k} × {k} to match ",
      "{.field Xgp} basis functions."
    ))
  }

  if (dim(zgp)[2] != k || dim(zgp)[3] != k) {
    stop(insight::format_error(
      "Dimension mismatch: {.field zgp} has dimensions ",
      "{dim(zgp)[1]} × {dim(zgp)[2]} × {dim(zgp)[3]}, but expected ",
      "{n_draws} × {k} × {k}."
    ))
  }

  if (nrow(sdgp) != n_draws || ncol(sdgp) != k) {
    stop(insight::format_error(
      "Dimension mismatch: {.field sdgp} has {nrow(sdgp)} rows and ",
      "{ncol(sdgp)} columns, but expected {n_draws} × {k}."
    ))
  }

  if (nrow(lscale) != n_draws || ncol(lscale) != k) {
    stop(insight::format_error(
      "Dimension mismatch: {.field lscale} has {nrow(lscale)} rows ",
      "and {ncol(lscale)} columns, but expected {n_draws} × {k}."
    ))
  }

  # Pre-allocate result matrix
  result <- matrix(0, nrow = n_draws, ncol = n_obs)

  # Compute GP contribution per draw
  for (i in seq_len(n_draws)) {
    # Step 1: Transform latent factors with length scales
    # zgp[i,,] is (k × k), lscale[i,] is (k) -> result is (k)
    step1 <- zgp[i, , ] %*% lscale[i, ]

    # Step 2: Scale by marginal standard deviations
    # sdgp[i,] is (k), step1 is (k) -> element-wise multiply gives (k)
    step2 <- sdgp[i, ] * step1

    # Step 3: Apply spectral density transformation
    # Mgp is (k × k), step2 is (k) -> result is (k)
    step3 <- Mgp %*% step2

    # Step 4: Project onto observation space
    # Xgp is (N × k), step3 is (k) -> result is (N)
    result[i, ] <- Xgp %*% step3
  }

  result
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
  # Note: brms uses 0-based indexing for monotonic effects
  checkmate::assert_integerish(xmo_data, lower = 0, any.missing = FALSE)

  Xmo <- as.integer(xmo_data)

  if (length(Xmo) != n_obs) {
    stop(insight::format_error(
      "Monotonic design matrix {.field {xmo_name}} has ",
      "{length(Xmo)} elements but expected {n_obs} observations."
    ))
  }

  # Validate Xmo indices are within valid range (0-based)
  if (any(Xmo < 0) || any(Xmo >= k_levels)) {
    stop(insight::format_error(
      "Monotonic design matrix {.field {xmo_name}} contains ",
      "values outside valid range [0, {k_levels - 1}]. ",
      "Found range: [{min(Xmo)}, {max(Xmo)}]."
    ))
  }

  Xmo
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
  
  # Compute monotonic contribution: bsp * mo(simo, Xmo)
  # simo_draws[, Xmo] gives [n_draws × n_obs] via column indexing
  # bsp_draws is [n_draws × 1], broadcast to [n_draws × n_obs]
  mo_contrib <- bsp_draws * simo_draws[, Xmo, drop = FALSE]
  eta + mo_contrib
}

#' Add Smooth Terms Contributions Using Metadata-Driven Approach
#'
#' Process smooth terms by grouping components using brms metadata (nb_ fields).
#' Each nb_<id> field indicates the number of components for smooth term <id>.
#' This unified approach handles both regular smooths and tensor products.
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
  gp_info <- detect_gp_terms(prep)
  if (!is.null(gp_info)) {
    for (suffix in gp_info$suffixes) {
      # Extract all GP components for this term
      Xgp <- prep$sdata[[paste0("Xgp_", suffix)]]
      Mgp <- prep$sdata[[paste0("Mgp_", suffix)]]
      zgp <- prep$dpars[[paste0("zgp_", suffix)]]
      sdgp <- prep$dpars[[paste0("sdgp_", suffix)]]
      lscale <- prep$dpars[[paste0("lscale_", suffix)]]

      # Compute and add GP contribution
      gp_contrib <- approx_gp_pred(Xgp, Mgp, zgp, sdgp, lscale)
      eta <- eta + gp_contrib
    }
  }

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
    gp_info <- detect_gp_terms(prep)
    if (!is.null(gp_info)) {
      for (suffix in gp_info$suffixes) {
        # Check if GP term belongs to current response
        # Response-specific: "count_1" matches response "count"
        # Shared: "1" applies to all responses (no letter prefix)
        is_resp_specific <- grepl(paste0("^", resp_name, "_"), suffix)
        is_shared <- !grepl("^[a-zA-Z]", suffix)

        if (!is_resp_specific && !is_shared) {
          next
        }

        # Extract all GP components for this term
        Xgp <- prep$sdata[[paste0("Xgp_", suffix)]]
        Mgp <- prep$sdata[[paste0("Mgp_", suffix)]]
        zgp <- prep$dpars[[paste0("zgp_", suffix)]]
        sdgp <- prep$dpars[[paste0("sdgp_", suffix)]]
        lscale <- prep$dpars[[paste0("lscale_", suffix)]]

        # Compute and add GP contribution
        gp_contrib <- approx_gp_pred(Xgp, Mgp, zgp, sdgp, lscale)
        eta <- eta + gp_contrib
      }
    }

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
