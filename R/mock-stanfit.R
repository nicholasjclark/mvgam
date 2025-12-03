#' Get Safe Dummy Value for Family
#'
#' Generates a safe dummy response value based on family type and bounds.
#' Follows brms's three-tier logic: integer families get 1L, positive
#' continuous families get 1, unconstrained families get 0.
#'
#' @param family_obj A brmsfamily object with type and ybounds properties
#'
#' @return Integer (1L) for discrete families, numeric (1 or 0) for continuous
#'
#' @details
#' Implements brms dummy_y_values() logic using public family properties:
#' 1. Integer families (type == "int"): Returns 1L
#' 2. Positive continuous (ybounds[1] > 0): Returns 1
#' 3. Unconstrained families: Returns 0
#'
#' This matches brms internal behavior without relying on unexported functions.
#' Works with both built-in and custom families via standardized properties.
#'
#' @noRd
get_safe_dummy_value <- function(family_obj) {
  # Validate family object type
  checkmate::assert_class(family_obj, "brmsfamily")

  # Check if integer/discrete family (brms uses type = "int" or "real")
  # Integer families need integer dummy values for Stan type checking
  if (!is.null(family_obj$type) && family_obj$type == "int") {
    return(1L)
  }

  # For continuous families, check lower bound
  # Extract bounds from ybounds (brms stores as [lower, upper])
  ybounds <- family_obj$ybounds

  if (is.null(ybounds) || length(ybounds) < 2) {
    # Fallback to 0 if ybounds not available (shouldn't happen with valid family)
    return(0)
  }

  lb <- ybounds[1]

  # Positive-constrained continuous families (gamma, weibull, exponential, etc.)
  # Use lb >= 0 (not > 0) because families with lb=0 still need positive dummy
  # values to pass validation (gamma requires y > 0, not y >= 0)
  if (is.finite(lb) && lb >= 0) {
    return(1)
  }

  # Unconstrained families (gaussian, student, etc.)
  return(0)
}

#' Extract Random Effects Parameter Mapping from brms Object
#'
#' Creates a mapping from design matrix names to parameter names using
#'   brms ranef structure. Maps Z_1_1 style names to r_1[level,term]
#'   style parameter patterns.
#'
#' @param brmsfit_object A fitted brms model
#' 
#' @return Named list mapping Z matrix names to parameter name vectors
#' 
#' @noRd
get_brms_re_mapping <- function(brmsfit_object) {
  # Validate inputs
  checkmate::assert_class(brmsfit_object, "brmsfit")
  
  # Get ranef structure (data frame with one row per grouping factor/term combination)
  ranef_df <- brmsfit_object$ranef
  
  if (is.null(ranef_df) || nrow(ranef_df) == 0) {
    # No random effects in model
    return(list())
  }
  
  mapping <- list()
  
  # Get levels for each group (stored as attribute)
  group_levels <- attr(ranef_df, "levels")
  
  # Process each row (each group/term combination)
  for (row_idx in 1:nrow(ranef_df)) {
    row_info <- ranef_df[row_idx, ]
    
    # Extract semantic information  
    group_name <- row_info$group    # e.g., "group"
    term_name <- row_info$coef      # e.g., "Intercept"  
    group_idx <- row_info$gn        # Group number (1, 2, ...)
    term_idx <- row_info$cn         # Term number (1, 2, ...)
    
    # Get number of levels for this group
    levels_for_group <- group_levels[[group_name]]
    n_levels <- length(levels_for_group)
    
    # Construct design matrix name (brms internal convention)
    z_name <- paste0("Z_", group_idx, "_", term_idx)

    # Construct parameter names (brms numeric convention)
    # brms stores parameters as r_<group_num>_<term_num>[level], not semantic
    # names
    param_names <- paste0(
      "r_", group_idx, "_", term_idx, "[", 1:n_levels, "]"
    )
    
    # Store mapping
    mapping[[z_name]] <- param_names
  }
  
  return(mapping)
}


#' Create Mock stanfit for Parameter Subsetting
#'
#' Creates a lightweight S3 object that mimics a stanfit for the purpose of
#' providing parameter subsets to brms functions during prediction. This mock
#' only supports draws extraction via as_draws_matrix().
#'
#' @param draws_matrix A draws_matrix object containing a subset of parameters
#'   from the original stanfit. Must inherit from "draws_matrix" and "draws"
#'   classes and be a matrix object.
#'
#' @return S3 object of class c("mock_stanfit", "stanfit") containing the
#'   cached draws.
#'
#' @details
#' This function enables parameter subsetting for brms prediction functions
#' without creating a full stanfit object. The mock stanfit only implements
#' the as_draws_matrix() method, which is sufficient for brms prediction
#' workflows.
#'
#' The mock inherits from "stanfit" to satisfy brms type checks while
#' providing custom extraction behavior through the S3 method dispatch system.
#'
#' Usage pattern:
#' \code{
#' # Extract observation parameters from combined fit
#' full_draws <- posterior::as_draws_matrix(mvgam_fit$fit)
#' obs_params <- c("b_Intercept", "b_temperature", "sigma")
#' obs_draws <- full_draws[, obs_params]
#'
#' # Create mock stanfit
#' mock_fit <- create_mock_stanfit(obs_draws)
#'
#' # Use in brmsfit object for predictions
#' obs_brmsfit$fit <- mock_fit
#' prep <- brms:::prepare_predictions(obs_brmsfit, newdata)
#' }
#'
#' @noRd
create_mock_stanfit <- function(draws_matrix) {
  # Validate draws_matrix has required posterior package classes
  checkmate::assert_class(draws_matrix, "draws_matrix")

  # Verify required structure for brms compatibility
  if (!"draws" %in% class(draws_matrix)) {
    stop(insight::format_error(
      "{.field draws_matrix} must inherit from 'draws' class."
    ))
  }

  if (!is.matrix(draws_matrix)) {
    stop(insight::format_error(
      "{.field draws_matrix} must be a matrix."
    ))
  }

  # Construct lightweight mock with cached draws for brms compatibility
  structure(
    list(draws_cache = draws_matrix),
    class = c("mock_stanfit", "stanfit")
  )
}


#' Extract Draws from Mock stanfit
#'
#' S3 method for extracting draws from a mock stanfit object created by
#' create_mock_stanfit(). This method is called by brms prediction functions
#' to access parameter draws.
#'
#' @param x A mock_stanfit object created by create_mock_stanfit()
#' @param ... Additional arguments (ignored, for S3 compatibility)
#'
#' @return The cached draws_matrix from the mock stanfit
#'
#' @details
#' This method provides the core functionality of the mock stanfit pattern.
#' When brms calls as_draws_matrix() on our mock stanfit, it receives the
#' parameter subset we provided during creation.
#'
#' The method simply returns the cached draws without any transformation,
#' maintaining the original posterior samples and parameter structure.
#'
#' @method as_draws_matrix mock_stanfit
#' @export
as_draws_matrix.mock_stanfit <- function(x, ...) {
  x$draws_cache
}


#' Prepare Predictions for Mock stanfit
#'
#' S3 method for prepare_predictions that works with mock_stanfit objects
#' created by create_mock_stanfit(). This method generates design matrices
#' and prediction metadata using brmsfit formula and family information
#' without requiring a full Stan fit object.
#'
#' @param object A mock_stanfit object created by create_mock_stanfit()
#'   containing parameter draws subset
#' @param brmsfit A brmsfit object providing formula, family, and other
#'   metadata needed for design matrix construction
#' @param newdata Data frame with predictor values for predictions. If NULL,
#'   uses original fitting data from brmsfit
#' @param re_formula Formula for random effects. NULL includes all random
#'   effects, NA excludes all
#' @param allow_new_levels Logical indicating whether new factor levels in
#'   random effects are allowed
#' @param sample_new_levels Character specifying how to sample new levels:
#'   "uncertainty" or "gaussian"
#' @param ... Additional arguments passed to brms::make_standata()
#'
#' @return A brmsprep object (S3 list) containing design matrices, formula
#'   metadata, family information, and parameter draws needed for posterior
#'   predictions. Compatible with downstream mvgam prediction functions.
#'
#' @details
#' This method implements the prepare_predictions interface for
#'   mock_stanfit objects, enabling mvgam to use brms prediction
#'   infrastructure with parameter subsets extracted from combined
#'   Stan fits.
#'
#' The function leverages brms::standata() to generate design matrices
#'   for fixed effects, random effects, smooths, GPs, and other special
#'   terms. This ensures compatibility with all brms formula features.
#'
#' For nonlinear formula models (nl = TRUE), this method automatically
#'   calls compute_nonlinear_dpars() to evaluate the nonlinear
#'   expression and add dpars$mu to the returned prep object.
#'
#' The returned brmsprep object contains all metadata needed for
#'   computing linear predictors via extract_linpred_from_prep().
#'
#' @method prepare_predictions mock_stanfit
#' @export
prepare_predictions.mock_stanfit <- function(object,
                                              brmsfit,
                                              newdata = NULL,
                                              re_formula = NULL,
                                              allow_new_levels = FALSE,
                                              sample_new_levels = "uncertainty",
                                              ...) {
  # Validate core object types
  checkmate::assert_class(object, "mock_stanfit")
  checkmate::assert_class(brmsfit, "brmsfit")

  # Validate newdata if provided
  if (!is.null(newdata)) {
    if (!is.data.frame(newdata)) {
      stop(insight::format_error(
        "{.field newdata} must be a data frame.",
        "Received object of class: {class(newdata)}"
      ))
    }
    if (nrow(newdata) < 1) {
      stop(insight::format_error(
        "{.field newdata} must contain at least one row.",
        "Received data frame with {nrow(newdata)} rows."
      ))
    }
  }

  # Validate re_formula parameter
  if (!is.null(re_formula) && !identical(re_formula, NA)) {
    if (!inherits(re_formula, "formula")) {
      stop(insight::format_error(
        "{.field re_formula} must be NULL, NA, or a valid formula object.",
        "Current value has class: {class(re_formula)}"
      ))
    }
  }

  # Validate prediction parameters
  checkmate::assert_logical(allow_new_levels, len = 1)
  checkmate::assert_choice(
    sample_new_levels,
    choices = c("uncertainty", "gaussian")
  )

  # Validate logical relationship between parameters
  if (sample_new_levels != "uncertainty" && !allow_new_levels) {
    stop(insight::format_error(
      "{.field sample_new_levels} can only be modified when ",
      "{.field allow_new_levels} is TRUE.",
      "Set {.field allow_new_levels = TRUE} or use default sampling."
    ))
  }

  # Use newdata if provided, otherwise use original data from brmsfit
  prediction_data <- newdata %||% brmsfit$data

  # Add dummy response variables for standata generation
  # Pattern follows brms internal validate_newdata() → add_dummy_responses()
  # Dummy values are required for formula evaluation and data structure
  # consistency, but are never used in prediction computations
  newdata_with_resp <- prediction_data

  # Extract response variable(s) - handle both univariate and multivariate
  if (brms::is.mvbrmsformula(brmsfit$formula)) {
    # Multivariate: get all response variables from formula components
    resp_vars <- names(brmsfit$formula$forms)
  } else {
    # Univariate: extract single response from formula LHS
    resp_vars <- as.character(brmsfit$formula$formula[[2]])
  }

  # Add dummy values for any missing response variables
  # Use family-appropriate values based on bounds (never used in computation)
  for (rv in resp_vars) {
    if (!rv %in% names(newdata_with_resp)) {
      # Get family for this response
      if (brms::is.mvbrmsformula(brmsfit$formula)) {
        family_obj <- brmsfit$family[[rv]]
      } else {
        family_obj <- brmsfit$family
      }

      # Generate safe dummy based on family type and bounds
      dummy_value <- get_safe_dummy_value(family_obj)

      newdata_with_resp[[rv]] <- rep(
        dummy_value,
        nrow(newdata_with_resp)
      )
    }
  }

  # Generate Stan data structure using brmsfit object directly
  # This creates all design matrices via brms machinery
  # Use internal=TRUE for compatibility with brms prediction workflow
  sdata <- brms::standata(
    brmsfit,
    newdata = newdata_with_resp,
    re_formula = re_formula,
    allow_new_levels = allow_new_levels,
    internal = TRUE,
    ...
  )

  # Extract draws from mock stanfit using cached draws
  draws <- object$draws_cache

  # Pre-compute random effects mapping for efficient extraction
  re_mapping <- get_brms_re_mapping(brmsfit)

  # Build minimal brmsprep structure compatible with mvgam prediction workflow
  # Following brms prepare_predictions() return structure
  prep <- structure(
    list(
      sdata = sdata,
      draws = draws,
      formula = brmsfit$formula,
      family = brmsfit$family,
      newdata = prediction_data,
      nobs = nrow(prediction_data),
      re_mapping = re_mapping  # Store pre-computed mapping
    ),
    class = c("brmsprep", "mvgam_prep")
  )

  # Generate dpars component for nonlinear formula models
  if (has_nlpars(brmsfit$formula)) {
    prep$dpars <- compute_nonlinear_dpars(prep, brmsfit$formula)
  }

  return(prep)
}


#' Compute dpars for Nonlinear Formula Models
#'
#' Replicates brms's predictor.bprepnl() logic to evaluate
#'   nonlinear formulas and generate dpars$mu for prediction.
#'   Handles nlpar extraction, covariate mapping, and formula
#'   evaluation.
#'
#' @param prep A brmsprep object from prepare_predictions.mock_stanfit()
#' @param formula A brmsformula object with nonlinear specification
#'   (nl = TRUE)
#'
#' @return List with $mu component (matrix [ndraws × nobs])
#'
#' @details
#' Nonlinear formulas specify R expressions combining nonlinear
#'   parameters (nlpars) and covariates. Example:
#'   `bf(y ~ b1 * exp(b2 * x), b1 + b2 ~ 1, nl = TRUE)`
#'   where b1 and b2 are nlpars with parameter formulas (both
#'   intercept-only here).
#'
#' This function:
#'   1. Extracts nlpar names from formula$pforms
#'   2. Computes linear predictors for each nlpar
#'   3. Extracts and maps covariates from standata
#'   4. Evaluates the nonlinear formula expression
#'
#' @section Attribution:
#' This implementation replicates the logic from brms's
#'   predictor.bprepnl() and get_nlpar() functions.
#'   Full credit to Paul-Christian Buerkner and the brms
#'   development team for the original implementation.
#'   See: https://github.com/paul-buerkner/brms
#'
#' @noRd
compute_nonlinear_dpars <- function(prep, formula) {
  # Validate inputs
  checkmate::assert_class(prep, "brmsprep")
  checkmate::assert_class(formula, "brmsformula")

  if (is.null(formula$pforms) || length(formula$pforms) == 0) {
    stop(insight::format_error(
      "Nonlinear formula must have {.field pforms} component."
    ))
  }

  # Extract nlpar names and formula expression
  nlpar_names <- names(formula$pforms)
  nlform_expr <- formula$formula[[3]]

  # Compute linear predictor for each nlpar
  eta_nlpars <- list()

  for (nlp in nlpar_names) {
    # Get design matrix
    x_name <- paste0("X_", nlp)
    if (!x_name %in% names(prep$sdata)) {
      stop(insight::format_error(
        "Missing design matrix {.field {x_name}} for nlpar ",
        "{.val {nlp}}."
      ))
    }

    X_nlpar <- prep$sdata[[x_name]]

    # Get coefficient parameters (use array notation for nl models)
    # Array pattern: b_nlpar[1], b_nlpar[2], ...
    # Underscore pattern: b_nlpar_coef1, b_nlpar_coef2, ...
    array_pattern <- paste0("^b_", nlp, "\\[")
    underscore_pattern <- paste0("^b_", nlp, "_")

    array_params <- grep(
      array_pattern,
      colnames(prep$draws),
      value = TRUE
    )
    underscore_params <- grep(
      underscore_pattern,
      colnames(prep$draws),
      value = TRUE
    )

    matching_params <- c(array_params, underscore_params)

    if (length(matching_params) == 0) {
      stop(insight::format_error(
        "No coefficients found for nlpar {.val {nlp}}. ",
        "Expected parameters matching {.val {array_pattern}} or ",
        "{.val {underscore_pattern}}."
      ))
    }

    b_nlpar <- prep$draws[, matching_params, drop = FALSE]

    # Validate dimensions
    if (ncol(b_nlpar) != ncol(X_nlpar)) {
      stop(insight::format_error(
        "Dimension mismatch for nlpar {.val {nlp}}: ",
        "design matrix has {ncol(X_nlpar)} columns but ",
        "found {ncol(b_nlpar)} coefficient parameters."
      ))
    }

    # Compute linear predictor: b %*% t(X)
    eta_nlpars[[nlp]] <- b_nlpar %*% t(X_nlpar)
  }

  # Extract covariates from standata
  covariates <- list()
  c_vars <- grep("^C_", names(prep$sdata), value = TRUE)

  if (length(c_vars) > 0) {
    # Extract covariate variable names from formula
    all_vars <- all.vars(formula$formula)
    response_var <- as.character(formula$formula[[2]])
    covariate_names <- setdiff(all_vars, c(response_var, nlpar_names))

    if (length(covariate_names) != length(c_vars)) {
      stop(insight::format_error(
        "Covariate mismatch: formula has ",
        "{length(covariate_names)} covariate(s) but standata has ",
        "{length(c_vars)} C_* entries."
      ))
    }

    # Map C_1, C_2, etc. to actual variable names
    for (i in seq_along(c_vars)) {
      cov_data <- prep$sdata[[c_vars[i]]]
      cov_name <- covariate_names[i]

      # Broadcast covariate across draws to [ndraws × nobs] matrix
      if (is.matrix(cov_data)) {
        # Already a matrix - validate dimensions
        if (nrow(cov_data) != prep$nobs) {
          stop(insight::format_error(
            "Covariate {.field {cov_name}} has {nrow(cov_data)} ",
            "rows but expected {prep$nobs} observations."
          ))
        }
        covariates[[cov_name]] <- cov_data
      } else if (is.array(cov_data) && length(dim(cov_data)) == 1) {
        # Vector - broadcast to matrix
        n_draws <- nrow(prep$draws)
        covariates[[cov_name]] <- matrix(
          cov_data,
          nrow = n_draws,
          ncol = length(cov_data),
          byrow = TRUE
        )
      } else {
        # Scalar or other type - attempt coercion
        covariates[[cov_name]] <- cov_data
      }
    }
  }

  # Build args list for formula evaluation
  args <- c(eta_nlpars, covariates)

  # Evaluate nonlinear formula
  mu <- eval(nlform_expr, envir = args)

  # Validate result dimensions
  if (!is.matrix(mu)) {
    mu <- as.matrix(mu)
  }

  if (nrow(mu) != nrow(prep$draws)) {
    stop(insight::format_error(
      "Formula evaluation produced {nrow(mu)} rows but expected ",
      "{nrow(prep$draws)} draws."
    ))
  }

  if (ncol(mu) != prep$nobs) {
    stop(insight::format_error(
      "Formula evaluation produced {ncol(mu)} columns but ",
      "expected {prep$nobs} observations."
    ))
  }

  return(list(mu = mu))
}
