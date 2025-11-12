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
#' @noRd
extract_smooth_coef <- function(draws_mat, smooth_label, resp = NULL,
                                n_basis) {
    # Validate inputs
    checkmate::assert_matrix(draws_mat)
    checkmate::assert_string(smooth_label)
    checkmate::assert_string(resp, null.ok = TRUE)
    checkmate::assert_integerish(n_basis, lower = 1, len = 1)

    # Build parameter name patterns
    if (!is.null(resp)) {
      zs_pattern <- paste0("^zs_", resp, "_", smooth_label, "\\[")
      s_pattern <- paste0("^s_", resp, "_", smooth_label, "\\[")
      sds_pattern <- paste0("^sds_", resp, "_", smooth_label, "\\[")
    } else {
      zs_pattern <- paste0("^zs_", smooth_label, "\\[")
      s_pattern <- paste0("^s_", smooth_label, "\\[")
      sds_pattern <- paste0("^sds_", smooth_label, "\\[")
    }

    # Try standardized form first (zs_* + sds_*)
    zs_names <- grep(zs_pattern, colnames(draws_mat), value = TRUE)

    if (length(zs_names) > 0) {
      # Standardized smooth requires unstandardization
      if (length(zs_names) != n_basis) {
        stop(insight::format_error(
          "Smooth coefficient count mismatch: {length(zs_names)} ",
          "{.field zs} parameters but {n_basis} basis functions."
        ))
      }

      # Extract standardized coefficients [n_draws × n_basis]
      zs_draws <- draws_mat[, zs_names, drop = FALSE]

      # Extract standard deviation (single scalar per smooth)
      sds_names <- grep(sds_pattern, colnames(draws_mat), value = TRUE)

      if (length(sds_names) == 0) {
        stop(insight::format_error(
          "Found {.field zs_*} parameters but missing ",
          "{.field sds_*} standard deviations. Model may be corrupted."
        ))
      }

      if (length(sds_names) != 1) {
        stop(insight::format_error(
          "Expected single {.field sds} parameter but found ",
          "{length(sds_names)}. Check smooth term specification."
        ))
      }

      # Extract sds [n_draws × 1]
      sds_draws <- draws_mat[, sds_names, drop = FALSE]

      # Unstandardize via R broadcasting: [n_draws × n_basis] * [n_draws × 1]
      sm_coef <- zs_draws * sds_draws

      return(sm_coef)
    }

    # Try unstandardized form (s_*)
    s_names <- grep(s_pattern, colnames(draws_mat), value = TRUE)

    if (length(s_names) > 0) {
      # Unstandardized smooth - use directly
      if (length(s_names) != n_basis) {
        stop(insight::format_error(
          "Smooth coefficient count mismatch: {length(s_names)} ",
          "{.field s} parameters but {n_basis} basis functions."
        ))
      }

      return(draws_mat[, s_names, drop = FALSE])
    }

    # No matching coefficients found
    return(NULL)
  }

#' Extract Linear Predictor from Prep Object
#'
#' Computes linear predictors (on link scale) from a brmsprep object using
#' fully vectorized matrix operations. Supports fixed effects and smooth
#' terms for both univariate and multivariate models.
#'
#' @param prep A brmsprep object from prepare_predictions()
#' @param resp Optional response name for multivariate models. If NULL and
#'   model is multivariate, returns named list of matrices (one per response).
#'   If specified, returns matrix for that response only.
#'
#' @return For univariate models: Matrix [ndraws × nobs]
#'   For multivariate models with resp=NULL: Named list of matrices
#'   For multivariate models with resp specified: Matrix [ndraws × nobs]
#'
#' @details
#' Computes eta = Intercept + X * b + smooth terms via vectorized matrix
#' operations. Intercept is stored as a separate parameter; X matrix
#' contains only non-intercept predictors.
#'
#' For multivariate models, loops over responses (not draws), with each
#' response computation fully vectorized.
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


#' Extract Linear Predictor for Univariate Models
#'
#' @param prep A brmsprep object from prepare_predictions()
#'
#' @return Matrix [ndraws × nobs]
#'
#' @noRd
extract_linpred_univariate <- function(prep) {
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

  # Add smooth terms
  smooth_matrices <- grep("^Zs_", names(prep$sdata), value = TRUE)

  for (zs_name in smooth_matrices) {
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

    n_basis <- ncol(Zs)

    # Extract smooth label: Zs_<label>
    smooth_label <- sub("^Zs_", "", zs_name)

    # Extract and unstandardize smooth coefficients
    sm_coef <- extract_smooth_coef(
      draws_mat = draws_mat,
      smooth_label = smooth_label,
      resp = NULL,
      n_basis = n_basis
    )

    # Skip if smooth not present (optional smooths may not exist)
    if (is.null(sm_coef)) {
      next
    }

    # Compute smooth contribution: [n_draws × n_obs]
    eta <- eta + sm_coef %*% t(Zs)
  }

  # Add random effects (vectorized)
  # Each Z_<group>_<term> is a single vector with corresponding
  # r_<group>_<term> parameter vector
  re_vectors <- grep("^Z_", names(prep$sdata), value = TRUE)

  for (z_name in re_vectors) {
    # Extract group and term IDs from Z_<group>_<term> pattern
    parts <- strsplit(z_name, "_")[[1]]
    if (length(parts) < 3) next

    group_id <- parts[2]
    term_id <- parts[3]

    # Get design vector (single column per term in brms)
    Z <- as.vector(prep$sdata[[z_name]])

    if (length(Z) != n_obs) {
      stop(insight::format_error(
        "Random effects design vector {.field {z_name}} has {length(Z)} ",
        "elements but expected {n_obs} observations."
      ))
    }

    # Get grouping indices
    J_name <- paste0("J_", group_id)
    if (!J_name %in% names(prep$sdata)) {
      next
    }

    J <- as.integer(prep$sdata[[J_name]])
    if (length(J) != n_obs) {
      stop(insight::format_error(
        "Grouping indices {.field {J_name}} length {length(J)} ",
        "does not match {n_obs} observations."
      ))
    }

    # Extract all group-level parameters for this term
    # Pattern matches: r_1_1[1], r_1_1[2], etc.
    r_pattern <- paste0("^r_", group_id, "_", term_id, "\\[")
    r_names <- grep(r_pattern, colnames(draws_mat), value = TRUE)

    if (length(r_names) > 0) {
      # Get random effects: [n_draws × n_groups]
      r_draws <- draws_mat[, r_names, drop = FALSE]

      # Vectorized indexing and multiplication following Stan pattern:
      # mu[n] += r_group[J[n]] * Z[n]
      # r_draws[, J] gives [n_draws × n_obs] via column indexing
      # Z broadcast to [n_draws × n_obs] via matrix replication
      eta <- eta + r_draws[, J, drop = FALSE] *
        matrix(Z, nrow = n_draws, ncol = n_obs, byrow = TRUE)
    }
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

    # Add smooth terms for this response
    smooth_pattern <- paste0("^Zs_", resp_name, "_")
    smooth_matrices <- grep(
      smooth_pattern,
      names(prep$sdata),
      value = TRUE
    )

    for (zs_name in smooth_matrices) {
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
          "but expected {n_obs} observations for ",
          "response {.val {resp_name}}."
        ))
      }

      n_basis <- ncol(Zs)

      # Extract smooth label: Zs_<response>_<label>
      smooth_label <- sub(smooth_pattern, "", zs_name)

      # Extract and unstandardize smooth coefficients
      sm_coef <- extract_smooth_coef(
        draws_mat = draws_mat,
        smooth_label = smooth_label,
        resp = resp_name,
        n_basis = n_basis
      )

      # Skip if smooth not present (optional smooths may not exist)
      if (is.null(sm_coef)) {
        next
      }

      # Compute smooth contribution: [n_draws × n_obs]
      eta <- eta + sm_coef %*% t(Zs)
    }

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

    result[[resp_name]] <- eta
  }

  # Return single matrix if resp was specified, otherwise list
  if (!is.null(resp)) {
    return(result[[1]])
  } else {
    return(result)
  }
}
