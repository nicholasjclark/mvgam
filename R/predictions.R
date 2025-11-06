#' Extract Linear Predictor from Prep Object
#'
#' Computes linear predictors (on link scale) from a brmsprep object using
#' fully vectorized matrix operations. Supports fixed effects for both
#' univariate and multivariate models.
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
#' Computes eta = Intercept + X * b via vectorized matrix operations.
#' Intercept is stored as a separate parameter; X matrix contains only
#' non-intercept predictors.
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

    result[[resp_name]] <- eta
  }

  # Return single matrix if resp was specified, otherwise list
  if (!is.null(resp)) {
    return(result[[1]])
  } else {
    return(result)
  }
}
