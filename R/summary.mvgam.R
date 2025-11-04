# ==============================================================================
# MVGAM SUMMARY METHODS
# ==============================================================================
# Summary methods for mvgam fitted models following brms conventions.
# Uses posterior package for all computation, organized by parameter category.

#' Comprehensive summary of mvgam model fits
#'
#' @description
#' Provides posterior summary statistics for mvgam model parameters, organized
#' by category (fixed effects, smooth terms, family parameters, trend parameters,
#' and factor loadings).
#'
#' @param object An object of class \code{mvgam}.
#' @param probs Numeric vector of length 2 specifying quantile probabilities
#'   for credible intervals. Default is \code{c(0.025, 0.975)} for 95% intervals.
#' @param robust Logical; if \code{TRUE}, use median and MAD instead of mean
#'   and SD as measures of central tendency and spread. Default is \code{FALSE}.
#' @param include_states Logical; if \code{TRUE}, include latent state parameters
#'   (\code{trend[i,s]}, \code{lv_trend[i,k]}) in summary. Default is \code{FALSE} as these
#'   are typically too numerous for display.
#' @param ... Additional arguments (currently unused).
#'
#' @return An object of class \code{summary.mvgam} containing:
#' \itemize{
#'   \item{\code{fixed}}{Fixed effect parameter summaries}
#'   \item{\code{smooth}}{Smooth (GAM) parameter summaries}
#'   \item{\code{spec}}{Family-specific parameter summaries (sigma, shape, etc.)}
#'   \item{\code{trend}}{Trend model parameter summaries}
#'   \item{\code{loadings}}{Factor loading (Z matrix) summaries}
#'   \item{Metadata}{Formula, family, sampling information}
#' }
#'
#' @details
#' The summary includes the following columns:
#' \itemize{
#'   \item{\code{Estimate}}{Posterior mean (or median if robust = TRUE)}
#'   \item{\code{Est.Error}}{Posterior SD (or MAD if robust = TRUE)}
#'   \item{Quantile bounds}{Credible interval limits based on probs argument}
#'   \item{\code{Rhat}}{Potential scale reduction factor (should be < 1.05)}
#'   \item{\code{Bulk_ESS}}{Bulk effective sample size}
#'   \item{\code{Tail_ESS}}{Tail effective sample size}
#' }
#'
#' @export
summary.mvgam <- function(object, probs = c(0.025, 0.975),
                          robust = FALSE, include_states = FALSE, ...) {
  # Input validation
  checkmate::assert_class(object, "mvgam")
  checkmate::assert_numeric(probs, len = 2, lower = 0, upper = 1)
  checkmate::assert_true(probs[1] < probs[2])
  checkmate::assert_logical(robust, len = 1)
  checkmate::assert_logical(include_states, len = 1)

  # Check for fitted model
  if (is.null(object$fit)) {
    stop(
      insight::format_error(
        "No fitted model found in mvgam object.",
        "The model may not have been fitted successfully."
      )
    )
  }

  # Compute summaries once for efficiency (then filter by category)
  all_summaries <- compute_all_summaries(
    object,
    probs = probs,
    robust = robust
  )

  # Get all parameter names from rownames
  pars <- rownames(all_summaries)

  # Exclude latent states unless requested
  if (!include_states) {
    pars_to_keep <- !is_latent_state_param(pars)
    all_summaries <- all_summaries[pars_to_keep, , drop = FALSE]
    pars <- rownames(all_summaries)
  }

  # Exclude lprior, lp__, and uncentered b_Intercept_trend
  # brms shows only centered Intercept_trend in summaries, hiding the
  # uncentered b_Intercept_trend generated quantity used for back-transformation
  exclude_pars <- c(object$exclude, "b_Intercept_trend")
  pars_to_keep <- !(rownames(all_summaries) %in% exclude_pars)
  all_summaries <- all_summaries[pars_to_keep, , drop = FALSE]
  pars <- rownames(all_summaries)

  # Build output structure with metadata
  draws_obj <- posterior::as_draws(object$fit)
  out <- list(
    formula = object$formula,
    family = object$family,
    nchains = posterior::nchains(draws_obj),
    niter = posterior::niterations(draws_obj),
    ndraws = posterior::ndraws(draws_obj)
  )

  # Organize summaries by observation vs trend formula
  # First identify which parameters belong to trend model
  is_trend_param <- grepl("_trend", pars)

  # Detect distributional parameters (those with formulas like sigma ~ x)
  dpars_with_formulas <- get_dpar_names(pars)

  # Observation model parameters (no _trend suffix, no dpar prefix)
  obs_fixed_idx <- match_fixed_pars(pars) & !is_trend_param
  if (any(obs_fixed_idx)) {
    out$fixed <- all_summaries[obs_fixed_idx, , drop = FALSE]
    # Clean names: b_Intercept → Intercept, b_x → x
    rownames(out$fixed) <- gsub("^b_", "", rownames(out$fixed))
  }

  obs_smooth_idx <- match_smooth_pars(pars) & !is_trend_param
  if (any(obs_smooth_idx)) {
    out$smooth <- all_summaries[obs_smooth_idx, , drop = FALSE]
  }

  obs_random_idx <- match_random_pars(pars) & !is_trend_param
  if (any(obs_random_idx)) {
    out$random <- all_summaries[obs_random_idx, , drop = FALSE]
  }

  # Distributional parameters (b_{dpar}_* pattern, following brms)
  if (length(dpars_with_formulas) > 0) {
    for (dpar in dpars_with_formulas) {
      # Fixed effects for this dpar
      dpar_fixed_idx <- match_dpar_fixed_pars(pars, dpar)
      if (any(dpar_fixed_idx)) {
        out[[paste0("dpar_", dpar, "_fixed")]] <-
          all_summaries[dpar_fixed_idx, , drop = FALSE]
        # Clean names: b_sigma_Intercept → Intercept, b_sigma_x → x
        rownames(out[[paste0("dpar_", dpar, "_fixed")]]) <-
          gsub(paste0("^b_", dpar, "_"), "",
               rownames(out[[paste0("dpar_", dpar, "_fixed")]]))
      }

      # Smooths for this dpar
      dpar_smooth_idx <- match_dpar_smooth_pars(pars, dpar)
      if (any(dpar_smooth_idx)) {
        out[[paste0("dpar_", dpar, "_smooth")]] <-
          all_summaries[dpar_smooth_idx, , drop = FALSE]
      }
    }
  }

  # Family-specific parameters WITHOUT formulas (pass dpars to exclude them)
  family_idx <- match_family_pars(pars, has_dpar_formulas = dpars_with_formulas)
  if (any(family_idx)) {
    out$spec <- all_summaries[family_idx, , drop = FALSE]
  }

  # Trend model parameters (subdivided by type)
  trend_fixed_idx <- match_trend_fixed_pars(pars)
  if (any(trend_fixed_idx)) {
    out$trend_fixed <- all_summaries[trend_fixed_idx, , drop = FALSE]
    # Clean names: Intercept_trend → Intercept
    rownames(out$trend_fixed) <- gsub("_trend$", "",
                                       rownames(out$trend_fixed))
  }

  trend_smooth_idx <- match_trend_smooth_pars(pars)
  if (any(trend_smooth_idx)) {
    out$trend_smooth <- all_summaries[trend_smooth_idx, , drop = FALSE]
  }

  trend_random_idx <- match_trend_random_pars(pars)
  if (any(trend_random_idx)) {
    out$trend_random <- all_summaries[trend_random_idx, , drop = FALSE]
  }

  trend_spec_idx <- match_trend_specific_pars(pars)
  if (any(trend_spec_idx)) {
    out$trend_spec <- all_summaries[trend_spec_idx, , drop = FALSE]
  }

  z_idx <- match_z_loadings(pars)
  if (any(z_idx)) {
    out$loadings <- all_summaries[z_idx, , drop = FALSE]
  }

  # Store mvgam-specific metadata for print.summary.mvgam() to display
  # model structure information (formula, trend type, dimensions)
  out$trend_formula <- object$trend_formula

  trend_comps <- object$trend_components
  out$trend_model <- if (!is.null(trend_comps) &&
                         !is.null(trend_comps$types)) {
    trend_comps$types[1]
  } else {
    NULL
  }

  out$n_series <- if (!is.null(object$series_info)) {
    object$series_info$n_series
  } else {
    NULL
  }

  out$n_timepoints <- if (!is.null(object$time_info)) {
    object$time_info$n_timepoints
  } else {
    NULL
  }

  # Store data name (captured at top-level mvgam() call)
  out$data_name <- object$data.name

  structure(out, class = "mvgam_summary")
}

# ==============================================================================
# CORE COMPUTATION FUNCTION (Internal Helper)
# ==============================================================================

#' Compute posterior summaries for all parameters
#'
#' @description
#' Internal helper that computes summaries once for all parameters using
#' posterior package, then returns the full result for filtering by category.
#' This is more efficient than multiple calls to posterior::summarise_draws().
#'
#' @param object An mvgam fitted object
#' @param probs Quantile probabilities (length 2)
#' @param robust Logical for median/MAD vs mean/SD
#'
#' @return Data frame with columns: variable, Estimate, Est.Error, quantiles,
#'   Rhat, Bulk_ESS, Tail_ESS
#'
#' @noRd
compute_all_summaries <- function(object, probs, robust) {
  # Extract draws
  draws <- posterior::as_draws_df(object$fit)

  # Compute summaries using posterior package
  # Suppress ESS capping warnings (users can call diagnostics functions if needed)
  if (robust) {
    out <- suppressWarnings(posterior::summarise_draws(
      draws,
      median,
      mad,
      ~quantile(.x, probs = probs),
      posterior::default_convergence_measures()
    ))
  } else {
    out <- suppressWarnings(posterior::summarise_draws(
      draws,
      mean,
      sd,
      ~quantile(.x, probs = probs),
      posterior::default_convergence_measures()
    ))
  }

  # Rename columns for display
  names(out) <- rename_summary_cols(names(out), probs, robust)

  # Move parameter names from column to rownames for cleaner printing
  out <- as.data.frame(out)
  rownames(out) <- out$variable
  out$variable <- NULL

  out
}

# ==============================================================================
# COLUMN RENAMING
# ==============================================================================

#' Rename summary columns for display
#'
#' @description
#' Renames columns from posterior::summarise_draws() to match brms conventions.
#'
#' @param col_names Character vector of column names
#' @param probs Quantile probabilities used
#' @param robust Whether robust estimation was used
#'
#' @return Character vector with renamed columns
#'
#' @noRd
rename_summary_cols <- function(col_names, probs, robust) {
  # Rename location and spread measures
  if (robust) {
    col_names[col_names == "median"] <- "Estimate"
    col_names[col_names == "mad"] <- "Est.Error"
  } else {
    col_names[col_names == "mean"] <- "Estimate"
    col_names[col_names == "sd"] <- "Est.Error"
  }

  # Rename convergence diagnostics
  col_names[col_names == "rhat"] <- "Rhat"
  col_names[col_names == "ess_bulk"] <- "Bulk_ESS"
  col_names[col_names == "ess_tail"] <- "Tail_ESS"

  # Rename quantile columns to credible interval format
  # posterior returns "2.5%", "97.5%" etc., convert to "l-95% CI", "u-95% CI"
  # CI coverage = (upper - lower) * 100 = (0.975 - 0.025) * 100 = 95
  ci_coverage <- (probs[2] - probs[1]) * 100
  ci_label <- paste0(ci_coverage, "% CI")

  for (i in seq_along(probs)) {
    q_col <- paste0(probs[i] * 100, "%")
    prefix <- if (i == 1) "l-" else "u-"
    new_name <- paste0(prefix, ci_label)

    if (q_col %in% col_names) {
      col_names[col_names == q_col] <- new_name
    }
  }

  col_names
}

# ==============================================================================
# PARAMETER MATCHING FUNCTIONS (Pattern-Based)
# ==============================================================================

#' Match fixed effect parameter names
#'
#' @description
#' Identifies fixed effect parameters (b_*) excluding trend formula effects.
#'
#' @param pars Character vector of all parameter names
#' @return Logical vector indicating which parameters are fixed effects
#'
#' @noRd
match_fixed_pars <- function(pars) {
  # Get all b_ parameters
  is_b_par <- grepl("^b_", pars)
  # Exclude trend formula parameters
  is_trend <- grepl("_trend", pars)

  is_b_par & !is_trend
}

#' Match smooth parameter names
#'
#' @description
#' Identifies GAM smooth parameters (s_* and sds_*).
#'
#' @param pars Character vector of all parameter names
#' @return Logical vector indicating which parameters are smooth terms
#'
#' @noRd
match_smooth_pars <- function(pars) {
  grepl("^s(ds)?_", pars)
}

#' Match random effect parameter names
#'
#' @description
#' Identifies random effect SDs (sd_*) and individual effects (r_*).
#'
#' @param pars Character vector of all parameter names
#' @return Logical vector indicating which parameters are random effects
#'
#' @noRd
match_random_pars <- function(pars) {
  grepl("^(sd_|r_)", pars)
}

#' Match family parameter names (observation model only)
#'
#' @description
#' Identifies family-specific parameters (sigma, shape, nu, phi, zi, hu)
#' excluding trend model parameters and distributional parameters that have
#' formulas (e.g., if sigma ~ x exists, exclude sigma[1]).
#'
#' @param pars Character vector of all parameter names
#' @param has_dpar_formulas Character vector of distributional parameter names
#'   that have formulas (from get_dpar_names())
#' @return Logical vector indicating which parameters are family parameters
#'
#' @noRd
match_family_pars <- function(pars, has_dpar_formulas = character()) {
  checkmate::assert_character(pars)
  checkmate::assert_character(has_dpar_formulas)

  # Match family parameter patterns
  is_family <- grepl("^(sigma|shape|nu|phi|zi|hu)(_|\\[|$)", pars)
  # Exclude trend parameters
  is_trend <- grepl("_trend", pars)

  # Exclude distributional parameters that have formulas
  is_dpar_with_formula <- FALSE
  if (length(has_dpar_formulas) > 0) {
    # If sigma has a formula (b_sigma_*), exclude sigma[1], sigma[2], etc.
    pattern <- paste0("^(", paste(has_dpar_formulas, collapse = "|"), ")(\\[|$)")
    is_dpar_with_formula <- grepl(pattern, pars)
  }

  is_family & !is_trend & !is_dpar_with_formula
}

#' Match trend parameter names
#'
#' @description
#' Identifies all trend model parameters (those with _trend suffix),
#' excluding latent states which are too numerous for default display.
#'
#' @param pars Character vector of all parameter names
#' @return Logical vector indicating which parameters are trend parameters
#'
#' @noRd
match_trend_pars <- function(pars) {
  # Get all parameters with _trend suffix
  is_trend <- grepl("_trend", pars)
  # Exclude latent states (handled separately with include_states argument)
  is_state <- grepl("^(trend|lv_trend)\\[", pars)

  is_trend & !is_state
}

#' Match trend fixed effect parameter names
#'
#' @description
#' Identifies population-level effects for trend formula, following brms
#' convention of showing only centered Intercept_trend (not b_Intercept_trend).
#'
#' @param pars Character vector of all parameter names
#' @return Logical vector indicating which parameters are trend fixed effects
#'
#' @noRd
match_trend_fixed_pars <- function(pars) {
  checkmate::assert_character(pars)
  if (length(pars) == 0) return(logical(0))

  # Match centered Intercept_trend (brms shows only centered, not b_Intercept_trend)
  is_intercept <- pars == "Intercept_trend"

  # Match b_trend[i] array elements (coefficients for predictors)
  is_coef <- grepl("^b_trend\\[", pars)

  is_intercept | is_coef
}

#' Match trend smooth parameter names
#'
#' @description
#' Identifies smooth terms (GAM splines) in trend formula.
#'
#' @param pars Character vector of all parameter names
#' @return Logical vector indicating which parameters are trend smooths
#'
#' @noRd
match_trend_smooth_pars <- function(pars) {
  checkmate::assert_character(pars)
  if (length(pars) == 0) return(logical(0))
  grepl("^s(ds)?_.*_trend", pars)
}

#' Match trend random effect parameter names
#'
#' @description
#' Identifies group-level effects (random effects) in trend formula.
#'
#' @param pars Character vector of all parameter names
#' @return Logical vector indicating which parameters are trend random effects
#'
#' @noRd
match_trend_random_pars <- function(pars) {
  checkmate::assert_character(pars)
  if (length(pars) == 0) return(logical(0))
  grepl("^(sd_|r_).*_trend", pars)
}

#' Match trend-specific parameter names
#'
#' @description
#' Identifies trend dynamics parameters (sigma_trend, ar1_trend, etc.) that
#' aren't fixed/smooth/random effects.
#'
#' @param pars Character vector of all parameter names
#' @return Logical vector indicating which parameters are trend-specific
#'
#' @noRd
match_trend_specific_pars <- function(pars) {
  checkmate::assert_character(pars)
  if (length(pars) == 0) return(logical(0))

  # Parameters with _trend that aren't formula effects or states
  is_trend <- grepl("_trend", pars)

  # Not fixed effects (not Intercept_trend or b_trend[i])
  is_not_fixed <- pars != "Intercept_trend" & !grepl("^b_trend\\[", pars)

  # Not smooths (not s_*_trend or sds_*_trend)
  is_not_smooth <- !grepl("^s(ds)?_.*_trend", pars)

  # Not random effects (not sd_*_trend or r_*_trend)
  is_not_random <- !grepl("^(sd_|r_).*_trend", pars)

  # Not latent states
  is_not_state <- !is_latent_state_param(pars)

  is_trend & is_not_fixed & is_not_smooth & is_not_random & is_not_state
}

#' Match factor loading parameter names
#'
#' @description
#' Identifies factor loading matrix parameters (Z[i,j]).
#'
#' @param pars Character vector of all parameter names
#' @return Logical vector indicating which parameters are factor loadings
#'
#' @noRd
match_z_loadings <- function(pars) {
  grepl("^Z\\[", pars)
}

#' Get distributional parameter names
#'
#' @description
#' Extracts unique distributional parameter names from parameter vector
#' following brms pattern (b_sigma_*, b_phi_*, etc.). Returns names of
#' distributional parameters that have formulas.
#'
#' @param pars Character vector of parameter names
#' @return Character vector of unique distributional parameter names
#'
#' @noRd
get_dpar_names <- function(pars) {
  checkmate::assert_character(pars, min.len = 0)

  # Match b_{dpar}_ pattern (e.g., b_sigma_Intercept, b_phi_x)
  # Exclude trend parameters (b_trend)
  dpar_pars <- pars[grepl("^b_[a-z][a-z0-9_]*_", pars) & !grepl("_trend", pars)]

  if (length(dpar_pars) == 0) {
    return(character(0))
  }

  # Extract dpar names (sigma, phi, nu, etc.)
  dpars <- gsub("^b_([a-z][a-z0-9_]*)_.*", "\\1", dpar_pars)
  dpars <- unique(dpars)
  checkmate::assert_character(dpars, min.chars = 1, any.missing = FALSE)

  dpars
}

#' Match distributional parameter fixed effects
#'
#' @description
#' Identifies fixed effects for a specific distributional parameter
#' (e.g., b_sigma_Intercept, b_sigma_x for sigma).
#'
#' @param pars Character vector of parameter names
#' @param dpar Distributional parameter name (e.g., "sigma", "phi")
#' @return Logical vector indicating matching parameters
#'
#' @noRd
match_dpar_fixed_pars <- function(pars, dpar) {
  checkmate::assert_character(pars, min.len = 0)
  checkmate::assert_string(dpar, min.chars = 1)
  if (length(pars) == 0) return(logical(0))

  # Match b_{dpar}_* pattern
  grepl(paste0("^b_", dpar, "_"), pars)
}

#' Match distributional parameter smooth terms
#'
#' @description
#' Identifies smooth terms for distributional parameters
#' (e.g., s_sigma_x_1[1] for sigma ~ s(x)).
#'
#' @param pars Character vector of parameter names
#' @param dpar Distributional parameter name (e.g., "sigma", "phi")
#' @return Logical vector indicating matching parameters
#'
#' @noRd
match_dpar_smooth_pars <- function(pars, dpar) {
  checkmate::assert_character(pars, min.len = 0)
  checkmate::assert_string(dpar, min.chars = 1)
  if (length(pars) == 0) return(logical(0))

  # Match s_{dpar}_* or sds_{dpar}_* patterns
  grepl(paste0("^s(ds)?_", dpar, "_"), pars)
}

#' Identify latent state parameters
#'
#' @description
#' Helper to identify time-indexed latent state parameters from state-space
#' model dynamics that should be excluded from summary output by default.
#' These are the actual trend evolution states and intermediate computations
#' that are numerous and typically not of direct interest. Excludes
#' hyperparameters (sigma_trend, ar1_trend) and trend formula effects
#' (Intercept_trend, b_*_trend) which are summarized separately.
#'
#' @param pars Character vector of parameter names
#' @return Logical vector indicating which parameters are latent states
#'
#' @details
#' Matches the following array-indexed parameters:
#' \itemize{
#'   \item{\code{trend[i,s]}}{Main state matrix for each series}
#'   \item{\code{lv_trend[i,k]}}{Latent variable states}
#'   \item{\code{innovations_trend[i,s]}}{Raw innovations}
#'   \item{\code{mu_trend[i]}}{Trend formula linear predictor}
#'   \item{\code{scaled_innovations_trend[i,s]}}{Scaled innovations}
#' }
#'
#' @noRd
is_latent_state_param <- function(pars) {
  checkmate::assert_character(pars)

  grepl(
    "^(trend|lv_trend|innovations_trend|mu_trend|scaled_innovations_trend)\\[",
    pars
  )
}

# ==============================================================================
# HELPER FUNCTIONS FOR PRINT METHOD
# ==============================================================================

#' Round numeric columns in a data frame
#'
#' @description
#' Rounds only numeric columns while preserving non-numeric columns and rownames.
#'
#' @param x Data frame or numeric object to round
#' @param digits Number of decimal places
#'
#' @return Object with numeric columns rounded
#'
#' @noRd
round_numeric <- function(x, digits = 2) {
  if (is.data.frame(x)) {
    num_cols <- sapply(x, is.numeric)
    x[num_cols] <- lapply(x[num_cols], round, digits = digits)
  } else if (is.numeric(x)) {
    x <- round(x, digits = digits)
  }
  x
}

#' Print a parameter table section if present
#'
#' @description
#' Helper to print a parameter table with header if the table exists.
#'
#' @param table Data frame or NULL
#' @param header Character string for section header or NULL for no header
#' @param digits Number of decimal places
#'
#' @noRd
print_param_section <- function(table, header, digits = 2) {
  if (!is.null(table) && nrow(table) > 0) {
    if (!is.null(header)) {
      cat(header, ":\n", sep = "")
    }
    print(round_numeric(table, digits = digits), quote = FALSE)
    cat("\n")
  }
}

# ==============================================================================
# PRINT METHOD
# ==============================================================================

#' Print method for mvgam_summary objects
#'
#' @param x An object of class \code{mvgam_summary}.
#' @param digits Integer for decimal places. Default is 2.
#' @param ... Additional arguments (currently unused).
#'
#' @return The \code{mvgam_summary} object invisibly.
#'
#' @export
print.mvgam_summary <- function(x, digits = 2, ...) {
  checkmate::assert_class(x, "mvgam_summary")
  checkmate::assert_int(digits, lower = 0)

  # Header formatting (brms style with fixed padding)

  # Section 1: Family and Links (aligned with fixed spacing)
  # Detect multivariate models by checking for mvbrmsformula with multiple forms
  is_multivariate <- inherits(x$formula, "mvbrmsformula") &&
    !is.null(x$formula$forms) &&
    length(x$formula$forms) > 1

  if (is_multivariate) {
    # Multivariate model with response-specific families and links
    # Extract response names and family info from formula$forms
    resp_names <- x$formula$responses
    families <- sapply(x$formula$forms, function(f) f$family$family)
    links <- sapply(x$formula$forms, function(f) f$family$link)

    # Format families following brms convention
    # Pattern: "resp1: family1 \n          resp2: family2"
    # Separator: " \n          " (space + newline + 10 spaces)
    family_str <- paste0(resp_names, ": ", families)
    family_str <- paste0(family_str, collapse = " \n          ")
    cat("Family: ", family_str, " \n", sep = "")

    # Format links following brms convention
    # Pattern: "resp1: link1 \n        resp2: link2"
    # Separator: " \n        " (space + newline + 8 spaces)
    link_str <- paste0(resp_names, ": ", links)
    link_str <- paste0(link_str, collapse = " \n        ")
    cat("Links:  ", link_str, " \n", sep = "")
  } else {
    # Univariate model
    cat("Family: ", x$family$family, " \n", sep = "")
    cat("  Links: mu = ", x$family$link, " \n", sep = "")
  }

  # Section 2: Formula
  if (is_multivariate) {
    # For multivariate, format each response formula separately
    # Extract formula from each form (not the entire form object)
    formulas <- sapply(x$formula$forms, function(f) format(f$formula))
    # Join with newline + 9 spaces to align with "Formula: "
    formulas_str <- paste0(formulas, collapse = " \n         ")
    cat("Formula: ", formulas_str, " \n", sep = "")
  } else {
    cat("Formula: ", format(x$formula), " \n", sep = "")
  }

  # Section 3: Data and dimensions (brms style)
  nobs <- x$n_series * x$n_timepoints
  if (!is.null(x$data_name)) {
    cat("   Data: ", x$data_name, " (Number of observations: ", nobs, ") \n",
        sep = "")
  } else {
    cat("   Data: (Number of observations: ", nobs, ") \n", sep = "")
  }

  # Add series count if multiple series
  if (!is.null(x$n_series) && x$n_series > 1) {
    cat(" Series: ", x$n_series, " \n", sep = "")
  }

  # Section 4: Trend information
  if (!is.null(x$trend_formula) || !is.null(x$trend_model)) {
    if (!is.null(x$trend_model)) {
      cat(" Trends: ", x$trend_model, "()", sep = "")
      # Add trend formula if it has predictors
      if (!is.null(x$trend_formula)) {
        trend_rhs <- if (length(x$trend_formula) == 3) {
          formula(delete.response(terms(x$trend_formula)))
        } else {
          x$trend_formula
        }
        trend_str <- format(trend_rhs)
        # Only show if not just ~1
        if (!grepl("^~\\s*1\\s*$", trend_str)) {
          cat("; formula: ", trend_str, sep = "")
        }
      }
      cat(" \n", sep = "")
    }
  }

  # Section 5: Sampling information (brms style with continuation line)
  warmup <- floor(x$niter / 2)
  cat(" Draws: ", x$nchains, " chains, each with iter = ", x$niter,
      "; warmup = ", warmup, "; thin = 1; \n", sep = "")
  cat("         total post-warmup draws = ", x$ndraws, "\n\n", sep = "")

  # Section 6: Observation Model Parameters
  has_obs_params <- !is.null(x$fixed) || !is.null(x$smooth) ||
                     !is.null(x$random)

  if (has_obs_params) {
    cat("== Observation Model ==\n")
    print_param_section(x$fixed, "Population-Level Effects", digits)
    print_param_section(x$smooth, "Smooth Terms", digits)
    print_param_section(x$random, "Group-Level Effects", digits)
  }

  # Section 7: Distributional Parameters (brms style)
  # Find all dpar sections (dpar_{name}_fixed, dpar_{name}_smooth)
  dpar_sections <- names(x)[grepl("^dpar_", names(x))]
  if (length(dpar_sections) > 0) {
    # Extract unique dpar names
    dpar_names <- unique(gsub("^dpar_([^_]+)_.*", "\\1", dpar_sections))

    for (dpar_name in dpar_names) {
      fixed_key <- paste0("dpar_", dpar_name, "_fixed")
      smooth_key <- paste0("dpar_", dpar_name, "_smooth")

      if (!is.null(x[[fixed_key]]) || !is.null(x[[smooth_key]])) {
        cat(paste0("Coefficients for '", dpar_name, "':\n"))
        if (!is.null(x[[fixed_key]])) {
          print(round_numeric(x[[fixed_key]], digits), quote = FALSE)
          cat("\n")
        }
        if (!is.null(x[[smooth_key]])) {
          print(round_numeric(x[[smooth_key]], digits), quote = FALSE)
          cat("\n")
        }
      }
    }
  }

  # Section 8: Further Distributional Parameters (brms convention)
  # These are family-specific parameters WITHOUT formulas
  print_param_section(x$spec, "Further Distributional Parameters", digits)

  # Section 9: Trend Model Parameters
  has_trend_params <- !is.null(x$trend_fixed) || !is.null(x$trend_smooth) ||
                      !is.null(x$trend_random) || !is.null(x$trend_spec) ||
                      !is.null(x$loadings)

  if (has_trend_params) {
    cat("== Trend Model ==\n")
    print_param_section(x$trend_fixed, "Population-Level Effects", digits)
    print_param_section(x$trend_smooth, "Smooth Terms", digits)
    print_param_section(x$trend_random, "Group-Level Effects", digits)
    print_param_section(x$trend_spec, "Trend Specific Parameters", digits)
    print_param_section(x$loadings, "Factor Loadings", digits)
  }

  # Section 10: Footer (brms style)
  cat("Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS\n")
  cat("and Tail_ESS are effective sample size measures, and Rhat is the potential\n")
  cat("scale reduction factor on split chains (at convergence, Rhat = 1).\n")

  invisible(x)
}

# ==============================================================================
# CONVERGENCE CHECKING (Internal Helper)
# ==============================================================================

#' Check convergence diagnostics and issue warnings
#'
#' @description
#' Checks Rhat and ESS diagnostics from posterior summaries and issues
#' warnings if convergence issues are detected. Follows brms pattern of
#' checking thresholds and providing remediation suggestions.
#'
#' @param all_summaries Data frame with posterior summaries including
#'   Rhat, Bulk_ESS, and Tail_ESS columns (after renaming)
#' @param nchains Number of MCMC chains used for fitting
#'
#' @details
#' Checks the following thresholds:
#' \itemize{
#'   \item Rhat > 1.05: Indicates lack of convergence
#'   \item Bulk_ESS < 100 * nchains: Insufficient bulk effective sample
#'     size
#'   \item Tail_ESS < 100 * nchains: Insufficient tail effective sample
#'     size
#' }
#'
#' Warnings are issued at most once per session using
#' rlang::warn(.frequency = "once").
#'
#' @noRd
check_mvgam_convergence <- function(all_summaries, nchains) {
  # Input validation
  checkmate::assert_data_frame(all_summaries)
  checkmate::assert_int(nchains, lower = 1)

  # Suppress warnings during testing
  if (identical(Sys.getenv("TESTTHAT"), "true")) {
    return(invisible(NULL))
  }

  # Extract diagnostic columns (using renamed column names)
  rhats <- all_summaries$Rhat
  bulk_ess <- all_summaries$Bulk_ESS
  tail_ess <- all_summaries$Tail_ESS

  # Check Rhat convergence (threshold: 1.05)
  if (!is.null(rhats) && any(!is.na(rhats))) {
    max_rhat <- max(rhats, na.rm = TRUE)
    if (max_rhat > 1.05) {
      # Find problematic parameters
      bad_pars <- rownames(all_summaries)[rhats > 1.05 & !is.na(rhats)]

      msg_parts <- c(
        sprintf(
          "Some Rhat values exceed 1.05 (max = %.3f).",
          max_rhat
        ),
        "The model may not have converged.",
        sprintf(
          "Problematic parameters (%d): %s",
          length(bad_pars),
          paste(head(bad_pars, 5), collapse = ", ")
        )
      )

      if (length(bad_pars) > 5) {
        msg_parts <- c(
          msg_parts,
          sprintf("... and %d more.", length(bad_pars) - 5)
        )
      }

      msg_parts <- c(
        msg_parts,
        "Consider:",
        "- Increasing 'iter' or 'warmup' arguments",
        "- Increasing 'chains' for better convergence assessment",
        "- Checking for model misspecification"
      )

      rlang::warn(
        insight::format_warning(msg_parts),
        .frequency = "once",
        .frequency_id = "mvgam_rhat_convergence"
      )
    }
  }

  # Check Bulk ESS (threshold: 100 per chain)
  if (!is.null(bulk_ess) && any(!is.na(bulk_ess))) {
    min_bulk <- min(bulk_ess, na.rm = TRUE)
    ess_threshold <- 100 * nchains

    if (min_bulk < ess_threshold) {
      rlang::warn(
        insight::format_warning(
          sprintf(
            "Bulk ESS too low (min = %.0f, should be > %.0f).",
            min_bulk,
            ess_threshold
          ),
          "Parameter estimates may be unreliable.",
          "Consider increasing 'iter' to improve effective sample size."
        ),
        .frequency = "once",
        .frequency_id = "mvgam_bulk_ess"
      )
    }
  }

  # Check Tail ESS (threshold: 100 per chain)
  if (!is.null(tail_ess) && any(!is.na(tail_ess))) {
    min_tail <- min(tail_ess, na.rm = TRUE)
    ess_threshold <- 100 * nchains

    if (min_tail < ess_threshold) {
      rlang::warn(
        insight::format_warning(
          sprintf(
            "Tail ESS too low (min = %.0f, should be > %.0f).",
            min_tail,
            ess_threshold
          ),
          "Tail quantile estimates may be unreliable.",
          "Consider increasing 'iter' to improve tail ESS."
        ),
        .frequency = "once",
        .frequency_id = "mvgam_tail_ess"
      )
    }
  }

  invisible(NULL)
}

# ==============================================================================
# MULTIPLE IMPUTATION SUMMARY METHOD
# ==============================================================================

#' Summary method for multiple imputation mvgam models
#'
#' @description
#' Provides posterior summary statistics for multiple imputation mvgam
#' models, with additional diagnostics about the imputation process and
#' combined posterior draws.
#'
#' @param object An object of class \code{mvgam_pooled}.
#' @param probs Numeric vector of length 2 specifying quantile
#'   probabilities for credible intervals. Default is \code{c(0.025,
#'   0.975)} for 95% intervals.
#' @param robust Logical; if \code{TRUE}, use median and MAD instead of
#'   mean and SD. Default is \code{FALSE}.
#' @param include_states Logical; if \code{TRUE}, include latent state
#'   parameters. Default is \code{FALSE}.
#' @param ... Additional arguments passed to \code{\link{summary.mvgam}}.
#'
#' @return An object of class \code{c("mvgam_pooled_summary",
#'   "mvgam_summary")} containing standard summary components plus
#'   multiple imputation diagnostics.
#'
#' @details
#' This method extends the standard \code{summary.mvgam} output with
#' multiple imputation specific information:
#' \itemize{
#'   \item{Number of imputations}
#'   \item{Total posterior draws (combined across all imputations)}
#'   \item{Per-imputation convergence summaries}
#'   \item{Information about the combination method}
#' }
#'
#' The combined posteriors are created using
#' \code{rstan::sflist2stanfit()}, which concatenates posterior draws
#' from all imputed datasets at the Stan level. Parameter estimates
#' reflect uncertainty from both the model and the imputation process.
#'
#' @seealso \code{\link{summary.mvgam}}, \code{\link{mvgam_multiple}},
#'   \code{\link{pool_mvgam_fits}}
#'
#' @examples
#' \dontrun{
#' # Fit models to multiply imputed datasets
#' pooled_fit <- mvgam_multiple(
#'   y ~ x,
#'   trend_formula = ~ RW(),
#'   data_list = list(imp1, imp2, imp3),
#'   combine = TRUE
#' )
#'
#' # Get enhanced summary with MI diagnostics
#' summary(pooled_fit)
#' }
#'
#' @export
summary.mvgam_pooled <- function(object, probs = c(0.025, 0.975),
                                  robust = FALSE, include_states = FALSE,
                                  ...) {
  # Input validation
  checkmate::assert_class(object, "mvgam_pooled")
  checkmate::assert_numeric(probs, len = 2, lower = 0, upper = 1)
  checkmate::assert_true(probs[1] < probs[2])
  checkmate::assert_logical(robust, len = 1)
  checkmate::assert_logical(include_states, len = 1)

  # Call parent method to get standard summary
  base_summary <- NextMethod("summary")

  # Extract MI-specific metadata from object attributes
  individual_fits <- attr(object, "individual_fits")
  n_imputations <- attr(object, "n_imputations")
  combination_method <- attr(object, "combination_method")

  if (is.null(individual_fits) || is.null(n_imputations)) {
    insight::format_warning(
      "Missing multiple imputation metadata in pooled object.",
      "Summary will proceed without MI diagnostics."
    )

    # Return standard summary if metadata missing
    return(base_summary)
  }

  # Validate consistency between metadata and actual fits
  if (length(individual_fits) != n_imputations) {
    insight::format_warning(
      sprintf(
        "Mismatch: {.field n_imputations} = %d but %d fits stored.",
        n_imputations, length(individual_fits)
      ),
      "Using actual number of stored fits."
    )
    n_imputations <- length(individual_fits)
  }

  # Calculate total draws across all imputations
  total_draws <- posterior::ndraws(posterior::as_draws(object$fit))
  draws_per_imp <- if (length(individual_fits) > 0) {
    posterior::ndraws(posterior::as_draws(individual_fits[[1]]$fit))
  } else {
    NA_integer_
  }

  # Extract per-imputation convergence diagnostics
  imp_convergence <- lapply(seq_along(individual_fits), function(i) {
    fit <- individual_fits[[i]]

    # Safely extract draws and compute diagnostics
    draws <- tryCatch(
      posterior::as_draws(fit$fit),
      error = function(e) NULL
    )

    if (is.null(draws) || posterior::ndraws(draws) == 0) {
      return(list(
        imputation = i,
        max_rhat = NA_real_,
        min_bulk_ess = NA_real_,
        min_tail_ess = NA_real_,
        n_params = NA_integer_,
        error = "Failed to extract draws"
      ))
    }

    # Compute convergence diagnostics only
    summ <- posterior::summarise_draws(
      draws,
      posterior::default_convergence_measures()
    )

    list(
      imputation = i,
      max_rhat = max(summ$rhat, na.rm = TRUE),
      min_bulk_ess = min(summ$ess_bulk, na.rm = TRUE),
      min_tail_ess = min(summ$ess_tail, na.rm = TRUE),
      n_params = nrow(summ)
    )
  })

  # Calculate summary statistics across imputations
  max_rhat_across_imps <- max(
    sapply(imp_convergence, function(x) x$max_rhat, USE.NAMES = FALSE),
    na.rm = TRUE
  )
  min_ess_across_imps <- min(
    sapply(imp_convergence, function(x) x$min_bulk_ess,
           USE.NAMES = FALSE),
    na.rm = TRUE
  )

  # Add MI diagnostics to summary object
  base_summary$mi_diagnostics <- list(
    n_imputations = n_imputations,
    total_draws = total_draws,
    draws_per_imputation = draws_per_imp,
    combination_method = if (is.null(combination_method)) {
      "sflist2stanfit"
    } else {
      combination_method
    },
    per_imputation_convergence = imp_convergence,
    max_rhat_across_imputations = max_rhat_across_imps,
    min_ess_across_imputations = min_ess_across_imps
  )

  # Set enhanced class
  class(base_summary) <- c("mvgam_pooled_summary", "mvgam_summary")

  return(base_summary)
}

# ==============================================================================
# MULTIPLE IMPUTATION PRINT METHOD
# ==============================================================================

#' Print method for multiple imputation summary objects
#'
#' @description
#' Prints a summary of a multiple imputation mvgam model, including
#' standard parameter estimates and multiple imputation diagnostics.
#'
#' @param x An object of class \code{mvgam_pooled_summary}.
#' @param digits Integer indicating number of decimal places. Default 2.
#' @param ... Additional arguments passed to
#'   \code{\link{print.mvgam_summary}}.
#'
#' @return The \code{mvgam_pooled_summary} object is returned
#'   invisibly.
#'
#' @details
#' This method extends \code{print.mvgam_summary} by adding a footer
#' section with multiple imputation diagnostics. The combined
#' posteriors are created using \code{rstan::sflist2stanfit()}, which
#' concatenates draws from all imputations at the Stan level.
#'
#' Diagnostics include:
#' \itemize{
#'   \item{Number of imputations and total draws}
#'   \item{Per-imputation convergence summaries (Rhat, ESS)}
#'   \item{Overall convergence metrics across imputations}
#' }
#'
#' @seealso \code{\link{print.mvgam_summary}},
#'   \code{\link{summary.mvgam_pooled}}
#'
#' @export
print.mvgam_pooled_summary <- function(x, digits = 2, ...) {
  # Input validation
  checkmate::assert_class(x, "mvgam_pooled_summary")
  checkmate::assert_int(digits, lower = 0)

  # Print standard summary using parent method
  NextMethod()

  # Add MI diagnostics section if available
  if (!is.null(x$mi_diagnostics)) {
    cat("\n")
    cat(strrep("=", 70), "\n")
    cat("Multiple Imputation Diagnostics\n")
    cat(strrep("=", 70), "\n\n")

    mi <- x$mi_diagnostics

    # Basic imputation information
    cat("  Number of imputations:", mi$n_imputations, "\n")
    cat("  Total posterior draws:", mi$total_draws, "\n")

    if (!is.na(mi$draws_per_imputation)) {
      cat("  Draws per imputation:", mi$draws_per_imputation, "\n")
    }

    cat("  Combination method:", mi$combination_method, "\n\n")

    # Overall convergence across imputations
    cat("  Convergence Summary Across Imputations:\n")

    if (!is.infinite(mi$max_rhat_across_imputations) &&
        !is.na(mi$max_rhat_across_imputations)) {
      cat(sprintf(
        "    Maximum Rhat: %.3f\n",
        mi$max_rhat_across_imputations
      ))
    }

    if (!is.infinite(mi$min_ess_across_imputations) &&
        !is.na(mi$min_ess_across_imputations)) {
      cat(sprintf(
        "    Minimum Bulk ESS: %.0f\n",
        mi$min_ess_across_imputations
      ))
    }

    # Per-imputation table if available
    if (!is.null(mi$per_imputation_convergence) &&
        length(mi$per_imputation_convergence) > 0) {

      cat("\n  Per-Imputation Convergence:\n")

      # Build table
      imp_df <- do.call(rbind, lapply(
        mi$per_imputation_convergence,
        function(imp) {
          data.frame(
            Imputation = imp$imputation,
            Max_Rhat = sprintf("%.3f", imp$max_rhat),
            Min_Bulk_ESS = sprintf("%.0f", imp$min_bulk_ess),
            Min_Tail_ESS = sprintf("%.0f", imp$min_tail_ess),
            stringsAsFactors = FALSE
          )
        }
      ))

      # Print table with proper alignment
      cat("    ")
      print(imp_df, row.names = FALSE, right = TRUE)
    }

    cat("\n")
    cat(strrep("-", 70), "\n")
    cat("Note: Parameter estimates combine draws from all ",
        "imputations.\n", sep = "")
    cat("      Uncertainty reflects both model and imputation ",
        "process.\n", sep = "")
    cat(strrep("=", 70), "\n")
  }

  invisible(x)
}
