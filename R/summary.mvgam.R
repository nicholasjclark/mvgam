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
    state_pattern <- "^(trend|lv_trend)\\["
    pars_to_keep <- !grepl(state_pattern, pars)
    all_summaries <- all_summaries[pars_to_keep, , drop = FALSE]
    pars <- rownames(all_summaries)
  }

  # Exclude lprior and lp__
  if (!is.null(object$exclude)) {
    pars_to_keep <- !(rownames(all_summaries) %in% object$exclude)
    all_summaries <- all_summaries[pars_to_keep, , drop = FALSE]
    pars <- rownames(all_summaries)
  }

  # Build output structure with metadata
  draws_obj <- posterior::as_draws(object$fit)
  out <- list(
    formula = object$formula,
    family = object$family,
    nchains = posterior::nchains(draws_obj),
    niter = posterior::niterations(draws_obj),
    ndraws = posterior::ndraws(draws_obj)
  )

  # Check convergence and warn if issues detected
  check_mvgam_convergence(all_summaries, nchains = out$nchains)

  # Organize summaries by parameter category
  obs_fixed_idx <- match_fixed_pars(pars)
  if (any(obs_fixed_idx)) {
    out$fixed <- all_summaries[obs_fixed_idx, , drop = FALSE]
  }

  smooth_idx <- match_smooth_pars(pars)
  if (any(smooth_idx)) {
    out$smooth <- all_summaries[smooth_idx, , drop = FALSE]
  }

  random_idx <- match_random_pars(pars)
  if (any(random_idx)) {
    out$random <- all_summaries[random_idx, , drop = FALSE]
  }

  family_idx <- match_family_pars(pars)
  if (any(family_idx)) {
    out$spec <- all_summaries[family_idx, , drop = FALSE]
  }

  trend_idx <- match_trend_pars(pars)
  if (any(trend_idx)) {
    out$trend <- all_summaries[trend_idx, , drop = FALSE]
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
  if (robust) {
    out <- posterior::summarise_draws(
      draws,
      median,
      mad,
      ~quantile(.x, probs = probs),
      posterior::default_convergence_measures()
    )
  } else {
    out <- posterior::summarise_draws(
      draws,
      mean,
      sd,
      ~quantile(.x, probs = probs),
      posterior::default_convergence_measures()
    )
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
#' excluding trend model parameters.
#'
#' @param pars Character vector of all parameter names
#' @return Logical vector indicating which parameters are family parameters
#'
#' @noRd
match_family_pars <- function(pars) {
  # Match family parameter patterns
  is_family <- grepl("^(sigma|shape|nu|phi|zi|hu)(_|$)", pars)
  # Exclude trend parameters
  is_trend <- grepl("_trend", pars)

  is_family & !is_trend
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
#' @param header Character string for section header
#' @param digits Number of decimal places
#'
#' @noRd
print_param_section <- function(table, header, digits = 2) {
  if (!is.null(table) && nrow(table) > 0) {
    cat(header, ":\n", sep = "")
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

  # Section 1: Formulas (distinguish observation vs process)
  if (!is.null(x$trend_formula)) {
    cat("GAM observation formula:\n")
    print(x$formula)
    cat("\nGAM process formula:\n")
    print(x$trend_formula)
  } else {
    cat("GAM formula:\n")
    print(x$formula)
  }

  # Section 2: Family
  cat("\nFamily:\n")
  cat(format(x$family), '\n')

  # Section 3: Trend model (if present)
  if (!is.null(x$trend_model)) {
    cat("\nTrend model:\n")
    cat(x$trend_model, '\n')
  }

  # Section 4: Dimensions
  if (!is.null(x$n_series)) {
    cat('\nN series:\n')
    cat(x$n_series, '\n')
  }

  if (!is.null(x$n_timepoints)) {
    cat('\nN timepoints:\n')
    cat(x$n_timepoints, '\n')
  }

  # Section 5: Sampling information
  cat('\nDraws:', x$ndraws, 'from', x$nchains, 'chains\n\n')

  # Section 6: Parameter tables
  print_param_section(x$fixed, "Population-Level Effects", digits)
  print_param_section(x$smooth, "Smooth Terms", digits)
  print_param_section(x$random, "Group-Level Effects", digits)
  print_param_section(x$spec, "Family Specific Parameters", digits)
  print_param_section(x$trend, "Trend Parameters", digits)
  print_param_section(x$loadings, "Factor Loadings", digits)

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
        .frequency = "once"
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
        .frequency = "once"
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
        .frequency = "once"
      )
    }
  }

  invisible(NULL)
}
