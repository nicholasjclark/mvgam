# ==============================================================================
# MVGAM CORE: Enhanced Model Fitting and Multiple Imputation Support
# ==============================================================================
# This file consolidates the core mvgam fitting architecture, dual-object
# system, and multiple imputation capabilities. The single-fit dual-object
# architecture enables seamless brms ecosystem integration while preserving
# mvgam-specific State-Space modeling functionality.

# The mvgam function provides unified entry point that handles both
# single datasets and multiple imputation scenarios transparently, ensuring
# consistent behavior across different input types.

#' mvgam Function with Single-Fit Architecture
#'
#' @description
#' mvgam implementation using single-fit dual-object architecture
#' with native multiple imputation support and brms ecosystem integration.
#'
#' @param formula Main observation model formula (supports brms syntax)
#' @param trend_formula Trend formula specification (may be response-specific)
#' @param data Data frame or list of multiply imputed datasets
#' @param backend Stan backend (defaults to "cmdstanr")
#' @param combine Logical, pool multiple imputation results (default TRUE)
#' @param family Family specification (supports all brms families)
#' @param ... Additional arguments passed to Stan fitting
#' @return mvgam object with dual brmsfit-like structure
#' @export
mvgam <- function(formula, trend_formula = NULL, data = NULL,
                           backend = getOption("brms.backend", "cmdstanr"),
                           combine = TRUE, family = gaussian(), ...) {

  checkmate::assert(
    checkmate::check_data_frame(data),
    checkmate::check_list(data, types = "data.frame"),
    .var.name = "data"
  )
  checkmate::assert_character(backend, len = 1)
  checkmate::assert_logical(combine, len = 1)

  # Handle multiple imputation input
  if (is.list(data) && !is.data.frame(data)) {
    if (combine) {
      return(mvgam_multiple(formula, trend_formula, data, backend,
                           combine = TRUE, ...))
    } else {
      return(mvgam_multiple(formula, trend_formula, data, backend,
                           combine = FALSE, ...))
    }
  }

  # Single dataset processing
  mvgam_object <- mvgam_single(
    formula = formula,
    trend_formula = trend_formula,
    data = data,
    backend = backend,
    family = family,
    ...
  )

  return(mvgam_object)
}

# ------------------------------------------------------------------------------
# SINGLE DATASET PROCESSING
# ------------------------------------------------------------------------------
# Core processing pipeline for single datasets using the two-stage Stan
# assembly system with brms ecosystem integration.

#' Process Single Dataset with Enhanced Architecture
#' @param formula Main formula
#' @param trend_formula Trend formula
#' @param data Single data frame
#' @param backend Stan backend
#' @param family Family specification
#' @param ... Additional arguments
#' @return mvgam object
#' @noRd
mvgam_single <- function(formula, trend_formula, data, backend,
                        family, ...) {

  # Create mvgam_formula object for shared processing
  mvgam_formula_obj <- mvgam_formula(formula, trend_formula)
  
  # Use existing shared infrastructure (same as stancode())
  stan_components <- generate_stan_components_mvgam_formula(
    formula = mvgam_formula_obj,
    data = data, 
    family = family,
    backend = backend,
    ...
  )

  # Fit the combined model using backend functions directly
  dots <- list(...)
  
  # Extract fitting parameters with defaults
  algorithm <- dots$algorithm %||% "sampling"
  iter <- dots$iter %||% 2000
  warmup <- dots$warmup %||% (iter %/% 2)
  thin <- dots$thin %||% 1
  chains <- dots$chains %||% 4
  cores <- dots$cores %||% 1
  threads <- dots$threads %||% NULL
  opencl <- dots$opencl %||% NULL
  init <- dots$init %||% "random"
  exclude <- dots$exclude %||% NULL
  seed <- dots$seed %||% sample.int(.Machine$integer.max, 1)
  control <- dots$control %||% NULL
  silent <- dots$silent %||% 1
  future <- dots$future %||% FALSE
  
  # Validate and normalize parameters
  silent <- validate_silent(silent)
  threads <- validate_threads(threads)
  opencl <- validate_opencl(opencl)
  
  # Parse/validate Stan code
  validated_code <- parse_model(
    model = stan_components$combined_components$stancode,
    backend = backend,
    silent = silent
  )
  
  # Compile Stan model
  if (silent < 2) {
    message("Compiling Stan model...")
  }
  compiled_model <- compile_model(
    model = validated_code,
    backend = backend,
    threads = threads,
    opencl = opencl,
    silent = silent
  )
  
  # Fit Stan model
  if (silent < 2) {
    message("Fitting Stan model...")
  }
  combined_fit <- fit_model(
    model = compiled_model,
    backend = backend,
    sdata = stan_components$combined_components$standata,
    algorithm = algorithm,
    iter = iter,
    warmup = warmup,
    thin = thin,
    chains = chains,
    cores = cores,
    threads = threads,
    opencl = opencl,
    init = init,
    exclude = exclude,
    seed = seed,
    control = control,
    silent = silent,
    future = future
  )
  
  # Store backend information for later use
  attr(combined_fit, "backend") <- backend
  attr(combined_fit, "algorithm") <- algorithm
  attr(combined_fit, "mvgam_version") <- utils::packageVersion("mvgam")
  attr(combined_fit, "fit_time") <- Sys.time()

  # Create mvgam object from combined fit - now we have all required components
  mvgam_object <- create_mvgam_from_combined_fit(
    combined_fit = combined_fit,
    obs_setup = stan_components$obs_setup,
    trend_setup = stan_components$trend_setup,
    mv_spec = stan_components$mv_spec,
    trend_metadata = stan_components$trend_metadata
  )

  return(mvgam_object)
}

# ------------------------------------------------------------------------------
# TREND STANVAR EXTRACTION
# ------------------------------------------------------------------------------
# Extracts and generates trend-specific stanvars to enable proper injection
# into the combined Stan model while maintaining compatibility with brms.

# Note: Legacy trend stanvar generation functions have been removed.
# The modern system in stan_assembly.R using generate_trend_injection_stanvars()
# provides the same functionality with better integration.

# ------------------------------------------------------------------------------
# COMBINED STAN CODE GENERATION
# ------------------------------------------------------------------------------
# Orchestrates the combination of observation and trend models into a single
# Stan program while maintaining separate parameterizations for ecosystem
# compatibility.

#' Generate Combined Stan Code and Data Using Modern System
#' @param obs_setup Observation model setup
#' @param trend_setup Trend model setup
#' @param mv_spec Multivariate specification
#' @return List with combined stancode and standata
#' @noRd
generate_combined_stancode_and_data <- function(obs_setup, trend_setup, mv_spec, validate = TRUE) {

  # Extract trend_specs from mv_spec for the new system
  trend_specs <- if (mv_spec$has_trends && !is.null(mv_spec$trend_specs)) {
    # Pass the complete trend_specs (handles both univariate and multivariate)
    mv_spec$trend_specs
  } else {
    NULL
  }

  # Use the enhanced two-stage assembly system
  result <- generate_combined_stancode(
    obs_setup = obs_setup,
    trend_setup = trend_setup,
    trend_specs = trend_specs,
    validate = validate,
    silent = 1
  )

  return(result)
}

# ------------------------------------------------------------------------------
# MODEL FITTING
# ------------------------------------------------------------------------------
# Orchestrates the actual Stan model fitting using the appropriate backend
# while maintaining compatibility with both rstan and cmdstanr.


# ==============================================================================
# MVGAM OBJECT CREATION FROM COMBINED FIT
# ==============================================================================
# Creates mvgam object from combined Stan fit with complete parameter map.

#' Create mvgam Object from Combined Stan Fit
#'
#' @param combined_fit Stan fit object from combined model
#' @param obs_setup Observation model setup components
#' @param trend_setup Trend model setup components
#' @param mv_spec Multivariate trend specification
#' @param trend_metadata Trend metadata for prediction
#' @return mvgam object
#' @noRd
create_mvgam_from_combined_fit <- function(combined_fit, obs_setup,
                                          trend_setup = NULL,
                                          mv_spec = NULL,
                                          trend_metadata = NULL) {
  checkmate::assert_class(combined_fit, "stanfit")
  checkmate::assert_list(obs_setup, names = "named")
  checkmate::assert_list(trend_setup, names = "named", null.ok = TRUE)
  checkmate::assert_list(mv_spec, names = "named", null.ok = TRUE)
  checkmate::assert_list(trend_metadata, names = "named", null.ok = TRUE)

  mvgam_components <- extract_mvgam_components(combined_fit, obs_setup,
                                              trend_setup, mv_spec)

  backend <- attr(combined_fit, "backend") %||% "rstan"

  mvgam_object <- structure(
    list(
      fit = combined_fit,
      formula = obs_setup$formula,
      trend_formula = if (!is.null(trend_setup)) trend_setup$formula else NULL,
      family = obs_setup$family,
      prior = obs_setup$prior,
      data = obs_setup$data,
      stancode = obs_setup$stancode,
      standata = obs_setup$standata,
      exclude = c("lprior", "lp__"),
      mv_spec = mv_spec,
      response_names = mv_spec$response_names %||% NULL,
      trend_components = mvgam_components$trend_components,
      series_info = mvgam_components$series_info,
      time_info = mvgam_components$time_info,
      trend_metadata = trend_metadata,
      backend = backend,
      algorithm = combined_fit@sim$algorithm %||% "sampling",
      brms_version = utils::packageVersion("brms"),
      mvgam_version = utils::packageVersion("mvgam"),
      creation_time = Sys.time()
    ),
    class = c("mvgam", "brmsfit")
  )

  mvgam_object$criteria <- list()
  mvgam_object$call <- match.call(sys.function(sys.parent()),
                                 sys.call(sys.parent()))

  return(mvgam_object)
}



# ------------------------------------------------------------------------------
# COMPONENT EXTRACTION
# ------------------------------------------------------------------------------
# Extracts mvgam-specific metadata and information from the combined fit to
# enable specialized State-Space model functionality and analysis.

#' Extract mvgam-Specific Components
#' @param combined_fit Stan fit object
#' @param obs_setup Observation setup
#' @param trend_setup Trend setup
#' @param mv_spec Multivariate specification
#' @return List of mvgam-specific components
#' @noRd
extract_mvgam_components <- function(combined_fit, obs_setup, trend_setup,
                                    mv_spec) {
  # Extract time series information
  time_info <- extract_time_information(obs_setup$data)

  # Extract series information
  series_info <- extract_series_information(obs_setup$data, mv_spec)

  # Extract trend components if available
  trend_components <- if (!is.null(mv_spec$has_trends) &&
                         mv_spec$has_trends) {
    extract_trend_component_info(combined_fit, mv_spec)
  } else {
    NULL
  }

  return(list(
    time_info = time_info,
    series_info = series_info,
    trend_components = trend_components
  ))
}

#' Extract Time Information from Data
#' @param data Model data frame
#' @return List with time-related metadata
#' @noRd
extract_time_information <- function(data) {
  if ("time" %in% names(data)) {
    list(
      n_timepoints = length(unique(data$time)),
      time_range = range(data$time, na.rm = TRUE),
      time_spacing = diff(sort(unique(data$time)))[1],
      has_time = TRUE
    )
  } else {
    list(has_time = FALSE)
  }
}

#' Extract Series Information from Data
#' @param data Model data frame
#' @param mv_spec Multivariate specification
#' @return List with series-related metadata
#' @noRd
extract_series_information <- function(data, mv_spec) {
  series_info <- list()

  if ("series" %in% names(data)) {
    series_info$n_series <- length(unique(data$series))
    series_info$series_names <- unique(data$series)
    series_info$has_series = TRUE
  } else {
    series_info$has_series <- FALSE
  }

  # Add multivariate response information
  if (!is.null(mv_spec$response_names)) {
    series_info$response_names <- mv_spec$response_names
    series_info$n_responses <- length(mv_spec$response_names)
    series_info$is_multivariate <- TRUE
  } else {
    series_info$is_multivariate <- FALSE
  }

  return(series_info)
}

#' Extract Trend Component Information
#' @param combined_fit Stan fit object
#' @param mv_spec Multivariate specification
#' @return List with trend component metadata
#' @noRd
extract_trend_component_info <- function(combined_fit, mv_spec) {
  trend_info <- list()

  if (!is.null(mv_spec$trend_specs)) {
    trend_info$specifications <- mv_spec$trend_specs

    # Handle both single spec and list of specs
    specs_list <- if (inherits(mv_spec$trend_specs, "mvgam_trend")) {
      list(mv_spec$trend_specs)
    } else {
      mv_spec$trend_specs
    }

    trend_info$n_trends <- length(specs_list)

    # Extract trend types
    trend_info$types <- sapply(specs_list, function(spec) {
      if (inherits(spec, "mvgam_trend")) {
        spec$trend
      } else {
        "custom"
      }
    })
  }

  return(trend_info)
}

# ==============================================================================
# MULTIPLE IMPUTATION SUPPORT: RUBIN'S RULES POOLING
# ==============================================================================
# Provides comprehensive multiple imputation support using Rubin's rules for
# proper uncertainty quantification when dealing with missing data in State-
# Space models, ensuring valid statistical inference.

#' Fit mvgam Models to Multiple Imputation Datasets
#'
#' @description
#' Fits Bayesian state-space models to multiply imputed datasets,
#' combining posteriors across imputations for proper uncertainty
#' quantification when dealing with missing data.
#'
#' @details
#' This function processes multiple imputed datasets in two modes:
#'
#' **Combined Mode (combine=TRUE)**:
#' Fits separate models to each imputed dataset, then combines
#' posteriors using `rstan::sflist2stanfit()` to create a pooled
#' posterior distribution. Returns an object of class
#' `c("mvgam_pooled", "mvgam", "brmsfit")` with MI-specific
#' attributes and methods.
#'
#' **List Mode (combine=FALSE)**:
#' Returns a list of individual mvgam fits, one per imputation,
#' allowing manual posterior combination or separate analysis.
#'
#' **Posterior Combination Approach**:
#' Uses `rstan::sflist2stanfit()` (validated brms pattern) to
#' concatenate draws from all imputations at the Stan level,
#' properly accounting for between-imputation and
#' within-imputation variance. This approach ensures valid
#' uncertainty quantification following proper multiple imputation
#' principles.
#'
#' **MI Diagnostics**:
#' Pooled objects include convergence diagnostics per imputation
#' (Rhat, ESS) accessible via `summary()`. The print method
#' displays comprehensive MI diagnostics including total draws,
#' draws per imputation, and convergence summaries.
#'
#' @param formula Model formula (observation model). Supports brms
#'   formula syntax including smooths and random effects.
#' @param trend_formula Optional trend formula specifying
#'   state-space dynamics. Can be response-specific in multivariate
#'   models.
#' @param data_list List of imputed data frames. Each element must
#'   have identical structure (same variables, same ordering).
#'   Alternatively, a `mids` object from the mice package.
#' @param backend Character string specifying Stan backend:
#'   "cmdstanr" or "rstan". Defaults to
#'   `getOption("brms.backend", "cmdstanr")`.
#' @param combine Logical; if `TRUE` (default), combines posteriors
#'   into a pooled mvgam_pooled object. If `FALSE`, returns list of
#'   individual fits.
#' @param check_data Logical; if `TRUE` (default), validates
#'   consistency across imputations (same column names, types,
#'   rows).
#' @param ... Additional arguments passed to `mvgam()` for each
#'   imputation (e.g., chains, iter, family, priors).
#'
#' @return
#' If `combine=TRUE`: An object of class
#' `c("mvgam_pooled", "mvgam", "brmsfit")` containing:
#' \itemize{
#'   \item{Pooled posterior draws from all imputations}
#'   \item{Attribute `individual_fits`: List of per-imputation
#'     mvgam objects}
#'   \item{Attribute `n_imputations`: Number of imputations}
#'   \item{Attribute `combination_method`: "sflist2stanfit"}
#' }
#'
#' If `combine=FALSE`: A list of mvgam objects, one per imputation.
#'
#' @examples
#' \dontrun{
#' # Create pseudo-imputed data (3 imputations)
#' base_data <- data.frame(
#'   time = 1:24,
#'   series = factor(rep("series1", 24)),
#'   y = rnorm(24, mean = 3, sd = 1),
#'   season = 1:24
#' )
#'
#' imputed_list <- lapply(1:3, function(i) {
#'   data_copy <- base_data
#'   data_copy$y <- data_copy$y + rnorm(nrow(data_copy), 0, 0.1)
#'   data_copy
#' })
#'
#' # Fit with combined posteriors (recommended)
#' fit_pooled <- mvgam_multiple(
#'   formula = y ~ s(season, bs = "cc", k = 5),
#'   trend_formula = ~ 1,
#'   data_list = imputed_list,
#'   family = gaussian(),
#'   combine = TRUE
#' )
#'
#' # Check MI diagnostics
#' summary(fit_pooled)
#' print(fit_pooled)
#'
#' # Fit separately (for advanced use cases)
#' fit_list <- mvgam_multiple(
#'   formula = y ~ s(season, bs = "cc", k = 5),
#'   trend_formula = ~ 1,
#'   data_list = imputed_list,
#'   family = gaussian(),
#'   combine = FALSE
#' )
#' }
#'
#' @seealso \code{\link{summary.mvgam_pooled}},
#'   \code{\link{print.mvgam_pooled_summary}}, \code{\link{mvgam}}
#'
#' @export
mvgam_multiple <- function(formula,
                           trend_formula = NULL,
                           data_list,
                           backend = getOption("brms.backend", "cmdstanr"),
                           combine = TRUE,
                           check_data = TRUE,
                           ...) {
  # Input validation
  checkmate::assert_list(data_list, min.len = 2)
  checkmate::assert_character(backend, len = 1, any.missing = FALSE)
  checkmate::assert_logical(combine, len = 1, any.missing = FALSE)
  checkmate::assert_logical(check_data, len = 1, any.missing = FALSE)

  # Handle mids objects from mice package
  if (inherits(data_list, "mids")) {
    rlang::check_installed("mice",
      reason = "for multiple imputation with mids objects"
    )
    n_imp <- data_list$m
    data_list <- lapply(seq_len(n_imp), function(i) {
      mice::complete(data_list, i)
    })
  }

  # Validate all elements are data frames
  if (!all(sapply(data_list, is.data.frame))) {
    stop(insight::format_error(
      "All elements in {.field data_list} must be data.frames.",
      "Found non-data.frame elements in imputation list."
    ))
  }

  # Validate multiple imputation datasets
  if (check_data) {
    validate_multiple_imputation_datasets(data_list)
  }

  # Fit individual models to each imputed dataset
  individual_fits <- fit_multiple_imputation_models(
    formula = formula,
    trend_formula = trend_formula,
    data_list = data_list,
    backend = backend,
    ...
  )

  # Return combined or individual results
  if (combine) {
    pooled_fit <- pool_mvgam_fits(individual_fits)
    return(pooled_fit)
  } else {
    return(individual_fits)
  }
}

# ------------------------------------------------------------------------------
# DATASET VALIDATION
# ------------------------------------------------------------------------------
# Ensures that multiple imputation datasets meet requirements for valid
# statistical inference, including structural consistency and proper handling
# of time series identifiers.

#' Validate Multiple Imputation Datasets
#' @param data_list List of imputed datasets
#' @return Invisible TRUE if valid, stops with error if invalid
#' @noRd
validate_multiple_imputation_datasets <- function(data_list) {
  checkmate::assert_list(data_list, min.len = 2)

  # Check all elements are data frames
  if (!all(sapply(data_list, is.data.frame))) {
    stop(insight::format_error(
      "All elements in data_list must be data.frames.",
      "Found non-data.frame elements in imputation list."
    ))
  }

  # Get reference structure from first dataset
  ref_data <- data_list[[1]]
  ref_names <- names(ref_data)
  ref_nrow <- nrow(ref_data)

  # Validate consistency across datasets
  for (i in seq_along(data_list)[-1]) {
    current_data <- data_list[[i]]

    # Check column names match
    if (!identical(names(current_data), ref_names)) {
      stop(insight::format_error(
        paste("Dataset", i, "has different column names than dataset 1."),
        "All imputed datasets must have identical structure."
      ))
    }

    # Check number of rows match
    if (nrow(current_data) != ref_nrow) {
      stop(insight::format_error(
        paste("Dataset", i, "has", nrow(current_data), "rows, expected",
             ref_nrow),
        "All imputed datasets must have same number of observations."
      ))
    }

    # Check essential columns (time, series) are identical
    essential_cols <- intersect(c("time", "series"), ref_names)
    for (col in essential_cols) {
      if (!identical(ref_data[[col]], current_data[[col]])) {
        stop(insight::format_error(
          paste("Column", col, "differs between datasets."),
          "Time and series identifiers must be identical across imputations."
        ))
      }
    }
  }

  # Validate missing data patterns
  validate_missing_patterns(data_list)

  invisible(TRUE)
}

#' Validate Missing Data Patterns
#' @param data_list List of imputed datasets
#' @return Invisible TRUE if valid, warns about potential issues
#' @noRd
validate_missing_patterns <- function(data_list) {
  n_datasets <- length(data_list)
  dataset_names <- names(data_list[[1]])

  # Check for variables that should not be imputed
  non_imputable <- c("time", "series", "weights", "trials")
  present_non_imputable <- intersect(non_imputable, dataset_names)

  for (col in present_non_imputable) {
    values_list <- lapply(data_list, function(d) d[[col]])

    # Check if any differences exist
    reference_values <- values_list[[1]]
    for (i in 2:n_datasets) {
      if (!identical(reference_values, values_list[[i]])) {
        insight::format_warning(
          paste("Column", col, "varies between imputed datasets."),
          "This may indicate improper imputation of structural variables.",
          .frequency = "once"
        )
      }
    }
  }

  invisible(TRUE)
}

# ------------------------------------------------------------------------------
# INDIVIDUAL MODEL FITTING
# ------------------------------------------------------------------------------
# Fits separate mvgam models to each imputed dataset using consistent
# specifications to enable proper pooling of results.

#' Fit Models to Multiple Imputation Datasets
#' @param formula Main formula
#' @param trend_formula Trend formula
#' @param data_list List of datasets
#' @param backend Stan backend
#' @param ... Additional arguments
#' @return List of fitted mvgam objects
#' @noRd
fit_multiple_imputation_models <- function(formula, trend_formula, data_list,
                                          backend, ...) {
  n_datasets <- length(data_list)

  insight::format_message(
    paste("Fitting mvgam models to", n_datasets, "imputed datasets..."),
    "This may take some time depending on model complexity."
  )

  # Fit individual models
  fits <- vector("list", n_datasets)
  names(fits) <- paste0("imputation_", seq_len(n_datasets))

  for (i in seq_len(n_datasets)) {
    insight::format_message(
      paste("Fitting imputation", i, "of", n_datasets, "...")
    )

    # Fit model using standard mvgam_single function
    fits[[i]] <- mvgam_single(
      formula = formula,
      trend_formula = trend_formula,
      data = data_list[[i]],
      backend = backend,
      ...
    )

    # Add imputation metadata
    fits[[i]]$imputation_id <- i
    fits[[i]]$n_imputations <- n_datasets
  }

  return(fits)
}


# ------------------------------------------------------------------------------
# POSTERIOR COMBINATION
# ------------------------------------------------------------------------------
# Combines posterior samples from multiple imputation fits using Stan-level
# combination. Follows brms pattern using rstan::sflist2stanfit().

#' Pool mvgam Fits Using Stan-Level Combination
#' @param fits List of individual mvgam fits
#' @return mvgam_pooled object with combined posteriors
#' @noRd
pool_mvgam_fits <- function(fits) {
  # Input validation
  checkmate::assert_list(fits, min.len = 2)

  if (!all(sapply(fits, function(x) inherits(x, "mvgam")))) {
    stop(insight::format_error(
      "All fits must be mvgam objects.",
      "Cannot combine fits of different types."
    ))
  }

  # Validate parameter consistency across fits
  ref_vars <- sort(variables(fits[[1]]))
  for (i in seq_along(fits)[-1]) {
    current_vars <- sort(variables(fits[[i]]))
    if (!identical(ref_vars, current_vars)) {
      stop(insight::format_error(
        sprintf("Model 1 and %d have different parameters.", i),
        "This may indicate fitting failures or model changes."
      ))
    }
  }

  n_imp <- length(fits)
  insight::format_message(
    sprintf("Combining posteriors across %d imputations...", n_imp)
  )

  # Extract stanfit objects
  sflist <- lapply(fits, function(fit) fit$fit)

  # Combine using rstan::sflist2stanfit()
  combined_stanfit <- rstan::sflist2stanfit(sflist)

  # Create combined object using first fit as template
  combined_fit <- fits[[1]]
  combined_fit$fit <- combined_stanfit

  # Store individual fits and metadata
  attr(combined_fit, "individual_fits") <- fits
  attr(combined_fit, "n_imputations") <- n_imp
  attr(combined_fit, "combination_method") <- "sflist2stanfit"
  attr(combined_fit, "combination_time") <- Sys.time()

  # Set class
  class(combined_fit) <- c("mvgam_pooled", "mvgam", "brmsfit")

  insight::format_message("Successfully combined posteriors.")
  return(combined_fit)
}

