#' Multiple Imputation Support for mvgam
#'
#' @description
#' Handles multiple imputation datasets and pools results using Rubin's rules.
#' Provides seamless integration with mvgam's State-Space modeling framework.
#'
#' @param formula Main observation model formula
#' @param trend_formula Trend formula specification
#' @param data_list List of multiply imputed datasets
#' @param backend Stan backend to use
#' @param combine Logical, whether to pool results using Rubin's rules
#' @param ... Additional arguments passed to mvgam()
#' @return mvgam_pooled object or list of individual fits
#' @noRd
mvgam_multiple <- function(formula, trend_formula = NULL, data_list, 
                           backend = NULL, combine = TRUE, ...) {
  checkmate::assert_formula(formula)
  checkmate::assert_list(data_list, min.len = 2, types = "data.frame")
  checkmate::assert_logical(combine, len = 1)
  
  # Validate multiple imputation datasets
  validate_multiple_imputation_datasets(data_list)
  
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
        paste("Dataset", i, "has", nrow(current_data), "rows, expected", ref_nrow),
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
    
    # Fit model to current imputed dataset
    fits[[i]] <- mvgam_single_imputation(
      formula = formula,
      trend_formula = trend_formula,
      data = data_list[[i]],
      backend = backend,
      imputation_id = i,
      ...
    )
    
    # Add imputation metadata
    fits[[i]]$imputation_id <- i
    fits[[i]]$n_imputations <- n_datasets
  }
  
  return(fits)
}

#' Fit mvgam to Single Imputation Dataset
#' @param formula Main formula
#' @param trend_formula Trend formula
#' @param data Single imputed dataset
#' @param backend Stan backend
#' @param imputation_id ID of current imputation
#' @param ... Additional arguments
#' @return Single mvgam fit object
#' @noRd
mvgam_single_imputation <- function(formula, trend_formula, data, backend, 
                                    imputation_id, ...) {
  # Parse multivariate trends
  mv_spec <- parse_multivariate_trends(formula, trend_formula)
  
  # Setup observation and trend models using lightweight brms
  obs_setup <- setup_brms_lightweight(formula, data, ...)
  
  trend_setup <- if (mv_spec$has_trends) {
    setup_brms_lightweight(mv_spec$base_formula, data, family = gaussian(), ...)
  } else {
    NULL
  }
  
  # Generate combined Stan code and data
  combined_components <- generate_combined_stancode_and_data(obs_setup, trend_setup, mv_spec)
  
  # Fit the combined model
  combined_fit <- fit_mvgam_model(
    stancode = combined_components$stancode,
    standata = combined_components$standata,
    backend = backend,
    ...
  )
  
  # Create mvgam object from combined fit
  mvgam_object <- create_mvgam_from_combined_fit(
    combined_fit = combined_fit,
    obs_setup = obs_setup,
    trend_setup = trend_setup,
    mv_spec = mv_spec
  )
  
  return(mvgam_object)
}

#' Pool mvgam Fits Using Rubin's Rules
#' @param fits List of individual mvgam fits
#' @return mvgam_pooled object with combined estimates
#' @noRd
pool_mvgam_fits <- function(fits) {
  checkmate::assert_list(fits, min.len = 2)
  
  # Validate all fits are mvgam objects
  if (!all(sapply(fits, function(x) inherits(x, "mvgam")))) {
    stop(insight::format_error(
      "All fits must be mvgam objects.",
      "Cannot pool fits of different types."
    ))
  }
  
  insight::format_message("Pooling results using Rubin's rules...")
  
  # Extract parameter estimates from each fit
  estimates_list <- lapply(fits, extract_fit_estimates)
  
  # Apply Rubin's rules pooling
  pooled_estimates <- apply_rubins_rules(estimates_list)
  
  # Create pooled mvgam object
  pooled_fit <- create_pooled_mvgam(fits[[1]], pooled_estimates)
  
  # Store individual fits and metadata
  attr(pooled_fit, "individual_fits") <- fits
  attr(pooled_fit, "n_imputations") <- length(fits)
  attr(pooled_fit, "pooling_method") <- "rubins_rules"
  attr(pooled_fit, "pooled_time") <- Sys.time()
  
  # Set appropriate class
  class(pooled_fit) <- c("mvgam_pooled", "mvgam", "brmsfit")
  
  return(pooled_fit)
}

#' Extract Parameter Estimates from mvgam Fit
#' @param fit Single mvgam fit object
#' @return List containing parameter estimates and uncertainties
#' @noRd
extract_fit_estimates <- function(fit) {
  # Extract posterior samples for key parameters
  obs_params <- extract_posterior_samples(fit$obs_fit)
  
  trend_params <- if (!is.null(fit$trend_fit)) {
    extract_posterior_samples(fit$trend_fit)
  } else {
    NULL
  }
  
  return(list(
    observation = obs_params,
    trend = trend_params,
    combined = extract_posterior_samples(fit$combined_fit)
  ))
}

#' Apply Rubin's Rules for Multiple Imputation Pooling
#' @param estimates_list List of parameter estimates from each imputation
#' @return List containing pooled estimates with proper uncertainty
#' @noRd
apply_rubins_rules <- function(estimates_list) {
  n_imputations <- length(estimates_list)
  
  # Get parameter names from first estimation
  param_structure <- estimates_list[[1]]
  
  pooled_results <- list()
  
  # Pool each parameter type
  for (param_type in names(param_structure)) {
    if (!is.null(param_structure[[param_type]])) {
      param_estimates <- lapply(estimates_list, function(x) x[[param_type]])
      pooled_results[[param_type]] <- pool_parameter_estimates(param_estimates)
    }
  }
  
  return(pooled_results)
}

#' Pool Parameter Estimates Using Rubin's Rules
#' @param param_list List of parameter estimates across imputations
#' @return List with pooled mean, variance, and diagnostics
#' @noRd
pool_parameter_estimates <- function(param_list) {
  n_imputations <- length(param_list)
  
  # Calculate means across imputations
  means <- lapply(param_list, function(x) apply(x, 2, mean))
  pooled_mean <- Reduce("+", means) / n_imputations
  
  # Calculate within-imputation variance
  within_var <- lapply(param_list, function(x) apply(x, 2, var))
  within_variance <- Reduce("+", within_var) / n_imputations
  
  # Calculate between-imputation variance
  mean_diffs <- lapply(means, function(m) m - pooled_mean)
  between_variance <- Reduce("+", lapply(mean_diffs, function(d) d^2)) / (n_imputations - 1)
  
  # Total variance using Rubin's rules
  total_variance <- within_variance + (1 + 1/n_imputations) * between_variance
  
  # Calculate degrees of freedom and other diagnostics
  relative_increase <- (1 + 1/n_imputations) * between_variance / within_variance
  degrees_freedom <- (n_imputations - 1) * (1 + within_variance / ((1 + 1/n_imputations) * between_variance))^2
  
  return(list(
    mean = pooled_mean,
    variance = total_variance,
    se = sqrt(total_variance),
    within_variance = within_variance,
    between_variance = between_variance,
    relative_increase = relative_increase,
    degrees_freedom = degrees_freedom,
    n_imputations = n_imputations
  ))
}

#' Create Pooled mvgam Object
#' @param template_fit Template mvgam object to base structure on
#' @param pooled_estimates Pooled parameter estimates
#' @return mvgam_pooled object
#' @noRd
create_pooled_mvgam <- function(template_fit, pooled_estimates) {
  # Start with template structure
  pooled_fit <- template_fit
  
  # Replace parameter estimates with pooled versions
  pooled_fit$pooled_estimates <- pooled_estimates
  
  # Update metadata
  pooled_fit$pooled <- TRUE
  pooled_fit$pooling_diagnostics <- extract_pooling_diagnostics(pooled_estimates)
  
  return(pooled_fit)
}

#' Extract Pooling Diagnostics
#' @param pooled_estimates Pooled parameter estimates
#' @return List of pooling diagnostic information
#' @noRd
extract_pooling_diagnostics <- function(pooled_estimates) {
  diagnostics <- list()
  
  for (param_type in names(pooled_estimates)) {
    if (!is.null(pooled_estimates[[param_type]])) {
      est <- pooled_estimates[[param_type]]
      
      diagnostics[[param_type]] <- list(
        n_imputations = est$n_imputations,
        avg_relative_increase = mean(est$relative_increase, na.rm = TRUE),
        avg_degrees_freedom = mean(est$degrees_freedom, na.rm = TRUE),
        fraction_missing_info = est$relative_increase / (1 + est$relative_increase)
      )
    }
  }
  
  return(diagnostics)
}