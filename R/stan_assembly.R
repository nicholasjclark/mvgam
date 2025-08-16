#' Stan Code Assembly System for mvgam
#'
#' @description
#' Complete Stan code generation and validation pipeline for mvgam-brms
#' integration. This file consolidates the two-stage assembly system,
#' trend-specific Stan code generators, and validation framework.
#'
#' @section Architecture:
#' The Stan assembly system follows a two-stage pipeline:
#' - **Stage 1**: brms generates base Stan code; mvgam produces trend stanvars
#' - **Stage 2**: mvgam injects trend effects into linear predictors
#' - **Validation**: Comprehensive Stan code structure and syntax validation
#' - **Generators**: Trend-specific Stan code generation for all trend types

# =============================================================================
# SECTION 1: STAN ASSEMBLY ORCHESTRATION
# =============================================================================
# WHY: The two-stage assembly system is critical for mvgam-brms integration
# because it allows mvgam to extend brms models without modifying brms
# internals. Stage 1 leverages brms' robust model compilation, while Stage 2
# adds mvgam-specific trend dynamics through stanvars injection.

#' Apply Response Suffix to Trend Stanvars
#' 
#' @description
#' Post-processes stanvars to apply response-specific naming for multivariate models.
#' This systematic approach ensures all trend parameters get proper suffixes without
#' requiring manual updates to individual trend generators.
#' 
#' @param stanvars List of stanvar objects or single stanvar object
#' @param response_suffix Response suffix (e.g., "_y1", "_y2")
#' @return Modified stanvars with response-specific parameter names
#' @noRd
apply_response_suffix_to_stanvars <- function(stanvars, response_suffix) {
  if (is.null(stanvars) || response_suffix == "" || is.null(response_suffix)) {
    return(stanvars)
  }
  
  # Define all trend parameter patterns that need suffixing
  trend_patterns <- c(
    "sigma_trend",
    "mu_trend", 
    "L_Omega_trend",
    "Sigma_trend",
    "raw_innovations",
    "LV_innovations",
    "LV",
    "theta1_trend",
    "ar\\d*_trend",  # ar1_trend, ar2_trend, etc.
    "A\\d*_trend",   # A1_trend, A2_trend for VAR
    "n_lv_trend",
    "n_trend",
    "trend_"
  )
  
  # Handle both single stanvar and list of stanvars
  if (inherits(stanvars, "stanvar")) {
    stanvars <- list(stanvars)
    single_stanvar <- TRUE
  } else {
    single_stanvar <- FALSE
  }
  
  # Process each stanvar
  for (i in seq_along(stanvars)) {
    stanvar <- stanvars[[i]]
    
    if (!is.null(stanvar) && inherits(stanvar, "stanvar")) {
      # Update the stanvar name if it matches a trend pattern
      original_name <- stanvar$name
      new_name <- apply_suffix_to_name(original_name, trend_patterns, response_suffix)
      stanvar$name <- new_name
      
      # Update all parameter references in the Stan code
      if (!is.null(stanvar$scode) && nchar(stanvar$scode) > 0) {
        stanvar$scode <- apply_suffix_to_stan_code(stanvar$scode, trend_patterns, response_suffix)
      }
      
      stanvars[[i]] <- stanvar
    }
  }
  
  # Return single stanvar or list as appropriate
  if (single_stanvar) {
    return(stanvars[[1]])
  } else {
    return(stanvars)
  }
}

#' Apply Suffix to Parameter Name
#' @noRd
apply_suffix_to_name <- function(name, patterns, suffix) {
  for (pattern in patterns) {
    if (grepl(paste0("^", pattern, "$"), name, perl = TRUE)) {
      return(paste0(name, suffix))
    }
  }
  return(name)
}

#' Apply Suffix to Stan Code
#' @noRd
apply_suffix_to_stan_code <- function(stan_code, patterns, suffix) {
  for (pattern in patterns) {
    # Handle word boundaries to avoid partial matches
    # Match pattern as whole word (not part of another identifier)
    regex_pattern <- paste0("\\b", pattern, "\\b")
    replacement <- paste0(pattern, suffix)
    
    # Use perl = TRUE for better regex support
    stan_code <- gsub(regex_pattern, replacement, stan_code, perl = TRUE)
  }
  
  return(stan_code)
}

#' @noRd
generate_combined_stancode <- function(obs_setup, trend_setup = NULL,
                                      trend_specs = NULL, backend = "rstan",
                                      validate = TRUE, silent = 1) {
  checkmate::assert_list(obs_setup, names = "named")
  checkmate::assert_list(trend_setup, null.ok = TRUE)
  checkmate::assert_list(trend_specs, null.ok = TRUE)
  checkmate::assert_choice(backend, c("rstan", "cmdstanr"))
  checkmate::assert_flag(validate)
  checkmate::assert_number(silent)

  # If no trend specification, return observation model as-is
  if (is.null(trend_setup) || is.null(trend_specs)) {
    if (silent < 2) {
      message("No trend specification provided. Returning observation model without trends.")
    }

    return(list(
      stancode = obs_setup$stancode,
      standata = obs_setup$standata,
      has_trends = FALSE,
      is_multivariate = FALSE
    ))
  }

  # Determine if we have multivariate trends
  is_multivariate <- is_multivariate_trend_specs(trend_specs)
  responses_with_trends <- c()
  
  if (silent < 2) {
    if (is_multivariate) {
      message("Processing multivariate model with ", length(trend_specs), " responses")
    } else {
      message("Processing univariate model")
    }
  }

  # Stage 1: Extract trend stanvars from brms setup
  if (silent < 2) {
    message("Stage 1: Extracting trend stanvars from brms setup...")
  }

  # Handle both univariate and multivariate cases
  if (is_multivariate) {
    # Extract response names from the trend_specs
    response_names <- names(trend_specs)
    
    # Generate stanvars for each response
    trend_stanvars_list <- list()
    for (resp_name in response_names) {
      resp_trend_specs <- trend_specs[[resp_name]]
      
      # Skip if no trend for this response
      if (is.null(resp_trend_specs)) {
        next
      }
      
      # Track this response as having a trend
      responses_with_trends <- c(responses_with_trends, resp_name)
      
      # Extract stanvars for this response with response suffix
      resp_stanvars <- extract_trend_stanvars_from_setup(
        trend_setup, 
        resp_trend_specs,
        response_suffix = paste0("_", resp_name)
      )
      
      if (!is.null(resp_stanvars)) {
        trend_stanvars_list[[resp_name]] <- resp_stanvars
      }
    }
    
    # Combine all response-specific stanvars
    if (length(trend_stanvars_list) > 0) {
      trend_stanvars <- do.call(combine_stanvars, trend_stanvars_list)
    } else {
      trend_stanvars <- NULL
    }
    
  } else {
    # Single trend specification - backward compatible
    trend_stanvars <- extract_trend_stanvars_from_setup(trend_setup, trend_specs)
    responses_with_trends <- "main"  # Mark univariate model
  }

  # Generate base observation Stan code with trend stanvars
  base_stancode <- generate_base_stancode_with_stanvars(
    obs_setup,
    trend_stanvars,
    backend = backend,
    silent = silent
  )

  # Stage 2: Modify observation linear predictor with trend injection
  if (silent < 2) {
    message("Stage 2: Injecting trend into observation linear predictor...")
  }

  # Inject trends into linear predictors
  if (is_multivariate) {
    combined_stancode <- inject_multivariate_trends_into_linear_predictors(
      base_stancode,
      trend_stanvars,
      responses_with_trends
    )
  } else {
    combined_stancode <- inject_trend_into_linear_predictor(
      base_stancode,
      trend_stanvars
    )
  }

  # Combine Stan data from both models
  combined_standata <- combine_stan_data(obs_setup$standata, trend_setup$standata)

  # Validate final Stan code if requested
  if (validate) {
    if (silent < 2) {
      message("Validating combined Stan code...")
    }

    validated_code <- validate_stan_code(
      combined_stancode,
      backend = backend,
      silent = silent
    )

    combined_stancode <- validated_code
  }

  return(list(
    stancode = combined_stancode,
    standata = combined_standata,
    has_trends = TRUE,
    trend_specs = trend_specs,
    is_multivariate = is_multivariate,
    responses_with_trends = responses_with_trends,
    backend = backend
  ))
}

#' Generate Base Stan Code with Stanvars
#'
#' @description
#' Generates base brms Stan code incorporating trend stanvars.
#' Uses brms::make_stancode with injected stanvars.
#'
#' @param obs_setup List containing observation model setup
#' @param trend_stanvars List of trend stanvars to inject
#' @param backend Character string specifying backend
#' @param silent Numeric indicating verbosity level
#' @return Character string of base Stan code with stanvars
#' @noRd
generate_base_stancode_with_stanvars <- function(obs_setup, trend_stanvars,
                                                backend = "rstan", silent = 1) {
  checkmate::assert_list(obs_setup)
  # trend_stanvars can be NULL, a stanvar, or stanvars collection
  if (!is.null(trend_stanvars)) {
    if (!inherits(trend_stanvars, c("stanvar", "stanvars"))) {
      stop(insight::format_error(
        "Invalid trend_stanvars class:",
        paste("Got class:", paste(class(trend_stanvars), collapse = ", ")),
        "Expected stanvar or stanvars object."
      ))
    }
  }

  # Combine existing stanvars with trend stanvars
  # brms expects stanvars combined with c() method, not lists
  all_stanvars <- NULL

  # Start with observation stanvars if they exist
  if (!is.null(obs_setup$stanvars)) {
    all_stanvars <- obs_setup$stanvars
  }

  # Add trend stanvars using c() method to maintain proper class
  if (!is.null(trend_stanvars)) {
    if (is.null(all_stanvars)) {
      all_stanvars <- trend_stanvars
    } else {
      # Use c() to properly combine stanvars objects
      all_stanvars <- c(all_stanvars, trend_stanvars)
    }
  }

  # Return NULL for empty stanvars (brms expectation)

  # Generate Stan code using brms with combined stanvars
  base_code <- brms::make_stancode(
      formula = obs_setup$formula,
      data = obs_setup$data,
      family = obs_setup$family,
      stanvars = all_stanvars,
      prior = obs_setup$prior
    )

  return(base_code)
}

#' Combine Stan Data
#'
#' @description
#' Combines Stan data from observation and trend models, handling conflicts.
#'
#' @param obs_data List containing observation model Stan data
#' @param trend_data List containing trend model Stan data
#' @return List of combined Stan data
#' @noRd
combine_stan_data <- function(obs_data, trend_data) {
  checkmate::assert_list(obs_data)
  checkmate::assert_list(trend_data)

  # Start with observation data as base
  combined_data <- obs_data

  # Add trend-specific data elements
  trend_only_names <- setdiff(names(trend_data), names(obs_data))
  combined_data[trend_only_names] <- trend_data[trend_only_names]

  # Validate combined data consistency
  validate_combined_standata(combined_data, obs_data, trend_data)

  return(combined_data)
}

#' Integration with Enhanced mvgam Function
#'
#' @description
#' Integration point for enhanced mvgam function to use two-stage assembly.
#' Includes time series validation to ensure data compatibility.
#'
#' @param obs_setup Observation model setup from setup_brms_lightweight
#' @param trend_setup Trend model setup from setup_brms_lightweight
#' @param trend_specs Parsed trend specification
#' @param data Original data for validation
#' @param backend Stan backend to use
#' @param validate Whether to validate Stan code
#' @param silent Verbosity level
#' @return List ready for mvgam model fitting
#' @noRd
prepare_mvgam_stancode <- function(obs_setup, trend_setup, trend_specs,
                                  data = NULL, backend = "rstan",
                                  validate = TRUE, silent = 1) {

  # Validate time series structure if data provided and trends specified
  if (!is.null(data) && !is.null(trend_specs)) {
    validate_time_series_for_trends(data, trend_specs, silent = silent)
  }

  # Generate combined Stan code and data
  stan_components <- generate_combined_stancode(
    obs_setup = obs_setup,
    trend_setup = trend_setup,
    trend_specs = trend_specs,
    backend = backend,
    validate = validate,
    silent = silent
  )

  # Return in format expected by mvgam fitting functions
  return(list(
    model_code = stan_components$stancode,
    model_data = stan_components$standata,
    has_trends = stan_components$has_trends,
    trend_specification = trend_specs,
    backend = backend,
    validation_passed = validate
  ))
}

#' Prepare Data for Stan
#'
#' @description
#' Orders data appropriately for Stan computation and stores metadata for restoration.
#' Stan expects data ordered by series first, then time within series for efficient
#' block matrix operations.
#'
#' @param data Validated data.frame
#' @param variable_info List containing variable names from trend constructor
#' @return List with ordered data and ordering metadata
#' @noRd
prepare_stan_data <- function(data, variable_info) {
  checkmate::assert_data_frame(data)
  checkmate::assert_list(variable_info)

  # Extract variable names
  time_var <- variable_info$trend$time
  series_var <- variable_info$trend$series
  gr_var <- variable_info$trend$gr
  subgr_var <- variable_info$trend$subgr

  # Auto-drop unused factor levels for Stan compatibility
  data <- validate_factor_levels(data, series_var, "prepared_data", auto_drop = TRUE)
  if (!is.null(gr_var)) {
    data <- validate_factor_levels(data, gr_var, "prepared_data", auto_drop = TRUE)
  }
  if (!is.null(subgr_var)) {
    data <- validate_factor_levels(data, subgr_var, "prepared_data", auto_drop = TRUE)
  }

  # Store ordering metadata for post-processing
  ordering_info <- list(
    original_rows = seq_len(nrow(data)),
    time_var = time_var,
    series_var = series_var
  )

  # Order data for Stan: series first for block processing, then time
  ordered_data <- data %>%
    dplyr::arrange(
      !!rlang::sym(series_var),  # Series blocks for efficient computation
      !!rlang::sym(time_var)      # Time within each series
    )

  return(list(
    data = ordered_data,
    ordering = ordering_info
  ))
}

# Data Extraction and Processing Utilities
# ========================================


# Note: Legacy merge_stan_data() function removed.
# Modern system uses combine_stan_data() for Stan data merging.

# Stan Code Processing Utilities
# ==============================

#' Extract Code Block from Stan Code
#'
#' @description
#' Extracts a specific block (e.g., "data", "parameters", "model") from Stan code.
#'
#' @param code_lines Character vector of Stan code lines
#' @param block_name Character string name of block to extract
#' @return Character string of block contents or NULL if not found
#' @noRd
extract_code_block <- function(code_lines, block_name) {
  checkmate::assert_character(code_lines)
  checkmate::assert_string(block_name)

  # Find block start line
  block_pattern <- paste0("^\\s*", block_name, "\\s*\\{\\s*$")
  start_line <- which(grepl(block_pattern, code_lines))

  if (length(start_line) == 0) {
    return(NULL)
  }

  start_line <- start_line[1]  # Take first match

  # Find matching closing brace
  end_line <- find_matching_brace(code_lines, start_line)

  if (length(end_line) == 0) {
    return(NULL)
  }

  # Extract block content (excluding the block declaration and closing brace)
  if (end_line > start_line + 1) {
    block_content <- code_lines[(start_line + 1):(end_line - 1)]
    return(paste(block_content, collapse = "\n"))
  } else {
    return("")  # Empty block
  }
}

#' Find Matching Brace
#'
#' @description
#' Finds the line number of the closing brace that matches an opening brace.
#'
#' @param code_lines Character vector of Stan code lines
#' @param start_line Integer line number containing opening brace
#' @return Integer line number of matching closing brace or integer(0) if not found
#' @noRd
find_matching_brace <- function(code_lines, start_line) {
  checkmate::assert_character(code_lines)
  checkmate::assert_integerish(start_line)

  # Handle empty input
  if (length(code_lines) == 0 || length(start_line) == 0) {
    return(integer(0))
  }

  if (start_line < 1 || start_line > length(code_lines)) {
    return(integer(0))
  }

  # Count braces starting from start_line
  depth <- 0
  found_opening <- FALSE

  for (i in start_line:length(code_lines)) {
    line <- code_lines[i]

    # Count opening and closing braces in this line
    opening_braces <- lengths(regmatches(line, gregexpr("\\{", line)))
    closing_braces <- lengths(regmatches(line, gregexpr("\\}", line)))

    depth <- depth + opening_braces - closing_braces

    # Mark that we've seen at least one opening brace
    if (opening_braces > 0) {
      found_opening <- TRUE
    }

    # If depth returns to 0 after seeing an opening brace, we found the match
    if (found_opening && depth == 0) {
      return(i)
    }

    # If depth goes negative, braces are unbalanced
    if (depth < 0) {
      return(integer(0))
    }
  }

  # No matching brace found
  return(integer(0))
}

# Integration Support Utilities
# =============================

#' Prepare Stanvars for brms Integration
#'
#' @description
#' Prepares stanvars for integration with brms by filtering invalid ones
#' and ensuring proper format.
#'
#' @param stanvars List of stanvar objects
#' @return List of valid stanvars ready for brms
#' @noRd
prepare_stanvars_for_brms <- function(stanvars) {
  checkmate::assert_list(stanvars)

  if (length(stanvars) == 0) {
    return(list())
  }

  # Filter to valid stanvars only
  valid_stanvars <- list()

  for (name in names(stanvars)) {
    .stanvar <- stanvars[[name]]

    # brms stanvars are containers - validate the actual stanvar element
    stanvar_element <- if (inherits(.stanvar, "stanvars") && length(.stanvar) == 1) {
      .stanvar[[1]]
    } else {
      .stanvar
    }

    if (is_valid_stanvar(stanvar_element)) {
      valid_stanvars[[name]] <- .stanvar
    } else {
      insight::format_warning(
        "Skipping invalid stanvar: {.field {name}}",
        "Stanvar must have valid 'scode' and 'block' components."
      )
    }
  }

  return(valid_stanvars)
}

# Missing Integration Functions
# ============================

#' Generate Trend Injection Stanvars
#'
#' @description
#' Central dispatcher function that routes trend specifications to appropriate
#' trend-specific stanvar generators. This function bridges the gap between
#' Extract Trend Stanvars from Setup
#'
#' @description
#' Extracts trend stanvars from brms setup and generates additional trend-specific stanvars.
#' Integrates with the trend injection system.
#'
#' @param trend_setup List containing trend model setup
#' @param trend_specs List containing trend specification with 'dimensions' field
#' @param response_suffix Response suffix for multivariate models
#' @return List of trend stanvars
#' 
#' @details
#' trend_specs must contain a 'dimensions' field calculated by 
#' extract_time_series_dimensions() during data validation. This ensures
#' reliable timing information without circular dependencies.
#' 
#' @noRd
extract_trend_stanvars_from_setup <- function(trend_setup, trend_specs, 
                                              response_suffix = "") {
  checkmate::assert_list(trend_setup, names = "named")
  checkmate::assert_list(trend_specs, names = "named")
  checkmate::assert_string(response_suffix)

  # Extract base stanvars from trend setup
  base_stanvars <- trend_setup$stanvars %||% NULL

  # Generate trend-specific stanvars if trend spec is provided
  # Handle both trend_type and trend_model for compatibility
  trend_type <- trend_specs$trend_type %||% trend_specs$trend_model
  trend_stanvars <- if (!is.null(trend_specs) && !is.null(trend_type)) {
    # Dimensions should be pre-calculated and included in trend_specs
    # This eliminates circular dependency and ensures reliable dimension information
    dimensions <- trend_specs$dimensions
    
    if (is.null(dimensions)) {
      stop(insight::format_error(
        "Missing dimension information in trend specification.",
        "trend_specs must contain a 'dimensions' field with time series dimensions.",
        "This should be calculated using extract_time_series_dimensions() during data validation."
      ), call. = FALSE)
    }
    
    # Extract timing information from pre-calculated dimensions
    n_obs <- dimensions$n_obs
    n_time <- dimensions$n_time
    data_info <- list(
      n_obs = n_obs,
      n_series = dimensions$n_series,
      n_lv = trend_specs$n_lv,
      n_time = n_time,
      n_groups = trend_specs$n_groups,
      n_subgroups = trend_specs$n_subgroups,
      time_var = dimensions$time_var,
      series_var = dimensions$series_var
    )

    # Use the existing trend injection system with response suffix
    generate_trend_injection_stanvars(trend_specs, data_info, response_suffix)
  } else {
    NULL
  }

  # Combine base and trend-specific stanvars using proper method
  if (is.null(base_stanvars) && is.null(trend_stanvars)) {
    return(NULL)
  } else if (is.null(base_stanvars)) {
    return(trend_stanvars)
  } else if (is.null(trend_stanvars)) {
    return(base_stanvars)
  } else {
    # Use c() to properly combine stanvars objects
    return(c(base_stanvars, trend_stanvars))
  }
}

#' Inject Trend into Linear Predictor
#'
#' @description
#' Injects trend effects into Stan linear predictor by modifying the transformed parameters
#' or model block to add trend components to mu.
#'
#' @param base_stancode Character string of base Stan code
#' @param trend_stanvars List of trend stanvars
#' @return Modified Stan code with trend injection
#' @noRd
inject_trend_into_linear_predictor <- function(base_stancode, trend_stanvars) {
  checkmate::assert_string(base_stancode)
  checkmate::assert_list(trend_stanvars, null.ok = TRUE)

  # If no trends, return unchanged
  if (is.null(trend_stanvars) || length(trend_stanvars) == 0) {
    return(base_stancode)
  }

  # Split code into lines for easier manipulation
  code_lines <- strsplit(base_stancode, "\n", fixed = TRUE)[[1]]

  # Find the transformed parameters block or create one if it doesn't exist
  tp_block_start <- which(grepl("^\\s*transformed\\s+parameters\\s*\\{", code_lines))

  if (length(tp_block_start) == 0) {
    # No transformed parameters block exists, create one before model block
    model_block_start <- which(grepl("^\\s*model\\s*\\{", code_lines))

    if (length(model_block_start) == 0) {
      stop(insight::format_error(
        "Cannot find model block in Stan code for trend injection.",
        "Stan code structure is invalid."
      ))
    }

    # Insert transformed parameters block before model block
    new_tp_block <- c(
      "transformed parameters {",
      "  // Combined linear predictor with trend effects",
      "  vector[N] mu_combined = mu;",
      "",
      "  // Add trend effects if available",
      "  if (size(trend) > 0) {",
      "    mu_combined += trend[obs_ind];",
      "  }",
      "}"
    )

    # Insert the new block
    code_lines <- c(
      code_lines[1:(model_block_start[1] - 1)],
      new_tp_block,
      "",
      code_lines[model_block_start[1]:length(code_lines)]
    )

  } else {
    # Transformed parameters block exists, modify it
    tp_start <- tp_block_start[1]
    tp_end <- find_matching_brace(code_lines, tp_start)

    if (length(tp_end) == 0) {
      stop(insight::format_error(
        "Cannot find end of transformed parameters block.",
        "Stan code structure may be invalid."
      ))
    }

    # Find mu declaration and modify it
    mu_lines <- which(grepl("vector\\[.*\\]\\s+mu\\s*=", code_lines[(tp_start+1):(tp_end-1)]))

    if (length(mu_lines) > 0) {
      # mu is declared in transformed parameters, modify it
      mu_line_idx <- tp_start + mu_lines[1]

      # Insert trend addition after mu declaration
      trend_addition <- c(
        "",
        "  // Add trend effects",
        "  if (size(trend) > 0) {",
        "    mu += trend[obs_ind];",
        "  }"
      )

      code_lines <- c(
        code_lines[1:mu_line_idx],
        trend_addition,
        code_lines[(mu_line_idx+1):length(code_lines)]
      )

    } else {
      # mu not found in transformed parameters, add combined predictor
      trend_addition <- c(
        "  // Combined linear predictor with trend effects",
        "  vector[N] mu_combined = mu;",
        "  if (size(trend) > 0) {",
        "    mu_combined += trend[obs_ind];",
        "  }"
      )

      # Insert before closing brace of transformed parameters
      code_lines <- c(
        code_lines[1:(tp_end-1)],
        trend_addition,
        code_lines[tp_end:length(code_lines)]
      )

      # Update likelihood to use mu_combined instead of mu
      code_lines <- gsub("\\bmu\\b", "mu_combined", code_lines)
    }
  }

  # Rejoin the code
  modified_code <- paste(code_lines, collapse = "\n")

  return(modified_code)
}

#' Inject Multivariate Trends into Linear Predictors
#'
#' @description
#' Injects response-specific trend effects into multivariate Stan model.
#' Handles brms naming convention with _y1, _y2 suffixes.
#'
#' @param base_stancode Character string of base Stan code
#' @param trend_stanvars List of trend stanvars (may be NULL)
#' @param responses_with_trends Character vector of response names that have trends
#' @return Modified Stan code with trend injections
#' @noRd
inject_multivariate_trends_into_linear_predictors <- function(
  base_stancode, 
  trend_stanvars, 
  responses_with_trends
) {
  checkmate::assert_string(base_stancode)
  checkmate::assert_character(responses_with_trends, min.len = 1)
  
  # If no trends, return unchanged
  if (is.null(trend_stanvars) || length(trend_stanvars) == 0) {
    return(base_stancode)
  }
  
  # Split code into lines
  code_lines <- strsplit(base_stancode, "\n", fixed = TRUE)[[1]]
  
  # For each response with a trend, inject the trend addition
  for (resp_name in responses_with_trends) {
    # Find where mu_<resp> is computed in model block
    mu_pattern <- paste0("mu_", resp_name, "\\s*=")
    mu_lines <- which(grepl(mu_pattern, code_lines))
    
    if (length(mu_lines) > 0) {
      # Find the last line where mu_<resp> is modified
      last_mu_line <- max(mu_lines)
      
      # Look for the end of mu computation (before likelihood)
      # Usually before target += or before the next mu_ declaration
      insert_point <- last_mu_line
      for (i in (last_mu_line + 1):length(code_lines)) {
        if (grepl("target\\s*\\+=", code_lines[i]) ||
            grepl("mu_[a-zA-Z0-9]+\\s*=", code_lines[i])) {
          insert_point <- i - 1
          break
        }
        # Also check if we're still modifying mu_<resp>
        if (grepl(paste0("mu_", resp_name), code_lines[i])) {
          insert_point <- i
        }
      }
      
      # Insert trend addition after mu computation
      trend_addition <- c(
        paste0("    // Add trend effects for response ", resp_name),
        paste0("    if (size(trend_", resp_name, ") > 0) {"),
        paste0("      mu_", resp_name, " += trend_", resp_name, "[obs_ind_", resp_name, "];"),
        "    }"
      )
      
      code_lines <- c(
        code_lines[1:insert_point],
        trend_addition,
        code_lines[(insert_point + 1):length(code_lines)]
      )
    }
  }
  
  return(paste(code_lines, collapse = "\n"))
}

# Main Assembly Functions
# =======================

#' Assemble Complete mvgam Stan Code
#'
#' @description
#' Main orchestrator function that assembles complete Stan code for mvgam models
#' by combining observation and trend components.
#'
#' @param obs_formula Formula for observation model
#' @param trend_stanvars List of trend stanvars
#' @param data Data for the model
#' @param family Family specification
#' @param backend Stan backend ("rstan" or "cmdstanr")
#' @param validate Whether to validate final Stan code
#' @return Character string of complete Stan code
#' @noRd
assemble_mvgam_stan_code <- function(obs_formula, trend_stanvars = NULL, data,
                                    family = gaussian(), backend = "rstan",
                                    validate = TRUE) {
  checkmate::assert_class(obs_formula, "formula")
  checkmate::assert_list(trend_stanvars, null.ok = TRUE)
  checkmate::assert_data_frame(data)

  # Generate base brms Stan code
  base_stancode <- generate_base_brms_stancode(
    formula = obs_formula,
    data = data,
    family = family,
    stanvars = trend_stanvars,
    backend = backend
  )

  # If no trend stanvars, return base code
  if (is.null(trend_stanvars) || length(trend_stanvars) == 0) {
    if (validate) {
      base_stancode <- validate_stan_code(base_stancode, backend = backend)
    }
    return(base_stancode)
  }

  # Inject trend effects into linear predictor
  final_stancode <- inject_trend_into_linear_predictor(
    base_stancode,
    trend_stanvars
  )

  # Validate final code if requested
  if (validate) {
    final_stancode <- validate_stan_code(final_stancode, backend = backend)
  }

  return(final_stancode)
}

#' Assemble Complete mvgam Stan Data
#'
#' @description
#' Assembles complete Stan data for mvgam models by combining observation
#' and trend data components.
#'
#' @param obs_formula Formula for observation model
#' @param trend_stanvars List of trend stanvars
#' @param data Data for the model
#' @param family Family specification
#' @return List of complete Stan data
#' @noRd
assemble_mvgam_stan_data <- function(obs_formula, trend_stanvars = NULL, data,
                                    family = gaussian()) {
  checkmate::assert_class(obs_formula, "formula")
  checkmate::assert_list(trend_stanvars, null.ok = TRUE)
  checkmate::assert_data_frame(data)

  # Generate complete Stan data including trend stanvars
  # brms automatically incorporates stanvar data when provided
  standata <- generate_base_brms_standata(
    formula = obs_formula,
    data = data,
    family = family,
    stanvars = trend_stanvars
  )

  # Validate final data structure
  validate_stan_data_structure(standata)

  return(standata)
}

#' Generate Base brms Stan Code
#'
#' @description
#' Generates base Stan code using brms with optional stanvars injection.
#'
#' @param formula Formula for the model
#' @param data Data for the model
#' @param family Family specification
#' @param stanvars Optional stanvars to inject
#' @param backend Stan backend
#' @return Character string of Stan code
#' @noRd
generate_base_brms_stancode <- function(formula, data, family = gaussian(),
                                       stanvars = NULL, backend = "rstan") {
  checkmate::assert_class(formula, "formula")
  checkmate::assert_data_frame(data)

  # Use brms to generate Stan code
  stancode <-
    brms::make_stancode(
      formula = formula,
      data = data,
      family = family,
      stanvars = stanvars,
      backend = backend
    )

  return(stancode)
}

#' Generate Base brms Stan Data
#'
#' @description
#' Generates base Stan data using brms with optional stanvars injection.
#'
#' @param formula Formula for the model
#' @param data Data for the model
#' @param family Family specification
#' @param stanvars Optional stanvars to inject
#' @return List of Stan data
#' @noRd
generate_base_brms_standata <- function(formula, data, family = gaussian(),
                                       stanvars = NULL) {
  checkmate::assert_class(formula, "formula")
  checkmate::assert_data_frame(data)

  # Use brms to generate Stan data
  standata <-
    brms::make_standata(
      formula = formula,
      data = data,
      family = family,
      stanvars = stanvars
    )

  return(standata)
}

# Stan Component Combination Utilities
# ====================================

# Note: Legacy combine_stan_components() function removed.
# Modern system uses generate_combined_stancode() with two-stage assembly.

# =============================================================================
# SECTION 2: TREND-SPECIFIC STAN CODE GENERATORS
# =============================================================================
# WHY: Each trend type requires specialized Stan code with unique parameters,
# priors, and computational patterns. Generators provide modular, extensible
# Stan code creation that integrates seamlessly with the registry system
# and maintains consistency across trend types.


#' Combine Stanvars Robustly
#'
#' Single function to handle all stanvar combination patterns used in mvgam.
#' Handles NULL values, individual stanvars, stanvars collections, lists, and mixed inputs.
#' Preserves proper class structure by using brms c() method exclusively.
#'
#' @param ... Stanvar components to combine (can be NULL, stanvars, lists, or mixed)
#' @return Combined stanvars collection with proper class structure, or NULL if all inputs are NULL
#' @noRd
combine_stanvars <- function(...) {
  components <- list(...)

  # Flatten nested structures and filter valid components
  valid_components <- list()
  for (component in components) {
    if (!is.null(component)) {
      if (inherits(component, "stanvars")) {
        # Direct stanvars collection
        valid_components <- append(valid_components, list(component))
      } else if (inherits(component, "stanvar")) {
        # Single stanvar object
        valid_components <- append(valid_components, list(component))
      } else if (is.list(component)) {
        # Handle lists that might contain stanvar/stanvars objects
        for (item in component) {
          if (!is.null(item)) {
            if (inherits(item, "stanvars")) {
              valid_components <- append(valid_components, list(item))
            } else if (inherits(item, "stanvar")) {
              valid_components <- append(valid_components, list(item))
            } else {
              stop(insight::format_error(
                "Invalid item in list component:",
                paste("Class:", paste(class(item), collapse = ", ")),
                "Expected stanvar or stanvars object."
              ))
            }
          }
        }
      } else {
        stop(insight::format_error(
          "Invalid component type:",
          paste("Class:", paste(class(component), collapse = ", ")),
          "Expected stanvar, stanvars, list, or NULL."
        ))
      }
    }
  }

  # Return NULL if no valid components (brms expectation)
  if (length(valid_components) == 0) {
    return(NULL)
  }

  # Combine all components using brms c() method
  # Start with first component
  result <- valid_components[[1]]

  # Add remaining components
  if (length(valid_components) > 1) {
    for (i in 2:length(valid_components)) {
      result <- c(result, valid_components[[i]])
    }
  }

  # Validate result has proper class
  if (!inherits(result, c("stanvar", "stanvars"))) {
    stop(insight::format_error(
      "combine_stanvars produced invalid result.",
      paste("Result class:", paste(class(result), collapse = ", ")),
      "Expected stanvar or stanvars object."
    ))
  }

  return(result)
}

# =============================================================================
# SHARED GAUSSIAN INNOVATION SYSTEM
# =============================================================================
# WHY: Most trend types (RW, AR, VAR, CAR, ZMVN) use Gaussian innovations with
# common parameters (sigma_trend, raw_innovations, correlation matrices). This
# system provides unified generation of these shared stanvars to avoid duplication
# across trend-specific generators and ensure consistent naming/structure.
# MA transformations are handled by individual trend generators sequentially.

#' Generate Shared Gaussian Innovation Parameters
#'
#' Creates stanvar objects for parameters common to Gaussian innovation trends.
#' Generates raw innovations only - MA transformations handled by trend generators.
#'
#' @param n_lv Number of latent variables
#' @param n_series Number of time series
#' @param cor Logical, whether to include correlation parameters
#' @param factor_model Logical, whether this is a factor model
#' @param hierarchical_info List with hierarchical structure info (NULL for simple models)
#' @return List of stanvar objects for shared parameters
#' @noRd
generate_shared_innovation_stanvars <- function(n_lv, n_series, cor = FALSE,
                                               factor_model = FALSE,
                                               hierarchical_info = NULL) {

  # Determine effective dimension for innovations
  effective_dim <- if (factor_model) n_lv else n_series

  # Check for hierarchical structure
  is_hierarchical <- !is.null(hierarchical_info) && hierarchical_info$has_groups

  # Create individual stanvar components
  stanvar_components <- list()

  if (is_hierarchical) {
    # Hierarchical case: leverage existing hierarchical correlation functions
    n_groups <- hierarchical_info$n_groups
    n_subgroups <- effective_dim

    # Add existing hierarchical correlation infrastructure
    hierarchical_functions <- generate_hierarchical_functions()
    hierarchical_params <- generate_hierarchical_correlation_parameters(n_groups, n_subgroups)
    hierarchical_priors <- generate_hierarchical_correlation_model(n_groups)

    # Add to component list
    stanvar_components <- append(stanvar_components,
                                list(hierarchical_functions, hierarchical_params, hierarchical_priors))

    # Add sigma_trend parameter for hierarchical case
    sigma_stanvar <- brms::stanvar(
      name = "sigma_trend",
      scode = paste0("vector<lower=0>[", effective_dim, "] sigma_trend;"),
      block = "parameters"
    )
    stanvar_components <- append(stanvar_components, list(sigma_stanvar))

  } else {
    # Simple case: non-hierarchical innovations

    # 1. sigma_trend - innovation standard deviations
    sigma_code <- if (effective_dim == 1) {
      "vector<lower=0>[1] sigma_trend;"
    } else {
      paste0("vector<lower=0>[", effective_dim, "] sigma_trend;")
    }

    sigma_stanvar <- brms::stanvar(
      name = "sigma_trend",
      scode = sigma_code,
      block = "parameters"
    )
    stanvar_components <- append(stanvar_components, list(sigma_stanvar))

    # 2. Correlation parameters (only if cor = TRUE and multivariate)
    if (cor && effective_dim > 1) {
      # Cholesky factor for correlation matrix
      l_omega_stanvar <- brms::stanvar(
        name = "L_Omega_trend",
        scode = paste0("cholesky_factor_corr[", effective_dim, "] L_Omega_trend;"),
        block = "parameters"
      )
      stanvar_components <- append(stanvar_components, list(l_omega_stanvar))

      # Derived covariance matrix in transformed parameters
      sigma_matrix_code <- paste0(
        "matrix[", effective_dim, ", ", effective_dim, "] Sigma_trend = ",
        "diag_pre_multiply(sigma_trend, L_Omega_trend);"
      )

      sigma_matrix_stanvar <- brms::stanvar(
        name = "Sigma_trend",
        scode = sigma_matrix_code,
        block = "tparameters"
      )
      stanvar_components <- append(stanvar_components, list(sigma_matrix_stanvar))
    }
  }

  # 4. Raw innovations parameter (Stan will sample these with std_normal prior)
  raw_innovations_stanvar <- brms::stanvar(
    name = "raw_innovations",
    scode = paste0("matrix[n_trend, ", effective_dim, "] raw_innovations;"),
    block = "parameters"
  )
  stanvar_components <- append(stanvar_components, list(raw_innovations_stanvar))

  # 5. Final innovations in transformed parameters (after correlation/MA transformation)
  if (is_hierarchical) {
    # Hierarchical case: innovations depend on group structure
    final_innovations_code <- paste0("
    // Final innovations after applying hierarchical correlations
    matrix[n, ", effective_dim, "] LV_innovations;

    // Apply group-specific correlations to raw innovations
    for (g in 1:n_groups) {
      // Derived group-specific correlation matrices (using existing combine_cholesky)
      array[n_groups] cholesky_factor_corr[", effective_dim, "] L_Omega_group;
      for (g_idx in 1:n_groups) {
        L_Omega_group[g_idx] = combine_cholesky(L_Omega_global, L_deviation_group[g_idx], alpha_cor);
      }

      // Transform raw innovations using group correlations
      matrix[", effective_dim, ", ", effective_dim, "] Sigma_group = diag_pre_multiply(sigma_trend, L_Omega_group[g]);
      // Apply to group time points (individual generators will specify the indexing)
    }")
  } else if (cor && effective_dim > 1) {
    # Simple correlated case
    final_innovations_code <- paste0("
    // Final innovations after applying correlations
    matrix[n, ", effective_dim, "] LV_innovations;

    // Apply correlation transformation to raw innovations
    for (i in 1:n) {
      LV_innovations[i, :] = (Sigma_trend * raw_innovations[i, :]')';
    }")
  } else {
    # Uncorrelated case
    final_innovations_code <- paste0("
    // Final innovations (uncorrelated case)
    matrix[n, ", effective_dim, "] LV_innovations;

    // Apply scaling to raw innovations
    for (i in 1:n) {
      for (j in 1:", effective_dim, ") {
        LV_innovations[i, j] = sigma_trend[j] * raw_innovations[i, j];
      }
    }")
  }

  final_innovations_stanvar <- brms::stanvar(
    name = "final_innovations",
    scode = final_innovations_code,
    block = "tparameters"
  )
  stanvar_components <- append(stanvar_components, list(final_innovations_stanvar))

  # Combine all components using do.call to handle the list properly
  return(do.call(combine_stanvars, stanvar_components))
}

#' Generate Standard Priors for Gaussian Innovations
#'
#' Creates standard priors for shared Gaussian innovation parameters.
#'
#' @details
#' @stan_blocks model
#'
#' @param effective_dim Effective dimension (n_lv for factor models, n_series otherwise)
#' @param cor Logical, whether correlation parameters exist
#' @param is_hierarchical Logical, whether using hierarchical structure
#' @return Stanvar object with prior code
#' @noRd
generate_innovation_model <- function(effective_dim, cor = FALSE, is_hierarchical = FALSE) {

  if (is_hierarchical) {
    # Hierarchical priors are handled by generate_hierarchical_correlation_model
    prior_code <- c(
      "// Raw innovations prior",
      "to_vector(raw_innovations) ~ std_normal();"
    )
  } else {
    # Simple case priors
    prior_code <- c(
      "// Shared Gaussian innovation priors",
      "sigma_trend ~ exponential(1);"
    )

    if (cor && effective_dim > 1) {
      prior_code <- c(prior_code,
        "L_Omega_trend ~ lkj_corr_cholesky(2);"
      )
    }

    prior_code <- c(prior_code,
      "to_vector(raw_innovations) ~ std_normal();"
    )
  }

  brms::stanvar(
    name = "innovation_priors",
    scode = paste(prior_code, collapse = "\n  "),
    block = "model"
  )
}

#' Extract Hierarchical Information from Data Specifications
#'
#' Processes data specifications to extract hierarchical grouping structure.
#'
#' @param data_info Data information list
#' @param trend_specs Trend specification list
#' @return List with hierarchical structure information
#' @noRd
extract_hierarchical_info <- function(data_info, trend_specs) {

  has_groups <- !is.null(trend_specs$gr) && trend_specs$gr != 'NA'

  if (!has_groups) {
    return(NULL)
  }

  list(
    has_groups = TRUE,
    n_groups = data_info$n_groups %||% 1,
    n_subgroups = data_info$n_subgroups %||% data_info$n_lv %||% data_info$n_series,
    gr_var = trend_specs$gr,
    subgr_var = trend_specs$subgr %||% 'NA'
  )
}

#' Generate Common Trend Data Variables
#'
#' Creates standard data block stanvars needed by most trend types.
#' Uses trend-specific naming to avoid conflicts with brms variables.
#'
#' @details
#' @stan_blocks data
#'
#' @param n_obs Number of observations (will be named n_trend in Stan)
#' @param n_series Number of observed series
#' @param n_lv Number of latent variables (optional)
#' @return List of common data block stanvars
#' @noRd
generate_common_trend_data <- function(n_obs, n_series, n_lv = NULL) {
  checkmate::assert_number(n_obs, lower = 1)
  checkmate::assert_number(n_series, lower = 1)
  checkmate::assert_number(n_lv, lower = 1, null.ok = TRUE)

  # Create n_trend stanvar with trend-specific naming to avoid brms conflicts
  n_trend_stanvar <- brms::stanvar(
    x = n_obs,
    name = "n_trend",
    scode = "int<lower=1> n_trend;",
    block = "data"
  )

  # Create n_series_trend stanvar
  n_series_trend_stanvar <- brms::stanvar(
    x = n_series,
    name = "n_series_trend",
    scode = "int<lower=1> n_series_trend;",
    block = "data"
  )

  # Create n_lv_trend stanvar if n_lv is provided
  if (!is.null(n_lv)) {
    n_lv_trend_stanvar <- brms::stanvar(
      x = n_lv,
      name = "n_lv_trend",
      scode = "int<lower=1> n_lv_trend;",
      block = "data"
    )
    return(combine_stanvars(n_trend_stanvar, n_series_trend_stanvar, n_lv_trend_stanvar))
  } else {
    return(combine_stanvars(n_trend_stanvar, n_series_trend_stanvar))
  }
}

#' Generate Data Block Injections for Matrix Z
#'
#' @details
#' @stan_blocks data
#'
#' @param is_factor_model Logical indicating if this is a factor model
#' @param n_lv Number of latent variables
#' @param n_series Number of observed series
#' @return List of data block stanvars
#' @noRd
generate_matrix_z_data <- function(is_factor_model, n_lv, n_series) {
  # Create individual stanvar objects with both data and scode
  # brms requires data stanvars to have both x (data) and scode (declaration)
  n_lv_stanvar <- brms::stanvar(
    x = n_lv,
    name = "n_lv",
    scode = "int<lower=1> n_lv_trend;",
    block = "data"
  )

  n_series_stanvar <- brms::stanvar(
    x = n_series,
    name = "n_series",
    scode = "int<lower=1> n_series_trend;",
    block = "data"
  )

  # Combine using brms c() method to create proper stanvars collection
  return(c(n_lv_stanvar, n_series_stanvar))
}

#' Generate Parameter Block Injections for Matrix Z
#'
#' @details
#' @stan_blocks parameters
#'
#' @param is_factor_model Logical indicating if this is a factor model
#' @param n_lv Number of latent variables
#' @param n_series Number of observed series
#' @return List of parameter block stanvars
#' @noRd
generate_matrix_z_parameters <- function(is_factor_model, n_lv, n_series) {
  if (is_factor_model) {
    # Factor model: estimate Z in parameters for dimensionality reduction
    z_matrix_stanvar <- brms::stanvar(
      name = "Z",
      scode = glue::glue("matrix[n_series_trend, n_lv_trend] Z;"),
      block = "parameters"
    )
    return(z_matrix_stanvar)
  } else {
    # Return NULL for empty case
    return(NULL)
  }
}

#' Generate Transformed Data Block Injections for Matrix Z
#'
#' @details
#' @stan_blocks transformed_data
#'
#' @param is_factor_model Logical indicating if this is a factor model
#' @param n_lv Number of latent variables
#' @param n_series Number of observed series
#' @return List of transformed data block stanvars
#' @noRd
generate_matrix_z_tdata <- function(is_factor_model, n_lv, n_series) {
  if (!is_factor_model) {
    # Non-factor model: diagonal Z in transformed data
    z_matrix_stanvar <- brms::stanvar(
      name = "Z",
      scode = glue::glue("matrix[n_series_trend, n_lv_trend] Z = diag_matrix(rep_vector(1.0, n_lv_trend));"),
      block = "tdata"
    )
    return(z_matrix_stanvar)
  } else {
    # Return NULL for empty case
    return(NULL)
  }
}

#' Generate Matrix Z Multiblock Stanvars (Consolidated Utility)
#'
#' Combines all matrix Z injection functions for factor/non-factor models.
#' This provides a single interface for matrix Z generation across all Stan blocks.
#'
#' @details
#' @stan_blocks data, parameters, transformed_data
#'
#' @param is_factor_model Logical indicating if this is a factor model
#' @param n_lv Number of latent variables
#' @param n_series Number of observed series
#' @return List of stanvars for matrix Z across all required blocks
#' @noRd
generate_matrix_z_multiblock_stanvars <- function(is_factor_model, n_lv, n_series) {
  # Get individual stanvar components
  data_stanvars <- generate_matrix_z_data(is_factor_model, n_lv, n_series)
  param_stanvars <- generate_matrix_z_parameters(is_factor_model, n_lv, n_series)
  tdata_stanvars <- generate_matrix_z_tdata(is_factor_model, n_lv, n_series)

  # Combine using brms's native c() method for stanvars
  # This should preserve proper class structure
  combined_stanvars <- data_stanvars
  if (!is.null(param_stanvars)) {
    combined_stanvars <- c(combined_stanvars, param_stanvars)
  }
  if (!is.null(tdata_stanvars)) {
    combined_stanvars <- c(combined_stanvars, tdata_stanvars)
  }

  return(combined_stanvars)
}

#' Generate Factor Model Block Code
#'
#' Provides standardized priors for factor models with fixed variance=1 constraint.
#' Only generates priors when is_factor_model=TRUE.
#'
#' @details
#' @stan_blocks model
#'
#' @param is_factor_model Logical indicating if this is a factor model
#' @param n_lv Number of latent variables
#' @return List of stanvars for factor model priors
#' @noRd
generate_factor_model <- function(is_factor_model, n_lv) {
  if (!is_factor_model) {
    return(NULL)
  }

  # Factor model: fixed variance=1 for identifiability, priors for Z
  factor_lv_priors <- brms::stanvar(
    name = "factor_lv_priors",
    scode = "to_vector(LV_raw) ~ std_normal();",
    block = "model"
  )

  factor_z_priors <- brms::stanvar(
    name = "factor_z_priors",
    scode = "to_vector(Z) ~ normal(0, 1);",
    block = "model"
  )

  return(combine_stanvars(factor_lv_priors, factor_z_priors))
}

#' Generate Transformed Parameters Block Injections for Trend Computation
#'
#' WHY: All trends must use the same computation pattern:
#' trend[i,s] = dot_product(Z[s,:], LV[i,:]) + mu_trend[ytimes[i,s]]
#'
#' @details
#' @stan_blocks transformed_parameters
#'
#' @param n_lv Number of latent variables
#' @param n_series Number of observed series
#' @return List of transformed parameters block stanvars
#' @noRd
generate_trend_computation_tparameters <- function(n_lv, n_series) {
  # Create individual stanvar
  trend_computation_stanvar <- brms::stanvar(
    name = "trend",
    scode = glue::glue("
      // Derived latent trends using universal computation pattern
      matrix[n_trend, n_series_trend] trend;

      // Universal trend computation: state-space dynamics + linear predictors
      // dot_product captures dynamic component, mu_trend captures trend_formula
      for (i in 1:n) {{
        for (s in 1:n_series) {{
          trend[i, s] = dot_product(Z[s, :], LV[i, :]) + mu_trend[ytimes[i, s]];
        }}
      }}
    "),
    block = "tparameters"
  )

  return(trend_computation_stanvar)
}

#' Generate Functions Block Injections for Hierarchical Correlations
#'
#' WHY: All trends (AR, VAR, CAR, ZMVN) need the same hierarchical correlation
#' machinery when groups are specified.
#'
#' @details
#' @stan_blocks functions
#'
#' @return List of functions block stanvars
#' @noRd
generate_hierarchical_functions <- function() {
  return(brms::stanvar(
    name = "combine_cholesky",
    scode = "
      /* Function to compute a partially pooled correlation matrix
       * Combines global correlation structure with group-specific deviations
       * alpha controls mixing: 1 = pure global, 0 = pure local
       */
      matrix combine_cholesky(matrix global_chol_cor, matrix local_chol_cor,
                              real alpha) {
        int dim = rows(local_chol_cor);
        matrix[dim, dim] global_cor = multiply_lower_tri_self_transpose(global_chol_cor);
        matrix[dim, dim] local_cor = multiply_lower_tri_self_transpose(local_chol_cor);
        matrix[dim, dim] combined_chol_cor;
        combined_chol_cor = cholesky_decompose(alpha * global_cor
                                               + (1 - alpha) * local_cor);
        return combined_chol_cor;
      }
    ",
    block = "functions"
  ))
}

#' Generate Parameters Block Injections for Hierarchical Correlations
#'
#' @details
#' @stan_blocks parameters
#'
#' @param n_groups Number of groups for hierarchical modeling
#' @param n_subgroups Number of subgroups (typically n_lv)
#' @return List of parameters block stanvars
#' @noRd
generate_hierarchical_correlation_parameters <- function(n_groups, n_subgroups) {
  l_omega_global <- brms::stanvar(
    name = "L_Omega_global",
    scode = glue::glue("cholesky_factor_corr[{n_subgroups}] L_Omega_global;"),
    block = "parameters"
  )

  l_deviation_group <- brms::stanvar(
    name = "L_deviation_group",
    scode = glue::glue("array[{n_groups}] cholesky_factor_corr[{n_subgroups}] L_deviation_group;"),
    block = "parameters"
  )

  alpha_cor <- brms::stanvar(
    name = "alpha_cor",
    scode = "real<lower=0, upper=1> alpha_cor;",
    block = "parameters"
  )

  return(combine_stanvars(l_omega_global, l_deviation_group, alpha_cor))
}

#' Generate Model Block Injections for Hierarchical Correlation Priors
#'
#' @details
#' @stan_blocks model
#'
#' @param n_groups Number of groups for hierarchical modeling
#' @return List of model block stanvars
#' @noRd
generate_hierarchical_correlation_model <- function(n_groups) {
  alpha_cor_prior <- brms::stanvar(
    name = "alpha_cor_prior",
    scode = "alpha_cor ~ beta(3, 2);",
    block = "model"
  )

  l_omega_global_prior <- brms::stanvar(
    name = "L_Omega_global_prior",
    scode = "L_Omega_global ~ lkj_corr_cholesky(1);",
    block = "model"
  )

  l_deviation_group_prior <- brms::stanvar(
    name = "L_deviation_group_prior",
    scode = glue::glue("for (g in 1:{n_groups}) {{ L_deviation_group[g] ~ lkj_corr_cholesky(6); }}"),
    block = "model"
  )

  return(combine_stanvars(alpha_cor_prior, l_omega_global_prior, l_deviation_group_prior))
}

#' Generate Trend Injection Stanvars
#'
#' Dispatches to appropriate trend generator based on trend_specs type.
#'
#' @param trend_specs Trend specification object
#' @param data_info Data information list
#' @return List of stanvars for trend injection
#' @noRd
generate_trend_injection_stanvars <- function(trend_specs, data_info, response_suffix = "") {
  # Validate inputs
  checkmate::assert_list(trend_specs)
  checkmate::assert_list(data_info)
  checkmate::assert_string(response_suffix)

  # Get trend type directly from trend object (no parsing needed!)
  trend_type <- trend_specs$trend
  if (is.null(trend_type)) {
    stop(insight::format_error(
      "trend_specs must contain {.field trend} field"
    ))
  }

  # Use registry-enhanced dispatch with clear error messages
  generator_function_name <- paste0("generate_", tolower(trend_type), "_trend_stanvars")
  
  # Check if the generator function exists
  if (!exists(generator_function_name, mode = "function")) {
    # Provide helpful guidance using registry information
    available_trends <- ls(trend_registry)
    stop(insight::format_error(
      "No Stan generator found for trend type: {.field {trend_type}}",
      "Expected function: {.field {generator_function_name}}",
      if (length(available_trends) > 0) {
        paste0("Available trend types: {.field {paste(available_trends, collapse = ', ')}}")
      } else {
        "Registry appears empty. Check that register_core_trends() was called."
      }
    ))
  }
  
  generator_function <- get(generator_function_name, mode = "function")

  # Extract hierarchical information for shared innovation system
  hierarchical_info <- extract_hierarchical_info(data_info, trend_specs)

  # Determine if this trend uses shared Gaussian innovations
  uses_shared_innovations <- trend_specs$shared_innovations %||%
                            (!trend_type %in% c("PW", "VAR"))  # PW and VAR opt out of shared innovations

  # Generate shared innovation stanvars if needed
  shared_stanvars <- NULL
  if (uses_shared_innovations) {
    # Extract relevant parameters for shared system
    n_lv <- trend_specs$n_lv %||% data_info$n_lv %||% data_info$n_series %||% 1
    n_series <- data_info$n_series %||% 1
    cor <- trend_specs$cor %||% FALSE
    factor_model <- !is.null(trend_specs$n_lv) && trend_specs$n_lv < n_series

    # Generate shared innovation stanvars
    shared_stanvars <- generate_shared_innovation_stanvars(
      n_lv = n_lv,
      n_series = n_series,
      cor = cor,
      factor_model = factor_model,
      hierarchical_info = hierarchical_info
    )

    # Add shared priors
    effective_dim <- if (factor_model) n_lv else n_series
    is_hierarchical <- !is.null(hierarchical_info) && hierarchical_info$has_groups

    shared_priors <- generate_innovation_model(
      effective_dim = effective_dim,
      cor = cor,
      is_hierarchical = is_hierarchical
    )

    shared_stanvars$priors <- shared_priors
    
    # Apply response suffix post-processing to shared stanvars for multivariate models
    if (response_suffix != "") {
      for (name in names(shared_stanvars)) {
        if (!is.null(shared_stanvars[[name]])) {
          shared_stanvars[[name]] <- apply_response_suffix_to_stanvars(shared_stanvars[[name]], response_suffix)
        }
      }
    }
  }

  # Generate trend-specific stanvars using consistent naming convention
  trend_stanvars <- generator_function(trend_specs, data_info)
  
  # Apply response suffix post-processing for multivariate models
  if (response_suffix != "") {
    trend_stanvars <- apply_response_suffix_to_stanvars(trend_stanvars, response_suffix)
  }

  # Combine shared and trend-specific stanvars
  if (!is.null(shared_stanvars)) {
    # shared_stanvars is a named list of stanvars objects, preserve the names
    # Combine with trend stanvars while preserving names from both sources
    combined_stanvars <- combine_stanvars(shared_stanvars, trend_stanvars)
    return(combined_stanvars)
  } else {
    return(trend_stanvars)
  }
}

#' Random Walk Trend Generator
#'
#' Generates Stan code components for random walk trends.
#'
#' @param trend_specs Trend specification for RW model
#' @param data_info Data information including dimensions
#' @return List of stanvars for RW trend
#' @noRd
generate_rw_trend_stanvars <- function(trend_specs, data_info) {
  # Extract dimensions
  n_lv <- trend_specs$n_lv %||% 1
  n_series <- data_info$n_series %||% 1
  is_factor_model <- !is.null(trend_specs$n_lv) && n_lv < n_series

  # Build components list
  components <- list()

  # 1. Matrix Z for factor models
  matrix_z <- generate_matrix_z_multiblock_stanvars(is_factor_model, n_lv, n_series)
  if (!is.null(matrix_z)) {
    components <- append(components, list(matrix_z))
  }

  # 2. MA parameters if needed
  if (trend_specs$ma %||% FALSE) {
    ma_components <- generate_ma_components(n_lv)
    components <- append(components, ma_components)
  }

  # 3. RW dynamics (always needed)
  dynamics <- generate_rw_dynamics(n_lv, has_ma = trend_specs$ma %||% FALSE)
  if (!is.null(dynamics)) {
    components <- append(components, list(dynamics))
  }

  # 4. Trend computation
  trend_computation <- generate_trend_computation_tparameters(n_lv, n_series)
  if (!is.null(trend_computation)) {
    components <- append(components, list(trend_computation))
  }

  # 5. Factor model priors if applicable
  if (is_factor_model) {
    factor_priors <- generate_factor_model(is_factor_model, n_lv)
    if (!is.null(factor_priors)) {
      components <- append(components, list(factor_priors))
    }
  }

  # Use the robust combine_stanvars function
  return(do.call(combine_stanvars, components))
}

#' Generate MA Components for Trend Models
#' @noRd
generate_ma_components <- function(n_lv) {
  list(
    # MA coefficient parameter
    theta1_param = brms::stanvar(
      name = "theta1_trend",
      scode = glue::glue("vector<lower=-1,upper=1>[n_lv_trend] theta1_trend;"),
      block = "parameters"
    ),
    # MA prior
    theta1_prior = brms::stanvar(
      name = "theta1_trend_prior",
      scode = "theta1_trend ~ normal(0, 0.5);",
      block = "model"
    )
  )
}

#' Generate RW Dynamics Stan Code
#' @noRd
generate_rw_dynamics <- function(n_lv, has_ma = FALSE) {
  if (has_ma) {
    # RW with MA transformation
    brms::stanvar(
      name = "LV",
      scode = glue::glue("
        // Latent states with RW dynamics
        matrix[n_trend, n_lv_trend] LV;
        matrix[n_trend, n_lv_trend] LV_innovations_transformed = LV_innovations;

        // Apply MA(1) transformation if needed
        for (i in 2:n_trend) {{
          for (j in 1:n_lv_trend) {{
            LV_innovations_transformed[i, j] += theta1_trend[j] * LV_innovations_transformed[i-1, j];
          }}
        }}

        // Apply RW dynamics
        LV[1, :] = LV_innovations_transformed[1, :];
        for (i in 2:n_trend) {{
          LV[i, :] = LV[i-1, :] + LV_innovations_transformed[i, :];
        }}
      "),
      block = "tparameters"
    )
  } else {
    # Simple RW dynamics
    brms::stanvar(
      name = "LV",
      scode = glue::glue("
        // Latent states with simple RW dynamics
        matrix[n_trend, n_lv_trend] LV;

        // Apply RW dynamics using innovations from shared system
        LV[1, :] = LV_innovations[1, :];
        for (i in 2:n_trend) {{
          LV[i, :] = LV[i-1, :] + LV_innovations[i, :];
        }}
      "),
      block = "tparameters"
    )
  }
}

#' AR Trend Generator
#'
#' Generates Stan code components for autoregressive trends.
#' Supports factor models, hierarchical correlations, and proper ar{lag}_trend naming.
#' Uses consistent non-centered parameterization throughout.
#'
#' @param trend_specs Trend specification for AR model
#' @param data_info Data information including dimensions
#' @return List of stanvars for AR trend
#' @noRd
generate_ar_trend_stanvars <- function(trend_specs, data_info) {
  # Extract key parameters
  n_lv <- trend_specs$n_lv %||% 1
  lags <- trend_specs$lags %||% 1
  n <- data_info$n_obs
  n_series <- data_info$n_series %||% 1
  use_grouping <- !is.null(trend_specs$gr) && trend_specs$gr != 'NA'
  unit_var <- trend_specs$unit %||% "time"

  # Convert lags to ar_lags if needed
  ar_lags <- if (is.list(trend_specs$ar_lags)) trend_specs$ar_lags else (1:lags)
  has_ma <- trend_specs$ma %||% FALSE

  # Determine if this is a factor model (n_lv < n_series)
  is_factor_model <- !is.null(trend_specs$n_lv) && n_lv < n_series

  # Generate common trend data variables first
  common_data_stanvars <- generate_common_trend_data(n, n_series, n_lv)

  # Generate block-specific injectors for matrix Z
  matrix_z_stanvars <- generate_matrix_z_multiblock_stanvars(is_factor_model, n_lv, n_series)

  # Start with common data stanvars, then add matrix Z stanvars
  result_stanvars <- combine_stanvars(common_data_stanvars, matrix_z_stanvars)

  # Build AR coefficient declarations
  ar_params <- paste0(
    sapply(ar_lags, function(lag) {
      glue::glue("vector<lower=-1,upper=1>[n_lv_trend] ar{lag}_trend;")
    }),
    collapse = "\n    "
  )

  # Build AR dynamics computation
  ar_terms <- paste0(
    sapply(ar_lags, function(lag) {
      glue::glue("ar{lag}_trend[j] * LV[i-{lag}, j]")
    }),
    collapse = " + "
  )

  # Max lag for initialization
  max_lag <- max(ar_lags)

  if (use_grouping) {
    # Hierarchical AR case: AR(p = 1, unit = site, gr = group, subgr = species)
    n_groups <- data_info$n_groups %||% 1
    n_subgroups <- data_info$n_subgroups %||% n_lv

    # AR-specific parameters for hierarchical case (using proper ar{lag}_trend naming)
    ar_hierarchical_params_stanvar <- brms::stanvar(
      name = "ar_hierarchical_params",
      scode = paste0(
        "// AR coefficients for each latent variable\n",
        paste0(
          sapply(ar_lags, function(lag) {
            glue::glue("        vector<lower=-1,upper=1>[n_lv_trend] ar{lag}_trend;")
          }),
          collapse = "\n"
        ),
        "\n        // Latent states\n",
        "        matrix[n_trend, n_lv_trend] LV;"
      ),
      block = "parameters"
    )

    # AR model for hierarchical case
    ar_hierarchical_model_stanvar <- brms::stanvar(
      name = "ar_hierarchical_model",
      scode = glue::glue("
        // Initialize first {max_lag} time points
        for (i in 1:{max_lag}) {{
          LV[i, :] = LV_innovations[i, :];
        }}

        // AR dynamics for remaining time points
        for (i in {max_lag + 1}:n_trend) {{
          for (j in 1:n_lv_trend) {{
            LV[i, j] = {ar_terms} + LV_innovations[i, j];
          }}
        }}
      "),
      block = "model"
    )

    # Combine stanvars for hierarchical case
    result_stanvars <- combine_stanvars(result_stanvars, ar_hierarchical_params_stanvar,
                                      ar_hierarchical_model_stanvar)

  } else {
    # Simple AR case (no grouping)
    if (is_factor_model) {
      # Factor model AR parameters
      ar_params_stanvar <- brms::stanvar(
        name = "ar_params",
        scode = ar_params,
        block = "parameters"
      )

      ar_transformed_stanvar <- brms::stanvar(
        name = "ar_transformed_dynamics",
        scode = glue::glue("
          // Latent states with AR dynamics
          matrix[n_trend, n_lv_trend] LV;

          // Initialize first {max_lag} time points
          for (i in 1:{max_lag}) {{
            LV[i, :] = LV_innovations[i, :];
          }}

          // AR dynamics for remaining time points
          for (i in {max_lag + 1}:n_trend) {{
            for (j in 1:n_lv_trend) {{
              LV[i, j] = {ar_terms} + LV_innovations[i, j];
            }}
          }}
        "),
        block = "tparameters"
      )

      result_stanvars <- combine_stanvars(result_stanvars, ar_params_stanvar, ar_transformed_stanvar)

      # Use shared factor model priors
      factor_priors <- generate_factor_model(is_factor_model, n_lv)

      # Add AR-specific priors for factor model
      ar_factor_priors_stanvar <- brms::stanvar(
        name = "ar_factor_priors",
        scode = paste0(
          sapply(ar_lags, function(lag) {
            glue::glue("ar{lag}_trend ~ normal(0, 0.5);")
          }),
          collapse = "\n  "
        ),
        block = "model"
      )

      result_stanvars <- combine_stanvars(result_stanvars, factor_priors, ar_factor_priors_stanvar)
    } else {
      # Non-factor AR model
      ar_params_stanvar <- brms::stanvar(
        name = "ar_params",
        scode = ar_params,
        block = "parameters"
      )

      ar_model_stanvar <- brms::stanvar(
        name = "ar_model_dynamics",
        scode = glue::glue("
          // Initialize first {max_lag} time points
          for (i in 1:{max_lag}) {{
            LV[i, :] = LV_innovations[i, :];
          }}

          // AR dynamics for remaining time points
          for (i in {max_lag + 1}:n_trend) {{
            for (j in 1:n_lv_trend) {{
              LV[i, j] = {ar_terms} + LV_innovations[i, j];
            }}
          }}

          // AR priors
          {paste0(
            sapply(ar_lags, function(lag) {
              glue::glue(ar{lag}_trend ~ normal(0, 0.5);)
            })",
            collapse = "\n"
        ),
        block = "model"
      )

      result_stanvars <- combine_stanvars(result_stanvars, ar_params_stanvar, ar_model_stanvar)
    }
  }

  # Add trend computation stanvars
  trend_computation <- generate_trend_computation_tparameters(n_lv, n_series)
  result_stanvars <- combine_stanvars(result_stanvars, trend_computation)

  return(result_stanvars)
}

#' VAR Trend Generator
#'
#' Generates Stan code components for vector autoregressive trends.
#' Supports factor models, hierarchical correlations, and consistent matrix Z patterns.
#'
#' @param trend_specs Trend specification for VAR model
#' @param data_info Data information including dimensions
#' @return List of stanvars for VAR trend
#' @noRd
generate_var_trend_stanvars <- function(trend_specs, data_info) {
  # Extract key parameters
  n_lv <- trend_specs$n_lv %||% 1
  lags <- trend_specs$lags %||% 1
  n <- data_info$n_obs
  n_series <- data_info$n_series %||% 1
  use_grouping <- !is.null(trend_specs$gr) && trend_specs$gr != 'NA'
  unit_var <- trend_specs$unit %||% "time"

  # Determine if this is a factor model (n_lv < n_series)
  is_factor_model <- !is.null(trend_specs$n_lv) && n_lv < n_series

  # Generate common trend data variables first
  common_data_stanvars <- generate_common_trend_data(n, n_series, n_lv)

  # Generate block-specific injectors for matrix Z
  matrix_z_stanvars <- generate_matrix_z_multiblock_stanvars(is_factor_model, n_lv, n_series)

  # Start with common data stanvars, then add matrix Z stanvars
  result_stanvars <- combine_stanvars(common_data_stanvars, matrix_z_stanvars)

  # VAR data block (needed for all cases)
  var_data_stanvar <- brms::stanvar(
    x = list(
      n_lv = n_lv,
      n_lags = lags,
      lv_coefs = array(0, dim = c(n_lv, n_lv, lags))
    ),
    name = "var_data",
    scode = glue::glue("
    int<lower=1> n_lv_trend;
    int<lower=1> n_lags;
    array[n_lv_trend, n_lv_trend, n_lags] real lv_coefs;
    ")
  )

  if (use_grouping) {
    # Hierarchical VAR case: VAR(p = 1, unit = site, gr = group, subgr = species)
    n_groups <- data_info$n_groups %||% 1
    n_subgroups <- data_info$n_subgroups %||% n_lv

    # Note: Hierarchical correlation utilities are now handled centrally
    # to avoid stanvar name duplication across trend generators

    # VAR-specific parameters for hierarchical case
    var_hierarchical_params_stanvar <- brms::stanvar(
      name = "var_hierarchical_params",
      scode = glue::glue("
        // VAR coefficient matrices for each lag
        array[{lags}] matrix[n_lv_trend, n_lv_trend] A_trend;
        // Latent states
        matrix[n_trend, n_lv_trend] LV;
      "),
      block = "parameters"
    )

    # Hierarchical VAR model implementation
    var_hierarchical_model_stanvar <- brms::stanvar(
      name = "var_hierarchical_model",
      scode = glue::glue("
        // Derived hierarchical correlation matrices
        array[{n_groups}] cholesky_factor_corr[{n_subgroups}] L_Omega_group;
        for (g in 1 : {n_groups}) {{
          L_Omega_group[g] = combine_cholesky(L_Omega_global, L_deviation_group[g],
                                              alpha_cor);
        }}

        // Hierarchical VAR process with group-specific correlations
        for (i in 1:n_{unit_var}) {{
          for (g in 1:{n_groups}) {{
            // VAR dynamics with hierarchical residual correlation
            for (t in ({lags}+1):n_trend) {{
              vector[{n_subgroups}] mu = rep_vector(0, {n_subgroups});
              for (lag in 1:{lags}) {{
                mu += A_trend[lag] * LV[t-lag, :]';
              }}
              to_vector(LV[group_unit_indices[i, g], t]) ~ multi_normal_cholesky(mu, L_Omega_group[g]);
            }}
          }}
        }}

        // VAR coefficient priors
        for (lag in 1:{lags}) {{
          to_vector(A_trend[lag]) ~ normal(0, 0.5);
        }}
      "),
      block = "model"
    )

    # Note: Hierarchical priors are now handled centrally

    # Combine stanvars for hierarchical case (without hierarchical utilities)
    result_stanvars <- combine_stanvars(result_stanvars, var_data_stanvar,
                                      var_hierarchical_params_stanvar,
                                      var_hierarchical_model_stanvar)

  } else {
    # Simple VAR case (no grouping)
    if (is_factor_model) {
      # Factor model: fixed variance in covariance, estimate Z
      var_params_stanvar <- brms::stanvar(
        name = "var_params",
        scode = glue::glue("
        parameters {{
          // VAR coefficient matrices for each lag
          array[{lags}] matrix[n_lv_trend, n_lv_trend] A;
          // Innovation correlation matrix (variances fixed to 1)
          corr_matrix[n_lv_trend] Omega;
          // Latent states
          matrix[n_trend, n_lv_trend] LV;
        }}
        "),
        block = "parameters"
      )

      # VAR model block for factor case
      var_model_stanvar <- brms::stanvar(
        name = "var_model",
        scode = glue::glue("
        model {{
          // VAR process with fixed variance = 1
          for (t in ({lags}+1):n_trend) {{
            vector[n_lv_trend] mu = rep_vector(0, n_lv_trend);
            for (lag in 1:{lags}) {{
              mu += A_trend[lag] * LV[t-lag, :]';
            }}
            LV[t, :] ~ multi_normal(mu', Omega);
          }}

          // Priors for factor model
          for (lag in 1:{lags}) {{
            to_vector(A_trend[lag]) ~ normal(0, 0.5);
          }}
          Omega ~ lkj_corr(2);
        }}
        "),
        block = "model"
      )
    } else {
      # Non-factor model: estimate full covariance, diagonal Z
      var_params_stanvar <- brms::stanvar(
        name = "var_params",
        scode = glue::glue("
        parameters {{
          // VAR coefficient matrices for each lag
          array[{lags}] matrix[n_lv_trend, n_lv_trend] A;
          // Innovation covariance matrix
          cov_matrix[n_lv_trend] Sigma_trend;
          // Latent states
          matrix[n_trend, n_lv_trend] LV;
        }}
        "),
        block = "parameters"
      )

      # VAR model block for non-factor case
      var_model_stanvar <- brms::stanvar(
        name = "var_model",
        scode = glue::glue("
        model {{
          // VAR process
          for (t in ({lags}+1):n_trend) {{
            vector[n_lv_trend] mu = rep_vector(0, n_lv_trend);
            for (lag in 1:{lags}) {{
              mu += A_trend[lag] * LV[t-lag, :]';
            }}
            LV[t, :] ~ multi_normal(mu', Sigma_trend);
          }}

          // Priors for non-factor model
          for (lag in 1:{lags}) {{
            to_vector(A_trend[lag]) ~ normal(0, 0.5);
          }}
          Sigma_trend ~ inv_wishart(n_lv_trend + 1, diag_matrix(rep_vector(1, n_lv_trend)));
        }}
        "),
        block = "model"
      )
    }

    # Combine stanvars for non-hierarchical case
    result_stanvars <- combine_stanvars(result_stanvars, var_data_stanvar,
                                      var_params_stanvar, var_model_stanvar)

    # Use shared factor model priors if applicable
    if (is_factor_model) {
      factor_priors <- generate_factor_model(is_factor_model, n_lv)
      result_stanvars <- combine_stanvars(result_stanvars, factor_priors)
    }
  }

  # Add trend computation stanvars
  trend_computation <- generate_trend_computation_tparameters(n_lv, n_series)
  result_stanvars <- combine_stanvars(result_stanvars, trend_computation)

  return(result_stanvars)
}

#' Calculate Time Distances for CAR Models
#'
#' Calculate temporal distances between observations for continuous-time AR.
#' Uses pmax(1e-3, dis_time) to prevent zero distances.
#'
#' @param data_info Data information containing data, time variable, and series
#' @return Matrix of time distances [n, n_series]
#' @noRd
calculate_car_time_distances <- function(data_info) {
  data <- data_info$data
  time_var <- data_info$time_var %||% "time"
  series_var <- data_info$series_var %||% "series"

  # Prepare time and series data
  all_times <- data.frame(
    series = as.numeric(data[[series_var]]),
    time = data[[time_var]]
  ) %>%
    dplyr::group_by(series) %>%
    dplyr::arrange(time) %>%
    dplyr::mutate(
      time_lag = dplyr::lag(time),
      dis_time = time - time_lag,
      dis_time = ifelse(is.na(dis_time), 1, dis_time),
      dis_time = pmax(1e-3, dis_time)  # Critical: cannot let distance go to zero
    ) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(time, series)

  # Convert to matrix format [n_time, n_series]
  n_time <- length(unique(all_times$time))
  n_series <- length(unique(all_times$series))

  time_dis <- matrix(
    NA,
    nrow = n_time,
    ncol = n_series
  )

  for (s in seq_len(n_series)) {
    series_data <- all_times[all_times$series == s, ]
    time_dis[seq_len(nrow(series_data)), s] <- series_data$dis_time
  }

  return(time_dis)
}

#' CAR Trend Generator
#'
#' Generates Stan code components for continuous-time autoregressive trends.
#' Does NOT support factor models or hierarchical correlations.
#'
#' @param trend_specs Trend specification for CAR model
#' @param data_info Data information including dimensions
#' @return List of stanvars for CAR trend
#' @noRd
generate_car_trend_stanvars <- function(trend_specs, data_info) {
  # Extract key parameters
  n_lv <- trend_specs$n_lv %||% data_info$n_series %||% 1
  n <- data_info$n_obs
  n_series <- data_info$n_series %||% 1

  # Generate common trend data variables first
  common_data_stanvars <- generate_common_trend_data(n, n_series, n_lv)

  # CAR does not support factor models (continuous-time AR requires
  # series-specific temporal evolution)
  if (!is.null(trend_specs$n_lv) && trend_specs$n_lv < n_series) {
    stop(insight::format_error(
      "CAR trends do not support factor models (n_lv < n_series).",
      "Continuous-time AR requires series-specific temporal evolution modeling."
    ))
  }

  # CAR does not support hierarchical correlations
  if (!is.null(trend_specs$gr) && trend_specs$gr != 'NA') {
    rlang::warn(
      "CAR trends do not support hierarchical correlations; ignoring 'gr' parameter",
      .frequency = "once",
      .frequency_id = "CAR_group_error"
    )
  }

  # Calculate time distances for continuous-time AR evolution
  time_dis <- calculate_car_time_distances(data_info)

  # Time distance data for continuous-time AR
  time_dis_data_stanvar <- brms::stanvar(
    x = time_dis,
    name = "time_dis",
    scode = glue::glue("  array[n, n_series_trend] real<lower=0> time_dis;"),
    block = "data"
  )

  # CAR parameters (continuous-time AR1)
  car_params_stanvar <- brms::stanvar(
    name = "car_params",
    scode = glue::glue("
  // CAR AR1 parameters
  vector<lower=-1,upper=1>[n_lv_trend] ar1;

  // latent state SD terms
  vector<lower=0>[n_lv_trend] sigma_trend;

  // raw latent states
  matrix[n_trend, n_lv_trend] LV_raw;"),
    block = "parameters"
  )

  # CAR transformed parameters (continuous-time evolution only)
  car_lv_evolution_stanvar <- brms::stanvar(
    name = "car_lv_evolution",
    scode = glue::glue("
  // CAR latent variable evolution
  matrix[n_trend, n_lv_trend] LV;

  // Apply continuous-time AR evolution
  LV = LV_raw .* rep_matrix(sigma_trend', rows(LV_raw));

  for (j in 1 : n_lv_trend) {{
    LV[1, j] += mu_trend[ytimes_trend[1, j]];
    for (i in 2 : n_trend) {{
      LV[i, j] += mu_trend[ytimes_trend[i, j]]
                  + pow(ar1[j], time_dis[i, j])
                    * (LV[i - 1, j] - mu_trend[ytimes_trend[i - 1, j]]);
    }}
  }}"),
    block = "tparameters"
  )

  # Use shared trend computation utility (consistent with all other trends)
  trend_computation <- generate_trend_computation_tparameters(n_lv, n_series)

  # CAR model priors
  car_priors_stanvar <- brms::stanvar(
    name = "car_priors",
    scode = "
  // CAR priors
  ar1 ~ std_normal();
  sigma_trend ~ exponential(3);
  to_vector(LV_raw) ~ std_normal();",
    block = "model"
  )

  # Combine all stanvars including common data variables
  result_stanvars <- combine_stanvars(common_data_stanvars, time_dis_data_stanvar, car_params_stanvar,
                                    car_lv_evolution_stanvar, trend_computation,
                                    car_priors_stanvar)

  return(result_stanvars)
}

#' GP Trend Generator
#'
#' Generates Stan code components for Gaussian process trends.
#'
#' @param trend_specs Trend specification for GP model
#' @param data_info Data information including dimensions
#' @return List of stanvars for GP trend
#' @noRd
# GP trends are handled via trend_formula, not as temporal dynamics
generate_gp_injection_stanvars <- function(trend_specs, data_info) {
  stop(insight::format_error(
    "GP trends are handled via trend_formula, not as temporal dynamics.",
    "Use: trend_formula = ~ gp(time, k = 10)..."
  ))
}

#' ZMVN Trend Generator
#'
#' Generates Stan code components for zero-mean multivariate normal trends.
#'
#' @param trend_specs Trend specification for ZMVN model
#' @param data_info Data information including dimensions
#' @return List of stanvars for ZMVN trend
#' @noRd
generate_zmvn_trend_stanvars <- function(trend_specs, data_info) {
  # Extract key parameters following original ZMVN pattern
  n_lv <- trend_specs$n_lv %||% data_info$n_lv %||% data_info$n_series %||% 1
  n <- data_info$n_obs
  n_series <- data_info$n_series %||% 1
  use_grouping <- !is.null(trend_specs$gr) && trend_specs$gr != 'NA'
  unit_var <- trend_specs$unit %||% "time"

  # Determine if this is a factor model (n_lv < n_series)
  is_factor_model <- !is.null(trend_specs$n_lv) && n_lv < n_series

  # Generate common trend data variables first
  common_data_stanvars <- generate_common_trend_data(n, n_series, n_lv)

  # Generate block-specific injectors for matrix Z
  matrix_z_stanvars <- generate_matrix_z_multiblock_stanvars(is_factor_model, n_lv, n_series)

  # Start with common data stanvars, then add matrix Z stanvars
  result_stanvars <- combine_stanvars(common_data_stanvars, matrix_z_stanvars)

  if (use_grouping) {
    # Hierarchical ZMVN case: ZMVN(unit = site, gr = group, subgr = species)
    n_groups <- data_info$n_groups %||% 1
    n_subgroups <- data_info$n_subgroups %||% n_lv

    # Note: Hierarchical correlation utilities are now handled centrally
    # to avoid stanvar name duplication across trend generators

    # ZMVN-specific parameters for hierarchical case
    zmvn_hierarchical_params_stanvar <- brms::stanvar(
      name = "zmvn_hierarchical_params",
      scode = glue::glue("
        // Latent states for ZMVN
        matrix[n_trend, n_lv_trend] LV;
      "),
      block = "parameters"
    )

    # Hierarchical ZMVN model implementation
    zmvn_hierarchical_model_stanvar <- brms::stanvar(
      name = "zmvn_hierarchical_model",
      scode = glue::glue("
        // Derived hierarchical correlation matrices
        array[{n_groups}] cholesky_factor_corr[{n_subgroups}] L_Omega_group;
        for (g in 1 : {n_groups}) {{
          L_Omega_group[g] = combine_cholesky(L_Omega_global, L_deviation_group[g],
                                              alpha_cor);
        }}

        // ZMVN residual correlation by {unit_var}
        for (i in 1:n_{unit_var}) {{
          for (g in 1:{n_groups}) {{
            to_vector(LV[group_unit_indices[i, g]]) ~ multi_normal_cholesky(rep_vector(0, {n_subgroups}),
                                                                             L_Omega_group[g]);
          }}
        }}
      "),
      block = "model"
    )

    # Note: Hierarchical priors are now handled centrally

    # Combine stanvars for hierarchical case (without hierarchical utilities)
    result_stanvars <- combine_stanvars(result_stanvars, zmvn_hierarchical_params_stanvar,
                                      zmvn_hierarchical_model_stanvar)

  } else {
    # Simple ZMVN case: ZMVN(unit = site, subgr = species)
    zmvn_simple_params_stanvar <- brms::stanvar(
      name = "zmvn_simple_params",
      scode = glue::glue("
        // correlation matrix for ZMVN
        cholesky_factor_corr[n_lv_trend] L_Omega;
        // Latent states for ZMVN
        matrix[n_trend, n_lv_trend] LV;
      "),
      block = "parameters"
    )

    zmvn_simple_model_stanvar <- brms::stanvar(
      name = "zmvn_simple_model",
      scode = glue::glue("
        // Simple ZMVN residual correlation by {unit_var}
        L_Omega ~ lkj_corr_cholesky(2);
        for (i in 1:n_{unit_var}) {{
          to_vector(LV[{unit_var}_indices[i]]) ~ multi_normal_cholesky(rep_vector(0, {n_lv}), L_Omega);
        }}
      "),
      block = "model"
    )

    # Combine stanvars for simple case
    result_stanvars <- combine_stanvars(result_stanvars, zmvn_simple_params_stanvar,
                                      zmvn_simple_model_stanvar)
  }

  # Add trend computation stanvars
  trend_computation <- generate_trend_computation_tparameters(n_lv, n_series)
  result_stanvars <- combine_stanvars(result_stanvars, trend_computation)

  # Add factor model priors if applicable
  if (is_factor_model) {
    factor_priors <- generate_factor_model(is_factor_model, n_lv)
    result_stanvars <- combine_stanvars(result_stanvars, factor_priors)
  }

  return(result_stanvars)
}

#' PW Trend Generator
#'
#' Generates Stan code components for piecewise trends.
#' Supports both linear and logistic piecewise trends with proper Prophet-style implementations.
#'
#' @param trend_specs Trend specification for PW model
#' @param data_info Data information including dimensions
#' @param growth Growth pattern: "linear" or "logistic" (optional, overrides trend_specs$type)
#' @return List of stanvars for PW trend
#' @noRd
generate_pw_trend_stanvars <- function(trend_specs, data_info, growth = NULL) {
  # Extract key parameters
  n_lv <- trend_specs$n_lv %||% 1
  n_changepoints <- trend_specs$n_changepoints %||% 5
  changepoint_scale <- trend_specs$changepoint_scale %||% 0.1
  # Use growth parameter if provided, otherwise fall back to trend_specs$type or trend_specs$growth
  trend_type <- growth %||% trend_specs$type %||% trend_specs$growth %||% "linear"
  n <- data_info$n_obs
  n_series <- data_info$n_series %||% 1

  # Validate trend type
  if (!trend_type %in% c("linear", "logistic")) {
    stop(insight::format_error(
      "Piecewise trend type must be 'linear' or 'logistic'.",
      paste0("Got type = '", trend_type, "'."),
      "Use type = 'linear' or type = 'logistic'."
    ))
  }

  # PW trends do not support factor models (series-specific changepoints required)
  is_factor_model <- FALSE

  # Generate common trend data variables first
  common_data_stanvars <- generate_common_trend_data(n, n_series, n_lv)

  # Generate block-specific injectors for matrix Z
  matrix_z_stanvars <- generate_matrix_z_multiblock_stanvars(is_factor_model, n_lv, n_series)

  # Start components list with common data
  components <- list(common_data_stanvars)
  if (!is.null(matrix_z_stanvars)) {
    components <- append(components, list(matrix_z_stanvars))
  }

  # Functions block - Prophet-style piecewise functions
  pw_functions_stanvar <- brms::stanvar(
    name = "pw_functions",
    scode = "
      matrix get_changepoint_matrix(vector t, vector t_change, int T, int S) {
        /* Function to sort changepoints */

        /* credit goes to the Prophet development team at Meta (https://github.com/facebook/prophet/tree/main)*/
        matrix[T, S] A;
        row_vector[S] a_row;
        int cp_idx;
        A = rep_matrix(0, T, S);
        a_row = rep_row_vector(0, S);
        cp_idx = 1;
        for (i in 1 : T) {
          while ((cp_idx <= S) && (t[i] >= t_change[cp_idx])) {
            a_row[cp_idx] = 1;
            cp_idx = cp_idx + 1;
          }
          A[i] = a_row;
        }
        return A;
      }

      vector logistic_gamma(real k, real m, vector delta, vector t_change, int S) {
        /* Function to compute a logistic trend with changepoints */

        /* credit goes to the Prophet development team at Meta (https://github.com/facebook/prophet/tree/main)*/
        vector[S] gamma; // adjusted offsets, for piecewise continuity
        vector[S + 1] k_s; // actual rate in each segment
        real m_pr;
        k_s = append_row(k, k + cumulative_sum(delta));
        m_pr = m; // The offset in the previous segment
        for (i in 1 : S) {
          gamma[i] = (t_change[i] - m_pr) * (1 - k_s[i] / k_s[i + 1]);
          m_pr = m_pr + gamma[i]; // update for the next segment
        }
        return gamma;
      }

      vector logistic_trend(real k, real m, vector delta, vector t, vector cap,
                            matrix A, vector t_change, int S) {
        /* Function to adjust a logistic trend using a carrying capacity */

        /* credit goes to the Prophet development team at Meta (https://github.com/facebook/prophet/tree/main)*/
        vector[S] gamma;
        gamma = logistic_gamma(k, m, delta, t_change, S);
        return cap .* inv_logit((k + A * delta) .* (t - (m + A * gamma)));
      }

      vector linear_trend(real k, real m, vector delta, vector t, matrix A,
                          vector t_change) {
        /* Function to compute a linear trend with changepoints */

        /* credit goes to the Prophet development team at Meta (https://github.com/facebook/prophet/tree/main)*/
        return (k + A * delta) .* t + (m + A * (-t_change .* delta));
      }
    ",
    block = "functions"
  )

  # Data block - piecewise-specific data components
  pw_data_stanvar <- brms::stanvar(
    x = list(
      n_changepoints = n_changepoints,
      t_change = seq(0.1, 0.9, length.out = n_changepoints),  # Default changepoint times
      changepoint_scale = changepoint_scale
    ),
    name = "pw_data",
    scode = glue::glue("
      int<lower=0> n_changepoints; // number of potential trend changepoints
      vector[n_changepoints] t_change; // times of potential changepoints
      real<lower=0> changepoint_scale; // scale of changepoint shock prior
    "),
    block = "data"
  )

  # Initialize logistic data variable
  pw_logistic_data_stanvar <- NULL

  # Logistic-specific data (carrying capacity)
  if (trend_type == "logistic") {
    pw_logistic_data_stanvar <- brms::stanvar(
      x = matrix(10, nrow = n, ncol = n_series),  # Default carrying capacity
      name = "pw_logistic_data",
      scode = glue::glue("matrix[n_trend, n_series_trend] cap; // carrying capacities"),
      block = "data"
    )
  }

  # Transformed data block - changepoint matrix computation
  pw_transformed_data_stanvar <- brms::stanvar(
    name = "pw_transformed_data",
    scode = glue::glue("
      // sorted changepoint matrix
      matrix[n_trend, n_changepoints] A = get_changepoint_matrix(time, t_change, n_trend, n_changepoints);
    "),
    block = "tdata"
  )

  # Parameters block - piecewise trend parameters
  pw_parameters_stanvar <- brms::stanvar(
    name = "pw_parameters",
    scode = glue::glue("
      // base trend growth rates
      vector[n_series_trend] k_trend;

      // trend offset parameters
      vector[n_series_trend] m_trend;

      // trend rate adjustments per series
      matrix[n_changepoints, n_series_trend] delta_trend;
    "),
    block = "parameters"
  )

  # Transformed parameters block - trend computation (type-specific)
  if (trend_type == "logistic") {
    pw_transformed_parameters_stanvar <- brms::stanvar(
      name = "pw_transformed_parameters",
      scode = glue::glue("
        // raw latent variables (logistic piecewise trends)
        matrix[n_trend, n_series_trend] LV;

        // logistic trend estimates
        for (s in 1 : n_series_trend) {{
          LV[1 : n_trend, s] = logistic_trend(k_trend[s], m_trend[s],
                                        to_vector(delta_trend[ : , s]), time,
                                        to_vector(cap[ : , s]), A, t_change,
                                        n_changepoints);
        }}
      "),
      block = "tparameters"
    )
  } else {
    # Linear type
    pw_transformed_parameters_stanvar <- brms::stanvar(
      name = "pw_transformed_parameters",
      scode = glue::glue("
        // raw latent variables (linear piecewise trends)
        matrix[n_trend, n_series_trend] LV;

        // linear trend estimates
        for (s in 1 : n_series_trend) {{
          LV[1 : n_trend, s] = linear_trend(k_trend[s], m_trend[s],
                                      to_vector(delta_trend[ : , s]), time, A,
                                      t_change);
        }}
      "),
      block = "tparameters"
    )
  }

  # Add trend computation stanvars
  trend_computation <- generate_trend_computation_tparameters(n_lv, n_series)

  # Add model block priors
  pw_model_stanvar <- brms::stanvar(
    name = "pw_model",
    scode = glue::glue("
      // trend parameter priors
      m_trend ~ student_t(3, 0, 2.5);
      k_trend ~ std_normal();
      to_vector(delta_trend) ~ double_exponential(0, changepoint_scale);
    "),
    block = "model"
  )

  # Add all required components to the list
  components <- append(components, list(
    pw_functions_stanvar,
    pw_data_stanvar,
    pw_transformed_data_stanvar,
    pw_parameters_stanvar,
    pw_transformed_parameters_stanvar,
    trend_computation,
    pw_model_stanvar
  ))

  # Add logistic-specific data if needed
  if (!is.null(pw_logistic_data_stanvar)) {
    components <- append(components, list(pw_logistic_data_stanvar))
  }

  # Use robust combination
  return(do.call(combine_stanvars, components))
}

# Note: PWlinear and PWlogistic wrapper functions have been removed.
# Use generate_pw_trend_stanvars(trend_specs, data_info, growth = "linear")
# or generate_pw_trend_stanvars(trend_specs, data_info, growth = "logistic") instead.




# =============================================================================
# SECTION 3: STAN CODE VALIDATION SYSTEM
# =============================================================================
# WHY: Stan code validation prevents runtime compilation failures and ensures
# generated code follows Stan syntax rules. This layer catches errors early
# and provides actionable error messages, critical for user experience and
# debugging the two-stage assembly system.

#' Parse Stan Model Code with rstan
#'
#' @description
#' Validates Stan model code using rstan::stanc.
#' Based on existing mvgam patterns and brms approach.
#'
#' @param model Stan model code
#' @param silent Numeric indicating verbosity level
#' @param ... Additional arguments passed to rstan::stanc
#' @return Validated Stan model code
#' @noRd
parse_model_rstan <- function(model, silent = 1, ...) {
  checkmate::assert_string(model, min.chars = 1)

  # Check if rstan is available
  if (!requireNamespace("rstan", quietly = TRUE)) {
    stop(insight::format_error(
      "Package {.pkg rstan} is required for Stan code validation.",
      "Install rstan or use cmdstanr backend."
    ))
  }

  # Parse Stan model code with rstan using existing eval_silent
  out <- eval_silent(
    rstan::stanc(model_code = model, ...),
    type = "message",
    try = TRUE,
    silent = silent >= 1L
  )

  if (inherits(out, "try-error")) {
    stop(insight::format_error(
      "Stan code validation failed with rstan backend.",
      "Check Stan syntax and model structure.",
      "Error details: {attr(out, 'condition')$message}"
    ))
  }

  # Return validated model code
  if (!is.null(out$model_code)) {
    return(out$model_code)
  } else {
    stop(insight::format_error(
      "Stan code validation did not return model code.",
      "rstan::stanc failed to produce validated output."
    ))
  }
}

#' Parse Stan Model Code with cmdstanr
#'
#' @description
#' Validates Stan model code using cmdstanr::cmdstan_model without compilation.
#' Based on existing mvgam patterns in backends.R.
#'
#' @param model Stan model code
#' @param silent Numeric indicating verbosity level
#' @param ... Additional arguments passed to cmdstanr functions
#' @return Validated Stan model code
#' @noRd
parse_model_cmdstanr <- function(model, silent = 1, ...) {
  checkmate::assert_string(model, min.chars = 1)

  # Check if cmdstanr is available
  if (!requireNamespace("cmdstanr", quietly = TRUE)) {
    stop(insight::format_error(
      "Package {.pkg cmdstanr} is required for Stan code validation.",
      "Install cmdstanr or use rstan backend."
    ))
  }

  # Write Stan model to temporary file - let errors bubble up
  temp_file <- cmdstanr::write_stan_file(model)

  # Validate using cmdstan_model without compilation, using existing eval_silent
  out <- eval_silent(
    cmdstanr::cmdstan_model(temp_file, compile = FALSE, ...),
    type = "message",
    try = TRUE,
    silent = silent > 0L
  )

  if (inherits(out, "try-error")) {
    stop(insight::format_error(
      "Stan code validation failed with cmdstanr backend.",
      "Check Stan syntax and model structure.",
      "Error details: {attr(out, 'condition')$message}"
    ))
  }

  # Check syntax and return code - let errors bubble up
  out$check_syntax(quiet = TRUE)
  return(paste(out$code(), collapse = "\n"))
}

#' Parse Data Declarations from Stan Data Block
#'
#' @description
#' Extracts variable names from Stan data block declarations.
#' This is a simplified parser for basic variable declarations.
#'
#' @param data_block Character string containing Stan data block content
#' @return Character vector of declared variable names
#' @noRd
parse_data_declarations <- function(data_block) {
  checkmate::assert_string(data_block)

  if (nchar(data_block) == 0) {
    return(character(0))
  }

  # Split into lines and clean up
  lines <- strsplit(data_block, "\n")[[1]]
  lines <- trimws(lines)
  lines <- lines[nchar(lines) > 0]  # Remove empty lines
  lines <- lines[!grepl("^//", lines)]  # Remove comment lines

  var_names <- character(0)

  for (line in lines) {
    # Look for variable declarations (simplified pattern)
    # Pattern: type<constraints> variable_name;
    # Examples: "int N;", "vector[N] y;", "real<lower=0> sigma;"

    # Remove inline comments
    line <- sub("//.*$", "", line)
    line <- trimws(line)

    if (nchar(line) == 0) next

    # Basic pattern for variable declarations
    # This is a simplified approach - a full parser would be more robust
    if (grepl(";\\s*$", line)) {  # Line ends with semicolon
      # Extract variable name (last word before semicolon)
      clean_line <- gsub(";\\s*$", "", line)  # Remove semicolon
      tokens <- strsplit(clean_line, "\\s+")[[1]]

      if (length(tokens) >= 2) {
        # Variable name is typically the last token
        var_name <- tokens[length(tokens)]

        # Remove array subscripts if present: variable[N] -> variable
        var_name <- gsub("\\[.*\\]", "", var_name)

        # Remove any remaining special characters
        var_name <- gsub("[^a-zA-Z0-9_]", "", var_name)

        if (nchar(var_name) > 0) {
          var_names <- c(var_names, var_name)
        }
      }
    }
  }

  return(unique(var_names))
}
