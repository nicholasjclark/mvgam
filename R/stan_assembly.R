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

  # Define trend parameter patterns that need response-specific suffixing
  # For multivariate models, only observation mapping arrays should be response-specific
  # Shared trend dynamics parameters (sigma_trend, L_Omega_trend, etc.) should NOT be suffixed
  trend_patterns <- c(
    "obs_trend_time",
    "obs_trend_series", 
    "times_trend",
    "trend",  # The computed trend matrix should be response-specific
    "mu_ones" # GLM compatibility stanvar should be response-specific
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

#' Generate Combined Stan Code for Observation and Trend Models
#'
#' @description
#' Generates complete Stan model code by combining observation model (from brms)
#' with trend component specifications. Uses two-stage assembly: Stage 1 extracts
#' trend stanvars from brms setup, Stage 2 injects trend effects into linear
#' predictors. Supports both univariate and multivariate models with
#' response-specific trend configurations.
#'
#' @param obs_setup Named list containing observation model setup from
#'   setup_brms_lightweight(), must include stancode and standata elements
#' @param trend_setup Named list containing trend model setup, default NULL
#' @param trend_specs List of trend specifications from parse_multivariate_trends(),
#'   default NULL
#' @param prior A brmsprior object containing custom prior specifications for
#'   both observation and trend parameters. If NULL, uses defaults. Default NULL.
#' @param backend Character string specifying Stan backend, either "rstan" or
#'   "cmdstanr". Default "rstan".
#' @param validate Logical indicating whether to validate generated Stan code
#'   structure. Default TRUE.
#' @param silent Numeric controlling message verbosity. 0 = all messages,
#'   1 = important only, 2 = silent. Default 1.
#'
#' @return Named list with elements:
#'   \itemize{
#'     \item stancode: Character string containing complete Stan model code
#'     \item standata: Named list containing all data for Stan model
#'     \item has_trends: Logical indicating if trends were included
#'     \item is_multivariate: Logical indicating if model is multivariate
#'   }
#'
#' The two-stage assembly process:
#' \enumerate{
#'   \item Extract trend stanvars from brms setup using trend specifications
#'   \item Inject trend effects into observation model linear predictors
#' }
#'
#' When trend_setup or trend_specs is NULL, returns observation model without
#' trend components. For multivariate models, processes each response
#' separately and applies response-specific parameter naming.
#'
#' @examples
#' # Setup observation and trend components
#' obs_setup <- setup_brms_lightweight(y ~ x, data = dat)
#' trend_specs <- parse_multivariate_trends(y ~ x, ~ AR(p = 1))
#'
#' # Generate combined Stan code
#' result <- generate_combined_stancode(obs_setup, NULL, trend_specs)
#'
#' # With custom priors
#' priors <- get_prior(y ~ x, trend_formula = ~ AR(p = 1), data = dat)
#' result <- generate_combined_stancode(obs_setup, NULL, trend_specs,
#'                                      prior = priors)
#'
#' @noRd
generate_combined_stancode <- function(obs_setup, trend_setup = NULL,
                                      trend_specs = NULL, prior = NULL,
                                      backend = "rstan", validate = TRUE,
                                      silent = 1) {
  checkmate::assert_list(obs_setup, names = "named")
  checkmate::assert_list(trend_setup, null.ok = TRUE)
  checkmate::assert_list(trend_specs, null.ok = TRUE)
  checkmate::assert_class(prior, "brmsprior", null.ok = TRUE)
  checkmate::assert_choice(backend, c("rstan", "cmdstanr"))
  checkmate::assert_flag(validate)
  checkmate::assert_number(silent)

  # If no trend specification, return observation model as-is
  if (is.null(trend_setup) || is.null(trend_specs)) {
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


  # Stage 1: Extract trend stanvars from brms setup

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
        response_suffix = paste0("_", resp_name),
        response_name = resp_name,
        obs_setup = obs_setup
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
    # Extract actual response name from observation formula
    response_names <- extract_response_names(obs_setup$formula)
    if (length(response_names) == 0) {
      stop(insight::format_error(
        "Could not extract response variable from observation formula.",
        "The formula must have a valid response variable on the left-hand side."
      ), call. = FALSE)
    }
    response_name <- response_names[1]  # Use first response for univariate
    
    trend_stanvars <- extract_trend_stanvars_from_setup(
      trend_setup = trend_setup,
      trend_specs = trend_specs,
      response_suffix = "",
      response_name = response_name,
      obs_setup = obs_setup
    )
    responses_with_trends <- "main"  # Mark univariate model
  }

  # Generate base observation Stan code with trend stanvars
  base_stancode <- generate_base_stancode_with_stanvars(
    obs_setup,
    trend_stanvars,
    backend = backend,
    silent = silent
  )

  # Stage 2: Inject trends using GLM-compatible approach

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

  # Generate complete standata using brms with trend stanvars included
  # This follows the same pattern as assemble_mvgam_stan_data()
  combined_standata <- generate_base_brms_standata(
    formula = obs_setup$formula,
    data = obs_setup$data,
    family = obs_setup$family,
    stanvars = trend_stanvars
  )

  # Validate final Stan code if requested
  if (validate) {
    if (silent < 2) {
      message("Validating combined Stan code...")
    }

    validate_stan_code(
      combined_stancode,
      backend = backend,
      silent = silent
    )

    # Validation function returns TRUE/FALSE, not modified code
    # combined_stancode remains unchanged after validation
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
#' Uses brms::make_stancode internally with injected stanvars.
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


# Note: Legacy merge_stan_data() and combine_stan_data() functions removed.
# Modern system uses brms automatic stanvar data merging via generate_base_brms_standata().

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
#' Integrates with the trend injection system. Optionally creates observation-to-trend
#' mapping arrays to solve the brms obs_ind problem.
#'
#' @param trend_setup List containing trend model setup
#' @param trend_specs List containing trend specification with 'dimensions' field
#' @param response_suffix Response suffix for multivariate models
#' @param obs_data Data frame with observations for creating mappings (optional)
#' @param response_name Name of response variable for mapping generation (optional)
#' @return List of trend stanvars
#'
#' trend_specs must contain a 'dimensions' field calculated by
#' extract_time_series_dimensions() during data validation. This ensures
#' reliable timing information without circular dependencies.
#'
#' @noRd
extract_trend_stanvars_from_setup <- function(trend_setup, trend_specs,
                                              response_suffix = "",
                                              response_name = NULL,
                                              obs_setup = NULL) {
  checkmate::assert_list(trend_setup, names = "named")
  checkmate::assert_list(trend_specs, names = "named")
  checkmate::assert_string(response_suffix)

  # Validate response_name parameter if provided
  if (!is.null(response_name)) {
    checkmate::assert_string(response_name)
  }

  # Extract base stanvars from trend setup
  base_stanvars <- trend_setup$stanvars %||% NULL

  # Generate trend-specific stanvars if trend spec is provided
  trend_type <- trend_specs$trend
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
      series_var = dimensions$series_var,
      unique_times = dimensions$unique_times,
      unique_series = dimensions$unique_series
    )

    # Extract brms parameters (handles mu_trend creation/extraction) + generate trend stanvars
    brms_stanvars <- extract_and_rename_trend_parameters(
      trend_setup = trend_setup,
      dimensions = dimensions,
      suffix = if (response_suffix == "") "_trend" else paste0("_trend", response_suffix)
    )

    # Extract pre-generated observation-to-trend mappings from dimensions
    # Mappings are centralized in extract_time_series_dimensions() for consistency
    mapping_stanvars <- if (!is.null(dimensions$mappings)) {
      # Determine which mapping to use based on response_name
      mapping <- if (!is.null(response_name) && response_name %in% names(dimensions$mappings)) {
        dimensions$mappings[[response_name]]
      } else if (length(dimensions$mappings) == 1) {
        # Single response case - use the only mapping available
        dimensions$mappings[[1]]
      } else {
        NULL
      }

      # Convert mapping to stanvars if we have a valid mapping
      if (!is.null(mapping)) {
        # Validate mapping structure
        checkmate::assert_names(names(mapping), must.include = c("obs_trend_time", "obs_trend_series"))

        # Create stanvars for the mapping arrays with proper naming
        obs_time_stanvar <- brms::stanvar(
          x = mapping$obs_trend_time,
          name = paste0("obs_trend_time", response_suffix),
          scode = paste0("array[N", response_suffix, "] int obs_trend_time", response_suffix, ";"),
          block = "data"
        )

        obs_series_stanvar <- brms::stanvar(
          x = mapping$obs_trend_series,
          name = paste0("obs_trend_series", response_suffix),
          scode = paste0("array[N", response_suffix, "] int obs_trend_series", response_suffix, ";"),
          block = "data"
        )

        # Return as stanvars collection
        c(obs_time_stanvar, obs_series_stanvar)
      } else {
        NULL
      }
    } else {
      NULL
    }

    # Generate trend-specific stanvars (without common components)
    trend_stanvars <- generate_trend_specific_stanvars(trend_specs, data_info, response_suffix)

    # Combine all stanvars: brms parameters + mapping + trend-specific
    combine_stanvars(brms_stanvars, mapping_stanvars, trend_stanvars)
  } else {
    NULL
  }

  # Check for GLM usage in observation model and add mu_ones if needed
  combined_stanvars <- if (is.null(base_stanvars) && is.null(trend_stanvars)) {
    NULL
  } else if (is.null(base_stanvars)) {
    trend_stanvars
  } else if (is.null(trend_stanvars)) {
    base_stanvars
  } else {
    # Use c() to properly combine stanvars objects
    c(base_stanvars, trend_stanvars)
  }
  
  # Add mu_ones stanvar if GLM optimization is detected in observation model
  if (!is.null(obs_setup) && !is.null(obs_setup$stancode)) {
    detected_glm_types <- detect_glm_usage(obs_setup$stancode)
    if (length(detected_glm_types) > 0) {
      # Create mu_ones data stanvar for GLM beta parameter
      mu_ones_stanvar <- stanvar(
        x = 1, 
        name = "mu_ones", 
        scode = "vector[1] mu_ones;"
      )
      
      # Add to combined stanvars
      if (is.null(combined_stanvars)) {
        combined_stanvars <- mu_ones_stanvar
      } else {
        combined_stanvars <- c(combined_stanvars, mu_ones_stanvar)
      }
    }
  }
  
  return(combined_stanvars)
}

#' Detect GLM Function Usage in Stan Code
#'
#' @description
#' Detects which GLM functions from brms are used in Stan code based on the
#' complete list from brms source code.
#'
#' @param stan_code Character string containing Stan code
#' @return Character vector of detected GLM function base names
#' @noRd
detect_glm_usage <- function(stan_code) {
  checkmate::assert_character(stan_code, min.len = 1)
  
  # Complete list from brms source: R/stan-likelihood.R
  glm_patterns <- c(
    "normal_id_glm",
    "poisson_log_glm", 
    "neg_binomial_2_log_glm",
    "bernoulli_logit_glm",
    "ordered_logistic_glm",
    "categorical_logit_glm"
  )
  
  detected <- sapply(glm_patterns, function(pattern) {
    any(grepl(paste0(pattern, "_l(pdf|pmf)\\s*\\("), stan_code))
  })
  
  return(names(detected)[detected])
}

#' Parse GLM Function Parameters
#'
#' @description
#' Extracts parameter names from GLM function calls in Stan code to avoid hardcoding.
#'
#' @param stan_code Character string containing Stan code
#' @param glm_type Character string specifying GLM function base name
#' @return List with extracted parameter names
#' @noRd
parse_glm_parameters <- function(stan_code, glm_type) {
  checkmate::assert_character(stan_code, min.len = 1)
  checkmate::assert_string(glm_type)
  
  # CRITICAL FIX: Split into lines first, then search for GLM call
  code_lines <- strsplit(stan_code, "\n", fixed = TRUE)[[1]]
  
  # Find the line containing the GLM function call - use same pattern as detect_glm_usage
  glm_pattern <- paste0(glm_type, "_l(pdf|pmf)")
  glm_line_idx <- grep(glm_pattern, code_lines)[1]
  
  if (is.na(glm_line_idx)) {
    return(NULL)
  }
  
  glm_line <- code_lines[glm_line_idx]
  
  # Extract the GLM function call from the line - use same pattern as detect_glm_usage
  pattern <- paste0(glm_type, "_l(pdf|pmf)\\([^)]+\\)")
  matches <- regmatches(glm_line, regexpr(pattern, glm_line))
  
  if (length(matches) == 0) {
    return(NULL)
  }
  
  # Extract content between parentheses
  call_content <- matches[1]
  paren_start <- regexpr("\\(", call_content) + 1
  paren_end <- nchar(call_content) - 1
  params_text <- substr(call_content, paren_start, paren_end)
  
  # Split by | to separate Y from predictors
  parts <- strsplit(params_text, "\\|")[[1]]
  if (length(parts) < 2) return(NULL)
  
  y_var <- trimws(parts[1])
  predictor_params <- trimws(strsplit(parts[2], ",")[[1]])
  
  list(
    y_var = y_var,
    design_matrix = if (length(predictor_params) > 0) predictor_params[1] else NULL,
    intercept = if (length(predictor_params) > 1) predictor_params[2] else NULL,
    coefficients = if (length(predictor_params) > 2) predictor_params[3] else NULL,
    other_params = if (length(predictor_params) > 3) predictor_params[4:length(predictor_params)] else NULL
  )
}

#' Transform GLM Call with Combined Linear Predictor
#'
#' @description
#' Transforms GLM function calls to use combined linear predictor with trends.
#'
#' @param stan_code Character string containing original Stan code
#' @param glm_type Character string specifying GLM function base name  
#' @param params List of parsed GLM parameters
#' @return Character string with transformed Stan code
#' @noRd
transform_glm_call <- function(stan_code, glm_type, params) {
  checkmate::assert_character(stan_code, min.len = 1)
  checkmate::assert_string(glm_type)
  checkmate::assert_list(params, names = "named")
  
  # Original pattern to match - use same pattern as detect_glm_usage
  original_pattern <- paste0(glm_type, "_l(pdf|pmf)\\s*\\([^\\)]+\\)")
  
  # Build replacement using GLM structure: glm_function(Y | mu_matrix, 0.0, mu_ones, ...)
  # This preserves GLM optimization while allowing trend injection into mu
  other_params_str <- if (!is.null(params$other_params)) {
    paste0(", ", paste(params$other_params, collapse = ", "))
  } else {
    ""
  }
  
  if (glm_type == "normal_id_glm") {
    # normal_id_glm_lpdf(Y | mu_matrix, alpha_real, beta_vector, sigma)
    replacement <- paste0(glm_type, "_lpdf(", params$y_var, " | to_matrix(mu), 0.0, mu_ones", other_params_str, ")")
    
  } else if (glm_type %in% c("poisson_log_glm", "neg_binomial_2_log_glm", 
                            "bernoulli_logit_glm", "ordered_logistic_glm", "categorical_logit_glm")) {
    # GLM_lpmf(Y | mu_matrix, alpha_real, beta_vector, ...)
    replacement <- paste0(glm_type, "_lpmf(", params$y_var, " | to_matrix(mu), 0.0, mu_ones", other_params_str, ")")
    
  } else {
    insight::format_error("Unsupported GLM type for transformation: {glm_type}")
  }
  
  gsub(original_pattern, replacement, stan_code)
}

#' Inject Trend into GLM Linear Predictor
#'
#' @description
#' GLM-compatible trend injection that creates combined linear predictor
#' and transforms GLM function calls.
#'
#' @param base_stancode Character string containing base Stan code
#' @param trend_stanvars List of stanvars containing trend components
#' @param glm_type Character string specifying detected GLM function type
#' @return Modified Stan code with GLM-compatible trend injection
#' @noRd
inject_trend_into_glm_predictor <- function(base_stancode, trend_stanvars, glm_type) {
  checkmate::assert_string(base_stancode, min.chars = 1)
  checkmate::assert_list(trend_stanvars)
  checkmate::assert_string(glm_type)
  
  # Extract mapping arrays for trend injection
  mapping_arrays <- list(time_arrays = character(0), series_arrays = character(0))
  
  for (stanvar in trend_stanvars) {
    if (is.list(stanvar) && !is.null(stanvar$name)) {
      if (grepl("obs_trend_time", stanvar$name)) {
        mapping_arrays$time_arrays <- c(mapping_arrays$time_arrays, stanvar$name)
      }
      if (grepl("obs_trend_series", stanvar$name)) {
        mapping_arrays$series_arrays <- c(mapping_arrays$series_arrays, stanvar$name)
      }
    }
  }
  
  # Validate mapping arrays exist
  if (length(mapping_arrays$time_arrays) == 0 || length(mapping_arrays$series_arrays) == 0) {
    stop(insight::format_error(
      "Missing observation-to-trend mapping arrays for GLM trend injection.",
      "Expected obs_trend_time and obs_trend_series arrays."
    ), call. = FALSE)
  }
  
  # Parse GLM parameters from existing function calls
  params <- parse_glm_parameters(base_stancode, glm_type)
  if (is.null(params)) {
    stop(insight::format_error(
      "Could not parse GLM parameters from function: {glm_type}",
      "Unable to extract parameter structure for transformation."
    ), call. = FALSE)
  }
  
  # Generate trend injection code (same logic as standard approach)
  trend_injection_code <- generate_trend_injection_code(mapping_arrays)
  
  # Create combined mu with observation + trend components using efficient matrix multiplication
  combined_mu_code <- paste0(
    "  // Efficient matrix multiplication for linear predictor\n",
    "  vector[N] mu = ", params$design_matrix, " * ", params$coefficients, ";\n",
    "  // Add intercept and trend components\n",
    "  for (n in 1:N) {\n",
    "    mu[n] += ", params$intercept, " + trend[obs_trend_time[n], obs_trend_series[n]];\n",
    "  }"
  )
  
  # Create stanvar for combined mu construction in transformed parameters
  mu_stanvar_code <- paste0(combined_mu_code, collapse = "\n")
  
  # Insert combined mu into transformed parameters block
  tparams_pattern <- "(transformed parameters\\s*\\{)(.*?)(\\}\\s*model)"
  if (grepl(tparams_pattern, base_stancode)) {
    # Add to existing transformed parameters
    modified_code <- gsub(
      tparams_pattern,
      paste0("\\1\\2\n", mu_stanvar_code, "\n\\3"),
      base_stancode
    )
  } else {
    # Insert new transformed parameters block
    model_pattern <- "(\\s*)(model\\s*\\{)"
    modified_code <- gsub(
      model_pattern,
      paste0("\\1transformed parameters {\n", mu_stanvar_code, "\n}\n\\1\\2"),
      base_stancode
    )
  }
  
  # Transform GLM function calls to use combined mu
  final_code <- transform_glm_call(modified_code, glm_type, params)
  
  return(final_code)
}

#' Extract Mapping Arrays from Trend Stanvars
#'
#' @description
#' Extracts observation-to-trend mapping arrays from trend stanvars.
#' Common utility used by both standard and GLM trend injection.
#'
#' @param trend_stanvars List of stanvars containing trend components
#' @return List with time_arrays and series_arrays vectors
#' @noRd
extract_mapping_arrays <- function(trend_stanvars) {
  checkmate::assert_list(trend_stanvars, null.ok = TRUE)
  
  mapping_arrays <- list(time_arrays = character(0), series_arrays = character(0))
  
  for (stanvar in trend_stanvars) {
    if (is.list(stanvar) && !is.null(stanvar$name)) {
      if (grepl("obs_trend_time", stanvar$name)) {
        mapping_arrays$time_arrays <- c(mapping_arrays$time_arrays, stanvar$name)
      }
      if (grepl("obs_trend_series", stanvar$name)) {
        mapping_arrays$series_arrays <- c(mapping_arrays$series_arrays, stanvar$name)
      }
    }
  }
  
  return(mapping_arrays)
}

#' Validate Mapping Arrays
#'
#' @description
#' Validates that mapping arrays exist and are properly paired.
#' Common validation used by trend injection functions.
#'
#' @param mapping_arrays List with time_arrays and series_arrays
#' @noRd
validate_mapping_arrays <- function(mapping_arrays) {
  checkmate::assert_list(mapping_arrays, names = "named")
  checkmate::assert_names(names(mapping_arrays), must.include = c("time_arrays", "series_arrays"))
  
  # Validate arrays exist
  if (length(mapping_arrays$time_arrays) == 0 || length(mapping_arrays$series_arrays) == 0) {
    stop(insight::format_error(
      "Missing observation-to-trend mapping arrays in trend_stanvars.",
      "Expected obs_trend_time and obs_trend_series arrays from generate_obs_trend_mapping().",
      "This indicates a problem in the stanvar generation pipeline."
    ), call. = FALSE)
  }
  
  # Validate arrays are paired
  if (length(mapping_arrays$time_arrays) != length(mapping_arrays$series_arrays)) {
    stop(insight::format_error(
      "Mismatched mapping arrays: {length(mapping_arrays$time_arrays)} time arrays but {length(mapping_arrays$series_arrays)} series arrays.",
      "Each obs_trend_time array must have a corresponding obs_trend_series array.",
      "Check the stanvar generation process for consistency."
    ), call. = FALSE)
  }
  
  return(invisible(TRUE))
}

#' Find Stan Block Boundaries
#'
#' @description
#' Finds start and end line indices for Stan code blocks.
#' Handles brace matching to locate block boundaries.
#'
#' @param code_lines Character vector of Stan code lines
#' @param block_name Character string of block name (e.g., "model", "transformed parameters")
#' @return List with start_idx and end_idx, or NULL if block not found
#' @noRd
find_stan_block <- function(code_lines, block_name) {
  checkmate::assert_character(code_lines, min.len = 1)
  checkmate::assert_string(block_name)
  
  # Find block start - properly escape regex
  block_pattern <- paste0("^\\s*", gsub(" ", "\\\\s+", block_name), "\\s*\\{")
  start_idx <- which(grepl(block_pattern, code_lines))
  
  if (length(start_idx) == 0) {
    return(NULL)
  }
  
  start_idx <- start_idx[1]
  
  # Find matching closing brace
  brace_count <- 0
  end_idx <- NULL
  
  for (i in start_idx:length(code_lines)) {
    line <- code_lines[i]
    # Count braces using base R - use literal braces with fixed = TRUE
    open_matches <- gregexpr("{", line, fixed = TRUE)[[1]]
    close_matches <- gregexpr("}", line, fixed = TRUE)[[1]]
    
    # Count actual matches (gregexpr returns -1 when no matches)
    open_braces <- if (open_matches[1] == -1) 0 else length(open_matches)
    close_braces <- if (close_matches[1] == -1) 0 else length(close_matches)
    
    brace_count <- brace_count + open_braces - close_braces
    
    
    # Only check for closing after we've processed the opening line
    if (i > start_idx && brace_count == 0) {
      end_idx <- i
      break
    }
  }
  
  if (is.null(end_idx)) {
    stop(insight::format_error(
      "Cannot find end of {block_name} block.",
      "Stan code structure is invalid or malformed."
    ), call. = FALSE)
  }
  
  return(list(start_idx = start_idx, end_idx = end_idx))
}

#' Insert Code into Stan Block
#'
#' @description
#' Inserts code into existing Stan block. Since brms always generates all
#' Stan blocks (even if empty), this function never creates new blocks.
#' Common utility for Stan code modification.
#'
#' @param code_lines Character vector of Stan code lines
#' @param block_name Character string of block name
#' @param insertion_code Character vector of code to insert
#' @return Modified character vector of Stan code lines
#' @noRd
insert_into_stan_block <- function(code_lines, block_name, insertion_code) {
  checkmate::assert_character(code_lines, min.len = 1)
  checkmate::assert_string(block_name)
  checkmate::assert_character(insertion_code, min.len = 1)
  
  block_info <- find_stan_block(code_lines, block_name)
  
  if (is.null(block_info)) {
    # Debug: show what blocks we can find
    block_pattern <- paste0("^\\s*", gsub(" ", "\\\\s+", block_name), "\\s*\\{")
    matching_lines <- which(grepl(block_pattern, code_lines))
    debug_info <- if (length(matching_lines) > 0) {
      paste("Found potential matches at lines:", paste(matching_lines, collapse = ", "),
            "Patterns:", paste(code_lines[matching_lines], collapse = " | "))
    } else {
      "No matches found"
    }
    
    stop(insight::format_error(
      paste0("Cannot find '", block_name, "' block in Stan code."),
      paste0("Debug info: ", debug_info),
      paste0("Pattern used: ", block_pattern),
      "This is unexpected as brms should generate all blocks.",
      "Please check that the Stan code is properly formed."
    ), call. = FALSE)
  }
  
  # Check if block is empty (only has opening and closing braces on same or consecutive lines)
  is_empty_block <- (block_info$end_idx - block_info$start_idx) <= 1
  
  
  if (is_empty_block) {
    # For empty blocks, insert between the braces
    # Handle both "block { }" and "block {\n}" formats
    if (block_info$end_idx == block_info$start_idx) {
      # Same line: "block { }"
      # Replace the line with expanded block
      modified_lines <- c(
        if (block_info$start_idx > 1) code_lines[1:(block_info$start_idx - 1)] else character(0),
        paste0(block_name, " {"),
        insertion_code,
        "}",
        if (block_info$end_idx < length(code_lines)) code_lines[(block_info$end_idx + 1):length(code_lines)] else character(0)
      )
    } else {
      # Consecutive lines: "block {\n}"
      modified_lines <- c(
        code_lines[1:block_info$start_idx],
        insertion_code,
        code_lines[block_info$end_idx:length(code_lines)]
      )
    }
  } else {
    # Non-empty block: insert before closing brace (at the END of the block)
    # This ensures trend injection happens after all necessary variables are declared
    modified_lines <- c(
      code_lines[1:(block_info$end_idx - 1)],
      "",  # Add blank line before injection  
      insertion_code,
      code_lines[block_info$end_idx:length(code_lines)]
    )
  }
  
  return(modified_lines)
}

#' Inject Trend into Linear Predictor
#'
#' @description
#' Injects trend effects into Stan linear predictor. Uses GLM-compatible approach
#' when brms GLM optimization is detected, fallback to standard approach otherwise.
#'
#' @param base_stancode Character string of base Stan code
#' @param trend_stanvars List of trend stanvars
#' @return Modified Stan code with trend injection
#' @noRd
inject_trend_into_linear_predictor <- function(base_stancode, trend_stanvars) {
  # Input validation
  checkmate::assert_string(base_stancode, min.chars = 1)
  checkmate::assert_list(trend_stanvars, null.ok = TRUE)

  # Early return if no trends to inject
  if (is.null(trend_stanvars) || length(trend_stanvars) == 0) {
    return(base_stancode)
  }

  # Detect GLM usage using existing detection system
  detected_glm_types <- detect_glm_usage(base_stancode)
  
  if (length(detected_glm_types) > 0) {
    # Use GLM-compatible approach when GLM optimization is detected
    glm_type <- detected_glm_types[1]  # Use first detected GLM type
    return(inject_trend_into_glm_predictor(base_stancode, trend_stanvars, glm_type))
  } else {
    # Use standard approach when no GLM optimization
    # Extract and validate mapping arrays using shared utility
    mapping_arrays <- extract_mapping_arrays(trend_stanvars)
    validate_mapping_arrays(mapping_arrays)

    # Generate trend injection code
    trend_injection_code <- generate_trend_injection_code(mapping_arrays)
    
    # Parse Stan code into lines
    code_lines <- strsplit(base_stancode, "\n", fixed = TRUE)[[1]]
    
    # Insert trend injection into model block after last mu += line
    modified_lines <- insert_after_mu_lines_in_model_block(code_lines, trend_injection_code)
    
    # Reconstruct Stan code
    return(paste(modified_lines, collapse = "\n"))
  }
}

#' Insert Trend Injection Code After Last mu += Line in Model Block
#'
#' @description
#' Uses existing block detection infrastructure to insert trend injection code
#' after the last mu += line in the model block.
#'
#' @param code_lines Character vector of Stan code lines
#' @param trend_injection_code Character vector of trend injection code lines
#' @return Modified character vector with trend injection inserted
#' @noRd
insert_after_mu_lines_in_model_block <- function(code_lines, trend_injection_code) {
  checkmate::assert_character(code_lines)
  checkmate::assert_character(trend_injection_code)
  
  # Use existing infrastructure to find model block
  block_info <- find_stan_block(code_lines, "model")
  if (is.null(block_info)) {
    insight::format_error("Model block not found in Stan code")
  }
  
  # Find last mu += line within the model block
  model_lines <- code_lines[block_info$start_idx:block_info$end_idx]
  mu_line_indices <- which(grepl("\\s*mu\\s*\\+=", model_lines))
  
  if (length(mu_line_indices) == 0) {
    insight::format_error("No mu += lines found in model block")
  }
  
  # Calculate absolute position of last mu += line
  last_mu_pos <- block_info$start_idx + mu_line_indices[length(mu_line_indices)] - 1
  
  # Insert trend injection after last mu += line with proper indentation
  indented_injection <- paste0("    ", trend_injection_code)
  
  result_lines <- c(
    code_lines[1:last_mu_pos],
    indented_injection,
    code_lines[(last_mu_pos + 1):length(code_lines)]
  )
  
  return(result_lines)
}

#' Generate Trend Injection Code for Stan Transformed Parameters Block
#'
#' @description
#' Creates Stan code lines that inject trend effects into the linear predictor
#' mu by adding trend matrix values using observation-to-trend mapping arrays.
#' Supports both univariate and multivariate responses with appropriate
#' variable naming.
#'
#' @param mapping_arrays Named list containing mapping arrays with elements:
#'   \itemize{
#'     \item time_arrays: Character vector of time mapping array names
#'     \item series_arrays: Character vector of series mapping array names  
#'   }
#'   Arrays must be paired (same length) with corresponding time/series mappings.
#'
#' @return Character vector containing Stan code lines for trend injection.
#'   Generated code follows the pattern:
#'   \preformatted{
#'   for (n in 1:N) {
#'     mu[n] += trend[obs_trend_time[n], obs_trend_series[n]];
#'   }
#'   }
#'   For multivariate responses, generates response-specific variable names
#'   (mu_count, trend_count, N_count, etc.).
#'
#' @details
#' This function is called internally by \code{inject_trend_into_linear_predictor()}
#' to generate the actual Stan code that adds trend effects to brms linear
#' predictors. The mapping arrays must have been created by 
#' \code{generate_obs_trend_mapping()} and included in trend stanvars.
#'
#' The function handles both univariate and multivariate cases by detecting
#' response suffixes in mapping array names (e.g., "_y1", "_count") and
#' generating appropriately named variables.
#'
#' @examples
#' \dontrun{
#' # Univariate case
#' mapping_arrays <- list(
#'   time_arrays = "obs_trend_time",
#'   series_arrays = "obs_trend_series"  
#' )
#' code_lines <- generate_trend_injection_code(mapping_arrays)
#'
#' # Multivariate case  
#' mapping_arrays <- list(
#'   time_arrays = c("obs_trend_time_count", "obs_trend_time_biomass"),
#'   series_arrays = c("obs_trend_series_count", "obs_trend_series_biomass")
#' )
#' code_lines <- generate_trend_injection_code(mapping_arrays)
#' }
#'
#' @noRd
generate_trend_injection_code <- function(mapping_arrays) {
  checkmate::assert_list(mapping_arrays)
  checkmate::assert_names(names(mapping_arrays), must.include = c("time_arrays", "series_arrays"))

  injection_lines <- c("", "  // Add trend effects using mapping arrays")

  # Generate for each response (handles both univariate and multivariate)
  for (i in seq_along(mapping_arrays$time_arrays)) {
    time_array <- mapping_arrays$time_arrays[i]
    series_array <- mapping_arrays$series_arrays[i]

    # Extract response suffix using base R
    response_suffix <- ""
    match_result <- regexpr("_y\\d+$", time_array)
    if (match_result != -1) {
      response_suffix <- regmatches(time_array, match_result)
    }

    # Generate variable names based on suffix
    mu_var <- if (response_suffix == "") "mu" else paste0("mu", response_suffix)
    trend_var <- if (response_suffix == "") "trend" else paste0("trend", response_suffix)
    n_var <- if (response_suffix == "") "N" else paste0("N", response_suffix)

    # Generate direct mu modification loop
    injection_lines <- c(injection_lines,
                         paste0("  for (n in 1:", n_var, ") {"),
                         paste0("    ", mu_var, "[n] += ", trend_var, "[", time_array, "[n], ", series_array, "[n]];"),
                         "  }"
    )
  }

  return(injection_lines)
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
      validate_stan_code(base_stancode, backend = backend)
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
    validate_stan_code(final_stancode, backend = backend)
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
append_if_not_null <- function(components, new_component) {
  checkmate::assert_list(components)
  if (!is.null(new_component)) {
    append(components, list(new_component))
  } else {
    components
  }
}

#' Robust stanvar combination for mvgam
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
  innovations_trend_stanvar <- brms::stanvar(
    name = "innovations_trend",
    scode = paste0("matrix[n_trend, ", effective_dim, "] innovations_trend;"),
    block = "parameters"
  )
  stanvar_components <- append(stanvar_components, list(innovations_trend_stanvar))

  # 5. Final innovations in transformed parameters (after correlation/MA transformation)
  if (is_hierarchical) {
    # Hierarchical case: innovations depend on group structure
    final_innovations_code <- paste0("
    // Scaled innovations after applying hierarchical correlations
    matrix[n_trend, ", effective_dim, "] scaled_innovations_trend;

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
    // Scaled innovations after applying correlations
    matrix[n_trend, ", effective_dim, "] scaled_innovations_trend;

    // Apply correlation transformation using efficient non-centered parameterization
    {
      matrix[", effective_dim, ", ", effective_dim, "] L_Sigma = diag_pre_multiply(sigma_trend, L_Omega_trend);
      scaled_innovations_trend = innovations_trend * L_Sigma';
    }")
  } else {
    # Uncorrelated case
    final_innovations_code <- paste0("
    // Scaled innovations (uncorrelated case)
    matrix[n_trend, ", effective_dim, "] scaled_innovations_trend;

    // Apply scaling using vectorized operations
    scaled_innovations_trend = innovations_trend * diag_matrix(sigma_trend);")
  }

  scaled_innovations_stanvar <- brms::stanvar(
    name = "scaled_innovations_trend",
    scode = final_innovations_code,
    block = "tparameters"
  )
  stanvar_components <- append(stanvar_components, list(scaled_innovations_stanvar))

  # Combine all components using do.call to handle the list properly
  return(do.call(combine_stanvars, stanvar_components))
}

#' Generate Standard Priors for Gaussian Innovations
#'
#' Creates standard priors for shared Gaussian innovation parameters.
#'
#'
#' @param effective_dim Effective dimension (n_lv for factor models, n_series otherwise)
#' @param cor Logical, whether correlation parameters exist
#' @param is_hierarchical Logical, whether using hierarchical structure
#' @return Stanvar object with prior code
#' @noRd
generate_innovation_model <- function(effective_dim, cor = FALSE, is_hierarchical = FALSE, prior = NULL) {

  if (is_hierarchical) {
    # Hierarchical priors are handled by generate_hierarchical_correlation_model
    prior_code <- c(
      "// Raw innovations prior",
      "to_vector(innovations_trend) ~ std_normal();"
    )
  } else {
    # Simple case priors - use centralized prior system
    sigma_prior_str <- get_trend_parameter_prior(prior, "sigma_trend")

    prior_code <- c("// Shared Gaussian innovation priors")

    # Add sigma_trend prior if specified
    if (sigma_prior_str != "") {
      prior_code <- c(prior_code, glue::glue("sigma_trend ~ {sigma_prior_str};"))
    }

    if (cor && effective_dim > 1) {
      prior_code <- c(prior_code,
        "L_Omega_trend ~ lkj_corr_cholesky(2);"
      )
    }

    prior_code <- c(prior_code,
      "to_vector(innovations_trend) ~ std_normal();"
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

#' Generate All Trend Dimension Stanvars - SINGLE SOURCE OF TRUTH
#'
#' Creates ALL standardized dimension data block stanvars needed by ALL trend types.
#' This function consolidates dimension creation that was previously duplicated
#' across multiple functions, ensuring consistent dimension parameter generation
#' while maintaining proper dimensional relationships for factor vs non-factor models.
#'
#' @param n_obs Number of observations (will be named n_trend in Stan)
#' @param n_series Number of observed series
#' @param n_lv Number of latent variables (optional, determines factor model behavior)
#' @param is_factor_model Logical indicating if this is a factor model (optional, inferred from n_lv)
#' @return List of all dimension data block stanvars (n_trend, n_series_trend, n_lv_trend)
#' @noRd
generate_common_trend_data <- function(n_obs, n_series, n_lv = NULL, is_factor_model = NULL) {
  # Input validation
  checkmate::assert_number(n_obs, lower = 1)
  checkmate::assert_number(n_series, lower = 1)
  checkmate::assert_number(n_lv, lower = 1, null.ok = TRUE)
  checkmate::assert_logical(is_factor_model, len = 1, null.ok = TRUE)

  # Infer factor model behavior if not specified
  if (is.null(is_factor_model)) {
    is_factor_model <- !is.null(n_lv) && n_lv < n_series
  }

  # Set n_lv default for non-factor models
  if (is.null(n_lv)) {
    n_lv <- n_series  # Non-factor model: n_lv_trend = n_series_trend
  }

  # Create n_trend stanvar - always needed
  n_trend_stanvar <- brms::stanvar(
    x = n_obs,
    name = "n_trend",
    scode = "int<lower=1> n_trend;",
    block = "data"
  )

  # Create n_series_trend stanvar - always needed
  n_series_trend_stanvar <- brms::stanvar(
    x = n_series,
    name = "n_series_trend",
    scode = "int<lower=1> n_series_trend;",
    block = "data"
  )

  # Create n_lv_trend stanvar - always needed for consistent parameter indexing
  if (is_factor_model) {
    # Factor model: n_lv_trend < n_series_trend (true latent factors)
    n_lv_trend_stanvar <- brms::stanvar(
      x = n_lv,
      name = "n_lv_trend",
      scode = "int<lower=1> n_lv_trend;",
      block = "data"
    )
  } else {
    # Non-factor model: n_lv_trend = n_series_trend for consistent indexing
    n_lv_trend_stanvar <- brms::stanvar(
      x = n_series,  # Use n_series for consistency, not n_lv
      name = "n_lv_trend",
      scode = "int<lower=1> n_lv_trend;",
      block = "data"
    )
  }

  return(combine_stanvars(n_trend_stanvar, n_series_trend_stanvar, n_lv_trend_stanvar))
}

# Function removed - dimension stanvars now handled by generate_common_trend_data()
# This eliminates "Duplicated names in 'stanvars'" bug

#' Generate Parameter Block Injections for Matrix Z
#'
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
#'
#' @param is_factor_model Logical indicating if this is a factor model
#' @param n_lv Number of latent variables
#' @param n_series Number of observed series
#' @return List of stanvars for matrix Z across all required blocks
#' @noRd
generate_matrix_z_multiblock_stanvars <- function(is_factor_model, n_lv, n_series) {
  # Validate inputs following CLAUDE.md standards
  checkmate::assert_logical(is_factor_model, len = 1)
  checkmate::assert_integerish(n_lv, len = 1, lower = 1)
  checkmate::assert_integerish(n_series, len = 1, lower = 1)

  # Get Z matrix components (dimensions handled by generate_common_trend_data)
  stanvars_list <- list(
    generate_matrix_z_parameters(is_factor_model, n_lv, n_series),
    generate_matrix_z_tdata(is_factor_model, n_lv, n_series)
  )

  # Remove NULL elements and combine
  non_null_stanvars <- Filter(Negate(is.null), stanvars_list)

  if (length(non_null_stanvars) == 0) {
    return(NULL)
  } else if (length(non_null_stanvars) == 1) {
    return(non_null_stanvars[[1]])
  } else {
    return(do.call(c, non_null_stanvars))
  }
}

#' Generate Factor Model Block Code
#'
#' Provides standardized priors for factor models with fixed variance=1 constraint.
#' Only generates priors when is_factor_model=TRUE.
#'
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
      for (i in 1:n_trend) {{
        for (s in 1:n_series_trend) {{
          trend[i, s] = dot_product(Z[s, :], lv_trend[i, :]) + mu_trend[times_trend[i, s]];
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
generate_trend_specific_stanvars <- function(trend_specs, data_info, response_suffix = "", prior = NULL) {
  # Validate inputs
  checkmate::assert_list(trend_specs)
  checkmate::assert_list(data_info)
  checkmate::assert_string(response_suffix)
  checkmate::assert_class(prior, "brmsprior", null.ok = TRUE)

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
      is_hierarchical = is_hierarchical,
      prior = prior
    )

    # Combine shared stanvars with priors properly using combine_stanvars
    shared_stanvars <- combine_stanvars(shared_stanvars, shared_priors)

    # Apply response suffix post-processing to shared stanvars for multivariate models
    if (response_suffix != "") {
      # Apply suffix to the entire stanvars collection
      shared_stanvars <- apply_response_suffix_to_stanvars(shared_stanvars, response_suffix)
    }
  }

  # Extract and validate parameters (single source of truth)
  n_obs <- data_info$n_obs
  n_series <- data_info$n_series %||% 1
  n_lv <- trend_specs$n_lv %||% n_series
  is_factor_model <- !is.null(trend_specs$n_lv) && n_lv < n_series
  use_grouping <- !is.null(trend_specs$gr) && trend_specs$gr != 'NA'

  # Cross-cutting system validation (integration between factor models and hierarchical correlations)
  validate_no_factor_hierarchical(trend_specs, n_series, trend_type)

  # Parameter validation
  checkmate::assert_int(n_obs, lower = 1)
  checkmate::assert_int(n_series, lower = 1)
  checkmate::assert_int(n_lv, lower = 1)

  # Generate common dimension stanvars (needed by ALL trend types)
  common_dimensions <- generate_common_trend_data(n_obs, n_series, n_lv, is_factor_model)

  # Generate trend-specific stanvars using consistent naming convention
  trend_stanvars <- generator_function(trend_specs, data_info, prior = prior)

  # Apply response suffix post-processing for multivariate models
  if (response_suffix != "") {
    trend_stanvars <- apply_response_suffix_to_stanvars(trend_stanvars, response_suffix)
  }

  # Combine all components: common dimensions + shared innovations + trend-specific
  all_components <- list(common_dimensions, shared_stanvars, trend_stanvars)
  all_components <- all_components[!sapply(all_components, is.null)]
  
  if (length(all_components) == 1) {
    return(all_components[[1]])
  } else {
    return(do.call(combine_stanvars, all_components))
  }
}


#' Random Walk Trend Generator
#'
#' @description
#' Generates Stan code components for random walk trends including latent
#' variables, innovation dynamics, factor model structures, and prior
#' specifications. Supports both univariate and multivariate models with
#' optional factor model dimension reduction.
#'
#' @param trend_specs Trend specification for RW model containing parameters
#'   like n_lv (number of latent variables) and ma (moving average flag)
#' @param data_info Data information including dimensions (n_series, n_time)
#'   and other model structure details
#' @param prior A brmsprior object containing custom prior specifications for
#'   trend parameters. If NULL, uses defaults from trend registry. Default NULL.
#'
#' @return Combined stanvars object containing Stan code for:
#'   \itemize{
#'     \item Matrix Z for factor models (if applicable)
#'     \item MA parameters (if specified)
#'     \item RW innovation dynamics
#'     \item Trend computation in transformed parameters
#'     \item Factor model priors (for dimension-reduced models)
#'   }
#'
#' The function generates different components based on model structure:
#' \enumerate{
#'   \item Factor models (n_lv < n_series): includes Z matrix and loading priors
#'   \item MA components: added if trend_specs$ma is TRUE
#'   \item Innovation priors: sigma_trend parameters with custom or default priors
#'   \item Computation: transformed parameters for trend extraction
#' }
#'
#' @examples
#' # Basic RW trend
#' trend_specs <- list(n_lv = 1, ma = FALSE)
#' data_info <- list(n_series = 3, n_time = 100)
#' stanvars <- generate_rw_trend_stanvars(trend_specs, data_info)
#'
#' # Factor model RW with custom priors
#' trend_specs <- list(n_lv = 2, ma = FALSE)
#' priors <- get_prior(y ~ x, trend_formula = ~ RW(), data = dat)
#' stanvars <- generate_rw_trend_stanvars(trend_specs, data_info, prior = priors)
#'
#' @noRd
generate_rw_trend_stanvars <- function(trend_specs, data_info, prior = NULL) {
  # Input validation
  checkmate::assert_list(trend_specs, names = "named")
  checkmate::assert_list(data_info, names = "named")
  checkmate::assert_class(prior, "brmsprior", null.ok = TRUE)

  # Extract dimensions and configuration
  n_lv <- trend_specs$n_lv %||% 1
  n_series <- data_info$n_series %||% 1
  n_obs <- data_info$n_obs

  # Validate dimensions
  checkmate::assert_int(n_lv, lower = 1)
  checkmate::assert_int(n_series, lower = 1)
  checkmate::assert_int(n_obs, lower = 1)

  is_factor_model <- !is.null(trend_specs$n_lv) && n_lv < n_series
  has_ma <- trend_specs$ma %||% FALSE

  # Cross-cutting validation handled by injection function

  # Build components list following the 3-stanvar pattern
  components <- list()

  # STEP 1: Dimensions handled by calling context (no duplication)

  # STEP 2: Always add matrix Z (factor=parameters, non-factor=diagonal in tdata)
  matrix_z <- generate_matrix_z_multiblock_stanvars(is_factor_model, n_lv, n_series)
  components <- append_if_not_null(components, matrix_z)

  # 1. PARAMETERS block - RW trend-specific parameters
  if (has_ma) {
    rw_parameters_stanvar <- brms::stanvar(
      name = "rw_parameters",
      scode = "vector<lower=-1,upper=1>[n_lv_trend] theta1_trend;",
      block = "parameters"
    )
    components <- append(components, list(rw_parameters_stanvar))
  }

  # 2. TPARAMETERS block - RW dynamics computation (always needed)
  rw_tparameters_stanvar <- brms::stanvar(
    name = "rw_tparameters",
    scode = glue::glue("
      // Latent states with RW dynamics
      matrix[n_trend, n_lv_trend] lv_trend;
      {if(has_ma) 'matrix[n_trend, n_lv_trend] ma_innovations_trend = scaled_innovations_trend;' else ''}

      {if(has_ma) '// Apply MA(1) transformation
      for (i in 2:n_trend) {{
        for (j in 1:n_lv_trend) {{
          ma_innovations_trend[i, j] += theta1_trend[j] * ma_innovations_trend[i-1, j];
        }}
      }}' else ''}

      // Apply RW dynamics
      lv_trend[1, :] = {if(has_ma) 'ma_innovations_trend' else 'scaled_innovations_trend'}[1, :];
      for (i in 2:n_trend) {{
        lv_trend[i, :] = lv_trend[i-1, :] + {if(has_ma) 'ma_innovations_trend' else 'scaled_innovations_trend'}[i, :];
      }}
    "),
    block = "tparameters"
  )
  components <- append(components, list(rw_tparameters_stanvar))

  # 3. MODEL block - RW priors (MA and possibly sigma_trend)
  # Build list of parameters that need priors
  rw_params_to_prior <- character(0)
  if (has_ma) {
    rw_params_to_prior <- c(rw_params_to_prior, "theta1_trend")
  }
  # Note: sigma_trend priors are handled by the shared innovation system
  # The trend generator shouldn't duplicate them

  if (length(rw_params_to_prior) > 0) {
    rw_model_stanvar <- generate_trend_priors_stanvar(
      param_names = rw_params_to_prior,
      prior = prior,
      stanvar_name = "rw_model"
    )
    components <- append_if_not_null(components, rw_model_stanvar)
  }

  # 4. Add trend computation (maps lv_trend through Z if needed)
  trend_computation <- generate_trend_computation_tparameters(n_lv, n_series)
  components <- append_if_not_null(components, trend_computation)

  # 5. Factor model priors if applicable
  if (is_factor_model) {
    factor_priors <- generate_factor_model(is_factor_model, n_lv)
    components <- append_if_not_null(components, factor_priors)
  }

  # Use the robust combine_stanvars function
  return(do.call(combine_stanvars, components))
}

#' Generate Trend Prior Stanvar from Parameter Names
#'
#' Creates a stanvar with prior statements for specified trend parameters.
#' Uses centralized prior system with get_trend_parameter_prior().
#'
#' @param param_names Character vector of parameter names (e.g., c("ar1_trend", "sigma_trend"))
#' @param prior brmsprior object or NULL
#' @param stanvar_name Name for the stanvar object
#' @return stanvar object with priors, or NULL if no priors specified
#' @noRd
generate_trend_priors_stanvar <- function(param_names, prior = NULL, stanvar_name = "trend_priors") {
  # Input validation
  checkmate::assert_character(param_names, min.len = 1, any.missing = FALSE)
  checkmate::assert_class(prior, "brmsprior", null.ok = TRUE)
  checkmate::assert_string(stanvar_name, min.chars = 1)

  # Extract prior strings for each parameter
  prior_lines <- sapply(param_names, function(param_name) {
    prior_str <- get_trend_parameter_prior(prior, param_name)
    if (prior_str != "") {
      glue::glue("{param_name} ~ {prior_str};")
    } else {
      ""
    }
  }, USE.NAMES = FALSE)

  # Filter out empty lines
  prior_lines <- prior_lines[prior_lines != ""]

  # Return stanvar only if there are priors to include
  if (length(prior_lines) > 0) {
    # Return the stanvars collection directly - combine_stanvars will handle it
    return(brms::stanvar(
      x = NULL,
      name = stanvar_name,
      scode = paste0(prior_lines, collapse = "\n"),
      block = "model"
    ))
  } else {
    return(NULL)
  }
}


#' AR Trend Generator
#'
#' @description
#' Generates Stan code components for autoregressive trends with support for
#' factor models, hierarchical correlations, and custom lag structures.
#' Uses consistent non-centered parameterization and proper ar{lag}_trend
#' parameter naming convention for seamless prior integration.
#'
#' @param trend_specs Trend specification for AR model containing parameters
#'   like lags (AR order), n_lv (latent variables), gr (grouping), ma (moving average)
#' @param data_info Data information including dimensions (n_obs, n_series, n_time)
#'   and other model structure details
#' @param prior A brmsprior object containing custom prior specifications for
#'   AR trend parameters (ar1_trend, ar2_trend, sigma_trend, etc.). If NULL,
#'   uses defaults from trend registry. Default NULL.
#'
#' @return Combined stanvars object containing Stan code for:
#'   \itemize{
#'     \item Common trend data (dimensions and indices)
#'     \item Matrix Z for factor models (if n_lv < n_series)
#'     \item AR coefficient parameters with lag-specific naming
#'     \item Innovation dynamics and variance parameters
#'     \item MA components (if specified)
#'     \item Hierarchical correlation structures (if grouping specified)
#'   }
#'
#' The function supports various AR model configurations:
#' \enumerate{
#'   \item Standard AR(p): ar1_trend, ar2_trend, ..., ar{p}_trend coefficients
#'   \item Factor models: dimension reduction with Z matrix when n_lv < n_series
#'   \item Hierarchical models: group-specific parameters when gr specified
#'   \item Non-standard lags: supports arbitrary lag structures (e.g., c(1,12,24))
#'   \item MA components: additional moving average terms if ma = TRUE
#' }
#'
#' Prior specifications use the _trend suffix convention for parameter matching.
#'
#' @examples
#' # Standard AR(2) model
#' trend_specs <- list(lags = 2, n_lv = 1)
#' data_info <- list(n_obs = 100, n_series = 3, n_time = 100)
#' stanvars <- generate_ar_trend_stanvars(trend_specs, data_info)
#'
#' # Factor model AR with custom priors
#' trend_specs <- list(lags = 1, n_lv = 2, ma = FALSE)
#' priors <- get_prior(y ~ x, trend_formula = ~ AR(p = 1), data = dat)
#' stanvars <- generate_ar_trend_stanvars(trend_specs, data_info, prior = priors)
#'
#' # Seasonal AR with non-standard lags
#' trend_specs <- list(ar_lags = c(1, 12), n_lv = 1)
#' stanvars <- generate_ar_trend_stanvars(trend_specs, data_info, prior = priors)
#'
#' @noRd
generate_ar_trend_stanvars <- function(trend_specs, data_info, prior = NULL) {
  # Input validation
  checkmate::assert_list(trend_specs, names = "named")
  checkmate::assert_list(data_info, names = "named")
  checkmate::assert_class(prior, "brmsprior", null.ok = TRUE)

  # Extract dimensions and configuration
  n_lv <- trend_specs$n_lv %||% 1
  lags <- trend_specs$lags %||% 1
  n_series <- data_info$n_series %||% 1
  n_obs <- data_info$n_obs

  # Validate dimensions
  checkmate::assert_int(n_lv, lower = 1)
  checkmate::assert_int(n_series, lower = 1)
  checkmate::assert_int(lags, lower = 1)
  checkmate::assert_int(n_obs, lower = 1)

  # Convert lags to ar_lags if needed
  ar_lags <- if (!is.null(trend_specs$ar_lags)) trend_specs$ar_lags else (1:lags)
  checkmate::assert_integerish(ar_lags, lower = 1, any.missing = FALSE)

  is_factor_model <- !is.null(trend_specs$n_lv) && n_lv < n_series
  has_ma <- trend_specs$ma %||% FALSE
  max_lag <- max(ar_lags)

  # Cross-cutting validation handled by injection function

  # Build components list following the 3-stanvar pattern
  components <- list()

  # STEP 1: Dimensions handled by calling context (no duplication)

  # STEP 2: Always add matrix Z (factor=parameters, non-factor=diagonal in tdata)
  matrix_z <- generate_matrix_z_multiblock_stanvars(is_factor_model, n_lv, n_series)
  components <- append_if_not_null(components, matrix_z)

  # 1. PARAMETERS block - AR trend-specific parameters
  ar_param_declarations <- sapply(ar_lags, function(lag) {
    glue::glue("vector<lower=-1,upper=1>[n_lv_trend] ar{lag}_trend;")
  })

  ar_parameters_stanvar <- brms::stanvar(
    name = "ar_parameters",
    scode = paste0(
      "// AR coefficient parameters\n",
      paste(ar_param_declarations, collapse = "\n")
    ),
    block = "parameters"
  )
  components <- append(components, list(ar_parameters_stanvar))

  # Add MA parameters if needed
  if (has_ma) {
    ma_parameters_stanvar <- brms::stanvar(
      name = "ar_ma_parameters",
      scode = "vector<lower=-1,upper=1>[n_lv_trend] theta1_trend;",
      block = "parameters"
    )
    components <- append(components, list(ma_parameters_stanvar))
  }

  # 2. TPARAMETERS block - AR dynamics computation (always needed)
  # Build AR dynamics terms
  ar_terms <- sapply(ar_lags, function(lag) {
    glue::glue("ar{lag}_trend[j] * lv_trend[i-{lag}, j]")
  })
  ar_sum <- paste(ar_terms, collapse = " + ")

  ar_tparameters_stanvar <- brms::stanvar(
    name = "ar_tparameters",
    scode = glue::glue("
      // Latent states with AR dynamics
      matrix[n_trend, n_lv_trend] lv_trend;
      {if(has_ma) 'matrix[n_trend, n_lv_trend] ma_innovations_trend = scaled_innovations_trend;' else ''}

      {if(has_ma) '// Apply MA(1) transformation
      for (i in 2:n_trend) {{
        for (j in 1:n_lv_trend) {{
          ma_innovations_trend[i, j] += theta1_trend[j] * ma_innovations_trend[i-1, j];
        }}
      }}' else ''}

      // Initialize first {max_lag} time points
      for (i in 1:{max_lag}) {{
        lv_trend[i, :] = {if(has_ma) 'ma_innovations_trend' else 'scaled_innovations_trend'}[i, :];
      }}

      // Apply AR dynamics
      for (i in {max_lag + 1}:n_trend) {{
        for (j in 1:n_lv_trend) {{
          lv_trend[i, j] = {ar_sum} + {if(has_ma) 'ma_innovations_trend' else 'scaled_innovations_trend'}[i, j];
        }}
      }}
    "),
    block = "tparameters"
  )
  components <- append(components, list(ar_tparameters_stanvar))

  # 3. MODEL block - AR priors (AR coefficients and possibly MA, but not sigma_trend)
  # Build list of parameters that need priors
  ar_params_to_prior <- paste0("ar", ar_lags, "_trend")
  if (has_ma) {
    ar_params_to_prior <- c(ar_params_to_prior, "theta1_trend")
  }
  # Note: sigma_trend priors are handled by the shared innovation system
  # The trend generator shouldn't duplicate them

  if (length(ar_params_to_prior) > 0) {
    ar_model_stanvar <- generate_trend_priors_stanvar(
      param_names = ar_params_to_prior,
      prior = prior,
      stanvar_name = "ar_model"
    )
    components <- append_if_not_null(components, ar_model_stanvar)
  }

  # 4. Add trend computation (maps lv_trend through Z if needed)
  trend_computation <- generate_trend_computation_tparameters(n_lv, n_series)
  components <- append_if_not_null(components, trend_computation)

  # 5. Factor model priors if applicable
  if (is_factor_model) {
    factor_priors <- generate_factor_model(is_factor_model, n_lv)
    components <- append_if_not_null(components, factor_priors)
  }

  # Use the robust combine_stanvars function
  return(do.call(combine_stanvars, components))
}

#' VAR Trend Generator
#'
#' @description
#' Generates Stan code components for vector autoregressive (VAR) trends with
#' support for factor models, hierarchical correlations, and stationarity
#' constraints. Uses efficient matrix formulation for multi-lag VAR models
#' with proper parameter naming and non-centered parameterization.
#'
#' @param trend_specs Trend specification for VAR model containing parameters
#'   like lags (VAR order), n_lv (latent variables), gr (grouping), unit (time units)
#' @param data_info Data information including dimensions (n_obs, n_series, n_time)
#'   and other model structure details
#' @param prior A brmsprior object containing custom prior specifications for
#'   VAR trend parameters (A_trend matrices, sigma_trend, etc.). If NULL,
#'   uses defaults from trend registry. Default NULL.
#'
#' @return Combined stanvars object containing Stan code for:
#'   \itemize{
#'     \item Common trend data (dimensions and indices)
#'     \item Matrix Z for factor models (if n_lv < n_series)
#'     \item VAR coefficient matrices (A1_trend, A2_trend, ...) with stationarity constraints
#'     \item Innovation covariance parameters (sigma_trend)
#'     \item Hierarchical correlation structures (if grouping specified)
#'     \item Trend computation in transformed parameters
#'   }
#'
#' The function generates VAR model components with different configurations:
#' \enumerate{
#'   \item Standard VAR(p): A1_trend, A2_trend, ..., Ap_trend coefficient matrices
#'   \item Factor models: dimension reduction with Z matrix when n_lv < n_series
#'   \item Stationarity: automatic constraint application for stable dynamics
#'   \item Hierarchical models: group-specific parameters when gr specified
#'   \item Innovation structure: Wishart or LKJ priors for covariance matrices
#' }
#'
#' VAR models require careful prior specification due to high dimensionality.
#' The _trend suffix convention applies to all VAR parameters.
#'
#' @examples
#' # Standard VAR(2) model
#' trend_specs <- list(lags = 2, n_lv = 3)
#' data_info <- list(n_obs = 100, n_series = 3, n_time = 100)
#' stanvars <- generate_var_trend_stanvars(trend_specs, data_info)
#'
#' # Factor model VAR with custom priors
#' trend_specs <- list(lags = 1, n_lv = 2)
#' priors <- get_prior(cbind(y1, y2, y3) ~ x, trend_formula = ~ VAR(p = 1), data = dat)
#' stanvars <- generate_var_trend_stanvars(trend_specs, data_info, prior = priors)
#'
#' # Hierarchical VAR with grouping
#' trend_specs <- list(lags = 1, n_lv = 3, gr = "site")
#' stanvars <- generate_var_trend_stanvars(trend_specs, data_info, prior = priors)
#'
#' @noRd
generate_var_trend_stanvars <- function(trend_specs, data_info, prior = NULL) {
  # Input validation following CLAUDE.md standards
  checkmate::assert_list(trend_specs, names = "named")
  checkmate::assert_list(data_info, names = "named")
  checkmate::assert_class(prior, "brmsprior", null.ok = TRUE)

  # Validate required trend_specs components
  checkmate::assert_int(trend_specs$lags %||% 1, lower = 1)
  checkmate::assert_int(trend_specs$ma_lags %||% 0, lower = 0)
  checkmate::assert_int(trend_specs$n_lv %||% 1, lower = 1)

  # Validate required data_info components
  checkmate::assert_int(data_info$n_obs, lower = 1)
  checkmate::assert_int(data_info$n_series %||% 1, lower = 1)

  # Extract key parameters with validation
  n_lv <- trend_specs$n_lv %||% 1
  lags <- trend_specs$lags %||% 1
  ma_lags <- trend_specs$ma_lags %||% 0
  n <- data_info$n_obs
  n_series <- data_info$n_series %||% 1

  # VAR/VARMA constraint validation
  if (lags < 1) {
    insight::format_error("VAR model requires {.field lags} >= 1")
  }
  if (ma_lags < 0) {
    insight::format_error("VARMA model requires {.field ma_lags} >= 0")
  }
  if (n_lv > n_series && ma_lags == 0) {
    insight::format_error("VAR factor model requires {.field n_lv} <= {.field n_series}")
  }

  # Check for hierarchical grouping requirements - CRITICAL for grouped VAR models
  is_hierarchical <- !is.null(trend_specs$gr) && trend_specs$gr != 'NA'
  hierarchical_info <- NULL

  if (is_hierarchical) {
    # Cross-cutting validation handled by injection function

    # Extract hierarchical grouping information from data_info
    # Expect data_info to contain sorted dataframe information for group indexing
    n_groups <- data_info$n_groups %||% stop("Missing n_groups in data_info for hierarchical VAR")
    n_subgroups <- data_info$n_subgroups %||% stop("Missing n_subgroups in data_info for hierarchical VAR")

    # Generate group_inds matrix: maps (group, subgroup) -> series index
    # Based on sorted data where series = interaction(gr, subgr)
    group_inds <- matrix(
      1:(n_groups * n_subgroups),
      nrow = n_groups,
      ncol = n_subgroups,
      byrow = TRUE
    )

    # Generate Stan code for group_inds initialization
    group_inds_code <- paste(
      sapply(1:nrow(group_inds), function(i) {
        paste0("group_inds_trend[", i, "] = {",
               paste(group_inds[i, ], collapse = ", "), "};")
      }),
      collapse = "\n        "
    )

    # VAR models use group-specific coefficients with shared hyperpriors
    # and block-structured matrices for proper within-group interactions only
  }

  # Determine model type with validation
  is_varma <- ma_lags > 0
  is_factor_model <- n_lv < n_series
  use_grouping <- !is.null(trend_specs$gr) && trend_specs$gr != 'NA'

  # Additional validation for logical consistency
  checkmate::assert_logical(is_varma, len = 1)
  if (is_varma && ma_lags <= 0) {
    insight::format_error("Internal error: VARMA flag set but {.field ma_lags} <= 0")
  }
  if (!is_varma && ma_lags > 0) {
    insight::format_error("Internal error: VARMA flag not set but {.field ma_lags} > 0")
  }

  # VAR/VARMA mathematical functions block with modern Stan syntax and numerical stability
  var_functions_stanvar <- brms::stanvar(
    name = "var_functions",
    scode = "
      /**
       * Compute matrix square root using eigendecomposition
       * Following Heaps 2022 methodology for stationary VAR/VARMA
       * @param A Symmetric positive definite matrix (m x m)
       * @return Matrix square root of A
       */
      matrix sqrtm(matrix A) {
        int m = rows(A);
        vector[m] eigenvals = eigenvalues_sym(A);

        // Numerical stability check for positive definiteness
        if (min(eigenvals) <= 1e-12) {
          reject(\"Matrix must be positive definite for square root computation\");
        }

        vector[m] root_root_evals = sqrt(sqrt(eigenvals));
        matrix[m, m] evecs = eigenvectors_sym(A);
        matrix[m, m] eprod = diag_post_multiply(evecs, root_root_evals);
        return tcrossprod(eprod);
      }

      /**
       * Transform P_real to P matrix using partial autocorrelation approach
       * Heaps 2022 transformation for stationarity constraints
       * @param P_real Real-valued unconstrained matrix
       * @return Constrained P matrix for stationary VAR coefficients
       */
      matrix AtoP(matrix P_real) {
        int m = rows(P_real);
        matrix[m, m] B = tcrossprod(P_real);
        for (i in 1:m) {
          B[i, i] += 1.0;
        }
        return mdivide_left_spd(sqrtm(B), P_real);
      }

      /**
       * Compute Kronecker product of two matrices
       * Used in companion matrix approach for VARMA initialization
       * @param A First matrix (m x n)
       * @param B Second matrix (p x q)
       * @return Kronecker product A ⊗ B (mp x nq)
       */
      matrix kronecker_prod(matrix A, matrix B) {
        int m = rows(A);
        int n = cols(A);
        int p = rows(B);
        int q = cols(B);
        matrix[m * p, n * q] C;

        for (i in 1:m) {
          for (j in 1:n) {
            int row_start = (i - 1) * p + 1;
            int row_end = (i - 1) * p + p;
            int col_start = (j - 1) * q + 1;
            int col_end = (j - 1) * q + q;
            C[row_start:row_end, col_start:col_end] = A[i, j] * B;
          }
        }
        return C;
      }

      /**
       * Perform reverse mapping from partial autocorrelations to stationary coefficients
       * Heaps 2022 Algorithm for computing phi coefficients from P matrices
       * @param P Array of partial autocorrelation matrices (modern syntax)
       * @param Sigma Innovation covariance matrix
       * @return Array containing phi coefficients and Gamma matrices [2, p]
       */
      array[,] matrix[,] rev_mapping(array[] matrix[,] P, matrix Sigma) {
        int p = size(P);
        int m = rows(Sigma);
        array[p, p] matrix[m, m] phi_for;
        array[p, p] matrix[m, m] phi_rev;
        array[p + 1] matrix[m, m] Sigma_for;
        array[p + 1] matrix[m, m] Sigma_rev;
        matrix[m, m] S_for;
        matrix[m, m] S_rev;
        array[p + 1] matrix[m, m] S_for_list;
        array[p + 1] matrix[m, m] Gamma_trans;
        array[2, p] matrix[m, m] phiGamma;

        // Step 1: Forward pass - compute Sigma_for and S_for_list
        Sigma_for[p + 1] = Sigma;
        S_for_list[p + 1] = sqrtm(Sigma);
        for (s in 1:p) {
          // Compute working matrices (S_rev is B^{-1}, S_for is working matrix)
          S_for = -tcrossprod(P[p - s + 1]);
          for (i in 1:m) {
            S_for[i, i] += 1.0;
          }
          S_rev = sqrtm(S_for);
          S_for_list[p - s + 1] = mdivide_right_spd(
            mdivide_left_spd(S_rev, sqrtm(quad_form_sym(Sigma_for[p - s + 2], S_rev))),
            S_rev
          );
          Sigma_for[p - s + 1] = tcrossprod(S_for_list[p - s + 1]);
        }

        // Step 2: Reverse pass - compute phi coefficients and Gamma matrices
        Sigma_rev[1] = Sigma_for[1];
        Gamma_trans[1] = Sigma_for[1];
        for (s in 0:(p - 1)) {
          S_for = S_for_list[s + 1];
          S_rev = sqrtm(Sigma_rev[s + 1]);
          phi_for[s + 1, s + 1] = mdivide_right_spd(S_for * P[s + 1], S_rev);
          phi_rev[s + 1, s + 1] = mdivide_right_spd(S_rev * P[s + 1]', S_for);
          Gamma_trans[s + 2] = phi_for[s + 1, s + 1] * Sigma_rev[s + 1];

          if (s >= 1) {
            // Update phi coefficients using recursive relations
            for (k in 1:s) {
              phi_for[s + 1, k] = phi_for[s, k] -
                                  phi_for[s + 1, s + 1] * phi_rev[s, s - k + 1];
              phi_rev[s + 1, k] = phi_rev[s, k] -
                                  phi_rev[s + 1, s + 1] * phi_for[s, s - k + 1];
            }
            // Update Gamma_trans using phi coefficients
            for (k in 1:s) {
              Gamma_trans[s + 2] = Gamma_trans[s + 2] +
                                   phi_for[s, k] * Gamma_trans[s + 2 - k];
            }
          }
          Sigma_rev[s + 2] = Sigma_rev[s + 1] -
                             quad_form_sym(Sigma_for[s + 1], phi_rev[s + 1, s + 1]');
        }

        // Pack results: phi coefficients in row 1, Gamma matrices in row 2
        for (i in 1:p) {
          phiGamma[1, i] = phi_for[p, i];
          phiGamma[2, i] = Gamma_trans[i]';
        }
        return phiGamma;
      }

      /**
       * Compute joint stationary covariance for VARMA(p,q) initialization
       * Heaps 2022 companion matrix approach for stationary distribution
       * @param Sigma Innovation covariance matrix (m x m)
       * @param phi Array of stationary VAR coefficient matrices
       * @param theta Array of stationary MA coefficient matrices
       * @return Joint covariance matrix Omega for (y_0,...,y_{1-p},eps_0,...,eps_{1-q})
       */
      matrix initial_joint_var(matrix Sigma, array[] matrix[,] phi, array[] matrix[,] theta) {
        int p = size(phi);
        int q = size(theta);
        int m = rows(Sigma);
        matrix[(p + q) * m, (p + q) * m] companion_mat = rep_matrix(0.0, (p + q) * m, (p + q) * m);
        matrix[(p + q) * m, (p + q) * m] companion_var = rep_matrix(0.0, (p + q) * m, (p + q) * m);
        matrix[(p + q) * m * (p + q) * m, (p + q) * m * (p + q) * m] tmp;
        matrix[(p + q) * m, (p + q) * m] Omega;

        // Construct companion matrix (phi_tilde) following Heaps 2022
        // VAR component: phi matrices in top-left block
        for (i in 1:p) {
          companion_mat[1:m, ((i - 1) * m + 1):(i * m)] = phi[i];
          if (i > 1) {
            // Identity blocks for VAR lags
            for (j in 1:m) {
              companion_mat[(i - 1) * m + j, (i - 2) * m + j] = 1.0;
            }
          }
        }

        // MA component: theta matrices in top-right block
        for (i in 1:q) {
          companion_mat[1:m, ((p + i - 1) * m + 1):((p + i) * m)] = theta[i];
        }

        // Identity blocks for MA lags (if q > 1)
        if (q > 1) {
          for (i in 2:q) {
            for (j in 1:m) {
              companion_mat[(p + i - 1) * m + j, (p + i - 2) * m + j] = 1.0;
            }
          }
        }

        // Construct innovation covariance matrix (Sigma_tilde)
        // Innovations affect y_t and eps_t simultaneously
        companion_var[1:m, 1:m] = Sigma;  // For y_t innovations
        companion_var[(p * m + 1):((p + 1) * m), (p * m + 1):((p + 1) * m)] = Sigma;  // For eps_t innovations
        companion_var[1:m, (p * m + 1):((p + 1) * m)] = Sigma;  // Cross-covariance
        companion_var[(p * m + 1):((p + 1) * m), 1:m] = Sigma;  // Symmetric cross-covariance

        // Solve Lyapunov equation: Omega = Sigma_tilde + Phi_tilde * Omega * Phi_tilde'
        // Vectorized form: vec(Omega) = (I - Phi_tilde ⊗ Phi_tilde)^{-1} vec(Sigma_tilde)
        tmp = diag_matrix(rep_vector(1.0, (p + q) * m * (p + q) * m)) -
              kronecker_prod(companion_mat, companion_mat);

        Omega = to_matrix(tmp \\ to_vector(companion_var), (p + q) * m, (p + q) * m);

        // Ensure numerical symmetry of result
        for (i in 1:(rows(Omega) - 1)) {
          for (j in (i + 1):rows(Omega)) {
            Omega[j, i] = Omega[i, j];
          }
        }

        return Omega;
      }
    ",
    block = "functions"
  )

  # VAR/VARMA transformed data with hyperparameter constants and utility arrays
  var_tdata_stanvar <- brms::stanvar(
    name = "var_tdata",
    scode = glue::glue("
      // Zero mean vector for VARMA process (following Heaps 2022)
      vector[{n_lv}] trend_zeros = rep_vector(0.0, {n_lv});

      {if(is_hierarchical) glue::glue('
      // Hierarchical grouping data structures
      int<lower=0> n_groups_trend = {n_groups};
      int<lower=0> n_subgroups_trend = {n_subgroups};

      // Group indexing matrix: maps (group, subgroup) -> series index
      // Populated from sorted data where series = interaction(gr, subgr)
      array[{n_groups}, {n_subgroups}] int<lower=1> group_inds_trend;
      {{
        {group_inds_code}
      }}
      ') else ''}

      // Hyperparameter constants for hierarchical priors (as constants, not user inputs)
      // For A_trend coefficient matrices: diagonal and off-diagonal elements
      array[2] vector[2] es_trend = {{
        {{0.0, 0.0}},    // Means for diagonal elements [lag 1, lag 2, ...]
        {{0.0, 0.0}}     // Means for off-diagonal elements [lag 1, lag 2, ...]
      }};
      array[2] vector[2] fs_trend = {{
        {{1.0, 1.0}},    // Standard deviations for diagonal elements
        {{1.0, 1.0}}     // Standard deviations for off-diagonal elements
      }};
      array[2] vector[2] gs_trend = {{
        {{2.0, 2.0}},    // Gamma shape parameters for diagonal precision
        {{2.0, 2.0}}     // Gamma shape parameters for off-diagonal precision
      }};
      array[2] vector[2] hs_trend = {{
        {{1.0, 1.0}},    // Gamma rate parameters for diagonal precision
        {{1.0, 1.0}}     // Gamma rate parameters for off-diagonal precision
      }};

      {if(is_varma) paste0('
      // Additional hyperparameters for D_trend (MA coefficient matrices) when ma_lags > 0
      array[2] vector[2] es_ma_trend = {{
        {{0.0, 0.0}},    // Means for MA diagonal elements
        {{0.0, 0.0}}     // Means for MA off-diagonal elements
      }};
      array[2] vector[2] fs_ma_trend = {{
        {{1.0, 1.0}},    // Standard deviations for MA diagonal elements
        {{1.0, 1.0}}     // Standard deviations for MA off-diagonal elements
      }};
      array[2] vector[2] gs_ma_trend = {{
        {{2.0, 2.0}},    // Gamma shape for MA diagonal precision
        {{2.0, 2.0}}     // Gamma shape for MA off-diagonal precision
      }};
      array[2] vector[2] hs_ma_trend = {{
        {{1.0, 1.0}},    // Gamma rate for MA diagonal precision
        {{1.0, 1.0}}     // Gamma rate for MA off-diagonal precision
      }};
      ') else ''}
    "),
    block = "tdata"
  )

  # Core VAR parameters block with conditional VARMA extensions
  var_parameters_stanvar <- brms::stanvar(
    name = "var_parameters",
    scode = glue::glue("
      {if(is_hierarchical) glue::glue('
      // Hierarchical VAR: group-specific raw matrices with shared hyperpriors
      array[{n_groups}, {lags}] matrix[{n_subgroups}, {n_subgroups}] A_raw_group_trend;

      // Group-specific variance parameters (for hierarchical correlations)
      array[{n_groups}] vector<lower=0>[{n_subgroups}] sigma_group_trend;
      ') else '
      // Standard VAR: single raw matrix
      array[{lags}] matrix[n_lv_trend, n_lv_trend] A_raw_trend;

      // Standard variance and correlation parameters
      vector<lower=0>[n_lv_trend] sigma_trend;
      cholesky_factor_corr[n_lv_trend] L_Omega_trend;
      '}

      // Shared hierarchical hyperparameters for A_raw coefficients (across all groups)
      // [1] = diagonal elements, [2] = off-diagonal elements
      array[2] vector[{lags}] Amu_trend;     // Shared means
      array[2] vector<lower=0>[{lags}] Aomega_trend;  // Shared precisions

      // Joint initialization vector for stationary distribution
      vector[{if(is_varma) paste0('(', lags, ' + ', ma_lags, ') * n_lv_trend') else paste0(lags, ' * n_lv_trend')}] init_trend;

      // Standard latent variable trends - consistent with other trend generators
      matrix[n_trend, n_lv_trend] lv_trend;
    "),
    block = "parameters"
  )

  # Conditional MA parameters block for VARMA(p,q) when ma_lags > 0
  # This creates a 4th stanvar component only for VARMA models
  # D_trend naming follows Heaps 2022 convention for MA coefficients
  if (is_varma) {
    var_ma_parameters_stanvar <- brms::stanvar(
      name = "var_ma_parameters",
      scode = glue::glue("
      // Raw MA partial autocorrelation matrices (unconstrained for stationarity)
      // D_raw_trend gets transformed to D_trend (stationary MA coefficients)
      array[{ma_lags}] matrix[n_lv_trend, n_lv_trend] D_raw_trend;

      // Hierarchical hyperparameters for D_raw_trend (MA) coefficients
      // Mirrors A_raw_trend hyperparameter structure for consistency
      // [1] = diagonal elements, [2] = off-diagonal elements
      array[2] vector[{ma_lags}] Dmu_trend;           // Means for D_raw_trend elements
      array[2] vector<lower=0>[{ma_lags}] Domega_trend;  // Precisions for D_raw_trend elements
      "),
      block = "parameters"
    )
  }

  # VAR/VARMA transformed parameters - compute stationary coefficients
  var_tparameters_stanvar <- brms::stanvar(
    name = "var_tparameters",
    scode = glue::glue("
      {if(is_hierarchical) glue::glue('
      // Hierarchical VAR: group-specific computations then block assembly

      // Group-specific hierarchical correlations and covariances
      array[{n_groups}] cov_matrix[{n_subgroups}] Sigma_group_trend;
      array[{n_groups}, {lags}] matrix[{n_subgroups}, {n_subgroups}] A_group_trend;

      // Compute group-specific covariances using hierarchical correlation structure
      for (g in 1:{n_groups}) {{
        // Combine global and local correlations using existing hierarchical infrastructure
        matrix[{n_subgroups}, {n_subgroups}] L_Omega_group =
          cholesky_compose(alpha_cor * multiply_lower_tri_self_transpose(L_Omega_global) +
                           (1 - alpha_cor) * multiply_lower_tri_self_transpose(L_deviation_group[g]));
        Sigma_group_trend[g] = multiply_lower_tri_self_transpose(
          diag_pre_multiply(sigma_group_trend[g], L_Omega_group));

        // Apply Heaps transformation per group
        for (lag in 1:{lags}) {{
          array[1] matrix[{n_subgroups}, {n_subgroups}] P_group;
          P_group[1] = AtoP(A_raw_group_trend[g, lag]);
          array[2, 1] matrix[{n_subgroups}, {n_subgroups}] result_group =
            rev_mapping(P_group, Sigma_group_trend[g]);
          A_group_trend[g, lag] = result_group[1, 1];
        }}
      }}

      // Build block-structured full matrices (groups do not interact)
      cov_matrix[n_lv_trend] Sigma_trend = rep_matrix(0, n_lv_trend, n_lv_trend);
      array[{lags}] matrix[n_lv_trend, n_lv_trend] A_trend;
      for (lag in 1:{lags}) {{
        A_trend[lag] = rep_matrix(0, n_lv_trend, n_lv_trend);
      }}

      for (g in 1:{n_groups}) {{
        Sigma_trend[group_inds_trend[g], group_inds_trend[g]] = Sigma_group_trend[g];
        for (lag in 1:{lags}) {{
          A_trend[lag][group_inds_trend[g], group_inds_trend[g]] = A_group_trend[g, lag];
        }}
      }}
      ') else '
      // Standard VAR: single covariance matrix and transformation
      matrix[n_lv_trend, n_lv_trend] L_Sigma_trend = diag_pre_multiply(sigma_trend, L_Omega_trend);
      cov_matrix[n_lv_trend] Sigma_trend = multiply_lower_tri_self_transpose(L_Sigma_trend);

      // Transform raw parameters to stationary coefficients
      array[{lags}] matrix[n_lv_trend, n_lv_trend] A_trend;

      // Working arrays for stationarity transformation
      array[{lags}] matrix[n_lv_trend, n_lv_trend] P_var;
      array[2, {lags}] matrix[n_lv_trend, n_lv_trend] result_var;

      for (i in 1:{lags}) {{
        P_var[i] = AtoP(A_raw_trend[i]);
      }}

      result_var = rev_mapping(P_var, Sigma_trend);

      for (i in 1:{lags}) {{
        A_trend[i] = result_var[1, i];
      }}
      '}

      {if(is_varma) glue::glue('array[{ma_lags}] matrix[n_lv_trend, n_lv_trend] D_trend;') else ''}

      // Joint covariance matrix for stationary initialization
      cov_matrix[{if(is_varma) paste0('(', lags, ' + ', ma_lags, ') * n_lv_trend') else paste0(lags, ' * n_lv_trend')}] Omega_trend;

      {if(is_varma) glue::glue('vector[n_lv_trend] ma_init_trend[{ma_lags}];  // Initial MA errors') else ''}

      // Working arrays for stationarity transformation
      array[{lags}] matrix[n_lv_trend, n_lv_trend] P_var;
      array[2, {lags}] matrix[n_lv_trend, n_lv_trend] result_var;

      // Transform A_raw_trend to stationary A_trend using Heaps methodology
      for (i in 1:{lags}) {{
        P_var[i] = AtoP(A_raw_trend[i]);
      }}

      // Apply reverse mapping to get stationary coefficients
      result_var = rev_mapping(P_var, Sigma_trend);

      // Extract stationary VAR coefficients (these become our final A_trend)
      for (i in 1:{lags}) {{
        A_trend[i] = result_var[1, i];
      }}

      {if(is_varma) glue::glue('
      // Transform D_raw_trend to stationary D_trend (VARMA only)
      array[{ma_lags}] matrix[n_lv_trend, n_lv_trend] P_ma;
      array[2, {ma_lags}] matrix[n_lv_trend, n_lv_trend] result_ma;

      // Transform D_raw_trend matrices using AtoP transformation
      for (i in 1:{ma_lags}) {{
        P_ma[i] = AtoP(D_raw_trend[i]);
      }}

      // Apply reverse mapping to get stationary MA coefficients
      result_ma = rev_mapping(P_ma, Sigma_trend);

      // Extract stationary MA coefficients (negative sign per Heaps 2022)
      for (i in 1:{ma_lags}) {{
        D_trend[i] = -result_ma[1, i];
      }}
      ') else ''}

      // Compute initial joint covariance matrix using companion matrix approach
      {if(is_varma) 'Omega_trend = initial_joint_var(Sigma_trend, A_trend, D_trend);' else 'array[1] matrix[n_lv_trend, n_lv_trend] empty_theta; empty_theta[1] = rep_matrix(0.0, n_lv_trend, n_lv_trend); Omega_trend = initial_joint_var(Sigma_trend, A_trend, empty_theta[1:0]);'}

      {if(is_varma) glue::glue('
      // Initialize MA error terms from init_trend (VARMA specific)
      for (i in 1:{ma_lags}) {{
        int start_idx = {lags} * n_lv_trend + (i - 1) * n_lv_trend + 1;
        int end_idx = {lags} * n_lv_trend + i * n_lv_trend;
        ma_init_trend[i] = init_trend[start_idx:end_idx];
      }}
      ') else ''}
    "),
    block = "tparameters"
  )

  # VAR/VARMA model block with joint initial distribution (Task 2.7.8.10)
  # Pre-calculate dimensions for clarity and validation
  checkmate::assert_logical(is_varma, len = 1)
  checkmate::assert_int(lags, lower = 1)
  checkmate::assert_int(n_lv, lower = 1)
  if (is_varma) {
    checkmate::assert_int(ma_lags, lower = 1)
  }

  # Calculate initialization vector dimension: lags*n_lv_trend for VAR, (lags+ma_lags)*n_lv_trend for VARMA
  init_dim_expr <- if (is_varma) glue::glue('({lags} + {ma_lags}) * n_lv_trend') else glue::glue('{lags} * n_lv_trend')

  var_model_stanvar <- brms::stanvar(
    name = "var_model",
    scode = glue::glue("
      // VARMA likelihood implementation following Heaps 2022 methodology

      // Initial joint distribution for stationary VARMA initialization
      vector[{init_dim_expr}] mu_init_trend = rep_vector(0.0, {init_dim_expr});
      init_trend ~ multi_normal(mu_init_trend, Omega_trend);

      // Conditional means for VARMA dynamics
      vector[n_lv_trend] mu_t_trend[n_trend];
      {if(is_varma) glue::glue('vector[n_lv_trend] ma_error_trend[n_trend];  // MA error terms') else ''}

      // Compute conditional means for all time points
      for (t in 1:n_trend) {{
        mu_t_trend[t] = rep_vector(0.0, n_lv_trend);

        // VAR component: Add autoregressive terms
        for (i in 1:{lags}) {{
          if (t - i <= 0) {{
            // Use values from earlier than series start (from init_trend)
            int init_idx = {lags} - (t - i) + 1;
            if (init_idx > 0 && init_idx <= {lags}) {{
              vector[n_lv_trend] lagged_lv;
              int start_idx = (init_idx - 1) * n_lv_trend + 1;
              int end_idx = init_idx * n_lv_trend;
              lagged_lv = init_trend[start_idx:end_idx];
              mu_t_trend[t] += A_trend[i] * lagged_lv;
            }}
          }} else {{
            // Use regular lv_trend values
            mu_t_trend[t] += A_trend[i] * lv_trend[t - i, :];
          }}
        }}

        {if(is_varma) glue::glue('
        // MA component: Add moving average terms for VARMA
        for (i in 1:{ma_lags}) {{
          if (t - i <= 0) {{
            // Use initial MA errors for early time points
            if (i <= {ma_lags}) {{
              mu_t_trend[t] += D_trend[i] * ma_init_trend[i];
            }}
          }} else {{
            // Use computed MA errors from previous time points
            mu_t_trend[t] += D_trend[i] * ma_error_trend[t - i];
          }}
        }}
        ') else ''}
      }}

      // Observation likelihood for time series
      for (t in 1:n_trend) {{
        lv_trend[t, :] ~ multi_normal(mu_t_trend[t], Sigma_trend);
        {if(is_varma) '// Compute MA error for next iteration\n        ma_error_trend[t] = lv_trend[t, :] - mu_t_trend[t];' else ''}
      }}

      {if(is_hierarchical) glue::glue('
      // Hierarchical VAR: group-specific priors with shared hyperpriors
      for (g in 1:{n_groups}) {{
        for (lag in 1:{lags}) {{
          // Diagonal elements prior (shared across all groups)
          diagonal(A_raw_group_trend[g, lag]) ~ normal(Amu_trend[1, lag], 1 / sqrt(Aomega_trend[1, lag]));

          // Off-diagonal elements prior (shared across all groups)
          for (i in 1:{n_subgroups}) {{
            for (j in 1:{n_subgroups}) {{
              if (i != j) {{
                A_raw_group_trend[g, lag, i, j] ~ normal(Amu_trend[2, lag], 1 / sqrt(Aomega_trend[2, lag]));
              }}
            }}
          }}
        }}
      }}
      ') else '
      // Standard VAR: single matrix priors
      for (lag in 1:{lags}) {{
        // Diagonal elements prior
        diagonal(A_raw_trend[lag]) ~ normal(Amu_trend[1, lag], 1 / sqrt(Aomega_trend[1, lag]));

        // Off-diagonal elements prior
        for (i in 1:n_lv_trend) {{
          for (j in 1:n_lv_trend) {{
            if (i != j) {{
              A_raw_trend[lag, i, j] ~ normal(Amu_trend[2, lag], 1 / sqrt(Aomega_trend[2, lag]));
            }}
          }}
        }}
      }}
      '}

      {if(is_varma) glue::glue('
      // Hierarchical priors for VARMA MA coefficient matrices (D_raw_trend)
      // Following same structure as VAR coefficients but conditional on ma_lags > 0
      for (ma_lag in 1:{ma_lags}) {{
        // Diagonal elements prior for MA coefficients
        diagonal(D_raw_trend[ma_lag]) ~ normal(Dmu_trend[1, ma_lag], 1 / sqrt(Domega_trend[1, ma_lag]));

        // Off-diagonal elements prior for MA coefficients
        for (i in 1:n_lv_trend) {{
          for (j in 1:n_lv_trend) {{
            if (i != j) {{
              D_raw_trend[ma_lag, i, j] ~ normal(Dmu_trend[2, ma_lag], 1 / sqrt(Domega_trend[2, ma_lag]));
            }}
          }}
        }}
      }}

      // Hyperpriors for hierarchical MA coefficient means and precisions
      for (component in 1:2) {{  // [1] diagonal, [2] off-diagonal
        Dmu_trend[component] ~ normal(es_ma_trend[component], fs_ma_trend[component]);
        Domega_trend[component] ~ gamma(gs_ma_trend[component], hs_ma_trend[component]);
      }}
      ') else ''}

      // Innovation variance and correlation priors (consistent with other trend generators)
      // Variance parameters using inverse gamma (equivalent to existing patterns)
      sigma_trend ~ inv_gamma(1.418, 0.452);

      // LKJ correlation prior on Cholesky factor
      L_Omega_trend ~ lkj_corr_cholesky(2);

      // Hyperpriors for hierarchical VAR coefficient means and precisions
      // Following Heaps 2022 exchangeable hyperprior structure
      for (component in 1:2) {{  // [1] diagonal, [2] off-diagonal
        Amu_trend[component] ~ normal(es_trend[component], fs_trend[component]);
        Aomega_trend[component] ~ gamma(gs_trend[component], hs_trend[component]);
      }}
    "),
    block = "model"
  )

  # Generate centralized priors for trend parameters
  # Standard parameters: sigma_trend (L_Omega_trend handled by LKJ in model block)
  # Hierarchical hyperparameters: Amu_trend, Aomega_trend, and conditional MA/group parameters
  var_params_to_prior <- c("sigma_trend", "Amu_trend", "Aomega_trend")

  # Add hierarchical group parameters
  if (is_hierarchical) {
    var_params_to_prior <- c(var_params_to_prior, "sigma_group_trend")
  }

  # Add VARMA MA hyperparameters
  if (is_varma) {
    var_params_to_prior <- c(var_params_to_prior, "Dmu_trend", "Domega_trend")
  }

  var_centralized_priors <- NULL
  if (length(var_params_to_prior) > 0) {
    var_centralized_priors <- generate_trend_priors_stanvar(
      param_names = var_params_to_prior,
      prior = prior,
      stanvar_name = "var_centralized_priors"
    )
  }

  # STEP 1: Dimensions handled by calling context (no duplication)

  # STEP 2: Add Z matrix using standard generation (VAR supports factor models unless hierarchical)
  matrix_z <- generate_matrix_z_multiblock_stanvars(is_factor_model, n_lv, n_series)

  # Add trend computation stanvars (maps lv_trend through Z matrix)
  trend_computation <- generate_trend_computation_tparameters(n_lv, n_series)

  # Create components list based on model type
  base_components <- if (is_varma) {
    # VARMA case: include MA parameters (7 components: Z + var-specific)
    list(matrix_z, var_functions_stanvar, var_tdata_stanvar,
         var_parameters_stanvar, var_ma_parameters_stanvar,
         var_tparameters_stanvar, var_model_stanvar)
  } else {
    # VAR-only case: no MA parameters (6 components: Z + var-specific)
    list(matrix_z, var_functions_stanvar, var_tdata_stanvar, var_parameters_stanvar,
         var_tparameters_stanvar, var_model_stanvar)
  }

  # Add trend computation (required for all VAR models)
  components <- append_if_not_null(base_components, trend_computation)

  # Add hierarchical correlation support if applicable
  if (is_hierarchical) {
    # Add existing hierarchical correlation infrastructure
    hierarchical_functions <- generate_hierarchical_functions()
    hierarchical_params <- generate_hierarchical_correlation_parameters(n_groups, n_subgroups)
    hierarchical_priors <- generate_hierarchical_correlation_model(n_groups)

    components <- append_if_not_null(components, list(hierarchical_functions, hierarchical_params, hierarchical_priors))
  }

  # Add factor model support if applicable
  if (is_factor_model) {
    factor_priors <- generate_factor_model(is_factor_model, n_lv)
    components <- append_if_not_null(components, factor_priors)
  }

  # Add centralized priors if they exist (final optional component)
  components <- append_if_not_null(components, var_centralized_priors)

  # Combine stanvars using established pattern
  result_stanvars <- do.call(combine_stanvars, components)

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
#' @description
#' Generates Stan code components for continuous-time autoregressive (CAR) trends
#' which model temporal dynamics using continuous-time damped oscillator formulation.
#' CAR trends are useful for irregularly spaced time series and automatic
#' handling of missing observations. Does NOT support factor models or
#' hierarchical correlations due to continuous-time constraints.
#'
#' @param trend_specs Trend specification for CAR model containing parameters
#'   like n_lv (must equal n_series), but NOT supporting factor models or grouping
#' @param data_info Data information including dimensions (n_obs, n_series, n_time)
#'   and time spacing details for continuous-time modeling
#' @param prior A brmsprior object containing custom prior specifications for
#'   CAR trend parameters (ar1_trend, sigma_trend). If NULL, uses defaults
#'   from trend registry. Default NULL.
#'
#' @return Combined stanvars object containing Stan code for:
#'   \itemize{
#'     \item Common trend data (dimensions and indices)
#'     \item CAR coefficient parameters (ar1_trend for damping)
#'     \item Innovation variance parameters (sigma_trend)
#'     \item Continuous-time dynamics formulation
#'     \item Trend computation in transformed parameters
#'   }
#'
#' CAR trends use continuous-time formulation with specific constraints:
#' \enumerate{
#'   \item No factor models: n_lv must equal n_series (series-specific evolution)
#'   \item No hierarchical correlations: gr parameter not supported
#'   \item Continuous-time: handles irregular time spacing automatically
#'   \item Damped oscillator: ar1_trend controls damping rate
#'   \item Missing data: natural handling of gaps in continuous formulation
#' }
#'
#' The ar1_trend parameter in CAR models represents the damping coefficient
#' in continuous time, distinct from discrete-time AR(1) coefficients.
#'
#' @examples
#' # Standard CAR trend
#' trend_specs <- list(n_lv = 3)  # Must equal n_series
#' data_info <- list(n_obs = 100, n_series = 3, n_time = 100)
#' stanvars <- generate_car_trend_stanvars(trend_specs, data_info)
#'
#' # CAR with custom priors
#' priors <- get_prior(cbind(y1, y2, y3) ~ x, trend_formula = ~ CAR(), data = dat)
#' stanvars <- generate_car_trend_stanvars(trend_specs, data_info, prior = priors)
#'
#' @noRd
generate_car_trend_stanvars <- function(trend_specs, data_info, prior = NULL) {
  # Input validation
  checkmate::assert_list(trend_specs, names = "named")
  checkmate::assert_list(data_info, names = "named")
  checkmate::assert_class(prior, "brmsprior", null.ok = TRUE)

  # Extract dimensions and configuration
  n_lv <- trend_specs$n_lv %||% data_info$n_series %||% 1
  n_series <- data_info$n_series %||% 1
  n_obs <- data_info$n_obs

  # Validate dimensions
  checkmate::assert_int(n_lv, lower = 1)
  checkmate::assert_int(n_series, lower = 1)
  checkmate::assert_int(n_obs, lower = 1)

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

  # Build components list following the 3-stanvar pattern
  components <- list()

  # STEP 1: Dimensions handled by calling context (no duplication)
  is_factor_model <- FALSE  # CAR never uses factor models

  # STEP 2: Always add matrix Z (CAR uses diagonal Z in transformed data)
  matrix_z <- generate_matrix_z_multiblock_stanvars(is_factor_model, n_lv, n_series)
  components <- append_if_not_null(components, matrix_z)

  # Calculate time distances for continuous-time AR evolution
  time_dis <- calculate_car_time_distances(data_info)

  # Time distance data for continuous-time AR
  time_dis_data_stanvar <- brms::stanvar(
    x = time_dis,
    name = "time_dis",
    scode = glue::glue("array[n, n_series_trend] real<lower=0> time_dis;"),
    block = "data"
  )
  components <- append(components, list(time_dis_data_stanvar))

  # 1. PARAMETERS block - CAR trend-specific parameters
  car_parameters_stanvar <- brms::stanvar(
    name = "car_parameters",
    scode = "// CAR AR1 parameters\nvector<lower=-1,upper=1>[n_lv_trend] ar1_trend;",
    block = "parameters"
  )
  components <- append(components, list(car_parameters_stanvar))

  # 2. TPARAMETERS block - CAR dynamics computation (always needed)
  car_tparameters_stanvar <- brms::stanvar(
    name = "car_tparameters",
    scode = glue::glue("
      // CAR latent variable evolution using shared innovation system
      matrix[n_trend, n_lv_trend] lv_trend;

      // Initialize first time point with innovations
      for (j in 1:n_lv_trend) {{
        lv_trend[1, j] = scaled_innovations_trend[1, j];
      }}

      // Apply continuous-time AR evolution for subsequent time points
      for (j in 1:n_lv_trend) {{
        for (i in 2:n_trend) {{
          lv_trend[i, j] = pow(ar1_trend[j], time_dis[i, j]) * lv_trend[i - 1, j]
                         + scaled_innovations_trend[i, j];
        }}
      }}
    "),
    block = "tparameters"
  )
  components <- append(components, list(car_tparameters_stanvar))

  # 3. MODEL block - CAR priors (ar1_trend, but not sigma_trend)
  # Build list of parameters that need priors
  car_params_to_prior <- c("ar1_trend")
  # Note: sigma_trend priors are handled by the shared innovation system
  # The trend generator shouldn't duplicate them

  if (length(car_params_to_prior) > 0) {
    car_model_stanvar <- generate_trend_priors_stanvar(
      param_names = car_params_to_prior,
      prior = prior,
      stanvar_name = "car_model"
    )
    components <- append_if_not_null(components, car_model_stanvar)
  }

  # 4. Add trend computation (maps lv_trend through Z if needed)
  trend_computation <- generate_trend_computation_tparameters(n_lv, n_series)
  components <- append_if_not_null(components, trend_computation)

  # Use the robust combine_stanvars function
  return(do.call(combine_stanvars, components))
}

#' ZMVN Trend Generator
#'
#' @description
#' Generates Stan code components for zero-mean multivariate normal (ZMVN) trends
#' which represent unstructured random effects across time. ZMVN trends provide
#' the simplest multivariate state-space structure with independent Gaussian
#' innovations and optional factor model dimension reduction. Supports both
#' hierarchical correlations and factor models.
#'
#' @param trend_specs Trend specification for ZMVN model containing parameters
#'   like n_lv (latent variables), gr (grouping), unit (time units)
#' @param data_info Data information including dimensions (n_obs, n_series, n_time)
#'   and grouping structure details for hierarchical models
#' @param prior A brmsprior object containing custom prior specifications for
#'   ZMVN trend parameters (sigma_trend, LV, Z matrix). If NULL, uses defaults
#'   from trend registry. Default NULL.
#'
#' @return Combined stanvars object containing Stan code for:
#'   \itemize{
#'     \item Common trend data (dimensions and indices)
#'     \item Matrix Z for factor models (if n_lv < n_series)
#'     \item Innovation variance parameters (sigma_trend)
#'     \item Latent variable parameters (LV for states)
#'     \item Hierarchical correlation structures (if grouping specified)
#'     \item Trend computation in transformed parameters
#'   }
#'
#' ZMVN trends support various model configurations:
#' \enumerate{
#'   \item Standard ZMVN: independent Gaussian innovations for each series
#'   \item Factor models: dimension reduction with Z matrix when n_lv < n_series
#'   \item Hierarchical models: group-specific parameters when gr specified
#'   \item Minimal structure: simplest multivariate state-space specification
#'   \item Default choice: used when no specific trend dynamics specified
#' }
#'
#' ZMVN trends use minimal prior specifications as they represent unstructured
#' variation. The _trend suffix convention applies to variance parameters.
#'
#' @examples
#' # Standard ZMVN trend
#' trend_specs <- list(n_lv = 3)
#' data_info <- list(n_obs = 100, n_series = 3, n_time = 100)
#' stanvars <- generate_zmvn_trend_stanvars(trend_specs, data_info)
#'
#' # Factor model ZMVN with custom priors
#' trend_specs <- list(n_lv = 2)  # n_lv < n_series
#' priors <- get_prior(cbind(y1, y2, y3) ~ x, trend_formula = ~ ZMVN(), data = dat)
#' stanvars <- generate_zmvn_trend_stanvars(trend_specs, data_info, prior = priors)
#'
#' # Hierarchical ZMVN with grouping
#' trend_specs <- list(n_lv = 3, gr = "site")
#' stanvars <- generate_zmvn_trend_stanvars(trend_specs, data_info, prior = priors)
#'
#' @noRd
generate_zmvn_trend_stanvars <- function(trend_specs, data_info, prior = NULL) {
  # Input validation
  checkmate::assert_list(trend_specs, names = "named")
  checkmate::assert_list(data_info, names = "named")
  checkmate::assert_class(prior, "brmsprior", null.ok = TRUE)

  # Extract key parameters following original ZMVN pattern
  n_lv <- trend_specs$n_lv %||% data_info$n_lv %||% data_info$n_series %||% 1
  n_obs <- data_info$n_obs
  n_series <- data_info$n_series %||% 1
  use_grouping <- !is.null(trend_specs$gr) && trend_specs$gr != 'NA'

  # Validate dimensions
  checkmate::assert_int(n_obs, lower = 1)
  checkmate::assert_int(n_series, lower = 1)
  checkmate::assert_int(n_lv, lower = 1)

  # Determine if this is a factor model (n_lv < n_series)
  is_factor_model <- !is.null(trend_specs$n_lv) && n_lv < n_series

  # Cross-cutting validation handled by injection function

  # Build components list following the 3-stanvar pattern
  components <- list()

  # STEP 1: Dimensions handled by calling context (no duplication)

  # STEP 2: Always add matrix Z (factor=parameters, non-factor=diagonal in tdata)
  matrix_z <- generate_matrix_z_multiblock_stanvars(is_factor_model, n_lv, n_series)
  components <- append_if_not_null(components, matrix_z)

  # Shared innovations handled by injection function (no duplication)

  # ZMVN parameters (empty for simple case - correlation handled by shared system)
  zmvn_parameters_stanvar <- brms::stanvar(
    name = "zmvn_parameters",
    scode = "",
    block = "parameters"
  )

  # ZMVN transformed parameters - direct transformation from scaled_innovations_trend
  zmvn_tparameters_stanvar <- brms::stanvar(
    name = "zmvn_tparameters",
    scode = "matrix[n_trend, n_lv_trend] lv_trend;\nlv_trend = scaled_innovations_trend;",
    block = "tparameters"
  )

  # ZMVN model (empty - priors handled by shared system)
  zmvn_model_stanvar <- brms::stanvar(
    name = "zmvn_model",
    scode = "",
    block = "model"
  )

  components <- append_if_not_null(components, zmvn_parameters_stanvar)
  components <- append_if_not_null(components, zmvn_tparameters_stanvar)
  components <- append_if_not_null(components, zmvn_model_stanvar)

  # Add trend computation stanvars
  trend_computation <- generate_trend_computation_tparameters(n_lv, n_series)
  components <- append_if_not_null(components, trend_computation)

  # Add factor model priors if applicable
  if (is_factor_model) {
    factor_priors <- generate_factor_model(is_factor_model, n_lv)
    components <- append_if_not_null(components, factor_priors)
  }

  # ZMVN doesn't need additional priors (shared system handles all priors)

  # Combine all components
  return(do.call(combine_stanvars, components))
}

#' PW Trend Generator
#'
#' @description
#' Generates Stan code components for piecewise (PW) trends using Prophet-style
#' changepoint detection and trend flexibility. Supports both linear and logistic
#' growth patterns with automatic changepoint identification and series-specific
#' parameters. Does NOT support factor models due to changepoint complexity
#' requirements.
#'
#' @param trend_specs Trend specification for PW model containing parameters
#'   like n_changepoints (number of changepoints), changepoint_scale (prior scale),
#'   type/growth (trend pattern)
#' @param data_info Data information including dimensions (n_obs, n_series, n_time)
#'   and time structure for changepoint placement
#' @param growth Growth pattern: "linear" or "logistic" (optional, overrides
#'   trend_specs$type or trend_specs$growth)
#' @param prior A brmsprior object containing custom prior specifications for
#'   PW trend parameters (k_trend, m_trend, delta_trend, sigma_trend). If NULL,
#'   uses defaults from trend registry. Default NULL.
#'
#' @return Combined stanvars object containing Stan code for:
#'   \itemize{
#'     \item Common trend data (dimensions and indices)
#'     \item Changepoint specifications (locations and scales)
#'     \item Growth rate parameters (k_trend for baseline growth)
#'     \item Offset parameters (m_trend for trend baseline)
#'     \item Changepoint effects (delta_trend for rate changes)
#'     \item Innovation variance (sigma_trend if applicable)
#'     \item Trend computation in transformed parameters
#'   }
#'
#' PW trends implement Prophet-style piecewise trend modeling:
#' \enumerate{
#'   \item Linear growth: piecewise linear trends with rate changes at changepoints
#'   \item Logistic growth: piecewise logistic with carrying capacity constraints
#'   \item Changepoints: automatic placement with specified number and scale
#'   \item No factor models: each series requires individual changepoint structure
#'   \item Prophet compatibility: follows Prophet parameter naming and formulation
#' }
#'
#' The _trend suffix convention applies to all PW parameters for consistency
#' with other trend types in mvgam.
#'
#' @examples
#' # Linear piecewise trend
#' trend_specs <- list(n_changepoints = 5, changepoint_scale = 0.1, type = "linear")
#' data_info <- list(n_obs = 100, n_series = 2, n_time = 100)
#' stanvars <- generate_pw_trend_stanvars(trend_specs, data_info)
#'
#' # Logistic piecewise with custom priors
#' priors <- get_prior(y ~ x, trend_formula = ~ PW(n_changepoints = 10), data = dat)
#' stanvars <- generate_pw_trend_stanvars(trend_specs, data_info,
#'                                        growth = "logistic", prior = priors)
#'
#' # Override growth pattern
#' stanvars <- generate_pw_trend_stanvars(trend_specs, data_info, growth = "linear")
#'
#' @noRd
generate_pw_trend_stanvars <- function(trend_specs, data_info, growth = NULL,
                                       prior = NULL) {
  # Input validation
  checkmate::assert_list(trend_specs, names = "named")
  checkmate::assert_list(data_info, names = "named")
  checkmate::assert_string(growth, null.ok = TRUE)
  checkmate::assert_class(prior, "brmsprior", null.ok = TRUE)

  # Extract key parameters
  n_lv <- trend_specs$n_lv %||% 1
  n_changepoints <- trend_specs$n_changepoints %||% 5
  changepoint_scale <- trend_specs$changepoint_scale %||% 0.1
  # Use growth parameter if provided, otherwise fall back to trend_specs$type or trend_specs$growth
  trend_type <- growth %||% trend_specs$type %||% trend_specs$growth %||% "linear"
  n_obs <- data_info$n_obs
  n_series <- data_info$n_series %||% 1

  # Validate dimensions and trend type
  checkmate::assert_int(n_obs, lower = 1)
  checkmate::assert_int(n_series, lower = 1)
  checkmate::assert_int(n_lv, lower = 1)

  if (!trend_type %in% c("linear", "logistic")) {
    stop(insight::format_error(
      "Piecewise trend type must be 'linear' or 'logistic'.",
      paste0("Got type = '", trend_type, "'."),
      "Use type = 'linear' or type = 'logistic'."
    ))
  }

  # PW trends do not support factor models (series-specific changepoints required)
  is_factor_model <- FALSE

  # Build components list following the 3-stanvar pattern
  components <- list()

  # STEP 1: Dimensions handled by calling context (no duplication)

  # STEP 2: Always add matrix Z (PW uses diagonal Z in transformed data)
  matrix_z <- generate_matrix_z_multiblock_stanvars(is_factor_model, n_lv, n_series)
  components <- append_if_not_null(components, matrix_z)

  # Functions block - Prophet-style piecewise functions
  pw_functions_stanvar <- brms::stanvar(
    name = "pw_functions",
    scode = "
      matrix get_changepoint_matrix(vector t, vector t_change_trend, int T, int S) {
        /* Function to sort changepoints */

        /* credit goes to the Prophet development team at Meta (https://github.com/facebook/prophet/tree/main)*/
        matrix[T, S] Kappa;
        row_vector[S] a_row;
        int cp_idx;
        Kappa = rep_matrix(0, T, S);
        a_row = rep_row_vector(0, S);
        cp_idx = 1;
        for (i in 1 : T) {
          while ((cp_idx <= S) && (t[i] >= t_change_trend[cp_idx])) {
            a_row[cp_idx] = 1;
            cp_idx = cp_idx + 1;
          }
          Kappa[i] = a_row;
        }
        return Kappa;
      }

      vector logistic_gamma(real k, real m, vector delta, vector t_change_trend, int S) {
        /* Function to compute a logistic trend with changepoints */

        /* credit goes to the Prophet development team at Meta (https://github.com/facebook/prophet/tree/main)*/
        vector[S] gamma; // adjusted offsets, for piecewise continuity
        vector[S + 1] k_s; // actual rate in each segment
        real m_pr;
        k_s = append_row(k, k + cumulative_sum(delta));
        m_pr = m; // The offset in the previous segment
        for (i in 1 : S) {
          gamma[i] = (t_change_trend[i] - m_pr) * (1 - k_s[i] / k_s[i + 1]);
          m_pr = m_pr + gamma[i]; // update for the next segment
        }
        return gamma;
      }

      vector logistic_trend(real k, real m, vector delta, vector t, vector cap_trend,
                            matrix Kappa_trend, vector t_change_trend, int S) {
        /* Function to adjust a logistic trend using a carrying capacity */

        /* credit goes to the Prophet development team at Meta (https://github.com/facebook/prophet/tree/main)*/
        vector[S] gamma;
        gamma = logistic_gamma(k, m, delta, t_change_trend, S);
        return cap_trend .* inv_logit((k + Kappa_trend * delta) .* (t - (m + Kappa_trend * gamma)));
      }

      vector linear_trend(real k, real m, vector delta, vector t, matrix Kappa_trend,
                          vector t_change_trend) {
        /* Function to compute a linear trend with changepoints */

        /* credit goes to the Prophet development team at Meta (https://github.com/facebook/prophet/tree/main)*/
        return (k + Kappa_trend * delta) .* t + (m + Kappa_trend * (-t_change_trend .* delta));
      }
    ",
    block = "functions"
  )

  # Data block - piecewise-specific data components
  pw_data_stanvar <- brms::stanvar(
    x = list(
      n_change_trend = n_changepoints,
      t_change_trend = seq(0.1, 0.9, length.out = n_changepoints),  # Default changepoint times
      change_scale_trend = changepoint_scale
    ),
    name = "pw_data",
    scode = glue::glue("
      int<lower=0> n_change_trend; // number of potential trend changepoints
      vector[n_change_trend] t_change_trend; // times of potential changepoints
      real<lower=0> change_scale_trend; // scale of changepoint shock prior
    "),
    block = "data"
  )

  # Initialize logistic data variable
  pw_logistic_data_stanvar <- NULL

  # Logistic-specific data (carrying capacity)
  if (trend_type == "logistic") {
    pw_logistic_data_stanvar <- brms::stanvar(
      x = matrix(10, nrow = n_obs, ncol = n_series),  # Default carrying capacity
      name = "pw_logistic_data",
      scode = glue::glue("matrix[n_trend, n_lv_trend] cap_trend; // carrying capacities"),
      block = "data"
    )
  }

  # Transformed data block - time vector and changepoint matrix computation
  pw_transformed_data_stanvar <- brms::stanvar(
    name = "pw_transformed_data",
    scode = glue::glue("
      // time vector for changepoint calculations
      vector[n_trend] time_trend;
      for (i in 1:n_trend) time_trend[i] = i;
      // sorted changepoint matrix
      matrix[n_trend, n_change_trend] Kappa_trend = get_changepoint_matrix(time_trend, t_change_trend, n_trend, n_change_trend);
    "),
    block = "tdata"
  )

  # Parameters block - piecewise trend parameters
  pw_parameters_stanvar <- brms::stanvar(
    name = "pw_parameters",
    scode = glue::glue("
      // base trend growth rates
      vector[n_lv_trend] k_trend;

      // trend offset parameters
      vector[n_lv_trend] m_trend;

      // trend rate adjustments per series
      matrix[n_change_trend, n_lv_trend] delta_trend;
    "),
    block = "parameters"
  )

  # Transformed parameters block - trend computation (type-specific)
  if (trend_type == "logistic") {
    pw_transformed_parameters_stanvar <- brms::stanvar(
      name = "pw_transformed_parameters",
      scode = glue::glue("
        // Standardized latent trend matrix (logistic piecewise trends)
        matrix[n_trend, n_lv_trend] lv_trend;

        // logistic trend estimates
        for (s in 1 : n_lv_trend) {{
          lv_trend[1 : n_trend, s] = logistic_trend(k_trend[s], m_trend[s],
                                        to_vector(delta_trend[ : , s]), time_trend,
                                        to_vector(cap_trend[ : , s]), Kappa_trend, t_change_trend,
                                        n_change_trend);
        }}
      "),
      block = "tparameters"
    )
  } else {
    # Linear type
    pw_transformed_parameters_stanvar <- brms::stanvar(
      name = "pw_transformed_parameters",
      scode = glue::glue("
        // Standardized latent trend matrix (linear piecewise trends)
        matrix[n_trend, n_lv_trend] lv_trend;

        // linear trend estimates
        for (s in 1 : n_lv_trend) {{
          lv_trend[1 : n_trend, s] = linear_trend(k_trend[s], m_trend[s],
                                      to_vector(delta_trend[ : , s]), time_trend, Kappa_trend,
                                      t_change_trend);
        }}
      "),
      block = "tparameters"
    )
  }

  # Add trend computation stanvars
  trend_computation <- generate_trend_computation_tparameters(n_lv, n_series)

  # PW trend priors - always generate defaults if no custom priors
  # PW has its own default priors that should always be included
  pw_model_stanvar <- brms::stanvar(
    name = "pw_model",
    scode = glue::glue("
      // PW trend default priors
      m_trend ~ student_t(3, 0, 2.5);
      k_trend ~ std_normal();
      to_vector(delta_trend) ~ double_exponential(0, {changepoint_scale});
    "),
    block = "model"
  )

  # Also check for custom priors if provided
  pw_custom_priors <- generate_trend_priors_stanvar(
    param_names = c("m_trend", "k_trend", "delta_trend"),
    prior = prior,
    stanvar_name = "pw_custom_priors"
  )

  # Add all required components to the list
  components <- append(components, list(
    pw_functions_stanvar,
    pw_data_stanvar,
    pw_transformed_data_stanvar,
    pw_parameters_stanvar,
    pw_transformed_parameters_stanvar,
    trend_computation,
    pw_model_stanvar  # Always include default priors
  ))

  # Add custom priors if specified (will override defaults)
  if (!is.null(pw_custom_priors)) {
    components <- append(components, list(pw_custom_priors))
  }

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

#' Extract and Rename brms Trend Parameters
#'
#' Extracts parameters and data from brms trend_setup, renames them with _trend suffix,
#' and creates times_trend matrix for integration with mvgam shared innovation system.
#' Supports both univariate and multivariate models with response-specific handling.
#' Maintains bidirectional parameter mapping for prediction compatibility.
#'
#' @param trend_setup List from setup_brms_lightweight containing stancode, standata, and brmsfit
#' @param dimensions List from extract_time_series_dimensions with time series structure
#' @param suffix Character suffix to append to parameter names (default "_trend")
#' @return List of stanvar objects with renamed parameters, times_trend matrix, and mapping
#' @noRd
extract_and_rename_trend_parameters <- function(trend_setup, dimensions, suffix = "_trend") {
  checkmate::assert_list(trend_setup, names = "named")
  checkmate::assert_subset(c("stancode", "standata", "brmsfit"), names(trend_setup))
  checkmate::assert_list(dimensions, names = "named")
  checkmate::assert_string(suffix, pattern = "^_[a-zA-Z][a-zA-Z0-9_]*$")

  # Extract dimensions for times_trend matrix
  n_time <- dimensions$n_time
  n_series <- dimensions$n_series
  unique_times <- dimensions$unique_times
  unique_series <- dimensions$unique_series

  # Detect if this is a multivariate model
  brmsfit <- trend_setup$brmsfit
  is_multivariate <- is_multivariate_brmsfit(brmsfit)
  response_names <- if (is_multivariate) extract_response_names_from_brmsfit(brmsfit) else NULL

  # Create parameter mapping for prediction compatibility
  parameter_mapping <- list(
    original_to_renamed = list(),
    renamed_to_original = list(),
    is_multivariate = is_multivariate,
    response_names = response_names
  )

  # 1. Extract and rename Stan code blocks (extract from model block but exclude likelihood)
  stan_code_result <- extract_and_rename_stan_blocks(
    stancode = trend_setup$stancode,
    suffix = suffix,
    mapping = parameter_mapping,
    is_multivariate = is_multivariate,
    response_names = response_names,
    standata = trend_setup$standata
  )

  # Update mapping with results from Stan code renaming
  parameter_mapping <- stan_code_result$mapping

  # 2. Extract and rename standata objects
  standata_stanvars <- extract_and_rename_standata_objects(
    standata = trend_setup$standata,
    suffix = suffix,
    mapping = parameter_mapping,
    is_multivariate = is_multivariate,
    response_names = response_names
  )

  # 3. Generate times_trend matrix (support both univariate and multivariate)
  times_trend_stanvars <- generate_times_trend_matrices(
    n_time = n_time,
    n_series = n_series,
    unique_times = unique_times,
    unique_series = unique_series,
    is_multivariate = is_multivariate,
    response_names = response_names
  )

  # Combine all stanvar components using combine_stanvars for proper class inheritance
  combined_stanvars <- combine_stanvars(
    stan_code_result$stanvars,
    standata_stanvars,
    times_trend_stanvars
  )
  
  # Attach metadata for later use in predictions and integration (only if not NULL)
  if (!is.null(combined_stanvars)) {
    attr(combined_stanvars, "mvgam_parameter_mapping") <- parameter_mapping
    attr(combined_stanvars, "mvgam_original_brmsfit") <- brmsfit
    attr(combined_stanvars, "mvgam_is_multivariate") <- is_multivariate
    attr(combined_stanvars, "mvgam_response_names") <- response_names
  }

  return(combined_stanvars)
}

#' Check if brmsfit is Multivariate
#' @param brmsfit brms model fit object
#' @return Logical indicating if model is multivariate
#' @noRd
is_multivariate_brmsfit <- function(brmsfit) {
  checkmate::assert_class(brmsfit, "brmsfit")

  # Check if formula has multivariate structure
  if (!is.null(brmsfit$formula) && inherits(brmsfit$formula, "mvbrmsformula")) {
    return(TRUE)
  }

  # Check brmsterms for multivariate structure
  if (!is.null(brmsfit$formula)) {
    terms_obj <- try(brms::brmsterms(brmsfit$formula), silent = TRUE)
    if (!inherits(terms_obj, "try-error") && inherits(terms_obj, "mvbrmsterms")) {
      return(TRUE)
    }
  }

  return(FALSE)
}

#' Extract Response Names from brmsfit
#' @param brmsfit brms model fit object
#' @return Character vector of response names or NULL
#' @noRd
extract_response_names_from_brmsfit <- function(brmsfit) {
  checkmate::assert_class(brmsfit, "brmsfit")

  if (!is.null(brmsfit$formula) && inherits(brmsfit$formula, "mvbrmsformula")) {
    # Extract from mvbrmsformula
    return(names(brmsfit$formula$forms))
  }

  # Try extracting from brmsterms
  if (!is.null(brmsfit$formula)) {
    terms_obj <- try(brms::brmsterms(brmsfit$formula), silent = TRUE)
    if (!inherits(terms_obj, "try-error") && inherits(terms_obj, "mvbrmsterms")) {
      return(terms_obj$responses)
    }
  }

  return(NULL)
}

#' Extract and Rename Stan Code Blocks
#'
#' Extracts data, parameters, transformed parameters, and model blocks from brms Stan code,
#' renames parameters with suffix. For model block, extracts priors and transformations
#' but excludes likelihood statements.
#'
#' @param stancode Character string of complete Stan model code
#' @param suffix Character suffix to append to parameter names
#' @param mapping List to store parameter name mappings
#' @param is_multivariate Logical indicating if model is multivariate
#' @param response_names Character vector of response names (NULL for univariate)
#' @param standata Named list of Stan data objects for creating data block stanvars
#' @return List of stanvar objects with renamed Stan code blocks
#' @noRd
extract_and_rename_stan_blocks <- function(stancode, suffix, mapping, is_multivariate, response_names, standata = NULL) {
  checkmate::assert_string(stancode)
  checkmate::assert_string(suffix)
  checkmate::assert_list(mapping)
  checkmate::assert_flag(is_multivariate)
  checkmate::assert_character(response_names, null.ok = TRUE)
  checkmate::assert_list(standata, names = "named", null.ok = TRUE)

  stanvar_list <- list()

  # Extract data block declarations from Stan code with selective filtering
  # This complements extract_and_rename_standata_objects by providing proper declarations
  
  # 0. Extract data block content with selective filtering and renaming
  data_block <- extract_stan_block_content(stancode, "data")
  if (!is.null(data_block) && nchar(data_block) > 0) {
    data_result <- rename_parameters_in_block(
      data_block, suffix, mapping, "data", is_multivariate, response_names
    )
    mapping <- data_result$mapping  # Update mapping
    
    # Store declaration information for use by extract_and_rename_standata_objects
    if (!is.null(data_result$code) && nchar(trimws(data_result$code)) > 0) {
      mapping$data_declarations <- data_result$code
    }
  }

  # 1. Extract parameters block content (without headers) and rename parameters
  params_block <- extract_stan_block_content(stancode, "parameters")
  if (!is.null(params_block) && nchar(params_block) > 0) {
    # FILTER OUT DUPLICATE DECLARATIONS BEFORE RENAMING
    filtered_params <- filter_block_content(params_block, "parameters")
    
    if (!is.null(filtered_params) && nchar(filtered_params) > 0) {
      params_result <- rename_parameters_in_block(
        filtered_params, suffix, mapping, "parameters", is_multivariate, response_names
      )
      mapping <- params_result$mapping  # Update mapping

      stanvar_list[["trend_parameters"]] <- brms::stanvar(
        scode = params_result$code,
        block = "parameters"
      )
    }
  }

  # 2. Extract transformed parameters block content (without headers) and rename references
  tparams_block <- extract_stan_block_content(stancode, "transformed parameters")
  if (!is.null(tparams_block) && nchar(tparams_block) > 0) {
    # FILTER OUT DUPLICATE DECLARATIONS BEFORE RENAMING
    filtered_tparams <- filter_block_content(tparams_block, "transformed_parameters")
    
    if (!is.null(filtered_tparams) && nchar(filtered_tparams) > 0) {
      tparams_result <- rename_parameters_in_block(
        filtered_tparams, suffix, mapping, "tparameters", is_multivariate, response_names
      )
      mapping <- tparams_result$mapping  # Update mapping

      stanvar_list[["trend_tparameters"]] <- brms::stanvar(
        scode = tparams_result$code,
        block = "tparameters"
      )
    }
  }

  # 3. Extract model block content (without headers) but exclude likelihood statements
  model_block <- extract_stan_block_content(stancode, "model")
  model_stanvar_created <- FALSE

  if (!is.null(model_block) && nchar(model_block) > 0) {
    # Extract non-likelihood parts (priors, transformations)
    non_likelihood_model <- extract_non_likelihood_from_model_block(model_block)

    if (!is.null(non_likelihood_model) && nchar(trimws(non_likelihood_model)) > 0) {
      model_result <- rename_parameters_in_block(
        non_likelihood_model, suffix, mapping, "model", is_multivariate, response_names
      )
      mapping <- model_result$mapping  # Update mapping

      stanvar_list[["trend_model_priors"]] <- brms::stanvar(
        scode = model_result$code,
        block = "model"
      )
      model_stanvar_created <- TRUE
    }
  }

  # 4. CRITICAL: Create mu_trend if needed for trend computation
  # Create when no model stanvar was created (no extracted priors/transformations from model block)
  if (!model_stanvar_created) {
    # Always create mu_trend for trend computation - it's needed by generate_trend_computation_tparameters
    # Check if this is an intercept-only model (common pattern in brms trend models)
    has_coefficients <- grepl(paste0("vector\\[.*\\]\\s+b", suffix), stancode) && 
                        grepl(paste0("matrix\\[.*\\]\\s+Xc", suffix), stancode)
    
    if (has_coefficients) {
      # Model with coefficients: create from linear predictor components
      n_param <- paste0("N", suffix)  # Will be N_trend
      mu_trend_code <- paste0(
        "vector[", n_param, "] mu", suffix, " = Xc", suffix, " * b", suffix, " + Intercept", suffix, ";"
      )
    } else {
      # Intercept-only model: create from intercept (most common case for trend models)
      # Transform brms pattern: vector[N] mu = rep_vector(0.0, N); mu += Intercept;
      # Into: vector[n_trend] mu_trend = rep_vector(Intercept_trend, n_trend);
      time_param <- "n_trend"
      mu_trend_code <- paste0(
        "vector[", time_param, "] mu", suffix, " = rep_vector(Intercept", suffix, ", ", time_param, ");"
      )
    }

    # Update parameter mapping for the created mu_trend
    mapping$original_to_renamed[["mu"]] <- paste0("mu", suffix)
    mapping$renamed_to_original[[paste0("mu", suffix)]] <- "mu"

    stanvar_list[["trend_model_mu_creation"]] <- brms::stanvar(
      scode = mu_trend_code,
      block = "tparameters"
    )
    model_stanvar_created <- TRUE
  }

  # Combine individual stanvar objects into proper stanvars collection
  combined_stanvars <- combine_stanvars(stanvar_list)
  
  return(list(
    stanvars = combined_stanvars,
    mapping = mapping
  ))
}

#' Extract Non-Likelihood Parts from Model Block
#'
#' Extracts priors and transformations from model block while excluding likelihood statements
#'
#' @param model_block Character string of model block code
#' @return Character string with non-likelihood code or NULL
#' @noRd
#' Filter Block Content
#'
#' Generic function to filter out unwanted patterns from any Stan block content.
#' Removes duplicate declarations, likelihood statements, and other patterns that
#' should not be duplicated between observation and trend models.
#'
#' @param block_content Character string of block content (without block headers)
#' @param block_type Character string indicating block type ("model", "transformed_parameters", etc.)
#' @return Filtered block content or NULL if nothing remains
#' @noRd
filter_block_content <- function(block_content, block_type = "model") {
  checkmate::assert_string(block_content, min.chars = 1)
  checkmate::assert_string(block_type)
  
  # Handle both abbreviated and full block type names
  block_type <- switch(block_type,
    "tparameters" = "transformed_parameters",
    block_type
  )
  checkmate::assert_choice(block_type, 
    choices = c("model", "transformed_parameters", "data", "parameters"))
  
  # Split into lines using base R
  lines <- strsplit(block_content, "\n", fixed = TRUE)[[1]]
  lines <- trimws(lines)

  # Keep filtered lines
  filtered_lines <- character(0)
  prev_line_was_sigma_prior <- FALSE

  for (line in lines) {
    # Skip empty lines first (optimization)
    if (nchar(line) == 0) {
      next
    }
    
    # Skip comments
    if (grepl("^\\s*//", line)) {
      next
    }

    # Check if this line is the continuation of sigma prior (lccdf part)
    is_sigma_lccdf <- grepl("^\\s*-\\s*1\\s*\\*\\s*student_t_lccdf\\s*\\(\\s*0\\s*\\|", line)
    
    # Skip specific lines we don't want (applies to all block types)
    skip_line <- any(c(
      # Prior-only conditional statements (but keep their contents)
      grepl("if\\s*\\(\\s*!\\s*prior_only\\s*\\)\\s*\\{?\\s*$", line),
      grepl("if\\s*\\(\\s*prior_only\\s*\\)\\s*\\{?\\s*$", line),

      # Standalone closing braces (likely end of prior_only blocks)
      grepl("^\\s*}\\s*$", line),

      # CRITICAL: lprior declarations and sigma priors (avoid duplication with observation model)
      grepl("^\\s*real\\s+lprior\\s*=\\s*0\\s*;", line),
      grepl("lprior\\s*\\+=\\s*student_t_lpdf\\s*\\(\\s*sigma\\s*\\|", line),
      
      # Skip lccdf line only if it follows sigma prior
      (is_sigma_lccdf && prev_line_was_sigma_prior)
    ))
    
    # Additional filtering for parameters and transformed parameters blocks
    if (block_type %in% c("parameters", "transformed_parameters")) {
      skip_line <- skip_line || any(c(
        # Skip duplicate sigma parameter declarations to prevent "Identifier 'sigma' is already in use" errors
        grepl("^\\s*real\\s*<[^>]*lower\\s*=\\s*0[^>]*>\\s+sigma\\s*;", line),
        grepl("^\\s*real<lower=0>\\s+sigma\\s*;", line),
        grepl("^\\s*real\\s+sigma\\s*;", line)
      ))
    }
    
    # Additional filtering for model block only
    if (block_type == "model") {
      skip_line <- skip_line || any(c(
        # Actual likelihood statements
        grepl("~\\s+normal\\s*\\(", line),
        grepl("target\\s*\\+=.*normal.*lpdf\\s*\\(", line),
        grepl("target\\s*\\+=.*normal.*glm.*lpdf\\s*\\(", line),
        grepl("target\\s*\\+=.*multi_normal.*lpdf\\s*\\(", line),
        grepl("target\\s*\\+=.*normal.*lpdf\\s*\\([^)]*\\|", line),
        grepl("target\\s*\\+=.*Y\\s*\\|", line)
      ))
    }

    # Track if this line was a sigma prior for next iteration
    prev_line_was_sigma_prior <- grepl("lprior\\s*\\+=\\s*student_t_lpdf\\s*\\(\\s*sigma\\s*\\|", line)

    if (!skip_line) {
      filtered_lines <- c(filtered_lines, line)
    }
  }

  if (length(filtered_lines) == 0) {
    return(NULL)
  }

  return(paste(filtered_lines, collapse = "\n"))
}

extract_non_likelihood_from_model_block <- function(model_block) {
  checkmate::assert_string(model_block)

  # Remove the "model {" and "}" wrapper using base R
  block_match <- regmatches(model_block, regexpr("\\{.*\\}", model_block))
  if (length(block_match) == 0) return(NULL)
  
  block_content <- gsub("^\\{|\\}$", "", block_match)
  
  # Use general filtering function for model blocks
  return(filter_block_content(block_content, "model"))
}

#' Extract Stan Code Block
#'
#' Extracts a specific block from Stan code (data, parameters, etc.)
#'
#' @param stancode Character string of Stan code
#' @param block_name Name of block to extract
#' @return Character string of extracted block or "Block not found"
#' @noRd
extract_stan_block <- function(stancode, block_name) {
  checkmate::assert_string(stancode)
  checkmate::assert_string(block_name)

  # Pattern to find the block (case insensitive)
  pattern <- paste0("(?i)", stringr::str_replace_all(block_name, " ", "\\\\s+"), "\\s*\\{")
  start_match <- regexpr(pattern, stancode, perl = TRUE)

  if (start_match == -1) {
    return("Block not found")
  }

  start_pos <- start_match[1]

  # Find the opening brace position
  code_from_start <- substr(stancode, start_pos, nchar(stancode))
  brace_pos <- regexpr("\\{", code_from_start)

  if (brace_pos == -1) {
    return("Block not found")
  }

  # Count braces to find matching closing brace
  brace_count <- 0
  end_pos <- start_pos + brace_pos[1] - 1

  for (i in (start_pos + brace_pos[1]):nchar(stancode)) {
    char <- substr(stancode, i, i)
    if (char == "{") {
      brace_count <- brace_count + 1
    } else if (char == "}") {
      brace_count <- brace_count - 1
      if (brace_count == 0) {
        end_pos <- i
        break
      }
    }
  }

  return(substr(stancode, start_pos, end_pos))
}

#' Extract Stan Block Inner Content
#'
#' @description
#' Extracts only the inner content of a Stan block (without block headers).
#' This is specifically designed for creating brms stanvars where the scode
#' should contain only raw declarations, not block headers.
#'
#' @param stancode Character string containing full Stan code
#' @param block_name Name of block to extract ("parameters", "data", etc.)
#' @return Character string with inner block content (trimmed)
#' @noRd
extract_stan_block_content <- function(stancode, block_name) {
  checkmate::assert_string(stancode, min.chars = 1)
  checkmate::assert_string(block_name, min.chars = 1)

  # Use line-by-line parsing with proper boundary detection for all blocks
  lines <- strsplit(stancode, "\n")[[1]]
  in_block <- FALSE
  content_lines <- c()
  
  # Create pattern to match block start (handle multi-word blocks like "transformed data")
  # Use base R gsub and trimws for robustness
  clean_block_name <- gsub("\\s+", "\\\\s+", trimws(block_name))
  block_pattern <- paste0("^\\s*", clean_block_name, "\\s*\\{")
  
  for (i in seq_along(lines)) {
    line <- lines[i]
    
    # Check for block start
    if (grepl(block_pattern, line, ignore.case = TRUE)) {
      in_block <- TRUE
      next  # Skip the opening brace line
    }
    
    # Check for block end (closing brace on its own line)
    if (in_block && grepl("^\\s*\\}\\s*$", line)) {
      break  # Stop at closing brace
    }
    
    # Collect content lines
    if (in_block) {
      content_lines <- c(content_lines, line)
    }
  }
  
  # Handle case where block wasn't found
  if (length(content_lines) == 0 && !in_block) {
    stop(insight::format_error(
      "Stan block {.field {block_name}} not found in code"
    ), call. = FALSE)
  }
  
  return(paste(content_lines, collapse = "\n"))
}

#' Rename Parameters in Stan Code Block
#'
#' Renames parameter references within a Stan code block using suffix.
#' Handles both univariate and multivariate parameter patterns.
#'
#' @param block_code Character string of Stan block code
#' @param suffix Character suffix to append
#' @param mapping List to store parameter mappings
#' @param block_type Type of block for context
#' @param is_multivariate Logical indicating if model is multivariate
#' @param response_names Character vector of response names (NULL for univariate)
#' @return Character string with renamed parameters
#' @noRd
rename_parameters_in_block <- function(block_code, suffix, mapping, block_type, is_multivariate, response_names) {
  checkmate::assert_string(block_code)
  checkmate::assert_string(suffix)
  checkmate::assert_list(mapping)
  checkmate::assert_string(block_type)
  checkmate::assert_flag(is_multivariate)
  checkmate::assert_character(response_names, null.ok = TRUE)

  renamed_code <- block_code

  # Since trend models are always univariate, use univariate renaming
  # Extract all identifiers from this block and apply comprehensive renaming
  all_identifiers <- extract_stan_identifiers(block_code)

  if (length(all_identifiers) > 0) {
    # Filter to get identifiers that should be renamed
    renameable_identifiers <- filter_renameable_identifiers(all_identifiers)

    # Sort by length (descending) to avoid partial replacements
    sorted_identifiers <- renameable_identifiers[order(-nchar(renameable_identifiers))]

    for (identifier in sorted_identifiers) {
      renamed_param <- paste0(identifier, suffix)

      # Store mapping for coordination between blocks
      mapping$original_to_renamed[[identifier]] <- renamed_param
      mapping$renamed_to_original[[renamed_param]] <- identifier

      # Apply replacement with word boundary protection
      renamed_code <- apply_safe_parameter_replacement(
        renamed_code,
        identifier,
        renamed_param
      )
    }
  }

  # Return both renamed code and updated mapping
  return(list(
    code = renamed_code,
    mapping = mapping
  ))
}

#' Get comprehensive Stan reserved words list
#'
#' Returns comprehensive list of Stan reserved words that should never be renamed
#' during parameter extraction. Based on Stan language specification and testing.
#'
#' @return Character vector of Stan reserved words
#' @noRd
get_stan_reserved_words <- function() {
  # Comprehensive Stan reserved words based on Stan language specification
  # and extensive testing documented in dev-tasks-stancode-generation-update.md
  c(
    # Stan data types
    "int", "real", "vector", "matrix", "array", "void",

    # Stan constraints and bounds
    "lower", "upper", "multiplier", "offset",

    # Stan block keywords
    "functions", "data", "transformed", "parameters", "model",
    "generated", "quantities",

    # Stan control flow
    "if", "else", "for", "while", "in", "break", "continue", "return",

    # Stan operators and special symbols
    "and", "or", "not",

    # Stan built-in variables and functions
    "target", "increment_log_prob", "get_lp", "print", "reject",
    "size", "length", "dimensions", "rows", "cols", "num_elements",

    # brms internal variables
    "prior_only", "lprior", "temp_intercept", "temp_Intercept",

    # Stan mathematical functions
    "abs", "acos", "acosh", "asin", "asinh", "atan", "atan2", "atanh",
    "cos", "cosh", "sin", "sinh", "tan", "tanh", "exp", "exp2", "expm1",
    "log", "log10", "log1p", "log2", "sqrt", "square", "pow", "inv",
    "inv_sqrt", "inv_square", "fabs", "fdim", "fmin", "fmax", "fmod",
    "floor", "ceil", "round", "trunc", "nearbyint", "rint",

    # Stan linear algebra functions
    "dot_product", "columns_dot_product", "rows_dot_product",
    "dot_self", "columns_dot_self", "rows_dot_self", "tcrossprod",
    "crossprod", "quad_form", "quad_form_diag", "quad_form_sym",
    "trace", "determinant", "log_determinant", "inverse", "eigenvalues",
    "eigenvectors", "cholesky_decompose", "singular_values",
    "qr_Q", "qr_R", "sort_asc", "sort_desc", "sort_indices_asc",
    "sort_indices_desc", "rank", "reverse",

    # Stan array/vector/matrix functions
    "rep_array", "rep_vector", "rep_row_vector", "rep_matrix",
    "ones_array", "ones_vector", "ones_row_vector", "ones_matrix",
    "zeros_array", "zeros_vector", "zeros_row_vector", "zeros_matrix",
    "uniform_simplex", "unit_vector", "linspaced_array", "linspaced_vector",
    "linspaced_row_vector", "one_hot_array", "one_hot_vector",
    "one_hot_row_vector", "identity_matrix", "diag_matrix",
    "diag_pre_multiply", "diag_post_multiply", "block", "sub_col",
    "sub_row", "head", "tail", "segment", "append_array", "append_col",
    "append_row",

    # Stan probability distributions (lpdf/lpmf)
    "normal_lpdf", "normal_id_glm_lpdf", "student_t_lpdf", "cauchy_lpdf",
    "double_exponential_lpdf", "logistic_lpdf", "gumbel_lpdf",
    "lognormal_lpdf", "chi_square_lpdf", "inv_chi_square_lpdf",
    "scaled_inv_chi_square_lpdf", "exponential_lpdf", "gamma_lpdf",
    "inv_gamma_lpdf", "weibull_lpdf", "frechet_lpdf", "rayleigh_lpdf",
    "wiener_lpdf", "pareto_lpdf", "pareto_type_2_lpdf", "beta_lpdf",
    "beta_proportion_lpdf", "uniform_lpdf", "bernoulli_lpdf",
    "bernoulli_logit_lpdf", "binomial_lpdf", "binomial_logit_lpdf",
    "beta_binomial_lpdf", "hypergeometric_lpdf", "categorical_lpdf",
    "categorical_logit_lpdf", "discrete_range_lpdf", "ordered_logistic_lpdf",
    "ordered_probit_lpdf", "poisson_lpdf", "poisson_log_lpdf",
    "neg_binomial_lpdf", "neg_binomial_2_lpdf", "neg_binomial_2_log_lpdf",
    "multinomial_lpdf", "dirichlet_lpdf", "multi_normal_lpdf",
    "multi_normal_prec_lpdf", "multi_normal_cholesky_lpdf",
    "multi_student_t_lpdf", "gaussian_dlm_obs_lpdf", "wishart_lpdf",
    "inv_wishart_lpdf", "lkj_corr_lpdf", "lkj_corr_cholesky_lpdf",

    # Stan distribution CDFs
    "normal_cdf", "normal_ccdf", "normal_lcdf", "normal_lccdf",
    "student_t_cdf", "student_t_ccdf", "student_t_lcdf", "student_t_lccdf",
    "cauchy_cdf", "cauchy_ccdf", "cauchy_lcdf", "cauchy_lccdf",
    "double_exponential_cdf", "double_exponential_ccdf",
    "double_exponential_lcdf", "double_exponential_lccdf",
    "logistic_cdf", "logistic_ccdf", "logistic_lcdf", "logistic_lccdf",
    "gumbel_cdf", "gumbel_ccdf", "gumbel_lcdf", "gumbel_lccdf",
    "lognormal_cdf", "lognormal_ccdf", "lognormal_lcdf", "lognormal_lccdf",
    "chi_square_cdf", "chi_square_ccdf", "chi_square_lcdf", "chi_square_lccdf",
    "inv_chi_square_cdf", "inv_chi_square_ccdf", "inv_chi_square_lcdf",
    "inv_chi_square_lccdf", "scaled_inv_chi_square_cdf",
    "scaled_inv_chi_square_ccdf", "scaled_inv_chi_square_lcdf",
    "scaled_inv_chi_square_lccdf", "exponential_cdf", "exponential_ccdf",
    "exponential_lcdf", "exponential_lccdf", "gamma_cdf", "gamma_ccdf",
    "gamma_lcdf", "gamma_lccdf", "inv_gamma_cdf", "inv_gamma_ccdf",
    "inv_gamma_lcdf", "inv_gamma_lccdf", "weibull_cdf", "weibull_ccdf",
    "weibull_lcdf", "weibull_lccdf", "frechet_cdf", "frechet_ccdf",
    "frechet_lcdf", "frechet_lccdf", "rayleigh_cdf", "rayleigh_ccdf",
    "rayleigh_lcdf", "rayleigh_lccdf", "pareto_cdf", "pareto_ccdf",
    "pareto_lcdf", "pareto_lccdf", "pareto_type_2_cdf", "pareto_type_2_ccdf",
    "pareto_type_2_lcdf", "pareto_type_2_lccdf", "beta_cdf", "beta_ccdf",
    "beta_lcdf", "beta_lccdf", "beta_proportion_cdf", "beta_proportion_ccdf",
    "beta_proportion_lcdf", "beta_proportion_lccdf", "uniform_cdf",
    "uniform_ccdf", "uniform_lcdf", "uniform_lccdf", "bernoulli_cdf",
    "bernoulli_ccdf", "bernoulli_lcdf", "bernoulli_lccdf",
    "bernoulli_logit_cdf", "bernoulli_logit_ccdf", "bernoulli_logit_lcdf",
    "bernoulli_logit_lccdf", "binomial_cdf", "binomial_ccdf", "binomial_lcdf",
    "binomial_lccdf", "binomial_logit_cdf", "binomial_logit_ccdf",
    "binomial_logit_lcdf", "binomial_logit_lccdf", "beta_binomial_cdf",
    "beta_binomial_ccdf", "beta_binomial_lcdf", "beta_binomial_lccdf",
    "hypergeometric_cdf", "hypergeometric_ccdf", "hypergeometric_lcdf",
    "hypergeometric_lccdf", "poisson_cdf", "poisson_ccdf", "poisson_lcdf",
    "poisson_lccdf", "poisson_log_cdf", "poisson_log_ccdf", "poisson_log_lcdf",
    "poisson_log_lccdf", "neg_binomial_cdf", "neg_binomial_ccdf",
    "neg_binomial_lcdf", "neg_binomial_lccdf", "neg_binomial_2_cdf",
    "neg_binomial_2_ccdf", "neg_binomial_2_lcdf", "neg_binomial_2_lccdf",
    "neg_binomial_2_log_cdf", "neg_binomial_2_log_ccdf",
    "neg_binomial_2_log_lcdf", "neg_binomial_2_log_lccdf",

    # Stan random number generators
    "normal_rng", "student_t_rng", "cauchy_rng", "double_exponential_rng",
    "logistic_rng", "gumbel_rng", "lognormal_rng", "chi_square_rng",
    "inv_chi_square_rng", "scaled_inv_chi_square_rng", "exponential_rng",
    "gamma_rng", "inv_gamma_rng", "weibull_rng", "frechet_rng",
    "rayleigh_rng", "wiener_rng", "pareto_rng", "pareto_type_2_rng",
    "beta_rng", "beta_proportion_rng", "uniform_rng", "bernoulli_rng",
    "bernoulli_logit_rng", "binomial_rng", "binomial_logit_rng",
    "beta_binomial_rng", "hypergeometric_rng", "categorical_rng",
    "categorical_logit_rng", "discrete_range_rng", "ordered_logistic_rng",
    "ordered_probit_rng", "poisson_rng", "poisson_log_rng",
    "neg_binomial_rng", "neg_binomial_2_rng", "neg_binomial_2_log_rng",
    "multinomial_rng", "dirichlet_rng", "multi_normal_rng",
    "multi_normal_prec_rng", "multi_normal_cholesky_rng",
    "multi_student_t_rng", "wishart_rng", "inv_wishart_rng",
    "lkj_corr_rng", "lkj_corr_cholesky_rng",

    # Special Stan functions
    "step", "is_inf", "is_nan", "positive_infinity", "negative_infinity",
    "not_a_number", "machine_precision", "max", "min", "sum", "prod",
    "log_sum_exp", "mean", "variance", "sd", "distance", "squared_distance",
    "fma", "multiply_log", "log1m", "log1p_exp", "log1m_exp", "log_diff_exp",
    "log_mix", "log_falling_factorial", "log_rising_factorial", "lgamma",
    "digamma", "trigamma", "lbeta", "binomial_coefficient_log", "bessel_first_kind",
    "bessel_second_kind", "modified_bessel_first_kind", "modified_bessel_second_kind",
    "falling_factorial", "rising_factorial", "expm1", "log1p", "hypot"
  )
}

#' Extract all identifiers from Stan code
#'
#' Uses regex to find all valid Stan identifiers for comprehensive parameter
#' renaming. Based on proven patterns from testing.
#'
#' @param stan_code Character string containing Stan code
#' @return Character vector of unique identifiers found in code
#' @noRd
extract_stan_identifiers <- function(stan_code) {
  checkmate::assert_character(stan_code, len = 1)

  # Extract all identifiers using regex pattern for valid Stan identifiers
  # Pattern matches: letter or underscore, followed by letters, digits, underscores
  identifiers <- regmatches(
    stan_code,
    gregexpr("\\b[a-zA-Z_][a-zA-Z0-9_]*\\b", stan_code, perl = TRUE)
  )[[1]]

  # Return unique identifiers, removing empty strings
  unique(identifiers[nchar(identifiers) > 0])
}

#' Filter identifiers to exclude Stan reserved words and response variables
#'
#' Removes Stan reserved words, essential system variables, and response variables
#' that should never be renamed during trend parameter extraction.
#'
#' @param identifiers Character vector of identifiers to filter
#' @return Character vector of identifiers that are safe to rename
#' @noRd
filter_renameable_identifiers <- function(identifiers) {
  checkmate::assert_character(identifiers)

  # Get comprehensive Stan reserved words
  reserved_words <- get_stan_reserved_words()

  # Filter out reserved words (case-sensitive comparison)
  non_reserved <- identifiers[!identifiers %in% reserved_words]

  # Additional filtering for response variables and obvious non-parameters
  # Response variables should NOT be carried from trend to observation model
  exclude_patterns <- c(
    # Response variables (trend models are always univariate with Y only)
    "Y",
    
    # Dimension variables handled by generate_common_trend_data (avoid duplication)
    "N",  # We use n_trend instead of N_trend
    
    # Global flags that should not be renamed
    "prior_only",

    # Innovation parameters handled by shared innovation system (avoid duplication)
    "sigma",  # Shared innovation system provides vector sigma_trend

    # Comment words and obvious non-parameters based on comprehensive testing
    "total", "number", "observations", "response", "variable",
    "population", "level", "effects", "design", "after", "centering",
    "should", "the", "likelihood", "be", "ignored", "group",
    "standard", "deviations", "standardized", "actual", "temporary",
    "intercept", "centered", "predictors", "regression", "coefficients",
    "dispersion", "parameter", "prior", "contributions", "log", "posterior"
  )

  # Return identifiers that are not reserved words, response variables, or comment words
  non_reserved[!non_reserved %in% exclude_patterns]
}

#' Apply parameter renaming with word boundary protection
#'
#' Performs safe identifier replacement using word boundaries to prevent
#' partial matches. Based on proven testing patterns.
#'
#' @param code Character string to modify
#' @param old_name Character string of identifier to rename
#' @param new_name Character string of new identifier name
#' @return Modified character string with renamed identifier
#' @noRd
apply_safe_parameter_replacement <- function(code, old_name, new_name) {
  checkmate::assert_character(code, len = 1)
  checkmate::assert_character(old_name, len = 1)
  checkmate::assert_character(new_name, len = 1)

  # Use word boundary regex to avoid partial matches
  # \b ensures we only match complete words
  pattern <- paste0("\\b", old_name, "\\b")
  gsub(pattern, new_name, code, perl = TRUE)
}

#' Rename Univariate Parameters using comprehensive identifier extraction
#'
#' Replaces hardcoded parameter patterns with comprehensive Stan reserved words
#' filtering to ensure robust parameter renaming for all brms patterns.
#' Based on extensive testing documented in dev-tasks file.
#'
#' @param code Character string of Stan code
#' @param suffix Character suffix to append
#' @param mapping List to store parameter mappings
#' @return Character string with renamed parameters
#' @noRd
rename_univariate_parameters <- function(code, suffix, mapping) {
  checkmate::assert_character(code, len = 1)
  checkmate::assert_character(suffix, len = 1)
  checkmate::assert_list(mapping)

  # Extract all identifiers from Stan code
  all_identifiers <- extract_stan_identifiers(code)

  if (length(all_identifiers) == 0) {
    return(code)
  }

  # Filter to get identifiers that should be renamed
  renameable_identifiers <- filter_renameable_identifiers(all_identifiers)

  # Apply renaming with word boundary protection
  renamed_code <- code

  # Sort by length (descending) to avoid partial replacements
  sorted_identifiers <- renameable_identifiers[order(-nchar(renameable_identifiers))]

  for (identifier in sorted_identifiers) {
    renamed_param <- paste0(identifier, suffix)

    # Store mapping for later use
    mapping$original_to_renamed[[identifier]] <- renamed_param
    mapping$renamed_to_original[[renamed_param]] <- identifier

    # Apply replacement with word boundary protection
    renamed_code <- apply_safe_parameter_replacement(
      renamed_code,
      identifier,
      renamed_param
    )
  }

  return(renamed_code)
}

#' Rename Multivariate Parameters
#' @param code Character string of Stan code
#' @param suffix Character suffix to append
#' @param mapping List to store parameter mappings
#' @param response_names Character vector of response names
#' @return Character string with renamed parameters
#' @noRd
rename_multivariate_parameters <- function(code, suffix, mapping, response_names) {
  renamed_code <- code

  # Multivariate parameter patterns for each response
  for (resp_name in response_names) {
    # Response-specific patterns
    mv_patterns <- c(
      paste0("\\bb_", resp_name, "\\b"),           # Fixed effects for response
      paste0("\\bmu_", resp_name, "\\b"),          # Linear predictor for response
      paste0("\\bsigma_", resp_name, "\\b"),       # Scale parameter for response
      paste0("\\bshape_", resp_name, "\\b"),       # Shape parameter for response
      paste0("\\bY_", resp_name, "\\b"),           # Response variable
      paste0("\\bX_", resp_name, "\\b"),           # Design matrix for response
      paste0("\\bN_", resp_name, "\\b"),           # Number of observations for response
      paste0("\\bK_", resp_name, "\\b")            # Number of predictors for response
    )

    # Apply renaming for this response
    for (param_pattern in mv_patterns) {
      # Find all matches
      matches <- gregexpr(param_pattern, renamed_code, perl = TRUE)
      match_strings <- regmatches(renamed_code, matches)[[1]]

      # Rename each unique match
      unique_matches <- unique(match_strings)
      for (param in unique_matches) {
        if (nchar(param) > 0) {
          renamed_param <- paste0(param, suffix)

          # Store mapping
          mapping$original_to_renamed[[param]] <- renamed_param
          mapping$renamed_to_original[[renamed_param]] <- param

          # Replace in code (use word boundaries)
          renamed_code <- gsub(paste0("\\b", param, "\\b"), renamed_param, renamed_code, perl = TRUE)
        }
      }
    }
  }

  return(renamed_code)
}

#' Extract and Rename Stan Data Objects
#'
#' Extracts data objects from brms standata and renames them with suffix.
#' Handles both univariate and multivariate data patterns.
#'
#' @param standata Named list of Stan data objects
#' @param suffix Character suffix to append to data names
#' @param mapping List to store parameter mappings
#' @param is_multivariate Logical indicating if model is multivariate
#' @param response_names Character vector of response names (NULL for univariate)
#' @return List of stanvar objects with renamed data
#' @noRd
extract_and_rename_standata_objects <- function(standata, suffix, mapping, is_multivariate, response_names) {
  checkmate::assert_list(standata, names = "named")
  checkmate::assert_string(suffix)
  checkmate::assert_list(mapping)
  checkmate::assert_flag(is_multivariate)
  checkmate::assert_character(response_names, null.ok = TRUE)

  stanvar_list <- list()

  if (is_multivariate && !is.null(response_names)) {
    # Handle multivariate data objects
    stanvar_list <- extract_multivariate_standata(standata, suffix, mapping, response_names)
  } else {
    # Handle univariate data objects
    stanvar_list <- extract_univariate_standata(standata, suffix, mapping)
  }

  # Combine individual stanvar objects into proper stanvars collection
  return(combine_stanvars(stanvar_list))
}

#' Extract Univariate Stan Data Objects using comprehensive filtering
#'
#' Applies comprehensive renaming to standata objects using same Stan reserved
#' words filtering as the code renaming to ensure consistent namespace separation.
#'
#' @param standata Named list of Stan data objects
#' @param suffix Character suffix to append
#' @param mapping List to store parameter mappings
#' @return List of stanvar objects with renamed data
#' @noRd
extract_univariate_standata <- function(standata, suffix, mapping) {
  checkmate::assert_list(standata, names = "named")
  checkmate::assert_character(suffix, len = 1)
  checkmate::assert_list(mapping, names = "named")
  
  # Validate mapping structure if data_declarations is present
  if (!is.null(mapping$data_declarations)) {
    checkmate::assert_character(mapping$data_declarations, len = 1)
  }

  stanvar_list <- list()

  # Get all standata names and filter using comprehensive approach
  all_data_names <- names(standata)
  renameable_data_names <- filter_renameable_identifiers(all_data_names)
  
  # Pre-parse declaration lines for efficiency
  declaration_lines <- NULL
  if (!is.null(mapping$data_declarations) && nzchar(mapping$data_declarations)) {
    declaration_lines <- strsplit(mapping$data_declarations, "\n")[[1]]
    declaration_lines <- trimws(declaration_lines)
    declaration_lines <- declaration_lines[nzchar(declaration_lines)]
  }

  for (data_name in renameable_data_names) {
    if (data_name %in% names(standata)) {
      renamed_data_name <- paste0(data_name, suffix)
      
      # Only include data objects that would be declared in the stancode
      # Check if the renamed version appears in declarations
      has_declaration <- FALSE
      if (!is.null(declaration_lines) && length(declaration_lines) > 0) {
        has_declaration <- any(grepl(paste0("\\b", renamed_data_name, "\\b"), declaration_lines))
      }
      
      # Skip data objects that have no declaration in stancode
      if (!has_declaration) {
        next
      }
      
      data_value <- standata[[data_name]]

      # Store mapping for coordination with Stan code renaming
      if (is.null(mapping$original_to_renamed)) mapping$original_to_renamed <- list()
      if (is.null(mapping$renamed_to_original)) mapping$renamed_to_original <- list()
      
      mapping$original_to_renamed[[data_name]] <- renamed_data_name
      mapping$renamed_to_original[[renamed_data_name]] <- data_name

      # Create stanvar for renamed data
      stanvar_args <- list(
        x = data_value,
        name = renamed_data_name,
        block = "data"
      )
      
      # Add declaration if available
      if (!is.null(declaration_lines)) {
        for (decl_line in declaration_lines) {
          if (grepl(paste0("\\b", renamed_data_name, "\\b"), decl_line)) {
            stanvar_args$scode <- paste0("  ", decl_line)
            break
          }
        }
      }
      
      stanvar_list[[renamed_data_name]] <- do.call(brms::stanvar, stanvar_args)
    }
  }

  return(combine_stanvars(stanvar_list))
}

#' Extract Multivariate Stan Data Objects
#' @param standata Named list of Stan data objects
#' @param suffix Character suffix to append
#' @param mapping List to store parameter mappings
#' @param response_names Character vector of response names
#' @return List of stanvar objects with renamed data
#' @noRd
extract_multivariate_standata <- function(standata, suffix, mapping, response_names) {
  stanvar_list <- list()

  # Multivariate data object patterns
  for (resp_name in response_names) {
    mv_data_patterns <- c(
      paste0("Y_", resp_name),     # Response variable
      paste0("X_", resp_name),     # Design matrix
      paste0("N_", resp_name),     # Number of observations
      paste0("K_", resp_name),     # Number of predictors
      paste0("Kc_", resp_name)     # Centered predictors
    )

    for (data_pattern in mv_data_patterns) {
      if (data_pattern %in% names(standata)) {
        renamed_data_name <- paste0(data_pattern, suffix)
        data_value <- standata[[data_pattern]]

        # Store mapping
        mapping$original_to_renamed[[data_pattern]] <- renamed_data_name
        mapping$renamed_to_original[[renamed_data_name]] <- data_pattern

        # Create stanvar for renamed data (use x for actual data values)
        stanvar_list[[renamed_data_name]] <- brms::stanvar(
          x = data_value,
          name = renamed_data_name,
          block = "data"
        )
      }
    }
  }

  return(combine_stanvars(stanvar_list))
}

#' Generate times_trend Matrices
#'
#' Creates time series indexing matrices for both univariate and multivariate models
#'
#' @param n_time Number of unique time points
#' @param n_series Number of series
#' @param unique_times Sorted vector of unique time points
#' @param unique_series Sorted vector of unique series identifiers
#' @param is_multivariate Logical indicating if model is multivariate
#' @param response_names Character vector of response names (NULL for univariate)
#' @return List of stanvar objects with times_trend matrices
#' @noRd
generate_times_trend_matrices <- function(n_time, n_series, unique_times, unique_series, is_multivariate, response_names) {
  checkmate::assert_integerish(n_time, lower = 1, len = 1)
  checkmate::assert_integerish(n_series, lower = 1, len = 1)
  checkmate::assert_vector(unique_times, len = n_time)
  checkmate::assert_vector(unique_series, len = n_series)
  checkmate::assert_flag(is_multivariate)
  checkmate::assert_character(response_names, null.ok = TRUE)

  stanvar_list <- list()

  # Create times_trend matrix using sorted dimension information from extract_time_series_dimensions()
  # Both univariate and multivariate use same structure since trend models are always univariate
  times_trend_stanvar <- create_times_trend_matrix(
    n_time, n_series, unique_times, unique_series, "times_trend"
  )
  stanvar_list[["times_trend"]] <- times_trend_stanvar

  return(combine_stanvars(stanvar_list))
}

#' Create Single times_trend Matrix
#' @param n_time Number of time points
#' @param n_series Number of series
#' @param unique_times Sorted unique time points
#' @param unique_series Sorted unique series identifiers
#' @param matrix_name Name for the matrix
#' @return Stanvar object with times_trend matrix
#' @noRd
create_times_trend_matrix <- function(n_time, n_series, unique_times, unique_series, matrix_name) {
  # Create properly structured 2D integer array [n_time, n_series]
  # Each entry times_array[i, j] contains the time index for time i, series j
  times_array <- array(data = as.integer(0), dim = c(n_time, n_series))

  # Fill array with time indices (proper 2D structure for Stan)
  for (i in seq_len(n_time)) {
    for (j in seq_len(n_series)) {
      times_array[i, j] <- as.integer(i)  # Time index for this time point
    }
  }

  # Create stanvar with explicit 2D integer array declaration
  # Use both data (x) and explicit scode to override brms auto-generation
  stan_declaration <- glue::glue("int {matrix_name}[{n_time}, {n_series}];")

  brms::stanvar(
    x = times_array,
    name = matrix_name,
    scode = stan_declaration,
    block = "data"
  )
}

#' Generate Stan Array Declaration
#'
#' Creates properly formatted Stan array declaration with data
#'
#' @param var_name Variable name
#' @param var_type Stan variable type (int, real, etc.)
#' @param dimensions Vector of array dimensions
#' @param data_matrix Matrix or array of data values
#' @return Character string with Stan declaration
#' @noRd
generate_stan_array_declaration <- function(var_name, var_type, dimensions, data_matrix) {
  checkmate::assert_string(var_name)
  checkmate::assert_string(var_type)
  checkmate::assert_integerish(dimensions, lower = 1)
  checkmate::assert_matrix(data_matrix)

  # Create dimension string
  dim_str <- paste(dimensions, collapse = ", ")

  # Format data matrix for Stan
  if (length(dimensions) == 2) {
    # 2D array
    formatted_data <- format_matrix_for_stan_array(data_matrix)
  } else {
    stop("Only 2D arrays currently supported")
  }

  # Create Stan declaration
  paste0(var_type, " ", var_name, "[", dim_str, "] = ", formatted_data, ";")
}

#' Format Matrix for Stan Array
#'
#' Converts R matrix to Stan array format
#'
#' @param matrix Matrix to format
#' @return Character string in Stan array format
#' @noRd
format_matrix_for_stan_array <- function(matrix) {
  checkmate::assert_matrix(matrix)

  # Convert each row to Stan format
  row_strings <- apply(matrix, 1, function(row) {
    paste0("{", paste(row, collapse = ", "), "}")
  })

  # Combine rows
  paste0("{", paste(row_strings, collapse = ", "), "}")
}
