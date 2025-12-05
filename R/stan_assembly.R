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
    "trend(?!_)",  # The computed trend matrix should be response-specific (negative lookahead to avoid matching *_trend)
    "mu_ones" # GLM compatibility stanvar should be response-specific
    # Note: N_trend deliberately excluded - it should remain shared across responses
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
    # Check if pattern already contains regex special characters (lookahead/lookbehind)
    if (grepl("\\?[=!]|\\(\\?", pattern)) {
      # Pattern already contains lookahead/lookbehind, use as-is with word boundary only at start
      regex_pattern <- paste0("\\b", pattern)
      # For negative lookahead patterns, extract the base pattern for replacement
      base_pattern <- gsub("\\(\\?[^)]*\\)", "", pattern)
      replacement <- paste0(base_pattern, suffix)
    } else {
      # Standard pattern, add word boundaries on both sides
      regex_pattern <- paste0("\\b", pattern, "\\b")
      replacement <- paste0(pattern, suffix)
    }

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
    # Validate multivariate trend_specs structure
    checkmate::assert_list(trend_specs, names = "named")

    # Extract response names from the trend_specs
    response_names <- names(trend_specs)

    # Helper function to detect shared trends across responses
    # Shared trends occur when parse_multivariate_trends replicates identical
    # trend specifications for multiple responses, causing duplicate stanvars
    detect_shared_trends <- function(specs) {
      # Get non-NULL trend specifications for comparison
      non_null_specs <- specs[!sapply(specs, is.null)]
      if (length(non_null_specs) <= 1) return(FALSE)

      # Use identical() for robust deep comparison of trend objects
      first_spec <- non_null_specs[[1]]
      all(sapply(non_null_specs[-1], function(x) {
        identical(x, first_spec, ignore.environment = TRUE)
      }))
    }

    # Check if all responses share identical trend specifications
    is_shared_trend <- detect_shared_trends(trend_specs)

    if (is_shared_trend) {
      # Shared trend case: generate stanvars only once to avoid duplicates
      # This handles cases like trend_formula = ~ RW() applied to multiple responses
      first_response <- names(trend_specs)[!sapply(trend_specs, is.null)][1]
      shared_trend_spec <- trend_specs[[first_response]]

      # Track all responses as having the shared trend
      responses_with_trends <- names(trend_specs)[!sapply(trend_specs, is.null)]

      # Generate shared trend stanvars without response suffix
      trend_stanvars <- extract_trend_stanvars_from_setup(
        trend_setup,
        shared_trend_spec,
        response_suffix = "",  # No suffix for shared trends
        response_name = NULL,  # Shared across all responses
        obs_setup = obs_setup,
        prior = prior
      )
    } else {
      # Response-specific trends: generate stanvars for each response separately
      # This handles cases like list(count = ~ AR(), biomass = ~ RW())
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
          obs_setup = obs_setup,
          prior = prior
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
      obs_setup = obs_setup,
      prior = prior
    )
    responses_with_trends <- "main"  # Mark univariate model
  }

  # Reorder trend stanvars to ensure proper variable declaration order
  trend_stanvars <- sort_stanvars(trend_stanvars)

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

  # Deduplicate functions (GP models may have identical functions in both models)
  combined_stancode <- deduplicate_stan_functions(combined_stancode)

  # Generate complete standata using brms with trend stanvars included
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

    # Sort ALL stanvars to ensure proper dependency order
    # This ensures mu injection code comes after trend computation
    all_stanvars <- sort_stanvars(all_stanvars)
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
                                              obs_setup = NULL,
                                              prior = NULL) {
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
    # Create basic data_info first
    data_info <- list(
      n_obs = n_obs,
      n_series = dimensions$n_series,
      n_lv = trend_specs$n_lv,
      n_time = n_time,
      time_var = dimensions$time_var,
      series_var = dimensions$series_var,
      unique_times = dimensions$unique_times,
      unique_series = dimensions$unique_series,
      data = obs_setup$data %||% trend_setup$data  # Use original data for hierarchical grouping and CAR calculations
    )

    # Compute hierarchical parameters if grouping specified
    hierarchical_info <- extract_hierarchical_info(data_info, trend_specs)

    # Add computed values to data_info
    data_info$n_groups <- hierarchical_info$n_groups %||% NULL
    data_info$n_subgroups <- hierarchical_info$n_subgroups %||% NULL

    # Extract brms parameters (handles mu_trend creation/extraction) + generate trend stanvars
    brms_stanvars <- extract_and_rename_trend_parameters(
      trend_setup = trend_setup,
      dimensions = dimensions,
      suffix = if (response_suffix == "") "_trend" else paste0("_trend", response_suffix)
    )

    # Extract pre-generated observation-to-trend mappings from dimensions
    # Mappings are centralized in extract_time_series_dimensions() for consistency
    mapping_stanvars <- if (!is.null(dimensions$mappings)) {
      # Handle different mapping scenarios
      if (is.null(response_name) && length(dimensions$mappings) > 1) {
        # SHARED TRENDS: Generate response-specific mapping arrays for all responses
        # Each response gets its own obs_trend_time_{resp} and obs_trend_series_{resp} arrays

        # Validate mappings structure for shared trends
        checkmate::assert_list(dimensions$mappings, min.len = 1, names = "named")

        all_mapping_stanvars <- list()

        for (resp_name in names(dimensions$mappings)) {
          mapping <- dimensions$mappings[[resp_name]]
          resp_suffix <- paste0("_", resp_name)

          # Validate this response's mapping structure
          if (!all(c("obs_trend_time", "obs_trend_series") %in% names(mapping))) {
            stop(insight::format_error(
              "Mapping for response {.field {resp_name}} missing required fields.",
              "Expected fields: {.field obs_trend_time}, {.field obs_trend_series}."
            ), call. = FALSE)
          }

          # Create stanvars for this response's mapping arrays
          obs_time_stanvar <- brms::stanvar(
            x = mapping$obs_trend_time,
            name = paste0("obs_trend_time", resp_suffix),
            scode = paste0("array[N", resp_suffix, "] int obs_trend_time", resp_suffix, ";"),
            block = "data"
          )

          obs_series_stanvar <- brms::stanvar(
            x = mapping$obs_trend_series,
            name = paste0("obs_trend_series", resp_suffix),
            scode = paste0("array[N", resp_suffix, "] int obs_trend_series", resp_suffix, ";"),
            block = "data"
          )

          all_mapping_stanvars <- c(all_mapping_stanvars, list(obs_time_stanvar, obs_series_stanvar))
        }

        # Convert list to stanvars collection and return
        do.call(c, all_mapping_stanvars)

      } else {
        # All other cases: get single mapping and process
        mapping <- if (!is.null(response_name) && response_name %in% names(dimensions$mappings)) {
          # Response-specific mapping for multivariate models
          dimensions$mappings[[response_name]]
        } else if (length(dimensions$mappings) == 1) {
          # Single response case - use the only mapping available
          dimensions$mappings[[1]]
        } else {
          NULL
        }

        # Convert single mapping to stanvars if we have a valid mapping
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
      }
    } else {
      NULL
    }

    # Generate trend-specific stanvars (without common components)
    trend_stanvars <- generate_trend_specific_stanvars(trend_specs, data_info, response_suffix, prior = prior)

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
    # Check if this is a multivariate case requiring response-specific GLM vectors
    is_multivariate <- !is.null(dimensions) && !is.null(dimensions$mappings) &&
                      length(dimensions$mappings) > 1 && is.null(response_name)

    if (is_multivariate) {
      # MULTIVARIATE: Generate response-specific GLM vectors
      response_names <- names(dimensions$mappings)
      response_glm_usage <- detect_glm_usage(obs_setup$stancode, response_names)

      # Create mu_ones stanvars only for responses that use GLM
      for (resp_name in names(response_glm_usage)) {
        if (isTRUE(response_glm_usage[[resp_name]])) {
          mu_ones_name <- paste0("mu_ones_", resp_name)
          mu_ones_stanvar <- stanvar(
            x = 1,
            name = mu_ones_name,
            scode = paste0("vector[1] ", mu_ones_name, ";")
          )

          # Add to combined stanvars
          if (is.null(combined_stanvars)) {
            combined_stanvars <- mu_ones_stanvar
          } else {
            combined_stanvars <- c(combined_stanvars, mu_ones_stanvar)
          }
        }
      }
    } else {
      # UNIVARIATE: Use original logic with generic mu_ones
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
  }

  return(combined_stanvars)
}

#' Detect GLM Function Usage in Stan Code
#'
#' @description
#' Detects GLM function usage in Stan code
#'
#' @param stan_code Character string containing Stan code
#' @param response_names Character vector of response names for response-specific detection (optional)
#' @return Character vector of GLM types (if response_names = NULL) or named list of GLM usage per response
#' @noRd
detect_glm_usage <- function(stan_code, response_names = NULL, skip_lines = integer(0)) {
  checkmate::assert_character(stan_code, min.len = 1)
  checkmate::assert_character(response_names, null.ok = TRUE)
  checkmate::assert_integerish(skip_lines, null.ok = TRUE)

  glm_patterns <- c(
    "normal_id_glm",
    "poisson_log_glm",
    "neg_binomial_2_log_glm",
    "bernoulli_logit_glm",
    "ordered_logistic_glm",
    "categorical_logit_glm"
  )

  # Parse model block and apply skip_lines regardless of response_names
  lines <- strsplit(stan_code, "\n")[[1]]
  
  # Blank out skip_lines to prevent re-processing
  if (length(skip_lines) > 0) {
    for (line_idx in skip_lines) {
      if (line_idx > 0 && line_idx <= length(lines)) {
        lines[line_idx] <- ""
      }
    }
  }
  
  # Reconstruct code after applying skip_lines
  processed_stan_code <- paste(lines, collapse = "\n")
  
  if (is.null(response_names)) {
    detected <- sapply(glm_patterns, function(pattern) {
      any(grepl(paste0("target\\s*\\+=.*", pattern, "_l(pdf|pmf)"), processed_stan_code))
    })
    return(names(detected)[detected])
  }

  # Parse model block for response-specific GLM usage (using processed code)
  stan_code <- processed_stan_code
  model_start <- grep("^\\s*model\\s*\\{", lines)
  if (length(model_start) == 0) {
    return(setNames(rep(FALSE, length(response_names)), response_names))
  }

  # Find model block end
  brace_count <- 1
  model_end <- model_start[1]
  for (i in (model_start[1] + 1):length(lines)) {
    line <- lines[i]
    open_braces <- lengths(regmatches(line, gregexpr("\\{", line)))
    close_braces <- lengths(regmatches(line, gregexpr("\\}", line)))
    brace_count <- brace_count + open_braces - close_braces
    if (brace_count == 0) {
      model_end <- i
      break
    }
  }

  model_lines <- lines[(model_start[1] + 1):(model_end - 1)]
  result <- setNames(rep(FALSE, length(response_names)), response_names)

  likelihood_lines <- grep("target \\+=.*_l(pdf|pmf)", model_lines, value = TRUE)

  for (line in likelihood_lines) {
    response_matches <- regmatches(line, regexpr("Y_\\w+", line))
    if (length(response_matches) > 0) {
      response_name <- gsub("Y_", "", response_matches[1])
      if (response_name %in% response_names) {
        result[[response_name]] <- grepl("_glm_l(pdf|pmf)", line)
      }
    }
  }

  return(result)
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
parse_glm_parameters_single <- function(stan_code, glm_type) {
  checkmate::assert_character(stan_code, min.len = 1)
  checkmate::assert_string(glm_type)

  # Split into lines first, then search for GLM call
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

#' Find Trend Computation End Using Semantic Structure Detection
#'
#' @description
#' Finds the end of nested trend computation loops using semantic understanding
#' of Stan code structure. Replaces flawed brace counting with pattern detection.
#'
#' @param stan_code_lines Character vector of Stan code lines
#' @return Integer line number where trend computation ends, or NULL if not found
#' @noRd
find_trend_computation_end <- function(stan_code_lines) {
  checkmate::assert_character(stan_code_lines)

  # Look for the start of nested trend computation loops
  trend_loop_start <- grep("for\\s*\\(\\s*i\\s+in\\s+1:N_trend\\s*\\)", stan_code_lines)

  if (length(trend_loop_start) == 0) {
    return(NULL)
  }

  # From trend loop start, find the matching closing brace
  start_line <- trend_loop_start[1]
  brace_depth <- 0
  in_trend_block <- FALSE

  for (i in start_line:length(stan_code_lines)) {
    line <- stan_code_lines[i]

    # Count opening braces
    open_braces <- lengths(regmatches(line, gregexpr("\\{", line)))
    # Count closing braces
    close_braces <- lengths(regmatches(line, gregexpr("\\}", line)))

    if (i == start_line && open_braces > 0) {
      # Start tracking when we enter the trend computation block
      in_trend_block <- TRUE
      brace_depth <- open_braces
    } else if (in_trend_block) {
      # Update depth as we go through the nested structure
      brace_depth <- brace_depth + open_braces - close_braces

      # When we've closed all braces from the trend computation
      if (brace_depth == 0) {
        return(i + 1)  # Return line after the closing brace
      }
    }
  }

  return(NULL)  # Could not find end
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

#' Find Insertion Point After if (!prior_only) in Model Block
#'
#' @description
#' Locates the optimal insertion point for code within a model block,
#' preferring after if (!prior_only) conditional when present.
#' Eliminates duplication between GLM and multivariate injection paths.
#'
#' @param code_lines Character vector of Stan code lines  
#' @param model_block_info List with start_idx, end_idx from find_stan_block()
#' @return Integer insertion point (line number where code should be inserted)
#' @noRd
find_prior_only_insertion_point <- function(code_lines, model_block_info) {
  checkmate::assert_character(code_lines, min.len = 1)
  checkmate::assert_list(model_block_info, names = "named")
  checkmate::assert_names(names(model_block_info), must.include = c("start_idx", "end_idx"))
  checkmate::assert_integerish(model_block_info$start_idx, lower = 1)
  checkmate::assert_integerish(model_block_info$end_idx, lower = model_block_info$start_idx)
  
  # Search for if (!prior_only) pattern within model block bounds
  search_start <- model_block_info$start_idx + 1
  search_end <- min(model_block_info$end_idx, length(code_lines))
  
  for (i in search_start:search_end) {
    if (grepl("if\\s*\\(\\s*!prior_only\\s*\\)", code_lines[i], perl = TRUE)) {
      # Check if opening brace is on same line or next line
      if (grepl("\\{", code_lines[i], perl = TRUE)) {
        return(i)  # Brace on same line, insert after this line
      } else if (i + 1 <= length(code_lines) && grepl("^\\s*\\{", code_lines[i + 1], perl = TRUE)) {
        return(i + 1)  # Brace on next line, insert after the brace line
      } else {
        return(i)  # Fallback to after the if line
      }
    }
  }
  
  # Fallback: insert after model block declaration
  return(model_block_info$start_idx)
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


  # Extract and validate mapping arrays
  mapping_arrays <- extract_mapping_arrays(trend_stanvars)
  validate_mapping_arrays(mapping_arrays)

  # Generate trend injection code
  trend_injection_code <- generate_trend_injection_code(mapping_arrays)
  trend_injection_string <- paste(trend_injection_code, collapse = "\n")

  # Apply linear transformation pipeline for GLM handling and trend injection
  modified_stancode <- transform_glm_code(base_stancode, trend_injection_string)


  return(modified_stancode)
}

#' Transform GLM Calls After Standard Trend Injection
#' 
#' @description
#' Post-processes Stan code to transform GLM function calls to work with 
#' trend-enhanced mu vectors. Applied after standard trend injection to
#' ensure GLM functions use the combined linear predictor.
#'
#' @param stan_code Character string containing Stan code with trend components
#' @param detected_glm_types Character vector of detected GLM function types.
#'   Must be supported GLM types from the mvgam GLM transformation system.
#' @return Character string with modified Stan code containing transformed GLM calls
#' @noRd
transform_glm_calls_post_processing <- function(stan_code, detected_glm_types) {
  checkmate::assert_character(stan_code, min.chars = 1)
  checkmate::assert_character(detected_glm_types, min.len = 1)
  
  # Validate GLM types are supported
  supported_glm_types <- c("normal_id_glm", "poisson_log_glm", "neg_binomial_2_log_glm", 
                          "bernoulli_logit_glm", "ordered_logistic_glm", "categorical_logit_glm")
  invalid_types <- setdiff(detected_glm_types, supported_glm_types)
  if (length(invalid_types) > 0) {
    stop(insight::format_error(
      "Unsupported GLM types detected: {.field {invalid_types}}",
      "Supported types: {.field {supported_glm_types}}"
    ), call. = FALSE)
  }
  
  modified_code <- stan_code
  
  # Transform each detected GLM type
  for (glm_type in detected_glm_types) {
    # Use cached parameters from analysis (required)
    if (is.null(analysis$glm_parameters[[glm_type]])) {
      stop(insight::format_error(
        "GLM parameters not found in analysis for type: {.field {glm_type}}",
        "Analysis object must contain pre-parsed GLM parameters."
      ), call. = FALSE)
    }
    params <- analysis$glm_parameters[[glm_type]]
    
    # Transform with validation
    previous_code <- modified_code
    modified_code <- transform_glm_call(modified_code, glm_type, params)
    
    # Verify transformation occurred
    if (identical(previous_code, modified_code)) {
      stop(insight::format_error(
        "GLM transformation failed for type: {.field {glm_type}}",
        "No changes were made to the Stan code during transformation."
      ), call. = FALSE)
    }
  }
  
  return(modified_code)
}

#' Convert GLM-Only Code to Standard Form for Trend Injection
#' 
#' @description
#' Converts models with pure GLM functions (no explicit mu construction) to
#' standard form with minimal mu construction. This enables use of existing proven 
#' trend injection infrastructure via recursive preprocessing.
#'
#' @param code_lines Character vector of Stan code lines
#' @param block_info List with start_idx and end_idx for model block  
#' @param detected_glm_types Character vector of detected GLM function types
#' @return Character vector with GLM calls converted to standard mu form
#' @noRd
convert_glm_to_standard_form <- function(code_lines, block_info, detected_glm_types, analysis, processed_glm_lines = integer(0)) {
  checkmate::assert_character(code_lines, min.len = 1)
  checkmate::assert_list(block_info)
  checkmate::assert_names(names(block_info), must.include = c("start_idx", "end_idx"))
  checkmate::assert_integerish(block_info$start_idx, len = 1)
  checkmate::assert_integerish(block_info$end_idx, len = 1)
  checkmate::assert_character(detected_glm_types, min.len = 1)
  checkmate::assert_integerish(processed_glm_lines, null.ok = TRUE)
  
  # Validate required analysis object
  checkmate::assert_class(analysis, "glm_analysis")
  checkmate::assert_list(analysis$glm_parameters)
  
  # Validate block indices
  if (block_info$start_idx > block_info$end_idx || block_info$end_idx > length(code_lines)) {
    stop(insight::format_error(
      "Invalid block indices in {.field block_info}",
      "start_idx: {block_info$start_idx}, end_idx: {block_info$end_idx}, code length: {length(code_lines)}"
    ), call. = FALSE)
  }
  
  # Work with a copy to avoid modifying the original
  modified_lines <- code_lines
  
  # Get the first detected GLM type for preprocessing
  glm_type <- detected_glm_types[1]
  
  # Use cached parameters from analysis
  if (is.null(analysis$glm_parameters[[glm_type]])) {
    stop(insight::format_error(
      "GLM parameters not found in analysis for type: {.field {glm_type}}",
      "Analysis object must contain pre-parsed GLM parameters."
    ), call. = FALSE)
  }
  params <- analysis$glm_parameters[[glm_type]]
  
  # Find GLM call line to preprocess (skip already processed lines)
  glm_pattern <- paste0(glm_type, "_l(pdf|pmf)")
  glm_line_idx <- NULL
  for (i in block_info$start_idx:block_info$end_idx) {
    if (grepl(glm_pattern, modified_lines[i]) && !i %in% processed_glm_lines) {
      glm_line_idx <- i
      break
    }
  }
  
  # If all GLM lines were already processed, return original code with tracking
  if (is.null(glm_line_idx)) {
    if (!identical(Sys.getenv("TESTTHAT"), "true")) {
      cat("  No unprocessed GLM lines found for type:", glm_type, "\n")
    }
    return(list(
      code_lines = modified_lines,
      processed_glm_lines = processed_glm_lines
    ))
  }
  

  # Check if mu vector already exists in the model block  
  model_text <- paste(modified_lines[block_info$start_idx:block_info$end_idx], collapse = "\n")
  mu_already_exists <- grepl("vector\\[N\\]\\s+mu\\s*=", model_text) || grepl("mu\\s*\\+=", model_text)
  
  # Create mu construction for trend injection
  if (mu_already_exists) {
    mu_initialization <- c("    // Add fixed effects for existing mu")
  } else {
    mu_initialization <- c(
      "    // Initialize mu for trend injection",  
      "    vector[N] mu = rep_vector(0.0, N);"
    )
  }
  
  # Add fixed effects if both design matrix and coefficients are present
  if (!is.null(params$design_matrix) && !is.null(params$coefficients)) {
    fixed_effects_line <- paste0("    mu += ", params$design_matrix, " * ", params$coefficients, ";")
    mu_initialization <- c(mu_initialization, fixed_effects_line)
    
  } else {
  }
  
  # Handle intercept (including no-intercept models)
  if (!is.null(params$intercept) && params$intercept != "0" && params$intercept != "0.0") {
    mu_initialization <- c(mu_initialization, paste0("    mu += ", params$intercept, ";"))
  }
  
  # Insert mu construction before GLM call
  modified_lines[glm_line_idx] <- paste0(paste(mu_initialization, collapse = "\n"), "\n", modified_lines[glm_line_idx])
  
  # Track this GLM line as processed
  processed_glm_lines <- c(processed_glm_lines, glm_line_idx)
  
  # Return both modified code and tracking info
  return(list(
    code_lines = modified_lines,
    processed_glm_lines = processed_glm_lines
  ))
}

#' Insert Trend Injection Code After Last mu += Line in Model Block
#'
#' @description
#' Uses existing block detection infrastructure to insert trend injection code
#' after the last mu += line in the model block.
#'
#' @param code_lines Character vector of Stan code lines
#' @param trend_injection_code Character vector of trend injection code lines
#' @param processed_glm_lines Integer vector of already processed GLM line indices
#' @return List with code_lines and processed_glm_lines for state tracking
#' @noRd
insert_after_mu_lines_in_model_block <- function(code_lines, trend_injection_code, processed_glm_lines = integer(0)) {
  checkmate::assert_character(code_lines)
  checkmate::assert_character(trend_injection_code)
  checkmate::assert_integerish(processed_glm_lines, null.ok = TRUE)

  # Use existing infrastructure to find model block
  block_info <- find_stan_block(code_lines, "model")
  if (is.null(block_info)) {
    insight::format_error("Model block not found in Stan code")
  }

  # Find last mu += line within the model block
  model_lines <- code_lines[block_info$start_idx:block_info$end_idx]
  mu_line_indices <- which(grepl("\\s*mu\\s*\\+=", model_lines))


  # Check for GLM calls regardless of existing mu += lines (robust hybrid handling)
  model_block_text <- paste(model_lines, collapse = "\n")
  detected_glm_types <- detect_glm_usage(model_block_text, skip_lines = processed_glm_lines)
  
  
  # Convert GLM calls to explicit form if present (handles both pure GLM and hybrid cases)
  if (length(detected_glm_types) > 0) {
    
    # Convert GLM calls to explicit form, then recurse to find new mu += lines
    # Create GLM analysis for conversion (missing parameter bug fix)
    analysis <- analyze_stan(paste(code_lines, collapse = "\n"))
    conversion_result <- convert_glm_to_standard_form(code_lines, block_info, detected_glm_types, analysis, processed_glm_lines)
    # Recursively call with updated tracking to prevent infinite recursion
    return(insert_after_mu_lines_in_model_block(
      conversion_result$code_lines,
      trend_injection_code,
      conversion_result$processed_glm_lines
    ))
  }
  
  # Handle cases with no GLM calls
  if (length(mu_line_indices) == 0) {
    
    # Handle nonlinear models: look for mu[n] = ... patterns inside for loops
    result_code <- handle_nonlinear_trend_injection(code_lines, block_info, trend_injection_code)
    return(list(
      code_lines = result_code,
      processed_glm_lines = processed_glm_lines
    ))
  }

  # Calculate absolute position of last mu += line
  last_mu_pos <- block_info$start_idx + mu_line_indices[length(mu_line_indices)] - 1
  
  # Use the new utility function to apply correct transformation order
  transformation_pattern <- "^\\s*mu(_\\w+)?\\s*=\\s*\\w+\\s*\\("
  
  result_code <- apply_correct_transformation_order(
    code_lines = code_lines,
    insert_pos = last_mu_pos,
    trend_injection_code = trend_injection_code,
    transform_pattern = transformation_pattern,
    block_info = block_info
  )
  
  return(list(
    code_lines = result_code,
    processed_glm_lines = processed_glm_lines
  ))
}

#' Handle Trend Injection with Transformation Extraction for Any Response Type
#'
#' @description Unified handler for trend injection that works with both GLM
#' and non-GLM responses. Extracts transformation lines, injects trends, then
#' re-adds transformations in correct order.
#'
#' @param code_lines Character vector of Stan code lines
#' @param resp_name String name of the response variable
#'
#' @return Character vector with properly ordered code
#' @noRd
handle_response_trend_injection <- function(code_lines, resp_name) {
  # Parameter validation
  checkmate::assert_character(code_lines, min.len = 1)
  checkmate::assert_string(resp_name)
  checkmate::assert_true(nchar(resp_name) > 0)
  
  # Find mu assignment lines (positive pattern)
  mu_assign_pattern <- paste0("mu_", resp_name, "\\s*(\\+=|=\\s*[^=])")
  mu_assign_lines <- which(grepl(mu_assign_pattern, code_lines, perl = TRUE))
  
  if (length(mu_assign_lines) == 0) {
    insight::format_warning("No mu assignment found for response {.field ", 
                           resp_name, "}")
    return(code_lines)
  }
  
  last_mu_line <- max(mu_assign_lines)
  
  # Get block context
  model_block <- find_stan_block(code_lines, "model")
  if (is.null(model_block)) {
    model_block <- list(start_idx = 1, end_idx = length(code_lines))
  }
  
  # Robust transformation pattern for complete statements
  transform_pattern <- paste0("^\\s*mu_", resp_name, 
                            "\\s*=\\s*\\w+\\s*\\(.*\\);?\\s*$")
  
  # Trend injection code
  trend_injection <- c(
    paste0("for (n in 1:N_", resp_name, ") {"),
    paste0("  mu_", resp_name, "[n] += trend[obs_trend_time_", 
           resp_name, "[n], obs_trend_series_", resp_name, "[n]];"),
    "}"
  )
  
  # Use existing utility for transformation ordering
  return(apply_correct_transformation_order(
    code_lines = code_lines,
    insert_pos = last_mu_line,
    trend_injection_code = trend_injection,
    transform_pattern = transform_pattern,
    block_info = model_block
  ))
}

#' Apply Correct Transformation Order for Stan Linear Predictors
#'
#' @description Implements correct order: base predictor -> trend injection -> 
#' link function transformations. Extracts transformations, injects trends, 
#' then re-adds transformations after trend injection.
#'
#' @param code_lines Character vector of Stan code lines
#' @param insert_pos Integer position where trend injection should occur
#' @param trend_injection_code Character vector of trend injection code lines
#' @param transform_pattern Regex pattern to detect transformation lines
#' @param block_info List with start_idx and end_idx for the code block
#'
#' @return Character vector with properly ordered code
#' @noRd
apply_correct_transformation_order <- function(code_lines, insert_pos, 
                                             trend_injection_code, 
                                             transform_pattern, block_info) {
  # Parameter validation
  checkmate::assert_character(code_lines, min.len = 1)
  checkmate::assert_int(insert_pos, lower = 1, upper = length(code_lines))
  checkmate::assert_character(trend_injection_code, min.len = 1)
  checkmate::assert_string(transform_pattern)
  checkmate::assert_list(block_info)
  checkmate::assert_names(names(block_info), 
                         must.include = c("start_idx", "end_idx"))
  
  # Find transformation lines within the block
  model_lines <- code_lines[block_info$start_idx:block_info$end_idx]
  transformation_indices <- which(grepl(transform_pattern, model_lines))
  
  # Convert to absolute indices
  abs_transform_indices <- block_info$start_idx + transformation_indices - 1
  
  # Extract transformation lines and remove from original position
  transformation_lines <- character(0)
  if (length(abs_transform_indices) > 0) {
    transformation_lines <- code_lines[abs_transform_indices]
    code_lines <- code_lines[-abs_transform_indices]
    
    # Adjust insert position for removed lines
    removed_before_insert <- sum(abs_transform_indices < insert_pos)
    insert_pos <- insert_pos - removed_before_insert
    
    # Validate adjusted position
    checkmate::assert_int(insert_pos, lower = 1, upper = length(code_lines))
  }
  
  # Insert trend injection with proper indentation
  indented_injection <- paste0("    ", trend_injection_code)
  
  # Build result: code before + trend injection + transformations + code after
  result_lines <- c(
    code_lines[1:insert_pos],
    indented_injection,
    if (length(transformation_lines) > 0) transformation_lines else character(0),
    if (insert_pos < length(code_lines)) {
      code_lines[(insert_pos + 1):length(code_lines)]
    } else {
      character(0)
    }
  )
  
  return(result_lines)
}

#' Handle Nonlinear Trend Injection for Models Using mu[n] = ... Patterns
#'
#' @description
#' Injects trend effects into nonlinear brms models that compute mu using
#' assignment patterns like mu[n] = (expression) within for loops. Modifies
#' the Stan code to add trend effects to the nonlinear predictor.
#'
#' @param code_lines Character vector of all model code lines
#' @param block_info List with start_idx and end_idx for model block
#' @param trend_injection_code Character vector of trend code to inject (unused in nonlinear case)
#' @return Modified code_lines with trend injection added to mu assignment
#' @noRd
handle_nonlinear_trend_injection <- function(code_lines, block_info,
                                           trend_injection_code) {
  # Validate inputs
  checkmate::assert_character(code_lines, min.len = 1)
  checkmate::assert_list(block_info)
  checkmate::assert_names(names(block_info),
                         must.include = c("start_idx", "end_idx"))
  checkmate::assert_integerish(block_info$start_idx, len = 1)
  checkmate::assert_integerish(block_info$end_idx, len = 1)
  checkmate::assert_character(trend_injection_code, min.len = 1)

  # Validate block indices
  if (block_info$start_idx > block_info$end_idx ||
      block_info$end_idx > length(code_lines)) {
    insight::format_error("Invalid block indices in {.field block_info}")
  }

  model_lines <- code_lines[block_info$start_idx:block_info$end_idx]

  # Find mu[n] = ... pattern within model block
  mu_assignment_indices <- which(grepl("\\s*mu\\[n\\]\\s*=", model_lines))

  if (length(mu_assignment_indices) == 0) {
    insight::format_error(
      "No mu[n] assignment patterns found in nonlinear model block. ",
      "Expected pattern: mu[n] = <expression>;"
    )
  }

  # Use the last mu assignment for trend injection
  last_mu_idx <- max(mu_assignment_indices)
  abs_idx <- block_info$start_idx + last_mu_idx - 1

  # Extract and modify the mu assignment line
  current_line <- code_lines[abs_idx]

  # Validate we can extract the right-hand side
  if (!grepl("mu\\[n\\]\\s*=\\s*(.+);", current_line)) {
    insight::format_error(
      "Could not parse mu assignment in line: {.field current_line}"
    )
  }

  # Extract the right-hand side expression
  rhs <- sub(".*mu\\[n\\]\\s*=\\s*(.+);.*", "\\1", current_line)

  # Create new line with trend addition
  indent <- sub("^(\\s*).*", "\\1", current_line)
  new_line <- paste0(indent, "mu[n] = (", rhs,
                    ") + trend[obs_trend_time[n], obs_trend_series[n]];")

  # Replace the line
  code_lines[abs_idx] <- new_line

  return(code_lines)
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

    # Extract response suffix
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
  checkmate::assert_list(trend_stanvars, null.ok = TRUE)

  # Detect GLM usage for each response
  detected_glm_types <- detect_glm_usage(base_stancode, responses_with_trends)

  # If no trends, return unchanged
  if (is.null(trend_stanvars) || length(trend_stanvars) == 0) {
    return(base_stancode)
  }

  # Separate responses by GLM usage
  glm_responses <- names(detected_glm_types[detected_glm_types])
  non_glm_responses <- setdiff(responses_with_trends, glm_responses)
  
  # Handle GLM responses with response-specific transformations
  if (length(glm_responses) > 0) {
    code_lines <- strsplit(base_stancode, "\n", fixed = TRUE)[[1]]

    for (resp_name in glm_responses) {
      # Transform GLM function call to use to_matrix(mu_<resp>)
      glm_pattern <- paste0("target \\+= [a-z_]+_glm_l(pdf|pmf)\\(Y_", resp_name, " \\|")
      glm_lines <- which(grepl(glm_pattern, code_lines))

      if (length(glm_lines) > 0) {
        for (line_idx in glm_lines) {
          # Replace GLM call pattern
          old_line <- code_lines[line_idx]

          # Pattern to find and replace GLM call structure
          # Match: target += normal_id_glm_lpdf(Y_resp | Xc_resp, Intercept_resp, b_resp, sigma_resp);
          # Replace: target += normal_id_glm_lpdf(Y_resp | to_matrix(mu_resp), 0.0, mu_ones_resp, sigma_resp);

          if (grepl("normal_id_glm_lpdf", old_line)) {
            # Normal GLM: has sigma parameter
            new_line <- gsub(
              "(target \\+= normal_id_glm_lpdf\\(Y_[a-z_]+ \\|) [^,]+, [^,]+, [^,]+, ([^)]+\\);)",
              paste0("\\1 to_matrix(mu_", resp_name, "), 0.0, mu_ones_", resp_name, ", \\2"),
              old_line
            )
          } else {
            # Other GLM types: no sigma parameter
            new_line <- gsub(
              "(target \\+= [a-z_]+_glm_l(pdf|pmf)\\(Y_[a-z_]+ \\|) [^,]+, [^,]+, [^,;]+;",
              paste0("\\1 to_matrix(mu_", resp_name, "), 0.0, mu_ones_", resp_name, ");"),
              old_line
            )
          }

          code_lines[line_idx] <- new_line
        }
      }

      # Add mu_<resp> computation in model block using shared utility
      model_info <- find_stan_block(code_lines, "model")
      if (!is.null(model_info)) {
        # Use shared utility for consistent prior_only insertion logic
        insert_point <- find_prior_only_insertion_point(code_lines, model_info)

        # For GLM responses: preserve prior_only insertion point for optimization
        # For non-GLM responses: use trend computation strategies for complex positioning
        if (!resp_name %in% glm_responses) {
          # Strategy 1: Look for nested trend computation loops with semantic understanding
          trend_computation_end <- find_trend_computation_end(code_lines)

          if (!is.null(trend_computation_end)) {
            # Use the detected end point
            insert_point <- trend_computation_end
          } else {
            # Fallback: Look for any trend computation pattern
            trend_pattern <- "trend\\[\\s*i\\s*,\\s*s\\s*\\]\\s*=.*dot_product"
            trend_lines <- which(grepl(trend_pattern, code_lines))

            if (length(trend_lines) > 0) {
              # Strategy 2: Find the outermost closing brace after trend computation
              trend_start <- trend_lines[1]

              # Look for the trend computation nested loop start
              loop_start <- grep("for\\s*\\(\\s*i\\s+in\\s+1:N_trend\\s*\\)", code_lines)
              if (length(loop_start) > 0 && loop_start[1] <= trend_start) {
                # Found the outer trend loop - look for its closing brace
                outer_loop_line <- loop_start[1]

                # Simple approach: find line with only closing brace after the trend computation
                search_start <- max(trend_lines) + 1
                search_end <- min(length(code_lines), search_start + 20)

                for (j in search_start:search_end) {
                  if (grepl("^\\s*}\\s*$", code_lines[j])) {
                    insert_point <- j + 1  # Insert after the closing brace
                    break
                  }
                }
              } else {
                # Strategy 3: Simple fallback - insert after last trend line
                insert_point <- max(trend_lines) + 1
              }
          } else {
            # Strategy 2: Fallback for edge cases (CAR without standard loop)
            matrix_pattern <- "matrix\\[\\s*N_trend\\s*,\\s*N_series_trend\\s*\\]\\s+trend"
            matrix_lines <- which(grepl(matrix_pattern, code_lines))

            if (length(matrix_lines) > 0) {
              checkmate::assert_integerish(matrix_lines, lower = 1,
                                          upper = length(code_lines))
              # Insert after the last trend matrix declaration
              insert_point <- max(matrix_lines)
            } else {
              # Strategy 3: Final fallback for any trend reference
              any_trend_pattern <- "\\btrend\\b"
              any_trend_lines <- which(grepl(any_trend_pattern, code_lines))

              if (length(any_trend_lines) > 0) {
                insert_point <- max(any_trend_lines)
              }
              # If no trend patterns found, use original behavior
            }
          }
        }
        }

        # Create mu computation code for GLM responses
        # Check if this variable already exists in base_stancode
        existing_mu_pattern <- paste0("vector\\[N_", resp_name, "\\]\\s+mu_", resp_name)
        existing_mu_lines <- grep(existing_mu_pattern, code_lines, value = TRUE)
        
        if (length(existing_mu_lines) > 0) {
          # Existing mu variable found - add trend effects to it
          
          # Find the for loop that modifies mu_resp and insert after its closing brace
          for_loop_pattern <- paste0("for \\(n in 1:N_", resp_name, "\\)")
          for_loop_lines <- which(grepl(for_loop_pattern, code_lines))
          
          if (length(for_loop_lines) > 0) {
            # Find the closing brace of the for loop
            for_start <- max(for_loop_lines)
            
            # Look for the next standalone closing brace after the for loop
            insert_point <- for_start
            for (i in (for_start + 1):length(code_lines)) {
              if (grepl("^\\s*}\\s*$", code_lines[i])) {
                insert_point <- i
                break
              }
            }
          } else {
            # Fallback: insert after mu declaration
            mu_decl_lines <- which(grepl(existing_mu_pattern, code_lines))
            insert_point <- max(mu_decl_lines)
          }
          
          # Create trend addition code with different loop variable to avoid collision
          mu_computation <- c(
            paste0("    // Add trend effects to existing mu_", resp_name),
            paste0("    for (i in 1:N_", resp_name, ") {"),
            paste0("      mu_", resp_name, "[i] += trend[obs_trend_time_", resp_name, "[i], obs_trend_series_", resp_name, "[i]];"),
            paste0("    }")
          )
          
        } else {
          # No existing mu variable found - create from scratch
          
          # Create full mu computation with declaration
          mu_computation <- c(
            paste0("    vector[N_", resp_name, "] mu_", resp_name, " = Xc_", resp_name, " * b_", resp_name, ";"),
            paste0("    for (n in 1:N_", resp_name, ") {"),
            paste0("      mu_", resp_name, "[n] += Intercept_", resp_name, " + trend[obs_trend_time_", resp_name, "[n], obs_trend_series_", resp_name, "[n]];"),
            paste0("    }")
          )
        }

        # Insert mu computation 
        code_lines <- c(
          code_lines[1:insert_point],
          mu_computation,
          code_lines[(insert_point + 1):length(code_lines)]
        )
      }
    }

    base_stancode <- paste(code_lines, collapse = "\n")
  }

  # Detect VARMA components for enhanced trend injection
  checkmate::assert_string(base_stancode)
  has_varma_components <- any(grepl("D_trend|ma_.*_trend", base_stancode))

  # Handle non-GLM responses using unified transformation handler
  if (length(non_glm_responses) > 0) {
    code_lines <- strsplit(base_stancode, "\n", fixed = TRUE)[[1]]

    for (resp_name in non_glm_responses) {
      code_lines <- handle_response_trend_injection(code_lines, resp_name)
    }

    base_stancode <- paste(code_lines, collapse = "\n")
  }

  return(base_stancode)
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
  # Accept both regular formulas and brms formula objects
  checkmate::assert(
    checkmate::check_class(formula, "formula"),
    checkmate::check_class(formula, "brmsformula"),
    checkmate::check_class(formula, c("mvbrmsformula", "bform")),
    combine = "or",
    .var.name = "formula"
  )
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

  # Return NULL if no valid components
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

  # Determine effective dimension for innovations using symbolic names
  effective_dim <- "N_lv_trend"

  # Check for hierarchical structure
  is_hierarchical <- !is.null(hierarchical_info) && hierarchical_info$has_groups

  # Create individual stanvar components
  stanvar_components <- list()

  if (is_hierarchical) {
    # Hierarchical case: add_hierarchical_support() handles variance parameters
    # This system generates innovations but no sigma_trend parameter

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
    scode = paste0("matrix[N_trend, ", effective_dim, "] innovations_trend;"),
    block = "parameters"
  )
  stanvar_components <- append(stanvar_components, list(innovations_trend_stanvar))

  # 5. Final innovations in transformed parameters (after correlation/MA transformation)
  if (is_hierarchical) {
    # Hierarchical case: only declare scaled_innovations_trend, hierarchical system will compute it
    final_innovations_code <- paste0("
    // Scaled innovations declaration (computation handled by hierarchical system)
    matrix[N_trend, ", effective_dim, "] scaled_innovations_trend;")
  } else if (cor && effective_dim > 1) {
    # Simple correlated case
    final_innovations_code <- paste0("
    // Scaled innovations after applying correlations
    matrix[N_trend, ", effective_dim, "] scaled_innovations_trend;

    // Apply correlation transformation using efficient non-centered parameterization
    {
      matrix[", effective_dim, ", ", effective_dim, "] L_Sigma_trend = diag_pre_multiply(sigma_trend, L_Omega_trend);
      scaled_innovations_trend = innovations_trend * L_Sigma_trend';
    }")
  } else {
    # Uncorrelated case
    final_innovations_code <- paste0("
    // Scaled innovations (uncorrelated case)
    matrix[N_trend, ", effective_dim, "] scaled_innovations_trend;

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
  # Input validation following project standards
  checkmate::assert_list(data_info, names = "named")
  checkmate::assert_list(trend_specs, names = "named")
  
  has_groups <- !is.null(trend_specs$gr) && trend_specs$gr != 'NA'

  if (!has_groups) {
    return(NULL)
  }
  
  # Compute n_groups from actual data using established pattern from validations.R
  unique_groups <- sort(unique(data_info$data[[trend_specs$gr]]))
  n_groups <- length(unique_groups)
  
  if (n_groups < 1) {
    stop(insight::format_error(
      "Grouping variable {.field {trend_specs$gr}} has no unique values"
    ))
  }

  list(
    has_groups = TRUE,
    n_groups = n_groups,
    n_subgroups = data_info$n_subgroups %||% data_info$n_lv %||% data_info$n_series,
    gr_var = trend_specs$gr,
    subgr_var = trend_specs$subgr %||% 'NA'
  )
}

#' Add Hierarchical Support to Trend Components  
#'
#' Adds hierarchical correlation support to trend specifications when grouping
#' variables are present. Uses existing hierarchical infrastructure functions
#' to maintain consistency across trend types.
#'
#' @param components List of existing stanvar components
#' @param trend_specs Trend specification list  
#' @param data_info Data information list
#' @return Updated components list with hierarchical support added if applicable
#' @noRd
add_hierarchical_support <- function(components, trend_specs, data_info, prior = NULL) {
  # Input validation following project standards
  checkmate::assert_list(components)
  checkmate::assert_list(trend_specs, names = "named") 
  checkmate::assert_list(data_info, names = "named")
  
  # Use existing hierarchical extraction logic instead of reimplementing
  hierarchical_info <- extract_hierarchical_info(data_info, trend_specs)
  
  
  # Early return if no hierarchical structure needed
  if (is.null(hierarchical_info)) {
    return(components)
  }
  
  # Generate data structures before parameters
  hierarchical_data <- generate_hierarchical_data_structures(hierarchical_info, data_info)
  
  # Extract parameters
  n_groups <- hierarchical_info$n_groups
  n_subgroups <- hierarchical_info$n_subgroups
  
  # Extract trend type for conditional innovation scaling
  # Prefer trend_specs$trend over trend_specs$type for backward compatibility
  trend_type <- trend_specs$trend %||% trend_specs$type
  
  # Validate trend type against registry if present
  if (!is.null(trend_type)) {
    # Ensure registry is loaded
    ensure_registry_initialized()
    trend_info <- list_trend_types()
    valid_trends <- trend_info$trend_type
    
    if (!trend_type %in% valid_trends) {
      stop(insight::format_error(
        paste0("Unknown trend type {.field ", trend_type, "}."),
        paste0("Valid trend types are: {.field ", paste(valid_trends, collapse = ", "), "}"),
        "Check spelling or register custom trend type first."
      ))
    }
  }
  
  # Generate infrastructure components
  hierarchical_functions <- generate_hierarchical_functions()
  hierarchical_params <- generate_hierarchical_correlation_parameters(n_groups, n_subgroups, trend_type)
  hierarchical_priors <- generate_hierarchical_correlation_model(n_groups, prior)
  
  # Add components in dependency order
  components <- append_if_not_null(components, list(
    hierarchical_data,
    hierarchical_functions, 
    hierarchical_params, 
    hierarchical_priors
  ))
  
  return(components)
}

#' Generate All Trend Dimension Stanvars
#'
#' Creates ALL standardized dimension data block stanvars needed by ALL trend types.
#' This function consolidates dimension creation that was previously duplicated
#' across multiple functions, ensuring consistent dimension parameter generation
#' while maintaining proper dimensional relationships for factor vs non-factor models.
#'
#' @param n_obs Number of observations (will be named N_trend in Stan)
#' @param n_series Number of observed series
#' @param n_lv Number of latent variables (optional, determines factor model behavior)
#' @param is_factor_model Logical indicating if this is a factor model (optional, inferred from n_lv)
#' @return List of all dimension data block stanvars (N_trend, N_series_trend, N_lv_trend)
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
    n_lv <- n_series  # Non-factor model: N_lv_trend = N_series_trend
  }

  # Validate factor model scenario requirements
  checkmate::assert_number(n_lv, lower = 1, null.ok = !is_factor_model)
  
  # Calculate effective n_lv value
  n_lv_effective <- if (is_factor_model) n_lv else n_series

  # Create individual stanvars instead of nested structure  
  n_series_stanvar <- brms::stanvar(
    x = n_series,
    name = "N_series_trend",
    scode = "int<lower=1> N_series_trend;",
    block = "data",
    position = "start"
  )

  n_lv_stanvar <- brms::stanvar(
    x = n_lv_effective,
    name = "N_lv_trend", 
    scode = "int<lower=1> N_lv_trend;",
    block = "data",
    position = "start"
  )

  # Return combined stanvars for compatibility with calling code
  return(c(n_series_stanvar, n_lv_stanvar))
}

#' Sort stanvars by dependency priority
#'
#' Reorders stanvars to ensure proper Stan variable declaration order,
#' preventing "identifier not in scope" compilation errors.
#'
#' @param stanvars A named list of stanvar objects, typically from brms
#' @return A reordered list of stanvars with same class and structure
#' @details
#' Priority levels:
#' - Level 1: Dimension variables (N_trend, N_series_trend, etc.)
#' - Level 2: Arrays referencing dimensions (times_trend, obs_trend)
#' - Level 3: All other stanvars
#'
#' Within each priority level, original order is preserved for stability.
#' @noRd
sort_stanvars <- function(stanvars) {
  # Validate input per CLAUDE.md standards
  checkmate::assert_list(stanvars, null.ok = TRUE)

  if (is.null(stanvars) || length(stanvars) == 0) {
    return(stanvars)
  }

  # Enhanced validation for stanvar structure
  if (length(stanvars) > 0) {
    for (i in seq_along(stanvars)) {
      if (!is.null(stanvars[[i]])) {
        checkmate::assert_list(stanvars[[i]], names = "named")
        if (!is.null(stanvars[[i]]$scode)) {
          # Allow empty scode for ZMVN stanvars that intentionally delegate to shared systems
          # ZMVN parameters and model blocks are empty by design (architectural decision)
          stanvar_name <- stanvars[[i]]$name %||% ""
          allows_empty_scode <- grepl("zmvn_parameters|zmvn_model", stanvar_name)

          if (allows_empty_scode) {
            # ZMVN stanvars can have empty scode - they delegate to shared systems
            checkmate::assert_string(stanvars[[i]]$scode)
          } else {
            # All other stanvars must have non-empty scode
            checkmate::assert_string(stanvars[[i]]$scode, min.chars = 1)
          }
        }
      }
    }
  }

  # Evidence-based patterns from actual Stan compilation failures
  # Level 0: Random effects declarations (must precede mu_trend usage)
  re_matrix_pattern <- "matrix\\s*\\[\\s*N_[0-9]+_trend\\s*,\\s*M_[0-9]+_trend\\s*\\]\\s+r_[0-9]+_trend"
  re_vector_pattern <- "vector\\s*\\[\\s*N_[0-9]+_trend\\s*\\]\\s+r_[0-9]+_[0-9]+_trend"
  re_scale_r_cor_pattern <- "r_[0-9]+_trend\\s*=\\s*scale_r_cor"
  re_extraction_pattern <- "r_[0-9]+_[0-9]+_trend\\s*=\\s*r_[0-9]+_trend\\s*\\["

  # Level 1: mu_trend declarations (must come after RE but before other foundation)
  mu_trend_decl_pattern <- "vector\\s*\\[\\s*N_trend\\s*\\]\\s+mu_trend\\s*="

  # Level 2: Foundation variables (no dependencies on trend)
  lv_trend_pattern <- "matrix\\s*\\[\\s*N_trend\\s*,\\s*N_lv_trend\\s*\\]\\s+lv_trend\\s*[;=]"
  scaled_innov_pattern <- "matrix\\s*\\[\\s*N_trend\\s*,\\s*N_lv_trend\\s*\\]\\s+scaled_innovations_trend\\s*[;=]"
  z_pattern <- "\\bmatrix\\s*\\[[^]]*\\]\\s+Z\\s*[;=]"

  # Level 3: trend matrix computation (depends on mu_trend and lv_trend)
  trend_matrix_decl_pattern <- "matrix\\s*\\[\\s*N_trend\\s*,\\s*N_series_trend\\s*\\]\\s+trend\\s*;"
  trend_computation_pattern <- "trend\\s*\\[\\s*i\\s*,\\s*s\\s*\\]\\s*=.*dot_product"

  # Level 4: mu linear predictor injections (depends on trend)
  mu_injection_pattern <- "mu_\\w+\\s*\\[\\s*n\\s*\\]\\s*\\+=.*trend\\s*\\["

  # Dimension ordering patterns
  dimension_pattern <- "^\\s*int<lower=1>\\s+(N_series_trend|N_lv_trend|N_trend)\\s*;"
  array_with_dims_pattern <- "^\\s*array\\[.*\\b(N_series_trend|N_lv_trend)\\b.*\\]"

  # Initialize level collections
  level0_re_declarations <- integer(0)
  level1_mu_trend <- integer(0)
  level2_foundation <- integer(0)
  level3_trend_comp <- integer(0)
  level4_mu_inject <- integer(0)

  # Add dimension-specific categories to fix Stan compilation ordering bugs
  dimensions <- integer(0)         # N_series_trend, N_lv_trend declarations (must come first)
  dimension_arrays <- integer(0)   # Arrays that reference dimensions (must come after)

  # Categorize stanvars by actual dependency patterns
  for (i in seq_along(stanvars)) {
    sv <- stanvars[[i]]

    # Skip stanvars without analyzable code
    if (is.null(sv) || is.null(sv$scode) || !is.character(sv$scode) || nchar(sv$scode) == 0) {
      next
    }

    scode <- as.character(sv$scode)

    # Use if-else chain to prevent double-categorization
    if (grepl(dimension_pattern, scode)) {
      dimensions <- c(dimensions, i)
    } else if (grepl(array_with_dims_pattern, scode)) {
      dimension_arrays <- c(dimension_arrays, i)
    } else if (grepl(re_matrix_pattern, scode) ||
               grepl(re_vector_pattern, scode) ||
               grepl(re_scale_r_cor_pattern, scode) ||
               grepl(re_extraction_pattern, scode)) {
      level0_re_declarations <- c(level0_re_declarations, i)
    } else if (grepl(mu_trend_decl_pattern, scode)) {
      level1_mu_trend <- c(level1_mu_trend, i)
    } else if (grepl(lv_trend_pattern, scode) ||
               grepl(scaled_innov_pattern, scode) ||
               grepl(z_pattern, scode)) {
      level2_foundation <- c(level2_foundation, i)
    } else if (grepl(trend_matrix_decl_pattern, scode) ||
               grepl(trend_computation_pattern, scode)) {
      level3_trend_comp <- c(level3_trend_comp, i)
    } else if (grepl(mu_injection_pattern, scode)) {
      level4_mu_inject <- c(level4_mu_inject, i)
    }
  }

  # All other stanvars (priors, etc.)
  categorized <- c(dimensions, dimension_arrays, level0_re_declarations, level1_mu_trend, level2_foundation, level3_trend_comp, level4_mu_inject)
  others <- setdiff(seq_along(stanvars), categorized)

  # Dependency-respecting order
  sort_order <- c(dimensions, level0_re_declarations, level1_mu_trend, level2_foundation, dimension_arrays, others, level3_trend_comp, level4_mu_inject)

  # Validate sort_order indices are within bounds
  checkmate::assert_integerish(sort_order, lower = 1, upper = length(stanvars),
                              unique = TRUE, len = length(stanvars))

  # Maintain stanvars class structure
  sorted_stanvars <- stanvars[sort_order]
  class(sorted_stanvars) <- class(stanvars)

  return(sorted_stanvars)
}

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
      scode = glue::glue("matrix[N_series_trend, N_lv_trend] Z;"),
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
      scode = glue::glue("matrix[N_series_trend, N_lv_trend] Z = diag_matrix(rep_vector(1.0, N_lv_trend));"),
      block = "tdata"
    )
    return(z_matrix_stanvar)
  } else {
    # Return NULL for empty case
    return(NULL)
  }
}

#' Generate Matrix Z Multiblock Stanvars
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
  # For factor models, skip parameters (handled by generate_factor_model)
  stanvars_list <- list(
    if (!is_factor_model) generate_matrix_z_parameters(is_factor_model, n_lv, n_series) else NULL,
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
  # Input validation
  checkmate::assert_logical(is_factor_model, len = 1)
  checkmate::assert_integerish(n_lv, lower = 1, any.missing = FALSE)

  if (!is_factor_model) {
    return(NULL)
  }

  components <- list()

  # 1. PARAMETERS block - Z_raw factor loadings parameter
  z_raw_parameters <- brms::stanvar(
    name = "z_raw_parameters",
    scode = "// Factor loading matrix (estimated for factor model)\nvector[N_series_trend * N_lv_trend] Z_raw;  // raw factor loadings",
    block = "parameters"
  )
  components <- append(components, list(z_raw_parameters))

  # 2. TPARAMETERS block - Z matrix construction with identifiability constraints
  z_construction <- brms::stanvar(
    name = "z_construction",
    scode = "// Factor loading matrix with identifiability constraints\n  matrix[N_series_trend, N_lv_trend] Z = rep_matrix(0, N_series_trend, N_lv_trend);\n  // constraints allow identifiability of loadings\n  {\n    int index = 1;\n    for (j in 1 : N_lv_trend) {\n      for (i in j : N_series_trend) {\n        Z[i, j] = Z_raw[index];\n        index += 1;\n      }\n    }\n  }",
    block = "tparameters"
  )
  components <- append(components, list(z_construction))

  # 3. MODEL block - Z_raw factor loading priors
  # Note: innovations_trend priors are handled by shared innovation system
  # Note: LV_raw no longer exists - was outdated parameter name
  factor_z_priors <- brms::stanvar(
    name = "factor_z_priors",
    scode = "Z_raw ~ student_t(3, 0, 1);",
    block = "model"
  )
  components <- append(components, list(factor_z_priors))

  return(combine_stanvars(z_raw_parameters, z_construction, factor_z_priors))
}

#' Generate Transformed Parameters Block Injections for Trend Computation
#'
#' All trends must use the same computation pattern:
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
      matrix[N_trend, N_series_trend] trend;

      // Universal trend computation: state-space dynamics + linear predictors
      // dot_product captures dynamic component, mu_trend captures trend_formula
      for (i in 1:N_trend) {{
        for (s in 1:N_series_trend) {{
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
#' All trends (AR, VAR, CAR, ZMVN) need the same hierarchical correlation
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

#' Generate Hierarchical Data Structure Stanvars
#'
#' Creates Stan data block declarations for hierarchical grouping structures.
#' Generates N_groups_trend dimension and group_inds_trend mapping array
#' following established stanvar patterns from generate_common_trend_data().
#'
#' @param hierarchical_info List from extract_hierarchical_info() containing
#'   validated n_groups, n_subgroups, and grouping variable information
#' @param data_info Data information list containing actual data for mapping creation
#' @return Combined stanvars for hierarchical data structures
#' @noRd
generate_hierarchical_data_structures <- function(hierarchical_info, data_info) {
  # Input validation following project standards with field existence checks
  checkmate::assert_list(hierarchical_info, names = "named")
  checkmate::assert_list(data_info, names = "named")
  checkmate::assert_names(names(hierarchical_info), 
                          must.include = c("n_groups", "n_subgroups", "gr_var"))
  checkmate::assert_number(hierarchical_info$n_groups, lower = 1)
  checkmate::assert_number(hierarchical_info$n_subgroups, lower = 1)
  
  # Validate grouping variable is specified
  checkmate::assert_string(hierarchical_info$gr_var)
  
  n_groups <- hierarchical_info$n_groups
  gr_var <- hierarchical_info$gr_var
  
  # Generate N_groups_trend dimension following generate_common_trend_data pattern
  n_groups_stanvar <- brms::stanvar(
    x = n_groups,
    name = "N_groups_trend",
    scode = "int<lower=1> N_groups_trend;",
    block = "data",
    position = "start"  # Dimensions go first
  )
  
  # Generate N_subgroups_trend dimension (number of series within each group)
  # Calculate from hierarchical_info (already computed in extract_hierarchical_info)
  n_subgroups <- hierarchical_info$n_subgroups
  n_subgroups_stanvar <- brms::stanvar(
    x = n_subgroups,
    name = "N_subgroups_trend", 
    scode = "int<lower=1> N_subgroups_trend;",
    block = "data",
    position = "start"  # Dimensions go first
  )
  
  # Create series-to-group mapping (one entry per series, not per observation)
  series_var <- data_info$series_var %||% "series"
  group_levels <- sort(unique(data_info$data[[gr_var]]))
  
  # Extract unique series-group combinations to create proper mapping
  unique_series_data <- data_info$data[!duplicated(data_info$data[[series_var]]), ]
  series_groups <- unique_series_data[[gr_var]]
  group_inds_array <- match(series_groups, group_levels)
  
  # Generate group_inds_trend array (maps each series to its group)
  group_inds_stanvar <- brms::stanvar(
    x = group_inds_array,
    name = "group_inds_trend", 
    scode = "array[N_series_trend] int<lower=1> group_inds_trend;",
    block = "data"
  )
  
  # Return combined stanvars following established pattern
  return(combine_stanvars(n_groups_stanvar, n_subgroups_stanvar, group_inds_stanvar))
}

#' Generate Parameters Block Injections for Hierarchical Correlations
#'
#' @param n_groups Number of groups for hierarchical modeling
#' @param n_subgroups Number of subgroups (typically n_lv)
#' @param trend_type Trend type for conditional parameter generation (VAR, AR, etc.)
#' @param exclude_computed Logical. If TRUE, exclude computed parameters (Sigma_group_trend, L_group_trend) 
#'   for trends that handle their own local computation (e.g., VAR). Defaults to FALSE.
#' @return List of parameters block stanvars
#' @noRd
generate_hierarchical_correlation_parameters <- function(n_groups, n_subgroups, trend_type = NULL, exclude_computed = FALSE) {
  # Validate inputs following project patterns
  checkmate::assert_integer(n_groups, len = 1, lower = 1)
  checkmate::assert_integer(n_subgroups, len = 1, lower = 1)
  checkmate::assert_character(trend_type, len = 1, null.ok = TRUE)
  checkmate::assert_logical(exclude_computed, len = 1)
  
  l_omega_global <- brms::stanvar(
    name = "L_Omega_global_trend",
    scode = "cholesky_factor_corr[N_subgroups_trend] L_Omega_global_trend;",
    block = "parameters"
  )

  l_deviation_group <- brms::stanvar(
    name = "L_deviation_group_trend",
    scode = "array[N_groups_trend] cholesky_factor_corr[N_subgroups_trend] L_deviation_group_trend;",
    block = "parameters"
  )

  alpha_cor <- brms::stanvar(
    name = "alpha_cor_trend",
    scode = "real<lower=0, upper=1> alpha_cor_trend;",
    block = "parameters"
  )

  l_omega_group <- brms::stanvar(
    name = "L_Omega_group_trend",
    scode = glue::glue("array[N_groups_trend] cholesky_factor_corr[N_subgroups_trend] L_Omega_group_trend;"),
    block = "tparameters"
  )

  # Group-specific innovation variances (one vector per group)
  sigma_group_params <- brms::stanvar(
    name = "sigma_group_trend",
    scode = glue::glue("array[N_groups_trend] vector<lower=0>[N_subgroups_trend] sigma_group_trend;"),
    block = "parameters"
  )
  
  # Conditionally create computed parameters (exclude for VAR)
  computed_stanvars <- list()
  if (!exclude_computed) {
    sigma_group <- brms::stanvar(
      name = "Sigma_group_trend", 
      scode = glue::glue("array[N_groups_trend] cov_matrix[N_subgroups_trend] Sigma_group_trend;"),
      block = "tparameters"
    )
    
    # Cholesky factors for innovation scaling
    l_group <- brms::stanvar(
      name = "L_group_trend",
      scode = glue::glue("array[N_groups_trend] matrix[N_subgroups_trend, N_subgroups_trend] L_group_trend;"),
      block = "tparameters"
    )
  
  # Determine if innovation scaling is needed
  # VAR, CAR, and PW sample directly using multi_normal, bypassing innovation scaling
  # AR, RW, and ZMVN use innovation scaling through L_group_trend matrices
  needs_innovation_scaling <- !is.null(trend_type) && 
                              !(trend_type %in% c("VAR", "CAR", "PW"))
  
  # Common correlation matrix computation for all hierarchical models
  common_correlation_code <- "
  // Compute group-specific correlation matrices using shared infrastructure
  for (g_idx in 1:N_groups_trend) {
    L_Omega_group_trend[g_idx] = combine_cholesky(
      L_Omega_global_trend, 
      L_deviation_group_trend[g_idx], 
      alpha_cor_trend
    );
    
    // Cholesky factor for group-specific covariance matrices
    L_group_trend[g_idx] = diag_pre_multiply(sigma_group_trend[g_idx], L_Omega_group_trend[g_idx]);
    
    // Full covariance matrix for group-specific components
    Sigma_group_trend[g_idx] = multiply_lower_tri_self_transpose(L_group_trend[g_idx]);
  }"
  
  # Conditionally add innovation scaling for AR/RW/ZMVN models
  innovation_scaling_code <- if (needs_innovation_scaling) {
    "
  
  // Apply group-specific innovation scaling (only for AR/RW/ZMVN models)
  for (t in 1:N_trend) {
    for (g_idx in 1:N_groups_trend) {
      vector[N_subgroups_trend] group_innov;
      
      // Collect innovations for this group's series  
      int k = 0;
      for (s in 1:N_lv_trend) {
        if (group_inds_trend[s] == g_idx) {
          k += 1;
          group_innov[k] = innovations_trend[t, s];
        }
      }
      
      // Scale using Cholesky factor and distribute back
      vector[N_subgroups_trend] scaled = L_group_trend[g_idx] * group_innov;
      k = 0;
      for (s in 1:N_lv_trend) {
        if (group_inds_trend[s] == g_idx) {
          k += 1;
          scaled_innovations_trend[t, s] = scaled[k];
        }
      }
    }
  }"
  } else {
    # VAR/CAR/PW models sample directly, no innovation scaling needed
    ""
  }
  
    # Combine common and conditional code
    computation_code <- paste0(common_correlation_code, innovation_scaling_code)
    
    # Add computation logic for hierarchical correlation matrices
    computation <- brms::stanvar(
      name = "hierarchical_correlation_computation",
      scode = computation_code,
      block = "tparameters"
    )
    
    computed_stanvars <- list(sigma_group, l_group, computation)
  }

  # Return combined stanvars with conditional computed parameters
  base_stanvars <- if (exclude_computed) {
    list(l_omega_global, l_deviation_group, alpha_cor, sigma_group_params)
  } else {
    list(l_omega_global, l_deviation_group, alpha_cor, sigma_group_params, l_omega_group)
  }
  all_stanvars <- append(base_stanvars, computed_stanvars)
  do.call(combine_stanvars, all_stanvars)
}

#' Generate Model Block Injections for Hierarchical Correlation Priors
#'
#'
#' @param n_groups Number of groups for hierarchical modeling
#' @return List of model block stanvars
#' @noRd
generate_hierarchical_correlation_model <- function(n_groups, prior = NULL) {
  
  alpha_cor_prior <- brms::stanvar(
    name = "alpha_cor_trend_prior",
    scode = glue::glue("alpha_cor_trend ~ {get_trend_parameter_prior(prior, 'alpha_cor_trend')};"),
    block = "model"
  )

  l_omega_global_prior <- brms::stanvar(
    name = "L_Omega_global_trend_prior",
    scode = "L_Omega_global_trend ~ lkj_corr_cholesky(1);",
    block = "model"
  )

  l_deviation_group_prior <- brms::stanvar(
    name = "L_deviation_group_trend_prior",
    scode = "for (g_idx in 1:N_groups_trend) { L_deviation_group_trend[g_idx] ~ lkj_corr_cholesky(6); }",
    block = "model"
  )

  # Prior for group-specific innovation variances
  sigma_group_prior <- brms::stanvar(
    name = "sigma_group_trend_prior",
    scode = glue::glue("for (g_idx in 1:N_groups_trend) {{ to_vector(sigma_group_trend[g_idx]) ~ {get_trend_parameter_prior(prior, 'sigma_trend')}; }}"),
    block = "model"
  )

  return(combine_stanvars(alpha_cor_prior, l_omega_global_prior, l_deviation_group_prior, sigma_group_prior))
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
                            (!trend_type %in% c("PW", "VAR", "CAR"))  # PW, VAR, and CAR opt out of shared innovations

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
    effective_dim <- "N_lv_trend"
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

  # Convert VAR ma parameter to ma_lags for compatibility with generate_var_trend_stanvars
  if (trend_type == "VAR") {
    if (!is.null(trend_specs$ma) && trend_specs$ma && is.null(trend_specs$ma_lags)) {
      trend_specs$ma_lags <- 1
    } else if (is.null(trend_specs$ma_lags)) {
      trend_specs$ma_lags <- 0
    }
  }

  # Generate trend-specific stanvars using consistent naming convention
  trend_stanvars <- generator_function(trend_specs, data_info, prior = prior)

  # Apply response suffix post-processing for multivariate models
  if (response_suffix != "") {
    trend_stanvars <- apply_response_suffix_to_stanvars(trend_stanvars, response_suffix)
  }

  # Determine correct block for lv_trend based on trend type
  # For VAR models, lv_trend must be in parameters block for proper MCMC sampling
  lv_trend_block <- if (trend_type == "VAR") "parameters" else "tparameters"

  # Add shared lv_trend matrix declaration (used by all trend types)
  lv_trend_stanvar <- brms::stanvar(
    name = "shared_lv_trend",
    scode = "matrix[N_trend, N_lv_trend] lv_trend;",
    block = lv_trend_block
  )

  # Combine all components: common dimensions + shared innovations + lv_trend + trend-specific
  all_components <- list(common_dimensions, shared_stanvars, lv_trend_stanvar, trend_stanvars)
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
      scode = "vector<lower=-1,upper=1>[N_lv_trend] theta1_trend;",
      block = "parameters"
    )
    components <- append(components, list(rw_parameters_stanvar))
  }

  # 2. TPARAMETERS block - RW dynamics computation (always needed)
  rw_tparameters_stanvar <- brms::stanvar(
    name = "rw_tparameters",
    scode = glue::glue("
      // Latent states with RW dynamics
      {if(has_ma) 'matrix[N_trend, N_lv_trend] ma_innovations_trend = scaled_innovations_trend;' else ''}

      {if(has_ma) '// Apply MA(1) transformation
      for (i in 2:N_trend) {{
        for (j in 1:N_lv_trend) {{
          ma_innovations_trend[i, j] += theta1_trend[j] * ma_innovations_trend[i-1, j];
        }}
      }}' else ''}

      // Apply RW dynamics
      lv_trend[1, :] = {if(has_ma) 'ma_innovations_trend' else 'scaled_innovations_trend'}[1, :];
      for (i in 2:N_trend) {{
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
  p <- trend_specs$p %||% 1
  n_series <- data_info$n_series %||% 1
  n_obs <- data_info$n_obs

  # Validate dimensions
  checkmate::assert_int(n_lv, lower = 1)
  checkmate::assert_int(n_series, lower = 1)
  checkmate::assert_int(n_obs, lower = 1)

  # Convert p parameter to ar_lags vector
  ar_lags <- if (!is.null(trend_specs$ar_lags)) trend_specs$ar_lags else p
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

  # STEP 3: Add hierarchical correlation support if applicable (BEFORE AR dynamics)
  components <- add_hierarchical_support(components, trend_specs, data_info, prior)

  # 1. PARAMETERS block - AR trend-specific parameters
  ar_param_declarations <- sapply(ar_lags, function(lag) {
    glue::glue("vector<lower=-1,upper=1>[N_lv_trend] ar{lag}_trend;")
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
      scode = "vector<lower=-1,upper=1>[N_lv_trend] theta1_trend;",
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
      {if(has_ma) 'matrix[N_trend, N_lv_trend] ma_innovations_trend = scaled_innovations_trend;' else ''}

      {if(has_ma) '// Apply MA(1) transformation
      for (i in 2:N_trend) {{
        for (j in 1:N_lv_trend) {{
          ma_innovations_trend[i, j] += theta1_trend[j] * ma_innovations_trend[i-1, j];
        }}
      }}' else ''}

      // Initialize first {max_lag} time points
      for (i in 1:{max_lag}) {{
        lv_trend[i, :] = {if(has_ma) 'ma_innovations_trend' else 'scaled_innovations_trend'}[i, :];
      }}

      // Apply AR dynamics
      for (i in {max_lag + 1}:N_trend) {{
        for (j in 1:N_lv_trend) {{
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

#' VAR/VARMA Trend Generator
#'
#' @description
#' Generates Stan code components for vector autoregressive (VAR) and VARMA trends with
#' support for factor models, hierarchical correlations, and stationarity
#' constraints. Uses efficient matrix formulation for multi-lag VAR models
#' with proper parameter naming and non-centered parameterization.
#' 
#' NOTE: VARMA implementation follows Heaps (2022) methodology with constraint
#' that MA order q=1 (ma_lags must be 0 or 1). Higher order MA components 
#' are not supported.
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
#' # VARMA(2,1) model 
#' trend_specs <- list(lags = 2, ma_lags = 1, n_lv = 3)
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
  n_series <- data_info$n_series %||% 1
  n_lv <- trend_specs$n_lv %||% n_series
  lags <- trend_specs$p %||% 1
  ma_lags <- trend_specs$ma_lags %||% 0
  n <- data_info$n_obs

  # VAR/VARMA constraint validation
  if (lags < 1) {
    insight::format_error("VAR model requires {.field lags} >= 1")
  }
  if (ma_lags < 0) {
    insight::format_error("VARMA model requires {.field ma_lags} >= 0")
  }
  # VARMA constraint: only q=1 is supported for mvgam
  # Reason: Simplifies initialization and computation while covering most practical use cases
  if (ma_lags > 1) {
    insight::format_error("mvgam VARMA models support only {.field ma_lags} = 1. Higher order MA components are not currently implemented.")
  }
  if (n_lv > n_series && ma_lags == 0) {
    insight::format_error("VAR factor model requires {.field n_lv} <= {.field n_series}")
  }

  # Check for hierarchical grouping requirements
  is_hierarchical <- !is.null(trend_specs$gr) && trend_specs$gr != 'NA'
  hierarchical_info <- NULL

  if (is_hierarchical) {
    # Cross-cutting validation handled by injection function

    # Get hierarchical parameters using existing extraction function
    hierarchical_info <- extract_hierarchical_info(data_info, trend_specs)
    if (is.null(hierarchical_info)) {
      stop(insight::format_error("Hierarchical VAR requires grouping specification"))
    }
    n_groups <- hierarchical_info$n_groups
    n_subgroups <- hierarchical_info$n_subgroups

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
      array[,] matrix rev_mapping(array[] matrix P, matrix Sigma) {
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
      matrix initial_joint_var(matrix Sigma, array[] matrix phi, array[] matrix theta) {
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

  # Dynamic parts that need variable interpolation
  dynamic_part <- glue::glue("
      // Zero mean vector for VARMA process (following Heaps 2022)
      vector[N_lv_trend] trend_zeros = rep_vector(0.0, N_lv_trend);

  ")

  var_tdata_stanvar <- brms::stanvar(
    name = "var_tdata",
    scode = dynamic_part,
    block = "tdata"
  )
  
  # Add lag count as data variable to eliminate hardcoded loop bounds
  var_data_stanvar <- brms::stanvar(
    x = lags,
    name = "N_lags_trend",
    scode = "int<lower=1> N_lags_trend;",
    block = "data"
  )

  # Core VAR parameters block with conditional VARMA extensions
  # Pre-compute string literals to avoid nested glue::glue() scoping issues
  var_specific_params <- if(is_hierarchical) {
    paste0(
      "      // Hierarchical VAR: group-specific raw matrices with shared hyperpriors\n",
      "      array[N_groups_trend, ", lags, "] matrix[N_subgroups_trend, N_subgroups_trend] A_raw_group_trend;"
    )
  } else {
    paste0(
      "      // Standard VAR: single raw matrix\n",
      "      array[", lags, "] matrix[N_lv_trend, N_lv_trend] A_raw_trend;\n\n",
      "      // Standard variance and correlation parameters\n",
      "      vector<lower=0>[N_lv_trend] sigma_trend;\n",
      "      cholesky_factor_corr[N_lv_trend] L_Omega_trend;"
    )
  }

  var_parameters_stanvar <- brms::stanvar(
    name = "var_parameters",
    scode = glue::glue("
      {var_specific_params}

      // Shared hierarchical hyperparameters for A_raw coefficients (across all groups)
      // [1] = diagonal elements, [2] = off-diagonal elements
      array[2] vector[{lags}] Amu_trend;     // Shared means
      array[2] vector<lower=0>[{lags}] Aomega_trend;  // Shared precisions

      // Joint initialization vector for stationary distribution
      vector[{if(is_varma) paste0('(', lags, ' + ', ma_lags, ') * N_lv_trend') else paste0(lags, ' * N_lv_trend')}] init_trend;

      // VAR dynamics
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
      array[{ma_lags}] matrix[N_lv_trend, N_lv_trend] D_raw_trend;

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
  # Pre-compute string literals to avoid nested glue::glue() scoping issues
  var_tparams_main <- if(is_hierarchical) {
    paste0(
      "      // Hierarchical VAR: group-specific computations then block assembly\n\n",
      "      // Group-specific hierarchical correlations and covariances\n",
      "      array[N_groups_trend] cov_matrix[N_subgroups_trend] Sigma_group_trend;\n",
      "      array[N_groups_trend, ", lags, "] matrix[N_subgroups_trend, N_subgroups_trend] A_group_trend;\n\n",
      "      // Compute group-specific covariances using hierarchical correlation structure\n",
      "      for (g_idx in 1:N_groups_trend) {\n",
      "        // Use shared combine_cholesky function to eliminate duplication\n",
      "        matrix[N_subgroups_trend, N_subgroups_trend] L_Omega_group_trend =\n",
      "          combine_cholesky(L_Omega_global_trend, L_deviation_group_trend[g_idx], alpha_cor_trend);\n",
      "        Sigma_group_trend[g_idx] = multiply_lower_tri_self_transpose(\n",
      "          diag_pre_multiply(sigma_group_trend[g_idx], L_Omega_group_trend));\n\n",
      "        // Apply Heaps transformation per group\n",
      "        for (lag in 1:N_lags_trend) {\n",
      "          array[1] matrix[N_subgroups_trend, N_subgroups_trend] P_group;\n",
      "          P_group[1] = AtoP(A_raw_group_trend[g_idx, lag]);\n",
      "          array[2, 1] matrix[N_subgroups_trend, N_subgroups_trend] result_group =\n",
      "            rev_mapping(P_group, Sigma_group_trend[g_idx]);\n",
      "          A_group_trend[g_idx, lag] = result_group[1, 1];\n",
      "        }\n",
      "      }\n\n",
      "      // Build block-structured full matrices (groups do not interact)\n",
      "      cov_matrix[N_lv_trend] Sigma_trend = rep_matrix(0, N_lv_trend, N_lv_trend);\n",
      "      array[N_lags_trend] matrix[N_lv_trend, N_lv_trend] A_trend;\n",
      "      for (lag in 1:N_lags_trend) {\n",
      "        A_trend[lag] = rep_matrix(0, N_lv_trend, N_lv_trend);\n",
      "      }\n\n",
      "      // Block-diagonal assembly using series-iteration pattern\n",
      "      for (g_idx in 1:N_groups_trend) {\n",
      "        // Collect series indices for this group\n", 
      "        int group_series[N_subgroups_trend];\n",
      "        int k = 0;\n",
      "        for (s in 1:N_lv_trend) {\n",
      "          if (group_inds_trend[s] == g_idx) {\n",
      "            k += 1;\n",
      "            group_series[k] = s;\n",
      "          }\n",
      "        }\n\n",
      "        // Insert group matrices into appropriate blocks\n",
      "        for (i in 1:k) {\n",
      "          for (j in 1:k) {\n",
      "            Sigma_trend[group_series[i], group_series[j]] = Sigma_group_trend[g_idx][i, j];\n",
      "            for (lag in 1:N_lags_trend) {\n",
      "              A_trend[lag][group_series[i], group_series[j]] = A_group_trend[g_idx, lag][i, j];\n",
      "            }\n",
      "          }\n",
      "        }\n",
      "      }"
    )
  } else {
    paste0(
      "      // Standard VAR: single covariance matrix and transformation\n",
      "      matrix[N_lv_trend, N_lv_trend] L_Sigma_trend = diag_pre_multiply(sigma_trend, L_Omega_trend);\n",
      "      cov_matrix[N_lv_trend] Sigma_trend = multiply_lower_tri_self_transpose(L_Sigma_trend);\n\n",
      "      // Transform raw parameters to stationary coefficients\n",
      "      array[", lags, "] matrix[N_lv_trend, N_lv_trend] A_trend;\n\n",
      "      // Working arrays for stationarity transformation\n",
      "      array[", lags, "] matrix[N_lv_trend, N_lv_trend] P_var;\n",
      "      array[2, ", lags, "] matrix[N_lv_trend, N_lv_trend] result_var;\n\n",
      "      for (i in 1:", lags, ") {\n",
      "        P_var[i] = AtoP(A_raw_trend[i]);\n",
      "      }\n\n",
      "      result_var = rev_mapping(P_var, Sigma_trend);\n\n",
      "      for (i in 1:", lags, ") {\n",
      "        A_trend[i] = result_var[1, i];\n",
      "      }"
    )
  }

  # Pre-compute VARMA-specific additions
  varma_d_trend <- if(is_varma) paste0("array[", ma_lags, "] matrix[N_lv_trend, N_lv_trend] D_trend;") else ""
  varma_ma_init <- if(is_varma) "vector[N_lv_trend] ma_init_trend;" else ""

  var_tparameters_stanvar <- brms::stanvar(
    name = "var_tparameters",
    scode = glue::glue("
      {var_tparams_main}

      {varma_d_trend}

      // Joint covariance matrix for stationary initialization
      cov_matrix[{if(is_varma) paste0('(', lags, ' + ', ma_lags, ') * N_lv_trend') else paste0(lags, ' * N_lv_trend')}] Omega_trend;

      {varma_ma_init}

      {if(is_varma) glue::glue('
      // Transform D_raw_trend to stationary D_trend (VARMA only)
      array[{ma_lags}] matrix[N_lv_trend, N_lv_trend] P_ma;
      array[2, {ma_lags}] matrix[N_lv_trend, N_lv_trend] result_ma;

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
      {if(is_varma) 'Omega_trend = initial_joint_var(Sigma_trend, A_trend, D_trend);' else 'array[1] matrix[N_lv_trend, N_lv_trend] empty_theta; empty_theta[1] = rep_matrix(0.0, N_lv_trend, N_lv_trend); Omega_trend = initial_joint_var(Sigma_trend, A_trend, empty_theta[1:0]);'}

      {if(is_varma) glue::glue('
      // Initialize MA error term from init_trend 
      ma_init_trend = init_trend[({lags} * N_lv_trend + 1):({lags + 1} * N_lv_trend)];
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

  # Calculate initialization vector dimension: lags*N_lv_trend for VAR, (lags+ma_lags)*N_lv_trend for VARMA
  init_dim_expr <- if (is_varma) glue::glue('({lags} + {ma_lags}) * N_lv_trend') else glue::glue('{lags} * N_lv_trend')

  # Pre-compute VAR priors to avoid nested glue::glue() scoping issues
  var_priors_block <- if(is_hierarchical) {
    paste0(
      "      // Hierarchical VAR: group-specific priors with shared hyperpriors\n",
      "      for (g_idx in 1:N_groups_trend) {\n",
      "        for (lag in 1:N_lags_trend) {\n",
      "          // Diagonal elements prior (shared across all groups)\n",
      "          diagonal(A_raw_group_trend[g_idx, lag]) ~ normal(Amu_trend[1, lag], 1 / sqrt(Aomega_trend[1, lag]));\n\n",
      "          // Off-diagonal elements prior (shared across all groups)\n",
      "          for (i in 1:N_subgroups_trend) {\n",
      "            for (j in 1:N_subgroups_trend) {\n",
      "              if (i != j) {\n",
      "                A_raw_group_trend[g_idx, lag, i, j] ~ normal(Amu_trend[2, lag], 1 / sqrt(Aomega_trend[2, lag]));\n",
      "              }\n",
      "            }\n",
      "          }\n",
      "        }\n",
      "      }"
    )
  } else {
    paste0(
      "      // Standard VAR: single matrix priors\n",
      "      for (lag in 1:N_lags_trend) {\n",
      "        // Diagonal elements prior\n",
      "        diagonal(A_raw_trend[lag]) ~ normal(Amu_trend[1, lag], 1 / sqrt(Aomega_trend[1, lag]));\n\n",
      "        // Off-diagonal elements prior\n",
      "        for (i in 1:N_lv_trend) {\n",
      "          for (j in 1:N_lv_trend) {\n",
      "            if (i != j) {\n",
      "              A_raw_trend[lag, i, j] ~ normal(Amu_trend[2, lag], 1 / sqrt(Aomega_trend[2, lag]));\n",
      "            }\n",
      "          }\n",
      "        }\n",
      "      }"
    )
  }

  # Pre-compute VARMA MA priors to avoid nested glue::glue() scoping issues
  varma_ma_priors <- if(is_varma) {
    paste0(
      "      // Hierarchical priors for VARMA MA coefficient matrices (D_raw_trend)\n",
      "      // Following same structure as VAR coefficients but conditional on ma_lags > 0\n",
      "      for (ma_lag in 1:", ma_lags, ") {\n",
      "        // Diagonal elements prior for MA coefficients\n",
      "        diagonal(D_raw_trend[ma_lag]) ~ normal(Dmu_trend[1, ma_lag], 1 / sqrt(Domega_trend[1, ma_lag]));\n\n",
      "        // Off-diagonal elements prior for MA coefficients\n",
      "        for (i in 1:N_lv_trend) {\n",
      "          for (j in 1:N_lv_trend) {\n",
      "            if (i != j) {\n",
      "              D_raw_trend[ma_lag, i, j] ~ normal(Dmu_trend[2, ma_lag], 1 / sqrt(Domega_trend[2, ma_lag]));\n",
      "            }\n",
      "          }\n",
      "        }\n",
      "      }\n\n",
      "      // Hyperpriors for hierarchical MA coefficient means and precisions\n",
      "      Dmu_trend[1, 1] ~ normal(0.0, 1.0);\n",
      "      Domega_trend[1, 1] ~ gamma(2.0, 1.0);\n",
      "      Dmu_trend[2, 1] ~ normal(0.0, 1.0);\n",
      "      Domega_trend[2, 1] ~ gamma(2.0, 1.0);"
    )
  } else ""

  # Create transpose strings to avoid quote escaping issues in glue
  lv_transpose <- "lv_trend[t, :]'"
  lv_transpose_lag <- "lv_trend[t - i, :]'"
  lv_transpose_prev <- "lv_trend[t - 1, :]'"
  
  # Conditional correlation prior for hierarchical vs non-hierarchical
  omega_prior <- if(!is_hierarchical) {
    "// LKJ correlation prior on Cholesky factor\n      L_Omega_trend ~ lkj_corr_cholesky(2);"
  } else {
    "// Hierarchical correlation priors handled by add_hierarchical_support()"
  }
  
  var_model_stanvar <- brms::stanvar(
    name = "var_model",
    scode = glue::glue("
      // VARMA likelihood implementation following Heaps 2022 methodology

      // Initial joint distribution for stationary VARMA initialization
      vector[{init_dim_expr}] mu_init_trend = rep_vector(0.0, {init_dim_expr});
      init_trend ~ multi_normal(mu_init_trend, Omega_trend);

      // Conditional means for VARMA dynamics
      vector[N_lv_trend] mu_t_trend[N_trend];

      // Compute conditional means for all time points
      for (t in 1:N_trend) {{
        mu_t_trend[t] = rep_vector(0.0, N_lv_trend);

        // VAR component: Add autoregressive terms
        for (i in 1:{lags}) {{
          if (t - i <= 0) {{
            // Use values from earlier than series start (from init_trend)
            int init_idx = {lags} + 1 - i;
            if (init_idx > 0 && init_idx <= {lags}) {{
              vector[N_lv_trend] lagged_lv;
              int start_idx = (init_idx - 1) * N_lv_trend + 1;
              int end_idx = init_idx * N_lv_trend;
              lagged_lv = init_trend[start_idx:end_idx];
              mu_t_trend[t] += A_trend[i] * lagged_lv;
            }}
          }} else {{
            // Use regular lv_trend values
            mu_t_trend[t] += A_trend[i] * {lv_transpose_lag};
          }}
        }}

        {if(is_varma) glue::glue('
        // MA component: Add moving average term for VARMA
        if (t - 1 <= 0) {{
          // Use initial MA error for first time point
          mu_t_trend[t] += D_trend[1] * ma_init_trend;
        }} else {{
          // Compute MA error inline (eliminates circular dependency)
          mu_t_trend[t] += D_trend[1] * ({lv_transpose_prev} - mu_t_trend[t - 1]);
        }}
        ') else ''}
      }}

      // Latent variable dynamics
      for (t in 1:N_trend) {{
        {lv_transpose} ~ multi_normal(mu_t_trend[t], Sigma_trend);
      }}

      {var_priors_block}

      {varma_ma_priors}

      {omega_prior}

      // Hyperpriors for hierarchical VAR coefficient means and precisions
      for (lag in 1:2) {{
        Amu_trend[lag] ~ normal(0, sqrt(0.455));
        Aomega_trend[lag] ~ gamma(1.365, 0.071175);
      }}
    "),
    block = "model"
  )

  # Generate centralized priors for trend parameters
  # Standard parameters: sigma_trend (L_Omega_trend handled by LKJ in model block)
  # Hierarchical hyperparameters: Amu_trend, Aomega_trend, and conditional MA/group parameters
  if (is_hierarchical) {
    # Hierarchical models: sigma_group_trend handled by shared hierarchical system
    var_params_to_prior <- c("Amu_trend", "Aomega_trend")
  } else {
    # Non-hierarchical models use sigma_trend
    var_params_to_prior <- c("sigma_trend", "Amu_trend", "Aomega_trend")
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
    # VARMA case: include MA parameters (8 components: Z + var-specific + data)
    list(matrix_z, var_functions_stanvar, var_tdata_stanvar, var_data_stanvar,
         var_parameters_stanvar, var_ma_parameters_stanvar,
         var_tparameters_stanvar, var_model_stanvar)
  } else {
    # VAR-only case: no MA parameters (7 components: Z + var-specific + data)
    list(matrix_z, var_functions_stanvar, var_tdata_stanvar, var_data_stanvar,
         var_parameters_stanvar, var_tparameters_stanvar, var_model_stanvar)
  }

  # Add trend computation (required for all VAR models)
  components <- append_if_not_null(base_components, trend_computation)

  # Add VAR hierarchical correlation parameters required for compilation
  # Use DRY approach via selective calls to avoid architectural conflicts from add_hierarchical_support() 
  if (is_hierarchical && !is.null(hierarchical_info)) {
    # Validate hierarchical_info has required fields
    checkmate::assert_names(names(hierarchical_info), 
                            must.include = c("n_groups", "n_subgroups"))
    
    # Add data structures (dimensions and mappings)
    hierarchical_data <- generate_hierarchical_data_structures(hierarchical_info, data_info)
    components <- append_if_not_null(components, hierarchical_data)
    
    # Add correlation parameters (alpha_cor_trend, L_Omega_global_trend, etc.)
    # Exclude computed parameters since VAR handles Sigma_group_trend locally
    hierarchical_params <- generate_hierarchical_correlation_parameters(
      hierarchical_info$n_groups, 
      hierarchical_info$n_subgroups, 
      "VAR",  # trend_type for conditional logic
      exclude_computed = TRUE  # VAR handles Sigma_group_trend locally
    )
    components <- append_if_not_null(components, hierarchical_params)
    
    # Add priors and functions
    hierarchical_priors <- generate_hierarchical_correlation_model(hierarchical_info$n_groups, prior)
    hierarchical_functions <- generate_hierarchical_functions()
    components <- append_if_not_null(components, hierarchical_priors)
    components <- append_if_not_null(components, hierarchical_functions)
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
      dis_time = pmax(1e-3, dis_time)
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
    scode = glue::glue("array[N_trend, N_series_trend] real<lower=0> time_dis;"),
    block = "data"
  )
  components <- append(components, list(time_dis_data_stanvar))

  # 1. PARAMETERS block - CAR trend-specific parameters
  car_parameters_stanvar <- brms::stanvar(
    name = "car_parameters",
    scode = "// CAR AR1 parameters\nvector<lower=-1,upper=1>[N_lv_trend] ar1_trend;",
    block = "parameters"
  )
  components <- append(components, list(car_parameters_stanvar))

  # CAR trends require dedicated innovation parameters since they opt out of the shared system
  car_innovation_parameters_stanvar <- brms::stanvar(
    name = "car_innovation_parameters",
    scode = "vector<lower=0>[N_lv_trend] sigma_trend;\n  matrix[N_trend, N_lv_trend] innovations_trend;",
    block = "parameters"
  )
  components <- append(components, list(car_innovation_parameters_stanvar))

  # CAR innovation computation in transformed parameters
  car_innovation_computation_stanvar <- brms::stanvar(
    name = "car_innovation_computation",
    scode = "matrix[N_trend, N_lv_trend] scaled_innovations_trend;\n  scaled_innovations_trend = innovations_trend * diag_matrix(sigma_trend);",
    block = "tparameters"
  )
  components <- append(components, list(car_innovation_computation_stanvar))

  # 2. TPARAMETERS block - CAR dynamics computation (always needed)
  car_tparameters_stanvar <- brms::stanvar(
    name = "car_tparameters",
    scode = glue::glue("
      // CAR latent variable evolution using shared innovation system

      // Initialize first time point with innovations
      for (j in 1:N_lv_trend) {{
        lv_trend[1, j] = scaled_innovations_trend[1, j];
      }}

      // Apply continuous-time AR evolution for subsequent time points
      for (j in 1:N_lv_trend) {{
        for (i in 2:N_trend) {{
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
  car_params_to_prior <- c("ar1_trend", "sigma_trend")
  # CAR trends use their own innovation system with sigma_trend priors

  if (length(car_params_to_prior) > 0) {
    car_model_stanvar <- generate_trend_priors_stanvar(
      param_names = car_params_to_prior,
      prior = prior,
      stanvar_name = "car_model"
    )
    components <- append_if_not_null(components, car_model_stanvar)
  }

  # 3.5. Add sampling statement for CAR innovations_trend parameter
  car_innovations_sampling_stanvar <- brms::stanvar(
    name = "car_innovations_sampling",
    scode = "to_vector(innovations_trend) ~ std_normal();",
    block = "model"
  )
  components <- append(components, list(car_innovations_sampling_stanvar))

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

  # STEP 3: Add hierarchical correlation support if applicable (BEFORE ZMVN stanvars)
  components <- add_hierarchical_support(components, trend_specs, data_info, prior)

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
    scode = "lv_trend = scaled_innovations_trend;",
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

        /* credit goes to the Prophet development team at Meta */
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

        /* credit goes to the Prophet development team at Meta */
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

        /* credit goes to the Prophet development team at Meta */
        vector[S] gamma;
        gamma = logistic_gamma(k, m, delta, t_change_trend, S);
        return cap_trend .* inv_logit((k + Kappa_trend * delta) .* (t - (m + Kappa_trend * gamma)));
      }

      vector linear_trend(real k, real m, vector delta, vector t, matrix Kappa_trend,
                          vector t_change_trend) {
        /* Function to compute a linear trend with changepoints */

        /* credit goes to the Prophet development team at Meta */
        return (k + Kappa_trend * delta) .* t + (m + Kappa_trend * (-t_change_trend .* delta));
      }
    ",
    block = "functions"
  )

  # Validate PW-specific parameters
  checkmate::assert_number(n_changepoints, lower = 0)
  checkmate::assert_number(changepoint_scale, lower = 0)
  t_change_values <- seq(0.1, 0.9, length.out = n_changepoints)

  # Create individual stanvars for each PW data component
  n_change_stanvar <- brms::stanvar(
    x = n_changepoints,
    name = "n_change_trend",
    scode = "int<lower=0> n_change_trend;",
    block = "data"
  )

  t_change_stanvar <- brms::stanvar(
    x = t_change_values,
    name = "t_change_trend",
    scode = "vector[n_change_trend] t_change_trend;",
    block = "data"
  )

  change_scale_stanvar <- brms::stanvar(
    x = changepoint_scale,
    name = "change_scale_trend",
    scode = "real<lower=0> change_scale_trend;",
    block = "data"
  )

  # Initialize logistic data variable
  pw_logistic_data_stanvar <- NULL

  # Logistic-specific data (carrying capacity)
  if (trend_type == "logistic") {
    pw_logistic_data_stanvar <- brms::stanvar(
      x = matrix(10, nrow = n_obs, ncol = n_series),  # Default carrying capacity
      name = "pw_logistic_data",
      scode = glue::glue("matrix[N_trend, N_lv_trend] cap_trend; // carrying capacities"),
      block = "data"
    )
  }

  # Transformed data block - time vector and changepoint matrix computation
  pw_transformed_data_stanvar <- brms::stanvar(
    name = "pw_transformed_data",
    scode = glue::glue("
      // time vector for changepoint calculations
      vector[N_trend] time_trend;
      for (i in 1:N_trend) time_trend[i] = i;
      // sorted changepoint matrix
      matrix[N_trend, n_change_trend] Kappa_trend = get_changepoint_matrix(time_trend, t_change_trend, N_trend, n_change_trend);
    "),
    block = "tdata"
  )

  # Parameters block - piecewise trend parameters
  pw_parameters_stanvar <- brms::stanvar(
    name = "pw_parameters",
    scode = glue::glue("
      // base trend growth rates
      vector[N_lv_trend] k_trend;

      // trend offset parameters
      vector[N_lv_trend] m_trend;

      // trend rate adjustments per series
      matrix[n_change_trend, N_lv_trend] delta_trend;
    "),
    block = "parameters"
  )

  # Transformed parameters block - trend computation (type-specific)
  if (trend_type == "logistic") {
    pw_transformed_parameters_stanvar <- brms::stanvar(
      name = "pw_transformed_parameters",
      scode = glue::glue("
        // Logistic piecewise trends

        // logistic trend estimates
        for (s in 1 : N_lv_trend) {{
          lv_trend[1 : N_trend, s] = logistic_trend(k_trend[s], m_trend[s],
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
        // Linear piecewise trends

        // linear trend estimates
        for (s in 1 : N_lv_trend) {{
          lv_trend[1 : N_trend, s] = linear_trend(k_trend[s], m_trend[s],
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
    n_change_stanvar,
    t_change_stanvar,
    change_scale_stanvar,
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
    response_names = response_names,
    n_time = n_time
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

  # Ensure stancode is a plain character string (handle brmsmodel class)
  if (inherits(stancode, "brmsmodel")) {
    stancode <- as.character(stancode)
  }

  stanvar_list <- list()

  # 0. Extract functions block content (without headers) and store for later
  # combination. Parse custom function names to prevent them from being renamed
  # with _trend suffix (e.g., scale_r_cor, multiply_lower_tri_self_transpose).
  # Functions block doesn't need parameter renaming since functions are global.
  # Position: Must be first to satisfy Stan's requirement that functions be
  # declared before use
  functions_block <- extract_stan_block_content(stancode, "functions")
  if (!is.null(functions_block) && nchar(functions_block) > 0) {
    # Parse custom function names to prevent renaming
    # brms generates different custom functions based on formula structure
    # (e.g., scale_r_cor for correlated random effects)
    functions_list <- parse_stan_functions(functions_block)
    if (length(functions_list) > 0) {
      mapping$custom_functions <- vapply(
        functions_list,
        function(f) f$name,
        character(1)
      )
    }

    stanvar_list[["trend_functions"]] <- brms::stanvar(
      scode = functions_block,
      block = "functions"
    )
  }

  # Extract data block declarations from Stan code with selective filtering
  # This complements extract_and_rename_standata_objects by providing proper declarations

  # 1. Extract data block content with selective filtering and renaming
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

  # 1. Extract transformed data block content (without headers) and rename references
  # This captures brms centering code like Xc = X - means_X
  tdata_block <- extract_stan_block_content(stancode, "transformed data")
  if (!is.null(tdata_block) && nchar(tdata_block) > 0) {
    tdata_result <- rename_parameters_in_block(
      tdata_block, suffix, mapping, "tdata", is_multivariate, response_names
    )
    mapping <- tdata_result$mapping  # Update mapping

    stanvar_list[["trend_transformed_data"]] <- brms::stanvar(
      scode = tdata_result$code,
      block = "tdata"
    )
  }

  # 2. Extract parameters block content (without headers) and rename parameters
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

      # Filter smooth coefficients - handled in mu_creation stanvar for correct ordering
      tparams_lines <- strsplit(tparams_result$code, "\n")[[1]]
      decl_pattern <- "^\\s*vector\\s*\\[.*\\]\\s+s_[0-9]+_[0-9]+_trend\\s*;"
      assign_pattern <- "^\\s*s_[0-9]+_[0-9]+_trend\\s*="
      keep_lines <- !grepl(decl_pattern, tparams_lines) &
                    !grepl(assign_pattern, tparams_lines)
      filtered_tparams_code <- paste(tparams_lines[keep_lines], collapse = "\n")

      stanvar_list[["trend_tparameters"]] <- brms::stanvar(
        scode = filtered_tparams_code,
        block = "tparameters"
      )
    }
  }

  # 3. Create mu_trend using variable-tracing system for complex patterns FIRST
  # This prevents duplicate mu construction by extracting mu lines before model block processing
  # Check if mu_trend declaration already exists in stancode
  mu_trend_pattern <- paste0("vector\\[.*?\\]\\s+mu", gsub("_", "\\_", suffix))
  mu_trend_exists <- any(grepl(mu_trend_pattern,
                              strsplit(stancode, "\n")[[1]],
                              perl = TRUE))

  extracted_mu_lines <- character(0)  # Track extracted mu lines for model block filtering

  if (!mu_trend_exists) {
    checkmate::assert_character(stancode, len = 1, min.chars = 1)
    checkmate::assert_list(mapping, names = "named")
    checkmate::assert_character(suffix, len = 1, min.chars = 1)

    time_param <- paste0("N", suffix)

    mu_construction_result <- extract_mu_construction_with_classification(stancode)

    if (length(mu_construction_result$mu_construction) > 0) {
      # Store extracted mu lines AND supporting declarations to filter them from model block (DRY solution)
      extracted_mu_lines <- c(
        mu_construction_result$mu_construction,
        mu_construction_result$supporting_declarations
      )

      required_vars <- mu_construction_result$referenced_variables
      missing_vars <- setdiff(required_vars, names(mapping$original_to_renamed))

      # Search all Stan blocks for missing variables
      if (length(missing_vars) > 0) {
        # Extract computed variables from entire stancode
        all_computed_vars <- extract_computed_variables(stancode)

        # Find declared variables in all Stan blocks (data, parameters, etc.)
        all_declared_vars <- find_variable_declarations(stancode, missing_vars)

        # Combine both computed and declared variables
        all_available_vars <- unique(c(all_computed_vars, all_declared_vars))

        # Add any missing variables that exist in stancode
        added_vars <- character(0)
        for (missing_var in missing_vars) {
          if (missing_var %in% all_available_vars) {
            renamed_var <- paste0(missing_var, suffix)
            mapping$original_to_renamed[[missing_var]] <- renamed_var
            mapping$renamed_to_original[[renamed_var]] <- missing_var
            added_vars <- c(added_vars, missing_var)
          }
        }

        # Recalculate missing vars after comprehensive search
        missing_vars <- setdiff(required_vars, names(mapping$original_to_renamed))

        # Only error if there are still truly missing variables after comprehensive search
        if (length(missing_vars) > 0) {
          insight::format_error(
            "Variable mapping missing required variables: {.field {missing_vars}}. These variables were referenced in mu construction but not found in any Stan block (data, parameters, transformed data, transformed parameters, or computed variables)."
          )
        }
      }

      mu_trend_code_lines <- reconstruct_mu_trend_with_renamed_vars(
        mu_construction = mu_construction_result$mu_construction,
        supporting_declarations = mu_construction_result$supporting_declarations,
        variable_mapping = mapping$original_to_renamed,
        time_param = time_param
      )

      stanvar_list[["trend_model_mu_creation"]] <- brms::stanvar(
        scode = paste(mu_trend_code_lines, collapse = "\n"),
        block = "tparameters"
      )
      create_mu_stanvar <- FALSE  # Stanvar already created above

    } else {
      create_mu_stanvar <- TRUE
      has_coefficients <- grepl("vector\\[.*\\]\\s+b[^_]", stancode) &&
                          (grepl("matrix\\[.*\\]\\s+Xc[^_]", stancode) ||
                           grepl("matrix\\[.*\\]\\s+X[^_]", stancode))

      if (has_coefficients) {
        has_xc <- grepl("matrix\\[.*\\]\\s+Xc[^_]", stancode) ||
                  grepl("matrix\\[.*\\]\\s+X[^_]", stancode)

        if (!has_xc) {
          insight::format_error(
            "Expected design matrix {.field Xc{suffix}} not found in Stan code."
          )
        }

        # Validate inputs following project standards
        checkmate::assert_string(stancode, min.chars = 1)
        checkmate::assert_string(suffix, min.chars = 1)
        checkmate::assert_string(time_param, min.chars = 1)

        # Check parameter existence using consistent patterns
        intercept_param <- paste0("Intercept", suffix)
        intercept_present <- grepl("real.*Intercept[^_]", stancode)
        
        # Determine which covariate parameter is present
        xc_present <- grepl("matrix\\[.*\\]\\s+Xc[^_]", stancode)
        x_present <- grepl("matrix\\[.*\\]\\s+X[^_]", stancode)
        covariates_present <- xc_present || x_present
        covariate_param <- if (xc_present) paste0("Xc", suffix) else paste0("X", suffix)

        # Build mu construction efficiently  
        base_declaration <- paste0("vector[", time_param, "] mu", suffix, " = ")
        zero_vector <- paste0("rep_vector(0.0, ", time_param, ")")
        intercept_vector <- paste0("rep_vector(", intercept_param, ", ", time_param, ")")

        # Handle the 4 cases systematically
        if (!intercept_present && !covariates_present) {
          # Case 1: No terms
          mu_trend_code <- paste0(base_declaration, zero_vector, ";")
        } else if (intercept_present && !covariates_present) {
          # Case 2: Intercept only - use efficient rep_vector
          mu_trend_code <- paste0(base_declaration, intercept_vector, ";")
        } else if (!intercept_present && covariates_present) {
          # Case 3: Covariates only
          mu_trend_code <- paste0(base_declaration, zero_vector, ";\n  mu", suffix,
                                  " += ", covariate_param, " * b", suffix, ";")
        } else {
          # Case 4: Both intercept and covariates
          mu_trend_code <- paste0(base_declaration, zero_vector, ";\n  mu", suffix,
                                  " += ", intercept_param, " + ", covariate_param,
                                  " * b", suffix, ";")
        }
      } else {
        intercept_present <- grepl("real.*Intercept[^_]", stancode)
        if (intercept_present) {
          mu_trend_code <- paste0("vector[", time_param, "] mu", suffix,
                                 " = rep_vector(Intercept", suffix, ", ",
                                 time_param, ");")
        } else {
          mu_trend_code <- paste0("vector[", time_param, "] mu", suffix,
                                 " = rep_vector(0.0, ", time_param, ");")
        }
      }

      # Create fallback stanvar
      stanvar_list[["trend_model_mu_creation"]] <- brms::stanvar(
        scode = mu_trend_code,
        block = "tparameters"
      )
    }

    mapping$original_to_renamed[["mu"]] <- paste0("mu", suffix)
    mapping$renamed_to_original[[paste0("mu", suffix)]] <- "mu"
  }

  # 4. Extract model block content (without headers) but exclude likelihood statements
  # Process AFTER mu_trend extraction to avoid duplicate mu construction (DRY solution)
  model_block <- extract_stan_block_content(stancode, "model")
  model_stanvar_created <- FALSE

  if (!is.null(model_block) && nchar(model_block) > 0) {
    # Extract non-likelihood parts (priors, transformations) and exclude extracted mu lines
    non_likelihood_model <- extract_non_likelihood_from_model_block(model_block, extracted_mu_lines)

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

  # 5. Extract generated quantities block content (without headers)
  genquant_block <- extract_stan_block_content(stancode, "generated quantities")
  
  if (!is.null(genquant_block) && nchar(trimws(genquant_block)) > 0) {
    genquant_result <- rename_parameters_in_block(
      genquant_block, suffix, mapping, "generated quantities", is_multivariate, response_names
    )
    mapping <- genquant_result$mapping  # Update mapping

    stanvar_list[["trend_generated_quantities"]] <- brms::stanvar(
      scode = genquant_result$code,
      block = "genquant"
    )
  }

  # Combine individual stanvar objects into proper stanvars collection
  combined_stanvars <- combine_stanvars(stanvar_list)

  return(list(
    stanvars = combined_stanvars,
    mapping = mapping
  ))
}

#' Should Include Declaration in Transformed Parameters
#'
#' Determines if a variable declaration should be included in transformed parameters block.
#' Only computed variables (GP predictions, splines, etc.) should be moved from model block.
#' Data/parameter block variables should stay in their original blocks.
#'
#' @param declaration Character string of Stan variable declaration
#' @return Logical indicating if declaration should be included in transformed parameters
#' @noRd
should_include_in_transformed_parameters <- function(declaration) {
  checkmate::assert_string(declaration, min.chars = 1)

  # Skip empty declarations
  if (nchar(trimws(declaration)) == 0) return(FALSE)

  # SEMANTIC RULE 1: No assignment = data/parameter declaration
  # Pure declarations like "int N;" or "vector[N] X;" belong in data/parameters blocks
  has_assignment <- grepl("=", declaration)
  if (!has_assignment) {
    return(FALSE)
  }

  # SEMANTIC RULE 2: Exclude simple initializations that don't need dependency ordering
  # These are basic Stan initializations that can be computed anywhere
  simple_initialization_patterns <- c(
    # Zero initializations
    "=\\s*rep_vector\\s*\\(\\s*0\\.",
    "=\\s*rep_matrix\\s*\\(\\s*0\\.",

    # Simple constant vectors/matrices (any numeric value)
    "=\\s*rep_vector\\s*\\(\\s*[0-9]",
    "=\\s*rep_matrix\\s*\\(\\s*[0-9]",

    # Simple scalar constants (integer or float)
    "=\\s*[0-9]+\\.?[0-9]*\\s*;\\s*$"
  )

  # Check if it's a simple initialization
  for (pattern in simple_initialization_patterns) {
    if (grepl(pattern, declaration, perl = TRUE)) {
      return(FALSE)
    }
  }

  # SEMANTIC RULE 3: Complex assignments should be included
  # If it has assignment and isn't simple initialization, it's a computation that needs ordering
  # This includes GP computations, matrix operations, function calls, etc.
  return(TRUE)
}

#' Find Variable Declarations Across All Stan Blocks
#'
#' Scans all Stan blocks to find declarations for referenced variables
#'
#' Extract variable dependencies from a Stan declaration
#'
#' Parses a Stan variable declaration to find what variables it depends on.
#' This is used for recursive dependency resolution in GP computations.
#'
#' @param declaration Character string containing a Stan variable declaration
#' @return Character vector of variable names this declaration depends on
#' @examples
#' \dontrun{
#' # Example declaration: "vector[Nsubgp_1] gp_pred_1 = Xgp_1 * rgp_1;"
#' # Returns: c("Nsubgp_1", "Xgp_1", "rgp_1")
#' extract_dependencies_from_declaration(declaration)
#' }
#' @noRd
extract_dependencies_from_declaration <- function(declaration) {
  checkmate::assert_string(declaration)

  if (nchar(trimws(declaration)) == 0) {
    return(character(0))
  }

  # Extract right-hand side of assignment if present
  if (grepl("=", declaration)) {
    rhs_match <- regmatches(declaration, regexpr("=\\s*([^;]+)", declaration, perl = TRUE))
    if (length(rhs_match) > 0) {
      rhs <- gsub("^=\\s*", "", rhs_match)

      # Extract identifiers from RHS using existing Stan identifier extraction
      identifiers <- extract_stan_identifiers(rhs)

      # Filter out known Stan functions, operators, and literals
      stan_functions <- get_stan_reserved_words()

      # Keep only variables (not functions or numeric literals)
      variables <- identifiers[!identifiers %in% stan_functions &
                              !grepl("^[0-9.]+$", identifiers)]

      return(variables)
    }
  }

  return(character(0))
}

#' Find Variable Declarations with Recursive Dependency Resolution
#'
#' Searches Stan code for variable declarations, following dependency chains
#' recursively. This is crucial for GP models where gp_pred_1 depends on rgp_1,
#' which in turn depends on other variables.
#'
#' @param stancode Character string containing complete Stan model code
#' @param referenced_vars Character vector of variable names to find declarations for
#' @return Character vector of variable declaration lines in dependency order
#' @examples
#' \dontrun{
#' # When searching for gp_pred_1, will also find rgp_1 declaration
#' declarations <- find_variable_declarations(stan_code, c("gp_pred_1"))
#' # Returns both: rgp_1 declaration and gp_pred_1 declaration
#' }
#' @noRd
find_variable_declarations <- function(stancode, referenced_vars,
                                    search_blocks = c("data", "transformed data", "parameters", "transformed parameters", "model"),
                                    sort_dependencies = FALSE) {
  if (length(referenced_vars) == 0) {
    return(character(0))
  }

  checkmate::assert_string(stancode)
  checkmate::assert_character(referenced_vars)
  checkmate::assert_character(search_blocks, min.len = 1)
  checkmate::assert_logical(sort_dependencies, len = 1)

  # Stan blocks to search for variable declarations
  blocks_to_search <- search_blocks

  # RECURSIVE DEPENDENCY RESOLUTION ALGORITHM (DEPTH-FIRST)
  already_searched <- character(0)  # Prevent infinite loops
  all_declarations <- character(0)  # Accumulate found declarations

  # Internal recursive function for depth-first traversal
  process_variable <- function(var_name) {
    # Skip if already processed (prevents infinite recursion)
    if (var_name %in% already_searched) {
      return()
    }

    # Search for current variable's declaration and assignment across all blocks
    all_matches <- character(0)
    for (block_name in blocks_to_search) {
      block_content <- extract_stan_block_content(stancode, block_name)

      if (is.null(block_content) || nchar(trimws(block_content)) == 0) {
        next
      }

      # Split into lines and clean
      lines <- strsplit(block_content, "\n")[[1]]
      lines <- trimws(lines)
      lines <- lines[nchar(lines) > 0]

      # Create regex pattern for this variable
      # brms generates smooth coefficients as two separate statements:
      # 1. Declaration without assignment: vector[knots_1[1]] s_1_1;
      # 2. Assignment in model block: s_1_1 = sds_1[1] * zs_1_1;
      # Standard declaration pattern misses (2), causing ordering errors
      escaped_var <- gsub("([.^$*+?{}\\[\\]()\\\\|])", "\\\\\\1", var_name)
      decl_pattern <- paste0(
        "(real|int|vector|matrix|array).*?\\b", escaped_var, "\\b.*?[;=]"
      )
      # Standalone assignment: var_name = expression; (anchored to line start)
      assign_pattern <- paste0(escaped_var, "\\s*=\\s*[^;]+;")
      var_pattern <- paste0("^\\s*(", decl_pattern, "|", assign_pattern, ")")

      matches <- grep(var_pattern, lines, value = TRUE, perl = TRUE)
      if (length(matches) > 0) {
        # Collect all matches from this block
        all_matches <- c(all_matches, matches)
      }
    }

    # Mark as processed before processing dependencies (prevents cycles)
    already_searched <<- c(already_searched, var_name)

    # If any matches found, process dependencies and add all matches
    if (length(all_matches) > 0) {
      # RECURSIVE STEP: Extract and process dependencies from all matches
      dependencies <- character(0)
      for (match in all_matches) {
        match_deps <- extract_dependencies_from_declaration(match)
        dependencies <- unique(c(dependencies, match_deps))
      }

      # Process each dependency recursively before adding declarations
      for (dep_var in dependencies) {
        if (!dep_var %in% already_searched) {
          process_variable(dep_var)
        }
      }

      # Add all matches (declaration and assignment) in order found
      all_declarations <<- c(all_declarations, all_matches)
    }
  }

  # Process each originally requested variable
  for (var in referenced_vars) {
    process_variable(var)
  }

  # Return unique declarations (dependency order naturally preserved by algorithm)
  return(unique(all_declarations))
}


#' Reconstruct mu_trend with Renamed Variables
#'
#' Transforms brms mu construction patterns into mu_trend patterns by applying
#' variable renaming with _trend suffixes. Handles complex expressions including
#' GP predictions, random effects, and splines.
#'
#' @param mu_construction Character vector of mu assignment expressions from extract_mu_construction_from_model_block()
#' @param supporting_declarations Character vector of variable declarations that support mu expressions
#' @param variable_mapping Named list mapping original variable names to renamed versions (original -> renamed_trend)
#' @param time_param Character string specifying time dimension parameter name (default: "N_trend")
#' @return Character vector of Stan code lines for mu_trend construction
#' @examples
#' \dontrun{
#' mu_exprs <- c("mu += Intercept + gp_pred_1[Jgp_1];")
#' support_decls <- c("vector[Nsubgp_1] gp_pred_1 = gp_exp_quad(Xgp_1, sdgp_1, lscale_1, zgp_1);")
#' var_map <- list("Intercept" = "Intercept_trend", "gp_pred_1" = "gp_pred_1_trend", "Jgp_1" = "Jgp_1_trend")
#' reconstruct_mu_trend_with_renamed_vars(mu_exprs, support_decls, var_map)
#' }
#' @noRd
reconstruct_mu_trend_with_renamed_vars <- function(mu_construction, supporting_declarations, variable_mapping, time_param = "N_trend") {
  # Enhanced validation following project standards
  checkmate::assert_character(mu_construction)
  checkmate::assert_character(supporting_declarations)
  checkmate::assert_list(variable_mapping, types = "character", names = "named", min.len = 1)
  checkmate::assert_string(time_param, pattern = "^[A-Za-z][A-Za-z0-9_]*$")

  if (length(mu_construction) == 0) {
    return(character(0))
  }

  # Validate consistent input - if mu_construction exists, we should have mapping
  if (length(variable_mapping) == 0) {
    insight::format_error(
      "mu construction expressions found but no {.field variable_mapping} provided. Variable mapping is required for renaming."
    )
  }

  # Sort variable names by length (descending) to prevent partial replacements
  sorted_var_names <- names(variable_mapping)[order(-nchar(names(variable_mapping)))]

  # Filter and rename supporting declarations - only include computed variables from model block
  renamed_supporting_decls <- character(0)

  # Create local copy of mapping to avoid side effects
  local_mapping <- variable_mapping
  
  for (decl in supporting_declarations) {
    if (nchar(trimws(decl)) == 0) {  # Skip empty declarations
      next
    }

    # Only include computed variables (GP, splines, random effects, etc.) - NOT data/parameter declarations
    # Use registry-aware checking to prevent duplicates
    include <- should_include_in_transformed_parameters(decl)

    # Include smooth coefficient declarations with split declaration+assignment pattern
    # brms generates: vector[k] s_1_1; followed by s_1_1 = sds_1[1] * zs_1_1;
    # These need to be in mu_creation for correct ordering because sort_stanvars
    # puts them in "others" category which appears AFTER mu_trend.
    # Random effects (r_*) are NOT included here - sort_stanvars classifies them
    # to level0_re_declarations which appears BEFORE mu_trend.
    if (!include && !grepl("=", decl) &&
        grepl("^\\s*(vector|matrix|real|int|array)", decl)) {
      var_match <- regmatches(decl, regexec("([a-zA-Z_][a-zA-Z0-9_]*)\\s*;", decl))[[1]]
      if (length(var_match) > 1) {
        var_name <- var_match[2]
        # Only include smooth coefficients (s_N_N pattern from brms)
        if (grepl("^s_[0-9]+_[0-9]+$", var_name)) {
          assign_pattern <- paste0("^\\s*", var_name, "\\s*=")
          include <- any(grepl(assign_pattern, supporting_declarations))
        }
      }
    }

    if (include) {
      # Extract the variable being declared (if this is a declaration with assignment)
      # Pattern: type[dims] var_name = expression
      decl_pattern <- "\\b(?:vector|matrix|real|int|array)\\s*(?:\\[[^\\]]*\\])*\\s+([a-zA-Z_][a-zA-Z0-9_]*)\\s*="
      decl_match <- regmatches(decl, regexec(decl_pattern, decl, perl = TRUE))[[1]]
      
      if (length(decl_match) > 1) {
        declared_var <- decl_match[2]
        # Validate variable name format
        if (grepl("^[a-zA-Z_][a-zA-Z0-9_]*$", declared_var)) {
          # Add to local mapping if not already there and doesn't already have _trend suffix
          if (!declared_var %in% names(local_mapping) && !grepl("_trend$", declared_var)) {
            local_mapping[[declared_var]] <- paste0(declared_var, "_trend")
          }
        }
      }
      
      # Apply renaming using the updated local mapping
      renamed_decl <- decl
      sorted_local_vars <- names(local_mapping)[order(-nchar(names(local_mapping)))]
      for (original_var in sorted_local_vars) {
        renamed_var <- local_mapping[[original_var]]
        renamed_decl <- gsub(paste0("\\b", original_var, "\\b"), renamed_var, renamed_decl)
      }
      renamed_supporting_decls <- c(renamed_supporting_decls, renamed_decl)
    }
  }

  # Transform mu expressions to mu_trend expressions with loop structure preservation
  mu_trend_expressions <- character(0)
  loop_expressions <- character(0)

  for (expr in mu_construction) {
    if (nchar(trimws(expr)) == 0) {  # Skip empty expressions
      next
    }

    # Check if this expression requires a loop (has mu[n] pattern)
    requires_loop <- grepl("\\bmu\\[n\\]", expr)

    if (requires_loop) {
      # Transform mu[n] to mu_trend[n] and apply variable renaming
      trend_expr <- gsub("\\bmu\\[n\\]", "mu_trend[n]", expr)

      # Apply variable renaming in sorted order
      for (original_var in sorted_var_names) {
        renamed_var <- variable_mapping[[original_var]]
        trend_expr <- gsub(paste0("\\b", original_var, "\\b"), renamed_var, trend_expr)
      }

      # Store as loop expression (will be wrapped in for loop later)
      loop_expressions <- c(loop_expressions, paste0("  ", trend_expr))

    } else {
      # Simple assignment - replace 'mu' with 'mu_trend'
      trend_expr <- gsub("\\bmu\\b", "mu_trend", expr)

      # Apply variable renaming in sorted order
      for (original_var in sorted_var_names) {
        renamed_var <- variable_mapping[[original_var]]
        trend_expr <- gsub(paste0("\\b", original_var, "\\b"), renamed_var, trend_expr)
      }

      mu_trend_expressions <- c(mu_trend_expressions, trend_expr)
    }
  }

  # Create complete mu_trend construction code
  # Check if initialization already exists in ORIGINAL mu_construction
  # to avoid duplication
  has_mu_initialization <- any(grepl("vector\\[.*\\]\\s+mu\\s*=\\s*rep_vector",
                                   mu_construction))

  # Define patterns for computed variable declarations that must come first
  # These patterns identify Stan computed variables that need declaration
  # before usage
  gp_pattern <- "^\\s*vector\\[.*\\]\\s+gp_pred_[0-9]+_trend\\s*="
  spline_pattern <- "^\\s*vector\\[.*\\]\\s+s_[0-9]+_[0-9]+_trend\\s*="
  mono_pattern <- "^\\s*vector\\[.*\\]\\s+mo_[a-zA-Z0-9_]+_trend\\s*="
  # General vector/matrix declaration pattern (excluding mu_trend init)
  general_decl_pattern <- paste0(
    "^\\s*(vector|matrix|array)\\[.*\\]\\s+[a-zA-Z_][a-zA-Z0-9_]*_trend\\s*=",
    "(?!.*mu_trend\\s*=\\s*rep_vector)"
  )

  # Separate computed variable declarations from mu_trend expressions
  # Computed vars must come BEFORE mu_trend usage for Stan compilation
  computed_var_declarations <- character(0)
  mu_only_expressions <- character(0)

  for (expr in mu_trend_expressions) {
    # Check if this expression is a computed variable declaration
    is_computed_declaration <- (
      grepl(gp_pattern, expr, perl = TRUE) ||
      grepl(spline_pattern, expr, perl = TRUE) ||
      grepl(mono_pattern, expr, perl = TRUE) ||
      grepl(general_decl_pattern, expr, perl = TRUE)
    )

    if (is_computed_declaration) {
      computed_var_declarations <- c(computed_var_declarations, expr)
    } else {
      mu_only_expressions <- c(mu_only_expressions, expr)
    }
  }

  # Sort computed variable declarations by dependencies for proper Stan compilation order
  # This fixes GP dependency ordering issues where gp_pred_* uses rgp_* before definition
  if (length(computed_var_declarations) > 1) {
    # Extract variable names from computed declarations
    var_names <- character(length(computed_var_declarations))
    for (i in seq_along(computed_var_declarations)) {
      # Use robust pattern to extract declared variable name (LHS of assignment)
      var_match <- regmatches(computed_var_declarations[i], regexpr("\\b[a-zA-Z_][a-zA-Z0-9_]*_trend\\b", computed_var_declarations[i]))
      if (length(var_match) > 0) {
        var_names[i] <- var_match[1]
      }
    }
    var_names <- var_names[nchar(var_names) > 0]

    # Use enhanced dependency resolution with depth-first traversal
    # Only search transformed parameters and model blocks (relevant for mu_trend)
    if (length(var_names) > 0) {
      full_context <- paste(c(mu_construction, supporting_declarations, computed_var_declarations), collapse = "\n")
      dependency_ordered_decls <- find_variable_declarations(
        stancode = full_context,
        referenced_vars = var_names,
        search_blocks = c("transformed parameters", "model")
      )

      # Map dependency-ordered declarations back to our renamed computed_var_declarations
      if (length(dependency_ordered_decls) > 0) {
        sorted_computed_vars <- character(0)
        for (ordered_decl in dependency_ordered_decls) {
          # Find matching renamed declaration
          for (i in seq_along(computed_var_declarations)) {
            if (length(var_names) >= i && nchar(var_names[i]) > 0) {
              # Match by variable name pattern (handles _trend renaming)
              base_var <- gsub("_trend$", "", var_names[i])
              if (grepl(base_var, ordered_decl, fixed = TRUE)) {
                sorted_computed_vars <- c(sorted_computed_vars, computed_var_declarations[i])
                break
              }
            }
          }
        }
        # Update with dependency-sorted order if mapping successful
        if (length(sorted_computed_vars) > 0) {
          computed_var_declarations <- sorted_computed_vars
        }
      }
    }
  }

  # Assemble final code with proper dependency ordering and loop structure
  base_code <- character(0)

  if (!has_mu_initialization) {
    # No initialization in input, add one
    init_code <- paste0("vector[", time_param,
                       "] mu_trend = rep_vector(0.0, ", time_param, ");")
    # Order: initialization, computed vars, other supporting decls, mu expressions
    base_code <- c(init_code, computed_var_declarations,
                  renamed_supporting_decls, mu_only_expressions)
  } else {
    # Initialization already exists in transformed expressions
    # Order: computed vars, other supporting decls, mu expressions
    base_code <- c(computed_var_declarations, renamed_supporting_decls,
                  mu_only_expressions)
  }

  # Add loop structure if there are loop expressions
  if (length(loop_expressions) > 0) {
    # Create for loop with proper indentation
    loop_code <- c(
      "",  # Empty line for readability
      paste0("for (n in 1:", time_param, ") {"),
      loop_expressions,
      "}"
    )
    complete_code <- c(base_code, loop_code)
  } else {
    complete_code <- base_code
  }

  return(complete_code)
}


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

      # lprior declarations and sigma priors (avoid duplication with observation model)
      grepl("^\\s*real\\s+lprior\\s*=\\s*0\\s*;", line),
      grepl("lprior\\s*\\+=\\s*student_t_lpdf\\s*\\(\\s*sigma\\s*\\|", line),

      # Skip lccdf line only if it follows sigma prior
      (is_sigma_lccdf && prev_line_was_sigma_prior)
    ))

    # Additional filtering for parameters and transformed parameters blocks
    if (block_type %in% c("parameters", "transformed_parameters")) {
      skip_line <- skip_line || any(c(
        # Skip duplicate sigma parameter declarations
        grepl("^\\s*real\\s*<[^>]*lower\\s*=\\s*0[^>]*>\\s+sigma\\s*;", line),
        grepl("^\\s*real<lower=0>\\s+sigma\\s*;", line),
        grepl("^\\s*real\\s+sigma\\s*;", line)
      ))
    }

    # Additional filtering for model block only
    if (block_type == "model") {
      skip_line <- skip_line || any(c(
        # Actual likelihood statements (not priors)
        grepl("~\\s+normal\\s*\\(", line),
        grepl("target\\s*\\+=.*normal.*lpdf\\s*\\(\\s*Y\\s*\\|", line),
        grepl("target\\s*\\+=.*normal.*glm.*lpdf\\s*\\(", line),
        grepl("target\\s*\\+=.*multi_normal.*lpdf\\s*\\(", line),
        grepl("target\\s*\\+=.*Y\\s*\\|", line),
        # Filter out lprior accumulation since observation model handles it
        grepl("^\\s*target\\s*\\+=\\s*lprior\\s*;", line),
        # Filter orphaned for loops from monotonic effects
        grepl("^\\s*for\\s*\\(\\s*n_trend\\s+in\\s+\\d+:\\s*[Nn]_?[Tt]rend\\s*\\)\\s*\\{?\\s*$", line)
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

extract_non_likelihood_from_model_block <- function(model_block, exclude_mu_lines = character(0)) {
  checkmate::assert_string(model_block)
  checkmate::assert_character(exclude_mu_lines)

  # extract_stan_block_content() already provides the complete inner content
  # No need for brace extraction since the content is already unwrapped

  # Use general filtering function for model blocks
  filtered_content <- filter_block_content(model_block, "model")

  # Additionally filter out mu construction lines if provided
  if (length(exclude_mu_lines) > 0 && !is.null(filtered_content)) {
    lines <- strsplit(filtered_content, "\n", fixed = TRUE)[[1]]
    lines <- trimws(lines)

    # Remove lines that match exclude_mu_lines and their preceding for loops if needed
    lines_to_remove <- character(0)
    
    for (mu_line in exclude_mu_lines) {
      mu_line_trimmed <- trimws(mu_line)
      if (nchar(mu_line_trimmed) > 0) {
        lines_to_remove <- c(lines_to_remove, mu_line_trimmed)
        
        # If this mu line contains mu[n] pattern, find and mark preceding for loop for removal
        if (grepl("mu\\[n\\]", mu_line_trimmed)) {
          # Find the index of this mu line
          mu_line_idx <- which(lines == mu_line_trimmed)
          if (length(mu_line_idx) > 0) {
            # Check the line immediately before it
            for (idx in mu_line_idx) {
              if (idx > 1) {
                preceding_line <- lines[idx - 1]
                if (grepl("^\\s*for\\s*\\(\\s*n\\s+in\\s+1:\\s*N\\s*\\)\\s*\\{?\\s*$", preceding_line)) {
                  lines_to_remove <- c(lines_to_remove, preceding_line)
                }
              }
            }
          }
        }
      }
    }
    
    # Remove all identified lines
    lines <- lines[!lines %in% lines_to_remove]

    filtered_content <- paste(lines, collapse = "\n")
    if (nchar(trimws(filtered_content)) == 0) {
      filtered_content <- NULL
    }
  }

  return(filtered_content)
}

#' Extract Stan Block Content with Improved Functions Block Handling
#'
#' @description
#' Extracts content from Stan code blocks. Uses specialized handling for functions
#' blocks to avoid brace counting issues with nested function definitions.
#'
#' @param stancode Character string containing full Stan code
#' @param block_name Name of block to extract ("functions", "data", "parameters", etc.)
#' @return Character string with inner block content (trimmed), or NULL if block not found
#' @details
#' For functions blocks, uses block boundary detection to avoid issues with nested braces.
#' For other blocks, uses the existing line-by-line parsing approach.
#' @noRd
extract_stan_block_content <- function(stancode, block_name) {
  checkmate::assert_string(stancode, min.chars = 1)
  checkmate::assert_string(block_name, min.chars = 1)

  # Validate basic Stan code structure
  if (!grepl("\\{", stancode)) {
    stop(insight::format_error(
      "Invalid Stan code: no block structure found in provided code."
    ), call. = FALSE)
  }

  if (block_name == "functions") {
    # Special handling for functions block to avoid nested brace issues
    lines <- strsplit(stancode, "\n")[[1]]

    # Find functions block start with comprehensive pattern
    functions_start <- which(grepl("^\\s*functions\\s*\\{", lines, ignore.case = TRUE))
    if (length(functions_start) == 0) {
      return(NULL)  # Block not found - consistent with other blocks
    }

    # Find next Stan block start (comprehensive pattern for all valid blocks)
    next_block_pattern <- paste0("^\\s*(data|parameters|transformed\\s+data|",
                                "transformed\\s+parameters|model|generated\\s+quantities)\\s*\\{")
    next_block <- which(grepl(next_block_pattern, lines, ignore.case = TRUE))
    next_block <- next_block[next_block > functions_start[1]]

    if (length(next_block) == 0) {
      # Functions block is last - find end of file or closing brace
      end_line <- length(lines)
      # Look for closing brace from end backwards
      for (i in length(lines):functions_start[1]) {
        if (grepl("^\\s*\\}\\s*$", lines[i])) {
          end_line <- i - 1
          break
        }
      }
      content_lines <- lines[(functions_start[1] + 1):end_line]
    } else {
      # Extract content between functions { and next block
      content_lines <- lines[(functions_start[1] + 1):(next_block[1] - 1)]
    }

    # Validate extracted content
    if (length(content_lines) == 0) {
      return("")  # Empty functions block
    }

    # Remove trailing closing brace if present
    last_line <- content_lines[length(content_lines)]
    if (grepl("^\\s*\\}\\s*$", last_line)) {
      content_lines <- content_lines[-length(content_lines)]
    }

    # Final validation - ensure we have actual content
    result <- paste(content_lines, collapse = "\n")
    if (nchar(trimws(result)) == 0) {
      return("")  # Empty after cleaning
    }

    return(result)

  } else {
    # Existing logic for other blocks (preserved exactly)
    lines <- strsplit(stancode, "\n")[[1]]
    in_block <- FALSE
    content_lines <- c()

    # Create pattern to match block start (handle multi-word blocks like "transformed data")
    clean_block_name <- gsub("\\s+", "\\\\s+", trimws(block_name))
    block_pattern <- paste0("^\\s*", clean_block_name, "\\s*\\{")

    for (i in seq_along(lines)) {
      line <- lines[i]

      # Check for block start
      if (grepl(block_pattern, line, ignore.case = TRUE)) {
        in_block <- TRUE
        next  # Skip the opening brace line
      }

      # Check for block end using Stan's mandatory block order
      if (in_block) {
        # For generated quantities (last block), collect everything until end
        if (tolower(gsub("\\s+", " ", trimws(block_name))) == "generated quantities") {
          content_lines <- c(content_lines, line)
          next
        }

        # For other blocks, detect next block boundary
        next_block_pattern <- switch(tolower(gsub("\\s+", " ", trimws(block_name))),
          "data" = "^\\s*transformed\\s+data\\s*\\{",
          "transformed data" = "^\\s*parameters\\s*\\{",
          "parameters" = "^\\s*transformed\\s+parameters\\s*\\{",
          "transformed parameters" = "^\\s*model\\s*\\{",
          "model" = "^\\s*generated\\s+quantities\\s*\\{"
        )

        if (!is.null(next_block_pattern) && grepl(next_block_pattern, line, ignore.case = TRUE)) {
          # Remove trailing brace if present
          if (length(content_lines) > 0 &&
              grepl("^\\s*\\}\\s*$", content_lines[length(content_lines)])) {
            content_lines <- content_lines[-length(content_lines)]
          }
          break
        }
      }

      # Collect content lines
      if (in_block) {
        content_lines <- c(content_lines, line)
      }
    }

    # Handle case where block wasn't found
    if (length(content_lines) == 0 && !in_block) {
      return(NULL)  # Block not found
    }

    # For generated quantities (last block), remove trailing closing brace
    if (tolower(gsub("\\s+", " ", trimws(block_name))) == "generated quantities" &&
        length(content_lines) > 0) {
      if (grepl("^\\s*\\}\\s*$", content_lines[length(content_lines)])) {
        content_lines <- content_lines[-length(content_lines)]
      }
    }

    return(paste(content_lines, collapse = "\n"))
  }
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
    # Pass mapping to exclude custom functions from functions block
    renameable_identifiers <- filter_renameable_identifiers(
      all_identifiers,
      mapping = mapping
    )

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
  c(
    # Stan data types
    "int", "real", "vector", "row_vector", "matrix", "array", "void",
    "simplex", "unit_vector", "ordered", "positive_ordered",
    "corr_matrix", "cov_matrix", "cholesky_factor_corr", "cholesky_factor_cov",

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

    # brms custom functions (generated in functions block)
    # These are detected dynamically but kept here as defensive programming
    "scale_r_cor",  # Generated for correlated random effects (x | group)
    "multiply_lower_tri_self_transpose",  # Correlation matrix from Cholesky
    "choose",  # Binomial coefficient function

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
    "append_row", "to_vector", "to_row_vector", "to_matrix", "to_array_1d",
    "to_array_2d",

    # Stan probability distributions (lpdf/lpmf)
    "normal_lpdf", "std_normal_lpdf", "normal_id_glm_lpdf", "student_t_lpdf", "cauchy_lpdf",
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
    "std_normal_cdf", "std_normal_ccdf", "std_normal_lcdf", "std_normal_lccdf",
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
    "normal_rng", "std_normal_rng", "student_t_rng", "cauchy_rng", "double_exponential_rng",
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

  if (nchar(trimws(stan_code)) == 0) {
    return(character(0))
  }

  # Remove comment portions from each line to avoid renaming words in comments
  # This preserves code but removes // comments that contain English words
  lines <- strsplit(stan_code, "\n")[[1]]
  code_only_lines <- gsub("//.*$", "", lines)
  code_without_comments <- paste(code_only_lines, collapse = "\n")

  # Also remove multi-line comments (/* ... */)
  code_without_comments <- gsub("/\\*.*?\\*/", "", code_without_comments, perl = TRUE)

  # Extract standalone identifiers using regex pattern for valid Stan identifiers
  # Pattern matches: letter or underscore, followed by letters, digits, underscores
  identifiers <- regmatches(
    code_without_comments,
    gregexpr("\\b[a-zA-Z_][a-zA-Z0-9_]*\\b", code_without_comments, perl = TRUE)
  )[[1]]

  # Extract computed variables from assignments (NEW: addresses computed variable mapping issue)
  computed_vars <- extract_computed_variables(code_without_comments)

  # Combine and deduplicate all identifiers
  all_identifiers <- c(identifiers, computed_vars)

  # Return unique identifiers, removing empty strings
  unique(all_identifiers[nchar(all_identifiers) > 0])
}

#' Extract computed variables from Stan assignment patterns
#'
#' Detects variables created through assignments in Stan code, such as:
#' - Type declarations with assignments: vector[N] gp_pred_1 = gp_exp_quad(...)
#' - Direct assignments: variable_name = expression
#'
#' @param stan_code Character string containing Stan code
#' @return Character vector of computed variable names
#' @noRd
extract_computed_variables <- function(stan_code) {
  checkmate::assert_character(stan_code, len = 1)

  if (nchar(trimws(stan_code)) == 0) {
    return(character(0))
  }

  computed_vars <- character(0)
  stan_reserved <- get_stan_reserved_words()

  # Pattern for typed declarations: type[optional_dims] var_name = expression
  # Matches: vector[N] gp_pred_1 = ..., matrix[N,K] r_1_1 = ..., real sigma = ...
  typed_pattern <- "\\b(?:vector|matrix|real|int|array)\\s*(?:\\[[^\\]]*\\])*\\s+([a-zA-Z_][a-zA-Z0-9_]*)\\s*="

  typed_matches <- gregexpr(typed_pattern, stan_code, perl = TRUE)[[1]]
  if (typed_matches[1] != -1) {
    for (i in seq_along(typed_matches)) {
      match_start <- typed_matches[i]
      match_length <- attr(typed_matches, "match.length")[i]
      full_match <- substr(stan_code, match_start, match_start + match_length - 1)

      var_name <- gsub(typed_pattern, "\\1", full_match, perl = TRUE)
      if (!var_name %in% stan_reserved && nchar(var_name) > 0) {
        computed_vars <- c(computed_vars, var_name)
      }
    }
  }

  # Pattern for direct assignments at line start: var_name = expression
  # Split by lines to ensure ^ anchor works correctly on each line
  lines <- strsplit(stan_code, "\n")[[1]]
  direct_pattern <- "^\\s*([a-zA-Z_][a-zA-Z0-9_]*)\\s*="

  for (line in lines) {
    if (grepl(direct_pattern, line, perl = TRUE)) {
      var_name <- gsub(direct_pattern, "\\1", line, perl = TRUE)
      # Filter out Stan reserved keywords and empty strings
      if (!var_name %in% stan_reserved && nchar(var_name) > 0) {
        computed_vars <- c(computed_vars, var_name)
      }
    }
  }

  return(computed_vars)
}

#' Filter identifiers to exclude Stan reserved words and response variables
#'
#' Removes Stan reserved words, essential system variables, and response variables
#' that should never be renamed during trend parameter extraction.
#'
#' @param identifiers Character vector of identifiers to filter
#' @return Character vector of identifiers that are safe to rename
#' @noRd
filter_renameable_identifiers <- function(identifiers,
                                          mapping = NULL) {
  checkmate::assert_character(identifiers)
  checkmate::assert_list(mapping, null.ok = TRUE)

  # Get comprehensive Stan reserved words
  reserved_words <- get_stan_reserved_words()

  # Add custom functions from mapping if available
  # These are brms-generated functions (e.g., scale_r_cor) that should not be
  # renamed
  if (!is.null(mapping) && !is.null(mapping$custom_functions)) {
    reserved_words <- c(reserved_words, mapping$custom_functions)
  }

  # Filter out reserved words (case-sensitive comparison)
  non_reserved <- identifiers[!identifiers %in% reserved_words]

  # Additional filtering for response variables and obvious non-parameters
  # Response variables should NOT be carried from trend to observation model
  exclude_patterns <- c(
    # Response variables (trend models are always univariate with Y only)
    "Y",

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
extract_and_rename_standata_objects <- function(standata, suffix, mapping, is_multivariate, response_names, n_time = NULL) {
  checkmate::assert_list(standata, names = "named")
  checkmate::assert_string(suffix)
  checkmate::assert_list(mapping)
  checkmate::assert_flag(is_multivariate)
  checkmate::assert_character(response_names, null.ok = TRUE)
  checkmate::assert_number(n_time, lower = 1, null.ok = TRUE)

  stanvar_list <- list()

  if (is_multivariate && !is.null(response_names)) {
    # Handle multivariate data objects
    stanvar_list <- extract_multivariate_standata(standata, suffix, mapping, response_names)
  } else {
    # Handle univariate data objects
    stanvar_list <- extract_univariate_standata(standata, suffix, mapping, n_time)
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
extract_univariate_standata <- function(standata, suffix, mapping, n_time = NULL) {
  checkmate::assert_list(standata, names = "named")
  checkmate::assert_character(suffix, len = 1)
  checkmate::assert_list(mapping, names = "named")
  checkmate::assert_number(n_time, lower = 1, null.ok = TRUE)

  # Validate mapping structure if data_declarations is present
  if (!is.null(mapping$data_declarations)) {
    checkmate::assert_character(mapping$data_declarations, len = 1)
  }

  stanvar_list <- list()

  # Get all standata names and filter using comprehensive approach
  all_data_names <- names(standata)
  renameable_data_names <- filter_renameable_identifiers(
    all_data_names,
    mapping = mapping
  )

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

      # Fix N_trend value for trend models - use source of truth n_time from dimensions
      if (data_name == "N" && renamed_data_name == "N_trend" && !is.null(n_time)) {
        data_value <- n_time
      }

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

  # Reorder stanvars to match brms declaration order

  # This prevents Stan compilation errors where variables are used before
  # being declared (e.g., knots_1_trend used in matrix dimension before
  # its own declaration)
  if (!is.null(declaration_lines) && length(stanvar_list) > 1) {
    # Map each stanvar name to its declaration line index
    name_to_line_idx <- vapply(names(stanvar_list), function(nm) {
      for (i in seq_along(declaration_lines)) {
        if (grepl(paste0("\\b", nm, "\\b"), declaration_lines[i])) {
          return(i)
        }
      }
      return(NA_integer_)
    }, integer(1))

    # Sort stanvar names by their declaration line order
    has_decl <- !is.na(name_to_line_idx)
    if (any(has_decl)) {
      sorted_names <- names(stanvar_list)[has_decl]
      sorted_names <- sorted_names[order(name_to_line_idx[has_decl])]

      # Reorder list (any unmatched names preserved at end as safety)
      unmatched <- names(stanvar_list)[!has_decl]
      stanvar_list <- stanvar_list[c(sorted_names, unmatched)]
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

  # Create stanvar with explicit 2D integer array declaration using new Stan syntax
  # Use both data (x) and explicit scode to override brms auto-generation
  # Reference dimension variables with _trend suffix (N_trend, N_series_trend)
  stan_declaration <- glue::glue("array[N_trend, N_series_trend] int {matrix_name};")

  brms::stanvar(
    x = times_array,
    name = matrix_name,
    scode = stan_declaration,
    block = "data"
  )
}

# =============================================================================
# SECTION 7: STAN FUNCTIONS DEDUPLICATION SYSTEM
# =============================================================================
# WHY: When combining observation and trend Stan models, both may contain
# identical functions (e.g., gp_exp_quad for Gaussian processes), causing
# Stan compilation failures. This system safely removes duplicates while
# preserving functionality and documentation.

#' Deduplicate Functions in Stan Code
#'
#' @description
#' Removes duplicate function definitions from Stan code while preserving
#' the first occurrence and merging unique documentation.
#'
#' @param stan_code Character string containing complete Stan model code
#' @return Character string with deduplicated functions block
#' @noRd
deduplicate_stan_functions <- function(stan_code) {
  checkmate::assert_character(stan_code, len = 1, any.missing = FALSE)

  if (nchar(trimws(stan_code)) == 0) {
    return(stan_code)
  }

  # Extract functions block
  functions_content <- extract_stan_functions_block(stan_code)
  if (is.null(functions_content) || nchar(trimws(functions_content)) == 0) {
    return(stan_code)  # No functions block to deduplicate
  }

  # Parse individual functions
  functions_list <- parse_stan_functions(functions_content)
  if (length(functions_list) <= 1) {
    return(stan_code)  # No potential duplicates
  }

  # Detect and remove duplicates
  unique_functions <- remove_duplicate_functions(functions_list)

  # Reconstruct Stan code with deduplicated functions
  new_functions_block <- reconstruct_functions_block(unique_functions)
  final_code <- replace_stan_functions_block(stan_code, new_functions_block)

  return(final_code)
}

#' Extract Functions Block from Stan Code
#'
#' @param stan_code Character string containing Stan code
#' @return Character string of functions block content (without "functions { }")
#' @noRd
extract_stan_functions_block <- function(stan_code) {
  checkmate::assert_character(stan_code, len = 1, any.missing = FALSE)

  lines <- strsplit(stan_code, "\n", fixed = TRUE)[[1]]

  # Find functions block start
  functions_start <- grep("^\\s*functions\\s*\\{", lines, ignore.case = TRUE)
  if (length(functions_start) == 0) {
    return(NULL)  # No functions block
  }

  # Find next block start (data, transformed data, etc.)
  block_patterns <- c("^\\s*data\\s*\\{", "^\\s*transformed\\s+data\\s*\\{",
                     "^\\s*parameters\\s*\\{", "^\\s*transformed\\s+parameters\\s*\\{",
                     "^\\s*model\\s*\\{", "^\\s*generated\\s+quantities\\s*\\{")

  next_block_lines <- c()
  for (pattern in block_patterns) {
    matches <- grep(pattern, lines, ignore.case = TRUE)
    if (length(matches) > 0) {
      next_block_lines <- c(next_block_lines, matches[matches > functions_start[1]])
    }
  }

  if (length(next_block_lines) == 0) {
    # Functions block extends to end of file
    content_lines <- lines[(functions_start[1] + 1):length(lines)]
  } else {
    # Functions block ends before next block
    next_block <- min(next_block_lines)
    content_lines <- lines[(functions_start[1] + 1):(next_block - 1)]
  }

  # Remove only the final closing brace of the functions block (if it exists)
  if (length(content_lines) > 0 && grepl("^\\s*}\\s*$", content_lines[length(content_lines)])) {
    content_lines <- content_lines[-length(content_lines)]
  }

  return(paste(content_lines, collapse = "\n"))
}

#' Parse Individual Functions from Functions Block
#'
#' @param functions_content Character string of functions block content
#' @return List of function objects with name, signature, body, and line info
#' @noRd
parse_stan_functions <- function(functions_content) {
  checkmate::assert_character(functions_content, len = 1, any.missing = FALSE)

  if (nchar(trimws(functions_content)) == 0) {
    return(list())
  }

  lines <- strsplit(functions_content, "\n", fixed = TRUE)[[1]]
  functions_list <- list()
  i <- 1

  while (i <= length(lines)) {
    line <- lines[i]

    # Skip empty lines, comments, and closing braces
    if (grepl("^\\s*$", line) || grepl("^\\s*//", line) ||
        grepl("^\\s*/\\*", line) || grepl("^\\s*}\\s*$", line)) {
      i <- i + 1
      next
    }

    # Check for function start (handle multi-line signatures)
    # First check if this line starts a function signature
    if (grepl("^\\s*[a-zA-Z_].*\\s+[a-zA-Z_][a-zA-Z0-9_]*\\s*\\(", line)) {
      # Accumulate lines until we find the opening brace
      signature_lines <- c(line)
      j <- i + 1

      # Keep reading until we find the opening brace
      while (j <= length(lines) && !grepl("\\{", paste(signature_lines, collapse = " "))) {
        signature_lines <- c(signature_lines, lines[j])
        j <- j + 1
      }

      # Join all signature lines into one
      full_signature <- paste(signature_lines, collapse = " ")

      # Now match the complete signature
      func_match <- regexpr("^\\s*([a-zA-Z_][a-zA-Z0-9_<>,\\[\\]\\s]*?)\\s+([a-zA-Z_][a-zA-Z0-9_]*)\\s*\\(([^)]*)\\)\\s*\\{", full_signature, perl = TRUE)

      if (func_match[1] > 0) {
        # Set i to the line after the opening brace for body parsing
        i <- j

        # Extract function components
        captures <- attr(func_match, "capture.start")
        capture_lengths <- attr(func_match, "capture.length")

        return_type <- trimws(substr(full_signature, captures[1], captures[1] + capture_lengths[1] - 1))
        function_name <- trimws(substr(full_signature, captures[2], captures[2] + capture_lengths[2] - 1))
        parameters <- trimws(substr(full_signature, captures[3], captures[3] + capture_lengths[3] - 1))

        # Find the end of this function by counting braces
        start_line <- i - length(signature_lines) + 1  # Start line of original signature
        function_lines <- c(full_signature)  # Use joined signature as first line
        brace_count <- 1  # Start with 1 for the opening brace we just found

        # Continue until braces balance
        while (i <= length(lines) && brace_count > 0) {
          current_line <- lines[i]
          function_lines <- c(function_lines, current_line)

          # Count braces in current line using robust method
          open_braces <- nchar(gsub("[^{]", "", current_line))
          close_braces <- nchar(gsub("[^}]", "", current_line))

          brace_count <- brace_count + open_braces - close_braces

          i <- i + 1
        }


        # Create function object
        current_function <- list(
          name = function_name,
          return_type = return_type,
          parameters = parameters,
          signature = paste0(return_type, " ", function_name, "(", parameters, ")"),
          start_line = start_line,
          end_line = i - 1,
          full_lines = function_lines,
          body_lines = if (length(function_lines) > 1) function_lines[-1] else character(0)
        )

        functions_list[[length(functions_list) + 1]] <- current_function
      } else {
        i <- i + 1
      }
    } else {
      i <- i + 1
    }
  }

  return(functions_list)
}

#' Remove Duplicate Functions from Function List
#'
#' @param functions_list List of function objects from parse_stan_functions
#' @return List of unique function objects
#' @noRd
remove_duplicate_functions <- function(functions_list) {
  checkmate::assert_list(functions_list, min.len = 1)

  if (length(functions_list) <= 1) {
    return(functions_list)
  }

  # Create signature-based grouping
  signatures <- sapply(functions_list, function(f) normalize_function_signature(f$signature))
  unique_signatures <- unique(signatures)

  unique_functions <- list()

  for (sig in unique_signatures) {
    matching_functions <- functions_list[signatures == sig]

    if (length(matching_functions) == 1) {
      # No duplicates for this signature
      unique_functions[[length(unique_functions) + 1]] <- matching_functions[[1]]
    } else {
      # Multiple functions with same signature - verify they're true duplicates
      first_func <- matching_functions[[1]]

      # Check if all functions with this signature have identical normalized bodies
      normalized_bodies <- sapply(matching_functions, function(f) normalize_function_body(f$body_lines))

      if (length(unique(normalized_bodies)) == 1) {
        # True duplicates - keep first occurrence
        unique_functions[[length(unique_functions) + 1]] <- first_func
      } else {
        # Different implementations - error
        insight::format_error(paste(
          "Functions with identical signatures but different implementations detected:",
          "{.field", first_func$name, "}",
          "This suggests a serious error in code generation."
        ))
      }
    }
  }

  return(unique_functions)
}

#' Normalize Function Signature for Comparison
#'
#' @param signature Character string of function signature
#' @return Normalized signature string
#' @noRd
normalize_function_signature <- function(signature) {
  checkmate::assert_character(signature, len = 1, any.missing = FALSE)

  # Remove extra whitespace
  normalized <- gsub("\\s+", " ", trimws(signature))

  # Standardize constraint formatting
  normalized <- gsub("< ", "<", normalized)
  normalized <- gsub(" >", ">", normalized)
  normalized <- gsub(" ,", ",", normalized)
  normalized <- gsub(", ", ",", normalized)

  return(normalized)
}

#' Normalize Function Body for Comparison
#'
#' @param body_lines Character vector of function body lines
#' @return Single normalized string
#' @noRd
normalize_function_body <- function(body_lines) {
  checkmate::assert_character(body_lines, any.missing = FALSE)

  # Remove comments
  body_lines <- gsub("//.*$", "", body_lines)
  body_lines <- gsub("/\\*.*?\\*/", "", body_lines, perl = TRUE)

  # Remove empty lines and trim whitespace
  body_lines <- trimws(body_lines)
  body_lines <- body_lines[nchar(body_lines) > 0]

  # Normalize whitespace
  body_lines <- gsub("\\s+", " ", body_lines)

  # Join into single string
  return(paste(body_lines, collapse = " "))
}

#' Reconstruct Functions Block from Unique Functions
#'
#' @param unique_functions List of unique function objects
#' @return Character string of reconstructed functions block
#' @noRd
reconstruct_functions_block <- function(unique_functions) {
  checkmate::assert_list(unique_functions, any.missing = FALSE)

  if (length(unique_functions) == 0) {
    return("")
  }

  function_strings <- sapply(unique_functions, function(f) {
    paste(f$full_lines, collapse = "\n")
  })

  return(paste(function_strings, collapse = "\n\n"))
}

#' Replace Functions Block in Stan Code
#'
#' @param stan_code Original Stan code
#' @param new_functions_content New functions block content
#' @return Stan code with replaced functions block
#' @noRd
replace_stan_functions_block <- function(stan_code, new_functions_content) {
  checkmate::assert_character(stan_code, len = 1, any.missing = FALSE)
  checkmate::assert_character(new_functions_content, len = 1, any.missing = FALSE)

  lines <- strsplit(stan_code, "\n", fixed = TRUE)[[1]]

  # Find functions block boundaries
  functions_start <- grep("^\\s*functions\\s*\\{", lines, ignore.case = TRUE)
  if (length(functions_start) == 0) {
    return(stan_code)  # No functions block to replace
  }

  # Find next block start
  block_patterns <- c("^\\s*data\\s*\\{", "^\\s*transformed\\s+data\\s*\\{",
                     "^\\s*parameters\\s*\\{", "^\\s*transformed\\s+parameters\\s*\\{",
                     "^\\s*model\\s*\\{", "^\\s*generated\\s+quantities\\s*\\{")

  next_block_lines <- c()
  for (pattern in block_patterns) {
    matches <- grep(pattern, lines, ignore.case = TRUE)
    if (length(matches) > 0) {
      next_block_lines <- c(next_block_lines, matches[matches > functions_start[1]])
    }
  }

  if (length(next_block_lines) == 0) {
    functions_end <- length(lines)
  } else {
    functions_end <- min(next_block_lines) - 1
  }

  # Reconstruct Stan code
  before_functions <- if (functions_start[1] > 1) lines[1:(functions_start[1] - 1)] else character(0)
  after_functions <- if (functions_end < length(lines)) lines[(functions_end + 1):length(lines)] else character(0)

  # Create new functions block
  if (nchar(trimws(new_functions_content)) == 0) {
    # Remove functions block entirely if empty
    new_lines <- c(before_functions, after_functions)
  } else {
    new_functions_block <- c("functions {", new_functions_content, "}")
    new_lines <- c(before_functions, new_functions_block, after_functions)
  }

  return(paste(new_lines, collapse = "\n"))
}
