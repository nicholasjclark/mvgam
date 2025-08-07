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
# adds mvgam-specific trend dynamics through stanvars injection.#' @noRd
generate_combined_stancode <- function(obs_setup, trend_setup = NULL,
                                      trend_spec = NULL, backend = "rstan",
                                      validate = TRUE, silent = 1) {
  checkmate::assert_list(obs_setup, names = "named")
  checkmate::assert_list(trend_setup, null.ok = TRUE)
  checkmate::assert_list(trend_spec, null.ok = TRUE)
  checkmate::assert_choice(backend, c("rstan", "cmdstanr"))
  checkmate::assert_flag(validate)
  checkmate::assert_number(silent)

  # If no trend specification, return observation model as-is
  if (is.null(trend_setup) || is.null(trend_spec)) {
    insight::format_warning(
      "No trend specification provided.",
      "Returning observation model without trend components."
    )

    return(list(
      stancode = obs_setup$stancode,
      standata = obs_setup$standata,
      has_trends = FALSE
    ))
  }

  # Stage 1: Extract trend stanvars from brms setup
  if (silent < 2) {
    message("Stage 1: Extracting trend stanvars from brms setup...")
  }

  trend_stanvars <- extract_trend_stanvars_from_setup(trend_setup, trend_spec)

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

  combined_stancode <- inject_trend_into_linear_predictor(
    base_stancode,
    trend_stanvars
  )

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

  # Apply any necessary autoformatting
  if (exists(".autoformat", envir = asNamespace("mvgam"))) {
    combined_stancode <- mvgam:::.autoformat(
      combined_stancode,
      backend = backend,
      silent = silent >= 1
    )
  }

  return(list(
    stancode = combined_stancode,
    standata = combined_standata,
    has_trends = TRUE,
    trend_spec = trend_spec,
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
  base_code <- try({
    brms::make_stancode(
      formula = obs_setup$formula,
      data = obs_setup$data,
      family = obs_setup$family,
      stanvars = all_stanvars,
      prior = obs_setup$prior
    )
  }, silent = TRUE)

  if (inherits(base_code, "try-error")) {
    error_msg <- attr(base_code, 'condition')$message
    stop(insight::format_error(
      "Failed to generate base Stan code with trend stanvars.",
      "Check observation formula and trend stanvar compatibility.",
      paste("Error:", error_msg)
    ))
  }

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

  # Handle conflicts by preferring trend data for trend-related elements
  trend_priority_patterns <- c("^trend_", "^n_trend", "^ar_", "^var_", "^rw_")

  for (pattern in trend_priority_patterns) {
    matching_names <- grep(pattern, names(trend_data), value = TRUE)
    combined_data[matching_names] <- trend_data[matching_names]
  }

  # Validate combined data consistency
  validate_combined_standata(combined_data, obs_data, trend_data)

  return(combined_data)
}

#' Create Stanvar Objects
#'
#' @description
#' Helper function to create brms stanvar objects for trend injection.
#'
#' @param x Data object to include in Stan model
#' @param name Character string name for the object in Stan
#' @param scode Character string Stan code type declaration
#' @return brms stanvar object
#' @noRd
stanvar <- function(x, name, scode = "data") {
  if (!requireNamespace("brms", quietly = TRUE)) {
    stop(insight::format_error(
      "Package {.pkg brms} is required for stanvar creation."
    ))
  }

  # Use brms::stanvar if available
  if (exists("stanvar", where = asNamespace("brms"))) {
    return(brms::stanvar(x = x, name = name, scode = scode))
  } else {
    # Fallback structure if brms::stanvar not available
    structure(
      list(
        name = name,
        sdata = x,
        scode = scode
      ),
      class = "stanvar"
    )
  }
}

#' Check if x is a try-error resulting from try()
is_try_error <- function(x) {
  inherits(x, "try-error")
}

#' Evaluate an expression without printing output or messages
#' @param expr expression to be evaluated
#' @param type type of output to be suppressed (see ?sink)
#' @param try wrap evaluation of expr in 'try' and
#'   not suppress outputs if evaluation fails?
#' @param silent actually evaluate silently?
#' @noRd
eval_silent <- function(
  expr,
  type = "output",
  try = FALSE,
  silent = TRUE,
  ...
) {
  try <- as_one_logical(try)
  silent <- as_one_logical(silent)
  type <- match.arg(type, c("output", "message"))
  expr <- substitute(expr)
  envir <- parent.frame()
  if (silent) {
    if (try && type == "message") {
      try_out <- try(utils::capture.output(
        out <- eval(expr, envir),
        type = type,
        ...
      ))
      if (is_try_error(try_out)) {
        # try again without suppressing error messages
        out <- eval(expr, envir)
      }
    } else {
      utils::capture.output(out <- eval(expr, envir), type = type, ...)
    }
  } else {
    out <- eval(expr, envir)
  }
  out
}

#' Integration with Enhanced mvgam Function
#'
#' @description
#' Integration point for enhanced mvgam function to use two-stage assembly.
#' Includes time series validation to ensure data compatibility.
#'
#' @param obs_setup Observation model setup from setup_brms_lightweight
#' @param trend_setup Trend model setup from setup_brms_lightweight
#' @param trend_spec Parsed trend specification
#' @param data Original data for validation
#' @param backend Stan backend to use
#' @param validate Whether to validate Stan code
#' @param silent Verbosity level
#' @return List ready for mvgam model fitting
#' @noRd
prepare_mvgam_stancode <- function(obs_setup, trend_setup, trend_spec,
                                  data = NULL, backend = "rstan",
                                  validate = TRUE, silent = 1) {

  # Validate time series structure if data provided and trends specified
  if (!is.null(data) && !is.null(trend_spec)) {
    validate_time_series_for_trends(data, trend_spec, silent = silent)
  }

  # Generate combined Stan code and data
  stan_components <- generate_combined_stancode(
    obs_setup = obs_setup,
    trend_setup = trend_setup,
    trend_spec = trend_spec,
    backend = backend,
    validate = validate,
    silent = silent
  )

  # Return in format expected by mvgam fitting functions
  return(list(
    model_code = stan_components$stancode,
    model_data = stan_components$standata,
    has_trends = stan_components$has_trends,
    trend_specification = trend_spec,
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


#' Merge Stan Data from Multiple Sources
#'
#' @description
#' Merges Stan data lists from different sources (e.g., observation and trend models).
#' Handles conflicts by preferring trend-specific data elements.
#'
#' @param base_data List containing base Stan data
#' @param trend_data List containing trend-specific Stan data
#' @return List of merged Stan data
#' @noRd
merge_stan_data <- function(base_data, trend_data) {
  checkmate::assert_list(base_data)
  checkmate::assert_list(trend_data)

  # Start with base data
  merged_data <- base_data

  # Flatten trend_data if it has nested structure
  if (any(sapply(trend_data, is.list))) {
    flattened_trend <- list()
    for (name in names(trend_data)) {
      if (is.list(trend_data[[name]]) && !is.data.frame(trend_data[[name]])) {
        # Add nested elements with prefix
        nested_elements <- trend_data[[name]]
        for (nested_name in names(nested_elements)) {
          flattened_trend[[nested_name]] <- nested_elements[[nested_name]]
        }
      } else {
        flattened_trend[[name]] <- trend_data[[name]]
      }
    }
    trend_data <- flattened_trend
  }

  # Identify conflicting names
  conflicting_names <- intersect(names(base_data), names(trend_data))

  # Warn about conflicts but prefer trend data
  if (length(conflicting_names) > 0) {
    insight::format_warning(
      "Data conflict detected for: {.field {conflicting_names}}",
      "Using trend data values."
    )
  }

  # Add all trend data (overwriting conflicts)
  for (name in names(trend_data)) {
    merged_data[[name]] <- trend_data[[name]]
  }

  return(merged_data)
}

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

# Feature Detection Utilities
# ===========================

#' Check if Stanvars Have Time Component
#'
#' @description
#' Checks if stanvars contain time-related components.
#'
#' @param stanvars List of stanvar objects
#' @return Logical indicating presence of time components
#' @noRd
has_time_component <- function(stanvars) {
  checkmate::assert_list(stanvars)

  # Look for time-related patterns in stanvar code
  time_patterns <- c("n_time", "time_vals", "time_data", "\\btime\\b")

  for (stanvar in stanvars) {
    if (is_valid_stanvar(stanvar)) {
      scode <- stanvar$scode

      for (pattern in time_patterns) {
        if (grepl(pattern, scode)) {
          return(TRUE)
        }
      }
    }
  }

  return(FALSE)
}

#' Check if Stanvars Have Series Component
#'
#' @description
#' Checks if stanvars contain series-related components.
#'
#' @param stanvars List of stanvar objects
#' @return Logical indicating presence of series components
#' @noRd
has_series_component <- function(stanvars) {
  checkmate::assert_list(stanvars)

  # Look for series-related patterns in stanvar code
  series_patterns <- c("n_series", "series_data", "\\bseries\\b")

  for (stanvar in stanvars) {
    if (is_valid_stanvar(stanvar)) {
      scode <- stanvar$scode

      for (pattern in series_patterns) {
        if (grepl(pattern, scode)) {
          return(TRUE)
        }
      }
    }
  }

  return(FALSE)
}

#' Check if Stanvars Have Correlation Component
#'
#' @description
#' Checks if stanvars contain correlation/covariance components.
#'
#' @param stanvars List of stanvar objects
#' @return Logical indicating presence of correlation components
#' @noRd
has_correlation_component <- function(stanvars) {
  checkmate::assert_list(stanvars)

  # Look for correlation-related patterns in stanvar code
  corr_patterns <- c("cov_matrix", "corr_matrix", "Sigma", "correlation", "covariance")

  for (stanvar in stanvars) {
    if (is_valid_stanvar(stanvar)) {
      scode <- stanvar$scode

      for (pattern in corr_patterns) {
        if (grepl(pattern, scode)) {
          return(TRUE)
        }
      }
    }
  }

  return(FALSE)
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
    stanvar <- stanvars[[name]]

    # brms stanvars are containers - validate the actual stanvar element
    stanvar_element <- if (inherits(stanvar, "stanvars") && length(stanvar) == 1) {
      stanvar[[1]]
    } else {
      stanvar
    }

    if (is_valid_stanvar(stanvar_element)) {
      valid_stanvars[[name]] <- stanvar
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
#' the Stan assembly system and individual trend generators.
#'
#' @param trend_spec List containing trend specification with trend_type or trend_model
#' @param data_info List containing data dimensions and structure information
#' @return List of stanvars for trend implementation, or empty list for None trends
#' @noRd
generate_trend_injection_stanvars <- function(trend_spec, data_info) {
  checkmate::assert_list(trend_spec, names = "named")
  checkmate::assert_list(data_info, names = "named")

  # Extract trend type from spec (handle both trend_type and trend_model for compatibility)
  trend_type <- trend_spec$trend_type %||% trend_spec$trend_model

  if (is.null(trend_type)) {
    insight::format_warning(
      "No trend type specified in trend_spec.",
      "Returning empty stanvars list."
    )
    return(list())
  }

  # Handle ZMVN default trend case
  if (trend_type == "ZMVN") {
    return(list())
  }

  # Construct generator function name following existing naming convention
  generator_name <- paste0("generate_", tolower(trend_type), "_trend_stanvars")

  # Check if generator function exists
  if (!exists(generator_name, mode = "function", envir = asNamespace("mvgam"))) {
    available_generators <- ls(
      pattern = "generate_.*_trend_stanvars",
      envir = asNamespace("mvgam")
    )

    stop(insight::format_error(
      paste("Trend generator function", generator_name, "not found."),
      paste("Available generators:", paste(available_generators, collapse = ", ")),
      paste("Trend type specified:", trend_type)
    ))
  }

  # Dispatch to appropriate generator
  do.call(generator_name, list(trend_spec, data_info))
}

#' Extract Trend Stanvars from Setup
#'
#' @description
#' Extracts trend stanvars from brms setup and generates additional trend-specific stanvars.
#' Integrates with the trend injection system.
#'
#' @param trend_setup List containing trend model setup
#' @param trend_spec List containing trend specification
#' @return List of trend stanvars
#' @noRd
extract_trend_stanvars_from_setup <- function(trend_setup, trend_spec) {
  checkmate::assert_list(trend_setup, names = "named")
  checkmate::assert_list(trend_spec, names = "named")

  # Extract base stanvars from trend setup
  base_stanvars <- trend_setup$stanvars %||% NULL

  # Generate trend-specific stanvars if trend spec is provided
  # Handle both trend_type and trend_model for compatibility
  trend_type <- trend_spec$trend_type %||% trend_spec$trend_model
  trend_stanvars <- if (!is.null(trend_spec) && !is.null(trend_type)) {
    # Prepare data info for trend stanvar generation
    # n_obs should be number of time points, not total observations
    n_obs <- trend_spec$n_time %||% trend_spec$n_obs

    if (is.null(n_obs)) {
      stop(insight::format_error(
        "Missing time dimension information in trend specification.",
        "trend_spec must contain either 'n_time' or 'n_obs' field.",
        "This indicates a problem with data processing or trend setup."
      ), call. = FALSE)
    }
    data_info <- list(
      n_obs = n_obs,
      n_series = trend_spec$n_series %||% 1,
      n_lv = trend_spec$n_lv,
      n_time = n_obs,  # Ensure consistency
      n_groups = trend_spec$n_groups,
      n_subgroups = trend_spec$n_subgroups,
      series_var = trend_spec$series_var %||% "series"
    )

    # Use the existing trend injection system
    generate_trend_injection_stanvars(trend_spec, data_info)
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

#' Combine Stan Components
#'
#' @description
#' Low-level utility function that combines observation Stan code, observation
#' data, and trend stanvars into a complete Stan model specification.
#'
#' This function merges stanvars into the observation code using brms stanvar
#' injection system and combines the data components.
#'
#' @param obs_stancode Character string containing observation model Stan code
#' @param obs_standata List containing observation model data
#' @param trend_stanvars List of stanvar objects for trend models
#' @return List with combined stancode, standata, and has_trends flag
#' @noRd
combine_stan_components <- function(obs_stancode, obs_standata, trend_stanvars) {
  checkmate::assert_string(obs_stancode, min.chars = 1)
  checkmate::assert_list(obs_standata, names = "named")
  checkmate::assert_list(trend_stanvars)

  # Track whether trends were actually added
  has_trends <- length(trend_stanvars) > 0

  # Start with observation code and data
  combined_code <- obs_stancode
  combined_data <- obs_standata

  # If no trend stanvars, return observation model as-is
  if (!has_trends) {
    return(list(
      stancode = combined_code,
      standata = combined_data,
      has_trends = FALSE
    ))
  }

  # Process each stanvar and inject into code
  for (stanvar_name in names(trend_stanvars)) {
    stanvar <- trend_stanvars[[stanvar_name]]

    if (is_valid_stanvar(stanvar)) {
      # Inject stanvar code into appropriate Stan block
      combined_code <- inject_stanvar_code(combined_code, stanvar)

      # Add stanvar data if present
      if (!is.null(stanvar$sdata)) {
        # Wrap sdata in a list with the stanvar name as key
        stanvar_data <- list()
        stanvar_data[[stanvar$name]] <- stanvar$sdata
        combined_data <- merge_stan_data(combined_data, stanvar_data)
      }
    }
  }

  return(list(
    stancode = combined_code,
    standata = combined_data,
    has_trends = has_trends
  ))
}

#' Inject Stanvar Code into Stan Model
#'
#' @description
#' Injects a stanvar's code into the appropriate Stan block of existing code.
#'
#' @param stan_code Character string containing existing Stan code
#' @param stanvar Stanvar object to inject
#' @return Updated Stan code with stanvar injected
#' @noRd
inject_stanvar_code <- function(stan_code, stanvar) {
  checkmate::assert_string(stan_code)

  if (!is_valid_stanvar(stanvar)) {
    return(stan_code)
  }

  block <- stanvar$block %||% "parameters"
  scode <- stanvar$scode %||% ""

  if (nchar(scode) == 0) {
    return(stan_code)
  }

  # Find the target block in Stan code
  block_pattern <- paste0("\\b", block, "\\s*\\{")

  if (!grepl(block_pattern, stan_code)) {
    # If block doesn't exist, append at end
    stan_code <- paste0(stan_code, "\n", block, " {\n", scode, "\n}\n")
  } else {
    # Find insertion point within existing block
    # This is a simplified implementation - more sophisticated block parsing
    # would be needed for production use

    # For now, append to end of existing block content
    # Find the closing brace of the target block
    block_start <- regexpr(block_pattern, stan_code)
    if (block_start > 0) {
      # Simple approach: add content before the last closing brace
      # This assumes the last } belongs to our target block (simplified)
      last_brace <- tail(gregexpr("\\}", stan_code)[[1]], 1)
      if (last_brace > 0) {
        before_brace <- substr(stan_code, 1, last_brace - 1)
        after_brace <- substr(stan_code, last_brace, nchar(stan_code))
        stan_code <- paste0(before_brace, "\n", scode, "\n", after_brace)
      }
    }
  }

  return(stan_code)
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
    hierarchical_functions <- generate_hierarchical_functions_injectors()
    hierarchical_params <- generate_hierarchical_correlation_parameter_injectors(n_groups, n_subgroups)
    hierarchical_priors <- generate_hierarchical_correlation_model_injectors(n_groups)

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
#' @param effective_dim Effective dimension (n_lv for factor models, n_series otherwise)
#' @param cor Logical, whether correlation parameters exist
#' @param is_hierarchical Logical, whether using hierarchical structure
#' @return Stanvar object with prior code
#' @noRd
generate_innovation_priors <- function(effective_dim, cor = FALSE, is_hierarchical = FALSE) {

  if (is_hierarchical) {
    # Hierarchical priors are handled by generate_hierarchical_correlation_model_injectors
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
#' @param trend_spec Trend specification list
#' @return List with hierarchical structure information
#' @noRd
extract_hierarchical_info <- function(data_info, trend_spec) {

  has_groups <- !is.null(trend_spec$gr) && trend_spec$gr != 'NA'

  if (!has_groups) {
    return(NULL)
  }

  list(
    has_groups = TRUE,
    n_groups = data_info$n_groups %||% 1,
    n_subgroups = data_info$n_subgroups %||% data_info$n_lv %||% data_info$n_series,
    gr_var = trend_spec$gr,
    subgr_var = trend_spec$subgr %||% 'NA'
  )
}

#' Generate Common Trend Data Variables
#'
#' Creates standard data block stanvars needed by most trend types.
#' Uses trend-specific naming to avoid conflicts with brms variables.
#'
#' @param n_obs Number of observations (will be named n_trend in Stan)
#' @param n_series Number of observed series
#' @param n_lv Number of latent variables (optional)
#' @return List of common data block stanvars
#' @noRd
generate_common_trend_data_injectors <- function(n_obs, n_series, n_lv = NULL) {
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
#' @param is_factor_model Logical indicating if this is a factor model
#' @param n_lv Number of latent variables
#' @param n_series Number of observed series
#' @return List of data block stanvars
#' @noRd
generate_matrix_z_data_injectors <- function(is_factor_model, n_lv, n_series) {
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
#' @param is_factor_model Logical indicating if this is a factor model
#' @param n_lv Number of latent variables
#' @param n_series Number of observed series
#' @return List of parameter block stanvars
#' @noRd
generate_matrix_z_parameter_injectors <- function(is_factor_model, n_lv, n_series) {
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
#' @param is_factor_model Logical indicating if this is a factor model
#' @param n_lv Number of latent variables
#' @param n_series Number of observed series
#' @return List of transformed data block stanvars
#' @noRd
generate_matrix_z_transformed_data_injectors <- function(is_factor_model, n_lv, n_series) {
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

#' Generate Matrix Z Stanvars (Consolidated Utility)
#'
#' Combines all matrix Z injection functions for factor/non-factor models.
#' This provides a single interface for matrix Z generation across all Stan blocks.
#'
#' @param is_factor_model Logical indicating if this is a factor model
#' @param n_lv Number of latent variables
#' @param n_series Number of observed series
#' @return List of stanvars for matrix Z across all required blocks
#' @noRd
generate_matrix_z_stanvars <- function(is_factor_model, n_lv, n_series) {
  # Get individual stanvar components
  data_stanvars <- generate_matrix_z_data_injectors(is_factor_model, n_lv, n_series)
  param_stanvars <- generate_matrix_z_parameter_injectors(is_factor_model, n_lv, n_series)
  tdata_stanvars <- generate_matrix_z_transformed_data_injectors(is_factor_model, n_lv, n_series)

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

#' Generate Factor Model Priors (Consolidated Utility)
#'
#' Provides standardized priors for factor models with fixed variance=1 constraint.
#' Only generates priors when is_factor_model=TRUE.
#'
#' @param is_factor_model Logical indicating if this is a factor model
#' @param n_lv Number of latent variables
#' @return List of stanvars for factor model priors
#' @noRd
generate_factor_model_priors <- function(is_factor_model, n_lv) {
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
#' @param n_lv Number of latent variables
#' @param n_series Number of observed series
#' @return List of transformed parameters block stanvars
#' @noRd
generate_trend_computation_transformed_parameters_injectors <- function(n_lv, n_series) {
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

#' Generate Model Block Injections for Factor Model Priors
#'
#' WHY: Factor models need fixed variance=1 constraints for identifiability
#' since the scale is captured by the loading matrix Z.
#'
#' @param is_factor_model Logical indicating if this is a factor model
#' @param n_lv Number of latent variables
#' @return List of model block stanvars
#' @noRd
generate_factor_model_model_injectors <- function(is_factor_model, n_lv) {
  if (is_factor_model) {
    # Factor model: fixed variance=1 for identifiability, priors for Z
    lv_priors_stanvar <- brms::stanvar(
      name = "factor_lv_priors",
      scode = "to_vector(LV_raw) ~ std_normal();",
      block = "model"
    )

    z_priors_stanvar <- brms::stanvar(
      name = "factor_z_priors",
      scode = "to_vector(Z) ~ normal(0, 1);",
      block = "model"
    )

    # Combine using brms c() method
    return(c(lv_priors_stanvar, z_priors_stanvar))
  } else {
    return(NULL)
  }
}

#' Generate Functions Block Injections for Hierarchical Correlations
#'
#' WHY: All trends (AR, VAR, CAR, ZMVN) need the same hierarchical correlation
#' machinery when groups are specified.
#'
#' @return List of functions block stanvars
#' @noRd
generate_hierarchical_functions_injectors <- function() {
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
#' @param n_groups Number of groups for hierarchical modeling
#' @param n_subgroups Number of subgroups (typically n_lv)
#' @return List of parameters block stanvars
#' @noRd
generate_hierarchical_correlation_parameter_injectors <- function(n_groups, n_subgroups) {
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
#' @param n_groups Number of groups for hierarchical modeling
#' @return List of model block stanvars
#' @noRd
generate_hierarchical_correlation_model_injectors <- function(n_groups) {
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
#' Dispatches to appropriate trend generator based on trend_spec type.
#'
#' @param trend_spec Trend specification object
#' @param data_info Data information list
#' @return List of stanvars for trend injection
#' @noRd
generate_trend_injection_stanvars <- function(trend_spec, data_info) {
  # Validate inputs
  checkmate::assert_list(trend_spec)
  checkmate::assert_list(data_info)

  # Validate factor model compatibility if n_lv is specified
  validate_factor_compatibility(trend_spec)

  # Validate n_lv parameter constraints
  if (!is.null(trend_spec$n_lv)) {
    n_series <- data_info$n_series %||% 1
    if (trend_spec$n_lv > n_series) {
      stop(insight::format_error(
        "{.field n_lv} cannot exceed {.field n_series}.",
        paste0("Got n_lv = ", trend_spec$n_lv, " and n_series = ", n_series, "."),
        "Use fewer or equal latent variables than observed series."
      ))
    }
    # n_lv < n_series → Factor model (estimated matrix Z)
    # n_lv = n_series → Non-factor model (diagonal matrix Z)
    # Both cases are valid after factor compatibility check above
  }

  # Get trend type - handle both trend_type and trend field names for compatibility
  trend_type <- trend_spec$trend_type %||% trend_spec$trend
  if (is.null(trend_type)) {
    stop(insight::format_error(
      "trend_spec must contain {.field trend_type} or {.field trend} field"
    ))
  }

  # Handle PW variations - PWlinear and PWlogistic both use PW generator
  # Preserve original type information for PW generator
  if (trend_type %in% c("PWlinear", "PWlogistic")) {
    trend_spec$type <- if (trend_type == "PWlogistic") "logistic" else "linear"
    trend_type <- "PW"
  }

  # Get trend info from registry
  trend_info <- get_trend_info(trend_type)
  if (is.null(trend_info)) {
    stop(insight::format_error(
      "Unknown trend type: {.field {trend_type}}"
    ))
  }

  # Extract hierarchical information for shared innovation system
  hierarchical_info <- extract_hierarchical_info(data_info, trend_spec)

  # Determine if this trend uses shared Gaussian innovations
  uses_shared_innovations <- trend_spec$shared_innovations %||%
                            (!trend_type %in% c("PW", "VAR"))  # PW and VAR opt out of shared innovations

  # Generate shared innovation stanvars if needed
  shared_stanvars <- NULL
  if (uses_shared_innovations) {
    # Extract relevant parameters for shared system
    n_lv <- trend_spec$n_lv %||% data_info$n_lv %||% data_info$n_series %||% 1
    n_series <- data_info$n_series %||% 1
    cor <- trend_spec$cor %||% FALSE
    factor_model <- !is.null(trend_spec$n_lv) && trend_spec$n_lv < n_series

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

    shared_priors <- generate_innovation_priors(
      effective_dim = effective_dim,
      cor = cor,
      is_hierarchical = is_hierarchical
    )

    shared_stanvars$priors <- shared_priors
  }

  # Generate trend-specific stanvars using the appropriate generator
  trend_stanvars <- trend_info$generator(trend_spec, data_info)

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
#' @param trend_spec Trend specification for RW model
#' @param data_info Data information including dimensions
#' @return List of stanvars for RW trend
#' @noRd
generate_rw_trend_stanvars <- function(trend_spec, data_info) {
  # Extract dimensions
  n_lv <- trend_spec$n_lv %||% 1
  n_series <- data_info$n_series %||% 1
  is_factor_model <- !is.null(trend_spec$n_lv) && n_lv < n_series

  # Build components list
  components <- list()

  # 1. Matrix Z for factor models
  matrix_z <- generate_matrix_z_stanvars(is_factor_model, n_lv, n_series)
  if (!is.null(matrix_z)) {
    components <- append(components, list(matrix_z))
  }

  # 2. MA parameters if needed
  if (trend_spec$ma %||% FALSE) {
    ma_components <- generate_ma_components(n_lv)
    components <- append(components, ma_components)
  }

  # 3. RW dynamics (always needed)
  dynamics <- generate_rw_dynamics(n_lv, has_ma = trend_spec$ma %||% FALSE)
  if (!is.null(dynamics)) {
    components <- append(components, list(dynamics))
  }

  # 4. Trend computation
  trend_computation <- generate_trend_computation_transformed_parameters_injectors(n_lv, n_series)
  if (!is.null(trend_computation)) {
    components <- append(components, list(trend_computation))
  }

  # 5. Factor model priors if applicable
  if (is_factor_model) {
    factor_priors <- generate_factor_model_model_injectors(is_factor_model, n_lv)
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
#' @param trend_spec Trend specification for AR model
#' @param data_info Data information including dimensions
#' @return List of stanvars for AR trend
#' @noRd
generate_ar_trend_stanvars <- function(trend_spec, data_info) {
  # Extract key parameters
  n_lv <- trend_spec$n_lv %||% 1
  lags <- trend_spec$lags %||% 1
  n <- data_info$n_obs
  n_series <- data_info$n_series %||% 1
  use_grouping <- !is.null(trend_spec$gr) && trend_spec$gr != 'NA'
  unit_var <- trend_spec$unit %||% "time"

  # Convert lags to ar_lags if needed
  ar_lags <- if (is.list(trend_spec$ar_lags)) trend_spec$ar_lags else (1:lags)
  has_ma <- trend_spec$ma %||% FALSE

  # Determine if this is a factor model (n_lv < n_series)
  is_factor_model <- !is.null(trend_spec$n_lv) && n_lv < n_series

  # Generate common trend data variables first
  common_data_stanvars <- generate_common_trend_data_injectors(n, n_series, n_lv)

  # Generate block-specific injectors for matrix Z
  matrix_z_stanvars <- generate_matrix_z_stanvars(is_factor_model, n_lv, n_series)

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
      factor_priors <- generate_factor_model_priors(is_factor_model, n_lv)

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
  trend_computation <- generate_trend_computation_transformed_parameters_injectors(n_lv, n_series)
  result_stanvars <- combine_stanvars(result_stanvars, trend_computation)

  return(result_stanvars)
}

#' VAR Trend Generator
#'
#' Generates Stan code components for vector autoregressive trends.
#' Supports factor models, hierarchical correlations, and consistent matrix Z patterns.
#'
#' @param trend_spec Trend specification for VAR model
#' @param data_info Data information including dimensions
#' @return List of stanvars for VAR trend
#' @noRd
generate_var_trend_stanvars <- function(trend_spec, data_info) {
  # Extract key parameters
  n_lv <- trend_spec$n_lv %||% 1
  lags <- trend_spec$lags %||% 1
  n <- data_info$n_obs
  n_series <- data_info$n_series %||% 1
  use_grouping <- !is.null(trend_spec$gr) && trend_spec$gr != 'NA'
  unit_var <- trend_spec$unit %||% "time"

  # Determine if this is a factor model (n_lv < n_series)
  is_factor_model <- !is.null(trend_spec$n_lv) && n_lv < n_series

  # Generate common trend data variables first
  common_data_stanvars <- generate_common_trend_data_injectors(n, n_series, n_lv)

  # Generate block-specific injectors for matrix Z
  matrix_z_stanvars <- generate_matrix_z_stanvars(is_factor_model, n_lv, n_series)

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
        array[{lags}] matrix[n_lv_trend, n_lv_trend] A;
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
                mu += A[lag] * LV[t-lag, :]';
              }}
              to_vector(LV[group_unit_indices[i, g], t]) ~ multi_normal_cholesky(mu, L_Omega_group[g]);
            }}
          }}
        }}

        // VAR coefficient priors
        for (lag in 1:{lags}) {{
          to_vector(A[lag]) ~ normal(0, 0.5);
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
              mu += A[lag] * LV[t-lag, :]';
            }}
            LV[t, :] ~ multi_normal(mu', Omega);
          }}

          // Priors for factor model
          for (lag in 1:{lags}) {{
            to_vector(A[lag]) ~ normal(0, 0.5);
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
          cov_matrix[n_lv_trend] Sigma;
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
              mu += A[lag] * LV[t-lag, :]';
            }}
            LV[t, :] ~ multi_normal(mu', Sigma);
          }}

          // Priors for non-factor model
          for (lag in 1:{lags}) {{
            to_vector(A[lag]) ~ normal(0, 0.5);
          }}
          Sigma ~ inv_wishart(n_lv_trend + 1, diag_matrix(rep_vector(1, n_lv_trend)));
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
      factor_priors <- generate_factor_model_model_injectors(is_factor_model, n_lv)
      result_stanvars <- combine_stanvars(result_stanvars, factor_priors)
    }
  }

  # Add trend computation stanvars
  trend_computation <- generate_trend_computation_transformed_parameters_injectors(n_lv, n_series)
  result_stanvars <- combine_stanvars(result_stanvars, trend_computation)

  return(result_stanvars)
}

#' AR Trend Generator
#'
#' Generates Stan code components for autoregressive trends.
#' Supports factor models, hierarchical correlations, and consistent matrix Z patterns.
#'
#' @param trend_spec Trend specification for AR model
#' @param data_info Data information including dimensions
#' @return List of stanvars for AR trend
#' @noRd
generate_ar_trend_stanvars <- function(trend_spec, data_info) {
  # Extract key parameters
  n_lv <- trend_spec$n_lv %||% 1
  lags <- trend_spec$lags %||% 1
  n <- data_info$n_obs
  n_series <- data_info$n_series %||% 1
  use_grouping <- !is.null(trend_spec$gr) && trend_spec$gr != 'NA'
  unit_var <- trend_spec$unit %||% "time"

  # Determine if this is a factor model (n_lv < n_series)
  is_factor_model <- !is.null(trend_spec$n_lv) && n_lv < n_series

  # Generate common trend data variables first
  common_data_stanvars <- generate_common_trend_data_injectors(n, n_series, n_lv)

  # Generate block-specific injectors for matrix Z
  matrix_z_stanvars <- generate_matrix_z_stanvars(is_factor_model, n_lv, n_series)

  # Start with common data stanvars, then add matrix Z stanvars
  result_stanvars <- combine_stanvars(common_data_stanvars, matrix_z_stanvars)

  if (use_grouping) {
    # Hierarchical AR case: AR(p = 1, unit = site, gr = group, subgr = species)
    n_groups <- data_info$n_groups %||% 1
    n_subgroups <- data_info$n_subgroups %||% n_lv

    # Note: Hierarchical correlation utilities are now handled centrally
    # to avoid stanvar name duplication across trend generators

    # AR-specific parameters for hierarchical case
    ar_hierarchical_params_stanvar <- brms::stanvar(
      name = "ar_hierarchical_params",
      scode = glue::glue("
        // AR coefficients for each latent variable
        matrix[n_lv_trend, {lags}] phi;
        // Latent states
        matrix[n_trend, n_lv_trend] LV;
      "),
      block = "parameters"
    )

    # Hierarchical AR model implementation
    ar_hierarchical_model_stanvar <- brms::stanvar(
      name = "ar_hierarchical_model",
      scode = glue::glue("
        // Derived hierarchical correlation matrices
        array[{n_groups}] cholesky_factor_corr[{n_subgroups}] L_Omega_group;
        for (g in 1 : {n_groups}) {{
          L_Omega_group[g] = combine_cholesky(L_Omega_global, L_deviation_group[g],
                                              alpha_cor);
        }}

        // Hierarchical AR process with group-specific correlations
        for (i in 1:n_{unit_var}) {{
          for (g in 1:{n_groups}) {{
            // AR dynamics with hierarchical residual correlation
            for (t in ({lags}+1):n_trend) {{
              vector[{n_subgroups}] mu = rep_vector(0, {n_subgroups});
              for (lag in 1:{lags}) {{
                for (j in 1:n_lv_trend) {{
                  mu[j] += phi[j, lag] * LV[t-lag, j];
                }}
              }}
              to_vector(LV[group_unit_indices[i, g], t]) ~ multi_normal_cholesky(mu, L_Omega_group[g]);
            }}
          }}
        }}

        // AR coefficient priors
        to_vector(phi) ~ normal(0, 0.5);
      "),
      block = "model"
    )

    # Note: Hierarchical priors are now handled centrally

    # Combine stanvars for hierarchical case (without hierarchical utilities)
    result_stanvars <- combine_stanvars(result_stanvars, ar_hierarchical_params_stanvar,
                                      ar_hierarchical_model_stanvar)

  } else {
    # Simple AR case (no grouping)
    if (is_factor_model) {
      # Factor model: fixed variance = 1, estimate Z
      ar_params_stanvar <- brms::stanvar(
        name = "ar_params",
        scode = glue::glue("
        parameters {{
          // AR coefficients for each latent variable
          matrix[n_lv_trend, {lags}] phi;
          // Raw latent states for factor model (variance = 1)
          matrix[n_trend, n_lv_trend] LV_raw;
        }}
        "),
        block = "parameters"
      )

      ar_transformed_stanvar <- brms::stanvar(
        name = "ar_transformed",
        scode = glue::glue("
        transformed parameters {{
          // Apply AR dynamics with fixed variance = 1
          matrix[n_trend, n_lv_trend] LV;
          LV = LV_raw;

          for (j in 1:{n_lv}) {{
            for (t in ({lags}+1):n_trend) {{
              real mu = 0;
              for (lag in 1:{lags}) {{
                mu += phi[j, lag] * LV[t-lag, j];
              }}
              LV[t, j] = mu + LV_raw[t, j];
            }}
          }}
        }}
        "),
        block = "tparameters"
      )
    } else {
      # Non-factor model: estimate variances, diagonal Z
      ar_params_stanvar <- brms::stanvar(
        name = "ar_params",
        scode = glue::glue("
        parameters {{
          // AR coefficients for each latent variable
          matrix[n_lv_trend, {lags}] phi;
          // Innovation standard deviations
          vector<lower=0>[n_lv_trend] sigma;
          // Latent states
          matrix[n_trend, n_lv_trend] LV;
        }}
        "),
        block = "parameters"
      )

      # AR model block for non-factor case
      ar_model_stanvar <- brms::stanvar(
        name = "ar_model",
        scode = glue::glue("
        model {{
          // AR process for each series independently
          for (j in 1:n_lv_trend) {{
            for (t in ({lags}+1):n) {{
              real mu = 0;
              for (lag in 1:{lags}) {{
                mu += phi[j, lag] * LV[t-lag, j];
              }}
              LV[t, j] ~ normal(mu, sigma[j]);
            }}
          }}

          // Priors for non-factor model
          to_vector(phi) ~ normal(0, 0.5);
          sigma ~ student_t(3, 0, 2.5);
        }}
        "),
        block = "model"
      )
    }

    # Combine stanvars for non-hierarchical case
    if (is_factor_model) {
      result_stanvars <- combine_stanvars(result_stanvars, ar_params_stanvar, ar_transformed_stanvar)

      # Use shared factor model priors
      factor_priors <- generate_factor_model_priors(is_factor_model, n_lv)

      # Add AR-specific priors for factor model
      ar_factor_priors_stanvar <- brms::stanvar(
        name = "ar_factor_priors",
        scode = glue::glue("
        model {{
          // AR coefficient priors for factor model
          to_vector(phi) ~ normal(0, 0.5);
        }}
        "),
        block = "model"
      )

      result_stanvars <- combine_stanvars(result_stanvars, factor_priors, ar_factor_priors_stanvar)
    } else {
      result_stanvars <- combine_stanvars(result_stanvars, ar_params_stanvar, ar_model_stanvar)
    }
  }

  # Add trend computation stanvars
  trend_computation <- generate_trend_computation_transformed_parameters_injectors(n_lv, n_series)
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
#' @param trend_spec Trend specification for CAR model
#' @param data_info Data information including dimensions
#' @return List of stanvars for CAR trend
#' @noRd
generate_car_trend_stanvars <- function(trend_spec, data_info) {
  # Extract key parameters
  n_lv <- trend_spec$n_lv %||% data_info$n_series %||% 1
  n <- data_info$n_obs
  n_series <- data_info$n_series %||% 1

  # Generate common trend data variables first
  common_data_stanvars <- generate_common_trend_data_injectors(n, n_series, n_lv)

  # CAR does not support factor models (continuous-time AR requires
  # series-specific temporal evolution)
  if (!is.null(trend_spec$n_lv) && trend_spec$n_lv < n_series) {
    stop(insight::format_error(
      "CAR trends do not support factor models (n_lv < n_series).",
      "Continuous-time AR requires series-specific temporal evolution modeling."
    ))
  }

  # CAR does not support hierarchical correlations
  if (!is.null(trend_spec$gr) && trend_spec$gr != 'NA') {
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
  vector<lower=0>[n_lv_trend] sigma;

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
  LV = LV_raw .* rep_matrix(sigma', rows(LV_raw));

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
  trend_computation <- generate_trend_computation_transformed_parameters_injectors(n_lv, n_series)

  # CAR model priors
  car_priors_stanvar <- brms::stanvar(
    name = "car_priors",
    scode = "
  // CAR priors
  ar1 ~ std_normal();
  sigma ~ exponential(3);
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
#' @param trend_spec Trend specification for GP model
#' @param data_info Data information including dimensions
#' @return List of stanvars for GP trend
#' @noRd
# GP trends are handled via trend_formula, not as temporal dynamics
generate_gp_injection_stanvars <- function(trend_spec, data_info) {
  stop(insight::format_error(
    "GP trends are handled via trend_formula, not as temporal dynamics.",
    "Use: trend_formula = ~ gp(time, k = 10)..."
  ))
}

#' ZMVN Trend Generator
#'
#' Generates Stan code components for zero-mean multivariate normal trends.
#'
#' @param trend_spec Trend specification for ZMVN model
#' @param data_info Data information including dimensions
#' @return List of stanvars for ZMVN trend
#' @noRd
generate_zmvn_trend_stanvars <- function(trend_spec, data_info) {
  # Extract key parameters following original ZMVN pattern
  n_lv <- trend_spec$n_lv %||% data_info$n_lv %||% data_info$n_series %||% 1
  n <- data_info$n_obs
  n_series <- data_info$n_series %||% 1
  use_grouping <- !is.null(trend_spec$gr) && trend_spec$gr != 'NA'
  unit_var <- trend_spec$unit %||% "time"

  # Determine if this is a factor model (n_lv < n_series)
  is_factor_model <- !is.null(trend_spec$n_lv) && n_lv < n_series

  # Generate common trend data variables first
  common_data_stanvars <- generate_common_trend_data_injectors(n, n_series, n_lv)

  # Generate block-specific injectors for matrix Z
  matrix_z_stanvars <- generate_matrix_z_stanvars(is_factor_model, n_lv, n_series)

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
  trend_computation <- generate_trend_computation_transformed_parameters_injectors(n_lv, n_series)
  result_stanvars <- combine_stanvars(result_stanvars, trend_computation)

  # Add factor model priors if applicable
  if (is_factor_model) {
    factor_priors <- generate_factor_model_priors(is_factor_model, n_lv)
    result_stanvars <- combine_stanvars(result_stanvars, factor_priors)
  }

  return(result_stanvars)
}

#' PW Trend Generator
#'
#' Generates Stan code components for piecewise trends.
#' Supports both linear and logistic piecewise trends with proper Prophet-style implementations.
#'
#' @param trend_spec Trend specification for PW model
#' @param data_info Data information including dimensions
#' @return List of stanvars for PW trend
#' @noRd
generate_pw_trend_stanvars <- function(trend_spec, data_info) {
  # Extract key parameters
  n_lv <- trend_spec$n_lv %||% 1
  n_changepoints <- trend_spec$n_changepoints %||% 5
  changepoint_scale <- trend_spec$changepoint_scale %||% 0.1
  trend_type <- trend_spec$type %||% "linear"  # "linear" or "logistic"
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
  common_data_stanvars <- generate_common_trend_data_injectors(n, n_series, n_lv)

  # Generate block-specific injectors for matrix Z
  matrix_z_stanvars <- generate_matrix_z_stanvars(is_factor_model, n_lv, n_series)

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
  trend_computation <- generate_trend_computation_transformed_parameters_injectors(n_lv, n_series)

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

#' PWlinear Trend Generator
#'
#' Generates Stan code components for linear piecewise trends.
#' Wrapper around PW generator with type = "linear".
#'
#' @param trend_spec Trend specification for PWlinear model
#' @param data_info Data information including dimensions
#' @return List of stanvars for PWlinear trend
#' @noRd
generate_pwlinear_trend_stanvars <- function(trend_spec, data_info) {
  # Force linear type for PWlinear
  trend_spec$type <- "linear"

  # Delegate to main PW generator
  generate_pw_trend_stanvars(trend_spec, data_info)
}

#' PWlogistic Trend Generator
#'
#' Generates Stan code components for logistic piecewise trends.
#' Wrapper around PW generator with type = "logistic".
#'
#' @param trend_spec Trend specification for PWlogistic model
#' @param data_info Data information including dimensions
#' @return List of stanvars for PWlogistic trend
#' @noRd
generate_pwlogistic_trend_stanvars <- function(trend_spec, data_info) {
  # Force logistic type for PWlogistic
  trend_spec$type <- "logistic"

  # Delegate to main PW generator
  generate_pw_trend_stanvars(trend_spec, data_info)
}




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
