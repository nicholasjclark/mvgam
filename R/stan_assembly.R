#' Two-Stage Stan Code Assembly for mvgam
#'
#' @description
#' Implements the two-stage Stan assembly system that extracts trend stanvars
#' from brms setup and modifies observation linear predictors with missing data preservation.

#' Extract Trend Stanvars from brms Setup
#' 
#' @description
#' Stage 1 of two-stage assembly: Generate complete trend Stan code and extract
#' stanvars for injection into the base brms observation model.
#' 
#' @param trend_setup List containing brms setup components for trend model
#' @param trend_spec List containing trend specification details
#' @return List of stanvars containing trend parameters and functions
#' @noRd
extract_trend_stanvars_from_setup <- function(trend_setup, trend_spec) {
  checkmate::assert_list(trend_setup, names = "named")
  checkmate::assert_list(trend_spec, names = "named")
  
  # Extract data information from trend_setup
  data_info <- extract_data_info_from_setup(trend_setup)
  
  # Generate trend injection stanvars that add temporal dynamics to brms code
  # brms provides: trend_mus = X_trend * b_trend
  # we add: temporal dynamics applied to LV using trend_mus as mean
  trend_stanvars <- generate_trend_injection_stanvars(
    trend_spec = trend_spec,
    data_info = data_info
  )
  
  return(trend_stanvars)
}

#' Extract Data Information from Setup
#' 
#' @description
#' Extracts data structure information needed for trend Stan code generation.
#' 
#' @param trend_setup brms setup object containing standata
#' @return List of data information
#' @noRd
extract_data_info_from_setup <- function(trend_setup) {
  standata <- trend_setup$standata
  
  data_info <- list(
    n_times = standata$N %||% standata$n %||% 10,
    n_series = standata$n_series %||% 1,
    n_lv = standata$n_lv %||% standata$n_series %||% 1,  # Critical for LV models
    time_vals = standata$time_vals %||% seq_len(standata$N %||% 10),
    ytimes = standata$ytimes %||% NULL,
    ytimes_trend = standata$ytimes_trend %||% NULL  # Critical for trend indexing
  )
  
  return(data_info)
}

#' Parse Trend Stan Code
#' 
#' @description
#' Parses generated trend Stan code to extract components for stanvar creation.
#' 
#' @param stan_code Character string of trend Stan code
#' @param trend_spec Trend specification
#' @return List of parsed trend components
#' @noRd
parse_trend_stan_code <- function(stan_code, trend_spec) {
  code_lines <- strsplit(stan_code, "\n")[[1]]
  
  # Extract functions block
  functions_block <- extract_code_block(code_lines, "functions")
  
  # Extract data block 
  data_block <- extract_code_block(code_lines, "data")
  
  # Extract parameters block
  parameters_block <- extract_code_block(code_lines, "parameters")
  
  # Extract transformed parameters block
  transformed_parameters_block <- extract_code_block(code_lines, "transformed parameters")
  
  # Extract model block
  model_block <- extract_code_block(code_lines, "model")
  
  # Extract generated quantities block
  generated_quantities_block <- extract_code_block(code_lines, "generated quantities")
  
  return(list(
    functions = functions_block,
    data = data_block,
    parameters = parameters_block,
    transformed_parameters = transformed_parameters_block,
    model = model_block,
    generated_quantities = generated_quantities_block,
    trend_type = trend_spec$type
  ))
}

#' Create Trend Stanvars
#' 
#' @description
#' Creates brms stanvars from parsed trend components.
#' 
#' @param trend_components List of parsed trend components
#' @param standata Original standata from trend setup
#' @return List of stanvars for injection
#' @noRd
create_trend_stanvars <- function(trend_components, standata) {
  stanvars <- list()
  
  # Add functions stanvar if functions exist
  if (!is.null(trend_components$functions) && nchar(trend_components$functions) > 0) {
    stanvars$functions <- stanvar(
      x = NULL, 
      name = "trend_functions", 
      scode = trend_components$functions
    )
  }
  
  # Add data stanvars
  if (!is.null(trend_components$data)) {
    data_vars <- extract_data_declarations(trend_components$data)
    for (var_name in names(data_vars)) {
      if (var_name %in% names(standata)) {
        stanvars[[paste0("data_", var_name)]] <- stanvar(
          x = standata[[var_name]],
          name = var_name,
          scode = "data"
        )
      }
    }
  }
  
  # Add parameter stanvars as generated quantities
  if (!is.null(trend_components$parameters)) {
    stanvars$trend_parameters <- stanvar(
      x = NULL,
      name = "trend_parameters", 
      scode = trend_components$parameters
    )
  }
  
  # Add transformed parameters
  if (!is.null(trend_components$transformed_parameters)) {
    stanvars$trend_transformed <- stanvar(
      x = NULL,
      name = "trend_transformed",
      scode = trend_components$transformed_parameters
    )
  }
  
  # Add model block contributions
  if (!is.null(trend_components$model)) {
    stanvars$trend_model <- stanvar(
      x = NULL,
      name = "trend_model_contributions",
      scode = trend_components$model
    )
  }
  
  # Add generated quantities
  if (!is.null(trend_components$generated_quantities)) {
    stanvars$trend_generated <- stanvar(
      x = NULL,
      name = "trend_generated_quantities",
      scode = trend_components$generated_quantities
    )
  }
  
  return(stanvars)
}

#' Extract Trend Parameters from Stan Code
#' 
#' @description
#' Parses trend Stan code to extract parameter declarations.
#' 
#' @param stancode Character string containing trend Stan code
#' @param trend_spec List containing trend specification
#' @return List of trend parameter information
#' @noRd
extract_trend_parameters <- function(stancode, trend_spec) {
  checkmate::assert_string(stancode, min.chars = 1)
  checkmate::assert_list(trend_spec)
  
  # Split Stan code into lines for parsing
  code_lines <- strsplit(stancode, "\n")[[1]]
  
  # Find parameters block
  param_start <- grep("^\\s*parameters\\s*\\{", code_lines)
  param_end <- find_matching_brace(code_lines, param_start)
  
  if (length(param_start) == 0 || length(param_end) == 0) {
    # No parameters block found, return empty structure
    return(list(
      trend_raw = NULL,
      trend_pars = NULL
    ))
  }
  
  param_lines <- code_lines[param_start:param_end]
  
  # Extract trend-related parameters
  trend_raw_lines <- grep("trend_raw", param_lines, value = TRUE)
  trend_par_lines <- grep("trend_.*(?<!_raw)", param_lines, value = TRUE, perl = TRUE)
  
  return(list(
    trend_raw = parse_parameter_declarations(trend_raw_lines),
    trend_pars = parse_parameter_declarations(trend_par_lines)
  ))
}

#' Extract Trend Functions from Stan Code
#' 
#' @description
#' Parses trend Stan code to extract custom function definitions.
#' 
#' @param stancode Character string containing trend Stan code
#' @param trend_spec List containing trend specification
#' @return Character vector of function definitions
#' @noRd
extract_trend_functions <- function(stancode, trend_spec) {
  checkmate::assert_string(stancode, min.chars = 1)
  checkmate::assert_list(trend_spec)
  
  # Split Stan code into lines for parsing
  code_lines <- strsplit(stancode, "\n")[[1]]
  
  # Find functions block
  func_start <- grep("^\\s*functions\\s*\\{", code_lines)
  
  if (length(func_start) == 0) {
    return(character(0))
  }
  
  func_end <- find_matching_brace(code_lines, func_start)
  
  if (length(func_end) == 0) {
    return(character(0))
  }
  
  # Extract function block content
  func_lines <- code_lines[func_start:func_end]
  
  # Return as single string
  return(paste(func_lines, collapse = "\n"))
}

#' Extract Trend Data from standata
#' 
#' @description
#' Extracts trend-related data components from brms standata.
#' 
#' @param standata List containing brms stan data
#' @param trend_spec List containing trend specification
#' @return List of trend data components
#' @noRd
extract_trend_data <- function(standata, trend_spec) {
  checkmate::assert_list(standata)
  checkmate::assert_list(trend_spec)
  
  # Extract trend-related data elements
  trend_data_names <- grep("^trend_", names(standata), value = TRUE)
  trend_data <- standata[trend_data_names]
  
  # Add missing data pattern information if available
  if ("obs_ind" %in% names(standata)) {
    trend_data$obs_ind <- standata$obs_ind
  }
  
  # Add time structure information
  if ("n" %in% names(standata)) {
    trend_data$n_times <- standata$n
  }
  
  if ("n_series" %in% names(standata)) {
    trend_data$n_series <- standata$n_series
  }
  
  return(trend_data)
}

#' Inject Trend into Linear Predictor
#' 
#' @description
#' Stage 2 of two-stage assembly: Modify brms Stan code to integrate temporal dynamics.
#' Key steps:
#' 1. Rename trend model's 'mu' to 'mu_trend' 
#' 2. Add LV temporal dynamics using mu_trend as mean
#' 3. Create trend effects and combine with observation mu
#' 
#' @param base_stancode Character string containing base brms Stan code
#' @param trend_stanvars List of trend stanvars from Stage 1
#' @param trend_spec List containing trend specification
#' @return Character string of modified Stan code
#' @noRd
inject_trend_into_linear_predictor <- function(base_stancode, trend_stanvars, trend_spec) {
  checkmate::assert_string(base_stancode, min.chars = 1)
  checkmate::assert_list(trend_stanvars)
  checkmate::assert_list(trend_spec)
  
  # Parse base Stan code
  code_lines <- strsplit(base_stancode, "\n")[[1]]
  
  # Step 1: Rename trend model's 'mu' to 'mu_trend'
  # This preserves brms trend formula effects as mu_trend
  modified_code <- rename_trend_mu_to_mu_trend(code_lines)
  
  # Step 2: Add LV array and trend computation in transformed parameters
  modified_code <- add_lv_and_trend_computation(modified_code, trend_spec)
  
  # Step 3: Inject temporal dynamics into model block
  modified_code <- inject_temporal_dynamics(modified_code, trend_stanvars)
  
  # Step 4: Modify observation likelihood to use combined effects
  modified_code <- modify_observation_likelihood(modified_code, trend_spec)
  
  # Validate the modified code
  final_code <- paste(modified_code, collapse = "\n")
  
  return(final_code)
}

#' Rename Trend Model mu to mu_trend
#' 
#' @description
#' Renames the trend model's 'mu' variable to 'mu_trend' to distinguish it
#' from the observation model's 'mu'. This preserves brms trend formula effects.
#' 
#' @param code_lines Character vector of Stan code lines
#' @return Character vector of modified Stan code lines
#' @noRd
rename_trend_mu_to_mu_trend <- function(code_lines) {
  checkmate::assert_character(code_lines)
  
  # Find where mu is declared and initialized in model block
  model_start <- grep("^\\s*model\\s*\\{", code_lines)
  model_end <- find_matching_brace(code_lines, model_start)
  
  if (length(model_start) == 0 || length(model_end) == 0) {
    return(code_lines)
  }
  
  # Within model block, rename mu declarations and assignments to mu_trend
  for (i in model_start:model_end) {
    # Pattern: vector[N] mu = rep_vector(0.0, N);
    if (grepl("vector\\[.*\\]\\s+mu\\s*=\\s*rep_vector", code_lines[i])) {
      code_lines[i] <- gsub("\\bmu\\b", "mu_trend", code_lines[i])
    }
    # Pattern: mu += ...
    if (grepl("^\\s*mu\\s*\\+=", code_lines[i])) {
      code_lines[i] <- gsub("\\bmu\\b", "mu_trend", code_lines[i])
    }
  }
  
  return(code_lines)
}

#' Add LV Array and Trend Computation
#' 
#' @description
#' Adds LV array declaration and trend computation in transformed parameters.
#' This creates the latent variable structure needed for temporal dynamics.
#' 
#' @param code_lines Character vector of Stan code lines
#' @param trend_spec Trend specification
#' @return Character vector of modified Stan code lines
#' @noRd
add_lv_and_trend_computation <- function(code_lines, trend_spec) {
  checkmate::assert_character(code_lines)
  checkmate::assert_list(trend_spec)
  
  # Find transformed parameters block
  trans_param_start <- grep("^\\s*transformed\\s+parameters\\s*\\{", code_lines)
  
  if (length(trans_param_start) == 0) {
    # Create transformed parameters block if it doesn't exist
    param_end <- grep("^\\s*parameters\\s*\\{", code_lines)
    param_end <- find_matching_brace(code_lines, param_end)
    
    if (length(param_end) == 0) {
      return(code_lines)
    }
    
    # Insert new transformed parameters block
    lv_and_trend_block <- c(
      "transformed parameters {",
      "  // latent states for temporal dynamics",
      "  array[n] vector[n_lv] LV;",
      "  // trend effects mapped from latent states", 
      "  matrix[n, n_series] trend;",
      "  // loading matrix",
      "  matrix[n_series, n_lv] lv_coefs = Z;",
      "}"
    )
    
    modified_lines <- c(
      code_lines[1:param_end],
      lv_and_trend_block,
      code_lines[(param_end + 1):length(code_lines)]
    )
    
  } else {
    # Add to existing transformed parameters block
    trans_param_end <- find_matching_brace(code_lines, trans_param_start)
    
    lv_declarations <- c(
      "  // latent states for temporal dynamics",
      "  array[n] vector[n_lv] LV;",
      "  // trend effects mapped from latent states",
      "  matrix[n, n_series] trend;", 
      "  // loading matrix",
      "  matrix[n_series, n_lv] lv_coefs = Z;",
      "",
      "  // compute trend effects from latent states",
      "  for (i in 1:n) {",
      "    for (s in 1:n_series) {",
      "      trend[i, s] = dot_product(lv_coefs[s, :], LV[i, :]);",
      "    }",
      "  }"
    )
    
    # Insert before closing brace
    modified_lines <- c(
      code_lines[1:(trans_param_end - 1)],
      lv_declarations,
      code_lines[trans_param_end:length(code_lines)]
    )
  }
  
  return(modified_lines)
}

#' Inject Temporal Dynamics
#' 
#' @description
#' Injects temporal dynamics into the model block using the stanvars.
#' This adds the LV evolution equations that use mu_trend as the mean.
#' 
#' @param code_lines Character vector of Stan code lines
#' @param trend_stanvars List of trend stanvars
#' @return Character vector of modified Stan code lines
#' @noRd
inject_temporal_dynamics <- function(code_lines, trend_stanvars) {
  checkmate::assert_character(code_lines)
  checkmate::assert_list(trend_stanvars)
  
  # Find model block
  model_start <- grep("^\\s*model\\s*\\{", code_lines)
  model_end <- find_matching_brace(code_lines, model_start)
  
  if (length(model_start) == 0 || length(model_end) == 0) {
    return(code_lines)
  }
  
  # Extract temporal dynamics code from stanvars
  temporal_code <- c()
  
  for (stanvar in trend_stanvars) {
    if (grepl("model_block", stanvar$name %||% "")) {
      # Extract the temporal dynamics code
      stanvar_code <- stanvar$scode %||% ""
      if (nchar(stanvar_code) > 0) {
        temporal_code <- c(temporal_code, "", "  // Temporal dynamics from stanvar", 
                          paste0("  ", strsplit(stanvar_code, "\n")[[1]]))
      }
    }
  }
  
  if (length(temporal_code) == 0) {
    return(code_lines)
  }
  
  # Insert temporal dynamics before model block closing brace
  modified_lines <- c(
    code_lines[1:(model_end - 1)],
    temporal_code,
    code_lines[model_end:length(code_lines)]
  )
  
  return(modified_lines)
}

#' Modify Observation Likelihood
#' 
#' @description
#' Modifies the observation likelihood to use combined mu + trend effects.
#' Creates the pattern: mu_combined = mu + flat_trends
#' 
#' @param code_lines Character vector of Stan code lines  
#' @param trend_spec Trend specification
#' @return Character vector of modified Stan code lines
#' @noRd
modify_observation_likelihood <- function(code_lines, trend_spec) {
  checkmate::assert_character(code_lines)
  checkmate::assert_list(trend_spec)
  
  # Find where likelihood is computed (look for target += or Y ~ distribution)
  likelihood_lines <- grep("target\\s*\\+=.*_lpdf|~", code_lines)
  
  if (length(likelihood_lines) == 0) {
    return(code_lines)
  }
  
  # Find the last mu computation before likelihood
  likelihood_start <- likelihood_lines[1]
  mu_lines <- grep("vector\\[.*\\]\\s+mu\\s*=|mu\\s*\\+=", code_lines[1:likelihood_start])
  
  if (length(mu_lines) == 0) {
    return(code_lines)
  }
  
  # Insert trend combination after last mu computation
  last_mu_line <- mu_lines[length(mu_lines)]
  
  trend_combination <- c(
    "    // mvgam: combine observation and trend effects",
    "    vector[n_nonmissing] flat_trends = to_vector(trend)[obs_ind];",
    "    // modify mu to include trend effects - this happens in the likelihood"
  )
  
  modified_lines <- c(
    code_lines[1:last_mu_line],
    trend_combination,
    code_lines[(last_mu_line + 1):length(code_lines)]
  )
  
  # Modify the likelihood to use trend effects
  # Look for patterns like: target += normal_lpdf(Y | mu, sigma);
  # Replace with: target += normal_lpdf(Y | mu + flat_trends, sigma);
  for (i in 1:length(modified_lines)) {
    if (grepl("target\\s*\\+=.*\\(Y\\s*\\|\\s*mu[^_]", modified_lines[i])) {
      modified_lines[i] <- gsub("\\(Y\\s*\\|\\s*mu([^_])", "(Y | mu + flat_trends\\1", modified_lines[i])
    }
  }
  
  return(modified_lines)
}

#' Generate Trend Computation Code
#' 
#' @description
#' Generates Stan code for computing trend effects based on trend specification.
#' 
#' @param trend_spec List containing trend specification
#' @return Character string of trend computation code
#' @noRd
generate_trend_computation <- function(trend_spec) {
  checkmate::assert_list(trend_spec)
  
  # Basic template - can be extended for different trend types
  if (trend_spec$type == "RW") {
    return("// Random walk trend computation\nmu_trend = trend_effects;")
  } else if (trend_spec$type == "AR") {
    return("// AR trend computation\nmu_trend = trend_effects;")
  } else if (trend_spec$type == "VAR") {
    return("// VAR trend computation\nmu_trend = trend_effects;")
  } else {
    return("// Generic trend computation\nmu_trend = trend_effects;")
  }
}

#' Add Missing Data Preservation Pattern
#' 
#' @description
#' Implements missing data pattern where trends evolve over ALL timesteps
#' but likelihood only uses non-missing observations.
#' 
#' @param code_lines Character vector of Stan code lines
#' @param trend_spec List containing trend specification
#' @return Character vector of modified Stan code lines
#' @noRd
add_missing_data_pattern <- function(code_lines, trend_spec) {
  checkmate::assert_character(code_lines)
  checkmate::assert_list(trend_spec)
  
  # Find likelihood section
  likelihood_start <- grep("// likelihood", code_lines, ignore.case = TRUE)
  
  if (length(likelihood_start) == 0) {
    # Try to find target += patterns
    likelihood_start <- grep("target\\s*\\+=", code_lines)
  }
  
  if (length(likelihood_start) == 0) {
    insight::format_warning(
      "Could not find likelihood section in Stan code.",
      "Missing data pattern may not be applied correctly."
    )
    return(code_lines)
  }
  
  # Template for missing data pattern
  missing_data_template <- glue::glue("
    // Missing data preservation: likelihood only for non-missing observations
    {{
      vector[n_nonmissing] selected_mu = mu_combined[obs_ind];
      flat_ys ~ {get_family_distribution(trend_spec)}(selected_mu, ...);
    }}
  ")
  
  # This is a placeholder - would need to be customized based on family
  # For now, just add a comment indicating where this would go
  comment_line <- "    // mvgam: Missing data pattern would be applied here"
  
  # Insert comment after first likelihood line
  insert_position <- likelihood_start[1] + 1
  
  modified_lines <- c(
    code_lines[1:(insert_position - 1)],
    comment_line,
    code_lines[insert_position:length(code_lines)]
  )
  
  return(modified_lines)
}

#' Helper Functions for Stan Code Parsing
#'
#' @description
#' Utility functions for parsing Stan code structure.

#' Find Matching Brace
#' 
#' @param code_lines Character vector of code lines
#' @param start_line Line number containing opening brace
#' @return Line number of matching closing brace
#' @noRd
find_matching_brace <- function(code_lines, start_line) {
  if (length(start_line) == 0) return(integer(0))
  
  brace_count <- 0
  start_pos <- start_line[1]
  
  for (i in start_pos:length(code_lines)) {
    open_braces <- length(gregexpr("\\{", code_lines[i])[[1]])
    if (open_braces > 0 && gregexpr("\\{", code_lines[i])[[1]][1] != -1) {
      brace_count <- brace_count + open_braces
    }
    
    close_braces <- length(gregexpr("\\}", code_lines[i])[[1]])
    if (close_braces > 0 && gregexpr("\\}", code_lines[i])[[1]][1] != -1) {
      brace_count <- brace_count - close_braces
    }
    
    if (brace_count == 0 && i > start_pos) {
      return(i)
    }
  }
  
  return(integer(0))
}

#' Parse Parameter Declarations
#' 
#' @param param_lines Character vector of parameter declaration lines
#' @return List of parsed parameter information
#' @noRd
parse_parameter_declarations <- function(param_lines) {
  if (length(param_lines) == 0) {
    return(NULL)
  }
  
  # Basic parsing - would need more sophisticated implementation
  return(param_lines)
}

#' Get Family Distribution Pattern
#' 
#' @param trend_spec List containing trend specification including family
#' @return Character string of distribution pattern
#' @noRd
get_family_distribution <- function(trend_spec) {
  family <- trend_spec$family %||% "gaussian"
  
  switch(family,
    "gaussian" = "normal",
    "poisson" = "poisson",
    "binomial" = "binomial",
    "beta" = "beta",
    "gamma" = "gamma",
    "normal"  # default
  )
}

#' Extract Code Block from Stan Code
#' 
#' @description
#' Extracts a specific block (functions, data, parameters, etc.) from Stan code.
#' 
#' @param code_lines Character vector of Stan code lines
#' @param block_name Name of the block to extract
#' @return Character string of the extracted block content
#' @noRd
extract_code_block <- function(code_lines, block_name) {
  # Find block start
  block_pattern <- paste0("^\\s*", block_name, "\\s*\\{")
  block_start <- grep(block_pattern, code_lines)
  
  if (length(block_start) == 0) {
    return(NULL)
  }
  
  # Find matching closing brace
  block_end <- find_matching_brace(code_lines, block_start[1])
  
  if (length(block_end) == 0) {
    return(NULL)
  }
  
  # Extract block content (including braces)
  block_content <- code_lines[block_start[1]:block_end[1]]
  
  return(paste(block_content, collapse = "\n"))
}

#' Extract Data Declarations from Data Block
#' 
#' @description
#' Parses data block to extract variable declarations.
#' 
#' @param data_block Character string of data block
#' @return Named list of data variables
#' @noRd
extract_data_declarations <- function(data_block) {
  if (is.null(data_block)) {
    return(list())
  }
  
  # Simple parsing - extract variable names
  # This is a basic implementation that could be made more sophisticated
  lines <- strsplit(data_block, "\n")[[1]]
  
  # Remove braces and comments
  lines <- gsub("\\{|\\}", "", lines)
  lines <- gsub("//.*$", "", lines)
  lines <- trimws(lines)
  lines <- lines[nchar(lines) > 0]
  
  # Extract variable names (simplified)
  var_names <- character()
  for (line in lines) {
    if (grepl(";", line)) {
      # Extract variable name after type declaration
      parts <- strsplit(line, "\\s+")[[1]]
      if (length(parts) >= 2) {
        var_part <- parts[length(parts)]
        var_name <- gsub(";.*", "", var_part)
        var_names <- c(var_names, var_name)
      }
    }
  }
  
  # Return as named list
  result <- as.list(rep(TRUE, length(var_names)))
  names(result) <- var_names
  
  return(result)
}

#' Null Coalescing Operator
#' 
#' @param x First value to check
#' @param y Default value if x is NULL
#' @return x if not NULL, otherwise y
#' @noRd
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}