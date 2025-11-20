#' Unified GLM Analysis System
#'
#' @description
#' Provides unified GLM analysis that replaces multiple detect_glm_usage() calls
#' with a single comprehensive analysis pass.
#'
#' @name glm_analysis
NULL

#' Analyze Stan Code for GLM Usage and Patterns
#'
#' @description
#' Single entry point for GLM analysis performing comprehensive GLM detection, 
#' classification, and optimization decisions in a single pass.
#'
#' @param stan_code Character string containing Stan code to analyze
#' @param response_names Character vector of response variable names (optional)
#' @param trend_info List containing trend information for optimization decisions (optional)
#'
#' @return S3 object of class "glm_analysis" containing:
#'   \itemize{
#'     \item glm_patterns - Named logical vector of detected GLM patterns
#'     \item mu_classification - Classification of mu construction patterns
#'     \item optimization_plan - Decisions about GLM preservation vs conversion
#'     \item response_mapping - Mapping between responses and GLM usage
#'   }
#'
#' @examples
#' \dontrun{
#' stan_code <- "model { target += poisson_log_glm_lpmf(Y | X, alpha, beta); }"
#' analysis <- analyze_stan(stan_code)
#' }
#'
#' @noRd
analyze_stan <- function(stan_code, response_names = NULL, trend_info = NULL) {
  checkmate::assert_character(stan_code, len = 1)
  checkmate::assert_character(response_names, null.ok = TRUE)
  
  if (nchar(stan_code) == 0) {
    insight::format_error("Stan code cannot be empty")
  }
  
  glm_patterns <- detect_glm_patterns(stan_code)
  mu_classification <- classify_mu_patterns(stan_code, glm_patterns)
  optimization_plan <- determine_glm_preservation(glm_patterns, trend_info)
  response_mapping <- create_response_mapping(stan_code, response_names, glm_patterns)
  
  structure(
    list(
      glm_patterns = glm_patterns,
      mu_classification = mu_classification,
      optimization_plan = optimization_plan,
      response_mapping = response_mapping,
      stan_code = stan_code
    ),
    class = "glm_analysis"
  )
}

#' Detect GLM Patterns in Stan Code
#'
#' @description
#' Comprehensive GLM pattern detection for all supported GLM types.
#'
#' @param stan_code Character string containing Stan code to analyze
#'
#' @return Named logical vector indicating which GLM patterns are present
#'
#' @noRd
detect_glm_patterns <- function(stan_code) {
  checkmate::assert_character(stan_code, len = 1)
  
  glm_patterns <- c(
    "normal_id_glm",
    "poisson_log_glm", 
    "neg_binomial_2_log_glm",
    "bernoulli_logit_glm",
    "ordered_logistic_glm",
    "categorical_logit_glm"
  )
  
  detected <- vapply(glm_patterns, function(pattern) {
    any(grepl(paste0("target\\s*\\+=.*", pattern, "_l(pdf|pmf)"), stan_code))
  }, logical(1))
  
  names(detected) <- glm_patterns
  return(detected)
}

#' Classify Mu Construction Patterns
#'
#' @description
#' Classifies mu construction patterns in the presence of GLM optimizations.
#'
#' @param stan_code Character string containing Stan code
#' @param glm_patterns Named logical vector of detected GLM patterns
#'
#' @return List containing mu pattern classification results
#'
#' @noRd
classify_mu_patterns <- function(stan_code, glm_patterns) {
  checkmate::assert_character(stan_code, len = 1)
  checkmate::assert_logical(glm_patterns)
  
  has_glm <- any(glm_patterns)
  glm_variables <- names(glm_patterns)[glm_patterns]
  
  mu_patterns <- list(
    has_glm = has_glm,
    glm_types = glm_variables,
    requires_conversion = has_glm && any(glm_patterns),
    mu_construction_type = determine_mu_construction_type(stan_code, has_glm)
  )
  
  return(mu_patterns)
}

#' Determine GLM Preservation Strategy
#'
#' @description
#' Decides whether to preserve GLM optimization or convert to standard form
#' based on trend requirements.
#'
#' @param glm_patterns Named logical vector of detected GLM patterns  
#' @param trend_info List containing trend information (optional)
#'
#' @return List containing optimization decisions
#'
#' @noRd
determine_glm_preservation <- function(glm_patterns, trend_info = NULL) {
  checkmate::assert_logical(glm_patterns)
  
  has_glm <- any(glm_patterns)
  has_trends <- !is.null(trend_info) && length(trend_info) > 0
  
  optimization_plan <- list(
    preserve_glm = has_glm && !has_trends,
    convert_to_standard = has_glm && has_trends,
    glm_types_to_convert = if (has_trends) names(glm_patterns)[glm_patterns] else character(0),
    optimization_reason = determine_optimization_reason(has_glm, has_trends)
  )
  
  return(optimization_plan)
}

#' Create Response-GLM Mapping
#'
#' @description
#' Creates mapping between response variables and GLM usage for multivariate models.
#'
#' @param stan_code Character string containing Stan code
#' @param response_names Character vector of response variable names (optional)
#' @param glm_patterns Named logical vector of detected GLM patterns
#'
#' @return Named list mapping responses to GLM usage
#'
#' @noRd
create_response_mapping <- function(stan_code, response_names, glm_patterns) {
  checkmate::assert_character(stan_code, len = 1)
  checkmate::assert_character(response_names, null.ok = TRUE)
  checkmate::assert_logical(glm_patterns)
  
  if (is.null(response_names)) {
    return(list(global_glm_usage = any(glm_patterns)))
  }
  
  lines <- strsplit(stan_code, "\n")[[1]]
  model_start <- grep("^\\s*model\\s*\\{", lines)
  
  if (length(model_start) == 0) {
    return(setNames(rep(FALSE, length(response_names)), response_names))
  }
  
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

#' Determine Mu Construction Type
#'
#' @description
#' Determines the type of mu construction in Stan code.
#'
#' @param stan_code Character string containing Stan code
#' @param has_glm Logical indicating presence of GLM patterns
#'
#' @return Character string describing mu construction type
#'
#' @noRd
determine_mu_construction_type <- function(stan_code, has_glm) {
  checkmate::assert_character(stan_code, len = 1)
  checkmate::assert_flag(has_glm)
  
  if (has_glm) {
    return("glm_optimized")
  } else if (grepl("mu\\s*\\[.*\\]\\s*=", stan_code)) {
    return("array_assignment")
  } else if (grepl("mu\\s*\\+=", stan_code)) {
    return("additive_construction")
  } else {
    return("standard")
  }
}

#' Determine Optimization Reason
#'
#' @description
#' Provides reason for optimization decisions.
#'
#' @param has_glm Logical indicating presence of GLM patterns
#' @param has_trends Logical indicating presence of trend requirements
#'
#' @return Character string explaining optimization decision
#'
#' @noRd
determine_optimization_reason <- function(has_glm, has_trends) {
  checkmate::assert_flag(has_glm)
  checkmate::assert_flag(has_trends)
  
  if (!has_glm) {
    return("no_glm_detected")
  } else if (has_trends) {
    return("trends_require_standard_form")
  } else {
    return("preserve_brms_optimization")
  }
}

#' Create Processing State Object
#'
#' @description
#' S3 constructor for immutable state objects that track Stan code processing
#' through the linear transformation pipeline.
#'
#' @param code_lines Character vector of Stan code lines
#' @param processed_positions Integer vector of line positions already processed
#' @param model_block List with start and end indices of model block
#' @param transformations_applied Character vector logging applied transformations
#' @param stage Character string indicating current processing stage
#'
#' @return S3 object of class "processing_state"
#'
#' @noRd
processing_state <- function(code_lines, processed_positions = integer(0), 
                            model_block = list(start = NA_integer_, end = NA_integer_),
                            transformations_applied = character(0),
                            stage = "initial",
                            operations_log = NULL) {
  
  checkmate::assert_character(code_lines, min.len = 1)
  checkmate::assert_integerish(processed_positions, null.ok = TRUE)
  checkmate::assert_list(model_block)
  checkmate::assert_character(transformations_applied)
  checkmate::assert_choice(stage, c("initial", "analyzed", "converted", "injected", "assembled"))
  
  if (!is.null(model_block$start) && !is.na(model_block$start)) {
    checkmate::assert_int(model_block$start, lower = 1)
  }
  if (!is.null(model_block$end) && !is.na(model_block$end)) {
    checkmate::assert_int(model_block$end, lower = 1)
  }
  
  structure(
    list(
      code_lines = code_lines,
      processed_positions = processed_positions,
      model_block = model_block,
      transformations_applied = transformations_applied,
      stage = stage,
      analysis = NULL,
      mu_analysis = NULL,
      operations_log = operations_log
    ),
    class = "processing_state"
  )
}

#' Transition to Analysis Stage
#'
#' @description
#' Analyzes GLM patterns and returns new state object with analysis results.
#'
#' @param state Object of class "processing_state"
#' @param response_names Character vector of response names (optional)
#' @param trend_info List containing trend information (optional)
#'
#' @return New "processing_state" object with analysis stage and GLM information
#'
#' @noRd
to_analysis <- function(state, response_names = NULL, trend_info = NULL) {
  checkmate::assert_class(state, "processing_state")
  checkmate::assert_character(response_names, null.ok = TRUE)
  
  if (state$stage != "initial") {
    format_pipeline_error(
      "Invalid state for GLM analysis",
      list(
        expected_stage = "initial",
        actual_stage = state$stage,
        operation = "to_analysis"
      )
    )
  }
  
  stan_code <- paste(state$code_lines, collapse = "\n")
  analysis <- analyze_stan(stan_code, response_names, trend_info)
  
  structure(
    list(
      code_lines = state$code_lines,
      processed_positions = state$processed_positions,
      model_block = state$model_block,
      transformations_applied = c(state$transformations_applied, "glm_analysis"),
      stage = "analyzed",
      analysis = analysis
    ),
    class = "processing_state"
  )
}

#' Transition to Conversion Stage
#'
#' @description
#' Converts GLM to standard form when needed using analysis from previous state.
#'
#' @param state Object of class "processing_state" in "analyzed" stage
#'
#' @return New "processing_state" object with conversion stage and modified code
#'
#' @noRd
to_conversion <- function(state) {
  checkmate::assert_class(state, "processing_state")
  
  if (state$stage != "analyzed") {
    format_pipeline_error(
      "Invalid state for GLM conversion",
      list(
        expected_stage = "analyzed",
        actual_stage = state$stage,
        operation = "to_conversion"
      )
    )
  }
  
  if (!state$analysis$optimization_plan$convert_to_standard) {
    new_state <- processing_state(
      code_lines = state$code_lines,
      processed_positions = state$processed_positions,
      model_block = state$model_block,
      transformations_applied = c(state$transformations_applied, "conversion_skipped"),
      stage = "converted"
    )
    new_state$analysis <- state$analysis
    return(new_state)
  }
  
  conversion_result <- convert_glm_to_standard_linear(state$code_lines, state$analysis)
  
  new_state <- processing_state(
    code_lines = conversion_result$updated_code,
    processed_positions = conversion_result$new_positions,
    model_block = update_model_block_positions(state$model_block, conversion_result),
    transformations_applied = c(state$transformations_applied, "glm_converted"),
    stage = "converted"
  )
  new_state$analysis <- state$analysis
  
  return(new_state)
}

#' Transition to Injection Stage
#'
#' @description
#' Injects trend effects using existing mu construction analysis.
#'
#' @param state Object of class "processing_state" in "converted" stage
#' @param trend_injection_code Character string containing trend injection code
#'
#' @return New "processing_state" object with injection stage and trend effects
#'
#' @noRd
to_injection <- function(state, trend_injection_code) {
  checkmate::assert_class(state, "processing_state")
  checkmate::assert_character(trend_injection_code, len = 1)
  
  if (state$stage != "converted") {
    format_pipeline_error(
      "Invalid state for trend injection",
      list(
        expected_stage = "converted",
        actual_stage = state$stage,
        operation = "to_injection"
      )
    )
  }
  
  stan_code <- paste(state$code_lines, collapse = "\n")
  
  # Extract mu construction with classification for smart injection
  mu_analysis <- extract_mu_construction_with_classification(stan_code)
  
  injected_code <- inject_trend_effects_linear(stan_code, trend_injection_code)
  injected_lines <- strsplit(injected_code, "\n")[[1]]
  
  new_state <- processing_state(
    code_lines = injected_lines,
    processed_positions = state$processed_positions,
    model_block = state$model_block,
    transformations_applied = c(state$transformations_applied, "mu_analyzed", "trend_injected"),
    stage = "injected"
  )
  new_state$analysis <- state$analysis
  new_state$mu_analysis <- mu_analysis
  
  return(new_state)
}

#' Transition to Assembly Stage
#'
#' @description
#' Assembles processed Stan code into final result.
#'
#' @param state Object of class "processing_state" in "injected" stage
#'
#' @return Character string containing final assembled Stan code
#'
#' @noRd
to_assembly <- function(state) {
  checkmate::assert_class(state, "processing_state")
  
  if (state$stage != "injected") {
    format_pipeline_error(
      "Invalid state for code assembly",
      list(
        expected_stage = "injected",
        actual_stage = state$stage,
        operation = "to_assembly"
      )
    )
  }
  
  final_code <- paste(state$code_lines, collapse = "\n")
  return(final_code)
}

#' Convert GLM to Standard Form (Linear)
#'
#' @description
#' Linear conversion of GLM patterns to standard form without recursion.
#'
#' @param code_lines Character vector of Stan code lines
#' @param analysis Object of class "glm_analysis" with GLM information
#'
#' @return List with code_lines and processed_glm_lines
#'
#' @noRd
convert_glm_to_standard_linear <- function(code_lines, analysis) {
  checkmate::assert_character(code_lines, min.len = 1)
  checkmate::assert_class(analysis, "glm_analysis")
  
  if (!analysis$optimization_plan$convert_to_standard) {
    return(list(code_lines = code_lines, processed_glm_lines = integer(0)))
  }
  
  block_info <- find_stan_block(code_lines, "model")
  if (is.null(block_info)) {
    format_pipeline_error(
      "Model block missing from Stan code",
      list(
        operation = "glm_conversion",
        code_length = length(code_lines)
      )
    )
  }
  
  if (block_info$start_idx > block_info$end_idx || block_info$end_idx > length(code_lines)) {
    insight::format_error(
      "Invalid block indices in model block: start={block_info$start_idx}, end={block_info$end_idx}, code length={length(code_lines)}"
    )
  }
  
  modified_lines <- code_lines
  processed_glm_lines <- integer(0)
  
  glm_types_to_convert <- names(analysis$glm_patterns)[analysis$glm_patterns]
  
  for (glm_type in glm_types_to_convert) {
    model_block_text <- paste(modified_lines[block_info$start_idx:block_info$end_idx], collapse = "\n")
    params <- parse_glm_parameters(model_block_text, glm_type)
    
    if (is.null(params)) {
      insight::format_error(
        "Could not parse GLM parameters for type: {glm_type}"
      )
    }
    
    glm_pattern <- paste0(glm_type, "_l(pdf|pmf)")
    glm_line_idx <- NULL
    for (i in block_info$start_idx:block_info$end_idx) {
      if (grepl(glm_pattern, modified_lines[i]) && !i %in% processed_glm_lines) {
        glm_line_idx <- i
        break
      }
    }
    
    if (is.null(glm_line_idx)) {
      next
    }
    
    model_text <- paste(modified_lines[block_info$start_idx:block_info$end_idx], collapse = "\n")
    mu_already_exists <- grepl("vector\\[N\\]\\s+mu\\s*=", model_text) || grepl("mu\\s*\\+=", model_text)
    
    if (mu_already_exists) {
      mu_initialization <- c("    // Add fixed effects for existing mu")
    } else {
      mu_initialization <- c(
        "    // Initialize mu for trend injection",  
        "    vector[N] mu = rep_vector(0.0, N);"
      )
    }
    
    if (!is.null(params$design_matrix) && !is.null(params$coefficients)) {
      fixed_effects_line <- paste0("    mu += ", params$design_matrix, " * ", params$coefficients, ";")
      mu_initialization <- c(mu_initialization, fixed_effects_line)
    }
    
    if (!is.null(params$intercept) && params$intercept != "0" && params$intercept != "0.0") {
      mu_initialization <- c(mu_initialization, paste0("    mu += ", params$intercept, ";"))
    }
    
    modified_lines[glm_line_idx] <- paste0(
      paste(mu_initialization, collapse = "\n"), 
      "\n", 
      modified_lines[glm_line_idx]
    )
    
    processed_glm_lines <- c(processed_glm_lines, glm_line_idx)
  }
  
  return(list(
    code_lines = modified_lines,
    processed_glm_lines = processed_glm_lines
  ))
}

#' Inject Trend Effects
#'
#' @description
#' Injects trend effects after mu construction without recursion.
#' Finds last mu += line and inserts trend code after it.
#'
#' @param stan_code Character string containing Stan code
#' @param trend_injection_code Character string containing trend injection code
#'
#' @return Character string with trend effects injected
#'
#' @noRd
inject_trend_effects_linear <- function(stan_code, trend_injection_code) {
  checkmate::assert_character(stan_code, len = 1)
  checkmate::assert_character(trend_injection_code, len = 1)
  
  code_lines <- strsplit(stan_code, "\n")[[1]]
  
  block_info <- find_stan_block(code_lines, "model")
  if (is.null(block_info)) {
    format_pipeline_error(
      "Model block missing from Stan code",
      list(
        operation = "trend_injection",
        code_length = length(code_lines)
      )
    )
  }
  
  model_lines <- code_lines[block_info$start_idx:block_info$end_idx]
  mu_line_indices <- which(grepl("\\s*mu\\s*\\+=", model_lines))
  
  if (length(mu_line_indices) > 0) {
    last_mu_idx <- max(mu_line_indices)
    absolute_mu_idx <- block_info$start_idx + last_mu_idx - 1
    
    trend_lines <- strsplit(trend_injection_code, "\n")[[1]]
    
    updated_lines <- c(
      code_lines[1:absolute_mu_idx],
      trend_lines,
      code_lines[(absolute_mu_idx + 1):length(code_lines)]
    )
    
    return(paste(updated_lines, collapse = "\n"))
  } else {
    code_lines <- strsplit(stan_code, "\n")[[1]]
    block_info <- find_stan_block(code_lines, "model")
    if (is.null(block_info)) {
      return(stan_code)
    }
    
    trend_lines <- strsplit(trend_injection_code, "\n")[[1]]
    modified_lines <- handle_nonlinear_trend_injection(code_lines, block_info, trend_lines)
    return(paste(modified_lines, collapse = "\n"))
  }
}

#' Transition State with Operation Tracking
#'
#' @description
#' Creates new state with stage transition and operation tracking in one call.
#' Eliminates duplicate pattern across state transition functions.
#'
#' @param state Object of class "processing_state"
#' @param new_stage Character string for new processing stage
#' @param operation Character string describing the operation performed
#' @param details List with additional operation details (optional)
#' @param modifications List with state field modifications (optional)
#'
#' @return New "processing_state" object with transition and operation logged
#'
#' @examples
#' \dontrun{
#' state <- transition_with_tracking(state, "analyzed", "glm_analysis",
#'                                  list(patterns = 3),
#'                                  list(analysis = analysis_result))
#' }
#'
#' @noRd
transition_with_tracking <- function(state, new_stage, operation, 
                                   details = list(), modifications = list()) {
  checkmate::assert_class(state, "processing_state")
  checkmate::assert_character(new_stage, len = 1)
  checkmate::assert_character(operation, len = 1, min.chars = 1)
  checkmate::assert_list(details)
  checkmate::assert_list(modifications)
  
  # Verify all original state fields exist and are not NULL
  required_fields <- c("code_lines", "processed_positions", "model_block", 
                      "transformations_applied", "stage", "analysis", 
                      "mu_analysis")
  missing_fields <- required_fields[!required_fields %in% names(state)]
  if (length(missing_fields) > 0) {
    insight::format_error("State object missing required {.field {missing_fields}}")
  }
  
  # Apply modifications or use existing values
  new_state <- processing_state(
    code_lines = modifications$code_lines %||% state$code_lines,
    processed_positions = modifications$processed_positions %||% state$processed_positions,
    model_block = modifications$model_block %||% state$model_block,
    transformations_applied = c(state$transformations_applied, operation),
    stage = new_stage,
    operations_log = state$operations_log
  )
  
  # Apply additional field modifications
  if (!is.null(modifications$analysis)) {
    new_state$analysis <- modifications$analysis
  } else if (!is.null(state$analysis)) {
    new_state$analysis <- state$analysis
  }
  
  if (!is.null(modifications$mu_analysis)) {
    new_state$mu_analysis <- modifications$mu_analysis
  } else if (!is.null(state$mu_analysis)) {
    new_state$mu_analysis <- state$mu_analysis
  }
  
  # Track operation with details
  track_operations(new_state, operation, details)
}

#' Track Processing Operations
#'
#' @description
#' Adds operation tracking to processing state for debugging and validation.
#' Maintains immutable list of completed operations with timestamps.
#'
#' @param state Object of class "processing_state"
#' @param operation Character string describing the operation performed
#' @param details List with additional operation details (optional)
#'
#' @return New "processing_state" object with operation logged
#'
#' @examples
#' \dontrun{
#' state <- processing_state(code_lines)
#' state <- track_operations(state, "glm_analysis", 
#'                          list(patterns_found = 3))
#' }
#'
#' @noRd
track_operations <- function(state, operation, details = list()) {
  checkmate::assert_class(state, "processing_state")
  checkmate::assert_character(operation, len = 1, min.chars = 1)
  checkmate::assert_list(details)
  
  # Verify all original state fields are preserved
  required_fields <- c("code_lines", "processed_positions", "model_block", 
                      "transformations_applied", "stage", "analysis", 
                      "mu_analysis")
  if (!all(required_fields %in% names(state))) {
    insight::format_error("State object missing required fields")
  }
  
  operation_entry <- list(
    operation = operation,
    stage = state$stage,
    timestamp = Sys.time(),
    details = details
  )
  
  new_transformations <- c(state$transformations_applied, operation)
  
  operations_log <- if (is.null(state$operations_log)) {
    list(operation_entry)
  } else {
    c(state$operations_log, list(operation_entry))
  }
  
  structure(
    list(
      code_lines = state$code_lines,
      processed_positions = state$processed_positions,
      model_block = state$model_block,
      transformations_applied = new_transformations,
      stage = state$stage,
      analysis = state$analysis,
      mu_analysis = state$mu_analysis,
      operations_log = operations_log
    ),
    class = "processing_state"
  )
}

#' Get Operations Summary
#'
#' @description
#' Extracts operations summary from processing state for debugging.
#'
#' @param state Object of class "processing_state"
#'
#' @return Data frame with operations summary
#'
#' @noRd
get_operations_summary <- function(state) {
  checkmate::assert_class(state, "processing_state")
  
  if (is.null(state$operations_log) || length(state$operations_log) == 0) {
    return(data.frame(
      operation = character(0),
      stage = character(0),
      timestamp = Sys.time()[0],
      stringsAsFactors = FALSE
    ))
  }
  
  operations_df <- data.frame(
    operation = vapply(state$operations_log, function(x) x$operation, 
                      character(1)),
    stage = vapply(state$operations_log, function(x) x$stage, character(1)),
    timestamp = do.call(c, lapply(state$operations_log, 
                                 function(x) x$timestamp)),
    stringsAsFactors = FALSE
  )
  
  return(operations_df)
}

#' Update Model Block Positions
#'
#' @description
#' Updates model block start/end positions after code modifications.
#'
#' @param model_block List with start and end positions
#' @param conversion_result List containing modification information
#'
#' @return Updated model_block list
#'
#' @noRd
update_model_block_positions <- function(model_block, conversion_result) {
  checkmate::assert_list(model_block)
  checkmate::assert_list(conversion_result)
  
  return(model_block)
}

#' Transform GLM Code Through Linear Pipeline
#'
#' @description
#' Main pipeline function that chains state transitions linearly:
#' analysis → conversion → injection → assembly.
#'
#' @param stan_code Character string containing Stan code to process
#' @param trend_injection_code Character string containing trend injection code
#' @param response_names Character vector of response variable names (optional)
#' @param trend_info List containing trend information (optional)
#'
#' @return Character string containing processed Stan code
#'
#' @examples
#' \dontrun{
#' stan_code <- "model { target += poisson_log_glm_lpmf(Y | X, alpha, beta); }"
#' processed <- transform_glm_code(stan_code, "mu += trend;")
#' }
#'
#' @noRd
transform_glm_code <- function(stan_code, trend_injection_code, response_names = NULL, trend_info = NULL) {
  checkmate::assert_character(stan_code, len = 1)
  checkmate::assert_character(trend_injection_code, len = 1)
  checkmate::assert_character(response_names, null.ok = TRUE)
  checkmate::assert_list(trend_info, null.ok = TRUE)
  
  if (nchar(stan_code) == 0) {
    insight::format_error("Stan code cannot be empty for parameter {.field stan_code}")
  }
  
  # Chain state transitions
  code_lines <- strsplit(stan_code, "\n")[[1]]
  state <- processing_state(code_lines, stage = "initial")
  state <- to_analysis(state, response_names, trend_info)
  state <- to_conversion(state)
  state <- to_injection(state, trend_injection_code)
  
  return(to_assembly(state))
}