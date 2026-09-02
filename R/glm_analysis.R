#' GLM Analysis System
#'
#' @description
#' Provides GLM analysis that replaces multiple detect_glm_usage() calls
#' with a single analysis pass.
#'
#' @name glm_analysis
NULL

#' Analyze Stan Code for GLM Usage and Patterns
#'
#' @description
#' Single entry point for GLM analysis performing GLM detection,
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
#' @noRd
analyze_stan <- function(stan_code, response_names = NULL, trend_info = NULL) {
  checkmate::assert_character(stan_code, len = 1)
  checkmate::assert_character(response_names, null.ok = TRUE)

  if (nchar(stan_code) == 0) {
    stop(insight::format_error("Stan code cannot be empty"))
  }

  glm_patterns <- detect_glm_patterns(stan_code)
  mu_classification <- classify_mu_patterns(stan_code, glm_patterns)
  optimization_plan <- determine_glm_preservation(glm_patterns, trend_info)
  response_mapping <- create_response_mapping(stan_code, response_names, glm_patterns)

  # Parse GLM parameters during analysis phase
  detected_glm_types <- names(glm_patterns)[glm_patterns]
  glm_parameters <- setNames(vector("list", length(detected_glm_types)), detected_glm_types)

  for (glm_type in detected_glm_types) {
    glm_parameters[[glm_type]] <- parse_glm_parameters_single(stan_code, glm_type)
  }

  # Filter out failed parsing attempts
  glm_parameters <- glm_parameters[!vapply(glm_parameters, is.null, logical(1))]

  structure(
    list(
      glm_patterns = glm_patterns,
      mu_classification = mu_classification,
      optimization_plan = optimization_plan,
      response_mapping = response_mapping,
      glm_parameters = glm_parameters,
      stan_code = stan_code
    ),
    class = "glm_analysis"
  )
}


# How each brms GLM likelihood lays its arguments out, and what the
# same likelihood looks like once the linear predictor has been pulled
# out into `mu`. brms folds the predictor into these calls, so a trend
# has no `mu` to reach until the call is rewritten.
#
# `arguments` names the role of every argument after the `|`, in the
# order brms writes it. The layouts differ: `normal_id_glm` and
# friends take `(x, alpha, beta)` with a scalar intercept, while
# `ordered_logistic_glm` takes `(x, beta, cutpoints)` and carries no
# intercept at all. Reading positions off one shared layout gives the
# ordinal families a design matrix multiplied by cutpoints.
#
# `rewrite` returns the arguments the replacement call takes, given
# the parsed roles, the `to_matrix(mu)` expression standing in for the
# design matrix and the `vector[1]` of ones standing in for the
# coefficients.
#
# `brms::categorical()` has no entry because mvgam refuses it in
# favour of `categ()`, and its per-category predictor has no single
# `mu` to unwind into. A `categorical_logit_glm` line would therefore
# be reported as an unrecognised family rather than silently rewritten.
glm_call_layout <- list(
  normal_id_glm = list(
    arguments = c("design_matrix", "intercept", "coefficients", "sigma"),
    rewrite = function(params, x, ones) c(x, "0.0", ones, params$sigma)
  ),
  poisson_log_glm = list(
    arguments = c("design_matrix", "intercept", "coefficients"),
    rewrite = function(params, x, ones) c(x, "0.0", ones)
  ),
  neg_binomial_2_log_glm = list(
    arguments = c("design_matrix", "intercept", "coefficients", "shape"),
    rewrite = function(params, x, ones) c(x, "0.0", ones, params$shape)
  ),
  bernoulli_logit_glm = list(
    arguments = c("design_matrix", "intercept", "coefficients"),
    rewrite = function(params, x, ones) c(x, "0.0", ones)
  ),
  ordered_logistic_glm = list(
    arguments = c("design_matrix", "coefficients", "cutpoints"),
    rewrite = function(params, x, ones) c(x, ones, params$cutpoints)
  )
)

# The GLM likelihoods mvgam knows how to unwind, stated once so that
# detection, the type gate on transformation and the per-line type
# lookup cannot disagree with the layouts above.
mvgam_glm_families <- names(glm_call_layout)

#' The argument layout for one GLM family
#'
#' @param glm_type Family name including the `_glm` suffix, e.g.
#'   `"ordered_logistic_glm"`.
#' @return The `glm_call_layout` entry for that family.
#' @noRd
glm_layout_for <- function(glm_type) {
  checkmate::assert_string(glm_type, min.chars = 1)
  layout <- glm_call_layout[[glm_type]]
  if (is.null(layout)) {
    stop(insight::format_error(c(
      cli::format_inline("Unsupported GLM type: {.field {glm_type}}"),
      i = paste0(
        "Recognised families are: ",
        paste(mvgam_glm_families, collapse = ", "), "."
      )
    )), call. = FALSE)
  }
  layout
}

#' Which GLM likelihoods a Stan program calls
#'
#' @param stan_code Character vector of Stan source.
#' @return Named logical over `mvgam_glm_families`.
#' @noRd
glm_calls_present <- function(stan_code) {
  checkmate::assert_character(stan_code, min.len = 1)
  vapply(mvgam_glm_families, function(fam) {
    any(grepl(paste0("target\\s*\\+=.*", stan_density_call_pattern(fam)),
              stan_code))
  }, logical(1L))
}

#' Detect GLM Patterns in Stan Code
#'
#' @param stan_code Character string containing Stan model code
#' @return Named logical vector, one entry per GLM family
#' @noRd
detect_glm_patterns <- function(stan_code) {
  checkmate::assert_character(stan_code, len = 1)
  glm_calls_present(stan_code)
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
    preserve_glm = has_glm,
    convert_to_standard = FALSE,
    glm_types_to_convert = character(0),
    optimization_reason = determine_optimization_reason(has_glm, FALSE)
  )

  return(optimization_plan)
}

#' Which responses brms wrote a GLM-optimised likelihood for
#'
#' Where it can, brms folds the linear predictor into the likelihood
#' call itself, as in `poisson_log_glm_lpmf(Y | Xc, Intercept, b)`.
#' That form constructs `mu` internally and never names it, so a trend
#' has no `mu` to reach and the call has to be unwound before one can
#' be injected. Every caller deciding whether to unwind asks here.
#'
#' @param lines Character vector of Stan source lines.
#' @param response_names Character vector of response names.
#' @return Named logical, one entry per response, TRUE where brms used
#'   the GLM form.
#' @noRd
map_responses_to_glm <- function(lines, response_names) {
  checkmate::assert_character(lines)
  checkmate::assert_character(response_names)
  result <- setNames(rep(FALSE, length(response_names)), response_names)
  model_lines <- stan_block_body(lines, "model")
  if (is.null(model_lines)) return(result)

  likelihood_lines <- grep(
    paste0("target \\+=.*", stan_density_call_pattern()), model_lines,
    value = TRUE
  )
  for (line in likelihood_lines) {
    matched <- regmatches(line, regexpr("Y_\\w+", line))
    if (length(matched) == 0L) next
    # Anchored, so a response whose own name contains `Y_` survives.
    response <- sub("^Y_", "", matched[1L])
    if (response %in% response_names) {
      result[[response]] <- grepl(stan_density_call_pattern("_glm"), line)
    }
  }
  result
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
  return(map_responses_to_glm(lines, response_names))
}

#' Inject Trends Into GLM Function Calls
#'
#' @description
#' Transforms GLM function calls to include trend effects while preserving 
#' GLM optimization by creating mu vectors and converting calls to to_matrix format.
#'
#' @param code_lines Character vector of Stan code lines
#' @param block_info List with model block start_idx and end_idx
#' @param trend_injection_code Character string with trend injection code  
#'
#' @return Character vector of modified code lines
#'
#' @noRd
inject_trends_into_glm_calls <- function(code_lines, block_info, trend_injection_code) {
  checkmate::assert_character(code_lines)
  checkmate::assert_list(block_info)
  checkmate::assert_character(trend_injection_code, len = 1)
  
  
  # Find GLM calls in model block
  model_range <- seq(block_info$start_idx, block_info$end_idx)
  glm_line_idx <- NULL
  glm_type <- NULL
  
  for (i in model_range) {
    line <- code_lines[i]
    if (grepl(stan_density_call_pattern("_glm"), line)) {
      glm_line_idx <- i
      
      
      # The family this line calls, under either density spelling.
      for (fam in mvgam_glm_families) {
        if (grepl(stan_density_call_pattern(fam), line)) {
          glm_type <- fam
          break
        }
      }
      
      break
    }
  }
  
  
  # No GLM call in the model block is the ordinary case for a family
  # brms does not optimise, and there is nothing to unwind.
  if (is.null(glm_line_idx)) {
    return(code_lines)
  }

  # Returning the line untouched here would leave the trend computed
  # but never added to the linear predictor: a model with no trend
  # that compiles and samples without complaint. Refuse instead, so a
  # family added to `mvgam_glm_families` without a transformation is
  # caught at code generation rather than in the results.
  if (is.null(glm_type)) {
    stop(insight::format_error(c(
      "Found a GLM likelihood whose family could not be identified.",
      x = paste0("The call was: ", trimws(code_lines[glm_line_idx])),
      i = paste0(
        "Recognised families are: ",
        paste(mvgam_glm_families, collapse = ", "), "."
      )
    )), call. = FALSE)
  }
  
  # Parse GLM parameters using existing pattern
  glm_params <- parse_glm_parameters_from_line(code_lines[glm_line_idx], glm_type)
  
  # Generate mu construction with trend effects
  mu_construction <- build_mu_with_trend_effects(glm_params, trend_injection_code)
  
  # Transform GLM call to use to_matrix(mu) format
  transformed_glm_call <- transform_glm_call_to_mu_format(code_lines[glm_line_idx], glm_type, glm_params)
  
  # Insert mu construction and replace GLM call
  modified_lines <- c(
    code_lines[1:(glm_line_idx - 1)],
    mu_construction,
    "",
    transformed_glm_call,
    code_lines[(glm_line_idx + 1):length(code_lines)]
  )
  
  
  return(modified_lines)
}

#' Parse GLM Parameters from Line
#'
#' @param glm_line Character string containing GLM function call
#' @param glm_type Character string of GLM type, including the `_glm`
#'   suffix (e.g., "poisson_log_glm")
#'
#' @return List naming each parsed argument by the role the family
#'   gives it in `glm_call_layout`, plus `y_var` and `response_name`.
#'
#' @noRd
parse_glm_parameters_from_line <- function(glm_line, glm_type) {
  checkmate::assert_character(glm_line, len = 1)
  checkmate::assert_character(glm_type, len = 1)

  layout <- glm_layout_for(glm_type)

  # Match the density name as well as its brackets, so the argument
  # list is read off the GLM call itself and not off whatever
  # parenthesis happens to come first on the line.
  call_match <- regexpr(
    paste0(stan_density_call_pattern(glm_type), "\\([^)]+\\)"), glm_line
  )
  if (call_match < 0) {
    stop(insight::format_error(c(
      cli::format_inline("Invalid {.field {glm_type}} call format."),
      x = paste0("The line was: ", trimws(glm_line), ".")
    )))
  }

  call_content <- regmatches(glm_line, call_match)
  call_content <- sub("^[^(]*\\(", "", call_content)
  call_content <- sub("\\)$", "", call_content)

  # Split by | to get Y and parameters
  parts <- strsplit(call_content, "\\|")[[1]]
  if (length(parts) < 2) {
    stop(insight::format_error("GLM call missing required parameters"))
  }

  y_var <- trimws(parts[1])
  params_part <- trimws(parts[2])

  # Split parameters by comma
  params <- strsplit(params_part, ",")[[1]]
  params <- trimws(params)

  # Extract response name for mu variable
  resp_name <- if (y_var == "Y") "" else gsub("Y_", "", y_var)

  # Name the arguments by role rather than by position, so a family
  # whose layout differs from `(x, alpha, beta)` is read correctly.
  roles <- layout$arguments
  named <- as.list(params[seq_along(roles)])
  names(named) <- roles
  named <- named[!vapply(named, is.na, logical(1))]

  c(list(y_var = y_var, response_name = resp_name), named)
}

#' Build Mu Construction with Trend Effects
#'
#' @param glm_params List of parsed GLM parameters
#' @param trend_injection_code Character string with trend effects
#'
#' @return Character vector of mu construction code lines
#'
#' @noRd  
build_mu_with_trend_effects <- function(glm_params, trend_injection_code) {
  checkmate::assert_list(glm_params)
  checkmate::assert_character(trend_injection_code, len = 1)
  
  resp_name <- glm_params$response_name
  # Create mu variable name: "mu" for univariate, "mu_response" for multivariate
  mu_var <- if (resp_name == "") "mu" else paste0("mu_", resp_name)
  
  mu_lines <- c(
    paste0("  vector[N] ", mu_var, ";"),
    paste0("  ", mu_var, " = rep_vector(0.0, N);"),
    "",
    paste0("  // Add fixed effects"),
    paste0("  ", mu_var, " += ", glm_params$design_matrix, " * ",
           glm_params$coefficients, ";")
  )

  # Only some families carry a scalar intercept. The ordinal families
  # put their cutpoints where the others put an intercept, and those
  # belong in the likelihood rather than the linear predictor.
  if (!is.null(glm_params$intercept)) {
    mu_lines <- c(mu_lines,
                  paste0("  ", mu_var, " += ", glm_params$intercept, ";"))
  }
  mu_lines <- c(mu_lines, "")

  # Parse and add trend effects
  trend_lines <- strsplit(trend_injection_code, "\n")[[1]]
  trend_lines <- gsub("mu\\[n\\]", paste0(mu_var, "[n]"), trend_lines)
  
  mu_lines <- c(mu_lines, trend_lines)
  
  return(mu_lines)
}

#' Rewrite one GLM call to read a named linear predictor
#'
#' Substitutes `to_matrix(mu)` for the design matrix and a `vector[1]`
#' of ones for the coefficients, so the GLM primitive evaluates the
#' predictor mvgam built rather than the one brms folded in. Every
#' other argument keeps the role its family gives it, which is what
#' carries the ordinal cutpoints through unchanged.
#'
#' The density suffix is read off the call being replaced, so a
#' program generated under `normalize = FALSE` keeps its unnormalised
#' spelling instead of silently regaining the constants.
#'
#' @param glm_line Character string with the original GLM call.
#' @param glm_type Family name including the `_glm` suffix.
#' @param glm_params List of parsed GLM parameters.
#' @param mu_var Name of the linear predictor vector.
#' @param mu_ones_var Name of the `vector[1]` of ones.
#'
#' @return Character string holding the replacement call, without the
#'   surrounding `target +=` or terminating semicolon.
#'
#' @noRd
build_glm_call_on_mu <- function(glm_line, glm_type, glm_params,
                                 mu_var, mu_ones_var) {
  checkmate::assert_character(glm_line, len = 1)
  checkmate::assert_character(glm_type, len = 1)
  checkmate::assert_list(glm_params)
  checkmate::assert_string(mu_var, min.chars = 1)
  checkmate::assert_string(mu_ones_var, min.chars = 1)

  layout <- glm_layout_for(glm_type)
  suffix <- stan_density_suffix(glm_line, glm_type)
  if (is.null(suffix)) {
    stop(insight::format_error(c(
      cli::format_inline(
        "No {.field {glm_type}} call found in the line to transform."
      ),
      i = "The line was located by a pattern that should have matched."
    )))
  }

  arguments <- layout$rewrite(
    glm_params, paste0("to_matrix(", mu_var, ")"), mu_ones_var
  )
  paste0(
    glm_type, suffix, "(", glm_params$y_var, " | ",
    paste(arguments, collapse = ", "), ")"
  )
}

#' Transform GLM Call to Use Mu Format
#'
#' @param glm_line Character string with original GLM call
#' @param glm_type Family name including the `_glm` suffix
#' @param glm_params List of parsed GLM parameters
#'
#' @return Character string with transformed GLM call
#'
#' @noRd
transform_glm_call_to_mu_format <- function(glm_line, glm_type, glm_params) {
  checkmate::assert_list(glm_params)

  resp_name <- glm_params$response_name
  # Univariate models name the predictor "mu"; a multivariate model
  # names one predictor per response.
  mu_var <- if (resp_name == "") "mu" else paste0("mu_", resp_name)
  mu_ones_var <- if (resp_name == "") {
    "mu_ones"
  } else {
    paste0("mu_ones_", resp_name)
  }

  paste0(
    "  target += ",
    build_glm_call_on_mu(glm_line, glm_type, glm_params, mu_var, mu_ones_var),
    ";"
  )
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

  conversion_result <- transform_glm(paste(state$code_lines, collapse = "\n"), state$analysis)

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

#' Transform GLM Calls to Standard Form
#'
#' @description
#' Consolidated GLM transformation function that converts GLM calls to standard
#' form using structured analysis results. Handles all GLM types with
#' deterministic transformation rules.
#'
#' @param stan_code Character string containing Stan code to transform
#' @param analysis S3 glm_analysis object with cached GLM patterns and parameters
#'
#' @return Character string with transformed Stan code
#'
#' @noRd
transform_glm <- function(stan_code, analysis) {
  checkmate::assert_character(stan_code, len = 1)
  checkmate::assert_class(analysis, "glm_analysis")

  # Early return if no GLM conversion needed
  if (!analysis$optimization_plan$convert_to_standard) {
    return(stan_code)
  }

  # Convert to lines for processing
  code_lines <- strsplit(stan_code, "\n", fixed = TRUE)[[1]]

  # Find model block for GLM transformations
  block_info <- find_stan_block(code_lines, "model")
  if (is.null(block_info)) {
    format_pipeline_error(
      "Model block missing from Stan code",
      list(operation = "glm_transformation", code_length = length(code_lines))
    )
  }

  # Apply GLM transformations
  transformation_result <- apply_glm_transformations(code_lines, block_info, analysis)

  # Return structured result for state pipeline compatibility
  list(
    updated_code = paste(transformation_result$code_lines, collapse = "\n"),
    new_positions = transformation_result$processed_glm_lines
  )
}

#' Apply GLM Transformations to Code Lines
#'
#' @description
#' Core GLM transformation logic that converts GLM calls to standard form
#' using cached analysis results.
#'
#' @param code_lines Character vector of Stan code lines
#' @param block_info List with model block start/end indices
#' @param analysis S3 glm_analysis object with GLM patterns and parameters
#'
#' @return Character vector with transformed code lines
#'
#' @noRd
apply_glm_transformations <- function(code_lines, block_info, analysis) {
  modified_lines <- code_lines
  processed_glm_lines <- integer(0)

  # Get GLM types that need transformation
  glm_types_to_convert <- names(analysis$glm_patterns)[analysis$glm_patterns]

  for (glm_type in glm_types_to_convert) {
    # Use cached parameters from analysis
    if (is.null(analysis$glm_parameters[[glm_type]])) {
      stop(insight::format_error(c(
        cli::format_inline(
          "GLM parameters not found in analysis for type: {.field {glm_type}}"
        ),
        i = "Analysis object must contain pre-parsed GLM parameters."
      )), call. = FALSE)
    }
    params <- analysis$glm_parameters[[glm_type]]

    # Find GLM call line to transform
    glm_pattern <- stan_density_call_pattern(glm_type)
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

    # Check if mu already exists in the model
    model_text <- paste(modified_lines[block_info$start_idx:block_info$end_idx], collapse = "\n")
    mu_already_exists <- grepl("vector\\[N\\]\\s+mu\\s*=", model_text) || grepl("mu\\s*\\+=", model_text)

    # Generate mu initialization code
    if (mu_already_exists) {
      mu_initialization <- c("    // Add fixed effects for existing mu")
    } else {
      mu_initialization <- c(
        "    // Initialize mu with fixed effects",
        "    vector[N] mu = rep_vector(0.0, N);"
      )
    }

    # Build coefficient addition code
    coeff_addition <- build_coefficient_addition_code(params)

    # Transform the GLM call
    transformation_result <- transform_single_glm_call(
      modified_lines[glm_line_idx],
      glm_type,
      params
    )

    # Insert mu initialization before the GLM call
    insertion_point <- glm_line_idx - 1
    modified_lines <- append(modified_lines, mu_initialization, insertion_point)

    # Adjust indices after insertion
    offset <- length(mu_initialization)
    glm_line_idx <- glm_line_idx + offset

    # Add coefficient code
    modified_lines <- append(modified_lines, coeff_addition, glm_line_idx)
    offset <- offset + length(coeff_addition)
    glm_line_idx <- glm_line_idx + length(coeff_addition)

    # Replace the GLM call
    modified_lines[glm_line_idx + 1] <- transformation_result

    # Track processed line (adjusted for insertions)
    processed_glm_lines <- c(processed_glm_lines, glm_line_idx + 1)
  }

  # Return structured result
  list(
    code_lines = modified_lines,
    processed_glm_lines = processed_glm_lines
  )
}

#' Transform Single GLM Call
#'
#' @description
#' Transforms a single GLM function call to use combined linear predictor.
#'
#' @param glm_line Character string containing GLM function call
#' @param glm_type Family name including the `_glm` suffix
#' @param params List of parsed GLM parameters
#'
#' @return Character string with transformed GLM call
#'
#' @noRd
transform_single_glm_call <- function(glm_line, glm_type, params) {
  # The line keeps its surrounding statement, so only the call itself
  # is rebuilt; the arguments come from the same per-family layout the
  # trend-injection path reads.
  if (is.null(stan_density_suffix(glm_line, glm_type))) {
    return(glm_line)
  }
  replacement <- build_glm_call_on_mu(
    glm_line, glm_type, params, "mu", "mu_ones"
  )

  original_pattern <- paste0(stan_density_call_pattern(glm_type),
                             "\\s*\\([^\\)]+\\)")
  gsub(original_pattern, replacement, glm_line)
}

#' Build Coefficient Addition Code
#'
#' @description
#' Generates Stan code for adding coefficients to mu vector.
#'
#' @param params List of parsed GLM parameters
#'
#' @return Character vector of coefficient addition code lines
#'
#' @noRd
build_coefficient_addition_code <- function(params) {
  coeff_lines <- character(0)

  # Add design matrix multiplication if present
  if (!is.null(params$design_matrix)) {
    coeff_lines <- c(coeff_lines, paste0("    mu += ", params$design_matrix, " * ", params$coefficients, ";"))
  }

  # Add intercept if present
  if (!is.null(params$intercept)) {
    coeff_lines <- c(coeff_lines, paste0("    mu += ", params$intercept, ";"))
  }

  coeff_lines
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

    # Check if this is GLM code that needs GLM-compatible trend injection
    model_lines <- code_lines[block_info$start_idx:block_info$end_idx] 
    has_glm_calls <- any(grepl(stan_density_call_pattern("_glm"),
                               model_lines))
    
    
    if (has_glm_calls) {
      # GLM case: inject trend by modifying GLM calls to use trend-adjusted parameters
      modified_lines <- inject_trends_into_glm_calls(code_lines, block_info, trend_injection_code)
    } else {
      # Nonlinear case: use existing nonlinear injection logic
      trend_lines <- strsplit(trend_injection_code, "\n")[[1]]
      modified_lines <- handle_nonlinear_trend_injection(code_lines, block_info, trend_lines)
    }
    
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
    stop(insight::format_error(
      cli::format_inline(
        "State object missing required {.field {missing_fields}}"
      )
    ))
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
    stop(insight::format_error("State object missing required fields"))
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
#' @noRd
transform_glm_code <- function(stan_code, trend_injection_code, response_names = NULL, trend_info = NULL) {
  checkmate::assert_character(stan_code, len = 1)
  checkmate::assert_character(trend_injection_code, len = 1)
  checkmate::assert_character(response_names, null.ok = TRUE)
  checkmate::assert_list(trend_info, null.ok = TRUE)

  if (nchar(stan_code) == 0) {
    stop(insight::format_error(
      cli::format_inline(
        "Stan code cannot be empty for parameter {.field stan_code}"
      )
    ))
  }

  # Chain state transitions
  code_lines <- strsplit(stan_code, "\n")[[1]]
  state <- processing_state(code_lines, stage = "initial")
  state <- to_analysis(state, response_names, trend_info)
  state <- to_conversion(state)
  state <- to_injection(state, trend_injection_code)

  return(to_assembly(state))
}

