# Comprehensive mu Expression Analysis System
# 
# Replaces hard-coded regex patterns with dynamic structural analysis
# for all brms mu construction patterns including monotonic, GP, spline,
# random effects, and GLM optimization patterns.

#' Extract and classify mu construction expressions from Stan code
#' 
#' Replaces extract_mu_construction_from_model_block() with comprehensive
#' structural analysis that dynamically discovers patterns without hard-coding.
#' 
#' @param stancode Character string containing complete Stan model code
#' @return List with mu_construction, supporting_declarations, referenced_variables
#'   for compatibility with existing pipeline
#' @noRd
extract_mu_construction_with_classification <- function(stancode) {
  checkmate::assert_string(stancode, min.chars = 1)

  # Extract model block using existing utility
  model_block <- extract_stan_block_content(stancode, "model")

  if (is.null(model_block) || nchar(trimws(model_block)) == 0) {
    return(create_empty_mu_result())
  }

  # Extract mu construction lines
  mu_lines <- extract_mu_assignment_lines(model_block)
  if (length(mu_lines) == 0) {
    return(create_empty_mu_result())
  }
  
  # Classify each mu expression structurally
  classified_expressions <- classify_mu_expressions_structurally(mu_lines, stancode)
  
  # Extract comprehensive variable references
  referenced_vars <- extract_comprehensive_variable_references(classified_expressions, stancode)
  
  # Find supporting declarations using existing utility
  # Only search model and transformed parameters blocks to avoid data/parameter contamination
  supporting_declarations <- find_variable_declarations(
    stancode = stancode, 
    referenced_vars = referenced_vars,
    search_blocks = c("transformed parameters", "model")
  )
  
  # Store classification metadata for downstream use
  attr(mu_lines, "classification") <- classified_expressions
  attr(mu_lines, "execution_plan") <- build_execution_dependency_plan(classified_expressions)
  
  return(list(
    mu_construction = mu_lines,
    supporting_declarations = supporting_declarations,
    referenced_variables = referenced_vars
  ))
}

#' Create empty result structure for compatibility
#' @return Empty list structure matching expected interface
#' @noRd
create_empty_mu_result <- function() {
  list(
    mu_construction = character(0),
    supporting_declarations = character(0),
    referenced_variables = character(0)
  )
}

#' Extract mu assignment lines from model block
#' @param model_block Character string containing model block content
#' @return Character vector of mu assignment lines
#' @noRd
extract_mu_assignment_lines <- function(model_block) {
  checkmate::assert_string(model_block)
  
  # Split into lines and clean
  lines <- strsplit(model_block, "\n")[[1]]
  lines <- trimws(lines)
  lines <- lines[nchar(lines) > 0]
  
  # Find lines that modify mu (assignment or addition)
  mu_pattern <- "\\bmu\\s*(\\[|\\+|=)"
  mu_lines <- lines[grepl(mu_pattern, lines, perl = TRUE)]
  
  # Filter out likelihood calls (normal_lpdf, etc.)
  likelihood_pattern <- "_lpdf\\s*\\("
  mu_lines <- mu_lines[!grepl(likelihood_pattern, mu_lines)]
  
  return(mu_lines)
}

#' Classify mu expressions by structural features  
#' @param mu_lines Character vector of mu assignment lines
#' @param stancode Full Stan code for context
#' @return List of classified expressions
#' @noRd
classify_mu_expressions_structurally <- function(mu_lines, stancode) {
  checkmate::assert_character(mu_lines)
  checkmate::assert_string(stancode)
  
  # Create analysis context
  context <- create_analysis_context(stancode)
  
  # Classify each expression
  classifications <- lapply(mu_lines, function(line) {
    classify_single_mu_expression_structurally(line, context)
  })
  
  names(classifications) <- mu_lines
  return(classifications)
}

#' Create analysis context from Stan code
#' @param stancode Full Stan code
#' @return List with context information
#' @noRd
create_analysis_context <- function(stancode) {
  checkmate::assert_string(stancode)
  
  # Use existing GLM detection
  glm_result <- detect_glm_usage(stancode)
  
  # Discover functions from functions block
  functions_block <- extract_stan_block_content(stancode, "functions")
  declared_functions <- extract_declared_functions(functions_block)
  
  # Get reserved Stan words using existing utility
  reserved_words <- get_stan_reserved_words()
  
  list(
    has_glm = length(glm_result) > 0,
    glm_variables = if (length(glm_result) > 0) c("b", "Xc", "means_X") else character(0),
    declared_functions = declared_functions,
    reserved_words = reserved_words,
    all_functions = unique(c(declared_functions, reserved_words))
  )
}

#' Extract declared functions from functions block
#' @param functions_block Character string of functions block content
#' @return Character vector of declared function names
#' @noRd
extract_declared_functions <- function(functions_block) {
  if (is.null(functions_block) || nchar(trimws(functions_block)) == 0) {
    return(character(0))
  }
  
  # Find function declarations: return_type function_name(
  func_pattern <- "\\b[a-zA-Z_][a-zA-Z0-9_]*\\s+([a-zA-Z_][a-zA-Z0-9_]*)\\s*\\("
  matches <- gregexpr(func_pattern, functions_block, perl = TRUE)
  
  if (length(matches[[1]]) == 1 && matches[[1]] == -1) {
    return(character(0))
  }
  
  # Extract function names (second capture group)
  func_matches <- regmatches(functions_block, matches)[[1]]
  func_names <- character(0)
  
  for (match in func_matches) {
    # Extract function name after return type
    parts <- strsplit(trimws(gsub("\\(.*", "", match)), "\\s+")[[1]]
    if (length(parts) >= 2) {
      func_names <- c(func_names, parts[length(parts)])
    }
  }
  
  return(unique(func_names))
}

#' Classify single mu expression by structural features
#' @param expr Character string of single mu expression
#' @param context Analysis context list
#' @return List with classification results
#' @noRd  
classify_single_mu_expression_structurally <- function(expr, context) {
  checkmate::assert_string(expr)
  checkmate::assert_list(context)
  
  # Normalize expression
  expr_clean <- normalize_mu_expression(expr)
  
  # Analyze structural features
  features <- analyze_expression_structure(expr_clean, context)
  
  # Determine expression type
  expr_type <- determine_expression_type(features)
  
  # Determine execution order
  execution_order <- determine_execution_order_from_type(expr_type, features)
  
  # Check loop requirement
  requires_loop <- check_loop_requirement(features, expr_type)
  
  # Extract variables
  variables <- extract_expression_variables_comprehensive(expr_clean, features, context)
  
  list(
    original = expr,
    normalized = expr_clean,
    type = expr_type,
    execution_order = execution_order,
    requires_loop = requires_loop,
    variables = variables,
    structural_features = features
  )
}

#' Normalize mu expression for analysis
#' @param expr Raw expression string
#' @return Cleaned and normalized expression
#' @noRd
normalize_mu_expression <- function(expr) {
  checkmate::assert_string(expr)
  
  # Remove leading/trailing whitespace
  expr_clean <- trimws(expr)
  
  # Normalize internal whitespace
  expr_clean <- gsub("\\s+", " ", expr_clean)
  
  # Remove trailing semicolon if present
  expr_clean <- gsub(";\\s*$", "", expr_clean)
  
  return(expr_clean)
}

#' Analyze structural features of expression
#' @param expr Normalized expression string  
#' @param context Analysis context
#' @return List of structural features
#' @noRd
analyze_expression_structure <- function(expr, context) {
  checkmate::assert_string(expr)
  checkmate::assert_list(context)
  
  list(
    # Assignment analysis
    assignment_type = analyze_assignment_type(expr),
    target_variable = extract_assignment_target(expr),
    
    # Indexing analysis
    indexing_patterns = analyze_indexing_patterns_comprehensive(expr),
    
    # Function analysis
    function_usage = analyze_function_usage(expr, context),
    
    # Mathematical operations
    operations = analyze_mathematical_operations_comprehensive(expr),
    
    # Variable relationships
    variable_relationships = analyze_variable_relationships_comprehensive(expr)
  )
}

#' Analyze assignment type in expression
#' @param expr Expression string
#' @return Character string indicating assignment type
#' @noRd
analyze_assignment_type <- function(expr) {
  checkmate::assert_string(expr)
  
  # Check for addition assignment first
  if (grepl("\\+=", expr)) {
    return("addition_assignment")
  }
  
  # Check for simple assignment (but not comparison ==)
  if (grepl("\\s*=\\s*", expr) && !grepl("==", expr)) {
    return("new_assignment")
  }
  
  return("unknown")
}

#' Extract assignment target variable
#' @param expr Expression string
#' @return Character string of target variable or empty
#' @noRd
extract_assignment_target <- function(expr) {
  checkmate::assert_string(expr)
  
  # Handle simple assignments (=)
  if (grepl("\\s*=", expr) && !grepl("\\+=|==", expr)) {
    # Split on = and get the left side
    left_side <- strsplit(expr, "\\s*=")[[1]][1]
    
    # Extract the last word (variable name) from the left side
    words <- strsplit(trimws(left_side), "\\s+")[[1]]
    if (length(words) > 0) {
      return(words[length(words)])
    }
  }
  
  # Handle addition assignments (+=)
  if (grepl("\\+=", expr)) {
    # Split on += and get the left side
    left_side <- strsplit(expr, "\\s*\\+=")[[1]][1]
    left_side <- trimws(left_side)
    
    # Extract variable name, handling indexed variables like mu[index]
    if (grepl("\\[", left_side)) {
      # For indexed variables like "mu[Igp_1_1]", extract part before [
      var_name <- gsub("\\[.*$", "", left_side)
      return(trimws(var_name))
    } else {
      # For simple variables, extract the last word (handles type declarations)
      words <- strsplit(left_side, "\\s+")[[1]]
      if (length(words) > 0) {
        return(words[length(words)])
      }
    }
  }
  
  return("")
}

#' Analyze indexing patterns comprehensively
#' @param expr Expression string
#' @return List of indexing pattern features
#' @noRd
analyze_indexing_patterns_comprehensive <- function(expr) {
  checkmate::assert_string(expr)
  
  list(
    has_loop_index = grepl("\\[\\s*n\\s*\\]", expr, perl = TRUE),
    has_subset_index = grepl("\\[I[a-zA-Z0-9_]+\\]", expr, perl = TRUE),
    has_group_index = grepl("\\[J[a-zA-Z0-9_]+\\]", expr, perl = TRUE),
    has_array_access = grepl("\\[[A-Z][a-zA-Z0-9_]*\\[", expr, perl = TRUE),
    has_multiple_indices = length(gregexpr("\\[", expr)[[1]]) > 1,
    bracket_nesting_depth = calculate_bracket_nesting_depth(expr)
  )
}

#' Calculate bracket nesting depth
#' @param expr Expression string
#' @return Integer nesting depth
#' @noRd
calculate_bracket_nesting_depth <- function(expr) {
  checkmate::assert_string(expr)
  
  chars <- strsplit(expr, "")[[1]]
  max_depth <- 0L
  current_depth <- 0L
  
  for (char in chars) {
    if (char == "[") {
      current_depth <- current_depth + 1L
      max_depth <- max(max_depth, current_depth)
    } else if (char == "]") {
      current_depth <- current_depth - 1L
    }
  }
  
  return(max_depth)
}

#' Analyze function usage in expression
#' @param expr Expression string
#' @param context Analysis context with function information
#' @return List of function usage features
#' @noRd
analyze_function_usage <- function(expr, context) {
  checkmate::assert_string(expr)
  checkmate::assert_list(context)
  
  # Extract potential function calls (word followed by parenthesis)
  func_pattern <- "\\b([a-zA-Z_][a-zA-Z0-9_]*)\\s*\\("
  matches <- gregexpr(func_pattern, expr, perl = TRUE)
  
  potential_functions <- character(0)
  if (length(matches[[1]]) > 0 && matches[[1]][1] != -1) {
    func_matches <- regmatches(expr, matches)[[1]]
    potential_functions <- gsub("\\s*\\(", "", func_matches)
  }
  
  # Classify functions using context
  known_functions <- intersect(potential_functions, context$all_functions)
  unknown_functions <- setdiff(potential_functions, context$all_functions)
  
  list(
    has_function_calls = length(potential_functions) > 0,
    potential_functions = potential_functions,
    known_functions = known_functions,
    unknown_functions = unknown_functions,
    function_count = length(potential_functions)
  )
}

#' Analyze mathematical operations comprehensively
#' @param expr Expression string
#' @return List of mathematical operation features
#' @noRd
analyze_mathematical_operations_comprehensive <- function(expr) {
  checkmate::assert_string(expr)
  
  list(
    has_matrix_multiplication = grepl("\\*", expr) && !grepl("\\*=", expr),
    has_element_wise_multiplication = grepl("\\.\\*", expr),
    has_addition = grepl("\\+", expr) && !grepl("\\+=", expr),
    has_dot_product = grepl("\\bdot_product\\b", expr),
    has_matrix_operations = grepl("\\b(transpose|inverse|cholesky_decompose)\\b", expr),
    operation_complexity = count_mathematical_operators(expr)
  )
}

#' Count mathematical operators in expression
#' @param expr Expression string
#' @return Integer count of mathematical operators
#' @noRd
count_mathematical_operators <- function(expr) {
  checkmate::assert_string(expr)
  
  operators <- c("\\+", "\\-", "\\*", "\\/", "\\.\\*", "\\.\\+")
  total_count <- 0L
  
  for (op in operators) {
    matches <- gregexpr(op, expr, perl = TRUE)
    if (length(matches[[1]]) > 0 && matches[[1]][1] != -1) {
      total_count <- total_count + length(matches[[1]])
    }
  }
  
  return(total_count)
}

#' Analyze variable relationships comprehensively
#' @param expr Expression string
#' @return List of variable relationship features
#' @noRd
analyze_variable_relationships_comprehensive <- function(expr) {
  checkmate::assert_string(expr)
  
  # Extract all identifiers
  identifiers <- extract_all_identifiers(expr)
  
  list(
    total_variables = length(identifiers),
    unique_variables = length(unique(identifiers)),
    has_indexed_variables = any(grepl("\\[", identifiers)),
    has_computed_variables = grepl("=", expr) && !grepl("\\+=", expr),
    variable_complexity = calculate_variable_complexity(identifiers)
  )
}

#' Extract all identifiers from expression
#' @param expr Expression string
#' @return Character vector of identifiers
#' @noRd
extract_all_identifiers <- function(expr) {
  checkmate::assert_string(expr)
  
  # Extract all valid Stan identifiers (simpler pattern without negative lookahead)
  id_pattern <- "\\b[a-zA-Z_][a-zA-Z0-9_]*"
  matches <- gregexpr(id_pattern, expr, perl = TRUE)
  
  if (length(matches[[1]]) == 1 && matches[[1]] == -1) {
    return(character(0))
  }
  
  all_identifiers <- regmatches(expr, matches)[[1]]
  
  # Remove identifiers that are followed by parentheses (functions)
  # Use a separate check for functions
  functions_pattern <- "\\b[a-zA-Z_][a-zA-Z0-9_]*\\s*\\("
  function_matches <- gregexpr(functions_pattern, expr, perl = TRUE)
  
  if (length(function_matches[[1]]) > 1 || function_matches[[1]][1] != -1) {
    function_names <- regmatches(expr, function_matches)[[1]]
    # Extract just the function names (remove the parenthesis part)
    function_names <- gsub("\\s*\\($", "", function_names)
    # Remove functions from the identifier list
    all_identifiers <- setdiff(all_identifiers, function_names)
  }
  
  return(all_identifiers)
}

#' Calculate variable complexity score
#' @param identifiers Character vector of identifiers
#' @return Integer complexity score
#' @noRd
calculate_variable_complexity <- function(identifiers) {
  checkmate::assert_character(identifiers)
  
  if (length(identifiers) == 0) return(0L)
  
  # Base complexity is number of unique variables
  base_complexity <- length(unique(identifiers))
  
  # Add complexity for underscores (compound variables)
  underscore_complexity <- sum(grepl("_", identifiers))
  
  # Add complexity for numeric suffixes (indexed variables)  
  numeric_complexity <- sum(grepl("_\\d+", identifiers))
  
  return(base_complexity + underscore_complexity + numeric_complexity)
}

#' Determine expression type from structural features
#' @param features List of structural features
#' @return Character string indicating expression type
#' @noRd
determine_expression_type <- function(features) {
  checkmate::assert_list(features)
  
  # Computed variable assignment (Stage 0)
  if (features$assignment_type == "new_assignment") {
    if (features$function_usage$has_function_calls) {
      return("computed_variable")
    } else {
      return("simple_assignment")
    }
  }
  
  # mu modification patterns (Stages 1-3)
  if (features$assignment_type == "addition_assignment" && 
      features$target_variable == "mu") {
    
    # Loop patterns (Stage 3)
    if (features$indexing_patterns$has_loop_index) {
      return("loop_assignment")
    }
    
    # Indexed subset patterns (Stage 2)
    if (features$indexing_patterns$has_subset_index || 
        features$indexing_patterns$has_group_index) {
      return("indexed_assignment")
    }
    
    # Simple vectorized patterns (Stage 1)
    return("vectorized_assignment")
  }
  
  return("unknown")
}

#' Determine execution order from expression type
#' @param expr_type Expression type string
#' @param features Structural features for additional context
#' @return Integer execution order (0-3)
#' @noRd
determine_execution_order_from_type <- function(expr_type, features) {
  checkmate::assert_string(expr_type)
  checkmate::assert_list(features)
  
  execution_orders <- list(
    "computed_variable" = 0L,
    "simple_assignment" = 0L,
    "vectorized_assignment" = 1L,
    "indexed_assignment" = 2L,
    "loop_assignment" = 3L
  )
  
  order <- execution_orders[[expr_type]]
  if (is.null(order)) {
    return(1L)  # Default to Stage 1
  }
  
  return(order)
}

#' Check if expression requires loop structure
#' @param features Structural features
#' @param expr_type Expression type
#' @return Logical indicating loop requirement
#' @noRd
check_loop_requirement <- function(features, expr_type) {
  checkmate::assert_list(features)
  checkmate::assert_string(expr_type)
  
  # Loop required for loop index patterns or loop assignment type
  return(features$indexing_patterns$has_loop_index || expr_type == "loop_assignment")
}

#' Extract variables comprehensively from expression
#' @param expr Expression string
#' @param features Structural features
#' @param context Analysis context
#' @return List of classified variables
#' @noRd
extract_expression_variables_comprehensive <- function(expr, features, context) {
  checkmate::assert_string(expr)
  checkmate::assert_list(features)
  checkmate::assert_list(context)
  
  # Get all identifiers
  all_identifiers <- extract_all_identifiers(expr)
  
  # Classify variables
  functions <- intersect(all_identifiers, context$all_functions)
  
  # Common index variables
  index_variables <- c("n", "i", "j", "k", "m", "d", "t", "s")
  indices <- intersect(all_identifiers, index_variables)
  
  # Parameters are non-function, non-index variables
  parameters <- setdiff(all_identifiers, c(functions, indices))
  
  # Remove assignment target from parameters if it's a computed variable
  if (features$assignment_type == "new_assignment" && 
      nchar(features$target_variable) > 0) {
    parameters <- setdiff(parameters, features$target_variable)
  }
  
  list(
    functions = functions,
    indices = indices,
    parameters = parameters,
    all_identifiers = all_identifiers
  )
}

#' Extract comprehensive variable references from all classified expressions
#' @param classified_expressions List of classified expressions
#' @param stancode Full Stan code for context
#' @return Character vector of all referenced variables needing mapping
#' @noRd
extract_comprehensive_variable_references <- function(classified_expressions, stancode) {
  checkmate::assert_list(classified_expressions)
  checkmate::assert_string(stancode)
  
  # Collect all parameters from all expressions
  all_parameters <- character(0)
  
  for (classification in classified_expressions) {
    all_parameters <- c(all_parameters, classification$variables$parameters)
  }
  
  # Remove duplicates and return unique variables
  unique_parameters <- unique(all_parameters)
  
  # Filter out any remaining reserved words or common indices
  context <- create_analysis_context(stancode)
  filtered_parameters <- setdiff(unique_parameters, context$all_functions)
  filtered_parameters <- setdiff(filtered_parameters, c("n", "i", "j", "k", "m", "d", "t", "s"))
  
  return(filtered_parameters)
}

#' Build execution dependency plan from classified expressions
#' @param classified_expressions List of classified expressions
#' @return List with execution plan organized by stages
#' @noRd
build_execution_dependency_plan <- function(classified_expressions) {
  checkmate::assert_list(classified_expressions)
  
  # Group expressions by execution order
  execution_stages <- list()
  
  for (i in seq_along(classified_expressions)) {
    classification <- classified_expressions[[i]]
    order <- as.character(classification$execution_order)
    
    if (is.null(execution_stages[[order]])) {
      execution_stages[[order]] <- list()
    }
    
    execution_stages[[order]] <- c(execution_stages[[order]], list(classification))
  }
  
  # Sort stages by execution order
  stage_names <- sort(as.integer(names(execution_stages)))
  sorted_stages <- execution_stages[as.character(stage_names)]
  names(sorted_stages) <- paste0("stage_", stage_names)
  
  list(
    execution_stages = sorted_stages,
    total_stages = length(sorted_stages),
    stage_order = stage_names
  )
}