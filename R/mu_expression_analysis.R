# The statements a brms program writes for `mu`. The trend extraction
# rebuilds them as `mu_trend`, with the variables they use.

#' The statements a brms program writes for `mu`
#'
#' The model block's statements on `mu`, then the fixed effects a
#' gaussian GLM likelihood takes as arguments. The variables those
#' statements use come with their declarations in transformed
#' parameters or the model block, preceded by the declarations of
#' everything they depend on.
#'
#' @param stancode A brms program
#' @return List of `mu_construction`, the statements,
#'   `supporting_declarations` and `referenced_variables`
#' @noRd
extract_mu_construction <- function(stancode) {
  checkmate::assert_string(stancode, min.chars = 1L)
  model_block <- extract_stan_block_content(stancode, "model")
  mu_lines <- if (!is.null(model_block)) {
    extract_mu_assignment_lines(model_block)
  }
  if (length(mu_lines) == 0L) {
    return(list(mu_construction = character(0),
                supporting_declarations = character(0),
                referenced_variables = character(0)))
  }
  mu_lines <- add_glm_hidden_fixed_effects(mu_lines, stancode)
  referenced <- mu_referenced_variables(mu_lines, stancode)
  list(
    mu_construction = mu_lines,
    supporting_declarations = find_variable_declarations(
      stancode, referenced,
      search_blocks = c("transformed parameters", "model")
    ),
    referenced_variables = referenced
  )
}

#' The lines of a model block that assign or add to `mu`
#'
#' @param model_block Body of a model block
#' @return Character vector of trimmed lines, likelihood calls excluded
#' @noRd
extract_mu_assignment_lines <- function(model_block) {
  checkmate::assert_string(model_block)
  lines <- trimws(strsplit(model_block, "\n")[[1L]])
  lines <- lines[nzchar(lines)]
  mu_lines <- lines[grepl("\\bmu\\s*(\\[|\\+|=)", lines, perl = TRUE)]
  mu_lines[!grepl("_lpdf\\s*\\(", mu_lines)]
}

#' The fixed effects a gaussian GLM likelihood takes as arguments
#'
#' brms passes the design matrix and coefficients of a gaussian model
#' to `normal_id_glm_lpdf()`. No statement adds them to `mu`. The
#' trend program is gaussian. Its `mu_trend` takes them as the
#' statement `mu += Xc * b;`. A program that writes statements on `mu`
#' passes `mu` itself as the GLM intercept, which needs no statement.
#'
#' @param mu_lines Statements on `mu` from the model block
#' @param stancode The brms program
#' @return `mu_lines`, extended
#' @noRd
add_glm_hidden_fixed_effects <- function(mu_lines, stancode) {
  checkmate::assert_character(mu_lines)
  checkmate::assert_string(stancode)
  glm_call <- response_glm_call(
    strsplit(stancode, "\n", fixed = TRUE)[[1L]], ""
  )
  if (is.null(glm_call)) {
    return(mu_lines)
  }
  if (!identical(glm_call$route, "declared")) {
    stop_mvgam_fault(
      "The trend program's GLM call must take its declared 'mu'.",
      "brms passes 'mu' as the GLM intercept once it writes statements on it."
    )
  }
  c(mu_lines, paste0("mu += ", glm_call$params$design_matrix, " * ",
                     glm_call$params$coefficients, ";"))
}

#' The variables the statements on `mu` use
#'
#' Every identifier of each statement, less the functions it calls,
#' Stan's reserved words, the functions the program defines and the
#' loop indices brms writes. A declaration with a value leaves out the
#' variable it declares.
#'
#' @param mu_lines Statements on `mu`
#' @param stancode The brms program
#' @return Unique identifiers, in order of first appearance
#' @noRd
mu_referenced_variables <- function(mu_lines, stancode) {
  checkmate::assert_character(mu_lines, any.missing = FALSE)
  checkmate::assert_string(stancode)
  functions_block <- extract_stan_block_content(stancode, "functions")
  defined <- if (!is.null(functions_block)) {
    vapply(parse_stan_functions(functions_block), function(f) f$name,
           character(1L))
  }
  excluded <- c(get_stan_reserved_words(), defined,
                "n", "i", "j", "k", "m", "d", "t", "s")
  unique(unlist(lapply(mu_lines, function(line) {
    ids <- setdiff(extract_stan_identifiers(line), stan_function_calls(line))
    if (grepl("=", line) && !grepl("\\+=|==", line)) {
      left <- strsplit(trimws(sub("\\s*=.*$", "", line)), "\\s+")[[1L]]
      ids <- setdiff(ids, left[length(left)])
    }
    setdiff(ids, excluded)
  })))
}
