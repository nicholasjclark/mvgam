#' GLM likelihood detection and trend injection
#'
#' @description
#' brms folds the linear predictor into its GLM likelihood calls. The
#' functions here find those calls and rewrite each one to take a named
#' predictor that a trend can be added to.
#'
#' @name glm_analysis
NULL


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
# `mu` to unwind into. A `categorical_logit_glm` line is refused as
# an unrecognised family, at every site that detects a GLM call.
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
    stop_mvgam_fault(
      paste0("mvgam has no layout for the GLM likelihood '", glm_type,
             "'."),
      paste0("Known GLM likelihoods: ", paste(mvgam_glm_families, collapse = ", "), ".")
    )
  }
  layout
}

#' The GLM family one likelihood line calls
#'
#' Refuses a GLM call whose family has no layout. Returning the line
#' untouched would leave the trend computed but never added to the
#' linear predictor: a model with no trend that compiles and samples
#' without complaint. The refusal catches a family added to brms's
#' GLM set without a transformation at code generation, before any
#' result exists.
#'
#' @param line One line of Stan source holding a `_glm` density call.
#' @return The family name including the `_glm` suffix.
#' @noRd
glm_family_of_line <- function(line) {
  checkmate::assert_string(line)
  hit <- vapply(mvgam_glm_families, function(fam) {
    grepl(stan_density_call_pattern(fam), line)
  }, logical(1L))
  if (!any(hit)) {
    stop_mvgam_fault(
      "Found a GLM likelihood whose family could not be identified.",
      paste0("The call was: ", trimws(line))
    )
  }
  mvgam_glm_families[hit][1L]
}

#' Which GLM likelihoods a Stan program calls
#'
#' Detection takes the `_glm` suffix every brms GLM density carries,
#' and `glm_family_of_line()` names the family of each hit. Detecting
#' with the closed list of layouts instead would report a `_glm` call
#' with no layout as absent, and the trend would be computed and
#' never added to the linear predictor.
#'
#' @param stan_code Character vector of Stan source.
#' @return Named logical over `mvgam_glm_families`.
#' @noRd
glm_calls_present <- function(stan_code) {
  checkmate::assert_character(stan_code, min.len = 1)
  lines <- unlist(strsplit(stan_code, "\n", fixed = TRUE))
  hits <- grep(
    paste0("target\\s*\\+=.*", stan_density_call_pattern("_glm")),
    lines, value = TRUE
  )
  found <- vapply(hits, glm_family_of_line, character(1),
                  USE.NAMES = FALSE)
  stats::setNames(mvgam_glm_families %in% found, mvgam_glm_families)
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
    stop_mvgam_fault(
      paste0("mvgam failed to parse the arguments of '", glm_type, "'."),
      paste0("The line was: ", trimws(glm_line))
    )
  }

  call_content <- regmatches(glm_line, call_match)
  call_content <- sub("^[^(]*\\(", "", call_content)
  call_content <- sub("\\)$", "", call_content)

  # Split by | to get Y and parameters
  parts <- strsplit(call_content, "\\|")[[1]]
  if (length(parts) < 2) {
    stop_mvgam_fault(
      paste0("The '", glm_type, "' call has no response separator."),
      paste0("The line was: ", trimws(glm_line))
    )
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
    stop_mvgam_fault(
      paste0("No '", glm_type, "' call found in the line to transform."),
      paste0("The line was: ", trimws(glm_line))
    )
  }

  arguments <- layout$rewrite(
    glm_params, paste0("to_matrix(", mu_var, ")"), mu_ones_var
  )
  paste0(
    glm_type, suffix, "(", glm_params$y_var, " | ",
    paste(arguments, collapse = ", "), ")"
  )
}

#' The linear predictor a GLM call is rewritten to read
#'
#' A univariate model names its predictor `mu` and the `vector[1]` of
#' ones standing in for its coefficients `mu_ones`; a multivariate
#' model names one of each per response, suffixed by the response's
#' key.
#'
#' @param resp_name The response key, `""` for a univariate model.
#' @return Named character vector with elements `mu` and `ones`.
#' @noRd
glm_mu_names <- function(resp_name) {
  checkmate::assert_string(resp_name)
  suffix <- if (nzchar(resp_name)) paste0("_", resp_name) else ""
  c(mu = paste0("mu", suffix), ones = paste0("mu_ones", suffix))
}

#' Whether a response's GLM call takes the predictor brms declared
#'
#' brms declares `mu` and passes it as the GLM intercept when the
#' predictor holds a term the GLM arguments have no slot for, an
#' offset among them. The design matrix and coefficients stay in the
#' call. mvgam then adds the trend to that vector and keeps the call
#' brms wrote. A call rewritten to take `to_matrix(mu)` would drop
#' `X * b`.
#'
#' @param stan_code Character string of the observation program.
#' @param resp_name The response key, `""` for a univariate model.
#' @return `TRUE` when every GLM call on the response passes its
#'   declared `mu` as the intercept.
#' @noRd
glm_takes_declared_mu <- function(stan_code, resp_name) {
  checkmate::assert_string(stan_code)
  checkmate::assert_string(resp_name)
  y_var <- if (nzchar(resp_name)) paste0("Y_", resp_name) else "Y"
  code_lines <- strsplit(stan_code, "\n", fixed = TRUE)[[1]]
  glm_lines <- grep(
    paste0(stan_density_call_pattern("_glm"), "\\(", y_var, " \\|"),
    code_lines, value = TRUE
  )
  if (!length(glm_lines)) {
    return(FALSE)
  }
  mu_var <- glm_mu_names(resp_name)[["mu"]]
  all(vapply(glm_lines, function(line) {
    params <- parse_glm_parameters_from_line(line, glm_family_of_line(line))
    identical(params$intercept, mu_var)
  }, logical(1L)))
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
  names <- glm_mu_names(params$response_name)
  replacement <- build_glm_call_on_mu(
    glm_line, glm_type, params, names[["mu"]], names[["ones"]]
  )

  original_pattern <- paste0(stan_density_call_pattern(glm_type),
                             "\\s*\\([^\\)]+\\)")
  gsub(original_pattern, replacement, glm_line)
}
