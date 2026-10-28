#' GLM likelihood detection and trend injection
#'
#' @description
#' brms folds the linear predictor into its GLM likelihood calls. The
#' functions here find those calls and decide how the trend joins each
#' one.
#'
#' @name glm_analysis
#' @noRd
NULL


# The role of every argument after the `|` of each brms GLM likelihood,
# in the order brms writes them. The layouts differ: `normal_id_glm`
# and friends take `(x, alpha, beta)`, while `ordered_logistic_glm`
# takes `(x, beta, cutpoints)` and carries no intercept at all.
# A parser taking positions from one shared layout would multiply the
# ordinal design matrix by the cutpoints.
#
# `brms::categorical()` has no entry. mvgam refuses it in favour of
# `categ()`. Its per-category predictor holds no single `mu` for a
# trend to join. `glm_family_of_line()` refuses a
# `categorical_logit_glm` line as an unrecognised family.
glm_call_roles <- list(
  normal_id_glm = c("design_matrix", "intercept", "coefficients", "sigma"),
  poisson_log_glm = c("design_matrix", "intercept", "coefficients"),
  neg_binomial_2_log_glm = c(
    "design_matrix", "intercept", "coefficients", "shape"
  ),
  bernoulli_logit_glm = c("design_matrix", "intercept", "coefficients"),
  ordered_logistic_glm = c("design_matrix", "coefficients", "cutpoints")
)

# The GLM likelihoods mvgam can add a trend to, as the roles above
# list them
mvgam_glm_families <- names(glm_call_roles)

#' The argument roles of one GLM family
#'
#' @param glm_type Family name including the `_glm` suffix, e.g.
#'   `"ordered_logistic_glm"`.
#' @return The `glm_call_roles` entry for that family.
#' @noRd
glm_roles_for <- function(glm_type) {
  checkmate::assert_string(glm_type, min.chars = 1)
  roles <- glm_call_roles[[glm_type]]
  if (is.null(roles)) {
    stop_mvgam_fault(
      paste0("The GLM likelihood '", glm_type,
             "' is missing from `glm_call_roles`."),
      paste0("Known GLM likelihoods: ",
             paste(mvgam_glm_families, collapse = ", "), ".")
    )
  }
  roles
}

#' The GLM family one likelihood line calls
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

#' Regex matching one GLM density call through its closing bracket
#'
#' The match ends at the first `)`. brms writes plain names as GLM
#' arguments. mvgam parses only the program brms wrote, before it
#' places any trend term in a call.
#'
#' @param glm_type Family name including the `_glm` suffix.
#' @return A regex string.
#' @noRd
glm_call_pattern <- function(glm_type) {
  paste0(stan_density_call_pattern(glm_type), "\\([^)]+\\)")
}

#' The arguments of one GLM likelihood call, named by role
#'
#' @param glm_line Character string containing the GLM call.
#' @param glm_type Family name including the `_glm` suffix, e.g.
#'   `"poisson_log_glm"`.
#' @return List naming each argument by the role `glm_call_roles`
#'   gives it, plus the response as `y_var`.
#' @noRd
parse_glm_parameters_from_line <- function(glm_line, glm_type) {
  checkmate::assert_string(glm_line)
  checkmate::assert_string(glm_type)
  roles <- glm_roles_for(glm_type)

  # The match starts at the density name. The argument list then comes
  # from the GLM call itself, whatever parenthesis precedes it on the
  # line.
  call_match <- regexpr(glm_call_pattern(glm_type), glm_line)
  if (call_match < 0) {
    stop_mvgam_fault(
      paste0("mvgam failed to parse the arguments of '", glm_type, "'."),
      paste0("The line was: ", trimws(glm_line))
    )
  }
  call_content <- regmatches(glm_line, call_match)
  call_content <- sub("\\)$", "", sub("^[^(]*\\(", "", call_content))

  parts <- strsplit(call_content, "|", fixed = TRUE)[[1L]]
  params <- trimws(strsplit(parts[2L], ",", fixed = TRUE)[[1L]])
  if (length(parts) != 2L || length(params) != length(roles)) {
    stop_mvgam_fault(
      paste0("The '", glm_type, "' call must take the arguments ",
             paste(roles, collapse = ", "), "."),
      paste0("The line was: ", trimws(glm_line))
    )
  }
  c(list(y_var = trimws(parts[1L])), stats::setNames(as.list(params), roles))
}


#' Rewrite the arguments of one GLM likelihood call
#'
#' The density keeps the suffix brms wrote. A program generated under
#' `normalize = FALSE` keeps its unnormalised density.
#'
#' @param glm_line Character string holding the GLM call.
#' @param glm_type Family name including the `_glm` suffix.
#' @param y_var The response the call scores.
#' @param arguments The arguments after the `|`, in order.
#' @param glm `FALSE` to call the family's density without the GLM form,
#'   as in `ordered_logistic_lpmf` for `ordered_logistic_glm_lpmf`.
#' @return `glm_line` with the call rewritten.
#' @noRd
rewrite_glm_call <- function(glm_line, glm_type, y_var, arguments,
                             glm = TRUE) {
  checkmate::assert_string(glm_line)
  checkmate::assert_string(y_var, min.chars = 1)
  checkmate::assert_character(arguments, min.len = 1L, any.missing = FALSE)
  checkmate::assert_flag(glm)
  call_match <- regexpr(glm_call_pattern(glm_type), glm_line)
  if (call_match < 0) {
    stop_mvgam_fault(
      paste0("No '", glm_type, "' call found in the line to rewrite."),
      paste0("The line was: ", trimws(glm_line))
    )
  }
  density <- sub("\\(.*$", "", regmatches(glm_line, call_match))
  if (!glm) {
    density <- sub("_glm_", "_", density, fixed = TRUE)
  }
  regmatches(glm_line, call_match) <- paste0(
    density, "(", y_var, " | ", paste(arguments, collapse = ", "), ")"
  )
  glm_line
}

#' The name of one response's linear predictor
#'
#' @param resp_name The response key, `""` for a univariate model.
#' @return `"mu"`, suffixed by the key in a multivariate model.
#' @noRd
response_mu <- function(resp_name) {
  checkmate::assert_string(resp_name)
  paste0("mu", predictor_suffix(resp_name))
}

#' The GLM likelihood of one response with the route its trend takes
#'
#' brms folds the linear predictor into a GLM likelihood where it can,
#' as in `poisson_log_glm_lpmf(Y | Xc, Intercept, b)`. That call names
#' no `mu` for a trend to reach. Three routes follow:
#' * `"declared"`: brms declared `mu` and passes it as the intercept,
#'   which a term the GLM arguments have no slot for causes, an offset
#'   among them. The trend is added to that `mu` and the call stays as
#'   brms wrote it.
#' * `"alpha"`: the family's GLM takes an intercept, which Stan
#'   accepts as a vector. The trend joins the intercept inside the
#'   call. The design matrix and coefficients stay in the GLM.
#' * `"density"`: the family's GLM takes no intercept, as for the
#'   ordinal families. The program builds `mu` with the trend. The
#'   call becomes the family's vectorised density on `mu`, which keeps
#'   the cutpoints.
#'
#' A family outside `mvgam_glm_families` is refused. A call left
#' untouched would leave the trend computed and never added to the
#' predictor.
#'
#' @param code_lines Character vector of Stan source lines.
#' @param resp The response key, `""` for a univariate model.
#' @return NULL when brms wrote the response's likelihood without a GLM
#'   call. Otherwise a list of the line index `at`, the `glm_type`, the
#'   parsed `params` and the `route`.
#' @noRd
response_glm_call <- function(code_lines, resp) {
  checkmate::assert_character(code_lines, min.len = 1)
  checkmate::assert_string(resp)
  model <- find_stan_block(code_lines, "model")
  if (is.null(model)) {
    return(NULL)
  }
  span <- seq.int(model$start_idx, model$end_idx)
  pattern <- paste0(stan_density_call_pattern("_glm"), "\\(Y",
                    predictor_suffix(resp), " \\|")
  at <- span[grepl(pattern, code_lines[span])]
  if (!length(at)) {
    return(NULL)
  }
  if (length(at) > 1L) {
    stop_mvgam_fault(
      paste0("The model block holds ", length(at),
             " GLM likelihoods for one response."),
      "brms writes one likelihood statement per response."
    )
  }
  glm_type <- glm_family_of_line(code_lines[at])
  params <- parse_glm_parameters_from_line(code_lines[at], glm_type)
  route <- if (identical(params$intercept, response_mu(resp))) {
    "declared"
  } else if ("intercept" %in% glm_roles_for(glm_type)) {
    "alpha"
  } else {
    "density"
  }
  list(at = at, glm_type = glm_type, params = params, route = route)
}
