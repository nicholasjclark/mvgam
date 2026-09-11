#' A response value a family's support accepts
#'
#' brms builds a model's Stan data by evaluating the whole formula,
#' response included, and a frame that only needs predictions still
#' needs a response brms will accept. This picks a point inside the
#' family's support: the midpoint of a bounded support, stepped in from
#' any open bound, and otherwise a value just inside the finite bound,
#' rounded for a count. Nothing downstream reads it.
#'
#' @param family_obj A `brmsfamily`, whose `ybounds`, `closed` and
#'   `type` describe the support
#' @return A single value inside the support; an integer for a count
#' @noRd
get_safe_dummy_value <- function(family_obj) {
  checkmate::assert_class(family_obj, "brmsfamily")

  # brms records the support as `ybounds` plus a `closed` flag per
  # bound, and both bounds matter: a family bounded above rejects a
  # dummy at or beyond its upper limit as readily as one below its
  # lower limit. Beta has ybounds c(0, 1) with both bounds open. The
  # lower bound alone gives 1, which brms refuses with "requires
  # response smaller than 1", and every prediction whose frame carries
  # an `NA` response would fail with it.
  ybounds <- family_obj$ybounds
  closed <- family_obj$closed
  is_int <- !is.null(family_obj$type) && family_obj$type == "int"

  if (is.null(ybounds) || length(ybounds) < 2L) {
    return(if (is_int) 1L else 0)
  }

  lb <- ybounds[1L]
  ub <- ybounds[2L]

  # A dummy sits strictly inside the support: an open bound is stepped
  # in from, and a closed bound is usable as it is. An absent `closed`
  # flag is read as open.
  step <- if (is_int) 1 else 1e-6
  lo <- if (!is.finite(lb)) {
    -Inf
  } else if (isTRUE(closed[1L])) {
    lb
  } else {
    lb + step
  }
  hi <- if (!is.finite(ub)) {
    Inf
  } else if (isTRUE(closed[2L])) {
    ub
  } else {
    ub - step
  }

  value <- if (is.finite(lo) && is.finite(hi)) {
    (lo + hi) / 2
  } else if (is.finite(lo)) {
    max(lo, 1)
  } else if (is.finite(hi)) {
    min(hi, 0)
  } else {
    0
  }

  if (is_int) as.integer(round(value)) else value
}


#' Check the arguments that choose a prediction's group-level terms
#'
#' `re_formula` takes `NULL`, for every group-level term, or `NA`, for
#' none. brms also takes a formula naming some of the terms, and then
#' numbers the terms it keeps afresh, while the fitted draws keep the
#' numbers the whole model gave them. Reading one against the other
#' pairs a term's design with another term's coefficients, and a
#' formula is refused for that reason. A grouping level the fit never
#' saw is refused when the predictor is composed, whatever
#' `allow_new_levels` says, and `sample_new_levels` is checked as brms
#' checks it.
#'
#' @param re_formula,allow_new_levels,sample_new_levels As the
#'   prediction methods take them
#' @return `NULL`, invisibly
#' @noRd
validate_group_level_args <- function(re_formula, allow_new_levels,
                                      sample_new_levels) {
  if (!is.null(re_formula) && !checkmate::test_scalar_na(re_formula)) {
    stop(insight::format_error(c(
      "'re_formula' takes NULL or NA.",
      x = "A formula choosing some of the group-level terms is not supported.",
      i = paste0("Use NULL to include every group-level term or NA to ",
                 "include none.")
    )), call. = FALSE)
  }
  checkmate::assert_flag(allow_new_levels)
  checkmate::assert_choice(sample_new_levels,
                           c("uncertainty", "gaussian", "old_levels"))
  if (sample_new_levels != "uncertainty" && !allow_new_levels) {
    stop(insight::format_error(c(
      paste0("'sample_new_levels' can only be changed when ",
             "'allow_new_levels' is TRUE."),
      i = "Set 'allow_new_levels = TRUE' or leave 'sample_new_levels' unset."
    )), call. = FALSE)
  }
  invisible(NULL)
}


#' The Stan data and draws one prediction's linear predictors read
#'
#' brms writes the design of every term into its Stan data. The data it
#' builds for `newdata` is the prediction's design, which
#' `extract_linpred_from_prep()` reads against the fit's own draws.
#'
#' @param draws `[ndraws x npar]` draws matrix of the parameters, named
#'   as `brmsfit`'s own Stan program names them
#' @param brmsfit The brms model the parameters belong to
#' @param newdata Data frame to predict for
#' @param re_formula,allow_new_levels,sample_new_levels As
#'   `validate_group_level_args()` takes them
#' @param req_vars The variables `newdata` has to carry, or `NULL` for
#'   every variable the model reads. brms fills each one left out from
#'   the first row of the fitted data, which suits a prediction that
#'   reads some of the terms and not the rest.
#' @return A `mvgam_prep`: the Stan data, the draws, the formula, the
#'   frame and its row count, the fit's table of group-level terms and
#'   the `re_formula` the data was built under
#' @noRd
prepare_linpred_data <- function(draws, brmsfit, newdata,
                                 re_formula = NULL,
                                 allow_new_levels = FALSE,
                                 sample_new_levels = "uncertainty",
                                 req_vars = NULL) {
  checkmate::assert_class(draws, "draws_matrix")
  checkmate::assert_class(brmsfit, "brmsfit")
  checkmate::assert_data_frame(newdata, min.rows = 1L)
  checkmate::assert_character(req_vars, any.missing = FALSE, null.ok = TRUE)
  validate_group_level_args(re_formula, allow_new_levels, sample_new_levels)

  # brms evaluates the response to build the data, and refuses a
  # missing one with "missing value where TRUE/FALSE needed". Each
  # response absent or `NA` takes a value its family accepts, which
  # nothing downstream reads. An ordinal response is a factor, where a
  # numeric value would land outside the levels and stay `NA`. Its
  # first level stands in.
  filled <- newdata
  columns <- response_columns(brmsfit$formula)
  families <- formula_families(brmsfit$formula, brmsfit$family)
  for (key in names(columns)) {
    column <- columns[[key]]
    existing <- filled[[column]]
    value <- if (is.factor(existing)) {
      levels(existing)[1L]
    } else {
      get_safe_dummy_value(families[[key]])
    }
    if (is.null(existing)) {
      filled[[column]] <- rep(value, nrow(filled))
    } else if (anyNA(existing)) {
      filled[[column]][is.na(existing)] <- value
    }
  }

  # `check_response = FALSE` turns off brms's check of the response
  # against its addition terms. The value filled in above only has to
  # lie in the family's support, and checking it against a real
  # addition term compares two unrelated things: a padded row of a
  # binomial fit carries `trials = 0` against a filled response of 1,
  # and brms refuses the whole frame with "Number of trials is smaller
  # than the number of events".
  sdata <- brms::standata(
    brmsfit,
    newdata = filled,
    re_formula = re_formula,
    allow_new_levels = allow_new_levels,
    check_response = FALSE,
    req_vars = req_vars,
    internal = TRUE
  )

  # `ranef` names each group-level term's correlation block and place
  # in it, which is how brms names its design and coefficients. Under
  # `re_formula = NA` brms writes no group-level data at all.
  structure(
    list(
      sdata = sdata,
      draws = draws,
      formula = brmsfit$formula,
      newdata = newdata,
      nobs = nrow(newdata),
      ranef = brmsfit$ranef,
      re_formula = re_formula
    ),
    class = "mvgam_prep"
  )
}
