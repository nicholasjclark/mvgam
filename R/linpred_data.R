#' A response value a family's support accepts
#'
#' brms builds a model's Stan data by evaluating the whole formula,
#' response included. Even a frame used only for prediction needs a
#' response brms will accept. This picks a point inside the
#' family's support. A bounded support takes its midpoint, stepped in
#' from any open bound. A half-bounded support takes a value just inside
#' the finite bound, rounded for a count. No prediction uses the
#' value.
#'
#' @param family_obj A `brmsfamily`, whose `ybounds`, `closed` and
#'   `type` describe the support
#' @return A single value inside the support; an integer for a count
#' @noRd
get_safe_dummy_value <- function(family_obj) {
  checkmate::assert_class(family_obj, "brmsfamily")

  # brms records the support as `ybounds` plus a `closed` flag per
  # bound. Both bounds matter. A family bounded above rejects a dummy
  # at or beyond its upper limit as readily as one below its lower
  # limit. Beta has ybounds c(0, 1) with both bounds open. Stepping up
  # from the lower bound alone gives 1, which brms refuses with
  # "requires response smaller than 1". Every prediction whose frame
  # carries an `NA` response would then fail.
  ybounds <- family_obj$ybounds
  closed <- family_obj$closed
  is_int <- !is.null(family_obj$type) && family_obj$type == "int"

  if (is.null(ybounds) || length(ybounds) < 2L) {
    return(if (is_int) 1L else 0)
  }

  lb <- ybounds[1L]
  ub <- ybounds[2L]

  # The dummy must lie strictly inside the support. The code steps in
  # from an open bound and uses a closed bound as it is. An absent
  # `closed` flag counts as open.
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
#' none. brms also takes a formula naming some of the terms. It then
#' numbers the kept terms afresh, while the fitted draws keep the
#' numbers the whole model gave them. Matching the two numberings would
#' pair a term's design with another term's coefficients. A formula is
#' refused for that reason. A grouping level the fit never saw is
#' refused when the predictor is composed, whatever `allow_new_levels`
#' says. `sample_new_levels` is checked as brms checks it.
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


#' Check the two ways a caller selects draws
#'
#' `ndraws` takes that many draws at random; `draw_ids` names the ones
#' to use. A call giving both asks for two different subsets. The
#' resolver would honour the indices and ignore the count. The pair is
#' refused here, at the boundary a user's arguments enter. So is a count
#' that is no positive whole number and an index that names no draw.
#' Every post-fit method taking either selector checks them through
#' this call alone.
#'
#' Internal helpers are exempt. `resolve_family_pars()` takes `ndraws`
#' as the number of rows it returns. Its `draw_ids` then
#' name the draws those rows hold.
#'
#' @param ndraws,draw_ids As the post-fit methods take them
#' @return `NULL`, invisibly
#' @noRd
validate_draw_selectors <- function(ndraws, draw_ids) {
  checkmate::assert_int(ndraws, lower = 1L, null.ok = TRUE)
  checkmate::assert_integerish(draw_ids, lower = 1L, any.missing = FALSE,
                               min.len = 1L, null.ok = TRUE)
  if (!is.null(ndraws) && !is.null(draw_ids)) {
    stop(insight::format_error(c(
      "Specify only one of 'ndraws' or 'draw_ids'.",
      x = paste0("Got 'ndraws' = ", ndraws, " and ", length(draw_ids),
                 " 'draw_ids'.")
    )), call. = FALSE)
  }
  invisible(NULL)
}


#' The Stan data and draws for one prediction's linear predictors
#'
#' brms writes the design of every term into its Stan data. The data it
#' builds for `newdata` is the prediction's design.
#' `extract_linpred_from_prep()` multiplies that design by the fit's
#' own draws.
#'
#' @param draws `[ndraws x npar]` draws matrix of the parameters, named
#'   as `brmsfit`'s own Stan program names them
#' @param brmsfit The brms model the parameters belong to
#' @param newdata Data frame to predict for
#' @param re_formula,allow_new_levels,sample_new_levels As
#'   `validate_group_level_args()` takes them
#' @param req_vars The variables `newdata` has to carry, or `NULL` for
#'   every variable the model uses. brms fills each one left out from
#'   the first row of the fitted data. That suits a prediction that
#'   uses some of the terms and ignores the rest.
#' @param trend Whether `brmsfit` is the trend's model. Its rows are
#'   the trend's cells. Its response is a placeholder without data.
#' @return A `mvgam_prep`: the Stan data, the draws, the formula and
#'   its `model_predictors()`, the frame and its row count, the fit's
#'   table of group-level terms, the `re_formula` the data was built
#'   under, the fitted data and the fitted row each prediction row
#'   repeats
#' @noRd
prepare_linpred_data <- function(draws, brmsfit, newdata,
                                 re_formula = NULL,
                                 allow_new_levels = FALSE,
                                 sample_new_levels = "uncertainty",
                                 req_vars = NULL, trend = FALSE) {
  checkmate::assert_class(draws, "draws_matrix")
  checkmate::assert_class(brmsfit, "brmsfit")
  checkmate::assert_data_frame(newdata, min.rows = 1L)
  checkmate::assert_character(req_vars, any.missing = FALSE, null.ok = TRUE)
  checkmate::assert_flag(trend)
  validate_group_level_args(re_formula, allow_new_levels, sample_new_levels)

  # brms evaluates the response to build the data. It refuses a
  # missing one with "missing value where TRUE/FALSE needed". Each
  # response absent or `NA` takes a value its family accepts. No
  # prediction uses that value. An ordinal response is a factor, where
  # a numeric value would land outside the levels and stay `NA`. It
  # takes its first level instead. A response written `y | mi()` keeps
  # its missing values, which the model imputes.
  filled <- newdata
  forms <- response_formulas(brmsfit$formula)
  families <- formula_families(brmsfit$formula, brmsfit$family)
  for (key in names(forms)) {
    column <- form_response_column(forms[[key]])
    existing <- filled[[column]]
    if (form_imputes(forms[[key]])) {
      if (is.null(existing)) filled[[column]] <- NA_real_
      next
    }
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
  # lie in the family's support. Checking it against a real addition
  # term compares two unrelated things. A padded row of a binomial fit
  # carries `trials = 0` against a filled response of 1. brms would
  # refuse the whole frame with "Number of trials is smaller than the
  # number of events".
  sdata <- without_brms_fit_advisories(brms::standata(
    brmsfit,
    newdata = filled,
    re_formula = re_formula,
    allow_new_levels = allow_new_levels,
    check_response = FALSE,
    req_vars = req_vars,
    internal = TRUE
  ))

  predictors <- lapply(model_predictors(brmsfit$formula), function(p) {
    p$special <- special_term_frame(p$pred, brmsfit$data)
    p
  })
  sdata <- restore_fixed_assign(sdata, predictors, brmsfit$data)

  # `ranef` gives each group-level term's block `id` and place `cn`,
  # which name its design and coefficients. `me` numbers the model's
  # `me()` terms as `Xn_<k>` and `Xme_<k>` take them.
  has_special <- any(vapply(predictors, function(p) {
    NROW(p$special) > 0L
  }, logical(1L)))
  structure(
    list(
      sdata = sdata,
      draws = draws,
      formula = brmsfit$formula,
      predictors = predictors,
      newdata = newdata,
      nobs = nrow(newdata),
      ranef = brmsfit$ranef,
      re_formula = re_formula,
      me = if (has_special) me_term_frame(brmsfit$formula, brmsfit$data),
      data = brmsfit$data,
      # Each observation of a trend cell takes the cell's draws. The
      # trend's placeholder response identifies no row.
      fitted_row = if (has_special) {
        fitted_row_index(
          newdata, brmsfit$data,
          cols = if (trend) {
            setdiff(names(brmsfit$data), response_columns(brmsfit$formula))
          } else {
            names(brmsfit$data)
          },
          shared = trend
        )
      }
    ),
    class = "mvgam_prep"
  )
}


#' The fitted row each prediction row repeats
#'
#' A latent quantity the fit sampled per row, such as the noise-free
#' value of an `me()` covariate or a missing value `mi()` imputes,
#' belongs to that row. A prediction row holding the same values in
#' `cols` as a fitted row, missing values included, takes that row's
#' draws. Every other row is new. Its latent values are drawn afresh.
#'
#' @param newdata The prediction frame
#' @param data The fitted model's data
#' @param cols The columns that identify a row
#' @param shared `FALSE` to pair identical fitted rows with identical
#'   prediction rows in order. `TRUE` to give every identical prediction
#'   row the first identical fitted row, as each observation of a trend
#'   cell takes the cell's draws.
#' @return Integer vector along the rows of `newdata`: the fitted row
#'   each repeats, or `NA` for a new row
#' @noRd
fitted_row_index <- function(newdata, data, cols = names(data),
                             shared = FALSE) {
  checkmate::assert_data_frame(newdata)
  checkmate::assert_data_frame(data)
  checkmate::assert_subset(cols, names(data))
  checkmate::assert_flag(shared)
  if (!all(cols %in% names(newdata))) {
    return(rep(NA_integer_, nrow(newdata)))
  }
  # One key column per identifying column. Paired rows add the
  # occurrence of the row among the rows identical to it.
  keyed <- function(frame, id) {
    out <- lapply(stats::setNames(cols, paste0(".k", seq_along(cols))),
                  function(v) {
                    x <- frame[[v]]
                    if (is.factor(x)) as.character(x) else as.vector(x)
                  })
    out <- as.data.frame(out)
    if (!shared) {
      out$.occurrence <- data.table::rowidv(out, names(out))
    }
    out[[id]] <- seq_len(nrow(frame))
    out
  }
  fitted <- keyed(data, ".fitted")
  if (shared) {
    fitted <- fitted[!duplicated(fitted[setdiff(names(fitted), ".fitted")]),
                     , drop = FALSE]
  }
  joined <- merge(keyed(newdata, ".new"), fitted, all.x = TRUE,
                  sort = FALSE)
  joined$.fitted[order(joined$.new)]
}


#' The special terms of one predictor, each with its formula term
#'
#' The model has one coefficient `bsp<sfx>[j]` per column of the special
#' terms' design. Each row of the frame is one column, with its `coef`
#' name, its `mo()`, `me()` and `mi()` calls and the covariate `Ic`
#' multiplying it. `mo(x):f` is one formula term with a column per
#' level of `f` after the first. The design here replaces each special
#' call by a column of ones. Its `assign` attribute then places each
#' column in its formula term.
#'
#' @param pred The predictor's `brmsterms` entry
#' @param data The fitted model's data
#' @return One row per coefficient, each with a `label` column naming
#'   its formula term. Empty without special terms.
#' @noRd
special_term_frame <- function(pred, data) {
  checkmate::assert_data_frame(data)
  form <- pred[["sp"]]
  if (!inherits(form, "formula")) return(data.frame())
  frame <- utils::getFromNamespace("frame_sp", "brms")(pred, data)
  labels <- formula_term_labels(form)
  # Each distinct special part becomes a column of ones, `.sp<k>`
  specials <- character(0L)
  terms <- character(length(labels))
  for (i in seq_along(labels)) {
    parts <- interaction_parts(str2lang(labels[i]))
    for (j in seq_along(parts)) {
      if (length(find_calls(parts[[j]], c("mo", "me", "mi"))) == 0L) next
      key <- deparse1(parts[[j]])
      specials <- union(specials, key)
      parts[[j]] <- as.name(paste0(".sp", match(key, specials)))
    }
    terms[i] <- deparse1(Reduce(function(a, b) call(":", a, b), parts))
  }
  data[paste0(".sp", seq_along(specials))] <- 1
  # The intercept gives a factor in an interaction treatment contrasts,
  # one column per level after the first. The columns themselves carry
  # no intercept.
  assign <- design_columns(stats::reformulate(terms), data)
  assign <- assign[names(assign) != "Intercept"]
  if (length(assign) != nrow(frame)) {
    stop_mvgam_fault(
      "Each special-term coefficient must belong to one formula term.",
      paste0(nrow(frame), " coefficients and ", length(assign),
             " design columns.")
    )
  }
  frame$label <- labels[assign]
  frame
}


#' The measurement-error terms of a model, numbered model-wide
#'
#' @param formula The model's `brmsformula` or `mvbrmsformula`
#' @param data The fitted model's data
#' @return One row per `me()` term, with its call `term`, covariate
#'   `xname`, grouping `grname` and coefficient name `coef`. Row `k` is
#'   the term whose data are `Xn_<k>` and `noise_<k>`.
#' @noRd
me_term_frame <- function(formula, data) {
  checkmate::assert_data_frame(data)
  utils::getFromNamespace("frame_me", "brms")(brms_terms(formula), data)
}


#' The parts of an interaction, `a:b:c` as `a`, `b` and `c`
#'
#' @param expr A parsed term
#' @return List of expressions
#' @noRd
interaction_parts <- function(expr) {
  if (is.call(expr) && identical(expr[[1L]], as.name(":"))) {
    return(c(interaction_parts(expr[[2L]]), interaction_parts(expr[[3L]])))
  }
  list(expr)
}


#' Restore the term numbers an ordinal design lacks
#'
#' The thresholds replace the intercept column of an ordinal
#' predictor's design and of its category-specific design `Xcs`.
#' Removing the column dropped `attr(X, "assign")`,
#' the term of each column. The numbers come back from
#' `design_columns()` on the fitted data. The fitted data fix the factor
#' levels, which fixes the columns.
#'
#' @param sdata Stan data from `brms::standata(internal = TRUE)`
#' @param predictors What `model_predictors()` gives for the model
#' @param data The fitted model's data
#' @return `sdata`, each design carrying its `assign` attribute
#' @noRd
restore_fixed_assign <- function(sdata, predictors, data) {
  checkmate::assert_data_frame(data)
  # Each design, by the `brmsterms` entry holding its formula
  designs <- c(fe = "X", cs = "Xcs")
  for (predictor in predictors) {
    for (part in names(designs)) {
      name <- paste0(designs[[part]], predictor$suffix)
      X <- sdata[[name]]
      if (is.null(X) || ncol(X) == 0L || !is.null(attr(X, "assign"))) {
        next
      }
      assign <- design_columns(predictor$pred[[part]], data)
      assign <- unname(assign[names(assign) != "Intercept"])
      if (length(assign) != ncol(X)) {
        stop_mvgam_fault(
          "An ordinal design must match the model matrix of its terms.",
          paste0("'", name, "' has ", ncol(X), " columns and the model ",
                 "matrix ", length(assign), ".")
        )
      }
      attr(sdata[[name]], "assign") <- assign
    }
  }
  sdata
}


#' The columns of a term formula's design, named as the draws name them
#'
#' `brms_rename()` names each column: the intercept `Intercept` and a
#' level `B` of a factor `f` `fB`.
#'
#' @param form A `brmsterms` formula, such as a predictor's `fe` or
#'   `cs` entry or a group-level block's `form`, or `NULL`
#' @param data A data frame holding the formula's variables
#' @return Integer vector of term numbers, as `attr(X, "assign")` holds
#'   them, named by column. Empty for a `NULL` formula.
#' @noRd
design_columns <- function(form, data) {
  checkmate::assert_data_frame(data)
  if (!inherits(form, "formula")) return(integer(0L))
  X <- stats::model.matrix(stats::terms(form), data)
  stats::setNames(attr(X, "assign"), brms_rename(colnames(X)))
}
