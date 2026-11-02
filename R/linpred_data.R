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
#' `re_formula` takes `NULL`, for every group-level term, `NA`, for
#' none, or a formula naming the terms to keep. `sample_new_levels` is
#' checked as brms checks it.
#'
#' @param re_formula,allow_new_levels,sample_new_levels As the
#'   prediction methods take them
#' @return `NULL`, invisibly
#' @noRd
validate_group_level_args <- function(re_formula, allow_new_levels,
                                      sample_new_levels) {
  if (!checkmate::test_scalar_na(re_formula)) {
    checkmate::assert_formula(re_formula, null.ok = TRUE)
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
#' @param row_units Function of no arguments giving what identifies a
#'   row, `list(new, fitted)`: one frame along `newdata` and one along
#'   the fitted data, with the same columns. It runs for a model with
#'   latent values alone. `NULL` takes every column of the fitted data.
#' @param impute Function drawing a missing `mi()` value at a new row.
#'   It takes the variable's key, its `[ndraws x nrows]` predictor at
#'   those rows and the row numbers.
#' @return A `mvgam_prep`: the Stan data, the draws, the formula and
#'   its `model_predictors()`, the frame and its row count, the fit's
#'   table of group-level terms, the `re_formula` the data was built
#'   under, the fitted data, the frames of `row_units`, the coefficients
#'   of `new_level_coefs()` and an environment `latent_once()` fills
#' @noRd
prepare_linpred_data <- function(draws, brmsfit, newdata,
                                 re_formula = NULL,
                                 allow_new_levels = FALSE,
                                 sample_new_levels = "uncertainty",
                                 req_vars = NULL, row_units = NULL,
                                 impute = NULL) {
  checkmate::assert_class(draws, "draws_matrix")
  checkmate::assert_class(brmsfit, "brmsfit")
  checkmate::assert_data_frame(newdata, min.rows = 1L)
  checkmate::assert_character(req_vars, any.missing = FALSE, null.ok = TRUE)
  checkmate::assert_function(row_units, null.ok = TRUE)
  checkmate::assert_function(impute, null.ok = TRUE)
  validate_group_level_args(re_formula, allow_new_levels, sample_new_levels)
  re_formula <- side_re_formula(brmsfit, re_formula)

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
  # A prediction row is its own hypothetical. Several rows of one level
  # of `me(gr = )` may hold different measured values, as a grid sweeping
  # the variable does. The data holds each row's value. `me_value()`
  # places the rows on their levels.
  per_row <- brmsfit
  per_row$formula <- ungroup_me_terms(brmsfit$formula)
  # The names of the rewritten terms' coefficients change with them.
  # brms looks up each `mo()` term's prior by name.
  if (!identical(per_row$formula, brmsfit$formula)) {
    per_row$prior <- brms::default_prior(
      per_row$formula, data = brmsfit$data, family = brmsfit$family,
      data2 = brmsfit$data2
    )
  }
  sdata <- without_brms_fit_advisories(brms::standata(
    per_row,
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
  has_latent <- any(vapply(predictors, function(p) {
    length(unlist(p$special$calls_me)) +
      length(unlist(p$special$calls_mi)) > 0L
  }, logical(1L)))
  prep <- structure(
    list(
      sdata = sdata,
      draws = draws,
      formula = brmsfit$formula,
      predictors = predictors,
      newdata = newdata,
      nobs = nrow(newdata),
      ranef = kept_group_terms(brmsfit, re_formula),
      re_formula = re_formula,
      me = if (has_latent) me_term_frame(brmsfit$formula, brmsfit$data),
      data = brmsfit$data,
      impute = impute,
      latents = new.env(parent = emptyenv()),
      units = if (has_latent) {
        if (is.null(row_units)) {
          list(new = newdata, fitted = brmsfit$data)
        } else {
          row_units()
        }
      }
    ),
    class = "mvgam_prep"
  )
  if (allow_new_levels) {
    prep$new_levels <- new_level_coefs(prep, sample_new_levels)
  }
  prep
}


#' The group-level terms a `re_formula` names
#'
#' @param re_formula A formula
#' @return The terms as brms parses them, one row per term
#' @noRd
re_formula_terms <- function(re_formula) {
  checkmate::assert_formula(re_formula)
  brms_terms(rlang::new_formula(
    quote(.mvgam_lhs), rlang::f_rhs(re_formula)
  ))$dpars$mu$re
}


#' Refuse a `re_formula` naming a term no formula of the model holds
#'
#' brms drops such a term without a word. A misspelt group would then
#' drop every group-level term from the prediction.
#'
#' @param object A fitted `mvgam` object
#' @param re_formula `NULL`, `NA` or a formula naming the terms
#' @return `NULL`, invisibly
#' @noRd
check_re_formula_terms <- function(object, re_formula) {
  checkmate::assert_class(object, "mvgam")
  if (!inherits(re_formula, "formula")) return(invisible(NULL))
  own <- dplyr::bind_rows(lapply(c("obs", "trend"), function(side) {
    side_model(object, side)$ranef[c("group", "form")]
  }))
  has_intercept <- function(form) attr(stats::terms(form), "intercept") == 1L
  asked <- re_formula_terms(re_formula)
  for (i in seq_len(NROW(asked))) {
    group <- asked$group[[i]]
    form <- asked$form[[i]]
    held <- own$form[own$group == group]
    known <- all(all.vars(form) %in% unlist(lapply(held, all.vars))) &&
      (!has_intercept(form) || any(vapply(held, has_intercept, logical(1L))))
    if (length(held) == 0L || !known) {
      stop(insight::format_error(c(
        "'re_formula' names a term outside the model.",
        x = paste0("Got '(", deparse1(form[[2L]]), " | ", group, ")'."),
        i = paste0("The model's groups are: ",
                   toString(unique(as.character(own$group))), ".")
      )), call. = FALSE)
    }
  }
  invisible(NULL)
}


#' `re_formula` in one side's own terms
#'
#' One formula applies to the observation and the trend models. A term
#' finds its group in a model that holds it, where it takes that
#' model's grouping as written, weights included. A term of another
#' model is dropped.
#'
#' @param brmsfit The brms model of one side
#' @param re_formula `NULL`, `NA` or a formula naming the terms
#' @return `re_formula`, a formula rewritten for the model
#' @noRd
side_re_formula <- function(brmsfit, re_formula) {
  checkmate::assert_class(brmsfit, "brmsfit")
  if (is.null(re_formula) || checkmate::test_scalar_na(re_formula)) {
    return(re_formula)
  }
  asked <- re_formula_terms(re_formula)
  own <- brmsfit$ranef
  terms <- character(0L)
  for (i in seq_len(NROW(asked))) {
    hit <- match(asked$group[[i]], own$group)
    if (is.na(hit)) next
    terms <- c(terms, paste0(
      "(", deparse1(asked$form[[i]][[2L]]),
      if (asked$cor[[i]]) " | " else " || ",
      own$gcall[[hit]]$label, ")"
    ))
  }
  stats::reformulate(if (length(terms)) terms else "1")
}


#' The group-level terms a prediction keeps
#'
#' brms numbers the terms `re_formula` keeps afresh in the Stan data.
#' The draws keep the numbers of the whole model. `id` and `cn` name the
#' draws. `data_id` and `data_cn` name the data.
#'
#' @param brmsfit The brms model
#' @param re_formula `NULL`, `NA` or a formula naming the terms
#' @return The fit's `ranef` table, reduced to the kept terms
#' @noRd
kept_group_terms <- function(brmsfit, re_formula) {
  checkmate::assert_class(brmsfit, "brmsfit")
  ranef <- brmsfit$ranef
  if (NROW(ranef) == 0L) return(ranef)
  ranef$data_id <- ranef$id
  ranef$data_cn <- ranef$cn
  if (is.null(re_formula) || checkmate::test_scalar_na(re_formula)) {
    return(ranef)
  }
  update <- utils::getFromNamespace("update_re_terms", "brms")
  frame <- utils::getFromNamespace("frame_re", "brms")
  kept <- frame(brms_terms(update(brmsfit$formula, re_formula)),
                brmsfit$data)
  cols <- c("group", "coef", "resp", "dpar", "nlpar", "type")
  at <- match(row_keys(kept, cols), row_keys(ranef, cols))
  out <- ranef[at, , drop = FALSE]
  out$data_id <- kept$id
  out$data_cn <- kept$cn
  out$form <- kept$form
  attr(out, "levels") <- attr(ranef, "levels")
  out
}


#' `me()` terms with their grouping removed
#'
#' @param x A `brmsformula`, `mvbrmsformula` or one formula of either
#' @return `x`, each `me(x, sdx, gr = g)` written `me(x, sdx)`
#' @noRd
ungroup_me_terms <- function(x) {
  if (inherits(x, "mvbrmsformula")) {
    x$forms <- lapply(x$forms, ungroup_me_terms)
    return(x)
  }
  if (inherits(x, "bform")) {
    x$formula <- ungroup_me_terms(x$formula)
    if (length(x$pforms) > 0L) {
      x$pforms[] <- lapply(x$pforms, ungroup_me_terms)
    }
    return(x)
  }
  if (!is.call(x)) return(x)
  if (identical(call_head_name(x), "me")) {
    matched <- match.call(brms::me, x)
    if (is.null(matched$gr)) return(x)
    matched$gr <- NULL
    return(matched)
  }
  # An index such as `z[, 1]` holds an empty argument
  for (i in seq_along(x)[-1L]) {
    if (!identical(x[[i]], quote(expr = ))) {
      x[[i]] <- ungroup_me_terms(x[[i]])
    }
  }
  x
}


#' One key per row of a frame, equal for rows equal in `cols`
#'
#' Missing values equal each other. An integer and a double holding one
#' number give one key.
#'
#' @param frame A data frame
#' @param cols The columns to compare
#' @return Character vector along the rows of `frame`
#' @noRd
row_keys <- function(frame, cols) {
  checkmate::assert_data_frame(frame)
  checkmate::assert_subset(cols, names(frame))
  values <- lapply(cols, function(v) {
    x <- frame[[v]]
    as.character(if (is.numeric(x)) as.numeric(x) else x)
  })
  do.call(paste, c(values, sep = "\r"))
}


#' The fitted row each prediction row repeats
#'
#' Rows identical in `cols` pair with fitted rows in order. They start
#' over once those run out. Copies of one fitted row stacked in a frame
#' then each take that row.
#'
#' @param newdata The prediction frame
#' @param data The fitted model's data
#' @param cols The columns that identify a row
#' @return Integer vector along the rows of `newdata`: the fitted row
#'   each repeats, or `NA` for a new row
#' @noRd
fitted_row_index <- function(newdata, data, cols = names(data)) {
  checkmate::assert_data_frame(newdata)
  checkmate::assert_data_frame(data)
  if (!all(cols %in% names(newdata))) {
    return(rep(NA_integer_, nrow(newdata)))
  }
  fitted <- row_keys(data, cols)
  groups <- unique(fitted)
  group <- match(fitted, groups)
  size <- tabulate(group, length(groups))
  start <- cumsum(c(0L, size))
  keys <- row_keys(newdata, cols)
  at <- match(keys, groups)
  turn <- data.table::rowidv(keys)
  order(group)[start[at] + (turn - 1L) %% size[at] + 1L]
}


#' The fitted unit of each prediction row
#'
#' A latent quantity the fit sampled, such as the noise-free value of an
#' `me()` covariate or a value `mi()` imputes, belongs to a fitted unit.
#' A prediction row in that unit keeps it, whatever covariates it
#' changes.
#'
#' @param prep A `mvgam_prep`
#' @return Integer vector along the rows of the prediction: the unit's
#'   fitted row, `NA` for a new unit
#' @noRd
unit_rows <- function(prep) {
  fitted_row_index(prep$units$new, prep$units$fitted)
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
