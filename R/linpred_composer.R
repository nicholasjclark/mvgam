# Composing a linear predictor from a fitted model's draws.
#
# A model has one linear predictor per response, per distributional
# parameter and per non-linear parameter. One naming rule, which
# `predictor_suffix()` states, covers every piece of each. Every term
# reader here takes that suffix. One body of code composes the mean of
# a model with one response, a response of a model with several, a
# distributional parameter and a non-linear parameter. Each reader
# returns its own `[ndraws x nobs]` contribution, zero when the
# predictor carries no term of that kind.


#' The suffix of the names of one linear predictor's data and draws
#'
#' The suffix holds the distributional parameter unless it is the mean,
#' then the response of a model with several, then the non-linear
#' parameter, each after an underscore.
#' `sigma` of the response `y1` takes its design from `X_sigma_y1` and
#' its coefficients from `b_sigma_y1`. The mean of a model with one
#' response takes `X` and `b`. A grouping index is named by the response
#' alone, as `J_1_y1`.
#'
#' @param resp The response's key on a model with several, or `NULL` or
#'   `""` for a model with one
#' @param dpar The distributional parameter, or `NULL` for the mean
#' @param nlpar The non-linear parameter, or `NULL`
#' @return `""`, or the suffix with its leading underscore
#' @noRd
predictor_suffix <- function(resp = NULL, dpar = NULL, nlpar = NULL) {
  checkmate::assert_string(resp, null.ok = TRUE)
  checkmate::assert_string(dpar, null.ok = TRUE)
  checkmate::assert_string(nlpar, null.ok = TRUE)
  parts <- c(if (!identical(dpar %||% "mu", "mu")) dpar, resp, nlpar)
  parts <- parts[nzchar(parts)]
  if (length(parts) == 0L) {
    return("")
  }
  paste0("_", paste(parts, collapse = "_"))
}


#' A predictor's name part, or NULL when the model has none
#'
#' `predictor_suffix()` takes `NULL` for a part a model does not have,
#' while a `brmsterms` frame spells the same absence as `""`.
#'
#' @param x A character scalar, or NULL.
#' @return `x`, or NULL when it is absent or empty.
#' @noRd
null_if_blank <- function(x) {
  if (length(x) == 0L || is.na(x[1L]) || !nzchar(x[1L])) {
    return(NULL)
  }
  as.character(x[1L])
}


#' Names with each missing value spelled as an empty string
#'
#' The `ranef` table spells an absent response, parameter or group as
#' `""` or `NA`. Comparisons take the one spelling.
#'
#' @param x A vector
#' @return Character vector the length of `x`
#' @noRd
blank_if_na <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x
}


#' Every linear predictor of a model
#'
#' Each response's mean has one unless the mean is non-linear. Each
#' further distributional parameter the response predicts has one, each
#' mixture component's mean included. Each non-linear parameter has one
#' of its own. A non-linear predictor is an expression over the
#' parameters below it and declares no parameters itself.
#'
#' @param formula A `brmsformula` or `mvbrmsformula`, or `NULL`
#' @return A list with one entry per predictor: `resp`, `dpar` and
#'   `nlpar` as `predictor_suffix()` takes them, each `NULL` where it
#'   does not apply and `dpar` `NULL` for the mean; `suffix`, the name
#'   suffix `predictor_suffix()` gives them; and `pred`, the predictor's
#'   `brmsterms` entry
#' @noRd
model_predictors <- function(formula) {
  if (is.null(formula)) {
    return(list())
  }
  out <- list()
  for (frame in response_frames(brms_terms(formula))) {
    linear <- Filter(function(pred) !inherits(pred, "btnl"),
                     c(frame$dpars, frame$nlpars))
    for (pred in unname(linear)) {
      dpar <- null_if_blank(pred$dpar)
      part <- list(
        resp = null_if_blank(frame$resp),
        dpar = if (!identical(dpar, "mu")) dpar,
        nlpar = null_if_blank(pred$nlpar)
      )
      out[[length(out) + 1L]] <- c(
        part,
        list(suffix = predictor_suffix(part$resp, part$dpar, part$nlpar),
             pred = pred)
      )
    }
  }
  out
}


#' Linear predictor of a prepared model, on the link scale
#'
#' @param prep A `mvgam_prep` from `prepare_linpred_data()`
#' @param resp One response of a model with several, or `NULL` for
#'   each
#' @param dpar The distributional parameter, or `NULL` for the mean
#' @return A `[ndraws x nobs]` matrix, or a list of them named by
#'   response when a model with several is asked for none
#' @noRd
extract_linpred_from_prep <- function(prep, resp = NULL, dpar = NULL) {
  over_responses(prep, resp, function(form, r) {
    compose_linpred(prep, form, r, dpar)
  })
}


#' Each formula term's part of a prepared model's linear predictor
#'
#' @inheritParams extract_linpred_from_prep
#' @return A named list of `[ndraws x nobs]` matrices, one per term as
#'   `linear_terms_parts()` names them, or a list of them named by
#'   response when a model with several is asked for none. A
#'   non-linear mean is one part, named by its expression.
#' @noRd
extract_parts_from_prep <- function(prep, resp = NULL, dpar = NULL) {
  over_responses(prep, resp, function(form, r) {
    compose_linpred_parts(prep, form, r, dpar)
  })
}


#' One result per response a prediction asks for
#'
#' @inheritParams extract_linpred_from_prep
#' @param compose Function of a response's `brmsformula` and its key,
#'   `NULL` on a model with one response
#' @return What `compose` gives, or a list of it named by response
#'   when a model with several is asked for none
#' @noRd
over_responses <- function(prep, resp, compose) {
  checkmate::assert_class(prep, "mvgam_prep")
  checkmate::assert_names(
    names(prep),
    must.include = c("draws", "sdata", "nobs", "formula", "predictors")
  )
  checkmate::assert_function(compose)
  resolve_resp(prep$formula, resp)
  forms <- response_formulas(prep$formula)
  if (length(forms) == 1L) {
    return(compose(forms[[1L]], NULL))
  }
  if (!is.null(resp)) {
    return(compose(forms[[resp]], resp))
  }
  lapply(stats::setNames(nm = names(forms)), function(r) {
    compose(forms[[r]], r)
  })
}


#' One response's mean or distributional parameter, on the link scale
#'
#' A non-linear mean is its expression evaluated at the non-linear
#' parameters, each of which is a linear predictor of its own. Every
#' other predictor is the sum of its terms.
#'
#' @param prep A `mvgam_prep`
#' @param form The response's `brmsformula`
#' @param resp The response's key on a model with several, or `NULL`
#' @param dpar The distributional parameter, or `NULL` for the mean
#' @return A `[ndraws x nobs]` matrix
#' @noRd
compose_linpred <- function(prep, form, resp, dpar) {
  draws <- as_plain_matrix(posterior::as_draws_matrix(prep$draws))
  n_obs <- predictor_nobs(prep, resp)
  predictor <- predictor_of(prep, form, resp, dpar)
  # Columns are positions in the frame. `b %*% t(X)` takes the design
  # matrix's row names as column names. A predictor with
  # population-level terms carried labels and one without carried
  # none. A closure-unit broadcast then copied a unit's first-row
  # label onto each of its visits.
  unname(if (is.null(predictor)) {
    nonlinear_linpred(prep, draws, form, resp, n_obs)
  } else {
    linear_terms_pred(prep, draws, predictor, n_obs)
  })
}


#' Each formula term's part of one response's mean or parameter
#'
#' A non-linear mean is one part. Its terms combine through its
#' expression, which no sum of parts reproduces.
#'
#' @inheritParams compose_linpred
#' @return A named list of `[ndraws x nobs]` matrices
#' @noRd
compose_linpred_parts <- function(prep, form, resp, dpar) {
  draws <- as_plain_matrix(posterior::as_draws_matrix(prep$draws))
  n_obs <- predictor_nobs(prep, resp)
  predictor <- predictor_of(prep, form, resp, dpar)
  parts <- if (is.null(predictor)) {
    stats::setNames(list(nonlinear_linpred(prep, draws, form, resp, n_obs)),
                    deparse1(form$formula[[3L]]))
  } else {
    linear_terms_parts(prep, draws, predictor, n_obs)
  }
  lapply(parts, unname)
}


#' The linear predictor a response's mean or parameter is written as
#'
#' @inheritParams compose_linpred
#' @return The entry of `prep$predictors`, or `NULL` for a non-linear
#'   mean, which is an expression over predictors of its own
#' @noRd
predictor_of <- function(prep, form, resp, dpar) {
  checkmate::assert_string(dpar, null.ok = TRUE)
  if (identical(dpar %||% "mu", "mu")) {
    if (isTRUE(attr(form$formula, "nl"))) {
      return(NULL)
    }
    return(find_predictor(prep, resp))
  }
  dpar_form <- form$pforms[[dpar]]
  if (is.null(dpar_form)) {
    stop_mvgam_fault(
      paste0("A linear predictor was asked for '", dpar,
             "' without a formula of its own."),
      "Only a parameter given a formula has a linear predictor."
    )
  }
  if (isTRUE(attr(dpar_form, "nl"))) {
    stop(insight::format_error(c(
      paste0("mvgam cannot predict '", dpar,
             "' from a non-linear formula."),
      i = paste0("Write the formula for '", dpar, "' as a linear ",
                 "predictor, or give the non-linear form to the mean.")
    )), call. = FALSE)
  }
  find_predictor(prep, resp, dpar)
}


#' The entry of `prep$predictors` one suffix names
#'
#' @param prep A `mvgam_prep`
#' @param resp,dpar,nlpar The predictor, as `predictor_suffix()` takes it
#' @return One entry of `model_predictors()`
#' @noRd
find_predictor <- function(prep, resp = NULL, dpar = NULL, nlpar = NULL) {
  sfx <- predictor_suffix(resp, dpar, nlpar)
  hit <- Filter(function(p) identical(p$suffix, sfx), prep$predictors)
  if (length(hit) != 1L) {
    stop_mvgam_fault(
      "Each linear predictor must have one entry in the model's terms.",
      paste0("Suffix '", sfx, "' has ", length(hit), " entries.")
    )
  }
  hit[[1L]]
}


#' The rows one response's predictors cover
#'
#' A model with one response covers every row of the prediction. Each
#' response of a model with several covers `N_<resp>` rows.
#'
#' @param prep A `mvgam_prep`
#' @param resp The response's key on a model with several, or `NULL`
#' @return A positive count
#' @noRd
predictor_nobs <- function(prep, resp) {
  n_obs <- if (is.null(resp)) prep$nobs else prep$sdata[[paste0("N_", resp)]]
  checkmate::assert_count(n_obs, positive = TRUE)
  n_obs
}


#' The non-linear parameters of one response's formula
#'
#' A non-linear mean names its parameters in its expression. Each one
#' has a formula of its own in `pforms`. Every other formula there
#' belongs to a distributional parameter.
#'
#' @param form The response's `brmsformula`
#' @return Character vector, empty when the mean is linear
#' @noRd
nonlinear_parameters <- function(form) {
  if (!isTRUE(attr(form$formula, "nl"))) {
    return(character(0L))
  }
  intersect(all.vars(form$formula[[3L]]), names(form$pforms))
}


#' The sum of every term of one linear predictor
#'
#' @param prep A `mvgam_prep`
#' @param draws Plain `[ndraws x npar]` matrix of the posterior
#' @param predictor One entry of `prep$predictors`
#' @param n_obs Number of rows the predictor covers
#' @return A `[ndraws x n_obs]` matrix
#' @noRd
linear_terms_pred <- function(prep, draws, predictor, n_obs) {
  # Each term adds into one matrix, which only one term holds at a time
  eta <- matrix(0, nrow(draws), n_obs)
  for (term in linear_terms(prep, draws, predictor, n_obs)) {
    eta <- eta + term()
  }
  eta
}


#' Each formula term's part of one linear predictor
#'
#' @inheritParams linear_terms_pred
#' @return Named list of `[ndraws x n_obs]` matrices, one per term the
#'   predictor holds, named as the formula writes the term. They sum to
#'   `linear_terms_pred()`.
#' @noRd
linear_terms_parts <- function(prep, draws, predictor, n_obs) {
  lapply(linear_terms(prep, draws, predictor, n_obs), function(term) term())
}


#' The terms of one linear predictor, each as a function to evaluate
#'
#' The terms are the intercept, each fixed-effect term, each smooth
#' term, each group-level block, each `gp()` term, each special term
#' and the offset. `linear_terms_pred()` sums them and
#' `linear_terms_parts()` keeps each one.
#'
#' @inheritParams linear_terms_pred
#' @return Named list of functions of no arguments, each returning its
#'   term's `[ndraws x n_obs]` matrix, named as the formula writes the
#'   term
#' @noRd
linear_terms <- function(prep, draws, predictor, n_obs) {
  checkmate::assert_names(names(predictor),
                          must.include = c("suffix", "pred"))
  sfx <- predictor$suffix
  sdata <- prep$sdata
  pred <- predictor$pred
  terms <- list()

  has_intercept <- has_sampled_intercept(draws, sfx)
  if (has_intercept) {
    terms[["Intercept"]] <- function() {
      matrix(draws[, paste0("b", sfx, "_Intercept")], nrow(draws), n_obs)
    }
  }
  X <- fixed_design(sdata, sfx, n_obs, has_intercept)
  fe <- fixed_term_columns(X, pred[["fe"]])
  smooths <- smooth_term_objects(sdata[[paste0("Xs", sfx)]])
  # One grouping factor may hold several blocks, as `(1 | g) + (0 + x |
  # g)` does. Each block is one term of the formula.
  blocks <- group_level_blocks(prep, predictor)
  kernels <- gp_kernels(pred[["gp"]])
  special <- special_term_rows(predictor$special)
  terms <- c(
    terms,
    lapply(fe, function(cols) function() fixed_pred(draws, X, sfx, cols)),
    lapply(smooths, function(objects) {
      function() smooth_pred(draws, sdata, sfx, n_obs, objects)
    }),
    lapply(blocks, function(block) {
      function() block_pred(prep, draws, block, predictor)
    }),
    Map(function(kernel, i) {
      function() gp_pred(draws, sdata, sfx, kernel, i, n_obs)
    }, kernels, seq_along(kernels)),
    lapply(special, function(rows) {
      function() special_pred(prep, draws, predictor, rows, n_obs)
    })
  )

  offsets <- sdata[[paste0("offsets", sfx)]]
  if (!is.null(offsets)) {
    checkmate::assert_numeric(as.vector(offsets), any.missing = FALSE,
                              finite = TRUE, len = n_obs)
    terms[["offset"]] <- function() {
      data2draws(as.numeric(offsets), c(nrow(draws), n_obs))
    }
  }
  terms
}


#' The columns of each term of a design
#'
#' @param X A design carrying `attr(X, "assign")`: the slopes' design
#'   from `fixed_design()`, or the category-specific `Xcs`
#' @param fe The formula whose term labels `attr(X, "assign")` numbers,
#'   the `fe` or `cs` entry of the predictor's `brmsterms`. Number 0 is
#'   the intercept, a slope column of an uncentred design such as
#'   `bf(..., center = FALSE)` fits.
#' @return Named list of column positions, one entry per term with
#'   columns, named as the formula writes the term
#' @noRd
fixed_term_columns <- function(X, fe) {
  checkmate::assert_matrix(X)
  if (ncol(X) == 0L) {
    return(list())
  }
  assign <- attr(X, "assign")
  labels <- c("Intercept", formula_term_labels(fe))
  if (is.null(assign) || length(assign) != ncol(X) ||
        any(assign < 0L | assign >= length(labels))) {
    stop_mvgam_fault(
      "Every slope column must belong to a fixed-effect term.",
      paste0("Columns: ", paste(colnames(X), collapse = ", "), ". Terms: ",
             paste(labels[-1L], collapse = ", "), ".")
    )
  }
  used <- sort(unique(assign))
  stats::setNames(lapply(used, function(k) which(assign == k)),
                  labels[used + 1L])
}


#' The draw columns of one parameter, in index order
#'
#' A scalar parameter has one column, `sigma`. A vector has one per
#' entry, `sigma[1]`, `sigma[2]`, ...
#'
#' @param cols Column names of a draws matrix
#' @param name The parameter's name
#' @return Character vector of column names, empty when the draws lack
#'   the parameter
#' @noRd
param_draw_columns <- function(cols, name) {
  checkmate::assert_string(name)
  if (name %in% cols) return(name)
  n <- sum(startsWith(cols, paste0(name, "[")))
  if (n == 0L) character(0L) else paste0(name, "[", seq_len(n), "]")
}


#' Posterior columns a term uses
#'
#' The design and the draws both come from the fitted model. A name the
#' design implies and the draws lack is a fault in mvgam.
#'
#' @param draws Plain `[ndraws x npar]` matrix of the posterior
#' @param cols The column names to take, in order
#' @return `draws[, cols]` as a matrix
#' @noRd
draw_columns <- function(draws, cols) {
  missing <- setdiff(cols, colnames(draws))
  if (length(missing) > 0L) {
    stop_mvgam_fault(
      "The posterior draws must hold every column a term uses.",
      paste0("Missing: ", paste(utils::head(missing, 3L), collapse = ", "),
             if (length(missing) > 3L) ", ...", ".")
    )
  }
  # Unnamed, which leaves every product unnamed as well
  unname(draws[, cols, drop = FALSE])
}


#' A data entry a term uses
#'
#' @param sdata The prediction's Stan data
#' @param name The entry's name
#' @return The entry
#' @noRd
sdata_entry <- function(sdata, name) {
  out <- sdata[[name]]
  if (is.null(out)) {
    stop_mvgam_fault(
      "A term of the linear predictor has no prediction data.",
      paste0("Missing: ", name, ".")
    )
  }
  out
}


#' Whether a predictor samples its intercept apart from the slopes
#'
#' @inheritParams fixed_pred
#' @return `TRUE` when the draws hold `b<sfx>_Intercept`
#' @noRd
has_sampled_intercept <- function(draws, sfx) {
  paste0("b", sfx, "_Intercept") %in% colnames(draws)
}


#' One population-level term
#'
#' The slopes multiply the design without its intercept column. The
#' intercept is a parameter of its own, `b_Intercept`. A
#' predictor without that parameter, such as `y ~ 0 + x` or a non-linear
#' parameter, keeps every column of the design.
#'
#' @param draws Plain `[ndraws x npar]` matrix of the posterior
#' @param X The slopes' design, from `fixed_design()`
#' @param sfx The predictor's `predictor_suffix()`
#' @param cols The term's positions among the design's columns
#' @return A `[ndraws x nrow(X)]` matrix
#' @noRd
fixed_pred <- function(draws, X, sfx, cols) {
  checkmate::assert_integerish(cols, lower = 1L, any.missing = FALSE,
                               min.len = 1L)
  b <- draw_columns(draws, paste0("b", sfx, "[", cols, "]"))
  tcrossprod(b, unname(X[, cols, drop = FALSE]))
}


#' The design of a predictor's slopes, each column with its term
#'
#' The slopes' design is `X` less the intercept column a sampled
#' `b_Intercept` replaces. `attr(X, "assign")` gives each column's
#' term, numbered as the term labels of the fixed-effect formula.
#'
#' @param sdata The prediction's Stan data
#' @param sfx The predictor's `predictor_suffix()`
#' @param n_obs Number of rows the predictor covers
#' @param has_intercept Whether the predictor samples `b_Intercept`
#' @return Numeric matrix with `n_obs` rows, possibly no columns, and
#'   an integer `assign` attribute holding each column's term number
#' @noRd
fixed_design <- function(sdata, sfx, n_obs, has_intercept) {
  X <- sdata[[paste0("X", sfx)]]
  if (is.null(X) || ncol(X) == 0L) {
    return(structure(matrix(0, n_obs, 0L), assign = integer(0L)))
  }
  checkmate::assert_matrix(X, nrows = n_obs, col.names = "named")
  assign <- attr(X, "assign")
  keep <- if (has_intercept) {
    colnames(X) != "Intercept"
  } else {
    rep(TRUE, ncol(X))
  }
  out <- X[, keep, drop = FALSE]
  attr(out, "assign") <- if (!is.null(assign)) as.integer(assign[keep])
  out
}


#' One smooth term
#'
#' A predictor's smooth objects are numbered in order, one per level of
#' a `by` factor. Each has an unpenalised part and one or more
#' penalised parts. The unpenalised parts share `Xs`,
#' multiplied by `bs`. `attr(Xs, "smcols")` gives each object's
#' columns of `Xs`. Object `i` has `nb_<i>` penalised parts, the
#' `Zs_<i>_<j>` bases multiplied by `s_<i>_<j>`.
#'
#' @inheritParams fixed_design
#' @param draws Plain `[ndraws x npar]` matrix of the posterior
#' @param objects The numbers of the term's smooth objects
#' @return A `[ndraws x n_obs]` matrix
#' @noRd
smooth_pred <- function(draws, sdata, sfx, n_obs, objects) {
  checkmate::assert_integerish(objects, lower = 1L, any.missing = FALSE,
                               min.len = 1L)
  Xs <- sdata_entry(sdata, paste0("Xs", sfx))
  smcols <- attr(Xs, "smcols")
  if (max(objects) > length(smcols)) {
    stop_mvgam_fault(
      "A smooth term's objects must be among its predictor's.",
      paste0("Object ", max(objects), " of ", length(smcols), ".")
    )
  }
  eta <- matrix(0, nrow(draws), n_obs)
  cols <- unlist(smcols[objects])
  if (length(cols) > 0L) {
    bs <- draw_columns(draws, paste0("bs", sfx, "[", cols, "]"))
    eta <- eta + tcrossprod(bs, unname(Xs[, cols, drop = FALSE]))
  }
  for (i in objects) {
    for (j in seq_len(sdata_entry(sdata, paste0("nb", sfx, "_", i)))) {
      stem <- paste0(sfx, "_", i, "_", j)
      Zs <- sdata_entry(sdata, paste0("Zs", stem))
      checkmate::assert_matrix(Zs, nrows = n_obs)
      s <- draw_columns(draws, paste0("s", stem, "[", seq_len(ncol(Zs)), "]"))
      eta <- eta + tcrossprod(s, unname(Zs))
    }
  }
  eta
}


#' One group-level block
#'
#' Each row of the fit's `ranef` table is one term of one grouping
#' factor. The term's correlation block `id` and its place `cn` in that
#' block name its data. Its design is `Z_<id><sfx>_<cn>`, its
#' coefficients are `r_<id><sfx>_<cn>[level]` and its grouping index
#' is `J_<id>` suffixed by the response alone.
#'
#' @inheritParams linear_terms_pred
#' @param block The rows of the fit's `ranef` table sharing one `id`
#' @return A `[ndraws x nobs]` matrix
#' @noRd
block_pred <- function(prep, draws, block, predictor) {
  if (identical(blank_if_na(block$gtype[1L]), "mm")) {
    stop(insight::format_error(c(
      "mvgam cannot predict from this group-level term.",
      x = paste0("Grouping factor '", block$group[1L],
                 "' is written as a multi-membership term."),
      i = "Write it as an ordinary '(term | group)'."
    )), call. = FALSE)
  }
  Reduce(`+`, lapply(seq_len(nrow(block)), function(k) {
    group_term_pred(prep, draws, block[k, , drop = FALSE], predictor)
  }))
}


#' One group-level term at the prediction's rows
#'
#' A special term's varying slope, `(mo(x) | g)`, has no design data.
#' `special_pred()` multiplies it by the term's value.
#'
#' @inheritParams linear_terms_pred
#' @param row One row of the fit's `ranef` table
#' @return A `[ndraws x nobs]` matrix
#' @noRd
group_term_pred <- function(prep, draws, row, predictor) {
  checkmate::assert_data_frame(row, nrows = 1L)
  stem <- paste0(row$id, predictor$suffix, "_", row$cn)
  Z <- if (identical(blank_if_na(row$type), "sp")) {
    1
  } else {
    as.vector(sdata_entry(prep$sdata, paste0("Z_", stem)))
  }
  J <- as.integer(sdata_entry(
    prep$sdata, paste0("J_", row$id, predictor_suffix(predictor$resp))
  ))
  group <- as.character(row$group)
  n_levels <- length(attr(prep$ranef, "levels")[[group]])
  if (length(J) > 0L && max(J) > n_levels) {
    stop(insight::format_error(c(
      paste0("Prediction for a grouping level the model never saw is ",
             "not supported."),
      x = paste0("Grouping factor '", group, "' was fitted with ",
                 n_levels, " level", if (n_levels != 1L) "s",
                 " and 'newdata' asks for level ", max(J), "."),
      i = paste0("Use 're_formula = NA' for the population effects or ",
                 "supply 'newdata' with fitted levels.")
    )), call. = FALSE)
  }
  r <- draw_columns(draws, paste0("r_", stem, "[", seq_len(n_levels), "]"))
  # A group-level intercept has a design of ones
  if (all(Z == 1)) {
    return(r[, J, drop = FALSE])
  }
  r[, J, drop = FALSE] * rep(Z, each = nrow(draws))
}


#' The group-level terms of one kind that one predictor holds
#'
#' The `type` column marks a term's kind. A category-specific term,
#' `(cs(1) | g)`, is `"cs"`. It shifts an ordinal response's thresholds,
#' outside the predictor. A special term's varying slope, `(mo(x) | g)`,
#' is `"sp"`. `special_pred()` adds it with the term. Every other term
#' is `""`.
#'
#' @inheritParams linear_terms_pred
#' @param type The kind of term
#' @return The rows of `prep$ranef` for that predictor, none under
#'   `re_formula = NA`
#' @noRd
group_level_rows <- function(prep, predictor, type = "") {
  checkmate::assert_choice(type, c("", "cs", "sp"))
  ranef <- prep$ranef
  # The Stan data under `re_formula = NA` holds no group-level terms
  if (NROW(ranef) == 0L ||
        checkmate::test_scalar_na(prep$re_formula)) {
    return(data.frame(id = integer(0L), group = character(0L)))
  }
  # The table leaves these blank for a model with one response and for
  # the mean, which it may also spell out as `mu`
  mean_as_blank <- function(x) {
    x <- blank_if_na(x)
    x[x == "mu"] <- ""
    x
  }
  ranef[blank_if_na(ranef$resp) == (predictor$resp %||% "") &
          mean_as_blank(ranef$dpar) == mean_as_blank(predictor$dpar %||% "") &
          blank_if_na(ranef$nlpar) == (predictor$nlpar %||% "") &
          blank_if_na(ranef$type) == type, , drop = FALSE]
}


#' The group-level blocks of one kind that one predictor holds
#'
#' @inheritParams group_level_rows
#' @return List of `ranef` row sets, one per block, named as
#'   `group_block_label()` writes the block
#' @noRd
group_level_blocks <- function(prep, predictor, type = "") {
  rows <- group_level_rows(prep, predictor, type)
  blocks <- split(rows, rows$id)
  # A block's label covers its terms of every kind
  labels <- vapply(names(blocks), function(id) {
    group_block_label(prep$ranef[prep$ranef$id == id, , drop = FALSE])
  }, character(1L))
  stats::setNames(blocks, labels)
}


#' A group-level block as the formula writes it
#'
#' Each kind of term in the block has its own one-sided formula `form`.
#' `(mo(x) | g)` holds `~1` for its ordinary terms and `~0 + mo(x)` for
#' its special ones. A block with ordinary terms lists its special
#' terms after them. An uncorrelated block, `(1 + x || g)`, has
#' `cor = FALSE`.
#'
#' @param block Every row of a fit's `ranef` table sharing one `id`
#' @return A label such as `"(1 + x | g)"`
#' @noRd
group_block_label <- function(block) {
  checkmate::assert_names(names(block),
                          must.include = c("group", "form", "cor", "type"))
  type <- blank_if_na(block$type)
  rhs <- function(kind) deparse1(block$form[[match(kind, type)]][[2L]])
  terms <- c(
    if ("" %in% type) rhs(""),
    if ("sp" %in% type) {
      if ("" %in% type) {
        formula_term_labels(block$form[[match("sp", type)]])
      } else {
        rhs("sp")
      }
    },
    if ("cs" %in% type) paste0("cs(", rhs("cs"), ")")
  )
  bar <- if (isTRUE(block$cor[1L])) " | " else " || "
  paste0("(", paste(terms, collapse = " + "), bar, block$group[1L], ")")
}


#' Each category-specific term's shift of an ordinal response's
#' thresholds
#'
#' A `cs()` term adds to each threshold's own predictor `mucs`.
#' Threshold `k` takes `Xcs %*% bcs[, k]` and every group-level `cs()`
#' coefficient written `<term>[k]`. The likelihood cuts the predictor
#' at each threshold of `thres - mucs`. The parts are each population
#' `cs()` term and each group-level `cs()` block.
#'
#' @inheritParams linear_terms_pred
#' @param resp The response's key on a model with several, or `NULL`
#' @return Named list of `[ndraws x n_obs x nthres]` arrays, empty
#'   without a category-specific term
#' @noRd
category_specific_parts <- function(prep, draws, resp, n_obs) {
  sdata <- prep$sdata
  sfx <- predictor_suffix(resp)
  predictor <- find_predictor(prep, resp)
  blocks <- group_level_blocks(prep, predictor, "cs")
  n_cs <- sdata[[paste0("Kcs", sfx)]] %||% 0L
  if (n_cs == 0L && length(blocks) == 0L) {
    return(list())
  }
  n_thres <- sdata_entry(sdata, paste0("nthres", sfx))
  empty <- array(0, c(nrow(draws), n_obs, n_thres))
  parts <- list()
  if (n_cs > 0L) {
    Xcs <- sdata_entry(sdata, paste0("Xcs", sfx))
    checkmate::assert_matrix(Xcs, nrows = n_obs, ncols = n_cs)
    columns <- fixed_term_columns(Xcs, predictor$pred[["cs"]])
    for (label in names(columns)) {
      cols <- columns[[label]]
      part <- empty
      for (k in seq_len(n_thres)) {
        b <- draw_columns(draws, paste0("bcs", sfx, "[", cols, ",", k, "]"))
        part[, , k] <- tcrossprod(b, unname(Xcs[, cols, drop = FALSE]))
      }
      parts[[paste0("cs(", label, ")")]] <- part
    }
  }
  for (label in names(blocks)) {
    block <- blocks[[label]]
    threshold <- cs_coef_frame(seq_len(nrow(block) %/% n_thres),
                               n_thres)$threshold
    if (length(threshold) != nrow(block) ||
          !all(endsWith(block$coef, paste0("[", threshold, "]")))) {
      stop_mvgam_fault(
        "A category-specific block must list each term's thresholds in turn.",
        paste0("Coefficients: ", paste(block$coef, collapse = ", "), ".")
      )
    }
    part <- empty
    for (i in seq_len(nrow(block))) {
      k <- threshold[[i]]
      part[, , k] <- part[, , k] +
        group_term_pred(prep, draws, block[i, , drop = FALSE], predictor)
    }
    parts[[label]] <- part
  }
  parts
}


#' The coefficients of a category-specific group-level block
#'
#' Each column of the block's design has one coefficient per threshold,
#' in turn: `Intercept[1]`, `Intercept[2]`, ..., `x[1]`, ...
#'
#' @param columns The design's columns, by name or by position
#' @param n_thres The response's number of thresholds
#' @return Data frame of each coefficient's column `var`, `threshold`
#'   and `name`, in the model's order
#' @noRd
cs_coef_frame <- function(columns, n_thres) {
  checkmate::assert_count(n_thres, positive = TRUE)
  out <- data.frame(var = rep(columns, each = n_thres),
                    threshold = rep(seq_len(n_thres), times = length(columns)))
  out$name <- paste0(out$var, "[", out$threshold, "]")
  out
}


#' The kernel of each `gp()` term of one predictor, in formula order
#'
#' The `i`-th `gp()` call of a predictor is the term whose draws are
#' `sdgp_<i>`. A term that sets no `cov` takes the default of
#' `brms::gp()`.
#'
#' @param gp_form The `gp` entry of the predictor's `brmsterms`, or
#'   `NULL` for a predictor without `gp()` terms
#' @return Character vector of kernel names, one per `gp()` term, named
#'   by the term as the formula writes it
#' @noRd
gp_kernels <- function(gp_form) {
  vapply(formula_term_calls(gp_form),
         function(call) gp_call_to_spec(call)$cov, character(1L))
}


#' One approximate Gaussian process term
#'
#' The term uses its basis `Xgp`, its eigenvalues `slambda`, its
#' marginal standard deviation `sdgp`, its length scales `lscale` and
#' its basis coefficients `zgp`, all under `<sfx>_<i>`. A term with a
#' factor `by` holds one of each per level, suffixed by the level. `Igp`
#' places that level's rows. `Jgp` maps rows onto the unique covariate
#' values the basis was computed from. `Cgp` scales a term by a
#' continuous `by`.
#'
#' @inheritParams smooth_pred
#' @param kernel The term's kernel, from `gp_kernels()`
#' @param i The term's place among the predictor's `gp()` terms
#' @return A `[ndraws x n_obs]` matrix
#' @noRd
gp_pred <- function(draws, sdata, sfx, kernel, i, n_obs) {
  stem <- paste0(sfx, "_", i)
  # One process per level of a factor `by`, one otherwise
  n_levels <- sdata_entry(sdata, paste0("Kgp", stem))
  if (n_levels == 1L) {
    return(gp_level_pred(draws, sdata, stem, 1L, stem, kernel))
  }
  eta <- matrix(0, nrow(draws), n_obs)
  for (g in seq_len(n_levels)) {
    rows <- sdata_entry(sdata, paste0("Igp", stem, "_", g))
    # A level absent from the prediction data has no rows to fill.
    if (length(rows) == 0L) {
      next
    }
    eta[, rows] <- gp_level_pred(draws, sdata, stem, g, paste0(stem, "_", g),
                                 kernel)
  }
  eta
}


#' One Gaussian process, or one level of a `by` factor, at its rows
#'
#' @inheritParams smooth_pred
#' @param stem The term's `<sfx>_<i>`
#' @param level The level's position in `sdgp` and `lscale`
#' @param data_stem The name its data and `zgp` coefficients carry:
#'   `stem`, or `<stem>_<level>` for a level of a `by` factor
#' @param kernel The term's kernel
#' @return A `[ndraws x nrow]` matrix, one column per row the term covers
#' @noRd
gp_level_pred <- function(draws, sdata, stem, level, data_stem, kernel) {
  Xgp <- sdata_entry(sdata, paste0("Xgp", data_stem))
  lscale_cols <- colnames(draws)[startsWith(
    colnames(draws), paste0("lscale", stem, "[", level, ",")
  )]
  out <- approx_gp_pred(
    Xgp = Xgp,
    slambda = sdata_entry(sdata, paste0("slambda", data_stem)),
    zgp = draw_columns(draws, paste0("zgp", data_stem, "[",
                                     seq_len(ncol(Xgp)), "]")),
    sdgp = draw_columns(draws, paste0("sdgp", stem, "[", level, "]"))[, 1L],
    lscale = draw_columns(draws, lscale_cols),
    kernel = kernel
  )
  Jgp <- sdata[[paste0("Jgp", data_stem)]]
  if (!is.null(Jgp)) {
    out <- out[, as.integer(Jgp), drop = FALSE]
  }
  Cgp <- sdata[[paste0("Cgp", data_stem)]]
  if (!is.null(Cgp)) {
    checkmate::assert_numeric(as.vector(Cgp), len = ncol(out))
    out <- out * matrix(Cgp, nrow(out), ncol(out), byrow = TRUE)
  }
  out
}


#' The coefficients of each special formula term
#'
#' @param special A predictor's `special_term_frame()`
#' @return Named list of row positions in `special`, one entry per
#'   formula term in formula order
#' @noRd
special_term_rows <- function(special) {
  if (NROW(special) == 0L) return(list())
  split(seq_len(nrow(special)),
        factor(special$label, levels = unique(special$label)))
}


#' One special formula term
#'
#' Coefficient `j` of the special terms adds `(bsp[j] + r[J]) * value_j`.
#' `value_j` is the column's expression in its `mo()`, `me()` and `mi()`
#' calls, times the covariate `Csp` multiplying it, as in `mo(x):z`.
#' `r` is a varying slope of the term, `(mo(x) | g)`. The other terms of
#' its block belong to that block.
#'
#' @inheritParams linear_terms_pred
#' @param rows The term's coefficients, rows of `predictor$special`
#' @return A `[ndraws x n_obs]` matrix
#' @noRd
special_pred <- function(prep, draws, predictor, rows, n_obs) {
  special <- predictor$special
  slopes <- group_level_rows(prep, predictor, "sp")
  eta <- matrix(0, nrow(draws), n_obs)
  for (j in rows) {
    coef <- draw_columns(draws, paste0("bsp", predictor$suffix, "[", j,
                                       "]"))[, 1L]
    for (k in which(slopes$coef == special$coef[j])) {
      coef <- coef + group_term_pred(prep, draws, slopes[k, , drop = FALSE],
                                     predictor)
    }
    eta <- eta + coef * special_value(prep, draws, predictor, j, n_obs)
  }
  eta
}


#' The value one special coefficient multiplies
#'
#' Each `mo()`, `me()` and `mi()` call of the column's expression takes
#' its `[ndraws x n_obs]` values. The expression is then evaluated cell
#' by cell.
#'
#' @inheritParams special_pred
#' @param j The coefficient's row of `predictor$special`
#' @return A `[ndraws x n_obs]` matrix
#' @noRd
special_value <- function(prep, draws, predictor, j, n_obs) {
  special <- predictor$special
  sdata <- prep$sdata
  sfx <- predictor$suffix
  expr <- str2lang(special$joint_call[[j]])
  calls <- c(special$calls_mo[[j]], special$calls_me[[j]],
             special$calls_mi[[j]])
  values <- c(
    lapply(special$Imo[[j]], function(m) {
      mo_value(draws, sdata, sfx, m, n_obs)
    }),
    lapply(match(special$calls_me[[j]], prep$me$term), function(k) {
      me_value(prep, draws, k)
    }),
    Map(function(var, idx) mi_value(prep, draws, var, idx, sfx),
        special$vars_mi[[j]], special$idx2_mi[[j]])
  )
  names(values) <- paste0(".sp", seq_along(values))
  value <- eval(replace_calls(expr, stats::setNames(names(values), calls)),
                values, baseenv())
  if (special$Ic[[j]] > 0L) {
    csp <- sdata_entry(sdata, paste0("Csp", sfx, "_", special$Ic[[j]]))
    value <- value * data2draws(as.numeric(csp), c(nrow(draws), n_obs))
  }
  value
}


#' Sub-calls of an expression replaced by names
#'
#' @param expr A parsed expression
#' @param map Named character vector: each call as `deparse1()` writes
#'   it, mapped to the name that replaces it
#' @return `expr` with each mapped call replaced
#' @noRd
replace_calls <- function(expr, map) {
  if (!is.call(expr)) return(expr)
  key <- deparse1(expr)
  if (key %in% names(map)) return(as.name(map[[key]]))
  for (i in seq_along(expr)[-1L]) {
    expr[[i]] <- replace_calls(expr[[i]], map)
  }
  expr
}


#' A monotonic variable's share of its full effect at each row
#'
#' `mo(x)` is `D * sum(simo[1:x])` at the ordered level `x` of a variable
#' with `D + 1` levels. `Xmo` and `simo` number the monotonic variables
#' of a predictor.
#'
#' @inheritParams fixed_design
#' @param draws Plain `[ndraws x npar]` matrix of the posterior
#' @param m The variable's number
#' @return A `[ndraws x n_obs]` matrix
#' @noRd
mo_value <- function(draws, sdata, sfx, m, n_obs) {
  stem <- paste0(sfx, "_", m)
  D <- sdata_entry(sdata, paste0("Jmo", sfx))[[m]]
  simo <- draw_columns(draws, paste0("simo", stem, "[", seq_len(D), "]"))
  x <- validate_monotonic_indices(
    sdata_entry(sdata, paste0("Xmo", stem)), paste0("Xmo", stem), D, n_obs
  )
  # The share of the full effect reached at each level, 0 at the first
  cumulative <- matrix(0, nrow(simo), D + 1L)
  for (k in seq_len(D)) {
    cumulative[, k + 1L] <- cumulative[, k] + simo[, k]
  }
  D * cumulative[, x + 1L, drop = FALSE]
}


#' Draws of a latent quantity at each row of a prediction
#'
#' The fit samples some quantities per fitted row or per level, such as
#' the noise-free value of an `me()` covariate. It keeps those draws
#' under `save_pars(latent = TRUE)`. A prediction row repeating a
#' fitted row takes the fit's draws. Every other row takes fresh
#' values.
#'
#' @param draws Plain `[ndraws x npar]` matrix of the posterior
#' @param name The quantity's parameter name, such as `Xme_1`
#' @param at Integer vector along the rows: the quantity's entry each
#'   repeated row takes, `NA` for a new row
#' @param fresh Function of no arguments returning fresh values, a
#'   `[ndraws x length(at)]` matrix
#' @return A `[ndraws x length(at)]` matrix
#' @noRd
latent_draws <- function(draws, name, at, fresh) {
  checkmate::assert_integerish(at, lower = 1L)
  checkmate::assert_function(fresh)
  seen <- !is.na(at)
  latent <- param_draw_columns(colnames(draws), name)
  if (any(seen) && length(latent) == 0L) {
    warn_once(c(
      paste0("Predictions at the fitted rows draw the latent values of ",
             "'me()' and 'mi()' terms afresh."),
      i = paste0("Refit with 'save_pars = brms::save_pars(latent = TRUE)' ",
                 "to use the fit's own draws.")
    ), "mvgam_latents_fresh")
    seen[] <- FALSE
  }
  out <- if (all(seen)) matrix(0, nrow(draws), length(at)) else fresh()
  if (any(seen)) {
    out[, seen] <- draw_columns(draws, latent)[, at[seen], drop = FALSE]
  }
  out
}


#' The noise-free values of one `me()` covariate at each row
#'
#' The fit samples the noise-free value `Xme_<k>` of each row it was
#' given, or of each level of a grouping factor `gr`, indexed by
#' `Jme`. A new row draws its value from the observed `Xn_<k>` with
#' its known SD `noise_<k>`.
#'
#' @inheritParams special_value
#' @param k The term's number among the model's `me()` terms
#' @return A `[ndraws x nrow]` matrix
#' @noRd
me_value <- function(prep, draws, k) {
  checkmate::assert_count(k, positive = TRUE)
  sdata <- prep$sdata
  group <- prep$me$grname[[k]]
  J <- if (nzchar(group)) {
    groups <- unique(prep$me$grname[nzchar(prep$me$grname)])
    as.integer(sdata_entry(sdata, paste0("Jme_", match(group, groups))))
  }
  # A grouped value belongs to the row's level, which the prediction
  # numbers as the fit does
  at <- if (is.null(J)) prep$fitted_row else {
    replace(J, is.na(prep$fitted_row), NA_integer_)
  }
  latent_draws(draws, paste0("Xme_", k), at, function() {
    Xn <- as.numeric(sdata_entry(sdata, paste0("Xn_", k)))
    dims <- c(nrow(draws), length(Xn))
    noise <- as.numeric(sdata_entry(sdata, paste0("noise_", k)))
    X <- matrix(stats::rnorm(prod(dims), data2draws(Xn, dims),
                             data2draws(noise, dims)), dims[1L])
    # Fresh values follow the levels of the prediction's own data
    if (is.null(J)) X else X[, as.integer(factor(J)), drop = FALSE]
  })
}


#' The values one `mi()` variable takes at each of its rows
#'
#' The variable is the response of a predictor of its own. Its observed
#' values fill `Yl`. A row repeating a fitted row whose value was
#' missing takes that value's imputed draw `Ymi`. A variable observed
#' with noise samples every value, `Yl`. A new row draws it from the
#' observed value with its noise, within the variable's bounds.
#' `mi(x, idx)` takes the rows `idxl` names.
#'
#' @inheritParams special_value
#' @param var The key of the variable's response
#' @param idx The index `mi(x, idx)` takes, or `NA` for none
#' @return A `[ndraws x n]` matrix, one column per row of the variable
#' @noRd
mi_value <- function(prep, draws, var, idx, sfx) {
  sdata <- prep$sdata
  Y <- as.numeric(sdata_entry(sdata, paste0("Y_", var)))
  dims <- c(nrow(draws), length(Y))
  noise <- sdata[[paste0("noise_", var)]]
  column <- form_response_column(response_formulas(prep$formula)[[var]])
  at <- prep$fitted_row
  if (is.null(noise)) {
    new_gaps <- which(is.na(prep$newdata[[column]]) & is.na(at))
    if (length(new_gaps) > 0L) {
      stop(insight::format_error(c(
        paste0("Prediction at a new row needs a value of '", column, "'."),
        x = paste0("Missing in 'newdata' rows ",
                   paste(utils::head(new_gaps, 5L), collapse = ", "),
                   if (length(new_gaps) > 5L) ", ...", "."),
        i = "The imputed draws cover the fitted rows alone."
      )), call. = FALSE)
    }
    Yl <- data2draws(Y, dims)
    # A repeated fitted row whose value was missing takes its draw
    k <- match(at, which(is.na(prep$data[[column]])))
    take <- !is.na(k)
    if (any(take)) {
      Yl[, take] <- draw_columns(draws, paste0("Ymi_", var, "[", k[take],
                                               "]"))
    }
  } else {
    noise <- as.numeric(noise)
    Yl <- latent_draws(draws, paste0("Yl_", var), at, function() {
      fresh <- function() {
        stats::rnorm(prod(dims), data2draws(Y, dims),
                     data2draws(noise, dims))
      }
      apply_truncation(
        matrix(fresh(), dims[1L]), sdata[[paste0("lbmi_", var)]],
        sdata[[paste0("ubmi_", var)]], ntrys = 1L, ndraws = dims[1L],
        nobs = dims[2L], redraw = fresh,
        spec = list(dist = "norm", args = function(i) {
          list(mean = Y[[i]], sd = noise[[i]])
        })
      )
    })
  }
  if (is.na(idx)) return(Yl)
  rows <- sdata_entry(sdata, paste0("idxl", sfx, "_", var, "_", idx))
  Yl[, rows, drop = FALSE]
}


#' A non-linear mean, evaluated at its non-linear parameters
#'
#' The mean is the formula's expression. Each name in it that has a
#' formula of its own is a non-linear parameter, evaluated as the
#' linear predictor that formula describes. Every other name is a
#' covariate. The Stan data holds the covariates as `C_<i>` in the
#' order the expression uses them.
#'
#' @inheritParams linear_terms_pred
#' @param form The response's `brmsformula`
#' @return A `[ndraws x n_obs]` matrix
#' @noRd
nonlinear_linpred <- function(prep, draws, form, resp, n_obs) {
  expr <- form$formula[[3L]]
  nlpars <- nonlinear_parameters(form)
  covariates <- setdiff(all.vars(expr), nlpars)
  env <- lapply(stats::setNames(nm = nlpars), function(p) {
    linear_terms_pred(prep, draws, find_predictor(prep, resp, nlpar = p),
                      n_obs)
  })
  for (i in seq_along(covariates)) {
    values <- sdata_entry(prep$sdata,
                          paste0("C", predictor_suffix(resp), "_", i))
    checkmate::assert_numeric(as.vector(values), len = n_obs)
    env[[covariates[i]]] <- data2draws(as.numeric(values),
                                       c(nrow(draws), n_obs))
  }
  mu <- eval(expr, envir = env)
  if (!is.matrix(mu) || !identical(dim(mu), c(nrow(draws), n_obs))) {
    stop(insight::format_error(c(
      "The non-linear formula did not give one value per draw and row.",
      x = paste0("Got ", paste(dim(as.matrix(mu)), collapse = " x "),
                 ", expected ", nrow(draws), " x ", n_obs, "."),
      i = "Write the expression with element-wise operators."
    )), call. = FALSE)
  }
  mu
}
