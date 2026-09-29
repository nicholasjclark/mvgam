# Shared utilities for multivariate (mvbind / mvbrmsformula) fits,
# used by residuals.mvgam / pp_check.mvgam / plot.mvgam /
# conditional_effects.mvgam / hindcast.mvgam / methods_md:
#
#   1. which responses a model has, keyed as brms keys them, and
#      the data column of each
#   2. detection of `set_rescor(TRUE)`
#   3. per-response fan-out: when a method receives no `resp`
#      arg on an mv fit, re-enter the method once per response
#      and return a named list


#' Return the first mvgam_trend spec on a fitted (or prefit)
#' mvgam object. `mv_spec$trend_specs` is either a single
#' `mvgam_trend` (univariate) or a list of them (multivariate).
#' Callers that need one slot (loadings prior, gr / subgr, trend
#' type) take the first.
#'
#' Used by `methods_md()` model-section renderers and
#' `residual_cor()` extractors; both want a single per-fit spec.
#'
#' @noRd
first_trend_spec <- function(object) {
  # `trend_spec_head()` requires the entries to be lists before it
  # takes the first. An unclassed specification written without a
  # `trend` field otherwise gives up its first element, which is a
  # column name where a specification is wanted.
  trend_spec_head(object$mv_spec$trend_specs)
}

#' The responses of a model, keyed as brms keys them
#'
#' brms keys a response by its column with every `.` and `_` removed:
#' the column `my_count` is the response `mycount`. `resp` takes the
#' key, `forms` is indexed by it, and every per-response parameter and
#' data array takes it as a suffix. A lookup in the data frame takes
#' the column. Code that used one spelling for the other looked for a
#' column that did not exist, and refused every trend model whose
#' response name contained an underscore or a dot.
#'
#' Every lookup of a model's responses goes through this function. It
#' derives them from the formula for a fit, a prefit and a bare
#' observation formula alike, and no stored copy can drift from the
#' model.
#'
#' @param x A fitted `mvgam`, a prefit, its summary, or an observation
#'   formula in any spelling `mvgam()` accepts
#' @return Character vector of response columns named by key, in
#'   formula order. An addition term such as `trials()` qualifies a
#'   response and is left out.
#' @noRd
response_columns <- function(x) {
  vapply(response_formulas(x), function(form) {
    all.vars(strip_addition_terms(form$formula)[[2L]])[1L]
  }, character(1L))
}

#' Each response's own formula, keyed as brms keys it
#'
#' A multivariate formula holds one `bf()` per response in `$forms`,
#' and any other formula is its own single response. The key is the
#' one `response_columns()` describes.
#'
#' @inheritParams response_columns
#' @return A list of `brmsformula` objects named by key, in formula
#'   order
#' @noRd
response_formulas <- function(x) {
  f <- if (inherits(x, c("formula", "bform"))) x else x$formula
  if (!inherits(f, "bform")) {
    f <- brms::bf(f)
  }
  forms <- if (inherits(f, "mvbrmsformula")) f$forms else list(f)
  keys <- vapply(forms, function(form) {
    form$resp %||% NA_character_
  }, character(1L), USE.NAMES = FALSE)
  if (anyNA(keys)) {
    stop(insight::format_error(c(
      "The observation formula needs a response.",
      i = "Write it as 'response ~ predictors'."
    )), call. = FALSE)
  }
  stats::setNames(forms, keys)
}

#' The family of each response
#'
#' A family written inside a response's `bf()` belongs to that
#' response. The family passed with the formula applies to every
#' response that names none, as brms applies it.
#'
#' @inheritParams response_columns
#' @param family The family passed with the formula
#' @return A list of family objects named by response key
#' @noRd
formula_families <- function(x, family) {
  lapply(response_formulas(x), function(form) form$family %||% family)
}

#' The family of one response of a model, or of each
#'
#' A model written with `brms::mvbf()` gives each response its own
#' family, and no single family describes it. Without `resp`, a model
#' with several responses returns one family per response, named by
#' brms's key for it. Otherwise the function returns one family.
#'
#' @param object A fitted `mvgam`, a prefit or its summary.
#' @param resp One response's key, or `NULL`.
#' @return A family object, or a list of them named by response.
#' @noRd
model_families <- function(object, resp = NULL) {
  resolve_resp(object, resp)
  families <- formula_families(object, object$family)
  if (!is.null(resp)) return(families[[resp]])
  if (length(families) == 1L) families[[1L]] else families
}

#' Check the response a caller named against the model's own
#'
#' Every method taking `resp` checks two things: that it names one of
#' the model's responses, and whether the method needs one. This
#' function checks both, and every method raises the same refusal.
#'
#' @param x A fitted `mvgam`, or anything else `response_columns()`
#'   accepts
#' @param resp The response a caller named, or `NULL`
#' @param required Whether the caller returns one response at a time.
#'   A model with several then needs `resp`.
#' @param caller Name of the method asking, used in the refusal
#' @return `resp`, unchanged, invisibly
#' @noRd
resolve_resp <- function(x, resp, required = FALSE, caller = NULL) {
  columns <- response_columns(x)
  keys <- names(columns)
  listed <- paste0("Its responses are ",
                   paste0("'", keys, "'", collapse = ", "), ".")
  if (is.null(resp)) {
    if (required && length(keys) > 1L) {
      stop(insight::format_error(c(
        "Choose a response with 'resp': this model has several.",
        x = if (!is.null(caller)) {
          paste0("'", caller, "' returns one response at a time.")
        },
        i = listed
      )), call. = FALSE)
    }
    return(invisible(NULL))
  }
  checkmate::assert_string(resp)
  if (!resp %in% keys) {
    # Users type a column name, and brms accepts the key alone. The
    # refusal names the key the column became.
    as_key <- keys[match(resp, columns)]
    stop(insight::format_error(c(
      "'resp' must be a response of this model.",
      x = paste0("Got '", resp, "'."),
      x = if (!is.na(as_key)) {
        paste0("brms renames the column '", resp, "' to '", as_key,
               "', removing every '.' and '_'.")
      },
      i = listed
    )), call. = FALSE)
  }
  invisible(resp)
}

#' The data column of one response
#'
#' @param object A fitted `mvgam` object
#' @param resp The response's key, or `NULL` on a model with one
#' @return A single column name
#' @noRd
response_column <- function(object, resp = NULL) {
  resolve_resp(object, resp, required = TRUE)
  columns <- response_columns(object)
  unname(columns[[resp %||% 1L]])
}

#' The suffix brms gives one response's parameters
#'
#' A model with several responses names each response's parameters
#' with its key appended, as in `sigma_count` or
#' `b_count_Intercept`. A model with one response appends nothing,
#' whichever `resp` a caller passed. The rule is `predictor_suffix()`'s.
#'
#' @param object A fitted `mvgam` object
#' @param resp The response's key, or `NULL`
#' @return `""`, or `"_<resp>"`
#' @noRd
response_suffix <- function(object, resp = NULL) {
  several <- length(response_formulas(object)) > 1L
  predictor_suffix(resp = if (several) resp)
}


#' @noRd
subset_obj_to_response <- function(obj, r) {
  # Per-response slice of a multi-response fit. Filters the prior
  # table to rows scoped to response `r` and to rows with no `resp`,
  # which all responses share. Narrows the formula and the family to
  # that response's own. Downstream extractors and renderers then work
  # on the single-response view with no argument of their own for it.
  out <- obj
  out$methods_md_resp <- r
  out$family <- model_families(obj, r)
  out$formula <- obj$formula$forms[[r]]
  prior <- obj$prior
  if (!is.null(prior) && nrow(prior) > 0L) {
    rsp <- prior$resp %||% rep("", nrow(prior))
    keep <- rsp == r | !nzchar(rsp)
    out$prior <- prior[keep, , drop = FALSE]
  }
  out
}


#' @noRd
has_rescor <- function(obj) {
  # TRUE when the user set `set_rescor(TRUE)` on a multivariate
  # brms formula. Prefer the formula attribute (authoritative);
  # fall back to detecting brms's `Lrescor` row in the prior
  # table (post-fit objects with stripped formula metadata).
  f <- obj$formula
  if (inherits(f, "mvbrmsformula") && isTRUE(f$rescor)) {
    return(TRUE)
  }
  prior <- obj$prior
  if (!is.null(prior) && nrow(prior) > 0L) {
    return(any(prior$class == "Lrescor"))
  }
  FALSE
}


#' The prefix brms gives each row of a group-level table
#'
#' brms names a group-level parameter by the predictor it enters, as
#' `brms:::combine_prefix()` joins the distributional parameter, the
#' response and the non-linear parameter: `sd_g__sigma_y1_Intercept`
#' and `r_g__sigma_y1[a,Intercept]` for `sigma` of the response `y1`.
#' That is the suffix `predictor_suffix()` gives, without its leading
#' underscore. `mvgam_ranef_aliases()`, `ranef.mvgam()` and
#' `VarCorr.mvgam()` all name their rows with it.
#'
#' @param nlpar,dpar,resp The table's columns of the same names
#' @return Character vector, one prefix per row, `""` for the mean of a
#'   model with one response
#' @noRd
make_row_prefix <- function(nlpar, dpar, resp) {
  vapply(seq_along(dpar), function(i) {
    sub("^_", "", predictor_suffix(
      resp = null_if_blank(resp[i]), dpar = null_if_blank(dpar[i]),
      nlpar = null_if_blank(nlpar[i])
    ))
  }, character(1L))
}


#' @noRd
mv_resp_fan_out <- function(object, resp, class = NULL, combine = NULL) {
  # Per-response fan-out for multivariate user-facing methods.
  # When `resp` is NULL on an mv fit, re-invoke the calling
  # function once per response with `resp = r` and return a named
  # list; otherwise return NULL so the caller can fall through to
  # its univariate body. Centralises the pattern shared by
  # residuals.mvgam, pp_check.mvgam, conditional_effects.mvgam,
  # mvgam_resid_panel, and hindcast.mvgam.
  #
  # `class` is the class of the caller's single-response result.
  # Given it, the list takes that class too, and a method dispatches
  # on the wrapper as it would on one result. Two attributes record
  # what the shape alone does not show: that the object is a wrapper,
  # and whether its responses are the fit's series. On a wide frame
  # they are, and the elements together cover the whole series axis.
  # On a long frame each element spans every series.
  #
  # `combine`, where given, turns the named list into the one object
  # the caller returns. A plotting method uses it to return one
  # figure.
  #
  # Implementation: capture the caller's matched call via
  # `match.call(sys.function(-1L), sys.call(-1L))`, swap `resp`
  # per iteration, and eval in the caller's parent frame so any
  # symbolic arguments resolve in the user's environment.
  #
  # Requires direct S3 method invocation (`residuals(fit)` etc.).
  # `do.call(method.mvgam, ...)` and S4 dispatch insert intermediate
  # frames and shift the -1L / -2L offsets. Every caller in mvgam
  # dispatches directly.
  if (!is.null(resp) ||
        !inherits(object$formula, "mvbrmsformula")) {
    return(NULL)
  }
  parent_fn <- sys.function(-1L)
  parent_call <- match.call(definition = parent_fn,
                              call = sys.call(-1L))
  # When the user called via an S3 generic (e.g. residuals(mod) ->
  # residuals.mvgam(mod)), `parent_call[[1L]]` is the .mvgam method
  # symbol, which isn't visible from the caller's frame. Strip
  # the ".mvgam" suffix so the recursive call goes back through
  # the generic.
  fn_sym <- parent_call[[1L]]
  if (is.symbol(fn_sym)) {
    fn_name <- as.character(fn_sym)
    bare_name <- sub("\\.mvgam$", "", fn_name)
    if (bare_name != fn_name) {
      parent_call[[1L]] <- as.name(bare_name)
    }
  }
  responses <- names(response_columns(object))
  eval_env <- parent.frame(n = 2L)
  out <- lapply(responses, function(r) {
    call_r <- parent_call
    call_r$resp <- r
    eval(call_r, envir = eval_env)
  })
  names(out) <- responses
  if (!is.null(combine)) {
    return(combine(out))
  }
  if (!is.null(class)) {
    class(out) <- class
    attr(out, "mv_wrapper") <- TRUE
    attr(out, "response_keyed") <- is_response_keyed(object)
  }
  out
}


#' Drop the posterior draws classes from a matrix
#'
#' `posterior::as_draws_matrix()` returns an object that keeps its
#' class through subsetting and arithmetic, and anything computed from
#' it inherits the class. A prediction whose class depends on
#' whether the model had random effects is a surprise on its own, and
#' S4 slots that accept a plain matrix reject it outright. Values,
#' dimensions and dimnames are untouched.
#'
#' @param x A matrix, possibly of class `draws_matrix`
#' @return The same matrix with the draws classes removed
#'
#' @noRd
as_plain_matrix <- function(x) {
  if (!inherits(x, "draws")) {
    return(x)
  }
  attr(x, "nchains") <- NULL
  class(x) <- setdiff(class(x), c("draws_matrix", "draws"))
  x
}
