# Shared utilities for multivariate (mvbind / mvbrmsformula) fits.
# Centralises three patterns shared across
# residuals.mvgam / pp_check.mvgam / plot.mvgam /
# conditional_effects.mvgam / hindcast.mvgam / methods_md:
#
#   1. which responses a model has, keyed as brms keys them, and
#      the column each is read from
#   2. detection of `set_rescor(TRUE)`
#   3. per-response fan-out: when a method receives no `resp`
#      arg on an mv fit, re-enter the method once per response
#      and return a named list


#' Return the first mvgam_trend spec on a fitted (or prefit)
#' mvgam object. `mv_spec$trend_specs` is either a single
#' `mvgam_trend` (univariate) or a list of them (multivariate);
#' callers that only need to read one slot (loadings prior,
#' gr / subgr, trend type) take the first.
#'
#' Used by `methods_md()` model-section renderers and
#' `residual_cor()` extractors; both want a single per-fit spec.
#'
#' @noRd
first_trend_spec <- function(object) {
  ts <- object$mv_spec$trend_specs
  if (is.null(ts)) return(NULL)
  if (inherits(ts, "mvgam_trend")) ts else ts[[1L]]
}

#' The responses a model reads, keyed as brms keys them
#'
#' brms keys a response by its column with every `.` and `_` taken
#' out, so the column `my_count` is the response `mycount`. The key is
#' what `resp` takes, what `forms` is indexed by and what every
#' per-response parameter and data array is suffixed with. Reading the
#' frame takes the column. Code that took one spelling for the other
#' looked for a column that was not there, which refused every trend
#' model whose response carried an underscore or a dot.
#'
#' This is the one reader of which responses a model has. A fit, a
#' prefit and a bare observation formula are all answered from the
#' formula, so no stored copy can drift from the model it describes.
#'
#' @param x A fitted `mvgam`, a prefit, its summary, or an observation
#'   formula in any spelling `mvgam()` accepts
#' @return Character vector of response columns named by key, in
#'   formula order. An addition term such as `trials()` qualifies a
#'   response rather than being one, so it is left out.
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
      "The observation formula names no response.",
      i = "Write it as 'response ~ predictors'."
    )), call. = FALSE)
  }
  stats::setNames(forms, keys)
}

#' The family of each response
#'
#' A family written inside a response's `bf()` belongs to that
#' response. The family given beside the formula applies to every
#' response that names none, which is how brms reads the pair.
#'
#' @inheritParams response_columns
#' @param family The family given beside the formula
#' @return A list of family objects named by response key
#' @noRd
formula_families <- function(x, family) {
  lapply(response_formulas(x), function(form) form$family %||% family)
}

#' The family of one response of a model, or of each
#'
#' A model written with `brms::mvbf()` gives each response its own
#' family, and no single family describes it. Asked without `resp`,
#' a model with several responses answers with one family per
#' response, named by brms's key for it; otherwise it answers with
#' one family.
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
#' Every method taking `resp` asks the same two questions of it: is it
#' one of the model's responses, and can the method answer without
#' one. They are answered here so the refusal reads the same wherever
#' it is met.
#'
#' @param x A fitted `mvgam`, or anything else `response_columns()`
#'   reads
#' @param resp The response a caller named, or `NULL`
#' @param required Whether the caller answers for one response at a
#'   time, so a model with several needs to be told which
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
        "Name a response with 'resp': this model has several.",
        x = if (!is.null(caller)) {
          paste0("'", caller, "' answers for one response at a time.")
        },
        i = listed
      )), call. = FALSE)
    }
    return(invisible(NULL))
  }
  checkmate::assert_string(resp)
  if (!resp %in% keys) {
    # A column name is the natural thing to type, and brms answers to
    # the key alone, so the refusal says which key the column became.
    as_key <- keys[match(resp, columns)]
    stop(insight::format_error(c(
      paste0("'", resp, "' is not a response of this model."),
      x = if (!is.na(as_key)) {
        paste0("brms names the column '", resp, "' as '", as_key,
               "', removing every '.' and '_'.")
      },
      i = listed
    )), call. = FALSE)
  }
  invisible(resp)
}

#' The column one response is read from
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
  # table to rows scoped to response `r` (including rows with no
  # `resp` set, which are shared across responses), and narrows the
  # formula and the family to that response's own. Downstream
  # extractors and renderers reading any of them then see the
  # single-response view without per-helper threading.
  out <- obj
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


#' @noRd
make_row_prefix <- function(nlpar, dpar, resp) {
  # Per-row routing prefix for brms parameter aliasing. brms's
  # stancode writes per-row aliases using whichever of these is
  # set, in priority order: nlpar > dpar > resp. Returns "" when
  # none is set (the univariate, no-dpar, no-nlpar case).
  # Centralised so the ifelse ladder lives in one place, shared by
  # mvgam_ranef_aliases / ranef.mvgam / VarCorr.mvgam.
  ifelse(
    !is.na(nlpar) & nzchar(nlpar), nlpar,
    ifelse(
      !is.na(dpar) & nzchar(dpar), dpar,
      ifelse(
        !is.na(resp) & nzchar(resp), resp,
        ""
      )
    )
  )
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
  # `class` is the class the caller's single-response answer has.
  # Given it, the list takes that class too, so a method dispatches
  # on the wrapper as it would on one answer, and records two facts a
  # reader would otherwise have to infer from its shape: that it is a
  # wrapper, and whether its responses are the fit's series. On a
  # wide frame they are, so the elements together are one answer over
  # the whole axis; on a long frame each element spans every series.
  #
  # `combine`, where given, turns the named list into the one object
  # the caller returns, which is how a plotting method hands back one
  # figure rather than a list that prints as a listing.
  #
  # Implementation: capture the caller's matched call via
  # `match.call(sys.function(-1L), sys.call(-1L))`, swap `resp`
  # per iteration, and eval in the caller's parent frame so any
  # symbolic arguments resolve in the user's environment.
  #
  # Assumes direct S3 method invocation (`residuals(fit)` etc.).
  # Does not support `do.call(method.mvgam, ...)` or S4 dispatch,
  # which insert intermediate frames and shift the -1L / -2L
  # offsets. Every caller in mvgam today uses direct dispatch, so
  # this is safe; revisit if a future caller wraps the method via
  # do.call or a magrittr pipe and the response loop ends up
  # iterating against an unexpected frame.
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
#' class through subsetting and arithmetic, so anything computed from
#' it carries the class too. A prediction whose class depends on
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


#' Is this parameter from the trend model?
#'
#' The trend side of a model names its parameters with a `_trend`
#' suffix, either at the end or before the index, so `sigma_trend[1]`
#' and `b_x_trend` are trend parameters. Asking whether the name merely
#' contains `_trend` reads a covariate as structure: a user with a
#' column called `pre_trend_score` gets `b_pre_trend_score`, which is
#' an observation-side coefficient and belongs in the observation
#' block. Every trend parameter the package emits ends the suffix, so
#' the suffix is what is tested.
#'
#' The `_trend\\[` alternative is why this differs from a bare
#' `grepl("_trend$", ...)`. A posterior draw carries an index, so
#' `sigma_trend[1]` has to match; a prior class never does, which is
#' why the prior tables test the suffix on its own rather than calling
#' here.
#'
#' @param pars Character vector of parameter names
#' @return Logical vector
#'
#' @noRd
is_trend_parameter <- function(pars) {
  grepl("_trend$|_trend\\[", pars)
}


#' Is this parameter an autoregressive coefficient?
#'
#' An `AR(p)` trend emits one coefficient per lag, `ar1_trend` through
#' `ar<p>_trend`. The test was written in two spellings across five
#' places, which is one fact and five chances to disagree about it.
#' Note the hierarchical mean and standard deviation of a coefficient,
#' `mu_ar1_trend` and `sigma_ar1_trend`, are different parameters and
#' are deliberately excluded by the anchor.
#'
#' @param pars Character vector of parameter names
#' @return Logical vector
#'
#' @noRd
is_ar_coefficient <- function(pars) {
  grepl("^ar[0-9]+_trend$", pars)
}
