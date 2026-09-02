# Shared utilities for multivariate (mvbind / mvbrmsformula) fits.
# Centralises three patterns shared across
# residuals.mvgam / pp_check.mvgam / plot.mvgam /
# conditional_effects.mvgam / hindcast.mvgam / methods_md:
#
#   1. response-name extraction (with prior-table fallback for
#      prefits where response_names is NULL)
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

#' Name the response a univariate scoring method works on
#'
#' Scoring methods that reduce a fit to one number, such as
#' `bayes_R2()` and `loo_R2()`, need the response to read observed
#' values from. A multivariate fit has to be told which one; a
#' univariate fit has exactly one, though a binomial fit also carries
#' its denominator in the same slot, so the first entry is the
#' response and the rest are addition terms.
#'
#' Reading the slot directly is what broke these methods on fits with
#' no trend formula, where it was left empty. `get_response_names()`
#' falls back to the prior table and then to the formula, so it
#' answers whatever the slot holds.
#'
#' @param object An `mvgam` model object
#' @param resp Response the caller asked for, or `NULL`
#' @return A single response name
#'
#' @noRd
scored_response_name <- function(object, resp = NULL) {
  if (!is.null(resp) && nzchar(resp)) {
    return(resp)
  }
  rn <- get_response_names(object)
  if (length(rn) == 0L) {
    stop(insight::format_error(c(
      "Cannot determine which response to score.",
      i = paste0(
        "Name it with 'resp', or refit the model so its response is ",
        "recorded."
      )
    )))
  }
  rn[1L]
}



#' @noRd
get_response_names <- function(obj) {
  # Three-tier fallback for multi-response detection:
  #   * obj$response_names if the fit has populated it (post-fit
  #     mvgam objects)
  #   * unique non-empty `resp` values on the prior table (prefits
  #     can leave response_names NULL but the prior table is
  #     already populated)
  #   * canonical formula-LHS triage via brms's
  #     extract_response_names (handles brmsformula /
  #     mvbrmsformula / mvbind triage in one place)
  rn <- obj$response_names
  if (length(rn) > 0L) return(rn)
  prior <- obj$prior
  if (!is.null(prior) && nrow(prior) > 0L) {
    rsp <- prior$resp %||% rep("", nrow(prior))
    have <- unique(rsp[nzchar(rsp)])
    if (length(have) > 0L) return(have)
  }
  f <- obj$formula
  if (is.null(f)) return(character(0L))
  if (inherits(f, "formula") && length(f) < 3L) {
    return(character(0L))
  }
  extract_response_names(f)
}


#' @noRd
subset_obj_to_response <- function(obj, r) {
  # Per-response slice of a multi-response fit. Filters the prior
  # table to rows scoped to response `r` (including rows with no
  # `resp` set, which are shared across responses), and pins
  # `response_names = r`. Downstream extractors / renderers that
  # read from `obj$prior` and `obj$response_names` then see the
  # single-response view without per-helper threading.
  out <- obj
  out$response_names <- r
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
mv_resp_fan_out <- function(object, resp) {
  # Per-response fan-out for multivariate user-facing methods.
  # When `resp` is NULL on an mv fit, re-invoke the calling
  # function once per response with `resp = r` and return a named
  # list; otherwise return NULL so the caller can fall through to
  # its univariate body. Centralises the pattern shared by
  # residuals.mvgam, pp_check.mvgam, conditional_effects.mvgam,
  # mvgam_resid_panel, and hindcast.mvgam.
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
  responses <- object$formula$responses
  eval_env <- parent.frame(n = 2L)
  out <- lapply(responses, function(r) {
    call_r <- parent_call
    call_r$resp <- r
    eval(call_r, envir = eval_env)
  })
  names(out) <- responses
  out
}


#' Resolve the family for a given response in a multi-response fit.
#'
#' brms `mvbf` puts a placeholder family on the top-level `mvgam`
#' object (the default `gaussian()`); the actual per-arm family
#' lives on `object$formula$forms[[resp]]$family`. Univariate fits
#' have one family at the top level. This helper returns the family
#' name string (`fam$family`).
#'
#' @noRd
resolve_resp_family <- function(object, resp = NULL) {
  if (!is.null(resp) && inherits(object$formula, "mvbrmsformula")) {
    bf_i <- object$formula$forms[[resp]]
    if (!is.null(bf_i$family)) return(resolve_family_name(bf_i$family))
  }
  resolve_family_name(object$family)
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
