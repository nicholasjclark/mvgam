# Shared utilities for multivariate (mvbind / mvbrmsformula) fits.
# Centralises three patterns previously duplicated across
# residuals.mvgam / pp_check.mvgam / plot.mvgam /
# conditional_effects.mvgam / hindcast.mvgam / methods_md:
#
#   1. response-name extraction (with prior-table fallback for
#      prefits where response_names is NULL)
#   2. detection of `set_rescor(TRUE)`
#   3. per-response fan-out: when a method receives no `resp`
#      arg on an mv fit, re-enter the method once per response
#      and return a named list


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
  # Centralised so the ifelse ladder lives in one place; was
  # previously duplicated in mvgam_ranef_aliases / ranef.mvgam /
  # VarCorr.mvgam.
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
  # its univariate body. Centralises the pattern previously
  # copy-pasted into residuals.mvgam, pp_check.mvgam,
  # conditional_effects.mvgam, mvgam_resid_panel, and
  # hindcast.mvgam.
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
