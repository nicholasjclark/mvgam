# Shared internal helpers for methods that operate on multiple
# fitted `mvgam` objects or `mvgam_forecast` objects (model
# averaging, posterior averaging, forecast ensembling). Mirrors
# the helper surface brms uses to back `posterior_average.brmsfit`
# and `pp_average.brmsfit`, with brms-internal calls replaced by
# exported equivalents per CRAN policy.


# Internal: variadic capture of mvgam fits (or, with `class =
# "mvgam_forecast"`, of forecast objects) passed as `x, ...`.
# Returns `list(models = <named list>, other = <named list of
# remaining args>)`. Non-model `...` entries must be named or
# the call errors.
#'@noRd
mvgam_split_models <- function(x, ..., model_names = NULL,
                                class = "mvgam") {
  checkmate::assert_string(class)
  checkmate::assert_class(x, class)
  dots <- list(...)
  user_names <- names(dots)
  if (is.null(user_names)) {
    user_names <- rep("", length(dots))
  }
  is_model_dot <- vapply(dots, inherits, logical(1L), class)
  other <- dots[!is_model_dot]
  other_names <- user_names[!is_model_dot]
  if (length(other) > 0L &&
      (length(other_names) == 0L ||
         any(!nzchar(other_names)))) {
    stop(insight::format_error(c(
      "All non-model arguments must be named.",
      i = paste0(
        "Pass mvgam fits positionally; pass other arguments ",
        "with explicit names."
      )
    )))
  }
  models <- c(list(x), dots[is_model_dot])
  if (!is.null(model_names)) {
    checkmate::assert_character(
      model_names, len = length(models), any.missing = FALSE,
      min.chars = 1L
    )
    names(models) <- model_names
  } else {
    dot_call <- substitute(list(x, ...), env = parent.frame())[-1L]
    deparsed <- vapply(dot_call, deparse1, character(1L))
    model_pos <- c(TRUE, is_model_dot)
    candidate <- deparsed[model_pos]
    names(models) <- candidate
    if (any(duplicated(names(models))) || !all(nzchar(names(models)))) {
      names(models) <- paste0("model", seq_along(models))
    }
  }
  list(models = models, other = other)
}


# Internal: check that all fits in `models` agree on the response
# variable(s) — both the name(s) and the actual numeric values.
# Mirrors brms's `match_response`/`hash_response` (the latter
# hashes standata$Y; we use direct equality on the response
# columns extracted from `$data`). Returns TRUE/FALSE.
#'@noRd
mvgam_match_response <- function(models) {
  checkmate::assert_list(models, min.len = 1L, types = "mvgam")
  if (length(models) < 2L) {
    return(TRUE)
  }
  resp_names <- lapply(models, function(m) {
    if (!is.null(m$response_names)) {
      m$response_names
    } else {
      mvgam_response_name(m)
    }
  })
  ref_names <- resp_names[[1L]]
  if (!all(vapply(resp_names[-1L], identical, logical(1L),
                  ref_names))) {
    return(FALSE)
  }
  ref_y <- lapply(ref_names, function(nm) {
    as.numeric(models[[1L]]$data[[nm]])
  })
  for (i in seq.int(2L, length(models))) {
    for (j in seq_along(ref_names)) {
      cand <- as.numeric(models[[i]]$data[[ref_names[[j]]]])
      if (!identical(cand, ref_y[[j]])) {
        return(FALSE)
      }
    }
  }
  TRUE
}


# Internal: numeric-weight validation + normalisation. Shared
# between `mvgam_validate_weights` (model averaging) and
# `ensemble.mvgam_forecast` (forecast ensembling). Errors with
# `insight::format_error` if `weights` is not finite,
# non-negative, length `n_models`, with positive total.
#'@noRd
mvgam_normalize_weights <- function(weights, n_models) {
  checkmate::assert_numeric(weights)
  checkmate::assert_integerish(n_models, lower = 1L, len = 1L)
  if (length(weights) != n_models) {
    stop(insight::format_error(c(
      "Numeric 'weights' must have one entry per model.",
      x = paste0(
        "Got ", length(weights), " weights for ",
        n_models, " models."
      )
    )))
  }
  if (any(weights < 0) || any(!is.finite(weights))) {
    stop(insight::format_error(
      "Numeric 'weights' must be finite and non-negative."
    ))
  }
  if (sum(weights) == 0) {
    stop(insight::format_error(
      "Numeric 'weights' must have a positive total."
    ))
  }
  weights / sum(weights)
}


# Internal: validate / compute model-averaging weights. Accepts
# either a numeric vector (length == length(models), non-negative,
# auto-normalised to sum to 1) or one of the named strategies
# `"stacking"`, `"pseudobma"`, `"pseudobma+"`, `"loo"`, `"waic"`.
# Stacking / pseudobma delegate to `loo::loo_model_weights`;
# `"loo"` / `"waic"` compute information criteria via the
# existing mvgam S3 methods and convert to weights using
# `w_i = exp(-0.5 * dIC_i) / sum(exp(-0.5 * dIC_i))`.
#'@noRd
mvgam_validate_weights <- function(weights, models,
                                    control = list()) {
  checkmate::assert_list(models, min.len = 1L, types = "mvgam")
  checkmate::assert_list(control)
  if (is.numeric(weights)) {
    return(mvgam_normalize_weights(weights, length(models)))
  }
  checkmate::assert_string(weights)
  if (identical(weights, "kfold")) {
    stop(insight::format_error(c(
      "Weight strategy 'kfold' is not supported for 'mvgam' fits.",
      x = "There is no 'kfold.mvgam' method.",
      i = paste0(
        "Use 'stacking', 'pseudobma', 'pseudobma+', 'loo', ",
        "'waic', or a numeric vector."
      )
    )))
  }
  valid <- c("stacking", "pseudobma", "pseudobma+",
             "loo", "waic")
  weights <- match.arg(weights, valid)
  if (weights %in% c("loo", "waic")) {
    crit_fun <- match.fun(weights)
    crits <- lapply(models, crit_fun)
    elpd_col <- paste0("elpd_", weights)
    elpds <- vapply(crits, function(cc) {
      cc$estimates[elpd_col, "Estimate"]
    }, numeric(1L))
    dic <- -2 * elpds
    dic <- dic - min(dic)
    w <- exp(-0.5 * dic)
    return(w / sum(w))
  }
  loos <- lapply(models, loo)
  if (identical(weights, "pseudobma+")) {
    method <- "pseudobma"
    bb <- TRUE
  } else if (identical(weights, "pseudobma")) {
    method <- "pseudobma"
    bb <- FALSE
  } else {
    method <- "stacking"
    bb <- FALSE
  }
  args <- c(
    list(loos, method = method, BB = bb),
    control[setdiff(names(control), c("method", "BB"))]
  )
  out <- do.call(loo::loo_model_weights, args)
  as.numeric(out)
}


# Internal: deterministic rounding of fractional draw counts to
# integers that preserve the input total. Verbatim port of
# brms's `round_largest_remainder`.
#'@noRd
mvgam_round_largest_remainder <- function(x) {
  x <- as.numeric(x)
  total <- round(sum(x))
  out <- floor(x)
  diff <- x - out
  J <- order(diff, decreasing = TRUE)
  I <- seq_len(total - floor(sum(out)))
  out[J[I]] <- out[J[I]] + 1L
  out
}


# Internal: normalise the `method` argument of `pp_average.mvgam`
# / future combined-forecasting methods.
#'@noRd
mvgam_validate_pp_method <- function(method) {
  match.arg(
    method,
    c("posterior_predict", "posterior_epred", "posterior_linpred")
  )
}


# Internal: brms-style deprecated-alias handler. Returns `alias`
# if it was supplied (non-NULL); otherwise returns `x`. Errors
# if both `x` and `alias` are supplied.
#'@noRd
mvgam_use_alias <- function(x, alias = NULL) {
  if (!is.null(alias)) {
    if (!is.null(x)) {
      stop(insight::format_error(
        "Specify only one of an argument and its deprecated alias."
      ))
    }
    return(alias)
  }
  x
}
