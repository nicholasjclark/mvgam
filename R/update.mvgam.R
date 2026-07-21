#' Update a fitted \pkg{mvgam} model
#'
#' Refit a fitted `mvgam` object with new data, an updated formula,
#' new priors, different sampler settings, or any other argument
#' accepted by [`mvgam()`]. The signature mirrors
#' [brms::update.brmsfit()] so users familiar with brms can drop
#' into the same workflow.
#'
#' @param object A fitted `mvgam` object.
#' @param formula. A new observation-side formula, or a formula
#'   update specification such as `~ . + new_term`. `NULL` (the
#'   default) reuses `object$formula`. Routed through
#'   [stats::update.formula()] so all of `~ . + z`, `~ . - z`,
#'   `y2 ~ x`, and `. ~ .` work via R's standard mechanics.
#' @param newdata An optional new training data frame. `NULL`
#'   (the default) reuses `object$data`. Passing `data = ...` in
#'   `...` is an error; use `newdata`.
#' @param recompile Optional logical. Controls Stan recompilation
#'   behaviour. Both supported backends (`cmdstanr`, `rstan`) cache
#'   compiled models by stancode hash, so an unchanged stancode
#'   skips the compile step transparently. `NULL` (the default)
#'   and `TRUE` both delegate to `mvgam()` and let the cache
#'   decide. `FALSE` regenerates the prospective stancode, compares
#'   it byte-for-byte against the cached `object$stancode`, and
#'   errors if they diverge (so the caller is alerted to a needed
#'   recompile rather than silently paying compile cost). When
#'   `FALSE` and the stancodes match, the cache is guaranteed to
#'   hit.
#' @param ... Any other argument accepted by [`mvgam()`] (e.g.
#'   `trend_formula`, `family`, `prior`, `chains`, `iter`,
#'   `warmup`, `cores`, `threads`, `algorithm`, `backend`,
#'   `silent`, `seed`, `init`, `control`). Each value overrides
#'   the matching slot inherited from `object`. Passing
#'   `prior = ...` replaces the original fit's prior table for
#'   this refit only; see "Prior inheritance and Stan model
#'   reuse" in Details.
#'
#' @return A refit `mvgam` object.
#'
#' @details
#' Argument inheritance: every `mvgam()` argument is taken from
#' the corresponding slot on `object` unless the caller overrides
#' via `...`. This means
#' `update(fit, iter = 4000)` reuses the original formula, data,
#' family, trend, prior, and backend, and only bumps the iteration
#' count.
#'
#' Prior inheritance and Stan model reuse. The fitted object's
#' `prior` table is reused on every `update()` call by default.
#' The `brms` adaptive priors (e.g. the response-centred Intercept
#' prior, response-scaled `sigma` prior) carry their literal
#' constants from the original fit rather than being regenerated
#' from new training data, so a refit with a different
#' `newdata` window does not change the stancode and the compiled
#' Stan model is reused. This is what makes [`lfo_cv()`] and other
#' refit-heavy workflows fast: subsequent refits skip the (slow)
#' compile step and only repeat sampling. To override the inherited
#' priors for a single refit (e.g. swap a coefficient's prior or
#' loosen a constraint) pass `prior = ...` directly to
#' [`update()`]; the change overrides the inherited prior table
#' for that call only:
#'
#' ```r
#' new_priors <- c(
#'   prior(normal(0, 1), class = "b"),
#'   prior(exponential(1), class = "sigma")
#' )
#' refit <- update(mod, prior = new_priors)
#' ```
#'
#' A change in priors that alters the Stan model will cause a
#' recompile (and a `recompile = FALSE` call would error). Use
#' `stancode(refit)` after the call to confirm the model surface
#' you intended.
#'
#' Multivariate fits: formula updates are allowed and route
#' through brms's own formula-update machinery via
#' `stats::update.formula`.
#'
#' Multiple-imputation fits (produced by `mvgam_multiple(combine =
#' TRUE)`): not currently supported on `update()`; re-fit each
#' imputation separately by calling [`mvgam()`] and re-pool.
#'
#' @author Nicholas J Clark
#'
#' @seealso [`mvgam()`], [stats::update.formula()],
#'   [brms::update.brmsfit()],
#'   [stancode.mvgam()] for inspecting the cached Stan model,
#'   [mvgam_diagnostics] / [mvgam_loo_extras] for diagnostics and
#'   model comparison on the refit object.
#'
#' @examples
#' \donttest{
#' set.seed(13)
#' simdat <- sim_mvgam(family = poisson(), n_series = 1L,
#'                      n_timepoints = 60L, trend_model = AR())
#' mod <- mvgam(y ~ s(x), trend_formula = ~ AR(p = 1),
#'               data    = simdat$data_train,
#'               family  = poisson(),
#'               chains  = 2, silent = 2)
#'
#' # Refit with a richer obs-side formula (adding a smooth of
#' # `time`). `update()` reuses the original family, trend
#' # constructor, priors, and data when the call doesn't override
#' # them, so only the new term has to be named.
#' upd <- update(mod, formula. = ~ . + s(time, k = 5), silent = 2)
#'
#' # Compare the two fits via leave-one-out cross-validation. A
#' # positive `elpd_diff` for the richer model indicates the smooth
#' # on time raised expected log predictive density; check that
#' # the difference exceeds its standard error before drawing a
#' # firm conclusion.
#' loo_compare(loo(mod), loo(upd))
#' }
#'
#' @method update mvgam
#' @export
update.mvgam <- function(object, formula. = NULL, newdata = NULL,
                          recompile = NULL, ...) {
  checkmate::assert_class(object, "mvgam")
  checkmate::assert_data_frame(newdata, null.ok = TRUE)
  if (!is.null(recompile)) {
    checkmate::assert_logical(recompile, len = 1L)
  }
  dots <- list(...)
  if ("data" %in% names(dots)) {
    stop(insight::format_error(c(
      "Use 'newdata' rather than 'data' to update the training data on a fitted mvgam.",
      i = "'data' is reserved for the original 'mvgam()' call; 'newdata' is the supported argument name on 'update()'."
    )))
  }
  if (isTRUE(attr(object, "is_pooled")) ||
      inherits(object, "mvgam_pooled")) {
    stop(insight::format_error(c(
      "Cannot 'update()' a pooled multiple-imputation mvgam fit.",
      i = paste0(
        "Refit each imputation by calling 'mvgam()' on its slice, ",
        "then re-pool with 'mvgam_multiple(combine = TRUE)'."
      )
    )))
  }
  if (is.null(object$trend_call) &&
      !is.null(object$trend_components) &&
      length(object$trend_components) > 0L &&
      !"trend_formula" %in% names(dots)) {
    stop(insight::format_error(c(
      paste0(
        "Cannot infer the original 'trend_formula' from this fit."
      ),
      x = paste0(
        "This 'mvgam' object lacks the 'trend_call' slot (likely ",
        "built with an older mvgam version) but has trend dynamics, ",
        "so the trend constructor cannot be reconstructed."
      ),
      i = paste0(
        "Pass 'trend_formula = ...' explicitly to 'update()', or ",
        "refit the model with the current mvgam version so the ",
        "original trend_formula is preserved."
      )
    )))
  }
  call_args <- mvgam_update_call(object, formula., newdata, dots)
  # Stancode comparison drives the recompile decision. Both
  # supported backends (rstan, cmdstanr) cache compiled models by
  # stancode hash, so an unchanged stancode collapses the compile
  # cost to near zero even when delegating to the full mvgam()
  # pipeline. `recompile = FALSE` makes a diverging stancode a
  # hard error rather than a silent slow path.
  if (isFALSE(recompile)) {
    new_stancode <- mvgam_dry_stancode(call_args)
    same <- identical(
      mvgam_normalise_stancode(new_stancode),
      mvgam_normalise_stancode(object$stancode)
    )
    if (!same) {
      stop(insight::format_error(c(
        paste0(
          "'recompile = FALSE' is incompatible with the requested ",
          "update because the new model would emit different Stan ",
          "code."
        ),
        i = paste0(
          "Pass 'recompile = TRUE' (or omit 'recompile') to refit ",
          "with the new Stan model. Use 'stancode(object)' to ",
          "inspect the current model."
        )
      )))
    }
  }
  do.call(mvgam, call_args)
}


# Internal: regenerate prospective stancode for the merged args
# without compiling or sampling. Routes through the same helper
# `mvgam_single()` uses (`generate_stan_components_mvgam_formula()`),
# so the comparison is byte-for-byte against what the next
# `mvgam()` call would produce.
#'@noRd
mvgam_dry_stancode <- function(call_args) {
  mvgam_formula_obj <- mvgam_formula(
    call_args$formula,
    call_args$trend_formula
  )
  pass_args <- call_args[
    !names(call_args) %in% c("formula", "trend_formula")
  ]
  components <- do.call(
    generate_stan_components_mvgam_formula,
    c(list(formula = mvgam_formula_obj), pass_args)
  )
  components$combined_components$stancode
}


# Internal: normalise a stancode string for byte-for-byte
# comparison against `object$stancode`. Mirrors the strip brms
# uses in `stancode(version = FALSE)` and inside
# `update.brmsfit`: the head of an mvgam-generated stancode is a
# single `// Generated with mvgam X.Y.Z using brms X.Y.Z` comment.
# The version values can drift across sessions but the Stan body
# is what drives compilation, so stripping a leading `//` comment
# line lets a version-only difference fall through. The strip is
# conditional on the line actually being a comment, so if the
# header convention is ever removed or replaced the normaliser
# degrades gracefully (no Stan code is ever lost).
#'@noRd
mvgam_normalise_stancode <- function(stancode) {
  if (is.null(stancode)) {
    return(character(0L))
  }
  txt <- as.character(stancode)
  txt <- sub("^//[^\n]*\n", "", txt)
  trimws(txt)
}


# Internal: argument table for update.mvgam slot inheritance.
# Each row pairs an `mvgam()` argument name with the slot on the
# fitted `mvgam` object that supplies its default when the user
# does not override via `...`. Optional `normaliser` entries route
# the inherited value through a converter so the user-facing form
# matches what mvgam() accepts on a fresh call.
#
# Extending the inheritance to a new `mvgam()` argument is one new
# entry here, no method changes.
#'@noRd
mvgam_update_inheritance <- list(
  trend_formula = list(slot = "trend_call"),
  family = list(slot = "family"),
  prior = list(slot = "prior"),
  backend = list(slot = "backend"),
  algorithm = list(slot = "algorithm")
)


# Internal: extract chain / iter / warmup / thin from `object$fit`
# in a backend-agnostic way. mvgam stores cmdstanr and rstan fits
# as `stanfit`-class objects with a `@stan_args` slot; this helper
# returns the original sampler dimensions so `update.mvgam` can
# reuse them when the user does not override.
#'@noRd
mvgam_sampler_inheritance <- function(object) {
  out <- list()
  fit_obj <- object$fit
  if (!isS4(fit_obj) ||
      !"stan_args" %in% methods::slotNames(fit_obj)) {
    return(out)
  }
  args <- fit_obj@stan_args
  if (length(args) == 0L) {
    return(out)
  }
  first <- args[[1L]]
  out$chains <- length(args)
  if (!is.null(first$iter)) out$iter <- first$iter
  if (!is.null(first$warmup)) out$warmup <- first$warmup
  if (!is.null(first$thin)) out$thin <- first$thin
  out
}


# Internal: assemble the merged argument list for the refit. The
# resolution order is (1) user-supplied via `...`, (2) the named
# `object` slot, (3) the `mvgam()` default. Returns a list ready
# to feed `do.call(mvgam, ...)`.
#'@noRd
mvgam_update_call <- function(object, formula., newdata, dots) {
  resolved <- list()
  resolved$formula <- if (is.null(formula.)) {
    object$formula
  } else {
    stats::update.formula(object$formula, formula.)
  }
  resolved$data <- if (is.null(newdata)) object$data else newdata
  for (arg_name in names(mvgam_update_inheritance)) {
    if (arg_name %in% names(dots)) {
      resolved[[arg_name]] <- dots[[arg_name]]
      next
    }
    entry <- mvgam_update_inheritance[[arg_name]]
    value <- object[[entry$slot]]
    if (!is.null(entry$normaliser)) {
      value <- entry$normaliser(value)
    }
    resolved[[arg_name]] <- value
  }
  # Inherit sampler dimensions from the original stanfit unless
  # the user explicitly overrides.
  sampler <- mvgam_sampler_inheritance(object)
  for (arg_name in names(sampler)) {
    if (!arg_name %in% names(dots) && !arg_name %in% names(resolved)) {
      resolved[[arg_name]] <- sampler[[arg_name]]
    }
  }
  # Pass through any remaining user dots (cores, threads, seed,
  # control, init, silent, ...) that are not already resolved.
  extra <- dots[!names(dots) %in% names(resolved)]
  c(resolved, extra)
}
