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
#' \dontrun{
#' set.seed(13)
#' simdat <- sim_mvgam(family = poisson(), n_series = 1L,
#'                      n_timepoints = 120L, trend_model = AR())
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
  # A jsdgam is a `c("mvgam", "jsdgam")` object, so it reaches this
  # method. Rebuilding its call would reach `mvgam()`, which knows
  # nothing of `factor_formula`, `n_lv`, `species`, `unit`, `traits`,
  # `trait_slopes` or `phylo`, and the refit would carry none of them:
  # a joint model rebuilt as an ordinary one, with nothing said. The
  # arguments cannot be recovered from `$call` either: it records
  # them as the symbols the user wrote rather than their values.
  if (inherits(object, "jsdgam")) {
    stop(insight::format_error(c(
      "Cannot 'update()' a 'jsdgam' fit.",
      x = paste0(
        "The factor structure, and any traits or phylogeny, are not ",
        "recoverable from the fitted object, so a refit would drop ",
        "them silently."
      ),
      i = paste0(
        "Call 'jsdgam()' again with the arguments you want changed. ",
        "'stancode(object)' and 'standata(object)' show what this ",
        "fit was built from."
      )
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
  fit <- do.call(mvgam, call_args)
  # `mvgam()` stamps the call of the frame that reached it, and this
  # one is `do.call()`'s: every argument has already resolved to its
  # value, so the recorded call would inline the whole data frame.
  # The refit's call is the original with the arguments the user
  # changed written over it, which keeps every symbol they typed and
  # is a call `update()` can be handed again.
  fit$call <- restate_updated_call(getCall(object), match.call())
  fit
}


# Internal: the model call an `update()` produced.
#
# Takes the call that built the original fit and overwrites the
# arguments this `update()` names. Two of them are spelled
# differently on the two sides -- `formula.` sets `formula` and
# `newdata` sets `data` -- and `recompile` steers the refit rather
# than the model, so it is not part of the call the model was built
# from.
#' @noRd
restate_updated_call <- function(original, update_call) {
  if (is.null(original)) {
    return(update_call)
  }
  changed <- as.list(update_call)[-1L]
  changed[c("object", "recompile")] <- NULL
  renamed <- c(formula. = "formula", newdata = "data")
  for (from in intersect(names(renamed), names(changed))) {
    changed[[renamed[[from]]]] <- changed[[from]]
    changed[[from]] <- NULL
  }
  for (nm in names(changed)) {
    original[[nm]] <- changed[[nm]]
  }
  original
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
  # mvgam lifts its own priors out of the generated Stan so a fit can
  # report what it sampled under, and regenerates them from the trend
  # spec on every fit. Handing them back to brms, which knows no
  # parameter called `theta_features` or `Z`, is an error rather than
  # a no-op, so only what a user or brms set travels.
  # Wrapped rather than named directly: this list is built when the
  # package loads, before the function below is bound.
  prior = list(
    slot = "prior",
    normaliser = function(value) drop_mvgam_sourced_priors(value)
  ),
  backend = list(slot = "backend"),
  algorithm = list(slot = "algorithm"),
  init = list(slot = "init"),
  newdata = list(getter = function(object) object$test_data),
  # brms keeps this as a `brmsthreads` object, present whether or not
  # the user asked for threading, with a NULL count when they did not.
  # `mvgam()` takes the count, so that is what travels.
  threads = list(
    getter = function(object) object$obs_model$threads$threads
  ),
  # A `trend_map` written on the trend constructor travels inside
  # `trend_call`, and supplying it at the top level as well is a
  # collision `mvgam()` refuses outright. Only a fit that set it at
  # the top level needs it handed back.
  trend_map = list(
    getter = function(object) {
      if (trend_call_names_arg(object$trend_call, "trend_map")) {
        return(NULL)
      }
      object$trend_metadata$fixed_Z
    }
  ),
  # A refit of a quiet fit is quiet. Falls back to the `mvgam()`
  # default on a fit made before the slot existed.
  silent = list(getter = function(object) object$silent),
  loadings_prior = list(
    getter = function(object) {
      denormalise_loadings_prior(
        object$mv_spec$trend_specs$loadings_prior_spec %||%
          object$trend_components$specifications$loadings_prior_spec
      )
    }
  )
)

# The four brms code-generation options all come off one slot, so they
# join the registry through one loop rather than four near-identical
# entries. A fit made before the slot existed returns NULL and the
# refit takes the `mvgam()` default, which is what it did before.
#
# Reason: built on first read rather than at load, because the names
# come from `mvgam_codegen_options()` in another file and DESCRIPTION
# sets no `Collate`, so which file is sourced first is a property of
# the locale.
codegen_inheritance_entries <- function() {
  entries <- lapply(names(mvgam_codegen_options()), function(nm) {
    list(getter = function(object) object$codegen[[nm]])
  })
  stats::setNames(entries, names(mvgam_codegen_options()))
}

# The registry every reader goes through: the hand-written entries
# above plus the code-generation options.
update_inheritance_table <- function() {
  c(mvgam_update_inheritance, codegen_inheritance_entries())
}


# Arguments a refit does not carry over, each with the reason: some
# because the fit does not store them, others because they change
# nothing about the model. An argument is either inherited above or
# named here, and `tests/testthat/test-update.R` asserts that every
# argument reaching the code generator appears in one of the two.
# Silence is what let `loadings_prior` go missing: a refit dropped the
# whole structured prior and nothing said so.
mvgam_update_uninherited <- c(
  data2 = "stored, but empty on every fit examined",
  stanvars = "stored with mvgam's own mixed in, so re-passing would double-inject",
  combine = "multiple-imputation only, and a pooled fit is refused",
  run_model = "a fitted object is by definition the run_model = TRUE case",
  save_model = "writes the Stan file out; the model is unchanged",
  validate = "whether the assembled code is checked, not what it holds"
)


#' Turn a resolved loadings-prior spec back into user-facing arguments
#'
#' `normalise_loadings_prior()` allow-lists the names a user writes
#' and rejects the rest, so the resolved spec cannot be handed back as
#' it stands. This maps the two that were renamed and drops the sizes
#' the normaliser recomputes.
#'
#' @param spec A `loadings_prior_spec`, or NULL.
#' @return A list `mvgam(loadings_prior = )` accepts, or NULL.
#' @noRd
denormalise_loadings_prior <- function(spec) {
  if (is.null(spec)) return(NULL)
  out <- list(
    features = spec$features_mat,
    distances = spec$distance_mats,
    column_shrinkage = spec$column_shrinkage
  )
  # The spec carries the MGP hyperparameters whatever the shrinkage,
  # while `normalise_loadings_prior()` refuses them unless the
  # shrinkage is `"mgp"`, so they travel only when they mean
  # something.
  if (loadings_spec_traits(spec)$mgp) {
    out$mgp_a1 <- spec$mgp_a1
    out$mgp_a2 <- spec$mgp_a2
  }
  out <- out[!vapply(out, is.null, logical(1L))]
  if (length(out) == 0L) NULL else out
}


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
#' Visit every named argument of a stored trend call
#'
#' `all.vars()` and `all.names()` see the values an argument was given
#' but never the argument's own name, so the call has to be walked.
#' Both readers below want the same walk.
#'
#' @param trend_call The `trend_call` slot, or NULL.
#' @param visit Called as `visit(name, value)` for each named
#'   argument, at any depth.
#' @return `NULL`, invisibly. Callers accumulate through `visit`.
#' @noRd
walk_trend_call_args <- function(trend_call, visit) {
  if (!inherits(trend_call, "formula")) return(invisible(NULL))
  recurse <- function(e) {
    if (!is.call(e)) return(invisible(NULL))
    arg_names <- names(e)
    for (i in seq_along(e)) {
      named <- !is.null(arg_names) && nzchar(arg_names[[i]])
      if (named) visit(arg_names[[i]], e[[i]])
      recurse(e[[i]])
    }
    invisible(NULL)
  }
  recurse(trend_call[[length(trend_call)]])
  invisible(NULL)
}


#' Does a stored trend call name a given constructor argument?
#'
#' @param trend_call The `trend_call` slot, or NULL.
#' @param arg Argument name to look for.
#' @return `TRUE` when the call names it anywhere.
#' @noRd
trend_call_names_arg <- function(trend_call, arg) {
  checkmate::assert_string(arg)
  found <- FALSE
  walk_trend_call_args(trend_call, function(name, value) {
    if (identical(name, arg)) found <<- TRUE
  })
  found
}


#' Drop the prior rows mvgam lifted from its own generated Stan
#'
#' Those rows exist to report what the compiled model sampled under.
#' They name mvgam parameters rather than brms ones, so a refit
#' regenerates them from the trend spec and brms rejects them if they
#' are passed in.
#'
#' @param prior A `brmsprior`, or NULL.
#' @return The same table without its mvgam-sourced rows.
#' @noRd
drop_mvgam_sourced_priors <- function(prior) {
  if (is.null(prior) || nrow(prior) == 0L) return(prior)
  if (!"source" %in% names(prior)) return(prior)
  keep <- !identical_source(prior$source, "mvgam")
  out <- prior[keep, , drop = FALSE]
  structure(out, class = class(prior))
}


# Vectorised `==` that treats NA as "not mvgam" rather than NA.
#'@noRd
identical_source <- function(source, value) {
  !is.na(source) & source == value
}


# Trend-constructor arguments whose resolved value the fitted object
# already carries, keyed by the `trend_metadata` slot holding it.
# `update()` rebuilds the trend side from the expression the user
# wrote, so an argument they passed by name has to be put back within
# reach of that expression.
trend_arg_metadata <- c(trend_map = "fixed_Z", n_lv = "n_lv")


#' Make a stored trend call evaluable again
#'
#' A formula keeps the expression, not the value, and carries the
#' environment it was written in. Written inside a function, or read
#' back from an `.rds` in a fresh session, that environment no longer
#' holds what the constructor referred to:
#' `~ AR(p = 1, trend_map = Z)` fails with `object 'Z' not found`.
#' The values are on the fit, so they are bound into a child of the
#' formula's own environment, leaving the expression as the user wrote
#' it while making it resolvable.
#'
#' A name the fit does not carry is left alone, so it reaches
#' `mvgam()` and fails there against the user's own argument rather
#' than somewhere inside the rebuild.
#'
#' @param trend_call The `trend_call` slot, or NULL.
#' @param metadata The `trend_metadata` slot, or NULL.
#' @return `trend_call`, with an environment that resolves what it
#'   names wherever the fit knows the value.
#' @noRd
restore_trend_call_env <- function(trend_call, metadata) {
  if (!inherits(trend_call, "formula") || is.null(metadata)) {
    return(trend_call)
  }
  parent <- environment(trend_call) %||% globalenv()
  unresolved <- Filter(
    function(v) !exists(v, envir = parent, inherits = TRUE),
    all.vars(trend_call)
  )
  if (length(unresolved) == 0L) return(trend_call)

  bindings <- list()
  walk_trend_call_args(trend_call, function(name, value) {
    if (!is.symbol(value) || !as.character(value) %in% unresolved) return()
    if (!name %in% names(trend_arg_metadata)) return()
    stored <- metadata[[trend_arg_metadata[[name]]]]
    if (!is.null(stored)) bindings[[as.character(value)]] <<- stored
  })

  if (length(bindings) == 0L) return(trend_call)
  environment(trend_call) <- list2env(bindings, parent = parent)
  trend_call
}


#'@noRd
mvgam_update_call <- function(object, formula., newdata, dots) {
  resolved <- list()
  resolved$formula <- if (is.null(formula.)) {
    object$formula
  } else {
    stats::update.formula(object$formula, formula.)
  }
  resolved$data <- if (is.null(newdata)) object$data else newdata
  inheritance <- update_inheritance_table()
  for (arg_name in names(inheritance)) {
    if (arg_name %in% names(dots)) {
      resolved[[arg_name]] <- dots[[arg_name]]
      next
    }
    entry <- inheritance[[arg_name]]
    value <- if (!is.null(entry$getter)) {
      entry$getter(object)
    } else {
      object[[entry$slot]]
    }
    if (!is.null(entry$normaliser)) {
      value <- entry$normaliser(value)
    }
    # A getter finding nothing means the fit did not use the argument,
    # which is not the same as passing it NULL: `threads = NULL` trips
    # the integer assertion downstream.
    if (is.null(value) && !is.null(entry$getter)) next
    resolved[[arg_name]] <- value
  }
  # The trend call came back as the user wrote it, which may name a
  # matrix or a count that is no longer in scope.
  resolved$trend_formula <- restore_trend_call_env(
    resolved$trend_formula, object$trend_metadata
  )

  # Inherit sampler dimensions from the original stanfit unless
  # the user explicitly overrides. `warmup` cannot be inherited on its
  # own: mvgam derives it as `iter %/% 2`, so pairing the original
  # warmup with a smaller user-supplied `iter` leaves warmup at or
  # above the new total and asks Stan for a negative number of
  # sampling iterations.
  sampler <- mvgam_sampler_inheritance(object)
  if ("iter" %in% names(dots) && !"warmup" %in% names(dots)) {
    sampler$warmup <- NULL
  }
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
