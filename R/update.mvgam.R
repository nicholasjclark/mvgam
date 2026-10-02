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
#' @param ... Any other argument accepted by [`mvgam()`], or by
#'   [`jsdgam()`] for a `jsdgam` fit (e.g.
#'   `trend_formula`, `family`, `prior`, `chains`, `iter`,
#'   `warmup`, `cores`, `threads`, `algorithm`, `backend`,
#'   `silent`, `seed`, `init`, `control`). Each value overrides
#'   the matching slot inherited from `object`. A `prior` joins
#'   the priors the original fit set. See "Prior inheritance and
#'   Stan model reuse" in Details.
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
#' Sampler settings follow [brms::update.brmsfit()]. `chains`,
#' `iter`, `warmup`, `thin` and `control` carry over when the refit
#' keeps the original `algorithm`, and `control` also needs the
#' original `backend`. A new `iter` without a `warmup` takes the
#' default warmup of `iter / 2`. `control` merges entry by entry:
#' `update(fit, adapt_delta = 0.99)` keeps the original
#' `max_treedepth`.
#'
#' New training data passed as `newdata` keeps the fit's held-out
#' data when that lies after the new training times. Held-out data
#' overlapping them is dropped with a message.
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
#' [`update()`]. As in [brms::update.brmsfit()], the new rows join
#' the priors the user set on the original fit, and a new row
#' replaces an old one for the same parameter:
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
#' Multivariate fits take a complete new formula as `formula.`. A
#' formula update written with `.` is refused, as brms refuses it.
#'
#' `jsdgam()` fits refit through [`jsdgam()`] with the arguments the
#' original call evaluated, including `factor_formula`, `n_lv`,
#' `traits`, `trait_slopes` and `phylo`. Any of them can be overridden
#' through `...`.
#'
#' Multiple-imputation fits (produced by `mvgam_multiple(combine =
#' TRUE)`) are not supported by `update()`. Refit each imputation with
#' [`mvgam()`] and pool them again.
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
  # A refit inherits `iter` and `warmup`, which the deprecated
  # `samples` and `burnin` would contradict
  dots <- translate_samples_burnin(list(...))
  attr(dots, "translated") <- NULL
  if ("data" %in% names(dots)) {
    stop(insight::format_error(c(
      "'update()' takes new training data as 'newdata'.",
      x = "'data' belongs to the original 'mvgam()' call."
    )), call. = FALSE)
  }
  if (inherits(object, "mvgam_pooled")) {
    stop(insight::format_error(c(
      "'update()' is not supported for pooled multiple-imputation fits.",
      i = paste0("Refit each imputation with 'mvgam()' and pool them ",
                 "with 'mvgam_multiple(combine = TRUE)'.")
    )), call. = FALSE)
  }
  plan <- update_refit_plan(object, formula., newdata, dots)
  # Both supported backends cache compiled models by stancode hash, and
  # an unchanged stancode costs no compile. `recompile = FALSE` turns a
  # changed stancode into an error.
  if (isFALSE(recompile) &&
      !identical(mvgam_normalise_stancode(refit_stancode(plan)),
                 mvgam_normalise_stancode(object$stancode))) {
    stop(insight::format_error(c(
      "'recompile = FALSE' is not supported for this update.",
      x = "The updated model has different Stan code.",
      i = "Pass 'recompile = TRUE' or omit 'recompile'.",
      i = "Use 'stancode(object)' to inspect the current model."
    )), call. = FALSE)
  }
  fit <- do.call(plan$fn, plan$args)
  # The fitting function stamps the call of the frame that reached it.
  # Under `do.call()` every argument arrives as its value, and the
  # recorded call would inline the whole data frame. The refit's call
  # is the original with the changed arguments written over it. It
  # keeps every symbol the user typed and can be handed to `update()`.
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


# Internal: the function and arguments a refit calls. A `jsdgam` fit
# has class `jsdgam` and refits through `jsdgam()`,
# which owns the factor structure, traits and phylogeny that
# `mvgam()` has no arguments for.
#'@noRd
update_refit_plan <- function(object, formula., newdata, dots) {
  if (inherits(object, "jsdgam")) {
    return(list(fn = jsdgam,
                args = jsdgam_update_call(object, formula., newdata, dots)))
  }
  list(fn = mvgam,
       args = mvgam_update_call(object, formula., newdata, dots))
}


# Internal: the stancode a refit plan would compile, from a prefit
# built by the same function the refit calls.
#'@noRd
refit_stancode <- function(plan) {
  args <- plan$args
  args$run_model <- FALSE
  do.call(plan$fn, args)$stancode
}


# Internal: normalise a stancode string to the code it compiles. Two
# programs are the same model when their code matches. The head of an
# mvgam program is a `// Generated with mvgam X.Y.Z using brms X.Y.Z`
# comment whose versions drift across sessions, and the comments the
# generators write change with their wording. Comments of both
# spellings go, string literals stay, and each line is trimmed with
# empty lines dropped.
mvgam_normalise_stancode <- function(stancode) {
  if (is.null(stancode)) {
    return(character(0L))
  }
  lines <- strsplit(paste(stancode, collapse = "\n"), "\n",
                    fixed = TRUE)[[1L]]
  lines <- stan_drop_block_comments(lines)
  lines <- trimws(vapply(lines, stan_drop_line_comment, character(1),
                         USE.NAMES = FALSE))
  paste(lines[nzchar(lines)], collapse = "\n")
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
  # A `run_model = FALSE` stub records `algorithm = "none"`, which no
  # backend lists, and handing it back refused the refit that turns a
  # prefit into a fit. Reported as absent, the loop below skips it and
  # the default applies.
  algorithm = list(
    getter = function(object) {
      value <- object$algorithm
      if (identical(value, "none")) NULL else value
    }
  ),
  init = list(slot = "init"),
  save_pars = list(slot = "save_pars"),
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
  # A refit of a quiet fit is quiet.
  silent = list(getter = function(object) object$silent),
  loadings_prior = list(
    getter = function(object) {
      denormalise_loadings_prior(
        first_trend_spec(object)$loadings_prior_spec
      )
    }
  )
)

# The four brms code-generation options all come off one slot and
# join the registry through one loop.
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
  stanvars = paste0("stored with mvgam's own mixed in; passing them ",
                    "again injects them twice"),
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
  out$chains <- realised_chain_count(fit_obj)
  if (!is.null(first$iter)) out$iter <- first$iter
  if (!is.null(first$warmup)) out$warmup <- first$warmup
  if (!is.null(first$thin)) out$thin <- first$thin
  if (length(first$control)) out$control <- first$control
  out
}


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


# Vectorised `==` that returns FALSE for an NA source.
#'@noRd
identical_source <- function(source, value) {
  !is.na(source) & source == value
}


#' The prior a refit is built with
#'
#' The rule follows `brms::update.brmsfit()`. A new `prior` joins the
#' rows the user set on the original fit. A new row replaces an old one
#' with the same `brmsprior_key_cols`. An unbounded row is `NA` from
#' `prior()` and `""` in a fitted table, and the two compare equal.
#'
#' @param new The `prior` passed to `update()`.
#' @param old_user The rows the user set on the original fit, or NULL.
#' @return A `brmsprior`.
#' @noRd
merge_update_priors <- function(new, old_user) {
  checkmate::assert_class(new, "brmsprior")
  checkmate::assert_class(old_user, "brmsprior", null.ok = TRUE)
  if (is.null(old_user) || nrow(old_user) == 0L) {
    return(new)
  }
  combined <- bind_brmsprior_rows(list(new, old_user))
  keys <- combined[, intersect(brmsprior_key_cols, names(combined)),
                   drop = FALSE]
  keys[] <- lapply(keys, function(x) {
    x <- as.character(x)
    x[is.na(x)] <- ""
    x
  })
  structure(combined[!duplicated(keys), , drop = FALSE],
            class = c("brmsprior", "data.frame"))
}


#' The formula a refit is built with
#'
#' A univariate formula takes the update through `stats::update()`,
#' which dispatches to brms for a `brmsformula`. A multivariate
#' formula has no update method: `formula.` then has to be the whole
#' new formula.
#'
#' @param old The fit's formula.
#' @param formula. The caller's `formula.`, or NULL.
#' @return The refit's formula.
#' @noRd
updated_formula <- function(old, formula.) {
  if (is.null(formula.)) {
    return(old)
  }
  if (!is_multivariate_formula(old)) {
    return(stats::update(old, formula.))
  }
  if ("." %in% all.vars(formula.)) {
    stop(insight::format_error(c(
      "A formula update with '.' is not supported for multivariate fits.",
      i = "Pass the complete formula, as in 'formula. = mvbind(y1, y2) ~ x'."
    )), call. = FALSE)
  }
  formula.
}


#' The held-out data a refit keeps
#'
#' A refit keeps the fit's held-out frame when it lies after the new
#' training data, as the refits of `kfold()` and `lfo_cv()` need. New
#' training data reaching into the held-out times leaves a frame
#' `forecast()` would start inside the training window, and the refit
#' drops it.
#'
#' @param object The fit being refitted.
#' @param training The refit's training data.
#' @param silent The refit's verbosity.
#' @return The held-out frame, or NULL.
#' @noRd
inherited_holdout <- function(object, training, silent) {
  holdout <- object$test_data
  time_var <- mvgam_axes(object)$vars$time_var
  if (is.null(holdout) || is.null(time_var) ||
      !time_var %in% names(holdout) || !time_var %in% names(training)) {
    return(holdout)
  }
  if (min(holdout[[time_var]]) > max(training[[time_var]])) {
    return(holdout)
  }
  if (silent < 2L) {
    rlang::inform(c(
      "The refit drops the fit's held-out data.",
      x = "The held-out times overlap the new training data.",
      i = "Pass test data to 'forecast()' through its 'newdata'."
    ))
  }
  NULL
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


# Of the arguments `trend_arg_metadata` maps, those a refit has to
# state outright rather than merely bring back into scope. The slot
# each one is read from stays in that table, so the two cannot drift.
#
# `n_lv` is the case: a top-level `trend_map` sets the factor count
# just as a constructor argument does, and `mvgam()` has no top-level
# `n_lv` for it to travel back on. `trend_map` itself is absent here
# because it does travel that way, through `mvgam_update_inheritance`.
trend_args_stated_on_rebuild <- "n_lv"


#' State a resolved trend argument the user's own expression omits
#'
#' A refit rebuilds the trend side by re-evaluating the expression the
#' user wrote, which is right for everything that expression settles.
#' It is wrong for a value the fit resolved from somewhere else. A
#' factor count set by a top-level `trend_map` lives only on the fit,
#' and rebuilding without it changes the model rather than failing:
#' `by = lv_axis()` reads the series axis instead of the factor axis,
#' so a fit with two per-factor smooths comes back with one smooth per
#' series. Measured on a five-series, two-factor fit, that is
#' `N_lv_trend` 5 against 2 and `N_trend` 300 against 120.
#'
#' @param trend_call The `trend_call` slot, or NULL.
#' @param metadata The `trend_metadata` slot, or NULL.
#' @return `trend_call`, with each resolved argument stated on the
#'   constructor that owns it.
#' @noRd
state_resolved_trend_args <- function(trend_call, metadata) {
  if (!inherits(trend_call, "formula") || is.null(metadata)) {
    return(trend_call)
  }
  stated <- list()
  for (arg in trend_args_stated_on_rebuild) {
    if (trend_call_names_arg(trend_call, arg)) next
    value <- metadata[[trend_arg_metadata[[arg]]]]
    if (!is.null(value)) stated[[arg]] <- value
  }
  if (length(stated) == 0L) return(trend_call)

  state <- new.env(parent = emptyenv())
  state$n <- 0L
  rhs <- state_args_on_trend_constructor(
    rlang::f_rhs(trend_call), stated, state
  )
  # No constructor to state them on. Rebuilding regardless would give
  # back a different model in silence.
  if (state$n == 0L) {
    stop(insight::format_error(c(
      "'update()' needs a trend constructor to rebuild this fit.",
      x = paste0(
        "The fit resolved ",
        paste0("'", names(stated), "'", collapse = ", "),
        " outside 'trend_formula'."
      ),
      x = "The fit's 'trend_formula' lacks a trend constructor.",
      i = paste0(
        "Pass 'trend_formula = ...' naming ",
        paste0("'", names(stated), "'", collapse = ", "), "."
      )
    )), call. = FALSE)
  }
  rlang::new_formula(
    lhs = rlang::f_lhs(trend_call), rhs = rhs,
    env = environment(trend_call)
  )
}


#' Add named arguments to every trend constructor in an expression
#'
#' The registry is what decides which calls those are, so a newly
#' registered trend needs no change here.
#'
#' @param expr Current expression node.
#' @param values Named list of argument values to state.
#' @param state Environment counting the constructors rewritten.
#' @param depth Recursion depth guard.
#' @return The rewritten expression. `state$n` is zero when the
#'   expression held no trend constructor.
#' @noRd
state_args_on_trend_constructor <- function(expr, values, state,
                                            depth = 0L) {
  if (depth > 50L || !rlang::is_call(expr)) {
    return(expr)
  }
  fn_name <- rlang::call_name(expr)
  if (!is.null(fn_name) && fn_name %in% mvgam_trend_registry()) {
    args <- rlang::call_args(expr)
    args[names(values)] <- values
    state$n <- state$n + 1L
    return(rlang::call2(fn_name, !!!args))
  }
  parts <- as.list(expr)
  rewritten <- lapply(parts[-1L], state_args_on_trend_constructor,
                      values = values, state = state,
                      depth = depth + 1L)
  rlang::call2(parts[[1L]], !!!rewritten)
}


#' Arguments for refitting an `mvgam` fit
#'
#' The resolution order is the caller's `...`, then the matching slot
#' on `object`, then the `mvgam()` default.
#'
#' @param object The fit being refitted.
#' @param formula. A replacement formula or an update to it, or NULL.
#' @param newdata Replacement training data, or NULL.
#' @param dots The other arguments the caller passed to `update()`.
#' @return A list ready to feed `do.call(mvgam, ...)`.
#' @noRd
mvgam_update_call <- function(object, formula., newdata, dots) {
  resolved <- list()
  resolved$formula <- updated_formula(object$formula, formula.)
  resolved$data <- if (is.null(newdata)) object$data else newdata
  # A refit reaches brms like any prediction does, and an empty
  # observation formula names a placeholder column that `mvgam()`
  # strips from the frame it stores. Stamping it back is what the two
  # prediction paths already do; without it brms refuses the refit for
  # a variable the user never wrote and cannot supply.
  resolved$data <- ensure_obs_placeholder(resolved$data, object)
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
  if (!is.null(newdata)) {
    resolved$newdata <- inherited_holdout(object, resolved$data,
                                          resolved$silent %||% 1L)
  }
  if (!is.null(dots$prior)) {
    old <- object$prior
    old_user <- if (!is.null(old) && "source" %in% names(old)) {
      old[identical_source(old$source, "user"), , drop = FALSE]
    }
    resolved$prior <- merge_update_priors(dots$prior, old_user)
  }
  # The held-out frame reaches brms alongside the training one, so it
  # carries the placeholder on the same terms.
  resolved$newdata <- ensure_obs_placeholder(resolved$newdata, object)
  # The column alone is not the whole of what the injection produced.
  # Its pin holds the coefficient at zero, and the stored table does
  # not carry it, so a refit would sample free what the fit held
  # fixed.
  resolved$prior <- ensure_obs_placeholder_pin(resolved$prior, object)
  # The trend call came back as the user wrote it, which may name a
  # matrix or a count that is no longer in scope.
  resolved$trend_formula <- restore_trend_call_env(
    resolved$trend_formula, object$trend_metadata
  )
  # A trend formula the caller supplied states its own structure, and
  # the fit's resolved values describe the model being replaced. Only
  # the inherited call needs what the fit settled outside it.
  if (!"trend_formula" %in% names(dots)) {
    resolved$trend_formula <- state_resolved_trend_args(
      resolved$trend_formula, object$trend_metadata
    )
  }

  # Pass through any remaining user dots (cores, threads, seed,
  # control, init, silent, ...) that are not already resolved.
  resolved <- c(resolved, dots[!names(dots) %in% names(resolved)])
  inherit_sampler_args(resolved, object, dots)
}


#' Arguments for refitting a `jsdgam` fit
#'
#' `jsdgam()` stores the arguments it was called with. The refit takes
#' those, the caller's overrides and the training frame with the
#' `time` and `series` columns `jsdgam()` added removed.
#'
#' @inheritParams mvgam_update_call
#' @return A list ready to feed `do.call(jsdgam, ...)`.
#' @noRd
jsdgam_update_call <- function(object, formula., newdata, dots) {
  args <- object$jsdgam_args
  if (is.null(args)) {
    stop(insight::format_error(c(
      "'update()' needs the 'jsdgam()' arguments stored on the fit.",
      i = "Refit with 'jsdgam()' to store them."
    )), call. = FALSE)
  }
  stored_prior <- args$prior
  args$formula <- updated_formula(args$formula, formula.)
  columns <- c(time = args$unit, series = args$species)
  added <- names(columns)[columns != names(columns)]
  args$data <- newdata %||%
    object$obs_data[setdiff(names(object$obs_data), added)]
  # The held-out frame is the fit's own, as on the 'mvgam()' path.
  args$newdata <- if (is.null(newdata)) {
    object$test_data
  } else {
    inherited_holdout(object, args$data,
                      dots$silent %||% object$silent %||% 1L)
  }
  args[names(dots)] <- dots
  if (!is.null(dots$prior)) {
    args$prior <- merge_update_priors(dots$prior, stored_prior)
  }
  inherit_sampler_args(args, object, dots)
}


#' Carry the original fit's sampler settings into a refit
#'
#' The rules follow `brms::update.brmsfit()`. The settings travel only
#' when the refit keeps the fit's algorithm: an iteration count means
#' something else to a variational fit than to NUTS. An argument the
#' caller or the fit already set is kept. An `iter` the caller
#' overrides without a `warmup` drops the original warmup, whether the
#' fit's sampler or its stored call holds it. mvgam then derives the
#' warmup as `iter %/% 2`. The original warmup paired with a smaller
#' `iter` would leave Stan no sampling iterations.
#'
#' `control` also needs the fit's backend, whose defaults it holds.
#' It merges entry by entry: a NUTS setting the caller names, bare or
#' inside `control`, replaces the fit's own, and the rest stay.
#'
#' @param args The refit's argument list.
#' @param object The fit being refitted.
#' @param dots The arguments the caller passed to `update()`.
#' @return `args` with `chains`, `iter`, `warmup`, `thin` and
#'   `control` filled in.
#' @noRd
inherit_sampler_args <- function(args, object, dots) {
  same_algorithm <- identical(args$algorithm %||% "sampling",
                              object$algorithm %||% "sampling")
  if (!same_algorithm) {
    return(args)
  }
  sampler <- mvgam_sampler_inheritance(object)
  if ("iter" %in% names(dots) && !"warmup" %in% names(dots)) {
    sampler$warmup <- NULL
    args$warmup <- NULL
  }
  inherited_control <- sampler$control
  sampler$control <- NULL
  if (length(inherited_control) &&
      identical(args$backend %||% object$backend, object$backend)) {
    given <- lift_sampler_control(dots, dots$control)
    args$control <- utils::modifyList(inherited_control, given %||% list())
    args[intersect(names(args), mvgam_nuts_control_args)] <- NULL
  }
  missing_args <- setdiff(names(sampler), names(args))
  args[missing_args] <- sampler[missing_args]
  args
}
