#' Update a fitted \pkg{mvgam} model
#'
#' Refit a fitted `mvgam` object with new data, an updated formula, new
#' priors, different sampler settings or any other argument [mvgam()]
#' accepts. The signature follows [brms::update.brmsfit()].
#'
#' @param object A fitted `mvgam` object.
#' @param formula. A new observation formula, or an update to the
#'   fitted one such as `~ . + new_term`. `NULL` (the default) keeps
#'   `object$formula`. [stats::update.formula()] applies the update.
#'   `~ . + z`, `~ . - z`, `y2 ~ x` and `. ~ .` all work.
#' @param newdata An optional new training data frame. `NULL` (the
#'   default) keeps `object$data`. A `data` argument in `...` is an
#'   error.
#' @param recompile Optional logical. `NULL` (the default) and `TRUE`
#'   refit through `mvgam()`. Both backends cache compiled models by
#'   their Stan code. A refit with unchanged code skips compilation.
#'   `FALSE` builds the new Stan code first and compares it with
#'   `object$stancode`. A difference is an error. A refit that passes
#'   the check compiles nothing.
#' @param ... Any other argument [mvgam()] accepts, or [jsdgam()] for a
#'   `jsdgam` fit (e.g. `trend_formula`, `family`, `prior`, `chains`,
#'   `iter`, `warmup`, `cores`, `threads`, `algorithm`, `backend`,
#'   `silent`, `seed`, `init`, `control`). Each value overrides the one
#'   inherited from `object`. A `prior` joins the priors the original
#'   fit set, as Details describes.
#'
#' @return A refit `mvgam` object.
#'
#' @details
#' Every `mvgam()` argument comes from `object` unless `...` overrides
#' it. `update(fit, iter = 4000)` keeps the original formula, data,
#' family, trend, prior and backend. Only the iteration count changes.
#'
#' Sampler settings follow [brms::update.brmsfit()]. `chains`, `iter`,
#' `warmup`, `thin` and `control` carry over when the refit keeps the
#' original `algorithm`. `control` also needs the original `backend`. A
#' new `iter` without a `warmup` takes the default warmup of
#' `iter / 2`. `control` merges entry by entry:
#' `update(fit, adapt_delta = 0.99)` keeps the original `max_treedepth`.
#'
#' New training data passed as `newdata` keeps the fit's held-out data
#' when that lies after the new training times. Held-out data
#' overlapping them is dropped with a message.
#'
#' A refit reuses the fitted prior table. The adaptive priors of brms,
#' such as the Intercept prior centred on the response, keep the
#' constants of the original fit. Because the constants stay fixed, a
#' refit on a different `newdata` window has the same Stan code and
#' reuses the compiled model. [lfo_cv()] and other workflows that refit
#' many times then repeat only the sampling. An inherited row for a
#' parameter the refit lacks, such as the smooth of a dropped term, is
#' left out of the refit. A `prior` passed to
#' `update()` joins the priors the user set on the original fit, as in
#' [brms::update.brmsfit()]. A new row replaces an old one for the same
#' parameter:
#'
#' ```r
#' new_priors <- c(
#'   prior(std_normal(), class = "b"),
#'   prior(exponential(1), class = "sigma")
#' )
#' refit <- update(mod, prior = new_priors)
#' ```
#'
#' A prior change that alters the Stan code recompiles the model. With
#' `recompile = FALSE` it is an error. `stancode(refit)` shows the
#' model the refit compiled.
#'
#' Multivariate fits take a complete new formula as `formula.`. A
#' formula update written with `.` is refused, as brms refuses it.
#'
#' A `jsdgam()` fit refits through [jsdgam()] with the arguments the
#' original call evaluated, including `factor_formula`, `n_lv`,
#' `traits`, `trait_slopes` and `phylo`. `...` overrides any of them.
#'
#' `update()` refuses multiple-imputation fits from
#' `mvgam_multiple(combine = TRUE)`. Refit each imputation with
#' [mvgam()] and pool them again.
#'
#' @author Nicholas J Clark
#'
#' @seealso [loo.mvgam()] and [loo_compare.mvgam()] compare a refit with the
#'   original. [stancode.mvgam()] shows the compiled model. The
#'   `r mvgam_article("forecast_evaluation", "forecast evaluation article")`
#'   compares competing models by their forecasts.
#'
#' @examples
#' \dontrun{
#' set.seed(13)
#' simdat <- sim_mvgam(family = poisson(), n_series = 1L,
#'                     n_timepoints = 120L, trend_model = AR())
#' mod <- mvgam(y ~ s(x), trend_formula = ~ AR(p = 1),
#'              data   = simdat$data_train,
#'              family = poisson(),
#'              chains = 2, silent = 2)
#'
#' # Add a smooth of time to the observation formula. The family,
#' # trend, priors, data and sampler settings carry over
#' upd <- update(mod, formula. = ~ . + s(time, k = 5))
#'
#' # Compare the two fits by approximate leave-one-out cross-validation.
#' # The better model has elpd_diff = 0. A difference smaller than about
#' # two of its standard errors favours neither model
#' loo_compare(mod, upd)
#'
#' # A change of sampler settings alone reuses the compiled model
#' longer <- update(mod, iter = 3000, recompile = FALSE)
#' }
#'
#' @family model fitting
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
  dots <- normalise_prior_arg_alias(dots)
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
  # Both supported backends cache compiled models by their Stan code.
  # Unchanged code costs no compile. `recompile = FALSE` turns changed
  # code into an error.
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
  # Under `do.call()` every argument arrives as its value. The recorded
  # call would then inline the whole data frame. The refit's call
  # is the original with the changed arguments written over it. It
  # keeps every symbol the user typed and can be handed to `update()`.
  fit$call <- restate_updated_call(getCall(object), match.call())
  fit
}


#' The model call an `update()` produced
#'
#' Overwrites the arguments this `update()` names on the call that built
#' the original fit. `formula.` sets `formula` and `newdata` sets
#' `data`. `object` and `recompile` steer the refit and are dropped from
#' the model call.
#'
#' @param original Call that built the original fit, or `NULL`
#' @param update_call Matched call of the `update()`
#' @return Call describing the refitted model
#' @noRd
restate_updated_call <- function(original, update_call) {
  checkmate::assert_class(original, "call", null.ok = TRUE)
  checkmate::assert_class(update_call, "call")
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


#' The function and arguments a refit calls
#'
#' A `jsdgam` fit refits through `jsdgam()`, which takes the factor
#' structure, traits and phylogeny that `mvgam()` has no arguments for.
#' Every other fit refits through `mvgam()`.
#'
#' @param object Fitted `mvgam` object
#' @param formula. Formula update, or `NULL` to keep the fitted formula
#' @param newdata Data frame of new training data, or `NULL`
#' @param dots Named list of further arguments to `update()`
#' @return List holding the fitting function `fn` and its `args`
#' @noRd
update_refit_plan <- function(object, formula., newdata, dots) {
  checkmate::assert_class(object, "mvgam")
  checkmate::assert(
    checkmate::check_null(formula.),
    checkmate::check_formula(formula.),
    checkmate::check_list(formula.)
  )
  checkmate::assert_data_frame(newdata, null.ok = TRUE)
  checkmate::assert_list(dots)
  if (inherits(object, "jsdgam")) {
    return(list(fn = jsdgam,
                args = jsdgam_update_call(object, formula., newdata, dots)))
  }
  list(fn = mvgam,
       args = mvgam_update_call(object, formula., newdata, dots))
}


#' The Stan code a refit plan would compile
#'
#' Builds a prefit with the same function the refit calls.
#'
#' @param plan Refit plan from `update_refit_plan()`
#' @return Character Stan code of the refitted model
#' @noRd
refit_stancode <- function(plan) {
  checkmate::assert_list(plan)
  checkmate::assert_function(plan$fn)
  args <- plan$args
  args$run_model <- FALSE
  do.call(plan$fn, args)$stancode
}


#' Stan code reduced to the statements it compiles
#'
#' Two programs are the same model when their code matches. The version
#' header of an mvgam program changes across sessions. The generator
#' comments change with their wording. Line and block comments are
#' removed and string literals are kept. Each line is trimmed and empty
#' lines are dropped.
#'
#' @param stancode Character Stan code, or `NULL`
#' @return Character scalar of normalised code, or `character(0)` when
#'   `stancode` is `NULL`
#' @noRd
mvgam_normalise_stancode <- function(stancode) {
  checkmate::assert_character(stancode, null.ok = TRUE)
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


# Arguments a refit inherits from the fitted object. Each entry pairs an
# `mvgam()` argument with the slot, or the getter, that supplies its
# default when `...` leaves it unset. A `normaliser` converts the stored
# value to the form `mvgam()` takes on a fresh call. A new inherited
# argument needs one new entry here.
mvgam_update_inheritance <- list(
  trend_formula = list(slot = "trend_call"),
  family = list(slot = "family"),
  # mvgam lifts its own priors out of the generated Stan to report what
  # a fit sampled under. It regenerates them from the trend spec on
  # every fit. brms has no parameter called `theta_features` or `Z` and
  # refuses those rows. Only the rows a user or brms set travel.
  # The normaliser is wrapped in a function because this list is built
  # when the package loads, before `drop_mvgam_sourced_priors()` is
  # bound.
  prior = list(
    slot = "prior",
    normaliser = function(value) drop_mvgam_sourced_priors(value)
  ),
  backend = list(slot = "backend"),
  # A `run_model = FALSE` stub records `algorithm = "none"`, which no
  # backend accepts. Reported as absent, the loop below skips it. The
  # refit of a prefit then takes the default algorithm.
  algorithm = list(
    getter = function(object) {
      value <- object$algorithm
      if (identical(value, "none")) NULL else value
    }
  ),
  init = list(slot = "init"),
  save_pars = list(slot = "save_pars"),
  newdata = list(getter = function(object) object$test_data),
  # brms stores a `brmsthreads` object on every fit. Its count is NULL
  # when the user asked for no threading. `mvgam()` takes the count.
  threads = list(
    getter = function(object) object$obs_model$threads$threads
  ),
  # A `trend_map` written on the trend constructor travels inside
  # `trend_call`. `mvgam()` refuses one given in both places. Only a
  # fit that set it at the top level gets it back there.
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
# Reason: the names come from `mvgam_codegen_options()` in another
# file. Without a `Collate` field in DESCRIPTION, the locale sets the
# order files are sourced in. A function builds the entries at call
# time, after every file is sourced.
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


# Arguments a refit does not carry over, each with the reason. The fit
# stores some of them nowhere. The others change nothing about the
# model. An argument is either inherited above or named here.
# `tests/testthat/test-update.R` asserts that every argument reaching
# the code generator appears in one of the two lists.
mvgam_update_uninherited <- c(
  data2 = "stored, but empty on every fit examined",
  stanvars = paste0("stored with mvgam's own mixed in; passing them ",
                    "again injects them twice"),
  combine = "multiple imputation only; update() refuses a pooled fit",
  run_model = "a fitted object is by definition the run_model = TRUE case",
  save_model = "writes the Stan file out; the model is unchanged",
  validate = "whether the assembled code is checked; the code is the same"
)


#' Turn a resolved loadings-prior spec back into user-facing arguments
#'
#' `normalise_loadings_prior()` accepts only the names a user writes.
#' The resolved spec holds two fields under other names, plus sizes the
#' normaliser recomputes. This maps the two fields back to their
#' argument names and drops the sizes.
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
  # The spec records the MGP hyperparameters for every shrinkage.
  # `normalise_loadings_prior()` accepts them only with
  # `column_shrinkage = "mgp"`. They travel only then.
  if (loadings_spec_traits(spec)$mgp) {
    out$mgp_a1 <- spec$mgp_a1
    out$mgp_a2 <- spec$mgp_a2
  }
  out <- out[!vapply(out, is.null, logical(1L))]
  if (length(out) == 0L) NULL else out
}


#' The sampler settings a fit ran with
#'
#' mvgam stores fits from both backends as `stanfit` objects with a
#' `stan_args` slot. A refit reuses these settings when the user leaves
#' them unset.
#'
#' @param object Fitted `mvgam` object
#' @return Named list holding any of `chains`, `iter`, `warmup`, `thin`
#'   and `control`. The list is empty when the fit holds no `stanfit`.
#' @noRd
mvgam_sampler_inheritance <- function(object) {
  checkmate::assert_list(object)
  out <- list()
  first <- recorded_stan_args(object$fit)[[1L]]
  if (is.null(first)) {
    return(out)
  }
  out$chains <- realised_chain_count(object$fit)
  if (!is.null(first$iter)) out$iter <- first$iter
  if (!is.null(first$warmup)) out$warmup <- first$warmup
  if (!is.null(first$thin)) out$thin <- first$thin
  control <- Filter(length, first$control)
  if (length(control)) out$control <- control
  out
}


#' Visit every named argument of a stored trend call
#'
#' `all.vars()` and `all.names()` return the values an argument was
#' given. Neither returns the argument's own name. This walks the call
#' for the two functions below that need the names.
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
#' Those rows report what the compiled model sampled under. They name
#' mvgam parameters, which brms refuses. A refit regenerates them from
#' the trend spec.
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
#' `prior()` and `""` in a fitted table. The two compare equal.
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
  keys <- brmsprior_row_keys(combined, brmsprior_key_cols)
  structure(combined[!duplicated(keys), , drop = FALSE],
            class = c("brmsprior", "data.frame"))
}


#' Keep the inherited prior rows the refit has a parameter for
#'
#' `brms::update.brmsfit()` marks the inherited table
#' `allow_invalid_prior` for the same purpose. A refit that drops a term
#' would otherwise carry that term's rows. brms refuses such a row on
#' the observation side. `check_trend_priors()` refuses it on the trend
#' side. The refit's own prior table names the parameters it has.
#' Bounds are left out of the match. A fitted table and a fresh one can
#' record the same bound differently.
#'
#' @param prior The inherited `brmsprior`, or NULL.
#' @param refit_table The prior table of the refitted model.
#' @return `prior` without the rows that match no parameter.
#' @noRd
priors_in_refit <- function(prior, refit_table) {
  checkmate::assert_class(prior, "brmsprior", null.ok = TRUE)
  checkmate::assert_data_frame(refit_table)
  if (is.null(prior) || nrow(prior) == 0L) {
    return(prior)
  }
  cols <- setdiff(brmsprior_key_cols, c("lb", "ub"))
  keep <- brmsprior_row_keys(prior, cols) %in%
    brmsprior_row_keys(refit_table, cols)
  structure(prior[keep, , drop = FALSE], class = class(prior))
}


#' The prior table of an `mvgam()` refit
#'
#' `get_prior()` builds it from the refit's arguments without
#' generating the Stan program.
#'
#' @param args The refit's arguments to `mvgam()`.
#' @return A `brmsprior`.
#' @noRd
mvgam_refit_prior_table <- function(args) {
  checkmate::assert_list(args)
  get_prior(
    mvgam_formula(args$formula, args$trend_formula),
    data = args$data,
    family = args$family %||% gaussian(),
    loadings_prior = args$loadings_prior,
    data2 = args$data2,
    trend_map = args$trend_map
  )
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
#' training data reaching into the held-out times would leave
#' `forecast()` starting inside the training window. The refit drops
#' such a frame.
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
# holds, keyed by the `trend_metadata` slot holding it. `update()`
# rebuilds the trend side from the expression the user wrote. An
# argument they passed by name must then resolve from that
# expression.
trend_arg_metadata <- c(trend_map = "fixed_Z", n_lv = "n_lv")


#' Make a stored trend call evaluable again
#'
#' A formula keeps the expression and the environment it was written
#' in. Written inside a function, or loaded from an `.rds` in a fresh
#' session, that environment may lack what the constructor names.
#' `~ AR(p = 1, trend_map = Z)` then fails with
#' `object 'Z' not found`. The fit stores the values in
#' `trend_metadata`. They are bound into a child of the formula's own
#' environment. The expression stays as the user wrote it.
#'
#' A name the fit does not store is left alone. `mvgam()` then refuses
#' it against the user's own argument.
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


# Of the arguments `trend_arg_metadata` maps, those a refit must write
# on the constructor. Binding them in scope is not enough. The slot
# holding each one stays in `trend_arg_metadata`.
#
# A top-level `trend_map` sets the factor count as a constructor
# argument does. `mvgam()` has no top-level `n_lv` to pass it back
# through. `trend_map` itself travels at the top level, through
# `mvgam_update_inheritance`.
trend_args_stated_on_rebuild <- "n_lv"


#' State a resolved trend argument the user's own expression omits
#'
#' A refit rebuilds the trend side by evaluating the expression the
#' user wrote. A value the fit resolved elsewhere is absent from that
#' expression. A factor count set by a top-level `trend_map` is held
#' only by the fit. Rebuilt without it, the model changes with no
#' error. `by = lv_axis()` then takes the series axis in place of the
#' factor axis. A fit with two per-factor smooths would come back with
#' one smooth per series.
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
#' The trend registry decides which calls are constructors. A newly
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
  # A refit reaches brms as a prediction does. An empty observation
  # formula names a placeholder column that `mvgam()` strips from the
  # frame it stores. The two prediction paths add the column back. A
  # refit does the same. brms would otherwise refuse it for a variable
  # the user never wrote.
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
    # A getter returning NULL means the fit did not use the argument.
    # Passing NULL would differ: `threads = NULL` fails the integer
    # assertion downstream.
    if (is.null(value) && !is.null(entry$getter)) next
    resolved[[arg_name]] <- value
  }
  if (!is.null(newdata)) {
    resolved$newdata <- inherited_holdout(object, resolved$data,
                                          resolved$silent %||% 1L)
  }
  # The held-out frame reaches brms with the training frame. It takes
  # the placeholder column too.
  resolved$newdata <- ensure_obs_placeholder(resolved$newdata, object)
  # The trend call stays as the user wrote it. It may name a matrix or
  # a count that is out of scope.
  resolved$trend_formula <- restore_trend_call_env(
    resolved$trend_formula, object$trend_metadata
  )
  # A trend formula the caller supplied states its own structure. The
  # fit's resolved values describe the model being replaced. Only the
  # inherited call needs what the fit settled outside it.
  if (!"trend_formula" %in% names(dots)) {
    resolved$trend_formula <- state_resolved_trend_args(
      resolved$trend_formula, object$trend_metadata
    )
  }

  # Pass through any remaining user dots (cores, threads, seed,
  # control, init, silent, ...) that are not already resolved.
  resolved <- c(resolved, dots[!names(dots) %in% names(resolved)])

  # A new `prior` joins the rows the user set on the original fit.
  # Without one, the refit inherits the whole fitted table.
  inherited <- resolved$prior
  if (!is.null(dots$prior)) {
    old <- object$prior
    inherited <- if (!is.null(old) && "source" %in% names(old)) {
      old[identical_source(old$source, "user"), , drop = FALSE]
    }
  }
  if (!is.null(inherited) && nrow(inherited) > 0L) {
    inherited <- priors_in_refit(inherited,
                                 mvgam_refit_prior_table(resolved))
  }
  resolved$prior <- if (is.null(dots$prior)) {
    inherited
  } else {
    merge_update_priors(dots$prior, inherited)
  }
  # The placeholder column comes with a prior that pins its
  # coefficient at zero. The stored prior table lacks that row. Without
  # it a refit would sample the coefficient the fit held fixed.
  resolved$prior <- ensure_obs_placeholder_pin(resolved$prior, object)
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
    object$data[setdiff(names(object$data), added)]
  # The held-out frame is the fit's own, as on the 'mvgam()' path.
  args$newdata <- if (is.null(newdata)) {
    object$test_data
  } else {
    inherited_holdout(object, args$data,
                      dots$silent %||% object$silent %||% 1L)
  }
  args[names(dots)] <- dots
  if (!is.null(stored_prior)) {
    # `jsdgam()` builds its formulas from these arguments. A prefit of
    # the refit gives the parameters it has.
    prefit <- do.call(jsdgam, utils::modifyList(
      args, list(prior = NULL, run_model = FALSE, silent = 2L)
    ))
    stored_prior <- priors_in_refit(stored_prior, prefit$prior)
  }
  args$prior <- if (is.null(dots$prior)) {
    stored_prior
  } else {
    merge_update_priors(dots$prior, stored_prior)
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
#' It merges entry by entry. A NUTS setting the caller names, bare or
#' inside `control`, replaces the fit's own. The others stay.
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
