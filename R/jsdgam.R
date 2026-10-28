# Joint species distribution GAMs.
#
# `jsdgam()` builds the `mvgam()` call of a joint species distribution
# model. The latent factors form the trend. `trend_formula` is
# `factor_formula`, where `~ -1` gives `ZMVN()`. `trend_map` is an
# all-NA `n_species` by `n_lv` matrix, which samples every loading. The
# fit is an mvgam fit with `jsdgam` after `mvgam` in its class. The
# mvgam methods serve it, using the extra slots set below.
#
# The loadings are identified after sampling by the QR rotation of
# Heaps and Jermyn. A `factor_formula` with `by = lv_axis()` constrains
# them instead and skips the rotation.

#' Fit Joint Species Distribution Models in mvgam
#'
#' Fits a joint species distribution model (JSDM) to the presences,
#' detections or counts of many species across many sampling units.
#' Species often co-occur more or less often than their responses to
#' the measured covariates predict. A few latent factors capture that
#' residual association. They need far fewer parameters than a full
#' correlation matrix among species. The observation model takes the
#' smooths, Gaussian processes and random effects of any mvgam model.
#' The factors take their own predictors through `factor_formula`.
#' Write [lv_axis()] in one of its terms to give each factor its own
#' effect of a covariate, a constrained ordination.
#'
#' @inheritParams mvgam
#' @param factor_formula A one-sided formula for the latent factors'
#'   linear predictor. Write [lv_axis()] in a term to give each factor
#'   its own effect, as in `~ s(elev, by = lv_axis())` or
#'   `~ elev:lv_axis()`. Each covariate must take one value per
#'   sampling unit. The deprecated `by = trend` acts as
#'   `by = lv_axis()`. Defaults to `~ -1`, factors without predictors.
#' @param knots Optional named list of knot positions for smooth terms,
#'   by covariate, as `knots` of [mvgam()].
#' @param data A `data.frame` or `list` holding the response, the
#'   covariates of both formulas, the `unit` column and the `species`
#'   factor.
#'
#'   Data held as arrays, as \pkg{unmarked}, \pkg{ubms},
#'   \pkg{flocker}, \pkg{Hmsc} and \pkg{spOccupancy} take them, convert
#'   to this long format. [pivot_detection_array()] converts detection
#'   histories of occupancy and N-mixture data: `[J, K]`, `[N, J, K]`,
#'   multi-season `[N, J, T, K]` or a list of `[J, K]` matrices per
#'   species. [pivot_species_matrix()] converts a wide `[J, N]` species
#'   composition matrix.
#' @param family The observation family every species shares, as a
#'   family object. [mvgam_families] lists them. Defaults to
#'   `binomial()` for presence-absence data. Counts take a count family
#'   such as `poisson()` or `negbinomial()`.
#'
#'   For one family per species, write `formula` with one [brms::bf()]
#'   per species, each naming its family, as in
#'   `bf(count ~ env, family = poisson()) +
#'   bf(seen ~ env, family = bernoulli())`. The responses then form the
#'   species axis, in place of a `species` column. `family` applies to
#'   any response that names none.
#'
#'   The simplex families [diri()], [multi()] and [categ()] need a
#'   per-species term, as in `y ~ env * species` or
#'   `y ~ 0 + species + env:species`. A coefficient shared by every
#'   species cancels from their likelihood. In `y ~ env`, `b_Intercept`
#'   and `b_env` take their priors alone. mvgam warns of this once per
#'   session. [diri()] sets out the identification.
#' @param unit The unquoted name of the numeric column indexing the
#'   sampling units, often `time` or `site`. Defaults to `time`.
#' @param species The unquoted name of the factor column indexing the
#'   species. Defaults to `series`, as in other mvgam models.
#' @param n_lv Integer number of latent factors, from 1 to the number
#'   of species. Defaults to 2. The residual covariance
#'   `Z Z' + diag(Psi^2)` has rank at most the number of species.
#'   Further columns of `Z` add nothing.
#'
#'   Under the default Student-t prior on `Z`, or a structured prior
#'   built from `traits`, `phylo` or `loadings_prior$distances`, `n_lv`
#'   is the number of factors in the likelihood. Under the
#'   multiplicative gamma process prior (`loadings_prior = "mgp"` or
#'   `list(column_shrinkage = "mgp", ...)`) it is a ceiling. The prior
#'   shrinks later columns towards zero. A larger
#'   `loadings_prior$mgp_a2` shrinks them harder. [active_factors()]
#'   counts the factors the data support.
#' @param priors Optional `brmsprior` of priors to set, from
#'   [brms::prior()]. `prior`, the brms spelling, is accepted in `...`.
#'   [get_prior.mvgam_formula()] lists the classes a model takes.
#' @param traits Optional species traits for a structured loadings
#'   prior: a `data.frame` or matrix with one row per species, or the
#'   name of an element of `data2`. Rows are matched to
#'   `levels(data[[species]])` by a `series` column, by row name or by
#'   order. Factor columns are one-hot encoded and numeric columns are
#'   standardised. They become `loadings_prior$features`. Supplying an
#'   explicit `loadings_prior` as well is refused.
#' @param trait_slopes Optional one-sided formula of traits, such as
#'   `~ trait1 + trait2`, for trait-mediated responses to the
#'   environment (the fourth-corner model of \pkg{Hmsc}). Each
#'   population-level term of `formula` becomes a non-linear parameter
#'   regressed on the traits, plus a species-level deviation. The
#'   deviations of intercept and slope correlate under an LKJ prior
#'   (`(1 | sp | species)`). The traits must be columns of `data`,
#'   constant within species. A smooth of a trait goes in
#'   `trait_slopes`, as `~ s(trait1)`. A smooth in `formula` is
#'   refused, because brms's top-level non-linear formula takes none.
#'   The defaults are `std_normal()` on each trait coefficient and
#'   `student_t(3, 0, 2.5)` on the species SD. `priors` overrides
#'   them.
#' @param phylo Optional phylogeny: an `ape::phylo` tree, whose
#'   cophenetic distances are taken, or a numeric distance matrix
#'   with row and column names matching `levels(data[[species]])`.
#'   The distances are rescaled to a maximum of 1 and become
#'   `loadings_prior$distances$phylo`. A tree that is not ultrametric
#'   draws a warning once. A tree requires the \pkg{ape} package.
#'   Supplying an explicit `loadings_prior` as well is refused.
#' @param loadings_prior Optional structured prior on the loadings, as
#'   `loadings_prior` of [mvgam()] takes it. Supplying `traits` or
#'   `phylo` as well is refused.
#' @param ... Further arguments of [mvgam()], such as `data2` (where
#'   `traits` and `phylo` find a named element), `algorithm`, `chains`
#'   and `silent`.
#'
#' @return A fitted model of class `c("mvgam", "jsdgam", "brmsfit")`.
#'   Every mvgam method applies, such as `summary()`, `predict()`,
#'   `forecast()` and `loo()`. [ordinate()] and [residual_cor()]
#'   describe the factors. `update(fit, n_lv = 3)` refits it through
#'   `jsdgam()` with the arguments given changed.
#'
#' @seealso [sim_jsdgam()] simulates data this function fits.
#'   [sim_closure_unit_data()] simulates the `occ()` and `nmix()`
#'   case. [residual_cor()], [ordinate()], [shared_variation()],
#'   [active_factors()] and [compare_loadings()] interpret the fitted
#'   factors. [lv_axis()] gives each factor its own covariate effect.
#'   [how_to_cite()] drafts a methods paragraph for the fit and
#'   [methods_md()] writes its equations.
#'   The `r mvgam_article("jsdgam", "JSDM article")` fits a negative
#'   binomial model with a trait-informed loadings prior, ordination
#'   biplots and multiplicative gamma process shrinkage.
#'
#' @references
#' Warton, D. I., Blanchet, F. G., O'Hara, R. B., Ovaskainen, O.,
#' Taskinen, S., Walker, S. C. and Hui, F. K. C. (2015). So many
#' variables: joint modeling in community ecology. *Trends in
#' Ecology and Evolution*, 30(12):766-779. \doi{10.1016/j.tree.2015.09.007}
#'
#' Ovaskainen, O., Tikhonov, G., Norberg, A., Blanchet, F. G., Duan,
#' L., Dunson, D., Roslin, T. and Abrego, N. (2017). How to make
#' more out of community data? *Ecology Letters*, 20(5):561-576.
#' \doi{10.1111/ele.12757}
#'
#' Tikhonov, G., Opedal, O. H., Abrego, N., Lehikoinen, A., de Jonge,
#' M. M. J., Oksanen, J. and Ovaskainen, O. (2020). Joint species
#' distribution modelling with the R-package Hmsc. *Methods in
#' Ecology and Evolution*, 11(3):442-447.
#' \doi{10.1111/2041-210X.13345}
#'
#' Hui, F. K. C. (2016). boral - Bayesian Ordination and Regression
#' Analysis of Multivariate Abundance Data in R. *Methods in Ecology
#' and Evolution*, 7(6):744-750. \doi{10.1111/2041-210X.12514}
#'
#' Heaps, S. E. and Jermyn, I. H. (2024). Structured prior
#' distributions for the covariance matrix in latent factor
#' models. *Statistics and Computing*, 34:143.
#' \doi{10.1007/s11222-024-10454-0}
#'
#' @examples
#' \dontrun{
#' # Detections of 6 species at 80 sites, 4 visits each. Occupancy
#' # responds to the site covariate `env` and detection to the time of
#' # day `tod_c`. Two latent factors drive the species associations.
#' simdat <- sim_closure_unit_data(
#'   family    = occ(),
#'   n_species = 6L,
#'   n_sites   = 80L,
#'   n_visits  = 4L,
#'   n_lv      = 2L,
#'   seed      = 1L
#' )
#'
#' # A joint occupancy model. Each species has its own occupancy
#' # intercept and `env` slope, plus its own detection intercept. Two
#' # latent factors take up the residual association.
#' mod <- jsdgam(
#'   formula        = bf(y ~ series * env, p ~ series + tod_c),
#'   factor_formula = ~ -1,
#'   data           = simdat$data_train,
#'   family         = occ(),
#'   n_lv           = 2L,
#'   chains         = 2,
#'   silent         = 2
#' )
#'
#' summary(mod)
#'
#' # The effect of `env` on the chance of a detection, occupancy times
#' # detection
#' conditional_effects(mod)
#'
#' # The residual correlations among species the factors imply
#' plot(residual_cor(mod))
#'
#' # Site scores and species loadings on the two factors
#' ordinate(mod, rotation = "varimax")
#'
#' # Next, compare n_lv = 1 with update(mod, n_lv = 1L) and loo()
#' }
#'
#' @family model fitting
#' @export
jsdgam <- function(formula,
                   factor_formula = ~ -1,
                   knots,
                   data,
                   newdata,
                   family = binomial(),
                   unit = time,
                   species = series,
                   priors,
                   n_lv = 2L,
                   traits = NULL,
                   trait_slopes = NULL,
                   phylo = NULL,
                   loadings_prior = NULL,
                   backend = getOption("brms.backend", "cmdstanr"),
                   threads = NULL,
                   run_model = TRUE,
                   ...) {
  call <- match.call(expand.dots = FALSE)

  local_mvgam_silent(list(...)$silent %||% 1L)

  # `unit` and `species` are unquoted column names, `time` and
  # `series` by default
  unit_chr <- column_name_arg(substitute(unit), "unit")
  species_chr <- column_name_arg(substitute(species), "species")

  checkmate::assert_data_frame(data, min.rows = 1L)
  # Every formula `mvgam()` takes: a plain `formula`, a `bf()` with
  # sub-formulas (`p ~ visit_cov` for occ() and nmix(), `phi ~ env` for
  # diri()), or a multivariate formula naming a family per response.
  checkmate::assert_multi_class(
    formula, c("formula", "brmsformula", "mvbrmsformula")
  )
  checkmate::assert_class(factor_formula, "formula")
  # A multivariate formula gives each species its own response and
  # family. Its species axis is the set of responses, with a data
  # column per species in place of a species column. The code below
  # takes the species from `species_levels` in both layouts.
  is_mv_formula <- inherits(formula, "mvbrmsformula")
  checkmate::assert_names(
    names(data),
    must.include = if (is_mv_formula) unit_chr else c(unit_chr, species_chr)
  )
  checkmate::assert_int(n_lv, lower = 1)

  if (is_mv_formula) {
    species_levels <- names(formula$forms)
  } else {
    assert_axis_column(data, species_chr, "series")
    species_levels <- levels(data[[species_chr]])
  }
  n_species <- length(species_levels)
  if (n_species < 2L) {
    stop(insight::format_error(c(
      "'jsdgam' requires at least 2 species levels.",
      i = paste0(
        "The '", species_chr, "' column has ", n_species, " level(s)."
      )
    )))
  }
  # Checked here as well as in `mvgam()`, where the error would name
  # series. This one names species.
  validate_n_lv_ceiling(
    n_lv         = n_lv,
    n_species    = n_species,
    fit_function = "jsdgam"
  )

  # The unit becomes the time axis.
  assert_axis_column(data, unit_chr, "time")

  # The unit and species columns are copied to the `time` and
  # `series` columns mvgam fits on. The originals stay. A prediction
  # grid can then name them, as `insight::find_predictors()` reports.
  # On a multivariate formula `mvgam()` builds the series from the
  # responses.
  data_train <- copy_axis_column(data, unit_chr, "time", "unit")
  if (!is_mv_formula) {
    data_train <- copy_axis_column(data_train, species_chr, "series",
                                   "species")
    data_train$series <- factor(data_train$series,
                                levels = species_levels)
  }

  # An all-NA `n_species` by `n_lv` map samples every loading. Its
  # column count sets `n_lv` in `normalise_trend_map()`.
  trend_map_mat <- matrix(NA_real_, nrow = n_species, ncol = as.integer(n_lv))
  rownames(trend_map_mat) <- species_levels

  # The deprecated `samples` and `burnin` become `iter` and `warmup`
  # here. `update()` merges new arguments into the stored ones. Under
  # the translated names, a new `iter` replaces the stored `iter`.
  dots <- translate_samples_burnin(list(...))
  attr(dots, "translated") <- NULL
  # The priors arrive as `priors` or as the brms spelling `prior` in
  # `...`. Both resolve to one table, which the refit arguments store.
  if (!missing(priors)) dots$priors <- priors
  dots <- normalise_prior_arg_alias(dots)
  user_prior <- dots$prior
  dots$prior <- NULL
  # The arguments as evaluated, which `update()` hands back to this
  # function. The call records symbols. A refit in another scope or
  # session could not resolve them.
  refit_args <- Filter(Negate(is.null), c(
    list(
      formula = formula, factor_formula = factor_formula,
      family = if (!missing(family)) family,
      unit = unit_chr, species = species_chr, n_lv = n_lv,
      traits = traits, trait_slopes = trait_slopes, phylo = phylo,
      loadings_prior = loadings_prior, backend = backend,
      threads = threads,
      prior = user_prior,
      knots = if (!missing(knots)) knots
    ),
    dots
  ))
  # `traits` and `phylo` become fields of `loadings_prior`
  loadings_prior_resolved <- build_jsdgam_loadings_prior(
    traits = traits,
    phylo = phylo,
    loadings_prior = loadings_prior,
    species_levels = species_levels
  )

  # A simplex family whose `formula` has no species term draws a
  # warning. The family a univariate `bf()` names is the model's, as
  # in `mvgam()`.
  if (is_simplex_response_family(
    resolve_observation_family(formula, family)$family
  )) {
    warn_simplex_obs_formula_lacks_species(formula, species_chr)
  }

  # `trait_slopes` rewrites `formula` as a brms non-linear formula.
  # Each population-level slope becomes a parameter regressed on the
  # traits, with species deviations of intercept and slopes correlated
  # under an LKJ prior. The new parameters take default priors. A user
  # prior with the same class, coef and nlpar replaces one.
  trait_slopes_priors <- NULL
  if (is_mv_formula && !is.null(trait_slopes)) {
    stop(insight::format_error(c(
      "'trait_slopes' is not supported with a multivariate 'formula'.",
      x = paste0(
        "The species-level random effect of 'trait_slopes' groups rows ",
        "by a 'series' column."
      ),
      x = paste0(
        "A formula with one response per species carries no 'series' ",
        "column."
      ),
      i = "Pass a single-response 'formula' with a 'species' column."
    )))
  }
  if (!is.null(trait_slopes)) {
    validate_trait_slopes(
      trait_slopes = trait_slopes,
      obs_formula  = formula,
      data         = data_train,
      species_chr  = "series"
    )
    # The default priors come from the formula as given, one per
    # population-level term. The right-hand side of the rewritten
    # formula holds the parameter names themselves.
    trait_slopes_priors <- default_trait_slopes_priors(formula)
    formula <- build_trait_slopes_formula(
      obs_formula  = formula,
      trait_slopes = trait_slopes,
      species_var  = "series"
    )
  }

  # The trend is `factor_formula`. Its default `~ -1` gives `ZMVN()`.
  # A multivariate formula has no series column to check `trend_map`
  # against. Its factors are requested as `ZMVN(n_lv = )` on the trend
  # formula instead. A `factor_formula` naming a constructor keeps the
  # caller's `n_lv`.
  trend_formula_out <- factor_formula
  if (is_mv_formula) {
    trend_map_mat <- NULL
    spec <- stats::terms(factor_formula)
    labels <- attr(spec, "term.labels")
    if (!any(vapply(labels, is_trend_constructor_call, logical(1L)))) {
      # `term.labels` omits the intercept. A `~ site - 1` keeps its
      # `- 1` explicitly.
      parts <- c(labels, paste0("ZMVN(n_lv = ", as.integer(n_lv), ")"))
      if (identical(as.integer(attr(spec, "intercept")), 0L)) {
        parts <- c(parts, "-1")
      }
      trend_formula_out <- stats::as.formula(
        paste("~", paste(parts, collapse = " + ")),
        env = environment(factor_formula)
      )
    } else {
      # The caller's constructor sets the number of factors. The
      # ceiling is checked against it.
      user_spec <- parse_trend_formula(factor_formula)$trend_model
      if (!is.null(user_spec$n_lv)) {
        validate_n_lv_ceiling(
          n_lv         = user_spec$n_lv,
          n_species    = n_species,
          fit_function = "jsdgam"
        )
      }
    }
  }

  forward_args <- list(
    formula = formula,
    trend_formula = trend_formula_out,
    trend_map = trend_map_mat,
    data = data_train,
    backend = backend,
    threads = threads,
    run_model = run_model
  )
  # A family named in a multivariate formula belongs to its response.
  # The default `binomial()` is forwarded only for a single response.
  # A `family` the caller gives reaches the responses naming none.
  if (!missing(family) || !is_mv_formula) {
    forward_args$family <- family
  }
  # `newdata` and `knots` are forwarded when given. `mvgam()` applies
  # its own defaults otherwise.
  if (!missing(newdata)) forward_args$newdata <- newdata
  if (!missing(knots)) forward_args$knots <- knots
  # Default priors on the trait-slope nlpars go first. A user row with
  # the same class, coef and nlpar follows and replaces the default.
  combined_prior <- if (!is.null(trait_slopes_priors)) {
    if (!is.null(user_prior)) {
      c(trait_slopes_priors, user_prior)
    } else {
      trait_slopes_priors
    }
  } else {
    user_prior
  }
  if (!is.null(combined_prior)) {
    forward_args$prior <- combined_prior
  }
  if (!is.null(loadings_prior_resolved)) {
    forward_args$loadings_prior <- loadings_prior_resolved
  }
  # `build_jsdgam_loadings_prior()` has resolved the loadings prior
  dots$loadings_prior <- NULL
  forward_args <- c(forward_args, dots)

  fit <- do.call(mvgam, forward_args)

  # The arguments record the unit and species columns the user
  # named. Messages and prediction frames name those columns in
  # place of the `time` and `series` copies mvgam fits on.
  fit$jsdgam_args <- refit_args
  # `mvgam()` recorded the `do.call()` of its resolved arguments. The
  # fit reports the `jsdgam()` call, with the symbols the user wrote.
  fit$call <- call
  fit$data.name <- call_data_name(call)

  # A fit keeps the classes `mvgam()` gave it, `brmsfit` for a fit
  # and `mvgam_prefit` for a stub, with `jsdgam` after `mvgam`.
  class(fit) <- append(class(fit), "jsdgam", after = 1L)
  fit
}


#' The column an unquoted argument names
#'
#' @param expr The `substitute()`d argument
#' @param arg The argument's name, for the error
#' @return Character scalar
#' @noRd
column_name_arg <- function(expr, arg) {
  checkmate::assert_string(arg)
  out <- as.character(expr)
  if (length(out) != 1L || !nzchar(out)) {
    stop(insight::format_error(
      paste0("'", arg, "' must name a single column.")
    ), call. = FALSE)
  }
  out
}


#' Copy a user's axis column to the column mvgam fits on
#'
#' @param data Data frame
#' @param from The user's column
#' @param to `"time"` or `"series"`
#' @param arg The argument naming `from`, for the error
#' @return `data` with `to` holding `from`
#' @noRd
copy_axis_column <- function(data, from, to, arg) {
  checkmate::assert_choice(to, c("time", "series"))
  if (identical(from, to)) {
    return(data)
  }
  if (to %in% names(data)) {
    stop(insight::format_error(c(
      paste0("'data' already contains a '", to, "' column."),
      x = paste0("'", arg, " = ", from, "' names a different column."),
      i = paste0("Rename the '", to, "' column or pass '", arg, " = ",
                 to, "'.")
    )), call. = FALSE)
  }
  data[[to]] <- data[[from]]
  data
}


#' Warn once when a simplex formula has no species term
#'
#' Without a term naming the species, every category shares each
#' coefficient. A shared coefficient such as `b_Intercept` or `b_env`
#' cancels from the softmax likelihood and takes its prior alone. The
#' latent factors carry every difference among categories.
#'
#' @param formula The observation formula
#' @param species_chr The user's species column
#' @return `NULL`, invisibly
#' @noRd
warn_simplex_obs_formula_lacks_species <- function(formula, species_chr) {
  if (!inherits(formula, "formula") && !inherits(formula, "brmsformula")) {
    return(invisible(NULL))
  }
  rhs_formula <- obs_arm_main_formula(formula)
  rhs_vars <- all.vars(rhs_formula[[length(rhs_formula)]])
  if (species_chr %in% rhs_vars || "series" %in% rhs_vars) {
    return(invisible(NULL))
  }
  warn_once(
    paste0(
      "All fixed effects in 'formula' are shared across categories. ",
      "The factor model 'Z' will carry the per-category differentiation. ",
      "For interpretable per-category fixed effects, consider ",
      "'y ~ env * ", species_chr, "' or ",
      "'y ~ 0 + ", species_chr, " + env:", species_chr, "'."
    ),
    "jsdgam_simplex_no_species_interaction"
  )
  invisible(NULL)
}


#' The unit and species columns a `jsdgam()` fit was given
#'
#' @param object A fitted `mvgam` object
#' @return A list naming `unit` and `species`, or `NULL` for a fit
#'   `jsdgam()` did not build
#' @noRd
jsdgam_axis_names <- function(object) {
  args <- object$jsdgam_args
  if (is.null(args)) NULL else args[c("unit", "species")]
}


#' Name axis columns as the user of a `jsdgam()` fit named them
#'
#' A message about a frame names the unit and species columns the user
#' supplied. The `time` and `series` copies are mvgam's own.
#'
#' @param object A fitted `mvgam` object
#' @param columns Column names, some of them `time` or `series`
#' @return `columns`, with each copy replaced by the user's column and
#'   repeats dropped
#' @noRd
user_axis_names <- function(object, columns) {
  named <- jsdgam_axis_names(object)
  if (is.null(named)) {
    return(columns)
  }
  vars <- axis_vars(object)
  columns[columns == vars$time_var] <- named$unit
  columns[columns == vars$series_var] <- named$species
  unique(columns)
}


#' Name the series of a fit as a message about a frame calls them
#'
#' @param object A fitted `mvgam` object
#' @return `"Series"`, or the user's species column for a `jsdgam()`
#'   fit
#' @noRd
series_column_label <- function(object) {
  species <- jsdgam_axis_names(object)$species
  if (is.null(species)) "Series" else paste0("Column '", species, "'")
}


#' Give a frame the `time` and `series` columns a `jsdgam()` fit uses
#'
#' `jsdgam()` fits on `time` and `series` columns that repeat the
#' user's unit and species columns. A frame naming only the user's
#' columns takes the two copies here.
#'
#' The observation formula may use either name of the species. A
#' prediction grid varies the column the caller conditions on and
#' holds the other at its most frequent level, which pairs each
#' species with the trend of another. Where the two columns differ
#' in a row, the one away from the held level is the varied column.
#' Both columns take its value.
#'
#' @param object A fitted `mvgam` object
#' @param data Frame to predict for
#' @return `data`. A fit `jsdgam()` did not build returns it unchanged.
#' @noRd
jsdgam_axis_columns <- function(object, data) {
  named <- jsdgam_axis_names(object)
  if (is.null(named) || !is.data.frame(data)) {
    return(data)
  }
  vars <- axis_vars(object)
  # A value the training data never held keeps its own label. The
  # check of unseen levels then names it in its error.
  as_fitted <- function(values, column) {
    fitted <- object$data[[column]]
    if (is.factor(fitted) && all(values %in% levels(fitted))) {
      factor(as.character(values), levels = levels(fitted))
    } else {
      values
    }
  }
  unit <- named$unit
  if (!identical(unit, vars$time_var) && unit %in% names(data) &&
        !vars$time_var %in% names(data)) {
    data[[vars$time_var]] <- data[[unit]]
  }
  species <- named$species
  series_var <- vars$series_var
  if (identical(species, series_var) || !species %in% names(data)) {
    return(data)
  }
  if (!series_var %in% names(data)) {
    data[[series_var]] <- as_fitted(data[[species]], series_var)
    return(data)
  }
  by_species <- as.character(data[[species]])
  by_series <- as.character(data[[series_var]])
  if (identical(by_species, by_series)) {
    return(data)
  }
  held <- names(which.max(table(object$data[[species]])))
  varied <- ifelse(by_species == held, by_series, by_species)
  data[[species]] <- as_fitted(varied, species)
  data[[series_var]] <- as_fitted(varied, series_var)
  data
}


#' The `loadings_prior` that `traits` and `phylo` describe
#'
#' `traits` becomes `features` and `phylo` becomes `distances$phylo`.
#' An explicit `loadings_prior` given with either is refused.
#'
#' @param traits,phylo,loadings_prior The arguments of `jsdgam()`
#' @param species_levels The species, in their fitted order
#' @return A `loadings_prior` list, or `NULL` for the default prior
#' @noRd
build_jsdgam_loadings_prior <- function(traits,
                                        phylo,
                                        loadings_prior,
                                        species_levels) {
  has_alias <- !is.null(traits) || !is.null(phylo)
  if (!is.null(loadings_prior) && has_alias) {
    stop(insight::format_error(c(
      paste0(
        "'loadings_prior' and the 'traits' / 'phylo' aliases were both ",
        "supplied."
      ),
      x = "The aliases build their own 'loadings_prior'."
    )))
  }
  if (!is.null(loadings_prior)) return(loadings_prior)
  if (!has_alias) return(NULL)
  spec <- list()
  if (!is.null(traits)) {
    spec$features <- traits
  }
  if (!is.null(phylo)) {
    spec$distances <- list(
      phylo = jsdgam_phylo_to_dist(phylo, species_levels)
    )
  }
  spec
}


#' A phylogeny as a distance matrix over the species
#'
#' An `ape::phylo` tree gives its cophenetic distances, with a warning
#' once when it is not ultrametric. A numeric matrix passes after a
#' check of its names. `validate_pairwise_distance()` checks symmetry,
#' the zero diagonal and the signs. It rescales to a maximum of 1.
#'
#' @param phylo An `ape::phylo` tree or a numeric distance matrix
#' @param species_levels The species, in their fitted order
#' @return A numeric matrix with species names on both dimensions
#' @noRd
jsdgam_phylo_to_dist <- function(phylo, species_levels) {
  checkmate::assert_character(species_levels, min.len = 2L,
                              any.missing = FALSE)
  if (inherits(phylo, "phylo")) {
    insight::check_if_installed("ape")
    if (!ape::is.ultrametric(phylo)) {
      warn_once(
        c(
          paste0(
            "Phylogeny passed to 'phylo' has unequal root-to-tip ",
            "path lengths."
          ),
          i = "The cophenetic distances are rescaled to a maximum of 1."
        ),
        "jsdgam_non_ultrametric_phylo"
      )
    }
    d <- ape::cophenetic.phylo(phylo)
  } else if (is.matrix(phylo) || is.data.frame(phylo)) {
    d <- as.matrix(phylo)
    if (!is.numeric(d)) {
      stop(insight::format_error(
        "'phylo' matrix must be numeric."
      ))
    }
  } else {
    stop(insight::format_error(c(
      paste0(
        "'phylo' must be an 'ape::phylo' object or a ",
        "numeric distance matrix."
      ),
      x = paste0("Got: '", class(phylo)[1L], "'.")
    )))
  }
  if (is.null(rownames(d)) || is.null(colnames(d))) {
    stop(insight::format_error(c(
      paste0(
        "Phylogenetic distance matrix needs row and column names ",
        "matching the species levels."
      ),
      i = paste0(
        "Set rownames(d) and colnames(d) before passing, or supply ",
        "an 'ape::phylo' object whose tip labels match the ",
        "species levels."
      )
    )))
  }
  missing_sp <- setdiff(species_levels, rownames(d))
  if (length(missing_sp) > 0L) {
    stop(insight::format_error(c(
      "Phylogeny is missing one or more species levels.",
      x = paste0(
        "Missing: ",
        paste0("'", missing_sp, "'", collapse = ", "), "."
      ),
      i = paste0(
        "Every species level must appear as a tip label / row name."
      )
    )))
  }
  d[species_levels, species_levels, drop = FALSE]
}
