# Joint Species Distribution GAMs.
#
# `jsdgam()` is a thin wrapper over `mvgam()` that fits a Joint Species
# Distribution Model with a Heaps-architecture factor model on the
# species loadings. The wrapper accepts the jsdgam signature
# (`formula`, `factor_formula`, `unit`, `species`, `n_lv`, etc.) and
# forwards everything to `mvgam()` via:
#   trend_formula = factor_formula   (`~ -1` resolves to ZMVN())
#   trend_map     = matrix(NA_real_, n_species, n_lv)   (partial-Z full mask)
# Class assignment is `c("mvgam", "jsdgam")` so existing mvgam methods
# inherit and the small jsdgam-specific surfaces (`ordinate.jsdgam`,
# `residual_cor.jsdgam`, the `is_jsdgam` branch in `print.mvgam`) fire
# at the right time.
#
# The wrapper does NOT patch Stan code, does NOT rotate species across
# factors via a modulo trend_map, and does NOT introduce new
# stanvar emission. Identification is handled either by the existing
# Heaps post-hoc QR (when `by = lv_axis()` is absent) or by the env
# constraint and rotate auto-skip (when by = lv_axis() is present in
# `factor_formula`). The user can still supply `loadings_prior` to
# layer the structured trait + phylo prior on top.

#' Fit Joint Species Distribution Models in mvgam
#'
#' Sets up a Joint Species Distribution Model (JSDM) in which the
#' residual associations among species are modelled in a reduced-rank
#' format using a set of latent factors. The factor specification accepts
#' spatial, temporal, or any other type of predictor effects, which enter
#' the latent factors via `factor_formula`, while the
#' observation model itself supports all the smooth, GP and random
#' effects that mvgam can handle. Use `by = lv_axis()` inside smooth
#' or GP terms of `factor_formula` to fit per-latent-factor smooths
#' (constrained ordination).
#'
#' @param formula A `formula` object specifying the GAM observation
#'   model formula. These behave exactly like the formula for a GLM
#'   except that smooth terms (`s()`, `te()`, `ti()`, `t2()`),
#'   nonparametric `gp()` terms and `offset()` can be added to the
#'   right-hand side. Time-varying effects are expressed as
#'   `s(time, by = X)` or `gp(time, by = X)`. `gp()` accepts
#'   both the approximate Hilbert-space form (`gp(x, k = 20)`) and
#'   the exact full-covariance form (`gp(x)`, no `k`). Both fit
#'   fine, but only the approximate form currently supports
#'   prediction at newdata; exact `gp()` terms emit a one-shot
#'   warning to that effect.
#'
#' @param factor_formula A `formula` for the latent factors' linear
#'   predictor. Use `by = lv_axis()` inside `s()`, `te()`, `ti()`,
#'   `t2()`, or `gp()` terms to fit a separate smooth per latent
#'   factor. The legacy spelling `by = trend` is still accepted and
#'   emits a one-time deprecation warning before being rewritten to
#'   `by = lv_axis()` internally. The companion sentinel `lv_axis()`
#'   is documented at `[lv_axis()]`.
#'
#' @param knots An optional `list` of knot values for any smooth
#'   terms, forwarded to [mvgam()] and handled exactly as its own
#'   `knots` argument is. Knot values are named by covariate rather
#'   than by formula, so there is one list for the model.
#'
#' @param data A `data.frame` or `list` containing the response
#'   variable and the covariates referenced by `formula` and
#'   `factor_formula`. Must contain the column named by `unit` (the
#'   sampling-unit index, often `time` or `site`) and the factor
#'   column named by `species` (the response-unit index).
#'
#'   Users arriving from `unmarked`, `ubms`, `flocker`, `Hmsc`, or
#'   `spOccupancy` typically carry their data in multi-dimensional
#'   arrays. Use [pivot_detection_array()] to convert occupancy /
#'   N-mixture detection-history arrays (2D `[J, K]`, 3D `[N, J, K]`,
#'   4D `[N, J, T, K]` multi-season, or named list of `[J, K]`
#'   matrices per species) into the long-format `data` that
#'   `jsdgam()` expects, and [pivot_species_matrix()] for the
#'   Hmsc-style wide `[J, N]` species-composition matrix.
#'
#' @param newdata Optional held-out `data.frame` / `list` of the
#'   same structure as `data`, forwarded to [mvgam()] and
#'   persisted on the returned fit for later prediction.
#'
#' @param family A `family` object specifying the observation
#'   distribution. Supported families are documented in
#'   `mvgam_families`. Defaults to `binomial()`, which is the standard
#'   choice for presence/absence JSDM responses; switch to a count
#'   family (`poisson()`, `nb()`) when modelling counts. For simplex
#'   multi-response families (`diri()`, `multi()`, `categ()`), write
#'   the formula with a per-`species` interaction (e.g.
#'   `y ~ env * species` or the brms-native-style
#'   `y ~ 0 + species + env:species`). All fixed effects shared
#'   across species drop out of the softmax likelihood, so a
#'   formula like `y ~ env` will fit but its `b_Intercept` and
#'   `b_env` coefficients carry no posterior information; a
#'   once-per-session warning fires in that case to point at the
#'   interpretable form. See `?diri` for the identification
#'   constraints that make this work.
#'
#' @param unit The unquoted name of the `numeric/integer` variable
#'   that indexes the sampling unit (typical names: `time` or
#'   `site`). Defaults to `time`.
#'
#' @param species The unquoted name of the `factor` variable that
#'   indexes the different response units (typical name in a JSDM:
#'   `species`). Defaults to `series` to stay consistent with other
#'   mvgam models.
#'
#' @param n_lv `integer`. Number of latent factors to use for
#'   modelling residual associations. Must be `>= 1`. Defaults to
#'   `2`. The upper bound depends on the `loadings_prior`:
#'   \itemize{
#'     \item Default iid Z prior (`student_t(3, 0, 0.5)`) or any
#'       kernel-driven structured prior built from `traits` /
#'       `phylo` / `loadings_prior$distances`: `n_lv` is the
#'       **exact factor count** that enters the likelihood, and
#'       must be **strictly less than** the number of species. The
#'       constraint exists because at `n_lv = n_species`, `Z Z'`
#'       saturates the residual covariance and the per-species
#'       residual variance loses identifiability under HMC,
#'       producing a heavy funnel.
#'     \item Multiplicative gamma process prior
#'       (`loadings_prior = "mgp"` or
#'       `loadings_prior = list(column_shrinkage = "mgp", ...)`):
#'       `n_lv` is a **truncation ceiling**. Set it at or above
#'       the rank you want to admit; the MGP shrinks later columns
#'       of `Z` toward zero by construction so unused columns are
#'       pruned by the prior. Allowed up to `n_lv = n_species`.
#'       Passing `n_lv > n_species` is rejected because the
#'       marginal `Z Z' + diag(Psi^2)` has rank at most `n_species`
#'       and additional columns add no expressive capacity --
#'       tighten `mgp_a2` for stronger shrinkage instead.
#'   }
#'   See [active_factors()] for a posterior summary of how many
#'   columns the data actually used under MGP.
#'
#' @param priors Optional `data.frame` or `brmsprior` vector with
#'   prior overrides. See [default_prior.mvgam_formula()] and
#'   [brms::prior()] for the conventions.
#'
#' @param traits Optional per-species feature `data.frame`, matrix,
#'   or single string referencing a slot in `data2`. Rows correspond
#'   to species and are matched against `levels(data[[species]])` by
#'   the `series` column (if present), rownames, or row order.
#'   Forwarded as `loadings_prior$features`; categorical columns are
#'   one-hot encoded and numerics z-scored downstream by
#'   `encode_loadings_features()`. Mutually exclusive with an
#'   explicit `loadings_prior` argument.
#'
#' @param trait_slopes Optional one-sided `formula` (e.g.
#'   `~ trait1 + trait2`) requesting trait-mediated environmental
#'   slopes, i.e. the Hmsc "fourth corner" regression. Each fixed
#'   term in `formula` becomes a non-linear parameter whose value
#'   is regressed on the supplied traits plus a species-level
#'   random deviation (`(1 | sp | species)` correlating intercept
#'   and slope deviations under LKJ). The trait columns must
#'   already live in `data`, must be constant within species, and
#'   must reference per-species values rather than per-observation
#'   covariates. Smooth specials (`s()`, `gp()`) in `formula` are
#'   rejected because brms's top-level non-linear formula cannot
#'   host them; supply smooths inside `trait_slopes` itself
#'   (e.g. `~ s(trait1)`) where they land in a sub-formula and
#'   brms accepts them. Weakly-informative default priors
#'   (`normal(0, 1)` on each gamma, `student_t(3, 0, 2.5)` on the
#'   species SD) are emitted; user `priors =` rows override on
#'   matching class / coef / nlpar tuples.
#'
#' @param phylo Optional phylogeny. Either an `ape::phylo` object
#'   (in which case `ape::cophenetic.phylo()` produces a pairwise
#'   distance matrix and a non-ultrametric tree triggers a one-time
#'   warning) or a pre-computed numeric distance matrix whose
#'   row/column names match `levels(data[[species]])`. The matrix
#'   is forwarded as `loadings_prior$distances$phylo` and rescaled
#'   to `max(d) = 1` by `validate_pairwise_distance()`. Requires the
#'   \pkg{ape} package when an `ape::phylo` object is supplied.
#'   Mutually exclusive with an explicit `loadings_prior` argument.
#'
#' @param loadings_prior Optional explicit specification of the
#'   structured loadings prior, mirroring the `mvgam()` argument.
#'   When supplied the `traits` and `phylo` aliases must be `NULL`;
#'   see `[mvgam()]` for the accepted field list.
#'
#' @param backend Character string specifying the Stan backend,
#'   either `"cmdstanr"` or `"rstan"`. Forwarded to [mvgam()].
#'   Defaults to `getOption("brms.backend", "cmdstanr")`.
#'
#' @param threads Positive integer or `NULL`. Forwarded to
#'   [`mvgam()`] / `cmdstanr`. With closure-unit families
#'   (`nmix()`, `occ()`) and multi-response families
#'   (`diri()`, `mvn()`, `mvt()`), enables `reduce_sum` threading of
#'   the per-unit lpmf. Combining `threads > 1` with a
#'   `trend_formula` on a brms-native family currently compiles
#'   un-threaded after a one-time warning; see [`mvgam()`] for the
#'   full threading notes and bench guidance.
#' @param run_model **(deprecated)** Logical. Forwarded to `mvgam()`;
#'   when `FALSE`, skips Stan parse / compile / sampling and returns a
#'   stub `mvgam` / `jsdgam` object with `$stancode` and `$standata`
#'   populated but `$fit = NULL`. New code should use [`stancode()`] /
#'   [`standata()`] on an [`mvgam_formula()`] object instead. Emits a
#'   one-time `rlang::warn()` when `FALSE`. Defaults to `TRUE`.
#'
#' @param ... Other arguments forwarded to `mvgam()`. Notable ones
#'   include `data2` (lookup list for string-named `traits` / `phylo`
#'   slots), `algorithm`, `chains`, and `silent`.
#'
#' @return A `list` of class `c("mvgam", "jsdgam")`. The full mvgam
#'   method surface (`summary`, `predict`, `forecast`, `loo`,
#'   `posterior_epred`, etc.) applies; jsdgam-specific surfaces
#'   (`ordinate`, `residual_cor`) recognise the additional class.
#'
#' @seealso [mvgam()], [lv_axis()], [residual_cor()],
#'   [ordinate()], [shared_variation()], [active_factors()],
#'   [compare_loadings()], [methods_md()], [how_to_cite()]. A
#'   worked online article walking through `jsdgam()` with a
#'   negative binomial observation model, the Heaps & Jermyn
#'   (2024) trait-informed loadings prior, ordination biplots
#'   and the MGP shrinkage criterion is at
#'   \url{https://nicholasjclark.github.io/mvgam/articles/jsdgam.html}.
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
#' # Simulate a small closure-unit JSDM: 4 species, 50 sites,
#' # 4 visits, env + elev site covariates, tod_c + effort visit
#' # covariates. Recipe 2L gives both state-level and detection-
#' # level covariates.
#' set.seed(1)
#' simdat <- sim_closure_unit_data(
#'   family    = occ(),
#'   n_species = 4L,
#'   n_sites   = 50L,
#'   n_visits  = 4L,
#'   type      = 2L
#' )
#'
#' # Fit a joint occupancy model: env on the occupancy linear
#' # predictor, tod_c on the detection sub-formula, two latent
#' # factors capturing residual species-species covariation.
#' mod <- jsdgam(
#'   formula        = bf(y ~ env, p ~ tod_c),
#'   factor_formula = ~ -1,
#'   data           = simdat$data_train,
#'   family         = occ(),
#'   n_lv           = 2L,
#'   chains         = 2,
#'   silent         = 2
#' )
#'
#' # `include_betas = FALSE` keeps the printed summary readable
#' # when the model carries many smooth coefficients.
#' summary(mod, include_betas = FALSE)
#'
#' # Marginal env effect on the response (occupancy * detection)
#' # scale. Pass `type = "link"` for the logit-occupancy scale,
#' # which often reads more cleanly for ecologists used to
#' # discussing logit psi directly.
#' conditional_effects(mod)
#'
#' # Inspect the implied residual species-species correlations.
#' residual_cor(mod)
#'
#' # Two-factor ordination biplot. Site scores in latent space
#' # plus species loading arrows.
#' ordinate(mod, rotation = "varimax")
#' }
#'
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

  # Stash silent on a global option so deep validators (the
  # exact-GP notice in particular) can honour `silent >= 2`
  # without threading the arg through every intermediate call.
  call_silent <- list(...)$silent %||% 1L
  old_silent <- options(mvgam.silent = call_silent)
  on.exit(options(old_silent), add = TRUE)

  # NSE capture of unit + species column names. Defaults are bare
  # symbols `time` / `series` so substitute() returns those names.
  unit_chr <- as.character(substitute(unit))
  species_chr <- as.character(substitute(species))
  if (length(unit_chr) != 1L || nchar(unit_chr) == 0L) {
    stop(insight::format_error(
      "'unit' must resolve to a single column name."
    ))
  }
  if (length(species_chr) != 1L || nchar(species_chr) == 0L) {
    stop(insight::format_error(
      "'species' must resolve to a single column name."
    ))
  }

  checkmate::assert_data_frame(data, min.rows = 1L)
  # Accept either plain `formula` or `brms::bf(...)` (`brmsformula`)
  # so detection / dpar sub-formulas (`p ~ visit_cov` for occ() /
  # nmix(); `phi ~ env` for diri()) can be threaded through
  # `mvgam()` downstream.
  checkmate::assert_multi_class(
    formula, c("formula", "brmsformula")
  )
  checkmate::assert_class(factor_formula, "formula")
  checkmate::assert_names(
    names(data),
    must.include = c(unit_chr, species_chr)
  )
  validate_pos_integer(n_lv)

  # Coerce species to factor if it isn't already so n_lv comparisons
  # against nlevels() are well defined.
  if (!is.factor(data[[species_chr]])) {
    data[[species_chr]] <- factor(data[[species_chr]])
  }
  n_species <- nlevels(data[[species_chr]])
  if (n_species < 2L) {
    stop(insight::format_error(c(
      "'jsdgam' requires at least 2 species levels.",
      i = paste0(
        "The '", species_chr, "' column has ", n_species, " level(s)."
      )
    )))
  }
  # Early `n_lv` ceiling gate so jsdgam-side errors mention
  # "species" rather than the canonical mvgam "series". The
  # downstream wrapper-layer call in `make_stan.R` covers the
  # `mvgam()` direct path; the two share `validate_n_lv_ceiling()`
  # so the iid vs MGP rule lives in one place.
  validate_n_lv_ceiling(
    n_lv           = n_lv,
    n_species      = n_species,
    loadings_prior = loadings_prior,
    fit_function   = "jsdgam"
  )

  # Unit must be numeric / integer because mvgam's time axis is.
  if (!is.numeric(data[[unit_chr]]) && !is.integer(data[[unit_chr]])) {
    stop(insight::format_error(c(
      paste0("'", unit_chr, "' must be numeric or integer."),
      i = paste0(
        "Convert via 'data$", unit_chr, " <- as.integer(...)' before",
        " calling jsdgam()."
      )
    )))
  }

  # Promote (unit, species) to the canonical (time, series) columns
  # mvgam expects. The original columns stay attached so downstream
  # surfaces (e.g. ordinate.jsdgam) can read them via unit_chr.
  data_train <- data
  if (!identical(unit_chr, "time")) {
    if ("time" %in% names(data_train)) {
      stop(insight::format_error(c(
        paste0(
          "'data' already contains a 'time' column, but 'unit = ",
          unit_chr, "' was supplied."
        ),
        i = paste0(
          "Drop the 'time' column or set 'unit = time' before calling",
          " 'jsdgam()'."
        )
      )))
    }
    data_train$time <- data_train[[unit_chr]]
  }
  if (!identical(species_chr, "series")) {
    if ("series" %in% names(data_train)) {
      stop(insight::format_error(c(
        paste0(
          "'data' already contains a 'series' column, but 'species = ",
          species_chr, "' was supplied."
        ),
        i = paste0(
          "Drop the 'series' column or set 'species = series' before",
          " calling 'jsdgam()'."
        )
      )))
    }
    data_train$series <- data_train[[species_chr]]
  }
  data_train$series <- factor(data_train$series,
                               levels = levels(data[[species_chr]]))

  # Partial-Z full mask: n_species x n_lv matrix of NAs triggers the
  # factor model via normalise_trend_map() -> ncol(Z) -> n_lv with
  # the standard 'is_factor_model <- n_lv < n_series' gate.
  trend_map_mat <- matrix(NA_real_, nrow = n_species, ncol = as.integer(n_lv))
  rownames(trend_map_mat) <- levels(data_train$series)

  # Resolve the trait + phylogeny aliases into a loadings_prior list
  # before forwarding. The downstream pipeline (normalise_loadings_prior
  # -> make_loadings_prior_stanvars -> generate_factor_model) does the
  # encoding, validation and Stan emission; the helper here only
  # translates user-facing names into that spec.
  dots <- list(...)
  loadings_prior_resolved <- build_jsdgam_loadings_prior(
    traits = traits,
    phylo = phylo,
    loadings_prior = loadings_prior,
    species_levels = levels(data_train$series)
  )

  # Soft warn (once per session) for simplex multi-response families
  # whose `formula` carries no term referencing the species factor.
  # Under the mode-1 + mode-2 constraints any such K-shared coefficient
  # is pulled to near zero by the prior and contributes nothing to the
  # likelihood; the user almost certainly wants per-species fixed
  # effects via `* series` (or the user's species column name) or the
  # brms-native-style `0 + series + env:series`.
  if (is_simplex_response_family(family)) {
    warn_simplex_obs_formula_lacks_species(formula, species_chr)
  }

  # trait_slopes: Hmsc-style trait-mediated environmental response.
  # When supplied, rewrite `formula` into a brms nl formula whose
  # nlpars regress each fixed slope on the traits, plus a shared
  # species-level RE block correlating the intercept and slope
  # deviations under LKJ. Emit weakly-informative default priors on
  # the new nlpars; user-supplied priors merge on top via the
  # existing prior pipeline.
  trait_slopes_priors <- NULL
  if (!is.null(trait_slopes)) {
    validate_trait_slopes(
      trait_slopes = trait_slopes,
      obs_formula  = formula,
      data         = data_train,
      species_chr  = "series"
    )
    # Reason: emit default priors from the ORIGINAL formula (one
    # nlpar per fixed term); the rewritten brms formula's top-level
    # RHS contains the nlpar tokens themselves and would yield
    # spurious b2 / b3 / b4 priors.
    trait_slopes_priors <- default_trait_slopes_priors(formula)
    formula <- build_trait_slopes_formula(
      obs_formula  = formula,
      trait_slopes = trait_slopes,
      species_var  = "series"
    )
  }

  # Forward to mvgam(). Optional args (knots, newdata, priors) only
  # enter the call if the user supplied them so mvgam's own argument
  # defaults handle the missing case. The trend comes from
  # `factor_formula`, which defaults to `~ -1` and so resolves to
  # `ZMVN()`, the correlated latent prior a JSDM wants.
  forward_args <- list(
    formula = formula,
    trend_formula = factor_formula,
    trend_map = trend_map_mat,
    data = data_train,
    family = family,
    backend = backend,
    run_model = run_model
  )
  # Only forward `threads` when the user actually set it; mvgam()
  # defaults the unset case via `getOption("mc.cores", 1)` further
  # down (in stancode.mvgam_formula), and passing NULL trips that
  # default's `assert_int(threads, lower = 1)` check.
  if (!is.null(threads)) {
    forward_args$threads <- threads
  }
  if (!missing(newdata)) forward_args$newdata <- newdata
  if (!missing(knots)) forward_args$knots <- knots
  # Reason: forward under the singular brms convention so the
  # prior table actually reaches the codegen pipeline. mvgam()
  # accepts both forms via normalise_prior_arg_alias() but
  # canonicalising at the call site avoids any chance of the
  # plural surviving into `...` and being dropped.
  # When trait_slopes is set, the wrapper-emitted default nlpar
  # priors go first so user-supplied 'priors =' rows can override
  # them via the existing brms prior-merge semantics (last wins on
  # matching class / coef / nlpar tuples).
  user_prior <- if (!missing(priors)) priors else NULL
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
  # Drop any duplicate loadings_prior coming through ..., since the
  # explicit argument and the alias resolver have already been
  # reconciled by build_jsdgam_loadings_prior().
  dots$loadings_prior <- NULL
  forward_args <- c(forward_args, dots)

  fit <- do.call(mvgam, forward_args)

  # Slot plumbing for the jsdgam-specific forward-compat surfaces.
  # `model_data`, `obs_data`, and `model_spec$is_jsdgam` are the slots
  # `ordinate.jsdgam`, `residual_cor.jsdgam`, and the `is_jsdgam`
  # branch in `print.mvgam` read.
  fit$model_data <- structure(
    data_train,
    prepped_trend_model = list(unit = unit_chr, species = species_chr)
  )
  fit$obs_data <- data_train
  fit$model_spec <- c(fit$model_spec %||% list(), list(is_jsdgam = TRUE))
  fit$jsdgam_call <- call

  # Preserve `mvgam_prefit` if mvgam returned a stub via
  # `run_model = FALSE`, otherwise plain c("mvgam", "jsdgam").
  fit_classes <- if (inherits(fit, "mvgam_prefit")) {
    c("mvgam", "jsdgam", "mvgam_prefit")
  } else {
    c("mvgam", "jsdgam")
  }
  class(fit) <- fit_classes
  fit
}


# Soft-warn (once per session) when a simplex `jsdgam()` call has no
# term in `formula` that interacts with or references the species
# factor. Such formulas put all per-category differentiation onto the
# latent factor `Z`, while the K-shared fixed-effect coefficients
# (e.g. `b_Intercept`, `b_env`) drop out of the softmax likelihood
# and sample from their default `student_t(3, 0, 2.5)` prior with no
# data contribution.
#
# Detects the species factor by looking for the user-supplied
# `species_chr` column name in the formula's term labels via
# `all.vars()`. Returns the formula invariant; only side effect is
# the warning.
#
# @noRd
warn_simplex_obs_formula_lacks_species <- function(formula, species_chr) {
  if (identical(Sys.getenv("TESTTHAT"), "true")) return(invisible(NULL))
  if (!inherits(formula, "formula") && !inherits(formula, "brmsformula")) {
    return(invisible(NULL))
  }
  rhs_formula <- if (inherits(formula, "brmsformula")) {
    formula$formula
  } else {
    formula
  }
  rhs_vars <- tryCatch(
    all.vars(rhs_formula[[length(rhs_formula)]]),
    error = function(e) character(0)
  )
  if (species_chr %in% rhs_vars || "series" %in% rhs_vars) {
    return(invisible(NULL))
  }
  rlang::warn(
    paste0(
      "All fixed effects in 'formula' are shared across categories. ",
      "The factor model 'Z' will carry the per-category differentiation. ",
      "For interpretable per-category fixed effects, consider ",
      "'y ~ env * ", species_chr, "' or ",
      "'y ~ 0 + ", species_chr, " + env:", species_chr, "'."
    ),
    .frequency = "once",
    .frequency_id = "jsdgam_simplex_no_species_interaction"
  )
  invisible(NULL)
}


# Resolve the trait + phylogeny aliases supplied to `jsdgam()` into a
# `loadings_prior` list spec consumed by `normalise_loadings_prior()`.
# - `traits` -> `loadings_prior$features`
# - `phylo`  -> `loadings_prior$distances$phylo`
# `loadings_prior` cannot be combined with the aliases; if both are
# present the function errors. When all three are NULL it returns
# NULL so the default iid prior fires downstream.
#
# @noRd
build_jsdgam_loadings_prior <- function(traits,
                                        phylo,
                                        loadings_prior,
                                        species_levels) {
  has_alias <- !is.null(traits) || !is.null(phylo)
  if (!is.null(loadings_prior) && has_alias) {
    stop(insight::format_error(c(
      paste0(
        "Supply EITHER an explicit 'loadings_prior' OR the ",
        "'traits' / 'phylo' aliases."
      ),
      i = paste0(
        "The aliases are compiled into a 'loadings_prior' spec ",
        "internally; pick one entry point per fit."
      )
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


# Convert a phylogeny supplied via `phylo =` into a pairwise distance
# matrix on the species axis. Accepts:
#   - `ape::phylo` objects: requires the `ape` package; computes
#     `ape::cophenetic.phylo()` and warns once when the tree is not
#     ultrametric (the path-length distances still propagate but the
#     downstream `validate_pairwise_distance()` rescales to max = 1).
#   - Numeric distance matrices: passed through after a names check.
# Downstream `validate_pairwise_distance()` does the symmetry,
# zero-diagonal, non-negativity and rescale checks. This helper only
# bridges between the phylogeny object types and the matrix shape
# downstream expects.
#
# @noRd
jsdgam_phylo_to_dist <- function(phylo, species_levels) {
  checkmate::assert_character(species_levels, min.len = 2L,
                              any.missing = FALSE)
  if (inherits(phylo, "phylo")) {
    insight::check_if_installed("ape")
    if (!ape::is.ultrametric(phylo)) {
      if (!identical(Sys.getenv("TESTTHAT"), "true")) {
        rlang::warn(
          insight::format_message(c(
            paste0(
              "Phylogeny passed to 'phylo' is not ultrametric."
            ),
            i = paste0(
              "Cophenetic distances use raw path lengths; the ",
              "loadings-prior pipeline rescales the matrix to ",
              "max(d) = 1 before constructing the kernel."
            )
          )),
          .frequency = "once",
          .frequency_id = "jsdgam_non_ultrametric_phylo"
        )
      }
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
