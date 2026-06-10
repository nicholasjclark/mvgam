# Joint Species Distribution GAMs.
#
# `jsdgam()` is a thin wrapper over `mvgam()` that fits a Joint Species
# Distribution Model with a Heaps-architecture factor model on the
# species loadings. The wrapper accepts the legacy jsdgam signature
# (`formula`, `factor_formula`, `unit`, `species`, `n_lv`, etc.) and
# forwards everything to `mvgam()` via:
#   trend_formula = factor_formula
#   trend_model   = ZMVN(cor = TRUE, subgr = species_col)
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
# constraint and rotate auto-skip from chunk 0 (when by = lv_axis() is
# present in `factor_formula`). The user can still supply
# `loadings_prior` to layer the structured trait + phylo prior on top.

#' Fit Joint Species Distribution Models in mvgam
#'
#' Sets up a Joint Species Distribution Model (JSDM) in which the
#' residual associations among species are modelled in a reduced-rank
#' format using a set of latent factors. The factor specification is
#' flexible: spatial, temporal, or any other type of predictor effects
#' can enter the latent factors via `factor_formula`, while the
#' observation model itself supports all the smooth, GP and random
#' effects that mvgam can handle. Use `by = lv_axis()` inside smooth
#' or GP terms of `factor_formula` to fit per-latent-factor smooths
#' (constrained ordination).
#'
#' @inheritParams mvgam
#'
#' @param formula A `formula` object specifying the GAM observation
#'   model formula. These behave exactly like the formula for a GLM
#'   except that smooth terms (`s()`, `te()`, `ti()`, `t2()`), time
#'   varying `dynamic()` terms, nonparametric `gp()` terms and
#'   `offset()` can be added to the right-hand side. See
#'   `mvgam_formulae` for details.
#'
#' @param factor_formula A `formula` for the latent factors' linear
#'   predictor. Use `by = lv_axis()` inside `s()`, `te()`, `ti()`,
#'   `t2()`, or `gp()` terms to fit a separate smooth per latent
#'   factor. The legacy spelling `by = trend` is still accepted and
#'   emits a one-time deprecation warning before being rewritten to
#'   `by = lv_axis()` internally. The companion sentinel `lv_axis()`
#'   is documented at `[lv_axis()]`.
#'
#' @param factor_knots An optional `list` of knot values for any
#'   smooth terms in `factor_formula`, mirroring the role of
#'   `knots` for the observation formula.
#'
#' @param data A `data.frame` or `list` containing the response
#'   variable and the covariates referenced by `formula` and
#'   `factor_formula`. Must contain the column named by `unit` (the
#'   sampling-unit index, often `time` or `site`) and the factor
#'   column named by `species` (the response-unit index).
#'
#' @param family A `family` object specifying the observation
#'   distribution. Supported families are documented in
#'   `mvgam_families`. Defaults to `binomial()`, which is the standard
#'   choice for presence/absence JSDM responses; switch to a count
#'   family (`poisson()`, `nb()`) when modelling counts.
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
#'   modelling residual associations. Must be `>= 1` and strictly
#'   less than the number of species. Defaults to `2`.
#'
#' @param share_obs_params Logical. Forwarded to `mvgam`.
#'
#' @param priors Optional `data.frame` or `brmsprior` vector with
#'   prior overrides. See `[get_mvgam_priors]` and `[brms::prior()]`
#'   for the conventions.
#'
#' @param ... Other arguments forwarded to `mvgam()`. Notable ones
#'   include `loadings_prior` (for trait- and phylogeny-informed
#'   priors on Z), `algorithm`, `chains`, `silent`, `run_model`.
#'
#' @return A `list` of class `c("mvgam", "jsdgam")`. The full mvgam
#'   method surface (`summary`, `predict`, `forecast`, `loo`,
#'   `posterior_epred`, etc.) applies; jsdgam-specific surfaces
#'   (`ordinate`, `residual_cor`) recognise the additional class.
#'
#' @seealso `[mvgam()]`, `[lv_axis()]`, `[residual_cor()]`,
#'   `[ordinate()]`.
#'
#' @references
#' Warton, D. I., Blanchet, F. G., O'Hara, R. B., Ovaskainen, O.,
#' Taskinen, S., Walker, S. C. and Hui, F. K. C. (2015). So many
#' variables: joint modeling in community ecology. *Trends in
#' Ecology and Evolution*, 30(12):766-779. \doi{10.1016/j.tree.2015.09.007}
#'
#' Heaps, S. E. and Jermyn, I. H. (2024). Structured prior
#' distributions for the covariance matrix in latent factor
#' models. *Statistics and Computing*, 34:143.
#' \doi{10.1007/s11222-024-10454-0}
#'
#' @export
jsdgam <- function(formula,
                   factor_formula = ~ -1,
                   knots,
                   factor_knots,
                   data,
                   newdata,
                   family = binomial(),
                   unit = time,
                   species = series,
                   share_obs_params = FALSE,
                   priors,
                   n_lv = 2L,
                   backend = getOption("brms.backend", "cmdstanr"),
                   ...) {
  call <- match.call(expand.dots = FALSE)

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
  checkmate::assert_class(formula, "formula")
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
  if (as.integer(n_lv) >= n_species) {
    stop(insight::format_error(c(
      paste0(
        "'n_lv' must be strictly less than the number of species."
      ),
      i = paste0(
        "Got n_lv = ", n_lv, " and n_species = ", n_species, "."
      )
    )))
  }

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

  # Forward to mvgam(). Optional args (knots, factor_knots, newdata,
  # priors) only enter the call if the user supplied them so mvgam's
  # own argument defaults handle the missing case.
  forward_args <- list(
    formula = formula,
    trend_formula = factor_formula,
    trend_model = ZMVN(cor = TRUE, subgr = "series"),
    trend_map = trend_map_mat,
    data = data_train,
    family = family,
    share_obs_params = share_obs_params,
    backend = backend
  )
  if (!missing(newdata)) forward_args$newdata <- newdata
  if (!missing(knots)) forward_args$knots <- knots
  if (!missing(factor_knots)) forward_args$trend_knots <- factor_knots
  if (!missing(priors)) forward_args$priors <- priors
  forward_args <- c(forward_args, list(...))

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

  class(fit) <- c("mvgam", "jsdgam")
  fit
}
