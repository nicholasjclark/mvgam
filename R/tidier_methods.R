#' @importFrom generics tidy
#' @export
generics::tidy

#' @importFrom generics augment
#' @export
generics::augment

#' @importFrom generics glance
#' @export
generics::glance


#' Tidy an `mvgam` object's parameter posteriors
#'
#' Posterior summaries for the fixed and random parameters of a
#' fitted `mvgam` model, returned as a tibble in the column
#' layout established by the \pkg{broom} / \pkg{broom.mixed}
#' package: `term`, `estimate`, `std.error`, `conf.low`,
#' `conf.high`. An additional `type` column carries an
#' `mvgam`-specific categorisation that distinguishes
#' observation-model and trend-model parameters.
#'
#' The `type` column places every parameter into one of the
#' following categories, each with the `effects` class it belongs to:
#'
#'   * `"observation_family_extra_param"` (`ran_pars`) -- auxiliary
#'     observation-family parameters such as `sigma`, `shape`,
#'     `nu`, `phi`, `zi`, `hu`.
#'   * `"observation_beta"` (`fixed`) -- population-level
#'     coefficients of the observation formula, the intercept
#'     included, and the fixed parts of its smooths and monotonic
#'     terms.
#'   * `"observation_smooth_param"` (`ran_pars`) -- the penalty of each
#'     smooth (`sds_`) and the marginal deviation and length scale of
#'     each Gaussian process.
#'   * `"observation_smooth_coef"` (`ran_vals`) -- the penalised basis
#'     coefficients of the smooths and Gaussian processes.
#'   * `"random_effect_group_level"` (`ran_pars`) -- the standard
#'     deviations and correlations of the observation formula's
#'     group-level terms.
#'   * `"random_effect_beta"` (`ran_vals`) -- the group-level
#'     coefficients themselves, one per level and term.
#'   * `"trend_model_param"` (`ran_pars`) -- parameters of the trend
#'     dynamics (`ar1_trend`, `theta_trend`, `Sigma_trend`,
#'     `k_trend`, `m_trend`, `delta_trend`, etc.) and the factor
#'     loadings.
#'   * `"trend_beta"`, `"trend_smooth_param"`, `"trend_smooth_coef"`,
#'     `"trend_random_effect_group_level"` and
#'     `"trend_random_effect_beta"` -- the same five blocks of the
#'     trend formula.
#'
#' brms writes a centred intercept beside the intercept on the data's
#' scale, `b_Intercept`. The table reports the coefficient once, as
#' `b_Intercept`.
#'
#' @param x A fitted `mvgam` object.
#' @param effects Character. One of `"all"` (the default;
#'   returns every parameter), `"fixed"`, `"ran_pars"` or
#'   `"ran_vals"`, selecting the categories above. The vocabulary
#'   matches `broom.mixed::tidy.brmsfit()`.
#' @param robust Logical. If `FALSE` (the default) the posterior
#'   mean and standard deviation are used as the point estimate
#'   and dispersion. If `TRUE` the median and the median
#'   absolute deviation (MAD) are used instead.
#' @param conf.int Logical. If `TRUE` (the default), include
#'   `conf.low` and `conf.high` posterior quantile columns.
#' @param conf.level Numeric. Probability covered by the
#'   credible interval reported in `conf.low` / `conf.high`.
#'   Defaults to `0.95`.
#' @param rhat Logical. If `TRUE`, add a `rhat` column with
#'   posterior::rhat() values.
#' @param ess Logical. If `TRUE`, add an `ess_bulk` column with
#'   posterior::ess_bulk() values.
#' @param ... Unused, included for generic consistency.
#'
#' @return A tibble with one row per parameter and columns
#'   `term`, `type`, `estimate`, `std.error`, optionally
#'   `conf.low` / `conf.high`, and optionally `rhat` /
#'   `ess_bulk`.
#'
#' @family tidiers
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
#' # `tidy()` returns one row per posterior parameter with
#' # broom-standard columns (`term`, `estimate`, `std.error`,
#' # `conf.low`, `conf.high`) for filtering and downstream piping.
#' td <- tidy(mod)
#' td
#'
#' # Add Rhat / effective-sample-size columns when you want to
#' # screen for convergence issues alongside the point estimates.
#' tidy(mod, rhat = TRUE, ess = TRUE)
#' }
#'
#' @export
tidy.mvgam <- function(x, effects = "all", robust = FALSE,
                        conf.int = TRUE, conf.level = 0.95,
                        rhat = FALSE, ess = FALSE, ...) {
  checkmate::assert_class(x, "mvgam")
  effects <- match.arg(
    effects, c("all", "fixed", "ran_pars", "ran_vals")
  )
  checkmate::assert_flag(robust)
  checkmate::assert_flag(conf.int)
  checkmate::assert_number(conf.level, lower = 0, upper = 1)
  checkmate::assert_flag(rhat)
  checkmate::assert_flag(ess)

  obj_vars <- categorize_mvgam_parameters(x)
  draws <- posterior::as_draws_array(x$fit)

  # `categorize_mvgam_parameters()` answers in the names Stan wrote.
  # The table a user reads carries the names `variables()` lists and
  # holds the same parameters: both the labels and the visibility come
  # from `mvgam_user_pars()`, the one projection every reader uses.
  user_map <- mvgam_user_pars(x)
  apply_alias <- function(raw) {
    out <- names(user_map)[match(raw, user_map)]
    out[is.na(out)] <- raw[is.na(out)]
    out
  }

  spec <- tidy_spec(obj_vars, user_map)
  spec <- dplyr::filter(spec, .effects_filter(effect, effects))

  out <- purrr::map_dfr(
    seq_len(nrow(spec)),
    function(i) {
      params <- spec$params[[i]]
      if (length(params) == 0L) return(tibble::tibble())
      summarise_param_block(
        draws = draws,
        param_names = params,
        type = spec$type[i],
        alias = apply_alias(params),
        robust = robust,
        conf.int = conf.int,
        conf.level = conf.level,
        rhat = rhat,
        ess = ess
      )
    }
  )

  # A request no parameter answers, `effects = "ran_vals"` on a fit
  # with no smooths or group-level terms say, keeps the table's columns.
  if (nrow(out) == 0L) {
    fns <- broom_summary_fns(robust = robust, conf.int = conf.int,
                             conf.level = conf.level, rhat = rhat,
                             ess = ess)
    out <- tibble::as_tibble(c(
      list(term = character(0L), type = character(0L)),
      lapply(fns, function(fn) numeric(0L))
    ))
  }
  out
}


# Internal: the per-block plan that tidy.mvgam walks. Each row is one
# bucket of the parameter taxonomy, with the broom effects class it
# belongs to.
#'@noRd
tidy_spec <- function(obj_vars, user_map) {
  # Every bucket is narrowed to the parameters `variables()` shows a
  # user. brms writes a centred intercept beside the `b_Intercept` it
  # is back-transformed into, and the table reports the coefficient
  # once, under the name on the data's own scale.
  visible <- function(pars) {
    pars <- intersect(pars, user_map)
    pars[mvgam_par_kind(pars) != "intercept"]
  }
  # The penalty of a smooth, the scales of a Gaussian process and the
  # standard deviations and correlations of a group-level block are
  # `ran_pars`; the coefficients they govern are `ran_vals`, as
  # broom.mixed reads a brms fit.
  by_role <- function(pars) {
    coef <- mvgam_par_kind(pars) %in%
      c("smooth_coef", "gp_coef", "ranef_coef")
    list(vals = pars[coef], pars = pars[!coef])
  }

  obs_family <- visible(obj_vars$observation_pars)
  trend_dynamic <- visible(obj_vars$trend_pars)
  obs_beta <- visible(obj_vars$observation_betas)
  trend_beta <- visible(obj_vars$trend_betas)
  obs_smooth <- by_role(visible(obj_vars$observation_smoothpars))
  trend_smooth <- by_role(visible(obj_vars$trend_smoothpars))
  obs_re <- by_role(visible(obj_vars$observation_re_params))
  trend_re <- by_role(visible(obj_vars$trend_re_params))

  tibble::tibble(
    type = c(
      "observation_family_extra_param",
      "observation_beta",
      "observation_smooth_param",
      "observation_smooth_coef",
      "random_effect_group_level",
      "random_effect_beta",
      "trend_model_param",
      "trend_beta",
      "trend_smooth_param",
      "trend_smooth_coef",
      "trend_random_effect_group_level",
      "trend_random_effect_beta"
    ),
    effect = c(
      "ran_pars",  # family extras are not "fixed" in broom sense
      "fixed",
      "ran_pars",
      "ran_vals",
      "ran_pars",
      "ran_vals",
      "ran_pars",
      "fixed",
      "ran_pars",
      "ran_vals",
      "ran_pars",
      "ran_vals"
    ),
    params = list(
      obs_family,
      obs_beta,
      obs_smooth$pars,
      obs_smooth$vals,
      obs_re$pars,
      obs_re$vals,
      trend_dynamic,
      trend_beta,
      trend_smooth$pars,
      trend_smooth$vals,
      trend_re$pars,
      trend_re$vals
    )
  )
}


# Internal: vectorised effects-class filter. Returns a logical
# vector of the same length as `effect_col` indicating which
# rows survive the user's `effects` choice.
#'@noRd
.effects_filter <- function(effect_col, effects) {
  if (effects == "all") {
    return(rep(TRUE, length(effect_col)))
  }
  effect_col == effects
}


# Internal: summarise a named subset of parameters using broom
# column conventions. Returns a tibble with columns
#   term, type, estimate, std.error[, conf.low, conf.high]
#   [, rhat, ess_bulk]
#
# All summaries are computed in a single `posterior::summarise_draws()`
# pass over the subset, so the function walks the draws exactly
# once regardless of which optional columns are requested.
#'@noRd
summarise_param_block <- function(draws, param_names, type,
                                    alias = NULL,
                                    robust = FALSE,
                                    conf.int = TRUE,
                                    conf.level = 0.95,
                                    rhat = FALSE, ess = FALSE) {
  sub <- posterior::subset_draws(draws, variable = param_names)
  summary_fns <- broom_summary_fns(
    robust = robust, conf.int = conf.int,
    conf.level = conf.level, rhat = rhat, ess = ess
  )
  summ <- do.call(
    posterior::summarise_draws, c(list(sub), summary_fns)
  )
  out <- tibble::tibble(
    term = if (!is.null(alias)) alias else summ$variable,
    type = type
  )
  for (nm in setdiff(names(summ), "variable")) {
    out[[nm]] <- summ[[nm]]
  }
  out
}


# Internal: build the list of named summary functions
# `posterior::summarise_draws()` will apply. Centralises the
# robust / quantile / convergence choices so tidy.mvgam,
# augment.mvgam and any future tidiers stay aligned on column
# naming.
#'@noRd
broom_summary_fns <- function(robust = FALSE,
                                conf.int = TRUE,
                                conf.level = 0.95,
                                rhat = FALSE, ess = FALSE) {
  fns <- list(
    estimate  = if (robust) stats::median else mean,
    std.error = if (robust) stats::mad else stats::sd
  )
  if (conf.int) {
    a <- (1 - conf.level) / 2
    fns$conf.low  <- function(.x)
      stats::quantile(.x, a, names = FALSE)
    fns$conf.high <- function(.x)
      stats::quantile(.x, 1 - a, names = FALSE)
  }
  if (rhat) fns$rhat <- posterior::rhat
  if (ess) fns$ess_bulk <- posterior::ess_bulk
  fns
}


#' Augment an `mvgam` object's training data with fitted values
#' and residuals
#'
#' Adds posterior summaries of the in-sample fits and
#' residuals to the training data, using broom column names.
#' Each row of the input data is preserved and three (or
#' five, when `conf.int = TRUE`) new columns are appended for
#' each of the fitted and residual posteriors.
#'
#' @param x A fitted `mvgam` object.
#' @param robust Logical. `FALSE` (the default) uses the
#'   posterior mean and standard deviation; `TRUE` uses the
#'   median and the median absolute deviation (MAD).
#' @param conf.int Logical. If `TRUE` (the default), include
#'   `.lower` / `.upper` (for `.fitted`) and `.resid.lower` /
#'   `.resid.upper` (for `.resid`) credible interval columns.
#' @param conf.level Numeric. Probability covered by the
#'   credible intervals. Defaults to `0.95`.
#' @inheritParams forecast.mvgam
#' @param ... Unused, included for generic consistency.
#'
#' @return A tibble (or `list`, when `class(x$obs_data) == "list"`)
#'   with the original training data plus:
#'
#'   * `.observed` -- the response value.
#'   * `.fitted` -- the posterior mean (or median) fitted value.
#'   * `.se.fit` -- the posterior standard deviation (or MAD) of
#'     the fitted value.
#'   * `.lower`, `.upper` -- the lower / upper bound of the
#'     `conf.level` credible interval for `.fitted`. Present only
#'     when `conf.int = TRUE`.
#'   * `.resid` -- the posterior mean (or median) residual.
#'   * `.resid.se` -- the posterior standard deviation (or MAD)
#'     of the residual.
#'   * `.resid.lower`, `.resid.upper` -- the residual credible
#'     interval bounds. Present only when `conf.int = TRUE`.
#'   * `.unit` -- only for closure-unit families (`nmix()`,
#'     `occ()`): the closure-unit ID this row belongs to. Rows
#'     sharing the same `.unit` share the same `.resid*` columns
#'     (see below).
#'
#' @section Closure-unit families (`nmix()`, `occ()`):
#'   `.fitted` is per-visit (`psi * p` for occ, `lambda * p` for
#'   nmix from the per-visit `posterior_epred`). `.resid` is
#'   computed at the closure-unit grain by [residuals.mvgam()]
#'   (one residual per site x season, using a sum-summary PIT
#'   on the per-visit posterior predictive draws) and **recycled**
#'   back to each visit row of the same unit so the augment
#'   output stays aligned with the per-visit training frame. The
#'   `.unit` column makes the recycling explicit: rows sharing
#'   the same `.unit` share the same `.resid*` values. See
#'   `?residuals.mvgam` for the per-unit residual semantics.
#'
#' @family tidiers
#' @seealso [fitted.mvgam()], [residuals.mvgam()]
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
#' # `augment()` returns the training data with fitted, residual,
#' # and (when applicable) closure-unit columns appended. Use it
#' # when you want a single tibble for plotting / scoring against
#' # `.observed` and `.fitted`.
#' aug <- augment(mod)
#' head(aug)
#' cor(aug$.observed, aug$.fitted, use = "complete.obs")
#' }
#'
#' @importFrom stats residuals
#' @export
augment.mvgam <- function(x, robust = FALSE, conf.int = TRUE,
                            conf.level = 0.95, resp = NULL, ...) {
  checkmate::assert_class(x, "mvgam")
  checkmate::assert_flag(robust)
  checkmate::assert_flag(conf.int)
  checkmate::assert_number(conf.level, lower = 0, upper = 1)
  checkmate::assert_string(resp, null.ok = TRUE)

  # Multi-response (mvbrmsformula) fits with no `resp` argument
  # fan out per outcome and stack the tibbles with a `.resp`
  # column so downstream tidy verbs can group / filter by
  # outcome. When `resp` is supplied, the single-response body
  # below runs scoped to that outcome.
  if (is.null(resp) && inherits(x$formula, "mvbrmsformula")) {
    resp_names <- names(response_columns(x))
    stacked <- lapply(resp_names, function(r) {
      out <- augment.mvgam(
        x, robust = robust, conf.int = conf.int,
        conf.level = conf.level, resp = r, ...
      )
      out$.resp <- r
      out
    })
    return(dplyr::bind_rows(stacked))
  }

  obs_data <- mvgam_training_data(x)
  obs_data$.observed <- obs_data[[response_column(x, resp)]]
  obs_data <- purrr::discard_at(
    obs_data,
    c("index..orig..order", "index..time..index")
  )

  a <- (1 - conf.level) / 2
  probs <- c(a, 1 - a)

  fit_draws <- stats::fitted(
    x, robust = robust, probs = probs, resp = resp,
    summary = FALSE
  )
  # An ordinal fit predicts a probability per category, so it has no
  # single fitted value to put in a column. The expected ordered level
  # is that prediction on the scale the residuals and
  # `posterior_predict()` already work on.
  if (length(dim(fit_draws)) == 3L) {
    fit_draws <- ordinal_category_mean(fit_draws)
  }
  fit_summ <- summarize_predictions(
    fit_draws, probs = probs, robust = robust
  ) |>
    tibble::as_tibble()
  resid_summ <- residuals(
    x, robust = robust, probs = probs, resp = resp
  ) |>
    tibble::as_tibble()
  # Closure-unit families return one residual per closure unit
  # (see ?residuals.mvgam). `.fitted` stays per-visit (psi*p or
  # lambda*p from the per-visit posterior_epred), and we recycle
  # the per-unit residual rows back to the per-visit obs frame so
  # every visit row carries its unit's residual. A `.unit` column
  # makes the grain explicit: rows sharing the same `.unit` share
  # the same `.resid*` columns and a user can deduplicate to the
  # unit grain via `dplyr::distinct(out, .unit, .keep_all = TRUE)`.
  unit_id <- NULL
  # The grain `residuals()` answered on, asked with the predicate that
  # names it. `is_closure_unit_family()` is the wire-format question
  # and is TRUE for the multi-response families too, whose residual is
  # per row: recycling those by unit gave every species at a site one
  # another's residual, and the `.unit` column this branch adds says
  # rows sharing a unit share a residual, which was then false.
  if (needs_closure_unit_aggregation(x$family)) {
    # Multi-season families return `c("series", "site", "time")`;
    # single-season families return NULL and use the 2-axis default.
    arrays <- closure_unit_arrays_for(x, obs_data)
    # Which unit each row belongs to, read from the array builder
    # rather than inverted here. Rows whose unit carried no observed
    # visit are `NA`, which is what recycling a per-unit residual
    # back to them should say.
    unit_of_visit <- arrays$row_unit
    resid_summ <- resid_summ[unit_of_visit, , drop = FALSE]
    unit_id <- arrays$unit_labels[unit_of_visit]
  }
  # `fitted.mvgam` may include forecast rows; align with residual
  # length (which is training-only) by slicing.
  fit_summ <- dplyr::slice_head(fit_summ, n = NROW(resid_summ))
  colnames(fit_summ) <- c(".fitted", ".se.fit", ".lower", ".upper")
  colnames(resid_summ) <- c(".resid", ".resid.se",
                              ".resid.lower", ".resid.upper")
  if (!conf.int) {
    fit_summ <- dplyr::select(fit_summ, .fitted, .se.fit)
    resid_summ <- dplyr::select(resid_summ, .resid, .resid.se)
  }

  augmented <- if (is.null(unit_id)) {
    c(obs_data, fit_summ, resid_summ)
  } else {
    c(obs_data, fit_summ, resid_summ, list(.unit = unit_id))
  }
  if (!identical(class(x$obs_data), "list")) {
    augmented <- tibble::as_tibble(augmented)
  }
  augmented
}


#' One-row model summary for an `mvgam` fit
#'
#' broom-style one-row summary of an `mvgam` fit. Reports the
#' Stan algorithm, post-warmup sample size, training-observation
#' count, response family, link function and (optionally) `loo`
#' criteria. Mirrors `broom.mixed::glance.brmsfit()`.
#'
#' @param x A fitted `mvgam` object.
#' @param looic Logical. If `TRUE`, compute leave-one-out
#'   information criteria via [loo.mvgam()] and add `elpd_loo`,
#'   `se_elpd_loo`, `p_loo` and `looic` columns. Defaults to
#'   `FALSE` because computing `loo` can be slow on large fits.
#' @inheritParams forecast.mvgam
#' @param ... Forwarded to [loo.mvgam()] when `looic = TRUE`.
#'
#' @return A one-row tibble with columns:
#'
#'   * `algorithm` -- the Stan algorithm used.
#'   * `pss` -- post-warmup sample size summed across chains.
#'   * `nobs` -- number of (non-missing) training observations.
#'   * `nseries` -- number of unique series in the training data.
#'   * `family` -- observation-family name.
#'   * `link` -- observation-family link.
#'   * Optional: `elpd_loo`, `se_elpd_loo`, `p_loo`, `looic`
#'     (only when `looic = TRUE`).
#'
#' @family tidiers
#' @seealso [tidy.mvgam()], [augment.mvgam()], [loo.mvgam()]
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
#' # `glance()` returns a one-row tibble summarising the fit's
#' # sampling configuration and data dimensions; pass
#' # `looic = TRUE` to add an out-of-sample fit score (slower).
#' glance(mod)
#' glance(mod, looic = TRUE)
#' }
#'
#' @export
glance.mvgam <- function(x, looic = FALSE, resp = NULL, ...) {
  checkmate::assert_class(x, "mvgam")
  checkmate::assert_flag(looic)
  checkmate::assert_string(resp, null.ok = TRUE)

  # Multi-response (mvbrmsformula) fits with no `resp` argument
  # return one row per outcome carrying that outcome's family
  # and link. The row order matches `x$formula$responses`.
  if (is.null(resp) && inherits(x$formula, "mvbrmsformula")) {
    resp_names <- names(response_columns(x))
    stacked <- lapply(resp_names, function(r) {
      out <- glance.mvgam(x, looic = looic, resp = r, ...)
      out$resp <- r
      out
    })
    return(dplyr::bind_rows(stacked))
  }

  # The family of the response in scope. On an mvbf fit `x$family`
  # holds the family given beside the formula, not any response's.
  fam <- model_families(x, resp)
  # `resolve_family_name()` returns the user-visible family
  # name even for customfamily objects (e.g. "tweedie" instead
  # of the brms-internal "custom").
  fam_name <- if (inherits(fam, "family")) resolve_family_name(fam) else
    as.character(fam)
  link_name <- if (inherits(fam, "family")) fam$link else
    NA_character_

  d <- mvgam_training_data(x)
  out <- tibble::tibble(
    algorithm = glance_algorithm(x),
    pss = posterior::ndraws(posterior::as_draws(x$fit)),
    nobs = sum(!is.na(d[[response_column(x, resp)]])),
    nseries = length(resolve_series_info(x)$series_levels),
    family = fam_name,
    link = link_name
  )

  if (looic) {
    l <- loo(x, ...)
    est <- l$estimates
    out$elpd_loo <- est["elpd_loo", "Estimate"]
    out$se_elpd_loo <- est["elpd_loo", "SE"]
    out$p_loo <- est["p_loo", "Estimate"]
    out$looic <- est["looic", "Estimate"]
  }
  out
}


# Internal: Stan algorithm string ('sampling' / 'variational' /
# 'optimizing' / 'fixed_param'), pulled from the underlying
# stanfit when available, otherwise NA.
#'@noRd
glance_algorithm <- function(x) {
  fit <- x$fit
  if (inherits(fit, "stanfit") &&
        length(fit@stan_args) > 0L) {
    algo <- fit@stan_args[[1L]][["method"]]
    return(if (is.null(algo)) NA_character_ else algo)
  }
  NA_character_
}


