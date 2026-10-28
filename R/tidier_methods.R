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
#' Posterior summaries of the parameters of a fitted `mvgam` model, as
#' a tibble with the columns of \pkg{broom.mixed}: `term`, `estimate`,
#' `std.error`, `conf.low` and `conf.high`. A `type` column assigns
#' each parameter to the observation model or the trend.
#'
#' The `type` column places every parameter into one of the
#' following categories, each with the `effects` class it belongs to:
#'
#'   * `"observation_family_extra_param"` (`ran_pars`) -- auxiliary
#'     observation-family parameters such as `sigma`, `shape`,
#'     `nu`, `phi`, `zi`, `hu`.
#'   * `"observation_beta"` (`fixed`) -- population-level
#'     coefficients of the observation formula, with the intercept
#'     and the fixed parts of its smooths and monotonic terms.
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
#'     dynamics (`ar1_trend`, `theta1_trend`, `Sigma_trend`,
#'     `k_trend`, `m_trend`, `delta_trend`, etc.) and the factor
#'     loadings.
#'   * `"trend_beta"`, `"trend_smooth_param"`, `"trend_smooth_coef"`,
#'     `"trend_random_effect_group_level"` and
#'     `"trend_random_effect_beta"` -- the same five blocks of the
#'     trend formula.
#'
#' Fixed-effect terms take the names [fixef.mvgam()] gives them, as
#' `broom.mixed::tidy.brmsfit()` names them: `b_x` is `x` and
#' `b_sigma_x` is `sigma_x`. Every other term keeps the name
#' [variables.mvgam()] lists. brms samples a centred intercept and
#' transforms it to the intercept on the data's scale. The table reports
#' the coefficient once, as `Intercept`.
#'
#' @inheritParams predict.mvgam
#' @param x A fitted `mvgam` object.
#' @param effects `"all"` (the default) for every parameter, or one of
#'   `"fixed"`, `"ran_pars"` and `"ran_vals"` for the categories above,
#'   as in `broom.mixed::tidy.brmsfit()`.
#' @param conf.int Logical. If `TRUE` (the default), include
#'   `conf.low` and `conf.high` posterior quantile columns.
#' @param conf.level Numeric. Probability covered by the
#'   credible interval reported in `conf.low` / `conf.high`.
#'   Defaults to `0.95`.
#' @param rhat Logical. If `TRUE`, add a `rhat` column with
#'   posterior::rhat() values.
#' @param ess Logical. If `TRUE`, add an `ess_bulk` column with
#'   posterior::ess_bulk() values.
#' @param ... Unused. Anything passed here is refused.
#'
#' @return A tibble with one row per parameter and the columns
#'   `term`, `type`, `estimate` and `std.error`. `conf.int = TRUE` adds
#'   `conf.low` and `conf.high`. `rhat` and `ess` add `rhat` and
#'   `ess_bulk`.
#'
#' @family tidiers
#'
#' @examples
#' \dontrun{
#' set.seed(13)
#' simdat <- sim_mvgam(family = poisson(), n_series = 1L,
#'                      n_timepoints = 120L, trend_model = AR())
#' mod <- mvgam(y ~ s(x, k = 5), trend_formula = ~ AR(p = 1),
#'               data    = simdat$data_train,
#'               family  = poisson(),
#'               chains  = 2, silent = 2)
#'
#' # One row per parameter, with broom's column names
#' td <- tidy(mod)
#' td
#'
#' # The `type` column separates the trend's parameters from the
#' # observation model's
#' td[td$type == "trend_model_param", ]
#'
#' # The fixed effects alone, with convergence diagnostics
#' tidy(mod, effects = "fixed", rhat = TRUE, ess = TRUE)
#'
#' # Next, augment() adds fitted values and residuals to the data
#' }
#'
#' @export
tidy.mvgam <- function(x, effects = "all", robust = FALSE,
                        conf.int = TRUE, conf.level = 0.95,
                        rhat = FALSE, ess = FALSE, ...) {
  checkmate::assert_class(x, "mvgam")
  require_fitted_model(x, "tidy")
  effects <- match.arg(
    effects, c("all", "fixed", "ran_pars", "ran_vals")
  )
  checkmate::assert_flag(robust)
  checkmate::assert_flag(conf.int)
  checkmate::assert_number(conf.level, lower = 0, upper = 1)
  checkmate::assert_flag(rhat)
  checkmate::assert_flag(ess)
  rlang::check_dots_empty()

  obj_vars <- categorize_mvgam_parameters(x)
  draws <- stanfit_draws(x$fit, pars = unlist(obj_vars, use.names = FALSE))

  # `categorize_mvgam_parameters()` returns the names Stan wrote. The
  # table takes the names and the parameters `variables()` lists, both
  # from `mvgam_user_pars()`.
  user_map <- mvgam_user_pars(x)
  apply_alias <- function(raw) {
    out <- names(user_map)[match(raw, user_map)]
    out[is.na(out)] <- raw[is.na(out)]
    out
  }

  # The lower-triangular zeros of identified loadings are constant.
  obj_vars$trend_pars <- varying_pars(draws, obj_vars$trend_pars)
  spec <- tidy_spec(obj_vars, user_map)
  spec <- dplyr::filter(spec, .effects_filter(effect, effects))

  out <- purrr::map_dfr(
    seq_len(nrow(spec)),
    function(i) {
      params <- spec$params[[i]]
      if (length(params) == 0L) return(tibble::tibble())
      alias <- apply_alias(params)
      # A fixed effect takes its `fixef()` name, as broom.mixed gives it
      if (spec$effect[i] == "fixed") alias <- fixef_name(alias)
      summarise_param_block(
        draws = draws,
        param_names = params,
        type = spec$type[i],
        alias = alias,
        robust = robust,
        conf.int = conf.int,
        conf.level = conf.level,
        rhat = rhat,
        ess = ess
      )
    }
  )

  # A request matching no parameter keeps the table's columns. An
  # example is `effects = "ran_vals"` on a fit without smooths or
  # group-level terms.
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


#' The parameter blocks `tidy.mvgam()` summarises
#'
#' Each row is one bucket of the parameter taxonomy with the broom
#' effects class it belongs to. Each bucket holds only the parameters
#' `variables()` shows a user.
#'
#' @param obj_vars Named list of parameter buckets from
#'   `categorize_mvgam_parameters()`
#' @param user_map Named character vector mapping user-facing names to
#'   Stan names, from `mvgam_user_pars()`
#' @return Tibble holding `type`, `effect` and the list column `params`
#' @noRd
tidy_spec <- function(obj_vars, user_map) {
  checkmate::assert_list(obj_vars)
  checkmate::assert_character(user_map)
  # Every bucket keeps the parameters `variables()` shows a user. brms
  # samples a centred intercept and back-transforms it to
  # `b_Intercept`. The table reports the coefficient once, on the
  # data's scale.
  visible <- function(pars) {
    # In the order `mvgam_user_pars()` gives, which is brms's
    pars <- unname(user_map[user_map %in% pars])
    pars[mvgam_par_kind(pars) != "intercept"]
  }
  # The penalty of a smooth, the scales of a Gaussian process and the
  # standard deviations and correlations of a group-level block are
  # `ran_pars`. The coefficients they govern are `ran_vals`, as in
  # broom.mixed's tidier for a brms fit.
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


#' Which rows the `effects` choice of `tidy()` keeps
#'
#' @param effect_col Character vector of broom effects classes
#' @param effects Character effects choice, `"all"` or one class
#' @return Logical vector shaped as `effect_col`
#' @noRd
.effects_filter <- function(effect_col, effects) {
  checkmate::assert_character(effect_col)
  checkmate::assert_string(effects)
  if (effects == "all") {
    return(rep(TRUE, length(effect_col)))
  }
  effect_col == effects
}


#' Broom-style summaries of one block of parameters
#'
#' One `posterior::summarise_draws()` pass computes every requested
#' column.
#'
#' @param draws Draws object of the fit
#' @param param_names Character vector of Stan parameter names
#' @param type Character block label for the `type` column
#' @param alias Character vector of user-facing names, one per
#'   parameter, or `NULL` to keep the Stan names
#' @param robust,conf.int,conf.level,rhat,ess As in
#'   `broom_summary_fns()`, which checks them
#' @return Tibble holding `term`, `type`, `estimate` and `std.error`,
#'   then any of `conf.low`, `conf.high`, `rhat` and `ess_bulk`
#' @noRd
summarise_param_block <- function(draws, param_names, type,
                                    alias = NULL,
                                    robust = FALSE,
                                    conf.int = TRUE,
                                    conf.level = 0.95,
                                    rhat = FALSE, ess = FALSE) {
  checkmate::assert_class(draws, "draws")
  checkmate::assert_character(param_names, min.len = 1L)
  checkmate::assert_string(type)
  checkmate::assert_character(alias, len = length(param_names),
                              null.ok = TRUE)
  sub <- posterior::subset_draws(draws, variable = param_names)
  summary_fns <- broom_summary_fns(
    robust = robust, conf.int = conf.int,
    conf.level = conf.level, rhat = rhat, ess = ess
  )
  summ <- without_ess_cap_notice(do.call(
    posterior::summarise_draws, c(list(sub), summary_fns)
  ))
  out <- tibble::tibble(
    term = if (!is.null(alias)) alias else summ$variable,
    type = type
  )
  for (nm in setdiff(names(summ), "variable")) {
    out[[nm]] <- summ[[nm]]
  }
  out
}


#' The named summary functions the tidiers apply
#'
#' `posterior::summarise_draws()` applies them to each parameter
#' block of `tidy()`. Their names also set the columns of the empty
#' table a request matching no parameter returns.
#'
#' @param robust Logical. `TRUE` reports the median and MAD
#' @param conf.int Logical. `TRUE` reports interval bounds
#' @param conf.level Numeric interval probability
#' @param rhat Logical. `TRUE` reports Rhat
#' @param ess Logical. `TRUE` reports the bulk effective sample size
#' @return Named list of summary functions
#' @noRd
broom_summary_fns <- function(robust = FALSE,
                                conf.int = TRUE,
                                conf.level = 0.95,
                                rhat = FALSE, ess = FALSE) {
  checkmate::assert_flag(robust)
  checkmate::assert_flag(conf.int)
  checkmate::assert_number(conf.level, lower = 0, upper = 1)
  checkmate::assert_flag(rhat)
  checkmate::assert_flag(ess)
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
#' The training data with posterior summaries of the fitted value and
#' residual of each row, under \pkg{broom}'s column names.
#'
#' @inheritParams tidy.mvgam
#' @inheritParams forecast.mvgam
#' @param conf.int Logical. `TRUE` (the default) adds the credible
#'   interval columns `.lower`, `.upper`, `.resid.lower` and
#'   `.resid.upper`.
#' @param ... Unused. Anything passed here is refused.
#'
#' @return A tibble (or `list`, when `class(x$data) == "list"`)
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
#'   `.fitted` is the expected count of each visit: `psi * p` for
#'   [occ()] and `lambda * p` for [nmix()]. [residuals.mvgam()]
#'   computes one residual per closure unit, a site in a season, from
#'   the PIT of the summed visit counts. Each visit row of a unit
#'   repeats that residual. The `.unit` column names the unit.
#'
#' @family tidiers
#' @seealso [fitted.mvgam()], [residuals.mvgam()]
#'
#' @examples
#' \dontrun{
#' set.seed(13)
#' simdat <- sim_mvgam(family = poisson(), n_series = 1L,
#'                      n_timepoints = 120L, trend_model = AR())
#' mod <- mvgam(y ~ s(x, k = 5), trend_formula = ~ AR(p = 1),
#'               data    = simdat$data_train,
#'               family  = poisson(),
#'               chains  = 2, silent = 2)
#'
#' # The training data with fitted values and residuals appended
#' aug <- augment(mod)
#' head(aug)
#'
#' # Residuals against fitted values, from one tibble
#' library(ggplot2)
#' ggplot(aug, aes(.fitted, .resid)) +
#'   geom_hline(yintercept = 0, linetype = "dashed") +
#'   geom_point(colour = "#8F2727", alpha = 0.6) +
#'   labs(x = "Fitted", y = "Residual") +
#'   mvgam_theme()
#'
#' # Next, glance() summarises the whole fit in one row
#' }
#'
#' @importFrom stats residuals
#' @export
augment.mvgam <- function(x, robust = FALSE, conf.int = TRUE,
                            conf.level = 0.95, resp = NULL, ...) {
  checkmate::assert_class(x, "mvgam")
  require_fitted_model(x, "augment")
  checkmate::assert_flag(robust)
  checkmate::assert_flag(conf.int)
  checkmate::assert_number(conf.level, lower = 0, upper = 1)
  checkmate::assert_string(resp, null.ok = TRUE)
  rlang::check_dots_empty()

  # A multivariate fit without `resp` gives the rows of every
  # response, stacked, with the response named in `.resp`
  fan <- mv_resp_fan_out(x, resp, combine = stack_response_rows(".resp"))
  if (!is.null(fan)) {
    return(fan)
  }

  obs_data <- x$data
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
  # An ordinal fit predicts a probability per category. Its column
  # takes the expected ordered level, the scale of its residuals and
  # of `posterior_predict()`.
  if (length(dim(fit_draws)) == 3L) {
    fit_draws <- ordinal_category_mean(fit_draws)
  }
  fit_summ <- tibble::as_tibble(summarise_predictions(
    fit_draws, probs = probs, robust = robust
  ))
  resid_summ <- tibble::as_tibble(residuals(
    x, robust = robust, probs = probs, resp = resp
  ))
  # A closure-unit family has one residual per unit and one fitted
  # value per visit. Each visit row repeats its unit's residual. The
  # `.unit` column names the unit. `is_closure_unit_family()` tests
  # for that grain. `uses_closure_unit_layout()` is also TRUE for the
  # multi-response families, whose residuals are per row.
  unit_id <- NULL
  if (is_closure_unit_family(x$family)) {
    arrays <- closure_unit_arrays_for(x, obs_data)
    # The unit of each row, from the array builder. A row whose unit
    # has no observed visit gets `NA`.
    unit_of_visit <- arrays$row_unit
    resid_summ <- resid_summ[unit_of_visit, , drop = FALSE]
    unit_id <- arrays$unit_labels[unit_of_visit]
  }
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
  if (!identical(class(x$data), "list")) {
    augmented <- tibble::as_tibble(augmented)
  }
  augmented
}


#' Model summary for an `mvgam` fit
#'
#' broom-style summary of an `mvgam` fit, one row per response. Reports the
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
#' @return A tibble with one row, or on a multivariate fit given no
#'   `resp` one row per response with a `resp` column. Columns:
#'
#'   * `algorithm` -- the algorithm the fit ran, such as `"sampling"`
#'     or `"meanfield"`.
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
#' @template ex-loo-models
#' @examples
#' \dontrun{
#' # The sampler and data dimensions of the fit, in one row
#' glance(mod)
#'
#' # Leave-one-out criteria take longer to compute
#' glance(mod, looic = TRUE)
#' }
#'
#' @export
glance.mvgam <- function(x, looic = FALSE, resp = NULL, ...) {
  checkmate::assert_class(x, "mvgam")
  checkmate::assert_flag(looic)
  checkmate::assert_string(resp, null.ok = TRUE)
  # The draw count below comes from `$fit`, past the accessor that
  # refuses a model without draws.
  require_fitted_model(x, "glance")
  # `...` reaches `loo()` only under `looic = TRUE`. Checking it
  # against the formals of `loo.mvgam()` on every call refuses a name
  # neither branch uses.
  refuse_unread_dots(list(...), names(formals(loo.mvgam)), "glance")

  # A multivariate fit without `resp` gives one row per response,
  # named in `resp`, in the order of `x$formula$responses`
  fan <- mv_resp_fan_out(x, resp, combine = stack_response_rows("resp"))
  if (!is.null(fan)) {
    return(fan)
  }

  # The family of the response in scope. On an mvbf fit `x$family`
  # is the `family` argument of the call. Each response has its own.
  fam <- model_families(x, resp)
  # `resolve_family_name()` returns the user-visible family
  # name even for customfamily objects (e.g. "tweedie" instead
  # of the brms-internal "custom").
  fam_name <- if (inherits(fam, "family")) resolve_family_name(fam) else
    as.character(fam)
  link_name <- if (inherits(fam, "family")) fam$link else
    NA_character_

  d <- x$data
  out <- tibble::tibble(
    algorithm = x$algorithm,
    pss = posterior::ndraws(x),
    nobs = sum(!is.na(d[[response_column(x, resp)]])),
    nseries = length(fitted_series_levels(x)),
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


#' Stack per-response tables, naming each row's response
#'
#' The `combine` function `mv_resp_fan_out()` takes for a tidier.
#'
#' @param col Name of the column holding the response
#' @return A function of a named list of tables, returning them
#'   stacked with `col` added
#' @noRd
stack_response_rows <- function(col) {
  checkmate::assert_string(col)
  function(tables) {
    dplyr::bind_rows(Map(function(tab, r) {
      tab[[col]] <- r
      tab
    }, tables, names(tables)))
  }
}
