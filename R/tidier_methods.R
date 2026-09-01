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
#' following categories:
#'
#'   * `"observation_family_extra_param"` -- auxiliary
#'     observation-family parameters such as `sigma`, `shape`,
#'     `nu`, `phi`, `zi`, `hu`.
#'   * `"observation_beta"` -- non-smoother coefficients of the
#'     observation linear predictor (intercepts + non-smooth
#'     fixed effects).
#'   * `"random_effect_group_level"` -- group-level random-effect
#'     parameters (`sd_`, `cor_`, etc.) attached to the
#'     observation formula.
#'   * `"random_effect_beta"` -- the individual random-effect
#'     coefficients attached to the observation formula.
#'   * `"trend_model_param"` -- parameters of the trend dynamics
#'     (`ar1_trend`, `theta_trend`, `Sigma_trend`, `k_trend`,
#'     `m_trend`, `delta_trend`, etc.) including any GP
#'     hyperparameters used inside `trend_model`.
#'   * `"trend_beta"` -- non-smoother coefficients of the trend
#'     linear predictor (`trend_formula`).
#'   * `"trend_random_effect_group_level"` -- group-level
#'     random-effect parameters attached to the trend formula.
#'   * `"trend_random_effect_beta"` -- the individual
#'     random-effect coefficients attached to the trend formula.
#'
#' @param x A fitted `mvgam` object.
#' @param effects Character. One of `"all"` (the default;
#'   returns every parameter), `"fixed"` (only
#'   `observation_beta` + `trend_beta`), `"ran_pars"` (only the
#'   group-level random-effect parameters, observation-family
#'   extras and trend-dynamics parameters) or `"ran_vals"`
#'   (only the individual random-effect coefficients). The
#'   vocabulary matches `broom.mixed::tidy.brmsfit()`.
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
  obs_var_names <- variables(x)
  alias_map <- c(mvgam_beta_aliases(x), mvgam_ranef_aliases(x))

  # Resolve mvgam beta aliases for a vector of raw Stan names.
  # Falls back to the raw name when no alias exists.
  apply_alias <- function(raw) {
    out <- alias_map[raw]
    out[is.na(out)] <- raw[is.na(out)]
    unname(out)
  }

  spec <- tidy_spec(x, obj_vars)
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

  if (length(grep("alpha_cor", out$term, fixed = TRUE)) > 0L &&
        !is.null(x$trend_model$gr)) {
    out <- split_hier_Sigma(x, out)
  }
  out
}


# Internal: the per-block plan that tidy.mvgam walks. Each row
# is one taxonomy bucket the parameter vector falls into,
# together with the broom effects-class it belongs to. Keeping
# this as a single tibble removes the 8+ near-identical blocks
# the original implementation carried around.
#'@noRd
tidy_spec <- function(x, obj_vars) {
  # `enrich_trend_metadata` records the trend type as a single string
  # ("AR", "VAR", "PW", ...); `$trend_model` is read only when that
  # slot is unset.
  meta <- get_enriched_trend_metadata(x)
  trend_model_name <- meta$trend_type %||%
    (if (inherits(x$trend_model, "mvgam_trend"))
       x$trend_model$trend_model else
       as.character(x$trend_model %||% "None"))
  trend_dynamic_pattern <- trend_dynamic_pattern_for(
    x, trend_model_name
  )

  obs_family <- obj_vars$observation_pars$orig_name %||% character(0L)
  obs_family <- grep("vec", obs_family, value = TRUE, invert = TRUE)

  trend_pars_all <- obj_vars$trend_pars$orig_name %||% character(0L)
  trend_dynamic <- if (nzchar(trend_dynamic_pattern)) {
    grep(trend_dynamic_pattern, trend_pars_all, value = TRUE)
  } else if (identical(trend_model_name, "None") &&
              !is.null(x$trend_call)) {
    # 'None' trend with a trend_formula -> only sigma_trend
    grep("sigma", trend_pars_all, value = TRUE)
  } else {
    character(0L)
  }

  obs_beta <- head_betas(x$mgcv_model, obj_vars$observation_betas)
  trend_beta <- if (!is.null(x$trend_call)) {
    head_betas(x$trend_mgcv_model, obj_vars$trend_betas)
  } else character(0L)

  # Smoothness penalties (`sds_*`) are reported as `ran_pars` (the
  # per-smooth variance components); the individual basis-coefficient
  # draws (`s_*` / `zs_*`) are `ran_vals`. Matches brms's
  # `tidy.brmsfit` convention so users get a familiar view.
  obs_smooth_all <- obj_vars$observation_smoothpars$orig_name %||%
    character(0L)
  obs_smooth_sds <- grep("^sds_", obs_smooth_all, value = TRUE)
  obs_smooth_vals <- setdiff(obs_smooth_all, obs_smooth_sds)
  trend_smooth_all <- obj_vars$trend_smoothpars$orig_name %||%
    character(0L)
  trend_smooth_sds <- grep("^sds_", trend_smooth_all, value = TRUE)
  trend_smooth_vals <- setdiff(trend_smooth_all, trend_smooth_sds)

  re_pars <- obj_vars$observation_re_params$orig_name %||%
    character(0L)
  re_beta <- random_effect_beta_names(x, obj_vars)
  trend_re_pars <- obj_vars$trend_re_params$orig_name %||%
    character(0L)
  trend_re_beta <- random_effect_beta_names(x, obj_vars,
                                              which = "trend")

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
      obs_smooth_sds,
      obs_smooth_vals,
      re_pars,
      re_beta,
      trend_dynamic,
      trend_beta,
      trend_smooth_sds,
      trend_smooth_vals,
      trend_re_pars,
      trend_re_beta
    )
  )
}


# Internal: regex of parameter prefixes for the trend dynamics
# parameters of `trend_model_name`. Returns a single regex
# string (alternation-separated) suitable for grep().
#'@noRd
trend_dynamic_pattern_for <- function(x, trend_model_name) {
  has_cor <- inherits(x$trend_model, "mvgam_trend") &&
    isTRUE(x$trend_model$cor)
  if (grepl("^VAR", trend_model_name)) {
    return("^A\\[|^alpha_cor|^theta|^Sigma")
  }
  if (grepl("^CAR|^AR|^RW", trend_model_name)) {
    sigma_name <- if (has_cor) "^Sigma" else "^sigma"
    return(paste(
      c("^ar", "^alpha_cor", "^theta", sigma_name),
      collapse = "|"
    ))
  }
  if (grepl("^ZMVN", trend_model_name)) {
    return("^alpha_cor|^Sigma")
  }
  if (grepl("^PW", trend_model_name)) {
    return("^k_trend|^m_trend|^delta_trend")
  }
  # GP-only / unknown -> nothing here; sigma fallback applies.
  ""
}


# Internal: parametric (non-smoother) betas from an obj_vars block.
# When a fitted `mgcv_model` is available, the first `nsdf` rows are
# the non-smoother coefficients (mgcv convention). Post-brms-
# integration the legacy `mgcv_model` slot is not populated, so we
# fall back to returning every orig_name in `betas_df`. The
# categorize step already excludes smooth-coefficient rows
# (`s_*` / `zs_*` / `sds_*` live in `*_smoothpars` instead).
#'@noRd
head_betas <- function(mgcv_model, betas_df) {
  if (is.null(betas_df) || nrow(betas_df) == 0L) {
    return(character(0L))
  }
  if (!is.null(mgcv_model) && !is.null(mgcv_model$nsdf)) {
    if (mgcv_model$nsdf <= 0L) return(character(0L))
    return(utils::head(betas_df$orig_name, mgcv_model$nsdf))
  }
  betas_df$orig_name
}


# Internal: extract the raw Stan names for individual random-effect
# coefficients from a fitted mgcv model. `which` chooses between
# the observation- and trend-side smooth lists.
#'@noRd
random_effect_beta_names <- function(x, obj_vars,
                                       which = c("obs", "trend")) {
  which <- match.arg(which)
  mgcv_model <- if (which == "obs") x$mgcv_model else
    x$trend_mgcv_model
  betas_all <- if (which == "obs") obj_vars$observation_betas else
    obj_vars$trend_betas
  if (is.null(mgcv_model) || is.null(betas_all)) {
    return(character(0L))
  }
  unlist(lapply(mgcv_model$smooth, function(sp) {
    if (!inherits(sp, "random.effect")) return(character(0L))
    re_label <- sp$label
    idx <- grep(re_label, betas_all$alias, fixed = TRUE)
    betas_all$orig_name[idx]
  }), use.names = FALSE) %||% character(0L)
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


# Internal: in hierarchical residual-correlation models the
# Stan `Sigma` block contains dummy entries that pad it to an
# (n_subgr * n_gr)^2 block-diagonal. This drops the zero
# entries and renames the remaining sub-matrix entries with a
# leading group index `Sigma_<g><i><j>`.
#'@noRd
split_hier_Sigma <- function(x, params) {
  is_sigma <- grepl("^Sigma", params$term)
  if (!any(is_sigma)) return(params)
  non_sigma <- params[!is_sigma, ]
  sigma <- params[is_sigma, ]

  gr <- x$trend_model$gr
  subgr <- x$trend_model$subgr
  n_gr <- length(levels(x$obs_data[[gr]]))
  n_subgr <- length(levels(x$obs_data[[subgr]]))

  # Drop dummy entries (mean and std.error both exactly zero)
  sigma <- sigma[sigma$estimate != 0 | sigma$std.error != 0, ]
  if (nrow(sigma) == 0L) return(non_sigma)
  index_strs <- sub("Sigma", "", sigma$term)[seq_len(n_subgr^2)]
  sigma$term <- paste0(
    "Sigma_",
    rep(seq_len(n_gr), each = n_subgr^2),
    index_strs
  )
  dplyr::bind_rows(non_sigma, sigma)
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
    resp_names <- get_response_names(x)
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
  # `resp` is threaded downstream only when scoped from an mvbf
  # fit; the univariate path leaves it NULL so the brms
  # dispatchers below don't reject it.
  down_resp <- resp
  resp <- mvgam_response_name(x, resp)
  obs_data$.observed <- obs_data[[resp]]
  obs_data <- purrr::discard_at(
    obs_data,
    c("index..orig..order", "index..time..index")
  )

  a <- (1 - conf.level) / 2
  probs <- c(a, 1 - a)

  fit_draws <- stats::fitted(
    x, robust = robust, probs = probs, resp = down_resp,
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
    x, robust = robust, probs = probs, resp = down_resp
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
  if (is_closure_unit_family(x$family)) {
    default_cap <- closure_unit_default_cap(x$family)
    # Multi-season families return `c("series", "site", "time")`;
    # single-season families return NULL and use the 2-axis default.
    arrays <- build_closure_unit_arrays(
      obs_data, response_var = resp,
      default_cap = default_cap,
      unit_grouping_vars = closure_unit_grouping(x$family)
    )
    # `visit_idx[g, 1:n_rep[g]]` gives the obs_data row indices
    # for unit g; invert to a row -> unit map.
    unit_of_visit <- integer(NROW(obs_data))
    for (g in seq_len(arrays$N_unit)) {
      idx <- arrays$visit_idx[g, seq_len(arrays$n_rep[g])]
      unit_of_visit[idx] <- g
    }
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
    resp_names <- get_response_names(x)
    stacked <- lapply(resp_names, function(r) {
      out <- glance.mvgam(x, looic = looic, resp = r, ...)
      out$resp <- r
      out
    })
    return(dplyr::bind_rows(stacked))
  }

  # Per-response family lookup: on mvbf `x$family` is the
  # gaussian placeholder, so scope to the per-arm family when
  # `resp` is supplied.
  fam <- if (!is.null(resp) &&
              inherits(x$formula, "mvbrmsformula")) {
    get_family_for_resp(x, resp)
  } else {
    x$family
  }
  # `resolve_family_name()` returns the user-visible family
  # name even for customfamily objects (e.g. "tweedie" instead
  # of the brms-internal "custom").
  fam_name <- if (inherits(fam, "family")) resolve_family_name(fam) else
    as.character(fam)
  link_name <- if (inherits(fam, "family")) fam$link else
    NA_character_

  resp <- mvgam_response_name(x, resp)
  d <- mvgam_training_data(x)
  out <- tibble::tibble(
    algorithm = glance_algorithm(x),
    pss = posterior::ndraws(posterior::as_draws(x$fit)),
    nobs = sum(!is.na(d[[resp]])),
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


