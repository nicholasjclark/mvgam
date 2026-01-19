#' @importFrom generics tidy
#' @export
generics::tidy

#' @importFrom generics augment
#' @export
generics::augment


#' Tidy an `mvgam` object's parameter posteriors
#'
#' Get parameters' posterior statistics, implementing the generic `tidy` from
#' the package \pkg{broom}.
#'
#' The parameters are categorized by the column "type". For instance, the
#' intercept of the observation model (i.e. the "formula" arg to `mvgam()`) has
#' the "type" "observation_beta". The possible "type"s are:
#'
#'   * observation_family_extra_param: any extra parameters for your observation
#'     model, e.g. sigma for a gaussian observation model. These parameters are
#'     not directly derived from the latent trend components (contrast to mu).
#'
#'   * observation_beta: betas from your observation model, excluding any
#'     smooths. If your formula was `y ~ x1 + s(x2, bs='cr')`, then your
#'     intercept and `x1`'s beta would be categorized as this.
#'
#'   * random_effect_group_level: Group-level random effects parameters, i.e.
#'     the mean and sd of the distribution from which the specific random
#'     intercepts/slopes are considered to be drawn from.
#'
#'   * random_effect_beta: betas for the individual random intercepts/slopes.
#'
#'   * trend_model_param: parameters from your `trend_model`.
#'
#'   * trend_beta: analog of "observation_beta", but for any `trend_formula`.
#'
#'   * trend_random_effect_group_level: analog of
#'     "random_effect_group_level", but for any `trend_formula`.
#'
#'   * trend_random_effect_beta: analog of "random_effect_beta", but for any
#'     `trend_formula`.
#'
#' Additionally, GP terms can be incorporated in several ways, leading to
#' different "type"s (or absence!):
#'
#'   * `s(bs = "gp")`: No parameters returned.
#'
#'   * `gp()` in `formula`: "type" of "observation_param".
#'
#'   * `gp()` in `trend_formula`: "type" of "trend_formula_param".
#'
#'   * `GP()` in `trend_model`: "type" of "trend_model_param".
#'
#' @param x An object of class `mvgam`.
#'
#' @param probs The desired probability levels of the parameters' posteriors.
#'   Defaults to `c(0.025, 0.5, 0.975)`, i.e. 2.5%, 50%, and 97.5%.
#'
#' @param ... Unused, included for generic consistency only.
#'
#' @returns A `tibble` containing:
#'
#'   * "parameter": The parameter in question.
#'
#'   * "type": The component of the model that the parameter belongs to (see
#'     details).
#'
#'   * "mean": The posterior mean.
#'
#'   * "sd": The posterior standard deviation.
#'
#'   * percentile(s): Any percentiles of interest from these posteriors.
#'
#' @family tidiers
#'
#' @examples
#' \dontrun{
#' set.seed(0)
#' simdat <- sim_mvgam(
#'   T = 100,
#'   n_series = 3,
#'   trend_model = AR(),
#'   prop_trend = 0.75,
#'   family = gaussian()
#' )
#'
#' simdat$data_train$x <- rnorm(nrow(simdat$data_train))
#' simdat$data_train$year_fac <- factor(simdat$data_train$year)
#'
#' mod <- mvgam(
#'   y ~ -1 + s(time, by = series, bs = 'cr', k = 20) + x,
#'   trend_formula = ~ s(year_fac, bs = 're') - 1,
#'   trend_model = AR(cor = TRUE),
#'   family = gaussian(),
#'   data = simdat$data_train,
#'   silent = 2
#' )
#'
#' tidy(mod, probs = c(0.2, 0.5, 0.8))
#' }
#'
#' @export
tidy.mvgam <- function(x, probs = c(0.025, 0.5, 0.975), ...) {
  object <- x
  obj_vars <- variables(object)
  digits <- 2 # TODO: Let user change?
  partialized_mcmc_summary <- purrr::partial(
    mcmc_summary,
    object$model_output,
    ... = ,
    ISB = FALSE, # Matches `x[i]`'s rather than `x`.
    probs = probs,
    digits = digits,
    Rhat = FALSE,
    n.eff = FALSE
  )
  out <- tibble::tibble()

  # Helper to add parameter column from row names before tibble operations
  add_param_col <- function(df, aliases = NULL) {
    df$parameter <- if (!is.null(aliases)) aliases else row.names(df)
    row.names(df) <- NULL
    df
  }

  # Observation family extra parameters --------
  xp_names_all <- obj_vars$observation_pars$orig_name
  # no matches -> length(xp_names) == 0, even if xp_names_all is NULL
  xp_names <- grep("vec", xp_names_all, value = TRUE, invert = TRUE)
  if (length(xp_names) > 0) {
    extra_params_out <- partialized_mcmc_summary(params = xp_names)
    extra_params_out <- add_param_col(extra_params_out)
    extra_params_out <- tibble::add_column(
      extra_params_out,
      type = "observation_family_extra_param",
      .before = 1
    )
    out <- dplyr::bind_rows(out, extra_params_out)
  }
  # END Observation family extra parameters

  # obs non-smoother betas --------
  if (object$mgcv_model$nsdf > 0) {
    obs_beta_name_map <- dplyr::slice_head(
      obj_vars$observation_betas,
      n = object$mgcv_model$nsdf
    ) # df("orig_name", "alias")
    obs_betas_out <- partialized_mcmc_summary(
      params = obs_beta_name_map$orig_name
    )
    obs_betas_out <- add_param_col(obs_betas_out, obs_beta_name_map$alias)
    obs_betas_out <- tibble::add_column(
      obs_betas_out,
      type = "observation_beta",
      .before = 1
    )
    out <- dplyr::bind_rows(out, obs_betas_out)
  }
  # END obs non-smoother betas

  # random effects --------
  # TODO: names for random slopes
  re_param_name_map <- obj_vars$observation_re_params
  if (!is.null(re_param_name_map)) {
    re_params_out <- partialized_mcmc_summary(
      params = re_param_name_map$orig_name
    )
    re_params_out <- add_param_col(re_params_out, re_param_name_map$alias)
    re_params_out <- tibble::add_column(
      re_params_out,
      type = "random_effect_group_level",
      .before = 1
    )
    out <- dplyr::bind_rows(out, re_params_out)

    # specific betas
    for (sp in object$mgcv_model$smooth) {
      if (inherits(sp, "random.effect")) {
        re_label <- sp$label
        betas_all <- obj_vars$observation_betas
        re_beta_idxs <- grep(re_label, betas_all$alias, fixed = TRUE)
        re_beta_name_map <- dplyr::slice(betas_all, re_beta_idxs)
        re_betas_out <- partialized_mcmc_summary(
          params = re_beta_name_map$orig_name
        )
        re_betas_out <- add_param_col(re_betas_out, re_beta_name_map$alias)
        re_betas_out <- tibble::add_column(
          re_betas_out,
          type = "random_effect_beta",
          .before = 1
        )
        out <- dplyr::bind_rows(out, re_betas_out)
      }
    }
  }
  # END random effects

  # GPs --------
  if (!is.null(obj_vars$trend_pars)) {
    tm_param_names_all <- obj_vars$trend_pars$orig_name
    gp_param_names <- grep(
      "^alpha_gp|^rho_gp",
      tm_param_names_all,
      value = TRUE
    )
    if (length(gp_param_names) > 0) {
      gp_params_out <- partialized_mcmc_summary(params = gp_param_names)
      gp_params_out <- add_param_col(gp_params_out)
      # where is GP? can be in formula, trend_formula, or trend_model
      if (grepl("^(alpha|rho)_gp_trend", gp_param_names[[1]])) {
        param_type <- "trend_formula_param"
      } else if (grepl("^(alpha|rho)_gp_", gp_param_names[[1]])) {
        # hmph.
        param_type <- "observation_param"
      } else {
        param_type <- "trend_model_param"
      }
      gp_params_out <- tibble::add_column(
        gp_params_out,
        type = param_type,
        .before = 1
      )
      out <- dplyr::bind_rows(out, gp_params_out)
    }
  }
  # END GPs

  # RW, AR, CAR, VAR, ZMVN --------
  # TODO: split out Sigma for heircor?
  trend_model_name <- ifelse(
    inherits(object$trend_model, "mvgam_trend"),
    object$trend_model$trend_model,
    object$trend_model
  ) # str vs called obj as arg to mvgam
  if (grepl("^VAR|^CAR|^AR|^RW|^ZMVN", trend_model_name)) {
    # theta = MA terms
    # alpha_cor = heirarchical corr term
    # A = VAR auto-regressive matrix
    # Sigma = correlated errors matrix
    # sigma = errors

    # setting up the params to extract
    if (trend_model_name == "VAR") {
      trend_model_params <- c("^A\\[", "^alpha_cor", "^theta", "^Sigma")
    } else if (grepl("^CAR|^AR|^RW", trend_model_name)) {
      cor <- inherits(object$trend_model, "mvgam_trend") &&
        object$trend_model$cor
      sigma_name <- ifelse(cor, "^Sigma", "^sigma")
      trend_model_params <- c("^ar", "^alpha_cor", "^theta", sigma_name)
    } else if (grepl("^ZMVN", trend_model_name)) {
      trend_model_params <- c("^alpha_cor", "^Sigma")
    }

    # extracting the params
    trend_model_params <- paste(trend_model_params, collapse = "|")
    tm_param_names_all <- obj_vars$trend_pars$orig_name
    tm_param_names <- grep(trend_model_params, tm_param_names_all, value = TRUE)
    tm_params_out <- partialized_mcmc_summary(params = tm_param_names)
    tm_params_out <- add_param_col(tm_params_out)
    tm_params_out <- tibble::add_column(
      tm_params_out,
      type = "trend_model_param",
      .before = 1
    )
    out <- dplyr::bind_rows(out, tm_params_out)
  }
  # END RW, AR, CAR, VAR

  # 'None' trend_model with a trend_formula --------
  if (trend_model_name == "None" && !is.null(object$trend_call)) {
    trend_pars_names_all <- obj_vars$trend_pars$orig_name
    trend_pars_names <- grep("sigma", trend_pars_names_all, value = TRUE)
    if (length(trend_pars_names) > 0) {
      trend_params_out <- partialized_mcmc_summary(params = trend_pars_names)
      trend_params_out <- add_param_col(trend_params_out)
      trend_params_out <- tibble::add_column(
        trend_params_out,
        type = "trend_model_param",
        .before = 1
      )
      out <- dplyr::bind_rows(out, trend_params_out)
    }
  }
  # END 'None' trend_model with a trend_formula

  # Piecewise --------
  # TODO: potentially lump into AR section, above; how to handle change points?
  # to lump in, just add an
  # `else if (grepl("^PW", trend_model_name)`, then
  # `trend_model_params <- c("^k_trend", "^m_trend", "^delta_trend")`
  # and change initial grep(ar car var) call
  if (grepl("^PW", trend_model_name)) {
    trend_model_params <- "^k_trend|^m_trend|^delta_trend"
    tm_param_names_all <- obj_vars$trend_pars$orig_name
    tm_param_names <- grep(trend_model_params, tm_param_names_all, value = TRUE)
    tm_params_out <- partialized_mcmc_summary(params = tm_param_names)
    tm_params_out <- add_param_col(tm_params_out)
    tm_params_out <- tibble::add_column(
      tm_params_out,
      type = "trend_model_param",
      .before = 1
    )
    out <- dplyr::bind_rows(out, tm_params_out)
  }
  # END Piecewise

  # Trend formula betas --------
  if (!is.null(object$trend_call) && object$trend_mgcv_model$nsdf > 0) {
    trend_beta_name_map <- dplyr::slice_head(
      obj_vars$trend_betas,
      n = object$trend_mgcv_model$nsdf
    ) # df("orig_name", "alias")
    trend_betas_out <- partialized_mcmc_summary(
      params = trend_beta_name_map$orig_name
    )
    trend_betas_out <- add_param_col(
      trend_betas_out, trend_beta_name_map$alias
    )
    trend_betas_out <- tibble::add_column(
      trend_betas_out,
      type = "trend_beta",
      .before = 1
    )
    out <- dplyr::bind_rows(out, trend_betas_out)
  }
  # END Trend formula betas

  # trend random effects --------
  trend_re_param_name_map <- obj_vars$trend_re_params
  if (!is.null(trend_re_param_name_map)) {
    trend_re_params_out <- partialized_mcmc_summary(
      params = trend_re_param_name_map$orig_name
    )
    trend_re_params_out <- add_param_col(
      trend_re_params_out, trend_re_param_name_map$alias
    )
    trend_re_params_out <- tibble::add_column(
      trend_re_params_out,
      type = "trend_random_effect_group_level",
      .before = 1
    )
    out <- dplyr::bind_rows(out, trend_re_params_out)

    # specific betas
    for (sp in object$trend_mgcv_model$smooth) {
      if (inherits(sp, "random.effect")) {
        trend_re_label <- sp$label
        trend_betas_all <- obj_vars$trend_betas
        trend_re_beta_idxs <- grep(
          trend_re_label,
          trend_betas_all$alias,
          fixed = TRUE
        )
        trend_re_beta_name_map <- dplyr::slice(
          trend_betas_all,
          trend_re_beta_idxs
        )
        trend_re_betas_out <- partialized_mcmc_summary(
          params = trend_re_beta_name_map$orig_name
        )
        trend_re_betas_out <- add_param_col(
          trend_re_betas_out, trend_re_beta_name_map$alias
        )
        trend_re_betas_out <- tibble::add_column(
          trend_re_betas_out,
          type = "trend_random_effect_beta",
          .before = 1
        )
        out <- dplyr::bind_rows(out, trend_re_betas_out)
      }
    }
  }
  # END trend random effects

  # Cleanup output --------
  # Reorder columns to put parameter first
  out <- out[c("parameter", setdiff(names(out), "parameter"))]

  # Split Sigma in case of hierarchical residual correlations
  alpha_cor_matches <- grep("alpha_cor", out$parameter, fixed = TRUE)
  if (length(alpha_cor_matches) > 0) {
    out <- split_hier_Sigma(object, out)
  }
  # END Cleanup output

  out
}


#' Helper function to split apart Sigma into its constituent sub-matrixes in
#' the case of a hierarchical latent process.
#'
#' The default MCMC output has dummy parameters filling out Sigma to make it
#' an nxn matrix. This removes those, and renames the remaining sub-matrixes
#' to align with the `gr` and `subgr` sizes from `mvgam()`'s `trend_model` argument.
#'
#' @param object An object of class `mvgam`.
#'
#' @param params `tibble` The parameters that are going to be returned by
#'   `tidy.mvgam()`. Assumed that the columns match what `tidy.mvgam()` will return.
#'   Specifically, that there is a "parameter" column.
#'
#' @returns `tibble` The `params`, but with the Sigma parameters split up by `gr`.
#'
#' @noRd
split_hier_Sigma <- function(object, params) {
  params_nonSigma <- dplyr::filter(params, !grepl("^Sigma", parameter))
  params_Sigma <- dplyr::filter(params, grepl("^Sigma", parameter))

  gr <- object$trend_model$gr
  subgr <- object$trend_model$subgr
  gr_levels <- levels(object$obs_data[[gr]])
  subgr_levels <- levels(object$obs_data[[subgr]])
  n_gr <- length(gr_levels)
  n_subgr <- length(subgr_levels)

  # anything besides the dummy params should have non-zero sd
  params_Sigma <- dplyr::filter(params_Sigma, mean != 0, sd != 0)
  index_strs <- sub("Sigma", "", params_Sigma$parameter)[1:(n_subgr**2)]

  # new names
  new_names <- paste0(
    "Sigma_",
    rep(seq_len(n_gr), each = n_subgr**2),
    index_strs
  )
  params_Sigma["parameter"] <- new_names

  dplyr::bind_rows(params_nonSigma, params_Sigma)
}


#' Augment an `mvgam` object's data
#'
#' Add fits and residuals to the data, implementing the generic `augment` from
#' the package \pkg{broom}.
#'
#' A `list` is returned if `class(x$obs_data) == 'list'`, otherwise a `tibble`
#' is returned, but the contents of either object is the same.
#'
#' The arguments `robust` and `probs` are applied to both the fit and residuals
#' calls (see [fitted.mvgam()] and [residuals.mvgam()] for details).
#'
#' @importFrom stats residuals
#'
#' @param x An object of class `mvgam`.
#'
#' @param robust If `FALSE` (the default) the mean is used as the measure of
#'   central tendency and the standard deviation as the measure of variability.
#'   If `TRUE`, the median and the median absolute deviation (MAD) are applied
#'   instead.
#'
#' @param probs The percentiles to be computed by the quantile function.
#'
#' @param ... Unused, included for generic consistency only.
#'
#' @returns A `list` or `tibble` (see details) combining:
#'
#'   * The data supplied to `mvgam()`.
#'
#'   * The outcome variable, named as `.observed`.
#'
#'   * The fitted backcasts, along with their variability and credible bounds.
#'
#'   * The residuals, along with their variability and credible bounds.
#'
#' @seealso
#'   \code{\link{residuals.mvgam}},
#'   \code{\link{fitted.mvgam}}
#'
#' @family tidiers
#'
#' @examples
#' \donttest{
#' set.seed(0)
#' dat <- sim_mvgam(
#'   T = 80,
#'   n_series = 3,
#'   mu = 2,
#'   trend_model = AR(p = 1),
#'   prop_missing = 0.1,
#'   prop_trend = 0.6
#' )
#'
#' mod1 <- mvgam(
#'   formula = y ~ s(season, bs = 'cc', k = 6),
#'   data = dat$data_train,
#'   trend_model = AR(),
#'   family = poisson(),
#'   noncentred = TRUE,
#'   chains = 2,
#'   silent = 2
#' )
#'
#' augment(mod1, robust = TRUE, probs = c(0.25, 0.75))
#' }
#'
#'
#' @export
augment.mvgam <- function(x, robust = FALSE, probs = c(0.025, 0.975), ...) {
  obs_data <- x$obs_data
  obs_data$.observed <- obs_data$y
  obs_data <- purrr::discard_at(
    obs_data,
    c("index..orig..order", "index..time..index")
  )

  resids <- residuals(x, robust = robust, probs = probs) %>%
    tibble::as_tibble()
  fits <- fitted(x, robust = robust, probs = probs) %>%
    tibble::as_tibble()
  hc_fits <- fits %>%
    dplyr::slice_head(n = NROW(resids)) # fits can include fcs
  colnames(resids) <- c(
    ".resid",
    ".resid.variability",
    ".resid.cred.low",
    ".resid.cred.high"
  )
  colnames(hc_fits) <- c(
    ".fitted",
    ".fit.variability",
    ".fit.cred.low",
    ".fit.cred.high"
  )

  augmented <- c(obs_data, hc_fits, resids) # coerces to list
  if (!identical(class(x$obs_data), "list")) {
    # data.frame
    augmented <- tibble::as_tibble(augmented)
  }

  augmented
}
