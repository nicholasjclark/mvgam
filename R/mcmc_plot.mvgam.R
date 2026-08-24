#' MCMC plots of \pkg{mvgam} parameters, as implemented in \pkg{bayesplot}
#'
#' Convenient way to call MCMC plotting functions
#' implemented in the \pkg{bayesplot} package for \pkg{mvgam} models
#' @importFrom bayesplot color_scheme_set color_scheme_get
#' @inheritParams brms::mcmc_plot
#' @inheritParams as.data.frame.mvgam
#' @param type The type of the plot.
#'   Supported types are (as names) \code{hist}, \code{dens},
#'   \code{hist_by_chain}, \code{dens_overlay},
#'   \code{violin}, \code{intervals}, \code{areas},
#'   \code{areas_ridges}, \code{combo}, \code{acf},
#'   \code{acf_bar}, \code{trace}, \code{trace_highlight},
#'   \code{scatter}, \code{hex}, \code{pairs}, \code{violin},
#'   \code{rhat}, \code{rhat_hist}, \code{neff}, \code{neff_hist}
#'   and \code{nuts_energy}.
#'   For an overview on the various plot types see
#'   \code{\link[bayesplot:MCMC-overview]{MCMC-overview}}.
#' @return A \code{\link[ggplot2:ggplot]{ggplot}} object
#' that can be further customized using the \pkg{ggplot2} package.
#' @seealso \code{\link{mvgam_draws}} for an overview of some of the shortcut strings
#' that can be used for argument `variable`
#'
#' @examples
#' \donttest{
#' set.seed(13)
#' simdat <- sim_mvgam(family = poisson(), n_series = 1L,
#'                      n_timepoints = 120L, trend_model = AR())
#' mod <- mvgam(y ~ s(x), trend_formula = ~ AR(p = 1),
#'               data    = simdat$data_train,
#'               family  = poisson(),
#'               chains  = 2, silent = 2)
#'
#' # Trend-dynamics intervals (sigma_trend, ar1_trend, ...).
#' mcmc_plot(mod, variable = "trend_params", type = "intervals")
#' }
#'
#' @export
mcmc_plot.mvgam = function(
  object,
  type = 'intervals',
  variable = NULL,
  regex = FALSE,
  use_alias = TRUE,
  ...
) {
  checkmate::assert_class(object, "mvgam")
  checkmate::assert_character(variable, null.ok = TRUE)
  checkmate::assert_logical(regex, len = 1L)
  checkmate::assert_logical(use_alias, len = 1L)

  # Check type validity
  valid_types <- as.character(bayesplot::available_mcmc(""))
  valid_types <- sub("^mcmc_", "", valid_types)
  if (!type %in% valid_types) {
    stop(insight::format_error(c(
      "'type' is not a recognised bayesplot MCMC plot.",
      x = paste0("Got: '", type, "'."),
      i = paste0(
        "Available: ",
        paste0("'", valid_types, "'", collapse = ", "), "."
      )
    )))
  }

  # Default variable set: every brms-named parameter except the bulk
  # per-observation / per-state arrays (latent trend, innovations,
  # posterior predictive draws) and the Stan housekeeping entries
  # (`lp__`, `lprior`). Spline-heavy models otherwise render hundreds
  # of panels.
  if (is.null(variable)) {
    all_vars <- posterior::variables(
      posterior::as_draws_array(object$fit)
    )
    drop_pattern <- paste0(
      "^(trend\\[|innovations_trend\\[|Y_pred\\[|",
      "lp__$|lprior$)"
    )
    variable <- all_vars[!grepl(drop_pattern, all_vars)]
    regex <- FALSE
  }

  # Formal arguments
  mcmc_fun <- get(paste0("mcmc_", type), asNamespace("bayesplot"))
  mcmc_arg_names <- names(formals(mcmc_fun))
  mcmc_args <- list(...)
  # NUTS sampler params are needed for both `x` (nuts_* plot types)
  # and `np` (any plot type that overlays divergences). Compute once.
  need_np <- ("x" %in% mcmc_arg_names && grepl("^nuts_", type)) ||
    "np" %in% mcmc_arg_names
  np <- if (need_np) nuts_params(object) else NULL
  if ("x" %in% mcmc_arg_names) {
    if (grepl("^nuts_", type)) {
      mcmc_args$x <- np
    } else {
      draws <- as.array(
        object,
        variable = variable,
        regex = regex,
        use_alias = use_alias
      )
      sel_variables <- dimnames(draws)$variable
      if (type %in% c("scatter", "hex") && length(sel_variables) != 2L) {
        stop(insight::format_error(c(
          paste0(
            "'type = ", type,
            "' requires exactly two parameters."
          ),
          x = paste0(
            "Selected: ",
            paste0("'", sel_variables, "'", collapse = ", "), "."
          ),
          i = "Restrict via 'variable' or 'regex'."
        )))
      }
      if (type == 'pairs' && length(sel_variables) == 1L) {
        stop(insight::format_error(c(
          "'type = pairs' requires two or more parameters.",
          x = paste0(
            "Selected: ",
            paste0("'", sel_variables, "'", collapse = ", "), "."
          ),
          i = "Widen 'variable' or drop 'regex'."
        )))
      }
      mcmc_args$x <- draws
    }
  }
  if ("np" %in% mcmc_arg_names) {
    mcmc_args$np <- np
  }
  interval_type <- type %in% c("intervals", "areas")
  if ("rhat" %in% mcmc_arg_names && !interval_type) {
    mcmc_args$rhat <- rhat(object)
  }
  if ("ratio" %in% mcmc_arg_names) {
    mcmc_args$ratio <- neff_ratio(object)
  }

  with_color_scheme("red", do.call(mcmc_fun, args = mcmc_args))
}

#' @export
#' @importFrom brms mcmc_plot
brms::mcmc_plot
