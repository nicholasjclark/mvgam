#' MCMC plots of mvgam parameters with bayesplot
#'
#' Draws any \pkg{bayesplot} MCMC plot of the posterior, as
#' [brms::mcmc_plot()] does. Without `variable`, the plot shows the
#' parameters [pairs.mvgam()] shows: the population coefficients, the
#' trend parameters and the family parameters.
#'
#' @importFrom bayesplot color_scheme_set color_scheme_get
#' @inheritParams as.data.frame.mvgam
#' @inheritParams brms::mcmc_plot
#' @param object A fitted `mvgam` object.
#' @param type The plot type, the name of a bayesplot `mcmc_*()` function
#'   without its prefix. `bayesplot::available_mcmc()` lists them. The
#'   default `"intervals"` shows posterior intervals. `"trace"` and
#'   `"rank_overlay"` check mixing. `"nuts_energy"` and the other
#'   `"nuts_*"` types diagnose the sampler.
#' @param ... Further arguments to the bayesplot function, such as
#'   `prob` for `"intervals"` or `true` for the `"recover_*"` types.
#'
#' @return A `ggplot` object, or a `bayesplot_grid` for `"pairs"` and
#'   `"combo"`.
#'
#' @seealso [mvgam_draws] lists the shortcuts `variable` accepts, such as
#'   `"trend_params"`. [pairs.mvgam()] plots the joint posterior. See
#'   the [bayesplot MCMC overview](https://mc-stan.org/bayesplot/reference/MCMC-overview.html).
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
#' # Posterior intervals of the trend parameters
#' mcmc_plot(mod, variable = "trend_params", type = "intervals")
#'
#' # Rank histograms of the same parameters check that the chains mix
#' mcmc_plot(mod, variable = "trend_params", type = "rank_overlay")
#'
#' # Next, pairs() shows how the parameters covary
#' }
#'
#' @family posterior summaries
#' @export
mcmc_plot.mvgam <- function(
  object,
  type = 'intervals',
  variable = NULL,
  regex = FALSE,
  ...
) {
  checkmate::assert_class(object, "mvgam")
  require_fitted_model(object, "mcmc_plot")
  checkmate::assert_string(type)
  checkmate::assert_character(variable, null.ok = TRUE)
  checkmate::assert_flag(regex)

  valid_types <- as.character(bayesplot::available_mcmc(""))
  valid_types <- sub("^mcmc_", "", valid_types)
  if (!type %in% valid_types) {
    stop(insight::format_error(c(
      "'type' must be a bayesplot MCMC plot type.",
      x = paste0("Got: '", type, "'."),
      i = paste0(
        "Available: ",
        paste0("'", valid_types, "'", collapse = ", "), "."
      )
    )), call. = FALSE)
  }

  # The interpretable parameters, as `pairs()` chooses them
  if (is.null(variable)) {
    variable <- default_plot_variables(object)
    regex <- FALSE
  }

  mcmc_fun <- get(paste0("mcmc_", type), asNamespace("bayesplot"))
  mcmc_arg_names <- names(formals(mcmc_fun))
  mcmc_args <- list(...)
  # bayesplot would ignore a name outside the function's formals
  refuse_unread_dots(mcmc_args, mcmc_arg_names, "mcmc_plot")
  # The sampler parameters are the `x` of a `nuts_*` type and the `np`
  # of a type that marks divergences
  need_np <- ("x" %in% mcmc_arg_names && grepl("^nuts_", type)) ||
    "np" %in% mcmc_arg_names
  np <- if (need_np) nuts_params(object) else NULL
  if ("x" %in% mcmc_arg_names) {
    if (grepl("^nuts_", type)) {
      mcmc_args$x <- np
    } else {
      draws <- as.array(object, variable = variable, regex = regex)
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
          i = "Name two parameters in 'variable'."
        )), call. = FALSE)
      }
      if (type == 'pairs' && length(sel_variables) == 1L) {
        stop(insight::format_error(c(
          "'type = pairs' requires two or more parameters.",
          x = paste0(
            "Selected: ",
            paste0("'", sel_variables, "'", collapse = ", "), "."
          ),
          i = "Name more parameters in 'variable'."
        )), call. = FALSE)
      }
      mcmc_args$x <- draws
    }
  }
  if ("np" %in% mcmc_arg_names) {
    mcmc_args$np <- np
  }
  if ("lp" %in% mcmc_arg_names) {
    mcmc_args$lp <- log_posterior(object)
  }
  interval_type <- type %in% c("intervals", "areas")
  if ("rhat" %in% mcmc_arg_names && !interval_type) {
    mcmc_args$rhat <- rhat(object)
  }
  if ("ratio" %in% mcmc_arg_names) {
    mcmc_args$ratio <- neff_ratio(object)
  }

  with_color_scheme(do.call(mcmc_fun, args = mcmc_args))
}

#' @export
#' @importFrom brms mcmc_plot
brms::mcmc_plot
