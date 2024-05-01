#' MCMC plots as implemented in \pkg{bayesplot}
#'
#' Convenient way to call MCMC plotting functions
#' implemented in the \pkg{bayesplot} package
#' @importFrom brms mcmc_plot
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
#' @examples
#' \dontrun{
#' simdat <- sim_mvgam(n_series = 1, trend_model = 'AR1')
#' mod <- mvgam(y ~ s(season, bs = 'cc', k = 6),
#'              trend_model = AR(),
#'              data = simdat$data_train,
#'              burnin = 300,
#'              samples = 300,
#'              chains = 2)
#' mcmc_plot(mod)
#' mcmc_plot(mod, type = 'neff_hist')
#' mcmc_plot(mod, variable = 'betas', type = 'areas')
#' mcmc_plot(mod, variable = 'trend_params', type = 'combo')
#' }
#' @export
mcmc_plot.mvgam = function(object,
                           type = 'intervals',
                           variable = NULL,
                           regex = FALSE,
                           use_alias = TRUE,
                           ...){

  # Set red colour scheme
  col_scheme <- attr(color_scheme_get(),
                     'scheme_name')
  color_scheme_set('red')

  # Check type validity
  valid_types <- as.character(bayesplot::available_mcmc(""))
  valid_types <- sub("^mcmc_", "", valid_types)
  if(!type %in% valid_types) {
    stop("Invalid plot type. Valid plot types are: \n",
                 paste0("'", valid_types, "'", collapse = ", "),
         call. = FALSE)
  }

  # Set default params to plot
  # By default, don't plot the Betas as there can be hundreds
  # of them in spline models
  if (is.null(variable)) {
    all_pars <- variables(object)
    variable <- c(all_pars$observation_pars[,1],
                  all_pars$observation_smoothpars[,1],
                  all_pars$observation_re_params[,1],
                  all_pars$trend_pars[,1],
                  all_pars$trend_smoothpars[,1],
                  all_pars$trend_re_params[,1])
    regex <- FALSE
  }

  # Form arguments
  mcmc_fun <- get(paste0("mcmc_", type), asNamespace("bayesplot"))
  mcmc_arg_names <- names(formals(mcmc_fun))
  mcmc_args <- list(...)
  if("x" %in% mcmc_arg_names){
    if (grepl("^nuts_", type)) {
      # x refers to a molten data.frame of NUTS parameters
      mcmc_args$x <- brms::nuts_params(object$model_output)
    } else {
    # x refers to a data.frame of draws
    draws <- as.array(object, variable = variable, regex = regex,
                      use_alias = use_alias)
    sel_variables <- dimnames(draws)$variable
    if(type %in% c("scatter", "hex") && length(sel_variables) != 2L){
      stop("Exactly 2 parameters must be selected for this type.",
                   "\nParameters selected: ",
                   paste0("'", sel_variables, "'", collapse = ", "),
           call. = FALSE)
    }

    if(type == 'pairs' && length(sel_variables) == 1L){
      stop("2 or more parameters must be selected for this type.",
           "\nParameters selected: ",
           paste0("'", sel_variables, "'", collapse = ", "),
           call. = FALSE)
    }

    mcmc_args$x <- draws
    }
  }

  if("np" %in% mcmc_arg_names){
    mcmc_args$np <- brms::nuts_params(object$model_output)
  }
  interval_type <- type %in% c("intervals", "areas")
  if("rhat" %in% mcmc_arg_names && !interval_type){
    mcmc_args$rhat <- rhat(object)
  }
  if("ratio" %in% mcmc_arg_names){
    mcmc_args$ratio <- neff_ratio(object)
  }

  # Generate plot and reset colour scheme
  out_plot <- do.call(mcmc_fun, args = mcmc_args)
  color_scheme_set(col_scheme)

  # Return the plot
  return(out_plot)
}
