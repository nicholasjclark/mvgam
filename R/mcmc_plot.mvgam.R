#' MCMC plots as implemented in \pkg{bayesplot}
#'
#' Convenient way to call MCMC plotting functions
#' implemented in the \pkg{bayesplot} package
#' @inheritParams brms::mcmc_plot
#' @inheritParams as.data.frame.mvgam
#' @param type The type of the plot.
#'   Supported types are (as names) \code{hist}, \code{dens},
#'   \code{hist_by_chain}, \code{dens_overlay},
#'   \code{violin}, \code{intervals}, \code{areas}, \code{acf},
#'   \code{acf_bar},\code{trace}, \code{trace_highlight}, \code{scatter},
#'   \code{rhat}, \code{rhat_hist}, \code{neff}, \code{neff_hist}
#'   and \code{nuts_energy}.
#'   For an overview on the various plot types see
#'   \code{\link[bayesplot:MCMC-overview]{MCMC-overview}}.
#' @return A \code{\link[ggplot2:ggplot]{ggplot}} object
#' that can be further customized using the \pkg{ggplot2} package.
#' @export
mcmc_plot.mvgam = function(object,
                           type = 'intervals',
                           variable = NULL,
                           regex = FALSE,
                           use_alias = TRUE,
                           ...){

  # Set red colour scheme
  col_scheme <- attr(bayesplot::color_scheme_get(),
                     'scheme_name')
  bayesplot::color_scheme_set('red')

  # Check type validity
  type <- brms:::as_one_character(type)
  valid_types <- as.character(bayesplot::available_mcmc(""))
  valid_types <- sub("^mcmc_", "", valid_types)
  if (!type %in% valid_types) {
    brms:::stop2("Invalid plot type. Valid plot types are: \n",
                 brms:::collapse_comma(valid_types))
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
    sel_variables <- dimnames(draws)[[2]]
    if(type %in% c("scatter", "hex") && length(sel_variables) != 2L){
      brms:::stop2("Exactly 2 parameters must be selected for this type.",
                   "\nParameters selected: ",
                   brms:::collapse_comma(sel_variables))
    }
    mcmc_args$x <- draws
    }
  }

  if("np" %in% mcmc_arg_names){
    mcmc_args$np <- brms::nuts_params(object$model_output)
  }
  interval_type <- type %in% c("intervals", "areas")
  if("rhat" %in% mcmc_arg_names && !interval_type){
    mcmc_args$rhat <- mcmc_summary(object$model_output)$Rhat
  }
  if("ratio" %in% mcmc_arg_names){
    mcmc_args$ratio <- mcmc_summary(object$model_output)$n.eff /
      dim(mcmc_chains(object$model_output, 'b'))[1]
  }

  # Generate plot and reset colour scheme
  out_plot <- do.call(mcmc_fun, args = mcmc_args)
  bayesplot::color_scheme_set(col_scheme)

  # Return the plot
  return(out_plot)
}

