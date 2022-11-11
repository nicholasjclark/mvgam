#'Default mvgam plots
#'
#'This function takes a fitted \code{mvgam} object and produces plots of smooth functions, forecasts, trends and
#'uncertainty components
#'
#'@param object \code{list} object returned from \code{mvgam}
#'@param type \code{character} specifying which type of plot to return. Options are:
#''series,
#'residuals,
#'smooths,
#'re (random effect smooths),
#'pterms (parametric effects),
#'forecast,
#'trend,
#'uncertainty,
#'factors
#'@param series \code{integer} specifying which series in the set is to be plotted. This is ignored
#'if \code{type == 're'}
#'@param newdata Optional \code{dataframe} or \code{list} of test data containing at least 'series' and 'time'
#'in addition to any other variables included in the linear predictor of the original \code{formula}.
#'This argument is optional when plotting out of sample forecast period observations
#'(when \code{type = forecast}) and required when plotting
#'uncertainty components (\code{type = uncertainty}).
#'@param data_test Deprecated. Still works in place of \code{newdata} but users are recommended to use
#'\code{newdata} instead for more seamless integration into `R` workflows
#'@param ... Additional arguments for each individual plotting function.
#'@details These plots are useful for getting an overview of the fitted model and its estimated
#'random effects or smooth functions,
#'but the individual plotting functions generally offer more customisation.
#'@seealso \code{\link{plot_mvgam_resids}}, \code{\link{plot_mvgam_smooth}}, \code{\link{plot_mvgam_fc}},
#'\code{\link{plot_mvgam_trend}}, \code{\link{plot_mvgam_uncertainty}}, \code{\link{plot_mvgam_factors}},
#'\code{\link{plot_mvgam_randomeffects}}
#'@author Nicholas J Clark
#'@return A base R plot or set of plots
#'@export
plot.mvgam = function(object, type = 'residuals',
                      series = 1, residuals = FALSE,
                      newdata, data_test, ...){

  # Argument checks
  type <- match.arg(arg = type, choices = c("residuals", "smooths", "re",
                                            "pterms", "forecast", "trend",
                                            "uncertainty", "factors", "series"))

  if(class(object) != 'mvgam'){
    stop('argument "object" must be of class "mvgam"')
  }

  if(!missing("newdata")){
    data_test <- newdata
  }

  # Other errors and warnings will propagate from individual functions below
  if(type == 'series'){
    plot_mvgam_series(object, series = series, data_test = data_test, ...)
  }

  if(type == 're'){
    plot_mvgam_randomeffects(object, ...)
  }

  if(type == 'pterms'){
    plot_mvgam_pterms(object, ...)
  }

  if(type == 'residuals'){
    plot_mvgam_resids(object, series = series, data_test = data_test, ...)
  }

  if(type == 'factors'){
    if(!object$use_lv){
      stop('no latent variables were fitted in the model')
    } else {
      plot_mvgam_factors(object)
    }
  }

  if(type == 'forecast'){
    if(missing(data_test)){
      plot_mvgam_fc(object, series = series, ...)
    } else {
      plot_mvgam_fc(object, series = series, data_test = data_test, ...)
    }
  }

  if(type == 'trend'){
    if(missing(data_test)){
      plot_mvgam_trend(object, series = series, ...)
    } else {
      plot_mvgam_trend(object, series = series, data_test = data_test, ...)
    }
  }

  if(type == 'uncertainty'){
    if(missing(data_test)){
      stop('data_test is required for plotting uncertainty contributions')
    } else {
      plot_mvgam_uncertainty(object, series = series, data_test = data_test, ...)
    }
  }

  if(type == 'smooths'){

    # Get labels of all included smooths from the object
    smooth_labs <- do.call(rbind, lapply(seq_along(object$mgcv_model$smooth), function(x){
      data.frame(label = object$mgcv_model$smooth[[x]]$label,
                 class = class(object$mgcv_model$smooth[[x]])[1],
                 mgcv_plottable = object$mgcv_model$smooth[[x]]$plot.me)
    }))
    n_smooths <- NROW(smooth_labs)
    smooth_labs$smooth_index <- 1:NROW(smooth_labs)
    if(n_smooths == 0) stop("No terms to plot - nothing for plot.mvgam() to do.")

    # Leave out random effects and MRF smooths, and any others that are not
    # considered plottable by mgcv
    smooth_labs %>%
      dplyr::filter(class != 'random.effect') %>%
      dplyr::filter(class != 'mrf.smooth') %>%
      dplyr::filter(mgcv_plottable) -> smooth_labs

    if(length(smooth_labs$label) == 0){
      stop("No terms to plot - nothing for plot.mvgam() to do.")
    }

    # Check which ones plot_mvgam_smooth can handle (no more than 2 dimensions)
    plottable = function(x){
      length(unlist(strsplit(x, ','))) <= 2 &
        length(unlist(strsplit(x, ':'))) <= 2
    }
    which_to_plot <- (smooth_labs$smooth_index)[sapply(as.character(smooth_labs$label), plottable)]
    n_smooths <- length(which_to_plot)

    # For remaining plots, get the needed page numbers
    n_plots <- n_smooths
    if (n_plots==0) stop("No suitable terms to plot - plot.mvgam() only handles smooths of 2 or fewer dimensions.")
    pages <- 1
    .pardefault <- par(no.readonly=T)
    par(.pardefault)

    if (n_plots > 4) pages <- 2
    if (n_plots > 8) pages <- 3
    if (n_plots > 12) pages <- 4
    if (pages != 0)  {
      ppp <- n_plots %/% pages

      if (n_plots %% pages != 0) {
        ppp<-ppp+1
        while (ppp*(pages-1)>=n_plots) pages<-pages-1
        }

    # Configure layout matrix
    c <- r <- trunc(sqrt(ppp))
    if (c<1) r <- c <- 1
    if (c*r < ppp) c <- c + 1
    if (c*r < ppp) r <- r + 1
    oldpar<-par(mfrow=c(r,c))

    } else { ppp<-1;oldpar<-par()}

    # Plot the smooths
    for(i in which_to_plot){
      plot_mvgam_smooth(object = object, smooth = i, series = series,
                        residuals = residuals, ...)
    }

    invisible()
    par(.pardefault)
    layout(1)
  }

}
