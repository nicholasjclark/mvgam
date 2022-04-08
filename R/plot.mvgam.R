#'Default mvgam plots
#'
#'This function takes a fitted \code{mvjagam} object and produces plots of smooth functions, forecasts, trends and
#'uncertainty components
#'
#'@param object \code{list} object returned from \code{mvjagam}
#'@param type \code{character} specifying which type of plot to return. Options are:
#'residuals,
#'smooths,
#'forecast,
#'trend,
#'uncertainty,
#'factors
#'@param series \code{integer} specifying which series in the set is to be plotted
#'@param smooth_residuals \code{logical}. If \code{TRUE} then posterior quantiles of partial residuals are added
#'to plots of 1-D smooths as a series of ribbon rectangles.
#'@param data_test A \code{dataframe} or \code{list} containing at least 'series' and 'time' for the forecast horizon, in
#'addition to any other variables included in the linear predictor of \code{formula}. This argument is optional when
#'plotting out of sample forecast period observations (when \code{type = forecast}) and required when plotting
#'uncertainty components (\code{type = uncertainty}).
#'@details These plots are useful for getting an overview of the fitted model and its estimated smooth functions,
#'but the individual plotting functions offer more customisation.
#'@seealso \code{\link{plot_mvgam_resids}}, \code{\link{plot_mvgam_smooth}}, \code{\link{plot_mvgam_fc}},
#'\code{\link{plot_mvgam_trend}}, \code{\link{plot_mvgam_uncertainty}}, \code{\link{plot_mvgam_factors}}
#'@author Nicholas J Clark
#'@return A base R plot or set of plots
#'@export
plot.mvgam = function(object, type = 'smooths', series = 1, smooth_residuals = FALSE,
                      data_test){
  type <- match.arg(arg = type, choices = c("residuals", "smooths", "forecast", "trend", "uncertainty",
                                            "factors"))

  if(type == 'residuals'){
    plot_mvgam_resids(object, series = series)
  }

  if(type == 'factors'){
    if(!object$use_lv){
      stop('no latent variables were fitted in the model')
    } else {
      plot_mvgam_resids(object, series = series)
    }
  }

  if(type == 'forecast'){
    if(missing(data_test)){
      plot_mvgam_fc(object, series = series)
    } else {
      plot_mvgam_fc(object, series = series, data_test = data_test)
    }
  }

  if(type == 'trend'){
    if(missing(data_test)){
      plot_mvgam_trend(object, series = series)
    } else {
      plot_mvgam_trend(object, series = series, data_test = data_test)
    }
  }

  if(type == 'uncertainty'){
    if(missing(data_test)){
      stop('data_test is required for plotting uncertainty contributions')
    } else {
      plot_mvgam_uncertainty(object, series = series, data_test = data_test)
    }
  }

  if(type == 'smooths'){

    smooth_labs <- do.call(rbind, lapply(seq_along(object$mgcv_model$smooth), function(x){
      data.frame(label = object$mgcv_model$smooth[[x]]$label, class = class(object$mgcv_model$smooth[[x]])[1])
    }))
    n_smooths <- NROW(smooth_labs)
    if(n_smooths == 0) stop("No terms to plot - nothing for plot.mvgam() to do.")

    # Check which ones plot_mvgam_smooth can handle (no ore than 2 dimensions)
    plottable = function(x){
      length(unlist(strsplit(x, ','))) <= 2
    }
    which_to_plot <- (1:n_smooths)[sapply(smooth_labs$label, plottable)]
    n_smooths <- length(which_to_plot)


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

    # now figure out number of rows and columns
    c <- r <- trunc(sqrt(ppp))
    if (c<1) r <- c <- 1
    if (c*r < ppp) c <- c + 1
    if (c*r < ppp) r <- r + 1
    oldpar<-par(mfrow=c(r,c))

    } else { ppp<-1;oldpar<-par()}

    for(i in 1:n_smooths){
      plot_mvgam_smooth(object = object, smooth = i, series = series,
                        residuals = smooth_residuals)
    }

    invisible()
    par(.pardefault)
  }

}
