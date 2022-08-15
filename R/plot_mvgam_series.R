#'Plot observed time series used for mvgam modelling
#'
#'This function takes either a fitted \code{mvgam} object or a \code{data_train} object
#'and produces plots of observed time series, ACF, CDF and histograms for exploratory data analysis
#'
#'@param object Optional \code{list} object returned from \code{mvgam}. Either \code{object} or \code{data_train}
#'must be supplied.
#'@param data_train Optional \code{dataframe} or \code{list} of training data containing at least 'series' and 'time'.
#'Use this argument if training data have been gathered in the correct format for \code{mvgam} modelling
#'but no model has yet been fitted.
#'@param data_test Optional \code{dataframe} or \code{list} of test data containing at least 'series' and 'time'
#'for the forecast horizon, in addition to any other variables included in the linear predictor of \code{formula}. If
#'included, the observed values in the test data are compared to the model's forecast distribution for exploring
#'biases in model predictions.
#'#'@param series Either a \code{integer} specifying which series in the set is to be plotted or
#'the string 'all', which plots all series available in the supplied data
#'@param n_bins \code{integer} specifying the number of bins to use for binning observed values when plotting
#'a the histogram. Default is to use the number of bins returned by a call to `hist` in base `R`
#'@param log_scale \code{logical}. If \code{series == 'all'}, this flag is used to control whether
#'the time series plot is shown on the log scale (using `log(Y + 1)`). This can be useful when
#'visualising many series that may have different observed ranges. Default is \code{FALSE}
#'@author Nicholas J Clark
#'@return A set of base \code{R} graphics plots. If \code{series} is an integer, the plots will
#'show observed time series, autocorrelation and
#'cumulative distribution functions, and a histogram for the series. If \code{series == 'all'},
#'a set of observed time series plots is returned in which all series are shown on each plot but
#'only a single focal series is highlighted, with all remaining series shown as faint gray lines.
#'@export
plot_mvgam_series = function(object, data_train, data_test,
                             series = 1, n_bins,
                             log_scale = FALSE){

  # Check arguments
  if(is.character(series)){
    if(series != 'all'){
      stop('argument "series" must be either a positive integer or "all"',
           call. =  FALSE)
    }
  } else {
    if(sign(series) != 1){
      stop('argument "series" must be either a positive integer or "all"',
           call. = FALSE)
    } else {
      if(series%%1 != 0){
        stop('argument "series" must be either a positive integer or "all"',
             call. = FALSE)
      }
    }
  }

  if(!missing(object)){
    if(!missing(data_train)){
      warning('both "object" and "data_train" were supplied; only using "object"')
    }
    data_train <- object$obs_data
  }

  if(series == 'all'){
    n_plots <- length(levels(data_train$series))
    pages <- 1
    .pardefault <- par(no.readonly=T)
    par(.pardefault)

    if (n_plots > 4) pages <- 2
    if (n_plots > 8) pages <- 3
    if (n_plots > 12) pages <- 4
    if (pages != 0)  {
      ppp <- n_plots %/% pages

      if (n_plots %% pages != 0) {
        ppp <- ppp + 1
        while (ppp * (pages - 1) >= n_plots) pages <- pages - 1
      }

      # Configure layout matrix
      c <- r <- trunc(sqrt(ppp))
      if (c<1) r <- c <- 1
      if (c*r < ppp) c <- c + 1
      if (c*r < ppp) r <- r + 1
      oldpar<-par(mfrow=c(r,c))

    } else { ppp <- 1; oldpar <- par()}

    all_ys <- lapply(seq_len(n_plots), function(x){
      if(log_scale){
        log(      data.frame(y = data_train$y,
                             series = data_train$series,
                             time = data_train$time) %>%
                    dplyr::filter(series == levels(data_train$series)[x]) %>%
                    dplyr::pull(y) + 1)
      } else {
        data.frame(y = data_train$y,
                   series = data_train$series,
                   time = data_train$time) %>%
          dplyr::filter(series == levels(data_train$series)[x]) %>%
          dplyr::pull(y)
      }
    })

    for(i in 1:n_plots){
      s_name <- levels(data_train$series)[i]

      truth <- data.frame(y = data_train$y,
                          series = data_train$series,
                          time = data_train$time) %>%
        dplyr::filter(series == s_name) %>%
        dplyr::select(time, y) %>%
        dplyr::distinct() %>%
        dplyr::arrange(time) %>%
        dplyr::pull(y)

      if(log_scale){
        truth <- log(truth + 1)
        ylab <- 'log(Y + 1)'
      } else {
        ylab <- 'Y'
      }

      plot(1, type = "n", bty = 'L',
           xlab = 'Time',
           ylab = ylab,
           ylim = range(unlist(all_ys), na.rm = TRUE),
           xlim = c(0, length(c(truth))))
      title(s_name, line = 0)

      if(n_plots > 1){
        for(x in 1:n_plots){
          lines(all_ys[[x]], lwd = 1.85, col = 'grey85')
        }
      }

      lines(x = 1:length(truth), y = truth, lwd = 3, col = "white")
      lines(x = 1:length(truth), y = truth, lwd = 2.5, col = "#8F2727")
      box(bty = 'L', lwd = 2)

    }

  } else {
    s_name <- levels(data_train$series)[series]
    truth <- data.frame(y = data_train$y,
                        series = data_train$series,
                        time = data_train$time) %>%
      dplyr::filter(series == s_name) %>%
      dplyr::select(time, y) %>%
      dplyr::distinct() %>%
      dplyr::arrange(time) %>%
      dplyr::pull(y)

    layout(matrix(1:4, nrow = 2, byrow = TRUE))
    if(!missing(data_test)){
      test <- data.frame(y = data_test$y,
                         series = data_test$series,
                         time = data_test$time) %>%
        dplyr::filter(series == s_name) %>%
        dplyr::select(time, y) %>%
        dplyr::distinct() %>%
        dplyr::arrange(time) %>%
        dplyr::pull(y)

      plot(1, type = "n", bty = 'L',
           xlab = 'Time',
           ylab = 'Y',
           ylim = range(c(truth, test), na.rm = TRUE),
           xlim = c(0, length(c(truth, test))))
      title('Time series', line = 0)

      lines(x = 1:length(truth), y = truth, lwd = 2, col = "#8F2727")
      lines(x = (length(truth)+1):length(c(truth, test)), y = test, lwd = 2, col = "black")
      abline(v = length(truth)+1, col = '#FFFFFF60', lwd = 2.85)
      abline(v = length(truth)+1, col = 'black', lwd = 2.5, lty = 'dashed')
      box(bty = 'L', lwd = 2)

      if(missing(n_bins)){
        n_bins <- max(c(length(hist(c(truth, test), plot = F)$breaks),
                        20))
      }

      hist(c(truth, test), border = "#8F2727",
           lwd = 2,
           freq = FALSE,
           breaks = n_bins,
           col = "#C79999",
           ylab = 'Density',
           xlab = 'Count', main = '')
      title('Histogram', line = 0)

      acf(c(truth, test),
          na.action = na.pass, bty = 'L',
          lwd = 2.5, ci.col = 'black', col = "#8F2727",
          main = '', ylab = 'Autocorrelation')
      acf1 <- acf(c(truth, test), plot = F,
                  na.action = na.pass)
      clim <- qnorm((1 + .95)/2)/sqrt(acf1$n.used)
      abline(h = clim,  col = '#FFFFFF', lwd = 2.85)
      abline(h = clim,  col = 'black', lwd = 2.5, lty = 'dashed')
      abline(h = -clim,  col = '#FFFFFF', lwd = 2.85)
      abline(h = -clim,  col = 'black', lwd = 2.5, lty = 'dashed')
      box(bty = 'L', lwd = 2)
      title('ACF', line = 0)

      ecdf_plotdat = function(vals, x){
        func <- ecdf(vals)
        func(x)
      }

      plot_x <- seq(min(c(truth, test), na.rm = T),
                    max(c(truth, test), na.rm = T))
      plot(1, type = "n", bty = 'L',
           xlab = 'Count',
           ylab = 'Empirical CDF',
           xlim = c(min(plot_x), max(plot_x)),
           ylim = c(0, 1))
      title('CDF', line = 0)
      lines(x = plot_x,
            y = ecdf_plotdat(c(truth, test),
                             plot_x),
            col = "#8F2727",
            lwd = 2.5)
      box(bty = 'L', lwd = 2)

    } else {
      plot(1, type = "n", bty = 'L',
           xlab = 'Time',
           ylab = 'Y',
           ylim = range(c(truth), na.rm = TRUE),
           xlim = c(0, length(c(truth))))
      title('Time series', line = 0)

      lines(x = 1:length(truth), y = truth, lwd = 2, col = "#8F2727")
      box(bty = 'L', lwd = 2)

      if(missing(n_bins)){
        n_bins <- max(c(length(hist(c(truth), plot = F)$breaks),
                        20))
      }

      hist(c(truth), border = "#8F2727",
           lwd = 2,
           freq = FALSE,
           breaks = n_bins,
           col = "#C79999",
           ylab = 'Density',
           xlab = 'Count', main = '')
      title('Histogram', line = 0)


      acf(c(truth),
          na.action = na.pass, bty = 'L',
          lwd = 2.5, ci.col = 'black', col = "#8F2727",
          main = '', ylab = 'Autocorrelation')
      acf1 <- acf(c(truth), plot = F,
                  na.action = na.pass)
      clim <- qnorm((1 + .95)/2)/sqrt(acf1$n.used)
      abline(h = clim,  col = '#FFFFFF', lwd = 2.85)
      abline(h = clim,  col = 'black', lwd = 2.5, lty = 'dashed')
      abline(h = -clim,  col = '#FFFFFF', lwd = 2.85)
      abline(h = -clim,  col = 'black', lwd = 2.5, lty = 'dashed')
      box(bty = 'L', lwd = 2)
      title('ACF', line = 0)


      ecdf_plotdat = function(vals, x){
        func <- ecdf(vals)
        func(x)
      }

      plot_x <- seq(min(truth, na.rm = T),
                    max(truth, na.rm = T))
      plot(1, type = "n", bty = 'L',
           xlab = 'Count',
           ylab = 'Empirical CDF',
           xlim = c(min(plot_x), max(plot_x)),
           ylim = c(0, 1))
      title('CDF', line = 0)
      lines(x = plot_x,
            y = ecdf_plotdat(truth,
                             plot_x),
            col = "#8F2727",
            lwd = 2.5)
      box(bty = 'L', lwd = 2)
    }

  }
  layout(1)
}
