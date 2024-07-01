#'Plot observed time series used for mvgam modelling
#'
#'This function takes either a fitted \code{mvgam} object or a \code{data_train} object
#'and produces plots of observed time series, ACF, CDF and histograms for exploratory data analysis
#'
#'@param object Optional \code{list} object returned from \code{mvgam}. Either \code{object} or \code{data_train}
#'must be supplied.
#'@param data Optional \code{dataframe} or \code{list} of training data containing at least 'series' and 'time'.
#'Use this argument if training data have been gathered in the correct format for \code{mvgam} modelling
#'but no model has yet been fitted.
#'@param data_train Deprecated. Still works in place of \code{data} but users are recommended to use
#'\code{data} instead for more seamless integration into `R` workflows
#'@param newdata Optional \code{dataframe} or \code{list} of test data containing at least 'series' and 'time'
#'for the forecast horizon, in addition to any other variables included in the linear predictor of \code{formula}. If
#'included, the observed values in the test data are compared to the model's forecast distribution for exploring
#'biases in model predictions.
#'@param data_test Deprecated. Still works in place of \code{newdata} but users are recommended to use
#'\code{newdata} instead for more seamless integration into `R` workflows
#'@param y Character. What is the name of the outcome variable in the supplied data? Defaults to
#'\code{'y'}
#'@param lines Logical. If \code{TRUE}, line plots are used for visualising time series. If
#'\code{FALSE}, points are used.
#'@param series Either a \code{integer} specifying which series in the set is to be plotted or
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
#'@examples
#'# Simulate and plot series with observations bounded at 0 and 1 (Beta responses)
#'sim_data <- sim_mvgam(family = betar(),
#'                      trend_model = RW(), prop_trend = 0.6)
#'plot_mvgam_series(data = sim_data$data_train, series = 'all')
#'plot_mvgam_series(data = sim_data$data_train,
#'                  newdata = sim_data$data_test, series = 1)
#'
#'# Now simulate series with overdispersed discrete observations
#'sim_data <- sim_mvgam(family = nb(), trend_model = RW(),
#'                      prop_trend = 0.6, phi = 10)
#'plot_mvgam_series(data = sim_data$data_train, series = 'all')
#'@export
plot_mvgam_series = function(object,
                             data,
                             data_train,
                             newdata,
                             data_test,
                             y = 'y',
                             lines = TRUE,
                             series = 1,
                             n_bins,
                             log_scale = FALSE){

  # Check arguments
  if(!missing(object)){
    if (!(inherits(object, "mvgam"))) {
      stop('argument "object" must be of class "mvgam"')
    }
  }

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

  # Check variables in data / data_train
  if(!missing("data")){
    data_train <- data
  }

# Choose models over data if both supplied
  if(!missing(object)){
    if(!missing(data_train)){
      warning('both "object" and "data" were supplied; only using "object"')
    }
    data_train <- object$obs_data

    resp_terms <- as.character(terms(formula(object$call))[[2]])
    if(length(resp_terms) == 1){
      out_name <- as.character(terms(object$call)[[2]])
    } else {
      if(any(grepl('cbind', resp_terms))){
        resp_terms <- resp_terms[-grepl('cbind', resp_terms)]
        out_name <- resp_terms[1]
      }
    }
    y <- out_name
  }

  if(!missing(data_train)){
    # Add series factor variable if missing
    if(!inherits(data_train, 'list')){
      if(!'series' %in% colnames(data_train)){
        data_train$series <- factor('series1')
      }

      # Must be able to index by time; it is too dangerous to 'guess' as this could make a huge
      # impact on resulting estimates / inferences
      if(!'time' %in% colnames(data_train)){
        stop('data does not contain a "time" column')
      }
    }

    if(inherits(data_train, 'list')){
      if(!'series' %in% names(data_train)){
        data_train$series <- factor('series1')
      }

      if(!'time' %in% names(data_train)){
        stop('data does not contain a "time" column')
      }
    }
  }

  if(!as.character(y) %in% names(data_train)){
    stop(paste0('variable "', y, '" not found in data'),
         call. = FALSE)
  } else {
    data_train$y <- data_train[[y]]
  }

  # Drop unused levels in data_train
  data_train$series <- droplevels(data_train$series)

# Check variables in newdata / data_test
  if(!missing(newdata)){
    data_test <- newdata
  }

  if(!missing(data_test)){
    # Add series factor variable if missing
    if(!inherits(data_test, 'list')){
      if(!'series' %in% colnames(data_test)){
        data_test$series <- factor('series1')
      }

      # Must be able to index by time; it is too dangerous to 'guess' as this could make a huge
      # impact on resulting estimates / inferences
      if(!'time' %in% colnames(data_test)){
        stop('newdata does not contain a "time" column')
      }
    }

    if(inherits(data_test, 'list')){
      if(!'series' %in% names(data_test)){
        data_test$series <- factor('series1')
      }

      if(!'time' %in% names(data_test)){
        stop('newdata does not contain a "time" column')
      }
    }

    if(!y %in% names(data_test)){
      stop(paste0('variable "', y, '" not found in newdata'),
           call. = FALSE)
    } else {
      data_test$y <- data_test[[y]]
    }

    # Drop unused levels in data_train
    data_test$series <- droplevels(data_test$series)
  }

  if(series == 'all'){
    n_plots <- length(levels(data_train$series))
    pages <- 1

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

      .pardefault <- par(no.readonly=T)
      on.exit(par(.pardefault))
      oldpar <- par(mfrow = c(r,c))

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
        ylab <- paste0('log(', y,' + 1)')
      } else {
        ylab <- paste0(y)
      }

      plot(1, type = "n", bty = 'L',
           xlab = 'Time',
           ylab = ylab,
           xaxt = 'n',
           ylim = range(unlist(all_ys), na.rm = TRUE),
           xlim = c(0, length(c(truth))))
      axis(side = 1,
           at = floor(seq(0, max(data_train$time) -
                            (min(data_train$time)-1),
                          length.out = 6)),
           labels = floor(seq(min(data_train$time),
                              max(data_train$time),
                              length.out = 6)))
      title(s_name, line = 0)

      if(n_plots > 1){
        if(lines){
          for(x in 1:n_plots){
            lines(all_ys[[x]], lwd = 1.85, col = 'grey85')
          }
        } else {
          for(x in 1:n_plots){
            points(all_ys[[x]], col = 'grey85', pch = 16)
          }
        }

      }

      if(lines){
        lines(x = 1:length(truth), y = truth, lwd = 3, col = "white")
        lines(x = 1:length(truth), y = truth, lwd = 2.5, col = "#8F2727")
      } else {
        points(x = 1:length(truth), y = truth, cex = 1.2, col = "white", pch = 16)
        points(x = 1:length(truth), y = truth, cex = 0.9, col = "#8F2727", pch = 16)
      }

      box(bty = 'L', lwd = 2)

    }
  layout(1)

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

    if(log_scale){
      truth <- log(truth + 1)
      ylab <- paste0('log(', y,' + 1)')
    } else {
      ylab <- paste0(y)
    }

    .pardefault <- par(no.readonly=T)
    on.exit(par(.pardefault))
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
           xlab = '',
           ylab = '',
           xaxt = 'n',
           ylim = range(c(truth, test), na.rm = TRUE),
           xlim = c(0, length(c(truth, test))))
        axis(side = 1,
             at = floor(seq(0, max(data_test$time) -
                              (min(data_train$time)-1),
                            length.out = 6)),
             labels = floor(seq(min(data_train$time),
                                max(data_test$time),
                                length.out = 6)))

      title('Time series', line = 0)
      title(ylab = ylab, line = 1.85)
      title(xlab = "Time", line = 1.85)

      if(lines){
        lines(x = 1:length(truth), y = truth, lwd = 2, col = "#8F2727")
        lines(x = (length(truth)+1):length(c(truth, test)),
              y = test, lwd = 2, col = "black")
      } else {
        points(x = 1:length(truth), y = truth, pch = 16, col = "#8F2727")
        points(x = (length(truth)+1):length(c(truth, test)),
               y = test, pch = 16, col = "black")
      }

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
           ylab = '',
           xlab = '',
           main = '')
      title('Histogram', line = 0)
      title(ylab = 'Density', line = 1.85)
      title(xlab = paste0(y), line = 1.85)

      acf(c(truth, test),
          na.action = na.pass, bty = 'L',
          lwd = 2.5, ci.col = 'black', col = "#8F2727",
          main = '', ylab = '', xlab = '')
      acf1 <- acf(c(truth, test), plot = F,
                  na.action = na.pass)
      clim <- qnorm((1 + .95)/2)/sqrt(acf1$n.used)
      abline(h = clim,  col = '#FFFFFF', lwd = 2.85)
      abline(h = clim,  col = 'black', lwd = 2.5, lty = 'dashed')
      abline(h = -clim,  col = '#FFFFFF', lwd = 2.85)
      abline(h = -clim,  col = 'black', lwd = 2.5, lty = 'dashed')
      box(bty = 'L', lwd = 2)
      title('ACF', line = 0)
      title(ylab = 'Autocorrelation', line = 1.85)
      title(xlab = 'Lag', line = 1.85)

      ecdf_plotdat = function(vals, x){
        func <- ecdf(vals)
        func(x)
      }

      plot_x <- seq(from = min(c(truth, test), na.rm = T),
                    to = max(c(truth, test), na.rm = T),
                    length.out = 100)

      plot(1, type = "n", bty = 'L',
           xlab = '',
           ylab = '',
           xlim = c(min(plot_x), max(plot_x)),
           ylim = c(0, 1))
      title('CDF', line = 0)
      title(ylab = 'Empirical CDF', line = 1.85)
      title(xlab = paste0(y), line = 1.85)
      lines(x = plot_x,
            y = ecdf_plotdat(c(truth, test),
                             plot_x),
            col = "#8F2727",
            lwd = 2.5)
      box(bty = 'L', lwd = 2)

    } else {
      plot(1, type = "n", bty = 'L',
           xlab = '',
           ylab = '',
           xaxt = 'n',
           ylim = range(c(truth), na.rm = TRUE),
           xlim = c(0, length(c(truth))))
      if(max(data_train$time < 6)){
        axis(side = 1,
             at = 0:(max(data_train$time) - 1),
             labels = 1:max(data_train$time))
      } else {
        axis(side = 1,
             at = floor(seq(0, max(data_train$time) -
                              (min(data_train$time)-1),
                            length.out = 6)),
             labels = floor(seq(min(data_train$time),
                                max(data_train$time),
                                length.out = 6)))
      }

      title('Time series', line = 0)
      title(ylab = ylab, line = 1.85)
      title(xlab = "Time", line = 1.85)

      if(lines){
        lines(x = 1:length(truth), y = truth, lwd = 2, col = "#8F2727")
      } else {
        points(x = 1:length(truth), y = truth, pch = 16, col = "#8F2727")
      }

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
           ylab = '',
           xlab = '',
           main = '')
      title('Histogram', line = 0)
      title(ylab = 'Density', line = 1.85)
      title(xlab = paste0(y), line = 1.85)


      acf(c(truth),
          na.action = na.pass, bty = 'L',
          lwd = 2.5, ci.col = 'black', col = "#8F2727",
          main = '', ylab = '', xlab = '')
      acf1 <- acf(c(truth), plot = F,
                  na.action = na.pass)
      clim <- qnorm((1 + .95)/2)/sqrt(acf1$n.used)
      abline(h = clim,  col = '#FFFFFF', lwd = 2.85)
      abline(h = clim,  col = 'black', lwd = 2.5, lty = 'dashed')
      abline(h = -clim,  col = '#FFFFFF', lwd = 2.85)
      abline(h = -clim,  col = 'black', lwd = 2.5, lty = 'dashed')
      box(bty = 'L', lwd = 2)
      title('ACF', line = 0)
      title(ylab = 'Autocorrelation', line = 1.85)
      title(xlab = 'Lag', line = 1.85)


      ecdf_plotdat = function(vals, x){
        func <- ecdf(vals)
        func(x)
      }

      plot_x <- seq(from = min(truth, na.rm = T),
                    to = max(truth, na.rm = T),
                    length.out = 100)
      plot(1, type = "n", bty = 'L',
           xlab = '',
           ylab = '',
           xlim = c(min(plot_x), max(plot_x)),
           ylim = c(0, 1))
      title('CDF', line = 0)
      title(ylab = 'Empirical CDF', line = 1.85)
      title(xlab = paste0(y), line = 1.85)
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
