#'Plot observed time series used for mvgam modelling
#'
#'This function takes either a fitted \code{mvgam} object or a \code{data.frame} object
#'and produces plots of observed time series, ACF, CDF and histograms for exploratory data analysis
#'
#'@importFrom stats lag
#'@param object Optional \code{list} object returned from \code{mvgam}. Either \code{object} or \code{data}
#'must be supplied.
#'@param data Optional \code{data.frame} or \code{list} of training data containing at least 'series' and 'time'.
#'Use this argument if training data have been gathered in the correct format for \code{mvgam} modelling
#'but no model has yet been fitted.
#'@param newdata Optional \code{data.frame} or \code{list} of test data containing at least 'series' and 'time'
#'for the forecast horizon, in addition to any other variables included in the linear predictor of \code{formula}. If
#'included, the observed values in the test data are compared to the model's forecast distribution for exploring
#'biases in model predictions.
#'@param y Character. What is the name of the outcome variable in the supplied data? Defaults to
#'\code{'y'}
#'@param lines Logical. If \code{TRUE}, line plots are used for visualizing time series. If
#'\code{FALSE}, points are used.
#'@param series Either a \code{integer} specifying which series in the set is to be plotted or
#'the string 'all', which plots all series available in the supplied data
#'@param n_bins \code{integer} specifying the number of bins to use for binning observed values when plotting
#'a the histogram. Default is to use the number of bins returned by a call to `hist` in base `R`
#'@param log_scale \code{logical}. If \code{series == 'all'}, this flag is used to control whether
#'the time series plot is shown on the log scale (using `log(Y + 1)`). This can be useful when
#'visualizing many series that may have different observed ranges. Default is \code{FALSE}
#'@author Nicholas J Clark and Matthijs Hollanders
#'@return A set of ggplot objects. If \code{series} is an integer, the plots will
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
                             newdata,
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
  } else {
    data_train <- rlang::missing_arg()
  }

  # Choose models over data if both supplied
  if(!missing(object)){
    if(!missing("data")){
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
  } else {
    data_test <- rlang::missing_arg()
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

  # only plot time series
  if(series == 'all'){

    # create tibble
    dat <- dplyr::as_tibble(data_train) %>%
      dplyr::distinct(time, y, series)

    # log transform and y-axis labels
    if(log_scale){
      dat$y <- log(dat$y + 1)
      ylab <- paste0('log(', y,' + 1)')
    } else {
      ylab <- paste0(y)
    }

    # create faceted time serires
    plot_ts <- dat %>%
      ggplot2::ggplot(ggplot2::aes(time, y)) +
      ggplot2::facet_wrap(~ series) +
      ggplot2::labs(x = "Time",
                    y = ylab) +
      ggplot2::theme_bw()

    if (lines) {
      plot_ts <- plot_ts + ggplot2::geom_line(colour = "#8F2727",
                                              linewidth = 0.75)
    } else {
      plot_ts <- plot_ts + ggplot2::geom_point(colour = "#8F2727")
    }
    plot_ts

    # multiple plots for one time series
  } else {
    # series name
    s_name <- levels(data_train$series)[series]

    # training data
    dat <- dplyr::as_tibble(data_train) %>%
      dplyr::filter(series == s_name) %>%
      dplyr::distinct(time, y) %>%
      dplyr::mutate(data = "train")

    # optionally bind test data
    if (!missing(data_test)) {
      dat <- dplyr::bind_rows(dat,
                              dplyr::as_tibble(data_test) %>%
                                dplyr::filter(series == s_name) %>%
                                dplyr::distinct(time, y) %>%
                                dplyr::mutate(data = "test"))
    }

    # log-transform y
    if (log_scale) {
      truth$y <- log(truth$y + 1)
      ylab <- paste0('log(', y,' + 1)')
    } else {
      ylab <- paste0(y)
    }

    # time series
    if (!missing(data_test)) {
      plot_ts <- dat %>%
        ggplot2::ggplot(ggplot2::aes(time, y, colour = data)) +
        ggplot2::geom_vline(xintercept = dat %>%
                              dplyr::filter(data == "test") %>%
                              dplyr::pull(time) %>%
                              min(),
                            linetype = "dashed",
                            colour = "black") +
        ggplot2::scale_colour_manual(values = c("black", "#8F2727")) +
        ggplot2::labs(title = "Time series",
                      x = "Time",
                      y = ylab) +
        ggplot2::theme_bw()
      if (lines) {
        plot_ts <- plot_ts + ggplot2::geom_line(show.legend = F,
                                                linewidth = 0.75)
      } else {
        plot_ts <- plot_ts + ggplot2::geom_point(show.legend = F)
      }
    } else {
      plot_ts <- dat %>%
        ggplot2::ggplot(ggplot2::aes(time, y)) +
        ggplot2::labs(title = "Time series",
                      x = "Time",
                      y = ylab) +
        ggplot2::theme_bw()
      if (lines) {
        plot_ts <- plot_ts + ggplot2::geom_line(colour = "#8F2727",
                                                linewidth = 0.75)
      } else {
        plot_ts <- plot_ts + ggplot2::geom_point(colour = "#8F2727")
      }
    }

    # histogram
    if(missing(n_bins)){
      n_bins <- max(c(length(hist(c(dat$y), plot = F)$breaks),
                      20))
    }
    plot_hist <- dat %>%
      ggplot2::ggplot(ggplot2::aes(y)) +
      ggplot2::geom_histogram(bins = n_bins, fill = "#8F2727",
                              col = 'white') +
      ggplot2::labs(title = "Histogram",
                    x = "y",
                    y = "Count") +
      ggplot2::theme_bw()

    # ACF
    acf_y <- acf(dat$y, plot = F, na.action = na.pass)
    plot_acf <- data.frame(acf = acf_y$acf[,,1],
                           lag = acf_y$lag[,1,1]) %>%
      ggplot2::ggplot(ggplot2::aes(x = lag, y = 0, yend = acf)) +
      ggplot2::geom_hline(yintercept = c(-1, 1) * qnorm((1 + 0.95) / 2) / sqrt(acf_y$n.used),
                          linetype = "dashed") +
      ggplot2::geom_hline(yintercept = 0,
                          colour = "#8F2727",
                          linewidth = 0.25) +
      ggplot2::geom_segment(colour = "#8F2727",
                            linewidth = 1) +
      ggplot2::labs(title = "ACF",
                    x = "Lag",
                    y = "Autocorrelation") +
      ggplot2::theme_bw()

    # ECDF plot
    range_y <- range(dat$y, na.rm = T)
    plot_ecdf <- data.frame(x = seq(range_y[1], range_y[2], length.out = 100)) %>%
      dplyr::mutate(y = ecdf(dat$y)(x)) %>%
      ggplot2::ggplot(ggplot2::aes(x, y)) +
      ggplot2::geom_line(colour = "#8F2727",
                         linewidth = 0.75) +
      ggplot2::scale_y_continuous(limits = c(0, 1)) +
      ggplot2::labs(title = "CDF",
                    x = "y",
                    y = "Empirical CDF") +
      ggplot2::theme_bw()

    # plot
    patchwork::wrap_plots(plot_ts,
                          plot_hist,
                          plot_acf,
                          plot_ecdf,
                          ncol = 2,
                          nrow = 2,
                          byrow = T)
  }
}
