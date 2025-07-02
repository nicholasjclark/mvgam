#'Plot observed time series used for \pkg{mvgam} modelling
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
plot_mvgam_series <- function(
  object,
  data,
  newdata,
  y = 'y',
  lines = TRUE,
  series = 1,
  n_bins = NULL,
  log_scale = FALSE
) {
  # Validate series
  if (is.character(series)) {
    if (series != 'all') {
      stop(
        'argument "series" must be either a positive integer or "all"',
        call. = FALSE
      )
    }
  } else {
    if (sign(series) != 1) {
      stop(
        'argument "series" must be either a positive integer or "all"',
        call. = FALSE
      )
    } else {
      if (series %% 1 != 0) {
        stop(
          'argument "series" must be either a positive integer or "all"',
          call. = FALSE
        )
      }
    }
  }

  # Extract training data
  if (!missing(object)) {
    if (!(inherits(object, "mvgam"))) {
      stop('argument "object" must be of class "mvgam"')
    }

    if (!missing("data")) {
      warning('both "object" and "data" were supplied; only using "object"')
    }

    data_train <- object$obs_data
    resp_terms <- as.character(terms(formula(object$call))[[2]])
    if (length(resp_terms) == 1) {
      y <- as.character(terms(object$call)[[2]])
    } else {
      if (any(grepl('cbind', resp_terms))) {
        resp_terms <- resp_terms[-grepl('cbind', resp_terms)]
        y <- resp_terms[1]
      }
    }
  } else {
    data_train <- data
  }

  # Validate data
  data_train <- validate_plot_data(data_train, y)
  if (!missing(newdata)) {
    data_test <- validate_plot_data(newdata, y)
  }

  # Determine what to plot
  if (is.character(series) && series == 'all') {
    # Only return a plot of time series
    dat <- dplyr::as_tibble(data_train) %>%
      dplyr::distinct(time, y, series)
    plot_ts <- plot_time_series(dat, lines, log_scale, y, series)
    return(plot_ts)
  } else {
    # Return multiple plots for one time series
    s_name <- levels(data_train$series)[series]
    dat <- dplyr::as_tibble(data_train) %>%
      dplyr::filter(series == s_name) %>%
      dplyr::distinct(time, y) %>%
      dplyr::mutate(data = "train")
    if (!missing(newdata)) {
      dat <- dplyr::bind_rows(
        dat,
        dplyr::as_tibble(data_test) %>%
          dplyr::filter(series == s_name) %>%
          dplyr::distinct(time, y) %>%
          dplyr::mutate(data = "validate")
      )
    }
    plot_ts <- plot_time_series(dat, lines, log_scale, y, series)
    plot_hist <- plot_histogram(dat, y, n_bins)
    plot_acf_obj <- plot_acf(dat)
    plot_ecdf_obj <- plot_ecdf(dat, y)
    return(
      patchwork::wrap_plots(
        plot_ts,
        plot_hist,
        plot_acf_obj,
        plot_ecdf_obj,
        ncol = 2,
        nrow = 2,
        byrow = TRUE
      )
    )
  }
}

#' Helper function to validate and format input plotting data
#' @noRd
validate_plot_data <- function(data, y) {
  # Check if data is not a list
  if (!inherits(data, 'list')) {
    # If 'series' column is missing, create a default factor
    if (!'series' %in% colnames(data)) {
      data$series <- factor('series1')
    }
    # If 'time' column is missing, stop with error
    if (!'time' %in% colnames(data)) {
      stop('data does not contain a "time" column', call. = FALSE)
    }
  } else {
    # If data is a list, check for 'series' and 'time' in names
    if (!'series' %in% names(data)) {
      data$series <- factor('series1')
    }
    if (!'time' %in% names(data)) {
      stop('data does not contain a "time" column')
    }
  }

  # Check if the outcome variable 'y' exists in data
  if (!y %in% names(data)) {
    stop(paste0('variable "', y, '" not found in data'), call. = FALSE)
  } else {
    # Assign the outcome variable to a standard column 'y'
    data$y <- data[[y]]
  }

  # Drop unused factor levels in 'series'
  data$series <- droplevels(data$series)

  # Return the validated and formatted data
  return(data)
}

#' Function to generate time series plots
#' @noRd
plot_time_series <- function(
  dat,
  lines = TRUE,
  log_scale = FALSE,
  ylab = 'y',
  series = 'all'
) {
  if (log_scale) {
    dat$y <- log(dat$y + 1)
    ylab <- paste0('log(', ylab, ' + 1)')
  }
  if (series == 'all') {
    p <- ggplot2::ggplot(dat, ggplot2::aes(time, y)) +
      ggplot2::facet_wrap(~series) +
      ggplot2::labs(x = "Time", y = ylab) +
      ggplot2::theme_bw()
    if (lines) {
      p <- p + ggplot2::geom_line(colour = "#8F2727", linewidth = 0.75)
    } else {
      p <- p + ggplot2::geom_point(colour = "#8F2727")
    }
  } else {
    p <- ggplot2::ggplot(dat, ggplot2::aes(time, y, colour = data)) +
      ggplot2::labs(title = "Time series", x = "Time", y = ylab) +
      ggplot2::geom_vline(
        xintercept = dat %>%
          dplyr::filter(data == "validate") %>%
          dplyr::pull(time) %>%
          min(c(., Inf)),
        linetype = "dashed",
        colour = "black"
      ) +
      ggplot2::scale_colour_manual(values = c("#8F2727", "black")) +
      ggplot2::theme_bw()
    if (lines) {
      p <- p + ggplot2::geom_line(show.legend = F, linewidth = 0.75)
    } else {
      p <- p + ggplot2::geom_point(show.legend = F)
    }
  }
  return(p)
}

#' Function to create histogram of observed values
#' @noRd
plot_histogram <- function(dat, ylab = 'y', n_bins = NULL) {
  if (is.null(n_bins)) {
    n_bins <- max(c(length(hist(c(dat$y), plot = F)$breaks), 20))
  }
  ggplot2::ggplot(dat, ggplot2::aes(y)) +
    ggplot2::geom_histogram(bins = n_bins, fill = "#8F2727", col = 'white') +
    ggplot2::labs(title = "Histogram", x = ylab, y = "Count") +
    ggplot2::theme_bw()
}

#' Function to compute and plot autocorrelation
#' @noRd
plot_acf <- function(dat) {
  acf_y <- acf(dat$y, plot = F, na.action = na.pass)
  data.frame(acf = acf_y$acf[,, 1], lag = acf_y$lag[, 1, 1]) %>%
    ggplot2::ggplot(ggplot2::aes(x = lag, y = 0, yend = acf)) +
    ggplot2::geom_hline(
      yintercept = c(-1, 1) * qnorm((1 + 0.95) / 2) / sqrt(acf_y$n.used),
      linetype = "dashed"
    ) +
    ggplot2::geom_hline(
      yintercept = 0,
      colour = "#8F2727",
      linewidth = 0.25
    ) +
    ggplot2::geom_segment(colour = "#8F2727", linewidth = 1) +
    ggplot2::labs(title = "ACF", x = "Lag", y = "Autocorrelation") +
    ggplot2::theme_bw()
}

#' Function to generate empirical cumulative distribution
#' @noRd
plot_ecdf <- function(dat, ylab = 'y') {
  range_y <- range(dat$y, na.rm = T)
  data.frame(x = seq(range_y[1], range_y[2], length.out = 100)) %>%
    dplyr::mutate(y = ecdf(dat$y)(x)) %>%
    ggplot2::ggplot(ggplot2::aes(x, y)) +
    ggplot2::geom_line(colour = "#8F2727", linewidth = 0.75) +
    ggplot2::scale_y_continuous(limits = c(0, 1)) +
    ggplot2::labs(
      title = "CDF",
      x = ylab,
      y = "Empirical CDF"
    ) +
    ggplot2::theme_bw()
}
