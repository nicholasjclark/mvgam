% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_mvgam_series.R
\name{plot_mvgam_series}
\alias{plot_mvgam_series}
\title{Plot observed time series used for \pkg{mvgam} modelling}
\usage{
plot_mvgam_series(
  object,
  data,
  newdata,
  y = "y",
  lines = TRUE,
  series = 1,
  n_bins,
  log_scale = FALSE
)
}
\arguments{
\item{object}{Optional \code{list} object returned from \code{mvgam}. Either \code{object} or \code{data}
must be supplied.}

\item{data}{Optional \code{data.frame} or \code{list} of training data containing at least 'series' and 'time'.
Use this argument if training data have been gathered in the correct format for \code{mvgam} modelling
but no model has yet been fitted.}

\item{newdata}{Optional \code{data.frame} or \code{list} of test data containing at least 'series' and 'time'
for the forecast horizon, in addition to any other variables included in the linear predictor of \code{formula}. If
included, the observed values in the test data are compared to the model's forecast distribution for exploring
biases in model predictions.}

\item{y}{Character. What is the name of the outcome variable in the supplied data? Defaults to
\code{'y'}}

\item{lines}{Logical. If \code{TRUE}, line plots are used for visualizing time series. If
\code{FALSE}, points are used.}

\item{series}{Either a \code{integer} specifying which series in the set is to be plotted or
the string 'all', which plots all series available in the supplied data}

\item{n_bins}{\code{integer} specifying the number of bins to use for binning observed values when plotting
a the histogram. Default is to use the number of bins returned by a call to \code{hist} in base \code{R}}

\item{log_scale}{\code{logical}. If \code{series == 'all'}, this flag is used to control whether
the time series plot is shown on the log scale (using \code{log(Y + 1)}). This can be useful when
visualizing many series that may have different observed ranges. Default is \code{FALSE}}
}
\value{
A set of ggplot objects. If \code{series} is an integer, the plots will
show observed time series, autocorrelation and
cumulative distribution functions, and a histogram for the series. If \code{series == 'all'},
a set of observed time series plots is returned in which all series are shown on each plot but
only a single focal series is highlighted, with all remaining series shown as faint gray lines.
}
\description{
This function takes either a fitted \code{mvgam} object or a \code{data.frame} object
and produces plots of observed time series, ACF, CDF and histograms for exploratory data analysis
}
\examples{
# Simulate and plot series with observations bounded at 0 and 1 (Beta responses)
sim_data <- sim_mvgam(family = betar(),
                     trend_model = RW(), prop_trend = 0.6)
plot_mvgam_series(data = sim_data$data_train, series = 'all')
plot_mvgam_series(data = sim_data$data_train,
                 newdata = sim_data$data_test, series = 1)

# Now simulate series with overdispersed discrete observations
sim_data <- sim_mvgam(family = nb(), trend_model = RW(),
                     prop_trend = 0.6, phi = 10)
plot_mvgam_series(data = sim_data$data_train, series = 'all')
}
\author{
Nicholas J Clark and Matthijs Hollanders
}
