% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mvgam_irf-class.R
\name{plot.mvgam_irf}
\alias{plot.mvgam_irf}
\title{Plot impulse responses from an \code{mvgam_irf} object}
\usage{
\method{plot}{mvgam_irf}(x, series = 1, ...)
}
\arguments{
\item{x}{\code{list} object of class \code{mvgam_irf}. See \code{\link[=irf]{irf()}}}

\item{series}{\code{integer} specifying which process series should be given the shock}

\item{...}{ignored}
}
\value{
A ggplot object showing the expected response of each latent time series to
a shock of the focal \code{series}
}
\description{
This function takes an \code{mvgam_irf} object and produces plots of Impulse Response Functions
}
\author{
Nicholas J Clark
}
