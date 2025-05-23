% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_residuals.R
\name{add_residuals.mvgam}
\alias{add_residuals.mvgam}
\alias{add_residuals}
\title{Calculate randomized quantile residuals for \pkg{mvgam} objects}
\usage{
add_residuals(object, ...)

\method{add_residuals}{mvgam}(object, ...)
}
\arguments{
\item{object}{\code{list} object of class \code{mvgam}. See \code{\link[=mvgam]{mvgam()}}}

\item{...}{unused}
}
\value{
A list object of class \code{mvgam} with residuals included in the \code{'resids'} slot
}
\description{
Calculate randomized quantile residuals for \pkg{mvgam} objects
}
\details{
For each series, randomized quantile (i.e. Dunn-Smyth) residuals are calculated for inspecting model diagnostics
If the fitted model is appropriate then Dunn-Smyth residuals will be standard normal in distribution and no
autocorrelation will be evident. When a particular observation is missing, the residual is calculated by comparing independent
draws from the model's posterior distribution
}
