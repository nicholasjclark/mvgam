% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mvgam_trend_types.R
\name{GP}
\alias{GP}
\title{Specify dynamic Gaussian process trends in \pkg{mvgam} models}
\usage{
GP(...)
}
\arguments{
\item{...}{unused}
}
\value{
An object of class \code{mvgam_trend}, which contains a list of
arguments to be interpreted by the parsing functions in \pkg{mvgam}
}
\description{
Set up low-rank approximate Gaussian Process trend models using Hilbert
basis expansions in \pkg{mvgam}. This function does not evaluate its arguments –
it exists purely to help set up a model with particular GP
trend models.
}
\details{
A GP trend is estimated for each series using Hilbert space
approximate Gaussian Processes.
In \code{mvgam}, latent squared exponential GP trends are approximated using by
default \code{20} basis functions and using a multiplicative factor of \code{c = 5/4},
which saves computational costs compared to fitting full GPs while adequately estimating
GP \code{alpha} and \code{rho} parameters.
}
\references{
Riutort-Mayol G, Burkner PC, Andersen MR, Solin A and Vehtari A (2023).
Practical Hilbert space approximate Bayesian Gaussian processes for probabilistic
programming. Statistics and Computing 33, 1. https://doi.org/10.1007/s11222-022-10167-2
}
\seealso{
\code{\link[brms]{gp}}
}
\author{
Nicholas J Clark
}
