% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_mvgam_pterms.R
\name{plot_mvgam_pterms}
\alias{plot_mvgam_pterms}
\title{Plot parametric term partial effects for \pkg{mvgam} models}
\usage{
plot_mvgam_pterms(object, trend_effects = FALSE)
}
\arguments{
\item{object}{\code{list} object of class \code{mvgam}. See \code{\link[=mvgam]{mvgam()}}}

\item{trend_effects}{logical. If \code{TRUE} and a \code{trend_formula} was used in model
fitting, terms from the trend (i.e. process) model will be plotted}
}
\value{
A base \code{R} graphics plot
}
\description{
This function plots posterior empirical quantiles for partial effects of parametric terms
}
\details{
Posterior empirical quantiles of each parametric term's partial effect estimates
(on the link scale) are calculated and visualised as ribbon plots. These effects can
be interpreted as the partial effect that a parametric term contributes when all other
terms in the model have been set to \code{0}
}
