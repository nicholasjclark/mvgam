% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/piecewise_trends.R
\name{PW}
\alias{PW}
\title{Specify piecewise linear or logistic trends}
\usage{
PW(
  n_changepoints = 10,
  changepoint_range = 0.8,
  changepoint_scale = 0.05,
  growth = "linear"
)
}
\arguments{
\item{n_changepoints}{A non-negative integer specifying the number of potential
changepoints. Potential changepoints are selected uniformly from the
first \code{changepoint_range} proportion of timepoints in \code{data}. Default is \code{10}}

\item{changepoint_range}{Proportion of history in \code{data} in which trend changepoints
will be estimated. Defaults to 0.8 for the first 80\%.}

\item{changepoint_scale}{Parameter modulating the flexibility of the
automatic changepoint selection by altering the scale parameter of a Laplace distribution.
The resulting prior will be \code{double_exponential(0, changepoint_scale)}.
Large values will allow many changepoints and a more flexible trend, while
small values will allow few changepoints. Default is \code{0.05}.}

\item{growth}{Character string specifying either 'linear' or 'logistic' growth of
the trend. If 'logistic', a variable labelled \code{cap} MUST be in \code{data} to specify the
maximum saturation point for the trend (see details and examples in \code{\link{mvgam}} for
more information).
Default is 'linear'.}
}
\value{
An object of class \code{mvgam_trend}, which contains a list of
arguments to be interpreted by the parsing functions in \code{mvgam}
}
\description{
Set up piecewise linear or logistic trend models
in \code{mvgam}. These functions do not evaluate their arguments –
they exist purely to help set up a model with particular piecewise
trend models.
}
\details{
\emph{Offsets and intercepts}:
For each of these trend models, an offset parameter is included in the trend
estimation process. This parameter will be incredibly difficult to identify
if you also include an intercept in the observation formula. For that reason,
it is highly recommended that you drop the intercept from the formula
(i.e. \code{y ~ x + 0} or \code{y ~ x - 1}, where \code{x} are your optional predictor terms).
\cr
\cr
\emph{Logistic growth and the cap variable}:
When forecasting growth, there is often some maximum achievable point that
a time series can reach. For example, total market size, total population size
or carrying capacity in population dynamics. It can be advantageous for the forecast
to saturate at or near this point so that predictions are more sensible.
This function allows you to make forecasts using a logistic growth trend model,
with a specified carrying capacity. Note that this capacity does not need to be static
over time, it can vary with each series x timepoint combination if necessary. But you
must supply a \code{cap} value for each observation in the data when using \code{growth = 'logistic'}.
For observation families that use a non-identity link function, the \code{cap} value will
be internally transformed to the link scale (i.e. your specified \code{cap} will be log
transformed if you are using a \code{poisson()} or \code{nb()} family). It is therefore important
that you specify the \code{cap} values on the scale of your outcome. Note also that
no missing values are allowed in \code{cap}.
}
\references{
Taylor, Sean J., and Benjamin Letham. "Forecasting at scale." The American Statistician 72.1 (2018): 37-45.
}
