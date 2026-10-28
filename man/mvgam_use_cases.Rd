\name{mvgam_use_cases}
\alias{mvgam_use_cases}
\title{Example use cases for \pkg{mvgam}}
\description{
New to \pkg{mvgam}? Start with \code{\link{mvgam}}, whose Workflow section lists the steps of an analysis and whose Terms section defines the vocabulary these pages use. \code{\link{mvgam_data}} checks a data frame before fitting. The \href{https://nicholasjclark.github.io/mvgam/dev/articles/mvgam_overview.html}{overview article} walks through a full analysis.

\pkg{mvgam} fits dynamic generalized additive models (GAMs) to one or many time series. Smooth functions of covariates sit beside a latent time-series process that carries the autocorrelation the covariates leave behind. Stan samples the posterior. Every prediction and forecast then carries the uncertainty of every parameter.

The links below lead to questions, blog posts, articles and videos where these models answered a practical problem. They cover counts and other non-Gaussian series, several series modelled together, and predictors such as seasonality that changes over time, monotonic effects and Gaussian processes.
}
\details{
\strong{Non-Gaussian time series modelling and forecasting}

Counts, proportions and binary outcomes rarely suit a Gaussian model. A latent trend on the link scale handles their autocorrelation.

\itemize{
  \item \href{https://stats.stackexchange.com/questions/657495}{Uncertain serial autocorrelation in GAM count model residuals}
  \item \href{https://discourse.mc-stan.org/t/fitting-an-autoregressive-model-and-poisson-process-interdependently/37268}{Fitting an autoregressive model and Poisson process interdependently}
  \item \href{https://stats.stackexchange.com/questions/652174}{Cyclical residual patterns and variable selection in GAMs}
  \item \href{https://stats.stackexchange.com/questions/437125}{Causality between two binary time series}
  \item \href{https://stats.stackexchange.com/questions/285100}{Logistic regression on time series data}
  \item \href{https://discourse.mc-stan.org/t/autocorrelation-for-unevenly-spaced-time-series/10001}{Autocorrelation for unevenly spaced time series}
  \item \href{https://stats.stackexchange.com/questions/664160}{Visualising autocorrelation in irregularly spaced count data}
  \item \href{https://ecogambler.netlify.app/blog/vector-autoregressions/}{Blog post: State-Space Vector Autoregressions in \code{mvgam}}
  \item \href{https://nicholasjclark.github.io/mvgam/dev/articles/var.html}{Article: State-space vector autoregressions with \code{mvgam}}
  \item \href{https://www.youtube.com/watch?v=0zZopLlomsQ&t=4s}{Video tutorial: Ecological forecasting with Dynamic Generalized Additive Models}
}

\strong{Multivariate time series modelling and forecasting}

Series measured together, such as several species at one site, can share latent trends or have trends that are correlated.

\itemize{
  \item \href{https://stats.stackexchange.com/questions/172645}{Ecological modelling: multivariate abundance time-series data}
  \item \href{https://discourse.mc-stan.org/t/account-for-relationships-between-species-in-a-multivariate-brms-model/32566}{Relationships between species in multivariate models}
  \item \href{https://discourse.mc-stan.org/t/confirmatory-factor-analysis-using-brms/23139}{Confirmatory factor analysis using \code{brms}}
  \item \href{https://discourse.mc-stan.org/t/chains-stuck-in-a-local-optimum-correlated-poisson-distributions/37414}{Chains stuck in a local optimum: correlated Poisson distributions}
  \item \href{https://ecogambler.netlify.app/blog/distributed-lags-mgcv/}{Blog post: Hierarchical distributed lag models in \code{mgcv} and \code{mvgam}}
  \item \href{https://nicholasjclark.github.io/mvgam/dev/articles/mvbf.html}{Article: One shared process, many sensors: multi-response \code{mvgam}}
  \item \href{https://www.youtube.com/watch?v=2POK_FVwCHk}{Video tutorial: Time series in R and Stan using the \code{mvgam} package: hierarchical GAMs}
}

\strong{Seasonality and other complex predictors}

Cyclic smooths model seasonal patterns. Gaussian processes, monotonic smooths and hierarchical smooths model other nonlinear effects.

\itemize{
  \item \href{https://stats.stackexchange.com/questions/478384}{Gaussian process smoothers (\code{bs = "gp"}) in GAMs}
  \item \href{https://stats.stackexchange.com/questions/612312}{Fitting a GAM with double seasonality to a daily time series}
  \item \href{https://stats.stackexchange.com/questions/648143}{Simulating time series with different seasonal effects}
  \item \href{https://discourse.mc-stan.org/t/adding-time-as-monotne-predictor/37109}{Adding time as a monotone predictor}
  \item \href{https://ecogambler.netlify.app/blog/time-varying-seasonality/}{Blog post: Incorporating time-varying seasonality in forecast models}
  \item \href{https://www.youtube.com/watch?v=fzPJUW8x6DU}{Video tutorial: Time series in R and Stan using the \code{mvgam} package: an introduction}
}
}
\seealso{
\code{\link{mvgam}}, \code{\link{mvgam_data}}, \code{\link{trend_constructors}}
}
\author{
Nicholas J Clark
}

