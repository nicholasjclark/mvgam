\name{mvgam_use_cases}
\alias{mvgam_use_cases}
\title{Example use cases for \pkg{mvgam}}
\description{
\pkg{mvgam} is a package for fitting dynamic generalized additive models (GAMs) to univariate or multivariate data. It combines the flexibility of smooth functions with latent temporal processes to model autocorrelation, seasonality, and uncertainty. The package supports both univariate and multivariate time series, making it especially useful for ecological and environmental forecasting. Bayesian inference via Stan allows for full uncertainty quantification and forecasting in complex, non-Gaussian settings.

This help page provides external links to example applications and discussions relevant to the use of \pkg{mvgam} models. These examples span non-Gaussian time series modelling, multivariate abundance forecasting, and the use of complex predictors such as time-varying seasonality, monotonic nonlinear effects and Gaussian processes.
}
\details{
\strong{Non-Gaussian time series modelling and forecasting}

\pkg{mvgam} is designed for real-world time series data that include discrete, zero-inflated, or overdispersed observations. It supports latent dynamic components and smooth terms to model autocorrelation, trends, and uncertainty.

\itemize{
  \item \href{https://stats.stackexchange.com/questions/657495}{Uncertain serial autocorrelation in GAM count model residuals}
  \item \href{https://discourse.mc-stan.org/t/fitting-an-autoregressive-model-and-poisson-process-interdependently/37268}{Fitting an autoregressive model and Poisson process interdependently}
  \item \href{https://stats.stackexchange.com/questions/652174}{Cyclical residual patterns and variable selection in GAMs}
  \item \href{https://stats.stackexchange.com/questions/437125}{Causality between two binary time series}
  \item \href{https://stats.stackexchange.com/questions/285100}{Logistic regression on time series data}
  \item \href{https://discourse.mc-stan.org/t/autocorrelation-for-unevenly-spaced-time-series/10001}{Autocorrelation for unevenly spaced time series}
  \item \href{https://stats.stackexchange.com/questions/664160}{Visualising autocorrelation in irregularly spaced count data}
  \item \href{https://ecogambler.netlify.app/blog/vector-autoregressions/}{Blog post: State-Space Vector Autoregressions in \code{mvgam}}
  \item \href{https://nicholasjclark.github.io/mvgam/articles/trend_formulas.html}{Vignette: State-Space models in \code{mvgam}}
  \item \href{https://www.youtube.com/watch?v=0zZopLlomsQ&t=4s}{Video tutorial: Ecological forecasting with Dynamic Generalized Additive Models}
}

\strong{Multivariate time series modelling and forecasting}

\pkg{mvgam} supports multivariate models with shared or correlated latent trends, making it suitable for a broad range of applications that gather data on multiple time series simultaneously.

\itemize{
  \item \href{https://stats.stackexchange.com/questions/172645}{Ecological modelling: multivariate abundance time-series data}
  \item \href{https://discourse.mc-stan.org/t/account-for-relationships-between-species-in-a-multivariate-brms-model/32566}{Relationships between species in multivariate models}
  \item \href{https://discourse.mc-stan.org/t/confirmatory-factor-analysis-using-brms/23139}{Confirmatory factor analysis using \code{brms}}
  \item \href{https://discourse.mc-stan.org/t/chains-stuck-in-a-local-optimum-correlated-poisson-distributions/37414}{Chains stuck in a local optimum: correlated Poisson distributions}
  \item \href{https://ecogambler.netlify.app/blog/distributed-lags-mgcv/}{Blog post: Hierarchical distributed lag models in \code{mgcv} and \code{mvgam}}
  \item \href{https://nicholasjclark.github.io/mvgam/articles/shared_states.html}{Vignette: Multivariate series with shared latent states}
  \item \href{https://www.youtube.com/watch?v=2POK_FVwCHk}{Video tutorial: Time series in R and Stan using the \code{mvgam} package: hierarchical GAMs}
}

\strong{Seasonality and other complex predictors}

\pkg{mvgam} allows for flexible modelling of seasonal patterns and nonlinear effects using cyclic smooths, Gaussian processes, monotonic smooths and hierarchical structures.

\itemize{
  \item \href{https://stats.stackexchange.com/questions/478384}{Gaussian process smoothers (\code{bs = "gp"}) in GAMs}
  \item \href{https://stats.stackexchange.com/questions/612312}{Fitting a GAM with double seasonality to a daily time series}
  \item \href{https://stats.stackexchange.com/questions/648143}{Simulating time series with different seasonal effects}
  \item \href{https://discourse.mc-stan.org/t/adding-time-as-monotne-predictor/37109}{Adding time as a monotone predictor}
  \item \href{https://ecogambler.netlify.app/blog/time-varying-seasonality/}{Blog post: Incorporating time-varying seasonality in forecast models}
  \item \href{https://nicholasjclark.github.io/mvgam/articles/time_varying_effects.html}{Vignette: Time-varying effects in \code{mvgam}}
  \item \href{https://www.youtube.com/watch?v=fzPJUW8x6DU}{Video tutorial: Time series in R and Stan using the \code{mvgam} package: an introduction}
}
}
\author{
Nicholas J Clark
}
