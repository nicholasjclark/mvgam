% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lv_correlations.R
\name{lv_correlations}
\alias{lv_correlations}
\title{Calculate trend correlations based on mvgam latent factor loadings}
\usage{
lv_correlations(object)
}
\arguments{
\item{object}{\code{list} object returned from \code{mvgam}}
}
\value{
A \code{list} object containing the mean posterior correlations
and the full array of posterior correlations
}
\description{
This function uses samples of latent trends for each series from a fitted
mvgam model to calculates correlations among series' trends
}
\examples{
\dontrun{
simdat <- sim_mvgam()
mod <- mvgam(y ~ s(season, bs = 'cc',
                  k = 6),
            trend_model = AR(),
            use_lv = TRUE,
            n_lv = 2,
            data = simdat$data_train,
            burnin = 300,
            samples = 300,
            chains = 2)
lvcors <- lv_correlations(mod)
names(lvcors)
lapply(lvcors, class)
}
}
