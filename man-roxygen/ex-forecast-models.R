#' @examples
#' \dontrun{
#' set.seed(<%= seed %>)
#' simdat <- sim_mvgam(family = poisson(), n_series = 1L,
#'                     n_timepoints = 120L, trend_model = AR())
#' # Two models of one count series: a smooth of x with and without
#' # an AR(1) trend. The trend model samples with smaller steps
#' # (adapt_delta = 0.95). That avoids divergent transitions when
#' # lfo_cv() refits it to shorter stretches of the series
#' mod_gam <- mvgam(y ~ s(x, k = 5), data = simdat$data_train,
#'                  family = poisson(), chains = 2, silent = 2)
#' mod_ar <- mvgam(y ~ s(x, k = 5), trend_formula = ~ AR(p = 1),
#'                 data = simdat$data_train, family = poisson(),
#'                 chains = 2, silent = 2,
#'                 control = list(adapt_delta = 0.95))
#' }
