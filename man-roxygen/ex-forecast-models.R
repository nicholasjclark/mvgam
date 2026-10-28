#' @examples
#' \dontrun{
#' set.seed(<%= seed %>)
#' simdat <- sim_mvgam(family = poisson(), n_series = 1L,
#'                     n_timepoints = 120L, trend_model = AR())
#' # Two models of one count series: a smooth of x with and without
#' # an AR(1) trend
#' mod_gam <- mvgam(y ~ s(x), data = simdat$data_train,
#'                  family = poisson(), chains = 2, silent = 2)
#' mod_ar <- mvgam(y ~ s(x), trend_formula = ~ AR(p = 1),
#'                 data = simdat$data_train, family = poisson(),
#'                 chains = 2, silent = 2)
#' }
