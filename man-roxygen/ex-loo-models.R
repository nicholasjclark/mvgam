#' @examples
#' \dontrun{
#' # A Poisson GAM without a latent trend. PSIS approximates each
#' # held-out count of such a model well (see Details of ?loo.mvgam)
#' set.seed(13)
#' simdat <- sim_mvgam(family = poisson(), n_series = 1L,
#'                     n_timepoints = 120L, trend_model = "None")
#' mod <- mvgam(y ~ s(x), data = simdat$data_train,
#'              family = poisson(), chains = 2, silent = 2)
#' }
