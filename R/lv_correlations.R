#' Calculate trend correlations based on latent factor loadings for
#' \pkg{mvgam} models
#'
#' This function uses factor loadings from a fitted dynamic factor
#' \code{mvgam} model to calculate temporal correlations among series' trends.
#'
#' @importFrom stats cov2cor cov
#'
#' @param object \code{list} object of class \code{mvgam} that used latent
#' factors, either with `use_lv = TRUE` or by supplying a `trend_map`. See
#' [mvgam()] for details and for an example.
#'
#' @return A \code{list} object containing the mean posterior correlations and
#' the full array of posterior correlations.
#'
#' @details Although this function will still work, it is now recommended to use
#' [residual_cor()] to obtain residual correlation information in a more
#' user-friendly format that allows for a deeper investigation of relationships
#' among the time series.
#'
#' @seealso [residual_cor()], [plot.mvgam_residcor()]
#'
#' @examples
#' \donttest{
#' #--------------------------------------------------
#' # Fit a model that uses two AR(1) dynamic factors to model
#' # the temporal dynamics of the four rodent species in the portal_data
#' #--------------------------------------------------
#' mod <- mvgam(
#'   captures ~ series,
#'   trend_model = AR(),
#'   use_lv = TRUE,
#'   n_lv = 2,
#'   data = portal_data,
#'   chains = 2,
#'   silent = 2
#' )
#'
#' # Plot the two dynamic factors
#' plot(mod, type = 'factors')
#'
#' # Calculate correlations among the series
#' lvcors <- lv_correlations(mod)
#' names(lvcors)
#' lapply(lvcors, class)
#'
#' # Recommended: use residual_cor() instead
#' lvcors <- residual_cor(mod)
#' names(lvcors)
#' lvcors$cor
#'
#' # Plot credible correlations as a matrix
#' plot(lvcors, cluster = TRUE)
#'
#' \dontshow{
#' # For R CMD check: make sure any open connections are closed afterward
#' closeAllConnections()
#' }
#' }
#'
#' @export
lv_correlations = function(object) {
  # Check arguments
  if (!(inherits(object, "mvgam"))) {
    stop('argument "object" must be of class "mvgam"')
  }

  # Series start and end indices
  ends <- seq(
    0,
    dim(mcmc_chains(object$model_output, 'ypred'))[2],
    length.out = NCOL(object$ytimes) + 1
  )
  starts <- ends + 1
  starts <- c(1, starts[-c(1, (NCOL(object$ytimes) + 1))])
  ends <- ends[-1]

  # Total number of MCMC samples
  n_preds <- dim(mcmc_chains(object$model_output, 'trend')[,
    starts[1]:ends[1]
  ])[1]
  data_train <- object$obs_data

  # Total number of observations per series
  if (inherits(data_train, 'list')) {
    n_obs <- length(data_train$y) / NCOL(object$ytimes)
  } else {
    n_obs <- NROW(data_train) / NCOL(object$ytimes)
  }

  # Extract series trends
  series_trends <- lapply(seq_len(length(ends)), function(y) {
    if (object$fit_engine == 'stan') {
      # For stan objects, trend is stored as a vector in column-major order
      mcmc_chains(object$model_output, 'trend')[, seq(
        y,
        dim(mcmc_chains(object$model_output, 'trend'))[2],
        by = NCOL(object$ytimes)
      )]
    } else {
      mcmc_chains(object$model_output, 'trend')[, starts[y]:ends[y]][, 1:n_obs]
    }
  })

  # Get list of trend correlation estimates
  all_trend_cors <- lapply(seq_len(n_preds), function(x) {
    cov2cor(cov(do.call(
      cbind,
      lapply(series_trends, function(y) {
        y[x, ]
      })
    )))
  })

  # Calculate posterior mean correlations
  mean_correlations <- Reduce(`+`, all_trend_cors) / length(all_trend_cors)
  rownames(mean_correlations) <- colnames(mean_correlations) <- levels(
    data_train$series
  )

  list(
    mean_correlations = mean_correlations,
    posterior_correlations = all_trend_cors
  )
}
