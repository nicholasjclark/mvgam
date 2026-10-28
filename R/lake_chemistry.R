#' Simulated lake chemistry with correlated random-walk state
#'
#' A state-space dataset for five lakes observed monthly across
#' 60 months. The latent process is a correlated random walk
#' across lakes (two regional clusters share positive innovation
#' correlation, a fifth lake is independent). The observation
#' equation adds a binary treatment indicator that switches on at
#' month 31, with heterogeneous slopes across lakes: two lakes
#' respond positively and three respond negatively.
#'
#' A pooled fit with one treatment slope for all lakes reports an
#' effect near zero with a wide credible interval. The positive and
#' negative responses cancel. A state-space fit
#' (`trend_formula = ~ RW(cor = TRUE)`) with a treatment slope for each
#' lake recovers the response of every lake.
#'
#' @format A `data.frame` with 300 rows and the following fields:
#' \describe{
#'   \item{series}{factor identifying the lake (`alpine`,
#'     `boreal`, `coastal`, `delta`, `estuary`); duplicated as
#'     `lake`.}
#'   \item{time}{integer month index from 1 to 60.}
#'   \item{y}{numeric chemistry measurement (Gaussian); duplicated
#'     as `chemistry`.}
#'   \item{lake}{factor, alias of `series`.}
#'   \item{month}{integer month index, alias of `time`.}
#'   \item{treated}{integer (0/1) treatment indicator; switches on
#'     from month 31 onwards in all lakes.}
#'   \item{chemistry}{numeric chemistry measurement, alias of `y`.}
#' }
#' @details
#' True generative components:
#' * Per-lake intercepts (centred).
#' * Latent random walk with cross-lake innovation correlation
#'   `0.6` between `(alpine, boreal)` and `(coastal, delta)`,
#'   independent for `estuary`. Innovation standard deviation
#'   `0.25`.
#' * Heterogeneous treatment slopes:
#'   `(alpine = +0.8, boreal = +0.6, coastal = -0.5,
#'   delta = -0.7, estuary = -0.6)`.
#' * Observation noise `sigma = 0.4`.
#'
#' @seealso [RW()] for the trend, [residual_cor()] to extract the fitted
#'   correlations between lakes, [ranef.mvgam()] for the lake slopes,
#'   [mvgam_data()] to check the data.
#'
#' @examples
#' data(lake_chemistry)
#' mvgam_data(lake_chemistry, family = gaussian())
#'
#' \dontrun{
#' # A model with the structure the data were simulated from
#' mod <- mvgam(
#'   formula = y ~ treated + (treated || lake),
#'   trend_formula = ~ RW(cor = TRUE),
#'   data = lake_chemistry,
#'   family = gaussian(),
#'   chains = 2, silent = 2
#' )
#' summary(mod)
#' }
"lake_chemistry"
