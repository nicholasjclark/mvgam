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
#' A pooled fit that ignores both the latent state and the
#' lake-level slope variation reports a near-zero aggregate
#' treatment effect with wide credible intervals. The state-space
#' fit (`trend_formula = ~ RW(cor = TRUE)`) with random treatment
#' slopes recovers the heterogeneity, exposing the wide aggregate
#' CI as hidden lake-level variation rather than absence of effect.
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
#' Recommended fit (state-space, with treatment on the observation
#' equation and the correlated drift on the latent state):
#' ```r
#' mvgam(
#'   formula = y ~ treated + (treated || lake),
#'   trend_formula = ~ RW(cor = TRUE),
#'   data = lake_chemistry,
#'   family = gaussian()
#' )
#' ```
#'
#' Built by `data-raw/build_lake_chemistry.R`.
"lake_chemistry"
