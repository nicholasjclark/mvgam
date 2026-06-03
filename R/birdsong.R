#' Simulated weekly bird song counts with hierarchically correlated AR(1) trends
#'
#' A multivariate count time series for four bird species observed
#' weekly across 80 weeks. Each species has its own AR(1) latent
#' trend; the four series are coupled through a contemporaneous
#' innovation correlation matrix. Two species (warbler, thrush)
#' rise together; two (wren, finch) move opposite to them. A
#' shared cyclic seasonal smooth is added on week-within-year.
#'
#' The dataset is designed so that an independent-AR fit
#' (`trend_formula = ~ AR(p = 1)`) treats the four trajectories as
#' unrelated, while `trend_formula = ~ AR(p = 1, cor = TRUE)`
#' recovers the cross-species correlation that drives the coupling.
#'
#' @format A `data.frame` with 320 rows and the following fields:
#' \describe{
#'   \item{series}{factor identifying the species (`warbler`,
#'     `thrush`, `wren`, `finch`); duplicated as `species` for
#'     readability.}
#'   \item{time}{integer week index from 1 to 80.}
#'   \item{y}{integer weekly count (Poisson sampled); duplicated as
#'     `count`.}
#'   \item{species}{factor, alias of `series`.}
#'   \item{week}{integer week index, alias of `time`.}
#'   \item{week_in_year}{integer week within the annual cycle
#'     (1..52), used for the shared seasonal smooth.}
#'   \item{count}{integer weekly count, alias of `y`.}
#' }
#' @details
#' True generative components:
#' * Species random intercepts (centred) on the log scale.
#' * Shared cyclic seasonal smooth (mgcv `bs = "cc"`) on
#'   week-within-year.
#' * Hierarchically correlated AR(1) trends with `phi = 0.7`,
#'   innovation standard deviation 0.35, and the 4 x 4 correlation
#'   matrix grouping `(warbler, thrush)` positively against
#'   `(wren, finch)`.
#'
#' Recommended fit:
#' ```r
#' mvgam(
#'   formula = y ~ s(week_in_year, bs = "cc", k = 8) +
#'                 s(species, bs = "re"),
#'   trend_formula = ~ AR(p = 1, cor = TRUE),
#'   data = birdsong,
#'   family = poisson()
#' )
#' ```
#'
#' Built by `data-raw/build_birdsong.R`.
"birdsong"
