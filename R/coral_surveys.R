#' Simulated coral bleaching index with irregular CAR(1) state
#'
#' A state-space dataset for three coral reefs surveyed at
#' irregular monthly intervals (gap distribution
#' `Uniform(1, 6)` months, mean approximately 3.5). The latent
#' state is a continuous-time AR(1) (CAR(1)) trend per reef with
#' `phi = 0.7` and `sigma = 0.5`. The observation equation adds
#' an observed sea-surface temperature (SST) driver via a smooth
#' nonlinear effect.
#'
#' Naive AR(1) treats every consecutive observation as a unit gap
#' and biases `phi` downward toward `phi^mean(dt)` (roughly 0.29
#' for this dataset), leaving the SST smooth to absorb persistence
#' that belongs to the latent state. `trend_formula = ~ CAR()`
#' with the supplied `time` column recovers `phi` near `0.7` in
#' continuous time and renders the SST smooth identifiable.
#'
#' @format A `data.frame` with 150 rows and the following fields:
#' \describe{
#'   \item{series}{factor identifying the reef (`flynn`,
#'     `myrmidon`, `pixie`); duplicated as `reef`.}
#'   \item{time}{integer month index (irregularly spaced within
#'     each reef).}
#'   \item{y}{numeric bleaching index (Gaussian, scaled);
#'     duplicated as `bleaching`.}
#'   \item{reef}{factor, alias of `series`.}
#'   \item{sst}{numeric observed sea-surface temperature in
#'     degrees Celsius.}
#'   \item{bleaching}{numeric bleaching index, alias of `y`.}
#' }
#' @details
#' True generative components:
#' * Per-reef intercepts (centred).
#' * Latent CAR(1) state per reef with `phi = 0.7` and
#'   `sigma = 0.5`, propagated through
#'   `trend[t] = phi^dt * trend[t-1] + N(0, sigma *
#'   sqrt((1 - phi^(2*dt)) / (1 - phi^2)))`.
#' * Shared smooth nonlinear obs-side effect of SST, evaluated via
#'   a thin-plate basis.
#' * Observation noise `sigma_obs = 0.3`.
#'
#' Recommended fit (state-space; SST on the observation equation,
#' CAR(1) on the latent state):
#' ```r
#' mvgam(
#'   formula = y ~ s(sst, k = 8),
#'   trend_formula = ~ CAR(),
#'   data = coral_surveys,
#'   family = gaussian()
#' )
#' ```
#'
#' Built by `data-raw/build_coral_surveys.R`.
"coral_surveys"
