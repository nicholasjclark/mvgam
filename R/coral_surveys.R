#' Simulated coral bleaching surveys with an irregular CAR(1) state
#'
#' A state-space dataset for three coral reefs surveyed at
#' irregular monthly intervals (gap distribution
#' `Uniform(1, 6)` months, mean approximately 3.5). Each survey
#' inspects between 20 and 40 colonies and counts those that have
#' bleached. The latent state is a continuous-time AR(1) (CAR(1))
#' trend per reef with `phi = 0.9` and `sigma = 0.3` on the logit
#' scale. The observation equation adds an observed sea-surface
#' temperature (SST) driver through a smooth nonlinear effect.
#'
#' A naive AR(1) treats every consecutive survey as one step. It
#' gives a one-month gap and a six-month gap the same persistence.
#' `trend_formula = ~ CAR()` with the supplied `time` column decays
#' the state by the length of each gap. Its `ar1_trend` measures the
#' persistence over the median gap between surveys, three months
#' here. The simulated value is `0.9^3 = 0.73`.
#'
#' @format A `data.frame` with 150 rows and the following fields:
#' \describe{
#'   \item{series}{factor identifying the reef (`flynn`,
#'     `myrmidon`, `pixie`); duplicated as `reef`.}
#'   \item{time}{integer month index (irregularly spaced within
#'     each reef).}
#'   \item{y}{integer count of bleached colonies; duplicated as
#'     `bleaching`.}
#'   \item{reef}{factor, alias of `series`.}
#'   \item{sst}{numeric observed sea-surface temperature in
#'     degrees Celsius.}
#'   \item{bleaching}{integer count of bleached colonies, alias of
#'     `y`.}
#'   \item{colonies}{integer number of colonies inspected, the
#'     binomial trials.}
#' }
#' @details
#' True generative components, on the logit scale:
#' * An intercept of -0.85, which puts a typical survey near 30%
#'   bleached. Each reef adds its own centred intercept.
#' * Latent CAR(1) state per reef with `phi = 0.9` and
#'   `sigma = 0.3`, propagated through
#'   `trend[t] = phi^dt * trend[t-1] + N(0, sigma *
#'   sqrt((1 - phi^(2*dt)) / (1 - phi^2)))`.
#' * Shared smooth nonlinear effect of SST, evaluated through a
#'   thin-plate basis.
#' * Binomial observations: `bleaching ~ Binomial(colonies, p)`.
#'
#' @seealso [CAR()] for the trend, [AR()] for evenly spaced surveys,
#'   [mvgam_data()] to check the data.
#'
#' @examples
#' data(coral_surveys)
#' mvgam_data(coral_surveys, formula = y | trials(colonies) ~ s(sst),
#'            family = binomial())
#'
#' \dontrun{
#' # A model with the structure the data were simulated from
#' mod <- mvgam(
#'   formula = y | trials(colonies) ~ series + s(sst, k = 8),
#'   trend_formula = ~ CAR(),
#'   data = coral_surveys,
#'   family = binomial(),
#'   chains = 2, silent = 2
#' )
#' summary(mod)
#' }
"coral_surveys"
