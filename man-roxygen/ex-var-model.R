#' @examples
#' \dontrun{
#' # Ten years of monthly counts of a prey species and its predator.
#' # Predators suppress the prey a month later. Prey raise the
#' # predators a month later.
#' set.seed(2025)
#' n_months <- 120
#' A <- rbind(prey     = c(0.6, -0.5),
#'            predator = c(0.4,  0.5))
#' z <- matrix(0, n_months, 2)
#' for (t in 2:n_months) {
#'   z[t, ] <- A %*% z[t - 1, ] + rnorm(2, sd = 0.35)
#' }
#' predator_prey <- data.frame(
#'   time = rep(seq_len(n_months), 2),
#'   series = factor(rep(c("prey", "predator"), each = n_months),
#'                   levels = c("prey", "predator")),
#'   y = rpois(2 * n_months, exp(c(2.5 + z[, 1], 1.5 + z[, 2])))
#' )
#' var_mod <- mvgam(y ~ series, trend_formula = ~ VAR(),
#'                  data = predator_prey, family = poisson(),
#'                  chains = 2, silent = 2)
#' }
