# Helpers shared by test-posterior-smooths.R and test-extra-posterior-smooths.R.

smooth_frame <- function(n = 40L) {
  set.seed(5L)
  data.frame(
    y = rnorm(n), x = rnorm(n), z = rnorm(n), w = rnorm(n),
    grp = factor(sample(letters[1:3], n, replace = TRUE),
                 levels = letters[1:3]),
    trend_y = rnorm(n),
    count = rbinom(n, 10L, 0.4), size = 10L
  )
}

# A fit-shaped object holding the brms models the smooth record reads,
# and `fit`, a draws matrix, where a test evaluates a smooth.
smooth_stub <- function(obs, data = smooth_frame(), family = gaussian(),
                        trend = NULL, fit = NULL, prior = NULL) {
  structure(
    list(
      formula = obs, data = data, family = family, fit = fit,
      obs_model = brms::brm(obs, data = data, family = family,
                            prior = prior, empty = TRUE),
      trend_model = if (!is.null(trend)) {
        brms::brm(trend, data = data, empty = TRUE)
      }
    ),
    class = "mvgam"
  )
}
