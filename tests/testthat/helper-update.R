# Helpers shared by test-update.R and test-extra-update.R.
# Tests for update.mvgam without a refit: signature parity with
# brms::update.brmsfit, the refusals, and the arguments a refit
# inherits, each checked on a stub fit.

make_update_stub <- function(n_iter = 50L, n_chains = 2L) {
  varnames <- c(
    "b_Intercept", "b_x", "Intercept",
    "sigma", "sigma_trend[1]", "ar1_trend[1]",
    "trend[1,1]", "lp__"
  )
  set.seed(3L)
  arr <- array(
    rnorm(n_iter * n_chains * length(varnames)),
    dim = c(n_iter, n_chains, length(varnames)),
    dimnames = list(NULL, NULL, varnames)
  )
  drws <- posterior::as_draws_array(arr)
  X <- model.matrix(~ x, data.frame(x = rnorm(10)))
  colnames(X)[1L] <- "Intercept"
  dat <- data.frame(y = rnorm(10), x = rnorm(10), z = rnorm(10))
  structure(
    list(
      fit = drws,
      formula = brms::bf(y ~ x),
      trend_formula = NULL,
      family = stats::gaussian(),
      prior = brms::get_prior(y ~ x, data = dat),
      data = dat,
      standata = list(X = X, K = ncol(X), Kc = ncol(X) - 1L),
      stancode = paste0("data { int N; } parameters { real mu; } ",
                        "model { mu ~ normal(0,1); }"),
      backend = "cmdstanr",
      algorithm = "sampling",
      call = call("mvgam", formula = y ~ x)
    ),
    class = "mvgam"
  )
}
