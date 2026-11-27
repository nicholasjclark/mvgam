# Helpers shared by test-stancode-standata.R and test-extra-stancode-standata.R.


# ---- AR() prior + coef_sharing surface (Step 3) -------------------

# Shared fixture for the four valid combinations + the
# constructor-side rejections. Small panel because we only
# inspect the emitted stancode, not the posterior.
ar_step3_fixture <- function() {
  set.seed(1L)
  data.frame(
    y = rnorm(80L),
    time = rep(1:20L, 4L),
    series = factor(rep(paste0("s", 1:4), each = 20L))
  )
}


# ---- brms code-generation options ---------------------------------
# `knots`, `sample_prior`, `drop_unused_levels` and `normalize` are
# documented arguments that reach brms three times over: once when the
# mock fit is built, once when the Stan program is regenerated and once
# when the Stan data is. Each of these pins one of those journeys.

codegen_test_data <- function(n_time = 24L, n_series = 2L) {
  set.seed(11)
  out <- data.frame(
    time = rep(seq_len(n_time), n_series),
    series = factor(rep(paste0("s", seq_len(n_series)), each = n_time)),
    elev = runif(n_time * n_series, 0, 100)
  )
  out$y <- rpois(nrow(out), 5)
  out
}

# One series of forty occasions, with a Gaussian partner response.
injection_frame <- function() {
  set.seed(11)
  n <- 40L
  d <- data.frame(
    time = seq_len(n), series = factor("s1"), x = rnorm(n),
    e = runif(n, 1, 2), tr = rep(10L, n), yh = rnorm(n), yg = rnorm(n),
    yB01 = runif(n, 0.1, 0.9), yp = rpois(n, 3), yk = rbinom(n, 10, 0.4)
  )
  d$yB01[1:4] <- c(0, 1, 0, 1)
  d
}
