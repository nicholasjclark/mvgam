# Shared by test-sample-innovations.R and its extras file.

# One draw's coefficients in the shape `ar_coef_draws()` returns.
one_draw_coefs <- function(lags, phi, theta = NULL) {
  list(lags = lags,
       phi = lapply(phi, function(v) matrix(v, nrow = 1L)),
       theta = if (is.null(theta)) NULL else matrix(theta, nrow = 1L))
}

stationary_factor <- function(...) {
  f <- ar_stationary_factor(one_draw_coefs(...))
  matrix(f[1L, , ], dim(f)[2L], dim(f)[3L])
}

