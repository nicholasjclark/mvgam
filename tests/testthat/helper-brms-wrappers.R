# A stub mvgam fit, shared by test-brms-wrappers.R and its extras file.
make_wrapper_stub <- function(with_re = FALSE) {
  varnames <- c(
    "b_Intercept", "b_x", "Intercept", "sigma",
    if (with_re) c("sd_grp__Intercept", "r_grp[a,Intercept]") else NULL,
    "lp__"
  )
  set.seed(2L)
  arr <- array(
    rnorm(50L * 2L * length(varnames)),
    dim = c(50L, 2L, length(varnames)),
    dimnames = list(NULL, NULL, varnames)
  )
  drws <- posterior::as_draws_array(arr)
  std <- list(N = 8L, X = matrix(rnorm(16L), 8L, 2L,
                                  dimnames = list(NULL,
                                                  c("Intercept", "x"))))
  if (with_re) {
    std$M_1 <- 1L
    std$N_1 <- 1L
  }
  form <- if (with_re) brms::bf(y ~ x + (1 | grp)) else brms::bf(y ~ x)
  structure(
    list(
      fit = drws,
      formula = form,
      data = data.frame(y = rnorm(8L), x = rnorm(8L),
                         grp = factor("a")),
      family = brms::brmsfamily("gaussian"),
      standata = std
    ),
    class = "mvgam"
  )
}

