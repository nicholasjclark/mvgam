
test_that("gp_call_to_spec errors on a malformed call with no variables", {
  # A gp() term needs a variable. One with named arguments alone is
  # refused.
  bad_call <- str2lang("gp(k = 5)")
  expect_error(
    mvgam:::gp_call_to_spec(bad_call),
    "needs at least one variable"
  )
})

# Prior distribution formatter: targeted unit tests on the
# regex-based string mapper. Easier to keep in sync with brms
# prior-string conventions when tested directly.

test_that("format_prior_distribution renders the arguments it is given", {
  fmt <- mvgam:::format_prior_distribution
  expect_equal(fmt("student_t(3, 0, 2.5)"), "\\text{StudentT}(3, 0, 2.5)")
  expect_equal(fmt("std_normal()"), "\\text{Normal}(0, 1)")
  expect_equal(fmt("(flat)"), "\\text{flat}")
  expect_equal(fmt(""), "\\text{flat}")
  # A full-precision location is shown to three significant digits
  expect_equal(fmt("normal(0.123456, 1)"), "\\text{Normal}(0.123, 1)")
  expect_equal(fmt("normal(0, sqrt(0.455))"),
               "\\text{Normal}(0, \\sqrt{0.455})")
  # A sampled hyperparameter takes its symbol. Another name is set
  # upright, with its underscore escaped.
  expect_equal(fmt("inv_gamma(mgp_a1, 1)", symbols = c(mgp_a1 = "a_1")),
               "\\text{InvGamma}(a_1, 1)")
  expect_equal(fmt("inv_gamma(mgp_a1, 1)"),
               "\\text{InvGamma}(\\mathrm{mgp\\_a1}, 1)")
  # An unrecognised distribution is rendered verbatim
  expect_equal(fmt("frechet(1, 2)"), "\\text{frechet(1, 2)}")
})


test_that("kernel and basis labels take the setting the spec records", {
  # `gp_call_to_spec()` and `parse_smooth_coef()` record brms's and
  # mgcv's own defaults where the user wrote none.
  spec <- mvgam:::gp_call_to_spec(quote(gp(x, k = 5)))
  expect_identical(
    mvgam:::gp_kernel_names(spec$cov),
    c(label = "exponentiated-quadratic", symbol = "k_{\\text{ExpQuad}}")
  )
  smooth <- mvgam:::parse_smooth_coef("s(x)")
  expect_identical(
    mvgam:::basis_label(smooth$bs, smooth$fname),
    "thin plate regression spline"
  )
})


test_that("a moving-average trend is named as one", {
  # `AR(ma = TRUE)` is recorded under trend type "AR" with its MA lags.
  # The order label and the coefficient definitions name the MA part.
  fake <- function(tt, ar, ma) {
    structure(
      list(trend_metadata = list(trend_type = tt, ar_lags = ar,
                                 ma_lags = ma)),
      class = "mvgam"
    )
  }
  expect_identical(
    mvgam:::trend_order_label(fake("AR", 1L, integer(0))), "AR(1)"
  )
  expect_identical(
    mvgam:::trend_order_label(fake("AR", 1L, 1L)), "ARMA(1, 1)"
  )
  expect_identical(
    mvgam:::trend_order_label(fake("VAR", 1L, integer(0))), "VAR(1)"
  )
  expect_identical(
    mvgam:::trend_order_label(fake("VAR", 1L, 1L)), "VARMA(1, 1)"
  )
  # A contiguous lag set is named by its order. `ARMA(1, 2, 1)` is
  # the spelling of an ARIMA order.
  expect_identical(
    mvgam:::trend_order_label(fake("AR", 1:2, integer(0))), "AR(2)"
  )
  expect_identical(
    mvgam:::trend_order_label(fake("AR", 1:2, 1L)), "ARMA(2, 1)"
  )
  expect_identical(
    mvgam:::trend_order_label(fake("VAR", 1:3, integer(0))), "VAR(3)"
  )
  # A sparse lag set lists its lags in braces.
  expect_identical(
    mvgam:::trend_order_label(fake("AR", c(1L, 12L), integer(0))),
    "AR({1, 12})"
  )
  expect_identical(
    mvgam:::trend_order_label(fake("AR", c(1L, 12L), 1L)),
    "ARMA({1, 12}, 1)"
  )
  expect_true(mvgam:::trend_has_ma(fake("AR", 1L, 1L)))
  expect_false(mvgam:::trend_has_ma(fake("AR", 1L, integer(0))))
})


test_that("me() arguments count as R matches them", {
  # A grouping factor written by position may follow a named SD
  for (term in c("me(x, sdx = s, g)", "me(x, g, sdx = s)",
                 "me(gr = g, s, x = x)")) {
    expect_identical(
      mvgam:::me_call_spec(str2lang(term)),
      list(var = "x", sdvar = "s", gr = "g")
    )
    expect_identical(
      mvgam:::me_call_spec(mvgam:::ungroup_me_terms(str2lang(term))),
      list(var = "x", sdvar = "s", gr = NA_character_)
    )
  }
})


test_that("a coefficient takes its group's prior before its class's", {
  prior <- data.frame(
    prior = c("student_t(3, 0, 3.4)", "student_t(3, 0, 2.5)", "", ""),
    class = "sd", coef = c("", "", "env", "env"),
    group = c("", "series", "series", "site"),
    resp = "", dpar = "", nlpar = ""
  )
  expect_identical(
    mvgam:::backfill_umbrella_priors(prior)$prior,
    c("student_t(3, 0, 3.4)", "student_t(3, 0, 2.5)",
      "student_t(3, 0, 2.5)", "student_t(3, 0, 3.4)")
  )
})


test_that("an expression's coefficient takes the expression's symbol", {
  # brms names the column of `I(env^2)` `IenvE2`
  expect_identical(
    mvgam:::formula_expression_symbols(
      list(formula = y ~ env + I(env^2) + log(x_1 + 1) + env:x2)
    ),
    c(IenvE2 = "(\\mathrm{env}^2)", logx_1P1 = "(log(\\mathrm{x\\_1} + 1))")
  )
})
