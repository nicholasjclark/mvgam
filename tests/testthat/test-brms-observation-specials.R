
# mvgam computes predictions and log-likelihoods in R without either
# term. A fit would report predictions that leave them out.
test_that("addition terms outside the row model are refused", {
  expect_error(
    specials_program(y | se(sex, sigma = TRUE) ~ x, gaussian()),
    "'se\\(\\)' is not supported"
  )
  expect_error(
    specials_program(ybin | rate(ntrials) ~ x, poisson()),
    "offset\\(log"
  )
  # A `subset()` response leaves the time and series axes
  d <- make_specials_data()
  d$keep <- TRUE
  d$unit <- seq_len(nrow(d))
  expect_error(specials_program(y | subset(keep) ~ x, gaussian(), d),
               "Set the response to NA")
  expect_error(specials_program(y | index(unit) ~ x, gaussian(), d),
               "'index\\(\\)' is not supported")
})

# ---- Trend-side formula_ad gatekeeper -------------------------------

test_that(
  "mvgam_formula() refuses every addition term in trend_formula", {
  banned <- c(
    "se", "cens", "trunc", "trials", "weights", "rate",
    "vreal", "vint", "subset", "index", "dec", "cat", "thres"
  )
  for (sp in banned) {
    tf <- stats::as.formula(paste0("~ ", sp, "(sex) + AR(p = 1)"))
    expect_error(
      mvgam_formula(y ~ 1, trend_formula = tf),
      regexp = "addition terms are not supported"
    )
  }
})

test_that(
  "a covariate sharing a name with an addition term is accepted", {
  # `defense` shares the substring `se` with the `se()` ad-term;
  # `se_x` starts with `se`; `mi_score` starts with `mi`. The
  # earlier regex-on-deparsed-string detector matched all three.
  # The structural AST walker (formula_rhs_function_names) must not.
  expect_no_error(
    mvgam_formula(
      y ~ 1, trend_formula = ~ defense + se_x + AR(p = 1)
    )
  )
  expect_no_error(
    mvgam_formula(
      y ~ 1, trend_formula = ~ mi_score + AR(p = 1)
    )
  )
})

# ---- Structural walker primitives -----------------------------------

test_that(
  "collect_call_names walks nested calls and resolves namespaces", {
  expect_setequal(
    collect_call_names(quote(s(x) + AR(p = 1))),
    c("+", "s", "AR")
  )
  # Namespace-qualified calls collapse to the bare function name.
  expect_true("mi" %in% collect_call_names(quote(brms::mi(x))))
  # Symbols and literals contribute nothing.
  expect_identical(collect_call_names(quote(x)), character(0L))
  expect_identical(collect_call_names(1L), character(0L))
})

test_that("every mvgam family's own dpars read as univariate", {
  # Regressing a distributional parameter, `mtail ~ site` say, still
  # describes one response. A dpar missing from the list reads as a
  # second response instead, and the trend then never reaches `mu`:
  # the model compiles, samples, and answers without the trend in
  # it. Ask each family for its parameters rather than naming them
  # here, so a family added with a new one cannot slip through the
  # way `beta_nb()`'s `mtail` did.
  families <- list(
    tweedie(), beta_nb(), com_binomial(), nmix(), occ(),
    diri(), multi(), categ(), mvn(), mvt()
  )
  declared <- unique(unlist(lapply(families, function(f) {
    setdiff(f$dpars, "mu")
  })))
  expect_true(all(declared %in% mvgam_distributional_params))

  for (dpar in declared) {
    form <- brms::bf(y ~ x) +
      brms::lf(stats::as.formula(paste(dpar, "~ z")))
    expect_false(is_multivariate_formula(form))
  }

  # brms-native dpars, and `Psi`, which mvn() and mvt() declare
  # through stanvars rather than as a dpar.
  for (dpar in c("sigma", "phi", "Psi")) {
    form <- brms::bf(y ~ x) +
      brms::lf(stats::as.formula(paste(dpar, "~ z")))
    expect_false(is_multivariate_formula(form))
  }
})

test_that("formula_rhs_function_names walks only the RHS", {
  expect_setequal(
    formula_rhs_function_names(y ~ s(x) + AR(p = 1)),
    c("+", "s", "AR")
  )
})

test_that("response_call_name() tolerates namespace qualification", {
  # `as.character()` on `brms::mvbind(y1, y2)` returns
  # c("::", "brms", "mvbind"), so comparing it to a single name errors
  # with "the condition has length > 1" instead of reporting an
  # unrecognised response.
  expect_identical(
    mvgam:::response_call_name(quote(mvbind(y1, y2))), "mvbind"
  )
  expect_identical(
    mvgam:::response_call_name(quote(brms::mvbind(y1, y2))), "mvbind"
  )
  expect_identical(
    mvgam:::response_call_name(quote(cbind(s, f))), "cbind"
  )
  # a bare symbol is not a call and has no function name
  expect_identical(mvgam:::response_call_name(quote(y)), "")
})

test_that("is_multivariate_formula() accepts a namespaced mvbind response", {
  expect_true(mvgam:::is_multivariate_formula(mvbind(y1, y2) ~ x))
  expect_true(mvgam:::is_multivariate_formula(brms::mvbind(y1, y2) ~ x))
  expect_false(mvgam:::is_multivariate_formula(y ~ x))
  # cbind() denotes binomial trials, not multiple responses
  expect_false(mvgam:::is_multivariate_formula(cbind(succ, fail) ~ x))
})

test_that("format_model_formula() deparses a brmsformula to one line", {
  # `format()` on a brmsformula returns one string per list element,
  # so printing with `sep = ""` would glue the trailing NULLs on
  # as `y | trials(n) ~ 1NULLNULLyNULL` unless they are collapsed
  # first.
  expect_identical(
    mvgam:::format_model_formula(bf(y | trials(n) ~ 1)),
    "y | trials(n) ~ 1"
  )
  # A plain formula and a bare `~` right-hand side still work.
  expect_identical(mvgam:::format_model_formula(y ~ x), "y ~ x")
})

test_that("an addition term is matched as a call, not as a name", {
  # Addition terms live on the response side, so a covariate that
  # happens to share the term's name must not read as the term.
  expect_identical(mvgam:::find_aterm_call(bf(y | trials(n) ~ x),
                                           "trials"), quote(trials(n)))
  expect_null(mvgam:::find_aterm_call(bf(y ~ x), "trials"))
  expect_null(mvgam:::find_aterm_call(y ~ trials, "trials"))
})

test_that("the trials denominator resolves against the prediction data", {
  # The likelihood covers observed responses only. Predictions cover
  # every row. The denominator is evaluated against the rows predicted.
  dat <- data.frame(y = 1:4, n_trials = c(5L, 6L, 7L, 8L))
  values <- function(f) {
    mvgam:::addition_term_values(f, binomial(), dat)$trials
  }
  expect_identical(values(bf(y | trials(n_trials) ~ 1)), c(5L, 6L, 7L, 8L))
  # A constant has no column and is recycled to every row. One held in
  # the formula's environment resolves there.
  n_fixed <- 30
  expect_identical(values(bf(y | trials(n_fixed) ~ 1)), rep(30, 4L))
  # A column the data lacks leaves nothing to resolve.
  expect_null(values(bf(y | trials(absent) ~ 1)))
})

test_that("an addition term is not counted as a response", {
  # `y | trials(n)` names one response. Reading variable names off the
  # whole left-hand side would also return the addition variables,
  # which every consumer reading the response column relies on being
  # excluded.
  expect_equal(response_columns(y | trials(n) ~ x), c(y = "y"))
  expect_equal(response_columns(y | weights(w) + cens(c) ~ x), c(y = "y"))
  expect_equal(response_columns(y | trunc(lb = 0) ~ x), c(y = "y"))
  # A multivariate response keeps every outcome past its aterms
  expect_equal(
    response_columns(mvbind(y1, y2) | weights(w) ~ x),
    c(y1 = "y1", y2 = "y2")
  )
})

test_that("a response is keyed as brms keys it and read by its column", {
  # brms drops every '.' and '_' from a response's name, so the key a
  # `resp` argument takes and the column the frame holds differ. Taking
  # one for the other refused every trend model whose response
  # carried either character.
  expect_equal(response_columns(my_y ~ x), c(myy = "my_y"))
  expect_equal(response_columns(brms::bf(my_y ~ x)), c(myy = "my_y"))
  expect_equal(response_columns(log(y) ~ x), c(logy = "y"))
  expect_equal(
    response_columns(brms::bf(my_count ~ x) + brms::bf(mass.kg ~ x)),
    c(mycount = "my_count", masskg = "mass.kg")
  )
  expect_equal(response_columns(mvbind(y1, y2) ~ x),
               c(y1 = "y1", y2 = "y2"))
})
