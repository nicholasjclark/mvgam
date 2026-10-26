## Coverage of brms observation-level specials through the
## mvgam_formula -> stancode/standata pipeline, plus the
## trend_formula formula_ad gatekeeper.
##
## Two surfaces under test:
##   1. Observation side: each brms special that mvgam claims to
##      support (`mm()`, `me()`, `cs()`, `se()`, `cens()`,
##      `trials()`, `car()`) plumbs through to specific standata
##      keys and stancode markers via the mvgam dispatch path.
##   2. Trend side: `mvgam_formula()` refuses every brms
##      `formula_ad` special. Detection walks the formula AST, which
##      keeps a covariate named like an addition term from matching.

make_specials_data <- function(seed = 1L, n = 60L) {
  set.seed(seed)
  data.frame(
    y        = rnorm(n),
    ybin     = rbinom(n, size = 10L, prob = 0.4),
    yord     = factor(sample(1:4, n, replace = TRUE), ordered = TRUE),
    x        = rnorm(n),
    sdx      = abs(rnorm(n, 0.2, 0.05)),
    sex      = abs(rnorm(n, 0.5, 0.1)),
    cens_ind = sample(c("none", "right"), n, replace = TRUE),
    ntrials  = sample(8:12, n, replace = TRUE),
    g1       = factor(sample(letters[1:4], n, replace = TRUE)),
    g2       = factor(sample(letters[5:8], n, replace = TRUE)),
    loc      = factor(paste0("L", rep(1:10, each = n / 10L))),
    time     = rep(seq_len(n / 2L), 2L),
    series   = factor(rep(c("a", "b"), each = n / 2L))
  )
}

# Adjacency for the `car()` test: a chain over the 10 location levels
make_loc_adjacency <- function(d) {
  M <- matrix(0L, 10L, 10L)
  diag(M[-1L, ]) <- 1L
  M <- M + t(M)
  rownames(M) <- colnames(M) <- levels(d$loc)
  M
}

# ---- Observation-side specials wire through to stancode/standata ----

# One prefit gives the program and its data. Each test checks the
# standata key brms emits for its special, the column that fills it,
# and the model-block line that uses it.
specials_program <- function(formula, family, d = make_specials_data(),
                             ...) {
  mod <- mvgam(formula, trend_formula = ~ AR(p = 1), data = d,
               family = family, run_model = FALSE, silent = 2, ...,
               validate = FALSE)
  list(code = as.character(mod$stancode), data = mod$standata)
}

test_that("trials() plumbs through to mvgam stancode and standata", {
  d <- make_specials_data()
  prog <- specials_program(ybin | trials(ntrials) ~ x, binomial(), d)
  # The column the user named is what arrives. A length check passed
  # on a constant, and on any other column of the frame.
  expect_identical(as.integer(prog$data$trials), as.integer(d$ntrials))
  expect_match(prog$code, "binomial.*\\|.*trials")
})

test_that("se() plumbs through and combines with sigma in the likelihood", {
  d <- make_specials_data()
  prog <- specials_program(y | se(sex, sigma = TRUE) ~ x, gaussian(), d)
  expect_identical(as.numeric(prog$data$se), as.numeric(d$sex))
  # brms emits `sqrt(square(sigma) + se2)` when sigma = TRUE.
  expect_match(prog$code, "sqrt\\(square\\(sigma\\) \\+ se2\\)")
})

test_that("cens() plumbs through and produces censored-likelihood blocks", {
  d <- make_specials_data()
  prog <- specials_program(y | cens(cens_ind) ~ x, gaussian(), d)
  expect_identical(as.integer(prog$data$cens),
                   as.integer(d$cens_ind == "right"))
  # Right-censored rows emit a `normal_lccdf` term in the likelihood.
  expect_match(prog$code, "normal_lccdf")
})

test_that("me() plumbs through measurement-error parameters and prior", {
  d <- make_specials_data()
  prog <- specials_program(y ~ me(x, sdx), gaussian(), d)
  # The noisy covariate and its standard error arrive as the columns
  # `me(x, sdx)` names. Lengths alone passed with the two swapped.
  expect_identical(as.numeric(prog$data$Xn_1), as.numeric(d$x))
  expect_identical(as.numeric(prog$data$noise_1), as.numeric(d$sdx))
  # Latent-observation prior connecting noisy x to error-free Xme.
  expect_match(prog$code, "normal_lpdf\\(Xn_1 \\| Xme_1, noise_1\\)")
})

test_that("mm() plumbs through multiple-membership grouping structure", {
  prog <- specials_program(y ~ x + (1 | mm(g1, g2)), gaussian())
  # brms emits one J_/W_ block per membership level.
  expect_true(all(c("J_1_1", "J_1_2", "W_1_1", "W_1_2") %in%
                    names(prog$data)))
  # Weighted sum across membership levels appears in the linear predictor.
  expect_match(prog$code, "W_1_1\\[n\\] \\* r_1_1\\[J_1_1\\[n\\]\\]")
})

test_that("cs() plumbs through category-specific effects for cumulative", {
  # brms warns that cs() is experimental for cumulative families.
  # mvgam passes the warning to the user once per call.
  expect_warning(
    prog <- specials_program(yord ~ cs(x), cumulative()),
    "Category specific effects"
  )
  expect_true(all(c("Xcs", "Kcs", "nthres") %in% names(prog$data)))
  expect_match(prog$code, "matrix\\[N, nthres\\] mucs = Xcs \\* bcs")
})

test_that("car() resolves its data2 adjacency and emits spatial CAR code", {
  # A special that looks up an object outside `data` once failed with
  # "Object 'M' was not found in 'data2'."
  d <- make_specials_data()
  M <- make_loc_adjacency(d)
  prog <- specials_program(y ~ x + car(M, gr = loc, type = "icar"),
                           gaussian(), d, data2 = list(M = M))
  # ICAR graph keys derived from the adjacency.
  expect_true(all(c("Jloc", "Nedges", "edges1", "edges2") %in%
                    names(prog$data)))
  expect_identical(as.integer(prog$data$Nloc), 10L)
  # Spatial-CAR pairwise-difference penalty appears in the model.
  expect_match(prog$code,
               "dot_self\\(zcar\\[edges1\\] - zcar\\[edges2\\]\\)")
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
  # A model has two row spaces: the likelihood covers observed
  # responses only, predictions cover every row. The denominator
  # brms stores in `standata` belongs to the first, so the
  # prediction path resolves it from the data instead.
  dat <- data.frame(y = 1:4, n_trials = c(5L, 6L, 7L, 8L))
  expect_identical(
    mvgam:::resolve_trials_denominator(bf(y | trials(n_trials) ~ 1), dat),
    c(5L, 6L, 7L, 8L)
  )
  # A constant denominator has no column and is recycled to fit.
  expect_identical(
    mvgam:::resolve_trials_denominator(bf(y | trials(30) ~ 1), dat),
    rep(30, 4L)
  )
  # Nothing to resolve when the term is absent or its column is not
  # carried by the data being predicted.
  expect_null(mvgam:::resolve_trials_denominator(bf(y ~ 1), dat))
  expect_null(
    mvgam:::resolve_trials_denominator(bf(y | trials(absent) ~ 1), dat)
  )
})

test_that("an addition term is not counted as a response", {
  # `y | trials(n)` names one response. Reading variable names off the
  # whole left-hand side would also return the addition variables,
  # which every consumer reading the response column relies on being
  # excluded.
  expect_equal(response_columns(y | trials(n) ~ x), c(y = "y"))
  expect_equal(response_columns(y | weights(w) + cens(c) ~ x), c(y = "y"))
  expect_equal(response_columns(y | trunc(lb = 0) ~ x), c(y = "y"))
  expect_equal(response_columns(y | se(s, sigma = TRUE) ~ x), c(y = "y"))
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

test_that("a response named with an underscore or a dot builds a trend model", {
  # The frame is read by the column and the program is written with the
  # key. Reading the frame by the key refused both of these outright.
  set.seed(3L)
  dat <- data.frame(time = 1:20, x = rnorm(20L), series = factor("a"))
  dat$my_y <- rpois(20L, 4)
  uni <- mvgam(brms::bf(my_y ~ x), trend_formula = ~ AR(), data = dat,
               family = poisson(), run_model = FALSE, silent = 2,
               validate = FALSE)
  expect_identical(as.integer(uni$standata$N), 20L)
  expect_identical(response_columns(uni), c(myy = "my_y"))

  wide <- data.frame(time = 1:20, x = rnorm(20L))
  wide$my_count <- rpois(20L, 4)
  wide$mass.kg <- rnorm(20L)
  wide$my_count[c(3L, 9L)] <- NA
  arms <- brms::bf(my_count ~ x, family = poisson()) +
    brms::bf(mass.kg ~ x, family = gaussian()) + brms::set_rescor(FALSE)
  # brms notes the gaps once; that notice is the only one expected.
  seen <- character(0L)
  fit <- withCallingHandlers(
    mvgam(arms, trend_formula = ~ AR(), data = wide,
          run_model = FALSE, silent = 2, validate = FALSE),
    warning = function(w) {
      seen <<- c(seen, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_identical(seen, "Rows containing NAs were excluded from the model.")
  # The series axis holds the keys, and each arm maps the rows its own
  # column was observed at.
  expect_identical(mvgam_axes(fit)$series$levels, c("mycount", "masskg"))
  expect_identical(as.integer(fit$standata$obs_trend_time_mycount),
                   setdiff(1:20, c(3L, 9L)))
  expect_identical(as.integer(fit$standata$obs_trend_time_masskg), 1:20)
})
