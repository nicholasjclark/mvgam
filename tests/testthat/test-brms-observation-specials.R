## Coverage of brms observation-level specials through the
## mvgam_formula -> stancode/standata pipeline, plus the
## trend_formula formula_ad gatekeeper.
##
## Two surfaces under test:
##   1. Observation side: each brms special that mvgam claims to
##      support (`mm()`, `me()`, `cs()`, `se()`, `cens()`,
##      `trials()`, `car()`) plumbs through to specific standata
##      keys and stancode markers via the mvgam dispatch path.
##   2. Trend side: `mvgam_formula()` rejects every brms
##      `formula_ad` special EXCEPT `mi()`. Detection walks the
##      formula AST so user variable names sharing substrings
##      with addition-term names cannot false-positive.

make_specials_data <- function(seed = 1L, n = 60L) {
  set.seed(seed)
  data.frame(
    y        = rnorm(n),
    ycount   = rpois(n, 5),
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
    defense  = rnorm(n),
    se_x     = rnorm(n),
    mi_score = rnorm(n),
    time     = rep(seq_len(n / 2L), 2L),
    series   = factor(rep(c("a", "b"), each = n / 2L))
  )
}

# Build the simple adjacency used by `car()` tests. Ring graph over
# the 10 location levels (chain plus a wrap edge).
make_loc_adjacency <- function(d) {
  M <- matrix(0L, 10L, 10L)
  diag(M[-1L, ]) <- 1L
  M <- M + t(M)
  rownames(M) <- colnames(M) <- levels(d$loc)
  M
}

# ---- Observation-side specials wire through to stancode/standata ----

# Each test follows the same shape: build via mvgam_formula() ->
# stancode()/standata(), then assert on (a) the distinctive
# standata key(s) brms emits for that special and (b) a non-trivial
# stancode marker showing the special is actually used in the
# model block, not just declared in data.

test_that("trials() plumbs through to mvgam stancode and standata", {
  d <- make_specials_data()
  mf <- mvgam_formula(
    ybin | trials(ntrials) ~ x, trend_formula = ~ AR(p = 1)
  )
  sc <- stancode(mf, data = d, family = binomial(),
                 trend_formula = ~ AR(p = 1))
  sd_ <- standata(mf, data = d, family = binomial(),
                  trend_formula = ~ AR(p = 1))
  expect_true("trials" %in% names(sd_))
  expect_length(sd_$trials, nrow(d))
  expect_match(as.character(sc), "binomial.*\\|.*trials")
})

test_that("se() plumbs through and combines with sigma in the likelihood", {
  d <- make_specials_data()
  mf <- mvgam_formula(
    y | se(sex, sigma = TRUE) ~ x, trend_formula = ~ AR(p = 1)
  )
  sc <- stancode(mf, data = d, family = gaussian(),
                 trend_formula = ~ AR(p = 1))
  sd_ <- standata(mf, data = d, family = gaussian(),
                  trend_formula = ~ AR(p = 1))
  expect_true("se" %in% names(sd_))
  expect_length(sd_$se, nrow(d))
  # brms emits `sqrt(square(sigma) + se2)` when sigma = TRUE.
  expect_match(as.character(sc), "sqrt\\(square\\(sigma\\) \\+ se2\\)")
})

test_that("cens() plumbs through and produces censored-likelihood blocks", {
  d <- make_specials_data()
  mf <- mvgam_formula(
    y | cens(cens_ind) ~ x, trend_formula = ~ AR(p = 1)
  )
  sc <- stancode(mf, data = d, family = gaussian(),
                 trend_formula = ~ AR(p = 1))
  sd_ <- standata(mf, data = d, family = gaussian(),
                  trend_formula = ~ AR(p = 1))
  expect_true("cens" %in% names(sd_))
  # Right-censored rows emit a `normal_lccdf` term in the likelihood.
  expect_match(as.character(sc), "normal_lccdf")
})

test_that("me() plumbs through measurement-error parameters and prior", {
  d <- make_specials_data()
  mf <- mvgam_formula(y ~ me(x, sdx), trend_formula = ~ AR(p = 1))
  sc <- stancode(mf, data = d, family = gaussian(),
                 trend_formula = ~ AR(p = 1))
  sd_ <- standata(mf, data = d, family = gaussian(),
                  trend_formula = ~ AR(p = 1))
  # Distinctive me() standata keys.
  expect_true(all(c("Xn_1", "noise_1", "Mme_1") %in% names(sd_)))
  expect_length(sd_$Xn_1, nrow(d))
  expect_length(sd_$noise_1, nrow(d))
  # Latent-observation prior connecting noisy x to error-free Xme.
  expect_match(as.character(sc), "normal_lpdf\\(Xn_1 \\| Xme_1, noise_1\\)")
})

test_that("mm() plumbs through multiple-membership grouping structure", {
  d <- make_specials_data()
  mf <- mvgam_formula(
    y ~ x + (1 | mm(g1, g2)), trend_formula = ~ AR(p = 1)
  )
  sc <- stancode(mf, data = d, family = gaussian(),
                 trend_formula = ~ AR(p = 1))
  sd_ <- standata(mf, data = d, family = gaussian(),
                  trend_formula = ~ AR(p = 1))
  # brms emits one J_/W_/Z_ block per membership level.
  expect_true(all(c("J_1_1", "J_1_2", "W_1_1", "W_1_2") %in% names(sd_)))
  # Weighted sum across membership levels appears in the linear predictor.
  expect_match(as.character(sc),
               "W_1_1\\[n\\] \\* r_1_1\\[J_1_1\\[n\\]\\]")
})

test_that("cs() plumbs through category-specific effects for cumulative", {
  d <- make_specials_data()
  mf <- mvgam_formula(yord ~ cs(x), trend_formula = ~ AR(p = 1))
  # brms warns that cs() is experimental for cumulative families;
  # mvgam should pass that warning through, not error.
  sc <- suppressWarnings(stancode(
    mf, data = d, family = cumulative(), trend_formula = ~ AR(p = 1)
  ))
  sd_ <- suppressWarnings(standata(
    mf, data = d, family = cumulative(), trend_formula = ~ AR(p = 1)
  ))
  expect_true(all(c("Xcs", "Kcs", "nthres") %in% names(sd_)))
  expect_match(as.character(sc), "matrix\\[N, nthres\\] mucs = Xcs \\* bcs")
})

test_that("car() resolves its data2 adjacency and emits spatial CAR code", {
  # Regression coverage for the data2-forwarding bug: prior to the
  # fix, `setup_brms_lightweight`, `generate_base_stancode_with_stanvars`
  # and `generate_base_brms_standata` all dropped data2 silently,
  # so any brms special that looks up an object outside `data`
  # (`car()`, `cov_ranef()`) failed with
  # "Object 'M' was not found in 'data2'."
  d <- make_specials_data()
  M <- make_loc_adjacency(d)
  mf <- mvgam_formula(
    y ~ x + car(M, gr = loc, type = "icar"),
    trend_formula = ~ AR(p = 1)
  )
  sc <- stancode(mf, data = d, family = gaussian(),
                 trend_formula = ~ AR(p = 1), data2 = list(M = M))
  sd_ <- standata(mf, data = d, family = gaussian(),
                  trend_formula = ~ AR(p = 1), data2 = list(M = M))
  # ICAR-specific spatial graph keys derived from the adjacency.
  expect_true(all(c("Nloc", "Jloc", "Nedges", "edges1", "edges2")
                  %in% names(sd_)))
  expect_identical(as.integer(sd_$Nloc), 10L)
  # Spatial-CAR pairwise-difference penalty appears in the model.
  expect_match(as.character(sc), "dot_self\\(zcar\\[edges1\\] - zcar\\[edges2\\]\\)")
})

# ---- Trend-side formula_ad gatekeeper -------------------------------

test_that("mvgam_formula() rejects every formula_ad special on trend_formula except mi()", {
  banned <- c(
    "se", "cens", "trunc", "trials", "weights", "rate",
    "vreal", "vint", "subset", "index", "dec", "cat", "thres"
  )
  for (sp in banned) {
    tf <- stats::as.formula(paste0("~ ", sp, "(sex) + AR(p = 1)"))
    expect_error(
      mvgam_formula(y ~ 1, trend_formula = tf),
      regexp = "addition-terms not allowed"
    )
  }
})

test_that("mvgam_formula() allows mi() on trend_formula", {
  d <- make_specials_data()
  d$x_mi <- d$x
  d$x_mi[c(3L, 17L, 28L)] <- NA_real_
  expect_no_error(
    mvgam_formula(y ~ 1, trend_formula = ~ mi(x_mi) + AR(p = 1))
  )
})

test_that("structural detector does not false-positive on user names sharing substrings with addition-term names", {
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

test_that("collect_call_names walks nested calls and resolves namespace heads", {
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

test_that("formula_rhs_function_names walks only the RHS", {
  expect_setequal(
    formula_rhs_function_names(y ~ s(x) + AR(p = 1)),
    c("+", "s", "AR")
  )
  # LHS addition-terms (e.g. `y | cens(c) ~ x`) live on the left
  # of `|` and are not walked by this helper; the trend gatekeeper
  # uses it on RHS-only trend formulas (`~ ...`) where there is no
  # LHS to consider.
  expect_no_error(formula_rhs_function_names(~ AR(p = 1)))
})
