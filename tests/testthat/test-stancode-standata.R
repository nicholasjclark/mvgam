

test_that("a statement polishing moves stays in the scope it needs", {
  # brms's mixture likelihood declares `ps` in its loop and ends the
  # loop by adding it to target. The statement carries no index, and
  # moving it to the end of the block takes `ps` out of scope.
  code <- c(
    "model {",
    "  if (!prior_only) {",
    "    target += normal_lpdf(Z | 0, 1);",
    "    for (n in 1:N) {",
    "      array[2] real ps;",
    "      ps[1] = log(theta1) + normal_lpdf(Y[n] | mu1[n], sigma1);",
    "      ps[2] = log(theta2) + normal_lpdf(Y[n] | mu2[n], sigma2);",
    "      target += log_sum_exp(ps);",
    "    }",
    "  }",
    "  target += lprior;",
    "}"
  )
  out <- mvgam:::reorganize_target_statements(code)

  loop_start <- grep("for (n in 1:N)", out, fixed = TRUE)
  loop_end <- mvgam:::find_matching_closing_brace(out, loop_start)
  nested <- grep("log_sum_exp(ps)", out, fixed = TRUE)
  expect_gt(nested, loop_start)
  expect_lt(nested, loop_end)

  # A top-level statement still moves to the end of the block
  top_level <- grep("normal_lpdf(Z | 0, 1)", out, fixed = TRUE)
  expect_gt(top_level, loop_end)
})

test_that("polishing moves a statement written over two lines whole", {
  # brms writes the density of a bounded parameter with its normalising
  # constant on a second line
  code <- c(
    "transformed parameters {",
    "  vector[Kc] b;",
    "  real lprior = 0;",
    "  b = zb .* sdb;",
    "  lprior += student_t_lpdf(hs_global | hs_df_global, 0, hs_scale_global)",
    "    - 1 * log(0.5);",
    "}",
    "model {",
    "  target += std_normal_lpdf(zb);",
    "  target += student_t_lpdf(hs_local | hs_df, 0, 1)",
    "    - rows(hs_local) * log(0.5);",
    "  if (!prior_only) {",
    "    target += poisson_log_glm_lpmf(Y | Xc, Intercept, b);",
    "  }",
    "  target += lprior;",
    "}"
  )
  follows <- function(out, head, tail) {
    grep(tail, out, fixed = TRUE) == grep(head, out, fixed = TRUE) + 1L
  }

  out <- mvgam:::reorganize_lprior_statements(code)
  expect_true(follows(out, "lprior += student_t_lpdf(hs_global", "- 1 * log(0.5);"))
  expect_lt(grep("lprior += ", out, fixed = TRUE),
            grep("b = zb .* sdb;", out, fixed = TRUE))

  out <- mvgam:::reorganize_model_block_statements(code)
  expect_true(follows(out, "target += student_t_lpdf(hs_local",
                      "- rows(hs_local) * log(0.5);"))
  # The target statements follow the likelihood guard, lprior first
  guard_end <- mvgam:::find_matching_closing_brace(
    out, grep("if (!prior_only) {", out, fixed = TRUE)
  )
  targets <- grep("^\\s*target \\+=", out)
  targets <- targets[targets > guard_end]
  expect_match(out[targets[1]], "target += lprior;", fixed = TRUE)
  expect_length(targets, 3L)
})

test_that("a comment moves with the statement written below it", {
  # Polishing regroups the model block: priors, then the likelihood
  # guard, then the target statements with lprior first. A comment
  # written directly above a statement describes it and travels with
  # it. A comment separated from any statement by a blank line goes.
  code <- c(
    "model {",
    "  // the likelihood",
    "  if (!prior_only) {",
    "    target += normal_lpdf(Y | mu, sigma);",
    "  }",
    "  // the accumulated priors",
    "  target += lprior;",
    "  // the scale's prior",
    "  target += exponential_lpdf(sigma | 1);",
    "  mu_raw ~ std_normal();",
    "  // an orphaned note",
    "",
    "}"
  )
  out <- mvgam:::reorganize_model_block_statements(code)
  above <- function(comment, statement) {
    grep(statement, out, fixed = TRUE) - 1L ==
      grep(comment, out, fixed = TRUE)
  }
  expect_true(above("// the likelihood", "if (!prior_only) {"))
  expect_true(above("// the accumulated priors", "target += lprior;"))
  expect_true(above("// the scale's prior",
                    "target += exponential_lpdf(sigma | 1);"))
  # The prior with no comment leads the block, and the orphan note goes.
  expect_identical(trimws(out[2L]), "mu_raw ~ std_normal();")
  expect_false(any(grepl("an orphaned note", out, fixed = TRUE)))
})

test_that("the lines inside a block comment are comment", {
  # The middle lines of a block comment spanning several lines are
  # comment. Stripped one line at a time, they pass for code.
  code <- c(
    "model {",
    "  /* a note",
    "     x = 1; on a middle line",
    "  */",
    "  target += lprior;",
    "}"
  )
  st <- mvgam:::stan_statements(code, list(start = 1L, end = 6L))
  expect_identical(st$head, c("", "", "", "target += lprior;"))
})

test_that("function documentation survives deduplication", {
  # Each function's comment travels with it, and a duplicate keeps the
  # first copy's.
  code <- paste(c(
    "functions {",
    "  // Doubles its argument.",
    "  real twice(real x) {",
    "    return 2 * x;",
    "  }",
    "  /**",
    "   * Halves its argument.",
    "   */",
    "  real half(real x) {",
    "    return x / 2;",
    "  }",
    "  // Doubles its argument.",
    "  real twice(real x) {",
    "    return 2 * x;",
    "  }",
    "}",
    "model {",
    "}"
  ), collapse = "\n")
  out <- strsplit(mvgam:::deduplicate_stan_functions(code), "\n")[[1L]]
  expect_identical(sum(grepl("real twice(real x)", out, fixed = TRUE)), 1L)
  expect_identical(
    grep("real twice(real x)", out, fixed = TRUE) - 1L,
    grep("// Doubles its argument.", out, fixed = TRUE)
  )
  expect_identical(
    grep("real half(real x)", out, fixed = TRUE) - 3L,
    grep("/**", out, fixed = TRUE)
  )
})

test_that("statements are split at their delimiters and headers", {
  code <- c(
    "model {",
    "  for (n in 1:N)",
    "    target += normal_lpdf(Y[n] | mu[n], sigma);",
    "  target += normal_lpdf(sigma | 0, 1)",
    "    - 1 * normal_lccdf(0 | 0, 1);",
    "  if (!prior_only) {",
    "    vector[N] mu = rep_vector(0.0, N);",
    "  }",
    "  print(\"{ in a string\"); // { in a comment",
    "}"
  )
  st <- mvgam:::stan_statements(code, list(start = 1L, end = length(code)))
  expect_identical(st$start, c(2L, 3L, 4L, 6L, 7L, 8L, 9L))
  expect_identical(st$end, c(2L, 3L, 5L, 6L, 7L, 8L, 9L))
  # A statement governed by a header without braces is nested, as is
  # everything inside a braced block up to its closing brace
  expect_identical(st$top, c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE))
})

test_that("a line comment goes and a string literal stays", {
  # Two families write `reject("...")` into their programs, and the
  # stripper that emits code has to leave the literal alone.
  f <- mvgam:::stan_drop_line_comment
  expect_identical(f('reject("bad // value"); // drop me'),
                   'reject("bad // value"); ')
  expect_identical(f("x = 1; // note"), "x = 1; ")
  expect_identical(f("y = 2;"), "y = 2;")
  expect_identical(f("// whole line"), "")
  # `stan_line_code()` gives a different result on the same line.
  # Analysis takes that helper, and emitted code takes the other.
  expect_identical(mvgam:::stan_line_code('reject("bad // value");'),
                   "reject();")
})

test_that("unwrapping the likelihood guard leaves other braces alone", {
  body <- c(
    "lprior += normal_lpdf(b | 0, 1);",
    "if (!prior_only) {",
    "  for (n in 1:N) {",
    "    target += poisson_log_lpmf(Y[n] | mu[n]);",
    "  }",
    "}"
  )
  # The guard's header and the brace matching it, and neither brace
  # belonging to the loop inside it
  expect_identical(mvgam:::prior_only_guard_indices(body), c(2L, 6L))
  expect_length(mvgam:::prior_only_guard_indices("x = 1;"), 0L)

  kept <- mvgam:::filter_block_content(paste(body, collapse = "\n"),
                                       "model")
  kept_lines <- strsplit(kept, "\n", fixed = TRUE)[[1]]
  expect_false(any(grepl("prior_only", kept_lines)))
  # The loop keeps the brace that closes it, and the block balances
  expect_true(any(grepl("^for \\(n in 1:N\\) \\{$", kept_lines)))
  expect_identical(
    sum(vapply(kept_lines, mvgam:::count_stan_braces, integer(1))), 0L
  )
})

test_that("trend renaming suffixes variables and leaves calls alone", {
  out <- mvgam:::rename_parameters_in_block(
    "vector[N] group = inv_logit(level * cumulative_sum(b));", "_trend",
    list(original_to_renamed = list(), renamed_to_original = list())
  )
  expect_identical(out$code, paste0(
    "vector[N_trend] group_trend = ",
    "inv_logit(level_trend * cumulative_sum(b_trend));"
  ))
})

test_that("a block ends at the brace its header opened", {
  code <- paste(c(
    "data {",
    "  int<lower=1> N;",
    "}",
    "",
    "// a note",
    "transformed data {",
    "  int M = N;",
    "}",
    "parameters {",
    "  real b;",
    "}"
  ), collapse = "\n")
  # A blank line and a comment separate the data block's closing brace
  # from the next header. The body stops at the brace.
  expect_identical(
    mvgam:::extract_stan_block_content(code, "data"),
    "  int<lower=1> N;"
  )
  expect_identical(
    mvgam:::extract_stan_block_content(code, "transformed data"),
    "  int M = N;"
  )
  # An absent block and an empty one are told apart
  expect_null(mvgam:::extract_stan_block_content(code, "model"))
  expect_identical(
    mvgam:::extract_stan_block_content("model {\n}", "model"), ""
  )
})


test_that("brms threading is withheld in the cases that break it", {
  trend <- list(has_trends = TRUE)
  none <- list(has_trends = FALSE)
  # Only a brms family under a trend warns. The closure-unit and
  # multi-response families parallelise with their own `reduce_sum`.
  expect_identical(
    c(mvgam:::brms_threading_case(2L, gaussian(), trend),
      mvgam:::brms_threading_case(2L, nmix("poisson_binomial"), trend),
      mvgam:::brms_threading_case(2L, occ(), none),
      mvgam:::brms_threading_case(2L, mvn(), none),
      mvgam:::brms_threading_case(2L, gaussian(), none),
      mvgam:::brms_threading_case(1L, nmix("poisson_binomial"), trend)),
    c("trend_native", "closure_unit", "closure_unit", "closure_unit",
      "none", "none")
  )
})


test_that("get_prior() surfaces mu_/sigma_ rows under coef_sharing = \"hierarchical\"", {
  dat <- ar_step3_fixture()
  gp <- get_prior(
    mvgam_formula(
      y ~ 1, trend_formula = ~ AR(p = 2, coef_sharing = "hierarchical")
    ),
    data = dat
  )
  classes <- gp$class
  expect_true("mu_ar1_pacf_trend" %in% classes)
  expect_true("mu_ar2_pacf_trend" %in% classes)
  expect_true("sigma_ar1_pacf_trend" %in% classes)
  expect_true("sigma_ar2_pacf_trend" %in% classes)
  # The per-series coefficients are drawn from the population
  # distribution those hyperparameters describe, so they are the
  # model's structure rather than a prior anyone can set. Offering a
  # row for them promised an override that silently did nothing.
  expect_false("ar1_trend" %in% classes)
  expect_false("ar2_trend" %in% classes)
})


# ============================================================
# brms helpers called from the mvgam namespace
# ============================================================
# `make_stancode()` and `make_standata()` call the `stancode()` and
# `standata()` generics, which dispatch to the mvgam methods.

test_that("brms helper generics are exported by mvgam", {
  ns_exports <- getNamespaceExports("mvgam")
  for (name in c("stancode", "standata", "make_stancode",
                 "make_standata", "default_prior", "get_prior",
                 "prior_summary")) {
    expect_true(
      name %in% ns_exports,
      label = paste0("mvgam exports `", name, "`")
    )
  }
})

test_that("standata.mvgam and default_prior.mvgam read from the fitted slot", {
  # Stub a fitted-mvgam shape; the methods only need
  # `$standata` / `$prior` and the class tag.
  stub <- structure(
    list(
      standata = list(N = 10L, Y = rnorm(10L)),
      prior = structure(
        data.frame(prior = "normal(0, 1)", class = "Intercept",
                   coef = "", group = "", resp = "", dpar = "",
                   nlpar = "", lb = NA_character_, ub = NA_character_,
                   source = "user", stringsAsFactors = FALSE),
        class = c("brmsprior", "data.frame")
      )
    ),
    class = "mvgam"
  )
  sd <- standata(stub)
  expect_identical(sd, stub$standata)
  dp <- default_prior(stub)
  expect_identical(dp, stub$prior)
})


test_that("control_params.mvgam / inits.mvgam read from the stanfit slot", {
  # Stub a minimal mvgam-shaped object with the @stan_args slot
  # populated. Avoids the cost of a real fit while still
  # exercising the dispatch and extraction.
  stub_stan_args <- list(
    list(control = list(adapt_delta = 0.95, max_treedepth = 12),
         init = "random"),
    list(control = list(adapt_delta = 0.95, max_treedepth = 12),
         init = "random")
  )
  stub_stanfit <- new(
    "stanfit",
    stan_args = stub_stan_args
  )
  stub <- structure(
    list(fit = stub_stanfit),
    class = "mvgam"
  )
  cp <- control_params(stub)
  expect_identical(cp$adapt_delta, 0.95)
  expect_identical(cp$max_treedepth, 12)
  ii <- inits(stub)
  expect_length(ii, 2L)
  expect_identical(ii[[1L]], "random")
  expect_identical(ii[[2L]], "random")
})

test_that("mvgam() rejects an invalid threads argument before compile", {
  # validate_threads errors on anything that is not NULL, numeric,
  # or a `brms::threading()` object. The outer call runs the check
  # before any compile so the error path is fast.
  expect_error(
    mvgam(y ~ 1, data = data.frame(y = 1:5), threads = "many"),
    "threads"
  )
})


test_that("com_binomial() refuses non-logit links", {
  expect_error(com_binomial(link = "probit"), "{'logit'}", fixed = TRUE)
})


# ---- time as a covariate inside trend_formula ----------------------

test_that("trend_covariate_names() drops the grouping columns", {
  # `time` and `series` are added by the caller and grouped on before
  # the summarise, and dplyr omits grouping columns from `across()`.
  # Naming them in the selection fails with "Element `time` doesn't
  # exist", which is what broke every trend_formula referring to time.
  expect_identical(mvgam:::trend_covariate_names(c("time", "x")), "x")
  expect_identical(mvgam:::trend_covariate_names(c("series", "x")), "x")
  expect_identical(mvgam:::trend_covariate_names(c("time", "series")),
                   character(0))
  expect_identical(mvgam:::trend_covariate_names(c("x", "z")), c("x", "z"))
  expect_identical(mvgam:::trend_covariate_names(character(0)), character(0))
  expect_error(mvgam:::trend_covariate_names(1:3), "character")
  expect_error(mvgam:::trend_covariate_names(c("x", NA)), "missing")
})

test_that("mvgam_codegen_options() refuses malformed settings", {
  expect_error(mvgam:::mvgam_codegen_options(knots = list(1, 2)),
               "names")
  expect_error(mvgam:::mvgam_codegen_options(sample_prior = "maybe"),
               "sample_prior")
  expect_error(mvgam:::mvgam_codegen_options(normalize = NA),
               "normalize")
})

test_that("codegen_args_for() hands each generator what it declares", {
  opts <- mvgam:::mvgam_codegen_options(normalize = FALSE)
  expect_named(
    mvgam:::codegen_args_for(opts, mvgam:::mvgam_codegen_stancode_options),
    c("knots", "sample_prior", "drop_unused_levels", "normalize")
  )
  # `brms::standata()` declares no `normalize`, so it never sees one.
  expect_named(
    mvgam:::codegen_args_for(opts, mvgam:::mvgam_codegen_standata_options),
    c("knots", "sample_prior", "drop_unused_levels")
  )
  expect_identical(
    mvgam:::codegen_args_for(NULL, mvgam:::mvgam_codegen_stancode_options),
    list()
  )
})

test_that("the reserved-word list carries both density spellings", {
  reserved <- mvgam:::get_stan_reserved_words()
  normalised <- grep("_lp[dm]f$", reserved, value = TRUE)
  expect_true(length(normalised) > 0)
  expect_true(all(
    sub("_lp([dm])f$", "_lup\\1f", normalised) %in% reserved
  ))
})

test_that("brms-deprecated arguments are named rather than dropped", {
  dat <- codegen_test_data()
  mf <- mvgam_formula(y ~ elev, trend_formula = ~ AR(p = 1))

  # brms moved both of these off its own top level, so mvgam points at
  # where brms now reads them rather than resurrecting the argument.
  expect_error(
    stancode(mf, data = dat, family = poisson(), sparse = TRUE,
             validate = FALSE),
    "sparse = TRUE"
  )
  expect_error(
    standata(mf, data = dat, family = poisson(),
             stan_funs = "real f(real x) { return x; }"),
    "stanvars"
  )
})

test_that("a fit records the options its program was generated under", {
  # `update()` reads all four off one slot, so a fit made before the
  # slot existed inherits nothing and falls back to the defaults.
  opts <- names(mvgam:::mvgam_codegen_options())
  expect_true(all(opts %in% names(mvgam:::update_inheritance_table())))
  expect_false(any(opts %in% names(mvgam:::mvgam_update_uninherited)))

  stored <- list(codegen = mvgam:::mvgam_codegen_options(
    knots = list(elev = 1:5), normalize = FALSE
  ))
  getters <- mvgam:::update_inheritance_table()
  expect_identical(getters$knots$getter(stored), list(elev = 1:5))
  expect_false(getters$normalize$getter(stored))
  expect_null(getters$knots$getter(list()))
})

test_that("the generator option sets stay pinned to the options list", {
  # A fifth option added to `mvgam_codegen_options()` and forgotten in
  # either set would be dropped on its way to brms without a word, so
  # both sets are derived rather than written out again.
  expect_identical(
    mvgam:::mvgam_codegen_stancode_options,
    names(mvgam:::mvgam_codegen_options())
  )
  expect_identical(
    mvgam:::mvgam_codegen_standata_options,
    setdiff(names(mvgam:::mvgam_codegen_options()), "normalize")
  )
})

test_that("codegen_from_dots reads the options a call carries", {
  opts <- mvgam:::codegen_from_dots(
    list(knots = list(x = 1:5), normalize = FALSE, chains = 4, iter = 100)
  )
  expect_identical(opts$knots, list(x = 1:5))
  expect_false(opts$normalize)
  # Sampler arguments are not code-generation options.
  expect_identical(names(opts), names(mvgam:::mvgam_codegen_options()))
})

test_that("the two code-generation entry points agree", {
  # `stancode()` and `standata()` reach the same generator, so an
  # argument on one and not the other means the pair describes
  # different models. `standata()` accepted five of these through its
  # dots while advertising none of them, and restated defaults drift
  # apart silently because nothing compares them.
  # `validate` parses the program, which `standata()` does not return.
  sc <- formals(mvgam:::stancode.mvgam_formula)
  sd <- formals(mvgam:::standata.mvgam_formula)
  expect_setequal(setdiff(names(sc), "validate"), names(sd))
  expect_error(
    standata(mvgam_formula(yg ~ x, trend_formula = ~ AR()),
             data = injection_frame(), validate = TRUE),
    "'validate' is not supported by standata()", fixed = TRUE
  )

  shared <- intersect(names(sc), names(sd))
  disagreeing <- shared[!vapply(
    shared, function(n) identical(sc[[n]], sd[[n]]), logical(1L)
  )]
  expect_identical(disagreeing, character(0))

  # Every argument they declare has to be one the generator takes, or
  # it is quietly discarded on the way down.
  generator <- names(formals(mvgam:::build_stan_components))
  expect_true(all(setdiff(names(sc), c("object", "...")) %in% generator))
})


test_that("the observed history counts every response or refuses", {
  # PW spreads its changepoints over the occasions a response was seen
  # at. A response the frame lacked dropped out of the count, and the
  # history came out shorter than the model's.
  d <- data.frame(time = 1:6, y1 = c(1, 2, NA, NA, NA, NA),
                  y2 = c(1, 2, 3, 4, NA, NA))
  expect_identical(count_observed_times(d, "time", c("y1", "y2")), 4L)
  expect_error(count_observed_times(d[, c("time", "y1")], "time",
                                    c("y1", "y2")),
               "'y2'")
})
