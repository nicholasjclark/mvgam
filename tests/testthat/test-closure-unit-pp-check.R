# Tests for the closure-unit pp_check helpers. Structural coverage
# of the new `closure_unit_pp_check_setup()` aggregation hook and
# the `check_closure_unit_var_unit_constant()` group / x validator;
# end-to-end MCMC integration on the cached occ fixture lives in
# tests/local/test-closure-unit-pp-check.R.

# Helpers shared with the residuals tests.
make_pp_check_grid <- function(n_unit = 4L, n_visit = 3L,
                                seed = 17L) {
  set.seed(seed)
  n_total <- n_unit * n_visit
  data.frame(
    series = factor(rep(seq_len(n_unit), each = n_visit)),
    time   = rep(1L, n_total),
    visit  = rep(seq_len(n_visit), n_unit),
    y      = sample(c(0L, 1L), n_total, replace = TRUE),
    elev   = rep(rnorm(n_unit), each = n_visit),
    tod    = stats::runif(n_total)
  )
}

mock_pp_check_object <- function(data, family = occ()) {
  structure(
    list(data = data, formula = y ~ 1, family = family),
    class = "mvgam"
  )
}

# ------------------------------------------------------------
# closure_unit_pp_check_setup(): aggregates per-visit y / yrep to
# the per-unit grain. resid_* types arrive with yrep already at
# the unit grain from residuals.mvgam.
# ------------------------------------------------------------

test_that("closure_unit_pp_check_setup() collapses non-resid yrep to the per-unit grain", {
  d <- make_pp_check_grid(n_unit = 4L, n_visit = 3L)
  obj <- mock_pp_check_object(d)
  yrep <- matrix(
    sample(c(0L, 1L), 5L * nrow(d), replace = TRUE),
    nrow = 5L, ncol = nrow(d)
  )
  res <- closure_unit_pp_check_setup(
    obj, newdata = d,
    y = as.numeric(d$y), yrep = yrep, type = "bars"
  )
  expect_identical(length(res$y), 4L)
  expect_identical(dim(res$yrep), c(5L, 4L))
  # Each y_unit must equal sum of the visits in that unit.
  for (g in seq_len(res$arrays$N_unit)) {
    idx <- res$arrays$visit_idx[g, seq_len(res$arrays$n_rep[g])]
    expect_equal(unname(res$y[g]), sum(d$y[idx]))
  }
  # First-visits lookup carries one obs index per unit.
  expect_identical(length(res$first_visits), 4L)
})

test_that("closure_unit_pp_check_setup() preserves the per-unit yrep grain on resid_* types", {
  d <- make_pp_check_grid()
  obj <- mock_pp_check_object(d)
  # residuals.mvgam returns [ndraws x N_unit]; mimic that shape.
  per_unit_resid <- matrix(rnorm(7L * 4L), nrow = 7L, ncol = 4L)
  res <- closure_unit_pp_check_setup(
    obj, newdata = d, y = as.numeric(d$y),
    yrep = per_unit_resid, type = "resid_hist"
  )
  expect_identical(dim(res$yrep), c(7L, 4L))
  # y is zeros at unit length for resid_* (bayesplot's
  # ppc_error_hist receives y = 0 + (-1)*resid).
  expect_identical(res$y, rep(0, 4L))
})

# ------------------------------------------------------------
# Closure-unit pp_check type validation: per-row scatter and
# fitted-vs-residual types are blocked because they do not match
# the closure-unit grain.
# ------------------------------------------------------------

test_that("pp_check.mvgam blocks closure-unit-incompatible types early with a clear pointer", {
  d <- make_pp_check_grid()
  obj <- mock_pp_check_object(d)
  blocked <- c("scatter_avg", "scatter_avg_grouped",
               "error_scatter_avg", "error_scatter_avg_vs_x",
               "error_binned",
               "resid_acf", "resid_pacf", "resid_vs_fitted",
               "resid_ribbon", "resid_ribbon_grouped")
  for (t in blocked) {
    expect_error(
      pp_check(obj, type = t),
      "not available for"
    )
  }
})

# ------------------------------------------------------------
# Unit-constant validator: per-visit covariates passed to
# `group =` or `x =` must be constant within every closure
# unit. The fixture `tod` varies within a unit; `elev` does not.
# ------------------------------------------------------------

test_that("check_closure_unit_var_unit_constant() rejects per-visit-varying covariates", {
  d <- make_pp_check_grid()
  arrays <- build_closure_unit_arrays(
    d, response_var = "y", default_cap = 1L
  )
  expect_error(
    check_closure_unit_var_unit_constant(d$tod, arrays, "tod"),
    "not constant within every closure unit"
  )
})

test_that("check_closure_unit_var_unit_constant() accepts unit-constant covariates", {
  d <- make_pp_check_grid()
  arrays <- build_closure_unit_arrays(
    d, response_var = "y", default_cap = 1L
  )
  expect_silent(
    check_closure_unit_var_unit_constant(d$elev, arrays, "elev")
  )
})

# ------------------------------------------------------------
# pp_check(type = "fit_stat"): chi-squared / Freeman-Tukey
# discrepancy GOF (Gelman et al. 1996). The dispatch is gated on
# closure-unit families; non-closure-unit fits get a typed error
# pointing to occ() / nmix(). The math itself is small and tested
# directly: chi-squared at y == E[y] is zero, and the F-T form is
# scale-equivariant under shared sqrt.
# ------------------------------------------------------------

test_that("pp_check(type = 'fit_stat') errors on non-closure-unit family", {
  # gaussian mvgam stub: family is gaussian(), not closure-unit
  d <- data.frame(y = rnorm(10L), x = rnorm(10L),
                  series = factor(rep(1L, 10L)),
                  time = seq_len(10L))
  obj <- structure(
    list(data = d, formula = y ~ x, family = gaussian()),
    class = "mvgam"
  )
  expect_error(
    pp_check(obj, type = "fit_stat"),
    "only available for closure-unit families"
  )
})

test_that("mvgam_ppc_fit_stat print + plot methods produce expected output", {
  # Hand-construct a fit_stat result with a known Bayesian p-value
  # so the formatting is testable without a real Stan fit.
  set.seed(7L)
  T_obs <- rnorm(50L, mean = 100, sd = 5)
  T_rep <- rnorm(50L, mean = 100, sd = 5)
  obj <- structure(
    list(
      stat = "chi_squared", group = NULL, grain = "closure unit",
      T_obs = T_obs, T_rep = T_rep,
      bayes_p = mean(T_rep >= T_obs),
      n_draws = 50L, family = "occ"
    ),
    class = c("mvgam_ppc_fit_stat", "list")
  )
  out <- capture.output(print(obj))
  expect_true(any(grepl("chi-squared", out)))
  expect_true(any(grepl("Bayesian p-value", out)))
  expect_true(any(grepl("closure unit", out)))
  p <- plot(obj)
  expect_s3_class(p, "ggplot")
})


# ------------------------------------------------------------
# pp_check_mv_category(): auto-grouping helper for multi-response
# custom families (diri / multi / categ via simplex layout, mvn /
# mvt via series). Single helper, two branches, exercised here
# without needing a real Stan fit.
# ------------------------------------------------------------

# Stub multi-response family without the full custom_family
# infrastructure. The family-kind predicates read these attrs
# directly so a structure() + class is all the helper needs to
# pick the right branch.
make_mv_family_stub <- function(family_name,
                                  multi = TRUE,
                                  simplex = TRUE) {
  structure(
    list(family = family_name, name = sub("^mvgam_", "", family_name)),
    class = "customfamily",
    mvgam_multi_response = multi,
    mvgam_simplex_response = simplex,
    mvgam_closure_unit = TRUE,
    mvgam_unit_grouping = c("time")
  )
}

# Long-form simplex data: 2 sites x 3 categories. Each (site, cat)
# row carries one component of a probability vector summing to 1
# within a site. build_closure_unit_arrays() with unit_grouping =
# "time" treats each site as one closure unit.
make_simplex_long <- function() {
  data.frame(
    series = factor(rep(paste0("y", 1:3), 2L),
                    levels = paste0("y", 1:3)),
    time   = rep(c(1L, 2L), each = 3L),
    y      = c(0.2, 0.5, 0.3, 0.4, 0.4, 0.2)
  )
}

test_that("pp_check_mv_category() maps simplex rows to within-unit position", {
  fam <- make_mv_family_stub("mvgam_dirichlet")
  obj <- structure(
    list(family = fam,
         formula = y ~ 1,
         data = make_simplex_long()),
    class = "mvgam"
  )
  cats <- mvgam:::pp_check_mv_category(obj, obj$data)
  expect_s3_class(cats, "factor")
  expect_identical(levels(cats),
                   c("cat_1", "cat_2", "cat_3"))
  # Each site fills positions 1..3 in row order, so the vector is
  # rep(1:3, 2) wrapped as cat_<int>.
  expect_identical(as.integer(cats),
                   rep(1:3, times = 2L))
})

test_that("pp_check_mv_category() uses series column for mvn / mvt", {
  fam <- make_mv_family_stub("mvgam_mvnormal", simplex = FALSE)
  obj <- structure(
    list(family = fam,
         formula = y ~ 1,
         data = data.frame(
           series = factor(c("a", "b", "a", "b")),
           y      = rnorm(4L)
         )),
    class = "mvgam"
  )
  cats <- mvgam:::pp_check_mv_category(obj, obj$data)
  expect_s3_class(cats, "factor")
  expect_identical(levels(cats), c("a", "b"))
  expect_identical(as.character(cats), c("a", "b", "a", "b"))
})

test_that("pp_check_mv_category() returns NULL for non-multi-response families", {
  obj <- structure(
    list(family = gaussian(),
         formula = y ~ 1,
         data = data.frame(series = factor(letters[1:3]),
                            y = rnorm(3L))),
    class = "mvgam"
  )
  expect_null(mvgam:::pp_check_mv_category(obj, obj$data))
})


# ---------------------------------------------------------------
# complete_closure_unit_newdata(): auto-fill closure-unit cols
# missing from a marginaleffects-style synthetic grid. Family-
# by-family coverage so default_cap / default_cap_buffer paths
# all stamp the right cap, and the response gets overridden.
# ---------------------------------------------------------------

stub_with_family <- function(fam, response_var = "y") {
  formula <- stats::as.formula(paste(response_var, "~ env"))
  structure(
    list(
      family  = fam,
      formula = formula,
      data    = data.frame(
        series = factor(rep(1:2, each = 3L)),
        time   = rep(1:3, 2L),
        visit  = rep(1:3, 2L),
        y      = c(0L, 1L, 0L, 1L, 1L, 0L),
        cap    = rep(7L, 6L),
        env    = rnorm(6L)
      )
    ),
    class = "mvgam"
  )
}

# Bare datagrid-shaped newdata: formula vars only, identifier
# columns stripped. Mirrors what marginaleffects::datagrid()
# hands to get_predict.mvgam.
make_stripped_grid <- function(n = 5L) {
  data.frame(
    rowid = seq_len(n),
    env   = seq(-2, 2, length.out = n)
  )
}

test_that("complete_closure_unit_newdata stamps occ defaults", {
  stub <- stub_with_family(occ())
  out  <- mvgam:::complete_closure_unit_newdata(
    stub, make_stripped_grid(5L)
  )
  expect_true(all(c("series", "time", "visit", "cap", "y") %in%
                    names(out)))
  expect_identical(out$cap, rep(1L, 5L))
  expect_identical(out$visit, rep(1L, 5L))
  expect_identical(out$time, 1:5)
  expect_identical(as.character(out$series), rep("1", 5L))
  # Response overridden to satisfy binary_y_check.
  expect_identical(out$y, rep(0L, 5L))
})

test_that("complete_closure_unit_newdata stamps nmix(PB) cap from template", {
  stub <- stub_with_family(nmix())
  out  <- mvgam:::complete_closure_unit_newdata(
    stub, make_stripped_grid(4L)
  )
  expect_identical(out$cap, rep(7L, 4L))
})

test_that("complete_closure_unit_newdata stamps nmix('royle_nichols') cap = 25", {
  stub <- stub_with_family(nmix("royle_nichols"))
  out  <- mvgam:::complete_closure_unit_newdata(
    stub, make_stripped_grid(3L)
  )
  expect_identical(out$cap, rep(25L, 3L))
})

test_that("complete_closure_unit_newdata stamps nmix('poisson_poisson') cap from template", {
  stub <- stub_with_family(nmix("poisson_poisson"))
  out  <- mvgam:::complete_closure_unit_newdata(
    stub, make_stripped_grid(3L)
  )
  expect_identical(out$cap, rep(7L, 3L))
})

test_that("complete_closure_unit_newdata is a no-op for non-closure-unit families", {
  stub <- structure(
    list(family = gaussian(),
         formula = y ~ env,
         data = data.frame(y = rnorm(5L), env = rnorm(5L))),
    class = "mvgam"
  )
  grid <- make_stripped_grid(5L)
  out  <- mvgam:::complete_closure_unit_newdata(stub, grid)
  expect_identical(out, grid)
})

test_that("complete_closure_unit_newdata is a no-op when columns already present", {
  stub <- stub_with_family(occ())
  full <- stub$data[1:3, , drop = FALSE]
  out  <- mvgam:::complete_closure_unit_newdata(stub, full)
  # Nothing changed (no rowid, no synthetic time stamp).
  expect_identical(out, full)
})

test_that("complete_closure_unit_newdata fills only the missing identifier cols", {
  stub <- stub_with_family(occ())
  partial <- data.frame(
    series = factor("1", levels = c("1", "2")),
    env    = 0,
    y      = 99L
  )
  partial <- partial[rep(1L, 3L), , drop = FALSE]
  rownames(partial) <- NULL
  out <- mvgam:::complete_closure_unit_newdata(stub, partial)
  # series was present; left alone (still all "1").
  expect_identical(as.character(out$series), rep("1", 3L))
  # time / visit / cap stamped.
  expect_identical(out$time, 1:3)
  expect_identical(out$visit, rep(1L, 3L))
  expect_identical(out$cap, rep(1L, 3L))
  # response always overridden to 0L.
  expect_identical(out$y, rep(0L, 3L))
})

test_that("complete_closure_unit_newdata handles multi-species fits", {
  stub <- stub_with_family(occ())
  # Original series factor has levels c("1", "2"); helper picks
  # the FIRST level as the default fill.
  out  <- mvgam:::complete_closure_unit_newdata(
    stub, make_stripped_grid(3L)
  )
  expect_identical(levels(out$series), levels(stub$data$series))
  expect_identical(as.character(out$series), rep("1", 3L))
})

test_that("complete_closure_unit_newdata overrides datagrid-pinned time", {
  # marginaleffects::datagrid() pins `time` at a single typical
  # value drawn from the training data. On the grid path (visit /
  # cap missing) the helper must overwrite that pinned `time` so
  # each row becomes its own closure unit; otherwise psi collapses
  # to a constant across the prediction grid and conditional_effects
  # / plot_predictions return a flat curve.
  stub <- stub_with_family(occ())
  grid <- data.frame(
    rowid = 1:5,
    env   = seq(-2, 2, length.out = 5L),
    time  = 16L  # pinned by datagrid -- must be overwritten
  )
  out <- mvgam:::complete_closure_unit_newdata(stub, grid)
  expect_identical(out$time, 1:5)
  expect_identical(out$visit, rep(1L, 5L))
})

test_that("complete_closure_unit_newdata respects user-supplied long-format newdata", {
  # The complement to the grid-path overwrite: when the user passes
  # real long-format data (visit + cap present), the helper must
  # leave (series, time, visit, cap) untouched. Forecasting and
  # multi-season fits encode meaningful time labels that we cannot
  # silently rewrite.
  stub <- stub_with_family(occ())
  real <- data.frame(
    series = factor(rep(1:2, each = 3L), levels = c("1", "2")),
    time   = c(7L, 7L, 7L, 8L, 8L, 8L),  # non-1:n on purpose
    visit  = rep(1:3, 2L),
    y      = c(1L, 0L, 1L, 0L, 1L, 0L),
    cap    = rep(3L, 6L),
    env    = rnorm(6L)
  )
  out <- mvgam:::complete_closure_unit_newdata(stub, real)
  expect_identical(out$time, real$time)
  expect_identical(out$visit, real$visit)
  expect_identical(out$cap, real$cap)
})
