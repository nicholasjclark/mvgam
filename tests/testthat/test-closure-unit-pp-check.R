# Tests for the closure-unit pp_check helpers. Structural coverage
# of the `closure_unit_pp_check_setup()` aggregation hook and
# the `refuse_unit_varying()` group / x validator;
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

test_that(
  "closure_unit_pp_check_setup() collapses non-resid yrep to unit grain", {
  d <- make_pp_check_grid(n_unit = 4L, n_visit = 3L)
  obj <- mock_pp_check_object(d)
  yrep <- matrix(
    sample(c(0L, 1L), 5L * nrow(d), replace = TRUE),
    nrow = 5L, ncol = nrow(d)
  )
  res <- closure_unit_pp_check_setup(
    obj, newdata = d, yrep = yrep, type = "bars"
  )
  expect_identical(res$arrays$N_unit, 4L)
  # Each unit's observed and replicated values are the sums of its own
  # visits. `visit_row` numbers the rows of the frame, the axis of both
  # `d$y` and the columns of `yrep`.
  for (g in seq_len(res$arrays$N_unit)) {
    idx <- res$arrays$visit_row[g, seq_len(res$arrays$n_rep[g])]
    expect_equal(unname(res$y[g]), sum(d$y[idx]))
    expect_equal(unname(res$yrep[, g]),
                 rowSums(yrep[, idx, drop = FALSE]))
  }
  # Each unit occupies three consecutive rows of the grid. Its first
  # visit is its first row.
  expect_identical(res$first_visits, c(1L, 4L, 7L, 10L))
})

test_that(
  "closure_unit_pp_check_setup() keeps the unit yrep grain on resid_*", {
  d <- make_pp_check_grid()
  obj <- mock_pp_check_object(d)
  # residuals.mvgam returns [ndraws x N_unit]; mimic that shape.
  per_unit_resid <- matrix(rnorm(7L * 4L), nrow = 7L, ncol = 4L)
  res <- closure_unit_pp_check_setup(
    obj, newdata = d,
    yrep = per_unit_resid, type = "resid_hist"
  )
  # The per-unit residuals pass through untouched
  expect_identical(res$yrep, per_unit_resid)
  # y is zeros at unit length for resid_* (bayesplot's
  # ppc_error_hist receives y = 0 + (-1)*resid).
  expect_identical(res$y, rep(0, 4L))
})

# ------------------------------------------------------------
# Closure-unit pp_check type validation: per-row scatter and
# fitted-vs-residual types are blocked because they do not match
# the closure-unit grain.
# ------------------------------------------------------------

test_that(
  "pp_check.mvgam blocks closure-unit-incompatible types with a pointer", {
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
      "not supported for\\s+closure-unit"
    )
  }
})

# ------------------------------------------------------------
# Unit-constant validator: per-visit covariates passed to
# `group =` or `x =` must be constant within every closure
# unit. The fixture `tod` varies within a unit; `elev` does not.
# ------------------------------------------------------------

test_that("refuse_unit_varying() refuses only a per-visit covariate", {
  d <- make_pp_check_grid()
  arrays <- build_closure_unit_arrays(
    d, response_var = "y", default_cap = 1L
  )
  expect_error(
    refuse_unit_varying(d$tod, arrays, "tod", PP_CHECK_UNIT_REASON),
    "'tod' varies within a closure unit"
  )
  expect_silent(
    refuse_unit_varying(d$elev, arrays, "elev", PP_CHECK_UNIT_REASON)
  )
})

# ------------------------------------------------------------
# pp_check(type = "fit_stat"): chi-squared / Freeman-Tukey
# discrepancy GOF (Gelman et al. 1996). The discrepancy compares a
# unit's aggregated visits against their aggregated expectation, so
# it is offered to the families that aggregate rather than to every
# family sharing the closure-unit pipeline. The math is small and
# tested directly: chi-squared at y == E[y] is zero, and the F-T
# form is scale-equivariant under shared sqrt.
# ------------------------------------------------------------

test_that("pp_check(type = 'fit_stat') is refused where no unit aggregates", {
  stub <- function(fam) {
    d <- data.frame(y = rnorm(10L), x = rnorm(10L),
                    series = factor(rep(1L, 10L)),
                    time = seq_len(10L))
    structure(list(data = d, formula = y ~ x, family = fam),
              class = "mvgam")
  }
  expect_error(
    pp_check(stub(gaussian()), type = "fit_stat"),
    "applies to the detection"
  )
  # These five share the closure-unit layout but never sum the rows of
  # a unit. A composition's rows sum to one whatever the fit says. The
  # discrepancy then has nothing to test.
  for (fam in list(diri(), multi(), categ(), mvn(), mvt())) {
    expect_error(
      pp_check(stub(fam), type = "fit_stat"),
      "one observation per row"
    )
  }
})

test_that("mvgam_ppc_fit_stat print names the direction of a far p-value", {
  fit_stat <- function(p) {
    structure(
      list(stat = "chi_squared", group = NULL, grain = "closure unit",
           T_obs = 1, T_rep = 1, bayes_p = p, n_draws = 50L,
           family = "occ"),
      class = c("mvgam_ppc_fit_stat", "list")
    )
  }
  # Replicates that rarely reach the data suggest lack of fit. Data
  # closer to the expectation than replicates follow from flexible terms.
  expect_output(print(fit_stat(0.01)), "may not fit")
  expect_output(print(fit_stat(0.99)), "deviate less")
  expect_false(any(grepl("Note", capture.output(print(fit_stat(0.5))))))
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

# A composition's categories are its species and a multivariate
# normal's responses are its species, so both read the same axis and
# are held to the same claim here.
for (fam_name in c("mvgam_dirichlet", "mvgam_mvnormal")) {
  test_that(paste0("pp_check_mv_category() names the species for ",
                   fam_name), {
    obj <- structure(
      list(family = make_mv_family_stub(
             fam_name,
             simplex = identical(fam_name, "mvgam_dirichlet")
           ),
           formula = y ~ 1,
           data = make_simplex_long()),
      class = "mvgam"
    )
    cats <- mvgam:::pp_check_mv_category(obj, obj$data)
    expect_s3_class(cats, "factor")
    expect_identical(levels(cats), paste0("y", 1:3))
    expect_identical(as.character(cats),
                     as.character(obj$data$series))

    # A site that lost a species must not relabel the ones after it.
    # Numbering each unit's rows 1..K did: with the second site's
    # `y2` gone, its `y3` sat in position two and was drawn in the
    # panel `y2`'s observations belong to.
    gapped <- obj$data[-5L, , drop = FALSE]
    expect_identical(
      as.character(mvgam:::pp_check_mv_category(obj, gapped)),
      as.character(gapped$series)
    )
  })
}

test_that(
  "pp_check_mv_category() returns NULL for non-multi-response families", {
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

test_that(
  "complete_closure_unit_newdata stamps nmix('royle_nichols') cap = 25", {
  stub <- stub_with_family(nmix("royle_nichols"))
  out  <- mvgam:::complete_closure_unit_newdata(
    stub, make_stripped_grid(3L)
  )
  expect_identical(out$cap, rep(25L, 3L))
})

test_that(
  "complete_closure_unit_newdata stamps nmix('poisson_poisson') cap", {
  stub <- stub_with_family(nmix("poisson_poisson"))
  out  <- mvgam:::complete_closure_unit_newdata(
    stub, make_stripped_grid(3L)
  )
  expect_identical(out$cap, rep(7L, 3L))
})

test_that(
  "complete_closure_unit_newdata returns other families' newdata as is", {
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

test_that(
  "complete_closure_unit_newdata returns a complete frame as is", {
  stub <- stub_with_family(occ())
  full <- stub$data[1:3, , drop = FALSE]
  out  <- mvgam:::complete_closure_unit_newdata(stub, full)
  # Nothing changed (no rowid, no synthetic time stamp).
  expect_identical(out, full)
})

test_that(
  "complete_closure_unit_newdata fills only missing identifier cols", {
  stub <- stub_with_family(occ())
  partial <- data.frame(
    series = factor("1", levels = c("1", "2")),
    env    = 0,
    y      = 99L
  )
  partial <- partial[rep(1L, 3L), , drop = FALSE]
  rownames(partial) <- NULL
  out <- mvgam:::complete_closure_unit_newdata(stub, partial)
  # The supplied series stays as given. The response is replaced.
  expect_identical(as.character(out$series), rep("1", 3L))
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

test_that(
  "complete_closure_unit_newdata respects user long-format newdata", {
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

test_that("pp_check() refuses the covariate part for loo_* types", {
  obj <- mock_pp_check_object(make_pp_check_grid(), family = gaussian())
  expect_error(
    pp_check(obj, type = "loo_pit_overlay", incl_autocor = FALSE),
    "not supported for the 'loo_\\*' types"
  )
})
