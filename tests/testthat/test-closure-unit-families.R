# Tests for the closure-unit family infrastructure and all
# closure-unit family constructors: `nmix()` (Poisson-Binomial,
# Royle-Nichols, Poisson-Poisson variants), `occ()`, and the
# multi-response families `diri()`, `multi()`, `categ()`, `mvn()`
# and `mvt()`.
# Stan emission contracts assert on the source-helper strings
# directly.

# ------------------------------------------------------------
# nmix() constructor
# ------------------------------------------------------------

test_that("nmix() takes a log abundance and a logit detection", {
  fam <- nmix()
  expect_identical(fam$dpars, c("mu", "p"))
  expect_identical(fam$link, "log")
  expect_identical(fam$link_p, "logit")
  expect_identical(fam$type, "int")
  expect_false(fam$loop)
  # The bounds on p keep a scalar detection in (0, 1), where the
  # lpmf's logit(p) is defined
  expect_identical(fam$lb, list(mu = "0", p = "0"))
  expect_identical(fam$ub, list(mu = NA_character_, p = "1"))
  # Response-scale helpers for the prediction dispatchers
  expect_equal(fam$linkinv(0), 1)
  expect_equal(fam$linkfun(1), 0)
  expect_true(uses_closure_unit_layout(fam))
})

test_that("uses_closure_unit_layout() returns FALSE for non-closure families", {
  expect_false(uses_closure_unit_layout(NULL))
  expect_false(uses_closure_unit_layout(gaussian()))
  expect_false(uses_closure_unit_layout(brms::brmsfamily("poisson")))
  expect_false(uses_closure_unit_layout(tweedie()))
})

# ------------------------------------------------------------
# Multi-response family predicates + gate
# ------------------------------------------------------------

test_that("the multi-response predicates are FALSE for other families", {
  for (fam in list(NULL, gaussian(), nmix(), tweedie())) {
    expect_false(is_multi_response_family(fam))
    expect_false(is_simplex_response_family(fam))
  }
})

test_that("validate_supported_family() admits every supported family", {
  admitted <- list(
    nmix(), nmix("royle_nichols"), nmix("poisson_poisson"), occ(),
    diri(), multi(), categ(), mvn(), mvt(), tweedie()
  )
  for (fam in admitted) {
    expect_invisible(validate_supported_family(fam))
  }
})

test_that("diri_stan_funs() emits the lpdf body with mu_unit[1] anchor", {
  sc <- mvgam:::diri_stan_funs()
  expect_match(sc, "real diri_lpdf(", fixed = TRUE)
  # Native Stan helpers: dirichlet_lpdf + softmax. The brms-emitted
  # `dirichlet_logit_lpdf` helper is NOT a Stan built-in.
  expect_match(
    sc, "dirichlet_lpdf(y_unit | softmax(mu_unit) * phi_g)",
    fixed = TRUE
  )
  # Subtracting `mu_unit[1]` before the softmax leaves the likelihood
  # unchanged and removes the per-site shift shared by all K entries.
  expect_match(sc, "vector[Kg] mu_unit = mu[idx] - mu[idx[1]];",
               fixed = TRUE)
  # Two-signature dispatch: vector-phi primary (emits the
  # `phi_g = phi[idx[1]]` per-unit collapse) and scalar-phi
  # broadcast that forwards via `rep_vector(phi, N)`.
  expect_match(sc, "real phi_g = phi[idx[1]];", fixed = TRUE)
  expect_match(
    sc, "diri_lpdf(y | mu, rep_vector(phi, N), N_unit",
    fixed = TRUE
  )
})

test_that("multi_stan_funs() emits the lpmf body with mu_unit[1] anchor", {
  sc <- mvgam:::multi_stan_funs()
  expect_match(sc, "real multi_lpmf(", fixed = TRUE)
  expect_match(sc, "multinomial_logit_lpmf(y_unit | mu_unit)", fixed = TRUE)
  # multi() takes integer counts; the assembled y_unit must be int.
  expect_match(sc, "array[Kg] int y_unit", fixed = TRUE)
  expect_match(sc, "vector[Kg] mu_unit = mu[idx] - mu[idx[1]];",
               fixed = TRUE)
})

test_that("categ_stan_funs() emits the lpmf body with mu_unit[1] anchor", {
  sc <- mvgam:::categ_stan_funs()
  expect_match(sc, "real categ_lpmf(", fixed = TRUE)
  expect_match(sc, "categorical_logit_lpmf(cat_code | mu_unit)",
               fixed = TRUE)
  expect_match(sc, "vector[Kg] mu_unit = mu[idx] - mu[idx[1]];",
               fixed = TRUE)
})

# Shared helper used by mvn() and generate_factor_model() tests.
extract_block_scode <- function(stanvar_obj, blocks) {
  if (is.null(stanvar_obj)) return("")
  paste(
    vapply(stanvar_obj, function(sv) {
      if (sv$block %in% blocks) sv$scode else ""
    }, character(1L)),
    collapse = "\n"
  )
}

test_that("mvn_stan_funs() emits a per-unit normal_lpdf using Psi as the SD", {
  # Conditional gllvm parameterisation: `Z[k, :] %*% lv[i, :]` is
  # added to `mu[i, k]` by the trend pipeline before the lpdf runs;
  # integrating out `lv ~ N(0, I)` recovers the marginal covariance
  # `Z Z' + diag(Psi^2)`. So the lpdf body just evaluates K
  # independent normals per closure unit.
  sc <- mvgam:::mvn_stan_funs()
  expect_match(sc, "real mvn_lpdf(", fixed = TRUE)
  expect_match(sc, "vector Psi)", fixed = TRUE)
  # Indexed by the component each row carries, not by the row's
  # position in the unit. Positional pairing is right only while
  # every unit holds every component in order, so a site missing one
  # response shifted every later component onto another's scale.
  expect_match(
    sc, "vector[Kg] psi_unit = Psi[visit_component[g, 1:Kg]];",
    fixed = TRUE
  )
  expect_match(sc, "array[,] int visit_component,", fixed = TRUE)
  expect_match(
    sc, "normal_lpdf(y_unit | mu_unit, psi_unit)",
    fixed = TRUE
  )
  # A normal likelihood moves with mu and keeps every shift
  expect_false(grepl("mu[idx[1]]", sc, fixed = TRUE))
})

test_that("mvn() and mvt() declare their residual parameters and priors", {
  dat <- make_mvn_long_data()
  arrays <- mvgam:::build_closure_unit_arrays(
    dat, response_var = "y",
    compute_y_max = FALSE,
    unit_grouping_vars = "time"
  )
  for (fam in list(mvn(), mvt())) {
    sv <- mvgam:::make_multi_response_stanvars(arrays, fam)
    param_sc <- extract_block_scode(sv, "parameters")
    model_sc <- extract_block_scode(sv, "model")
    # `K` (max_rep on the closure-unit arrays) is a literal. The trend's
    # `N_series_trend` is out of scope where the family declares `Psi`.
    expect_match(param_sc, "vector<lower=0>[4] Psi;", fixed = TRUE)
    expect_match(model_sc, "Psi ~ inv_gamma(3, 1.07);", fixed = TRUE)
  }
  # The bound keeps the residual variance of mvt() finite
  expect_match(param_sc, "real<lower=2> nu;", fixed = TRUE)
  expect_match(model_sc, "nu ~ gamma(2, 0.1);", fixed = TRUE)
})

test_that("a user prior on Psi or nu reaches the emitted statement", {
  dat <- make_mvn_long_data()
  arrays <- mvgam:::build_closure_unit_arrays(
    dat, response_var = "y",
    compute_y_max = FALSE,
    unit_grouping_vars = "time"
  )
  user <- brms::prior("exponential(2)", class = "Psi") +
    brms::prior("gamma(3, 0.2)", class = "nu")
  model_sc <- extract_block_scode(
    mvgam:::make_multi_response_stanvars(arrays, mvt(), user), "model"
  )
  expect_match(model_sc, "Psi ~ exponential(2);", fixed = TRUE)
  expect_match(model_sc, "nu ~ gamma(3, 0.2);", fixed = TRUE)
})

# ------------------------------------------------------------
# mvt(): independent Student-t rows of scale Psi[k] and shared df nu,
# given the factor term the trend adds to mu
# ------------------------------------------------------------

test_that("mvt_stan_funs() emits per-row student_t_lpdf with Psi and nu", {
  sc <- mvgam:::mvt_stan_funs()
  expect_match(sc, "real mvt_lpdf(", fixed = TRUE)
  expect_match(sc, "vector Psi,", fixed = TRUE)
  expect_match(sc, "real nu)", fixed = TRUE)
  # Indexed by component, as in mvn()
  expect_match(
    sc, "vector[Kg] psi_unit = Psi[visit_component[g, 1:Kg]];",
    fixed = TRUE
  )
  expect_match(
    sc, "student_t_lpdf(y_unit | nu, mu_unit, psi_unit)",
    fixed = TRUE
  )
})

test_that("the multi-response families share one closure-unit contract", {
  cases <- multi_response_cases()
  for (nm in names(cases)) {
    fam <- cases[[nm]]$fam
    # Every component takes the identity link on mu
    expect_identical(fam$link, "identity", label = nm)
    expect_identical(fam$dpars, cases[[nm]]$dpars, label = nm)
    expect_identical(fam$type, cases[[nm]]$type, label = nm)
    expect_false(fam$loop, label = nm)
    expect_true(uses_closure_unit_layout(fam), label = nm)
    expect_true(is_multi_response_family(fam), label = nm)
    expect_identical(
      is_simplex_response_family(fam), cases[[nm]]$simplex, label = nm
    )
    expect_identical(
      attr(fam, "mvgam_vars", exact = TRUE), cases[[nm]]$vars, label = nm
    )
  }
  expect_identical(cases$diri$fam$link_phi, "log")
  # A categorical outcome is one-hot, which bounds each row at one
  expect_true(isTRUE(attr(cases$categ$fam, "mvgam_binary_response",
                          exact = TRUE)))
})

test_that("a composition unit outside its family's support is refused", {
  prep <- function(fam, d, ...) {
    mvgam:::prepare_closure_unit_family(
      fam, d, response_var = "y", series_var = "series",
      time_var = "time", ...
    )
  }
  cases <- multi_response_cases()
  two_hot <- cases$categ$data
  two_hot$y[two_hot$time == 1] <- 1L
  expect_error(prep(categ(), two_hot), "one 1 among zeros")
  off_sum <- cases$diri$data
  off_sum$y[1] <- off_sum$y[1] + 0.3
  expect_error(prep(diri(), off_sum), "sum to one")
  negative <- cases$multi$data
  negative$y[1] <- -1L
  expect_error(prep(multi(), negative), "non-negative counts")
  # A unit's density takes one concentration
  expect_error(
    prep(diri(), cases$diri$data, dpar_forms = list(phi = phi ~ series)),
    "'series' varies within a closure unit"
  )
})

# ------------------------------------------------------------
# A simplex family declares each column of `Z` a Stan
# `sum_to_zero_vector[K]` (Stan >= 2.36), assembled into `Z` in
# transformed parameters. Other families sample a free `matrix Z`.
# ------------------------------------------------------------

test_that(
  "generate_matrix_z_parameters() emits sum_to_zero_vector for simplex", {
  sv_simplex <- mvgam:::generate_matrix_z_parameters(simplex = TRUE)
  param_sc <- extract_block_scode(sv_simplex, "parameters")
  tparam_sc <- extract_block_scode(sv_simplex, "tparameters")
  expect_match(
    param_sc,
    "array[N_lv_trend] sum_to_zero_vector[N_series_trend] Z_cols;",
    fixed = TRUE
  )
  expect_match(
    tparam_sc,
    "matrix[N_series_trend, N_lv_trend] Z;",
    fixed = TRUE
  )
  expect_match(tparam_sc, "Z[, l] = Z_cols[l];", fixed = TRUE)
})

test_that(
  "generate_matrix_z_parameters() emits plain free Z for non-simplex", {
  sv <- mvgam:::generate_matrix_z_parameters(simplex = FALSE)
  param_sc <- extract_block_scode(sv, "parameters")
  expect_match(
    param_sc,
    "matrix[N_series_trend, N_lv_trend] Z;",
    fixed = TRUE
  )
  expect_false(grepl("sum_to_zero_vector", param_sc, fixed = TRUE))
})

# ------------------------------------------------------------
# default_simplex_population_priors() + the formula warning
# helper for K-shared-only obs formulas.
# ------------------------------------------------------------

test_that(
  "default_simplex_population_priors() returns student_t for b + Intercept", {
  pr <- mvgam:::default_simplex_population_priors()
  expect_true(any(pr$class == "b"))
  expect_true(any(pr$class == "Intercept"))
  expect_true(all(grepl("student_t\\(3, 0, 2\\.5\\)", pr$prior)))
})

test_that(
  "warn_simplex_obs_formula_lacks_species() detects K-shared-only forms", {
  local_verbose_warnings()
  withr::with_envvar(c(TESTTHAT = ""), {
    # Warns when no species term
    expect_warning(
      mvgam:::warn_simplex_obs_formula_lacks_species(
        y ~ env, species_chr = "series"
      ),
      "shared across categories"
    )
    # No warn when series appears as a main effect
    expect_no_warning(
      mvgam:::warn_simplex_obs_formula_lacks_species(
        y ~ env * series, species_chr = "series"
      )
    )
    # No warn under brms-native form
    expect_no_warning(
      mvgam:::warn_simplex_obs_formula_lacks_species(
        y ~ 0 + series + env:series, species_chr = "series"
      )
    )
    # No warn when the user uses a non-default species column
    expect_no_warning(
      mvgam:::warn_simplex_obs_formula_lacks_species(
        y ~ env * habitat, species_chr = "habitat"
      )
    )
  })
})

test_that("build_closure_unit_arrays() returns correctly shaped arrays", {
  d <- make_nmix_data(n_unit = 4, n_visit = 3)
  arrs <- build_closure_unit_arrays(d, response_var = "y")
  expect_named(arrs, c(
    "N_unit", "n_rep", "K_max", "Y_max",
    "visit_idx", "visit_component", "visit_row", "row_unit", "max_rep",
    "unit_labels", "unit_grid", "unit_vars"
  ))

  # Two coordinate systems, and they are not interchangeable.
  # `visit_idx` numbers the rows brms retained, which is what the
  # Stan data indexes because brms drops a row whose response is
  # missing; `visit_row` numbers the rows of the frame as supplied,
  # which is what every post-fit path needs. With nothing missing
  # the two coincide, which is why indexing one against the other
  # went unnoticed until a frame arrived with unmade visits.
  expect_identical(arrs$visit_idx, arrs$visit_row)

  # `row_unit` answers for every row, not only the observed ones:
  # a visit that never happened has no density but still has an
  # expected value, so it still belongs to its unit.
  expect_identical(arrs$row_unit, rep(seq_len(4L), each = 3L))

  # `unit_grid` carries the grouping values for each unit in unit
  # order, so a caller labelling units reads it instead of rebuilding
  # the grid and risking a different ordering.
  expect_identical(nrow(arrs$unit_grid), 4L)
  # `unit_vars` names the grouping this call resolved. A consumer
  # takes the axis from here and never repeats the default.
  expect_identical(arrs$unit_vars, c("series", "time"))
  # The label is the values the caller supplied, not the integer key
  # the grouping is matched on: a reader has to be able to get from a
  # row of a per-unit table back to the unit it describes.
  expect_identical(arrs$unit_labels,
                   paste(arrs$unit_grid$series, arrs$unit_grid$time,
                         sep = "_"))
  expect_identical(arrs$N_unit, 4L)
  expect_identical(arrs$n_rep, rep(3L, 4))
  expect_identical(arrs$K_max, rep(20L, 4))
  expect_identical(arrs$max_rep, 3L)
  # visit_idx[g, ] gives the row indices in d that belong to
  # closure unit g. Each unit should land in a contiguous block.
  for (g in seq_len(arrs$N_unit)) {
    rows_g <- arrs$visit_idx[g, seq_len(arrs$n_rep[g])]
    expect_identical(rows_g, ((g - 1L) * 3L + 1L):(g * 3L))
    expect_identical(arrs$Y_max[g], max(d$y[rows_g]))
  }
})

test_that("build_closure_unit_arrays() handles ragged visit counts", {
  # Unit 1 gets 3 visits, unit 2 gets 2, unit 3 gets 1.
  d <- data.frame(
    series = factor(c(rep(1L, 3), rep(2L, 2), 3L)),
    time   = rep(1L, 6),
    y      = c(2L, 4L, 3L, 1L, 0L, 5L),
    cap    = rep(10L, 6)
  )
  arrs <- build_closure_unit_arrays(d, response_var = "y")
  expect_identical(arrs$N_unit, 3L)
  expect_identical(arrs$n_rep, c(3L, 2L, 1L))
  expect_identical(arrs$max_rep, 3L)
  expect_identical(arrs$Y_max, c(4L, 1L, 5L))
  # Padding columns must be a valid row index (1L) so Stan never
  # indexes outside the array even when those slots are unused.
  expect_identical(arrs$visit_idx[2, 3], 1L)
  expect_identical(arrs$visit_idx[3, 2:3], c(1L, 1L))
})

test_that(
  "build_closure_unit_arrays() supports custom column names", {
  d <- data.frame(
    site    = factor(rep(1L:2L, each = 3L)),
    visit_t = rep(1L, 6),
    counts  = c(1L, 2L, 3L, 4L, 5L, 6L),
    nmax    = rep(15L, 6)
  )
  arrs <- build_closure_unit_arrays(
    d,
    response_var = "counts",
    series_var   = "site",
    time_var     = "visit_t",
    cap_var      = "nmax"
  )
  expect_identical(arrs$N_unit, 2L)
  expect_identical(arrs$K_max, c(15L, 15L))
})

test_that("build_closure_unit_arrays() errors on missing columns", {
  d <- make_nmix_data()
  d$cap <- NULL
  expect_error(
    build_closure_unit_arrays(d, response_var = "y"),
    "Absent: 'cap'"
  )
})

test_that("build_closure_unit_arrays() errors when cap varies within a unit", {
  d <- make_nmix_data(n_unit = 2, n_visit = 3)
  d$cap[1L] <- 30L  # row 1 in unit 1
  expect_error(
    build_closure_unit_arrays(d, response_var = "y"),
    "'cap' varies within a closure unit"
  )
})

test_that("build_closure_unit_arrays() skips unvisited occasions", {
  d <- make_nmix_data()
  d_na_y <- d
  d_na_y$y[1L] <- NA_integer_
  arrays <- build_closure_unit_arrays(d_na_y, response_var = "y")
  # The unvisited occasion leaves its unit one replicate shorter. The
  # indices count positions among the rows brms keeps.
  expect_identical(sum(arrays$n_rep), sum(!is.na(d_na_y$y)))
  expect_true(max(arrays$visit_idx) <= sum(!is.na(d_na_y$y)))
})

test_that("build_closure_unit_arrays() still rejects a missing cap", {
  # The cap bounds the latent state for a whole unit, so unlike a
  # response there is no observed visit to fall back on.
  d_na_cap <- make_nmix_data()
  d_na_cap$cap[1L] <- NA_integer_
  expect_error(
    build_closure_unit_arrays(d_na_cap, response_var = "y"),
    "Missing values in 'cap'"
  )
})

# ------------------------------------------------------------
# validate_closure_unit_data()
# ------------------------------------------------------------

test_that("validate_closure_unit_data() accepts nmix data without a cap", {
  # The count variants bound each unit from its largest count
  d <- make_nmix_data()
  d$cap <- NULL
  expect_invisible(validate_closure_unit_data(
    d, nmix(), response_var = "y", has_obs_covariates = TRUE
  ))
})

test_that("validate_closure_unit_data() errors on cap < observed count", {
  d <- make_nmix_data()
  # cap=2 stays a positive integer, but a count of 5 in the
  # same row exceeds it, tripping the per-row cap >= y check.
  d$cap[1L] <- 2L
  d$cap[2L] <- 2L
  d$cap[3L] <- 2L
  d$y[1L]   <- 5L
  expect_error(
    validate_closure_unit_data(
      d, nmix(), response_var = "y", has_obs_covariates = TRUE
    ),
    "below the observed counts"
  )
})

test_that("validate_closure_unit_data() errors on cap varying within a unit", {
  d <- make_nmix_data()
  # Unit 1 has rows 1..3; raise cap on the first row only.
  d$cap[1L] <- 50L
  expect_error(
    validate_closure_unit_data(
      d, nmix(), response_var = "y", has_obs_covariates = TRUE
    ),
    "'cap' varies within a closure unit"
  )
})

test_that("validate_closure_unit_data() errors on non-integer counts", {
  d <- make_nmix_data()
  d$y <- d$y + 0.5
  expect_error(
    validate_closure_unit_data(
      d, nmix(), response_var = "y", has_obs_covariates = TRUE
    ),
    "Non-integer values found in response"
  )
})

test_that("validate_closure_unit_data() errors on negative counts", {
  d <- make_nmix_data()
  d$y[1L] <- -1L
  expect_error(
    validate_closure_unit_data(
      d, nmix(), response_var = "y", has_obs_covariates = TRUE
    ),
    "Negative counts found"
  )
})

test_that(
  "validate_closure_unit_data() errors on single visits for counts", {
  d <- make_nmix_data(n_unit = 5, n_visit = 1)
  # Count families (binary_response = FALSE) hard-error on this
  # configuration because lambda has unbounded support and the
  # lambda*p product is the only identified quantity (Solymos
  # et al. 2012, Dennis et al. 2015). Binary-response families
  # (occ) demote this to a warning per Royle and Dorazio 2008.
  expect_error(
    validate_closure_unit_data(
      d, nmix(),
      response_var       = "y",
      has_obs_covariates = FALSE,
      has_det_covariates = FALSE
    ),
    "closure-unit count family is not identified"
  )
})

test_that(
  "validate_closure_unit_data() accepts single visits given a covariate", {
  d <- make_nmix_data(n_unit = 5, n_visit = 1)
  # With at least one covariate in either layer the model is
  # identifiable from cross-unit shared structure.
  expect_invisible(validate_closure_unit_data(
    d, nmix(),
    response_var       = "y",
    has_obs_covariates = TRUE,
    has_det_covariates = FALSE
  ))
})


# ------------------------------------------------------------
# how_to_cite() coverage for nmix
# ------------------------------------------------------------

test_that("how_to_cite reference_db carries the closure-unit references", {
  db <- reference_db()
  keys <- c(
    "royle_nmix_2004", "dennis_nmix_2015", "kery_nmix_2018",
    "knape_overdispersion_2018", "royle_nichols_2003",
    "neyman_type_a_1939"
  )
  expect_contains(names(db), keys)
})

# ------------------------------------------------------------
# nmix() variants
# ------------------------------------------------------------

test_that("each nmix() variant carries its response type and cap rule", {
  # Royle-Nichols sees only detections and takes a default cap of 25,
  # as unmarked::occuRN() does. The count variants derive theirs from
  # the data as max(y) + 100. A 'cap' column overrides either.
  variants <- list(
    poisson_binomial = list(binary = FALSE, cap = NULL, buffer = 100L),
    royle_nichols = list(binary = TRUE, cap = 25L, buffer = NULL),
    poisson_poisson = list(binary = FALSE, cap = NULL, buffer = 100L)
  )
  for (type in names(variants)) {
    fam <- nmix(type)
    v <- variants[[type]]
    expect_identical(attr(fam, "mvgam_nmix_type", exact = TRUE), type)
    expect_identical(
      isTRUE(attr(fam, "mvgam_binary_response", exact = TRUE)), v$binary,
      label = type
    )
    expect_identical(attr(fam, "mvgam_default_cap", exact = TRUE), v$cap)
    expect_identical(
      attr(fam, "mvgam_default_cap_buffer", exact = TRUE), v$buffer
    )
    expect_identical(attr(fam, "mvgam_predict_types", exact = TRUE),
                     c("latent_state", "detection"))
  }
  # Poisson-Poisson detection is a rate per visit, with a log link and
  # no upper bound
  ppm <- nmix("poisson_poisson")
  expect_identical(ppm$link_p, "log")
  expect_true(is.na(ppm$ub[2L]))
})

test_that("nmix('poisson_poisson') warns that intercepts alone are weak", {
  # The data identify only lambda * p when neither formula has a
  # covariate. Verbose warnings lift the once-per-session limit.
  local_verbose_warnings()
  withr::local_envvar(TESTTHAT = "")
  set.seed(99)
  n_unit <- 15L; n_visit <- 3L
  d <- data.frame(
    series = factor(rep(seq_len(n_unit), each = n_visit)),
    time   = rep(1L, n_unit * n_visit),
    y      = rpois(n_unit * n_visit, 2),
    cap    = rep(20L, n_unit * n_visit)
  )
  prepare <- function(has_obs_covariates) {
    prepare_closure_unit_family(
      nmix("poisson_poisson"), data = d, response_var = "y",
      has_obs_covariates = has_obs_covariates,
      has_det_covariates = FALSE,
      series_var = "series", time_var = "time"
    )
  }
  expect_warning(
    prepare(FALSE),
    class = "nmix_poisson_poisson_intercept_only"
  )
  expect_no_warning(prepare(TRUE))
})

test_that("latent_N_saturation rejects non-closure-unit fits", {
  fake <- structure(
    list(family = gaussian()), class = "mvgam"
  )
  expect_error(
    latent_N_saturation(fake),
    "closure-unit family"
  )
})

test_that("build_closure_unit_arrays errors when user 'cap' < observed max", {
  dat <- data.frame(
    y      = c(2L, 1L, 3L, 0L, 2L, 1L),
    cap    = c(2L, 2L, 2L, 5L, 5L, 5L),
    time   = c(1L, 1L, 1L, 2L, 2L, 2L),
    series = factor(rep("s1", 6L))
  )
  expect_error(
    mvgam:::build_closure_unit_arrays(data = dat,
                                        response_var = "y"),
    "is below the observed max"
  )
})

test_that("build_closure_unit_arrays computes K_max = Y_max + buffer", {
  dat <- data.frame(
    y      = c(2L, 1L, 3L, 0L, 2L, 1L),
    time   = c(1L, 1L, 1L, 2L, 2L, 2L),
    series = factor(rep("s1", 6L))
  )
  arrays <- mvgam:::build_closure_unit_arrays(
    data = dat, response_var = "y",
    default_cap_buffer = 100L
  )
  expect_equal(arrays$Y_max, c(3L, 2L))
  expect_equal(arrays$K_max, c(103L, 102L))
})

test_that("uses_threading() detects reduce_sum in stancode", {
  threaded <- structure(
    list(stancode = "target += reduce_sum(partial_sum_nmix_lpmf, ...);"),
    class = "mvgam"
  )
  unthreaded <- structure(
    list(stancode = "target += normal_id_glm_lpdf(...);"),
    class = "mvgam"
  )
  expect_true(mvgam:::uses_threading(threaded))
  expect_false(mvgam:::uses_threading(unthreaded))
  # Missing stancode slot -> FALSE (defensive).
  expect_false(
    mvgam:::uses_threading(structure(list(), class = "mvgam"))
  )
})

# ----------------------------------------------------------------
# multi_season opt-in on the closure-unit family constructors.
# Default is single-season (no mvgam_unit_grouping attr); the
# downstream validator/builder fall back to (series, time).
# multi_season = TRUE sets mvgam_unit_grouping = c("series",
# "site", "time"); prepare_closure_unit_family() forwards this
# into validate_closure_unit_data() and build_closure_unit_arrays()
# so the closure unit is keyed on the 3-axis tuple.
# ----------------------------------------------------------------

test_that("only a multi_season opt-in declares a unit grouping", {
  # The attribute records an opt-in, so every constructor that did
  # not opt in must leave it unset and fall through to a default.
  declared <- list(
    occ(), nmix(), nmix("royle_nichols"), nmix("poisson_poisson"),
    diri(), multi(), categ(), mvn(), mvt()
  )
  for (fam in declared) {
    expect_null(closure_unit_grouping(fam))
  }
  for (fam in list(occ(multi_season = TRUE),
                   nmix(multi_season = TRUE),
                   nmix("royle_nichols", multi_season = TRUE),
                   nmix("poisson_poisson", multi_season = TRUE))) {
    expect_identical(
      closure_unit_grouping(fam),
      c("series", "site", "time")
    )
  }
})

test_that("closure_unit_key_vars() keys a multi-response unit on time alone", {
  # This is the accessor every consumer reads, and the two defaults
  # behind it are not the same. A detection unit is one series
  # visited repeatedly, so it keys on (series, time) and a
  # per-series cut of a frame keeps its units whole. A
  # multi-response unit is a site whose rows are the K response
  # components, so it keys on time alone and a per-series cut
  # leaves one row per unit -- for a composition, a simplex of
  # width one whose only probability is 1 by construction.
  #
  # Testing the raw attribute instead of this accessor is how the
  # multi-response default came to be missing here while the array
  # builder had it: `kfold()` then keyed folds on (series, time)
  # and split every composition unit across folds, scoring each
  # species against a site the training data still held.
  for (fam in list(diri(), multi(), categ(), mvn(), mvt())) {
    expect_identical(closure_unit_key_vars(fam), "time")
  }
  for (fam in list(occ(), nmix(), nmix("royle_nichols"),
                   nmix("poisson_poisson"))) {
    expect_identical(closure_unit_key_vars(fam), c("series", "time"))
  }
  # A declared grouping wins over both defaults.
  expect_identical(
    closure_unit_key_vars(occ(multi_season = TRUE)),
    c("series", "site", "time")
  )
  # A family with no closure unit is distinguishable from one
  # taking the default, so a caller can word its own refusal.
  expect_null(closure_unit_key_vars(brms::negbinomial()))
  # With no family the builder and the validator take the detection
  # default, built from the column names they pass.
  expect_identical(closure_unit_key_vars(NULL), c("series", "time"))
  expect_identical(
    closure_unit_key_vars(NULL, series_var = "sp", time_var = "yr"),
    c("sp", "yr")
  )
})

test_that("the closure-unit key survives a separator in its values", {
  # The columns are integer-coded before they are pasted, so a value
  # carrying the separator cannot merge two units. Pasting the values
  # themselves gives `series = "sp_1", time = "1"` and
  # `series = "sp", time = "1_1"` the single key "sp_1_1".
  d <- data.frame(
    series = c("sp_1", "sp_1", "sp", "sp"),
    time = c("1", "1", "1_1", "1_1"),
    y = c(1L, 0L, 1L, 1L)
  )
  idx <- mvgam:::closure_unit_index(d, c("series", "time"), "y")
  expect_length(idx$levels, 2L)
  expect_identical(idx$n_rep, c(2L, 2L))
})

# ------------------------------------------------------------
# The idiosyncratic split mvn() and mvt() ask the data to make.
# ------------------------------------------------------------

test_that("identified_factor_ceiling counts the split the data can make", {
  # The test evaluates the condition `(K - m)^2 >= K + m` directly.
  # The helper implements a closed form of it.
  for (K in 2:12) {
    m <- seq_len(K)
    ok <- (K - m)^2 >= K + m
    expected <- if (any(ok)) max(m[ok]) else 0L
    expect_identical(
      mvgam:::identified_factor_ceiling(K), as.integer(expected)
    )
  }
})

test_that("only mvn and mvt carry a per-component residual scale", {
  carries <- vapply(
    list(mvn(), mvt()), mvgam:::family_has_component_scale, logical(1L)
  )
  expect_true(all(carries))
  # A simplex family's spread is in the softmax, and a detection
  # family has no continuous residual to scale.
  others <- vapply(
    list(diri(), multi(), categ(), occ(), nmix(), poisson(), Beta()),
    mvgam:::family_has_component_scale, logical(1L)
  )
  expect_false(any(others))
})
