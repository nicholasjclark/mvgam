# Tests for the closure-unit family infrastructure and all
# closure-unit family constructors: `nmix()` (Poisson-Binomial,
# Royle-Nichols, Poisson-Poisson variants), `occ()`, and the
# simplex multi-response trio `diri()` / `multi()` / `categ()`.
# Stan emission contracts assert on the source-helper strings
# directly; all model-fitting tests live in tests/local/.

# ------------------------------------------------------------
# nmix() constructor
# ------------------------------------------------------------

test_that("nmix() returns a customfamily with mu/p dpars and logit/log links", {
  fam <- nmix()
  expect_s3_class(fam, "customfamily")
  expect_s3_class(fam, "brmsfamily")
  expect_identical(fam$name, "nmix")
  expect_identical(fam$dpars, c("mu", "p"))
  # brms stores the primary link in `link` and aux-dpar links in
  # `link_<dpar>` slots, not as a single `links` vector.
  expect_identical(fam$link, "log")
  expect_identical(fam$link_p, "logit")
  expect_identical(fam$type, "int")
  expect_false(fam$loop)
  # brms wraps lb/ub into a named list of character vectors per
  # dpar (the strings become Stan-side bounds at codegen time).
  # Bounds on p keep the scalar-dpar case sampled in (0, 1) so
  # the lpdf's logit(p) call stays valid even when there's no
  # sub-formula for detection.
  expect_identical(fam$lb, list(mu = "0", p = "0"))
  expect_identical(fam$ub, list(mu = NA_character_, p = "1"))
  # linkinv / linkfun helpers are attached so downstream
  # dispatchers (compute_family_epred etc.) don't have to
  # branch on customfamily.
  expect_true(is.function(fam$linkinv))
  expect_true(is.function(fam$linkfun))
  expect_equal(fam$linkinv(0), 1)
})

test_that("nmix() tags closure-unit and predict-type attributes", {
  fam <- nmix()
  expect_true(is_closure_unit_family(fam))
  expect_identical(
    attr(fam, "mvgam_predict_types", exact = TRUE),
    c("latent_state", "detection")
  )
  # The Stan stanvars slot is filled in at data-prep time; chunk 1
  # leaves it NULL so attach_family_stanvars() passes through.
  expect_null(attr(fam, "mvgam_stanvars", exact = TRUE))
})

test_that("is_closure_unit_family() returns FALSE for non-closure families", {
  expect_false(is_closure_unit_family(NULL))
  expect_false(is_closure_unit_family(gaussian()))
  expect_false(is_closure_unit_family(brms::brmsfamily("poisson")))
  expect_false(is_closure_unit_family(tweedie()))
})

# ------------------------------------------------------------
# Multi-response family predicates + gate
# ------------------------------------------------------------
#
# The mvgam_multi_response and mvgam_simplex_response attributes
# tag families that aggregate K species rows per closure unit and
# call Stan's native multivariate likelihood (dirichlet,
# multinomial, categorical, multivariate normal, multivariate T).
# Tests below use mock customfamily objects with the attributes
# set directly, so the predicates can be verified independently
# of the family constructors that eventually consume them.

mock_multi_response_family <- function(simplex = FALSE) {
  fam <- brms::custom_family(
    name = "mock_multi", dpars = c("mu"), links = "identity",
    type = "real", loop = FALSE
  )
  attr(fam, "mvgam_multi_response") <- TRUE
  if (simplex) attr(fam, "mvgam_simplex_response") <- TRUE
  fam
}

test_that("is_multi_response_family() detects the mvgam_multi_response attr", {
  expect_true(is_multi_response_family(mock_multi_response_family()))
  expect_true(is_multi_response_family(
    mock_multi_response_family(simplex = TRUE)
  ))
})

test_that("is_multi_response_family() returns FALSE for non-MV families", {
  expect_false(is_multi_response_family(NULL))
  expect_false(is_multi_response_family(gaussian()))
  expect_false(is_multi_response_family(nmix()))
  expect_false(is_multi_response_family(tweedie()))
})

test_that("is_simplex_response_family() detects only simplex MV families", {
  expect_true(is_simplex_response_family(
    mock_multi_response_family(simplex = TRUE)
  ))
  expect_false(is_simplex_response_family(mock_multi_response_family()))
  expect_false(is_simplex_response_family(NULL))
  expect_false(is_simplex_response_family(gaussian()))
})

test_that("validate_supported_family() admits multi-response customfamily objects", {
  expect_invisible(validate_supported_family(mock_multi_response_family()))
  expect_invisible(
    validate_supported_family(mock_multi_response_family(simplex = TRUE))
  )
})

test_that("validate_supported_family() points naked brms multi-category families at the mvgam wrapper", {
  expect_error(validate_supported_family(brms::dirichlet()), "diri\\(\\)")
  expect_error(validate_supported_family(brms::multinomial()),
               "multi\\(\\)")
  expect_error(validate_supported_family(brms::categorical()),
               "categ\\(\\)")
  expect_error(validate_supported_family(brms::logistic_normal()),
               "mvn\\(\\)")
})


# ------------------------------------------------------------
# diri(): family registration, attribute tagging,
# closure-unit grouping, and Stan emission round-trip
# ------------------------------------------------------------

make_dirichlet_long_data <- function(n_sites = 6L, n_species = 4L,
                                     seed = 11L) {
  set.seed(seed)
  species_levels <- paste0("y", seq_len(n_species))
  alpha <- exp(matrix(rnorm(n_sites * n_species, sd = 0.3),
                      n_sites, n_species))
  Y <- t(apply(alpha, 1L, function(a) {
    d <- rgamma(n_species, shape = a, rate = 1)
    d / sum(d)
  }))
  Y <- pmax(Y, 1e-4); Y <- Y / rowSums(Y)
  data.frame(
    series = factor(rep(species_levels, times = n_sites),
                    levels = species_levels),
    time   = rep(seq_len(n_sites), each = n_species),
    y      = as.vector(t(Y)),
    env    = rep(rnorm(n_sites), each = n_species)
  )
}

test_that("diri() returns a custom family with the right tags", {
  fam <- diri()
  expect_s3_class(fam, "customfamily")
  expect_identical(fam$name, "diri")
  expect_identical(fam$dpars, c("mu", "phi"))
  expect_identical(fam$link, "identity")
  expect_identical(fam$link_phi, "log")
  expect_true(is_closure_unit_family(fam))
  expect_true(is_multi_response_family(fam))
  expect_true(is_simplex_response_family(fam))
  expect_identical(
    attr(fam, "mvgam_vars", exact = TRUE),
    c("N_unit", "n_rep", "visit_idx")
  )
})

test_that("diri() composes with validate_supported_family", {
  expect_invisible(validate_supported_family(diri()))
})

test_that("prepare_closure_unit_family() groups dirichlet rows by site, not by (species, site)", {
  fam <- diri()
  dat <- make_dirichlet_long_data(n_sites = 5L, n_species = 4L)
  fam_prep <- mvgam:::prepare_closure_unit_family(
    fam, dat, response_var = "y",
    has_obs_covariates = FALSE, has_det_covariates = FALSE
  )
  sv <- attr(fam_prep, "mvgam_stanvars", exact = TRUE)
  expect_false(is.null(sv))
  expect_identical(fam_prep$vars, c("N_unit", "n_rep", "visit_idx"))
  # Verify the unit grouping by inspecting the standata round-trip.
  mf <- bf(y ~ env, family = fam_prep)
  sd <- brms::make_standata(mf, data = dat, stanvars = sv)
  expect_identical(sd$N_unit, 5L)
  expect_identical(sd$n_rep, rep(4L, 5L))
  expect_identical(dim(sd$visit_idx), c(5L, 4L))
})

test_that("diri_stan_funs() emits the lpdf body with mu_unit[1] anchor", {
  # Assert on the source-helper string directly. Cheaper than the
  # brms make_stancode round-trip used in older contract tests; the
  # closure-unit data-array side of the contract is exercised in the
  # prepare_closure_unit_family() standata test above.
  sc <- mvgam:::diri_stan_funs()
  expect_match(sc, "real diri_lpdf(", fixed = TRUE)
  # Native Stan helpers: dirichlet_lpdf + softmax. The brms-emitted
  # `dirichlet_logit_lpdf` helper is NOT a Stan built-in.
  expect_match(
    sc, "dirichlet_lpdf(y_unit | softmax(mu_unit) * phi_g)",
    fixed = TRUE
  )
  # Mode-2 hard identification: subtract `mu_unit[1]` from all
  # entries before softmax. Softmax is shift-invariant so this is
  # likelihood-neutral, but it removes the per-site K-shared shift
  # mode by construction. The soft `normal_lupdf` machinery is gone.
  expect_match(sc, "vector[Kg] mu_unit = mu[idx] - mu[idx[1]];",
               fixed = TRUE)
  expect_false(grepl("mu_unit_sums", sc, fixed = TRUE))
  expect_false(grepl("normal_lupdf(mu_unit_sums", sc, fixed = TRUE))
  # Two-signature dispatch: vector-phi primary (emits the
  # `phi_g = phi[idx[1]]` per-unit collapse) and scalar-phi
  # broadcast that forwards via `rep_vector(phi, N)`.
  expect_match(sc, "vector phi,", fixed = TRUE)
  expect_match(sc, "real phi,", fixed = TRUE)
  expect_match(sc, "real phi_g = phi[idx[1]];", fixed = TRUE)
  expect_match(
    sc, "diri_lpdf(y | mu, rep_vector(phi, N), N_unit",
    fixed = TRUE
  )
})


# ------------------------------------------------------------
# multi(): family registration + Stan emission for multinomial
# ------------------------------------------------------------

make_multi_long_data <- function(n_sites = 6L, n_species = 4L,
                                  seed = 21L) {
  set.seed(seed)
  species_levels <- paste0("y", seq_len(n_species))
  rows <- list()
  for (s in seq_len(n_sites)) {
    p <- exp(rnorm(n_species, sd = 0.4))
    p <- p / sum(p)
    N_s <- sample(30:80, 1L)
    y_s <- as.vector(rmultinom(1L, N_s, p))
    env_s <- rnorm(1L)
    for (k in seq_len(n_species)) {
      rows[[length(rows) + 1L]] <- data.frame(
        series = factor(species_levels[k], levels = species_levels),
        time   = s,
        y      = y_s[k],
        env    = env_s
      )
    }
  }
  do.call(rbind, rows)
}

test_that("multi() returns a custom family with the right tags", {
  fam <- multi()
  expect_s3_class(fam, "customfamily")
  expect_identical(fam$name, "multi")
  expect_identical(fam$dpars, "mu")
  expect_identical(fam$link, "identity")
  expect_identical(fam$type, "int")
  expect_false(fam$loop)
  expect_true(is_closure_unit_family(fam))
  expect_true(is_multi_response_family(fam))
  expect_true(is_simplex_response_family(fam))
  expect_identical(
    attr(fam, "mvgam_vars", exact = TRUE),
    c("N_unit", "n_rep", "visit_idx")
  )
})

test_that("multi() composes with validate_supported_family", {
  expect_invisible(validate_supported_family(multi()))
})

test_that("multi_stan_funs() emits the lpmf body with mu_unit[1] anchor", {
  sc <- mvgam:::multi_stan_funs()
  expect_match(sc, "real multi_lpmf(", fixed = TRUE)
  expect_match(sc, "multinomial_logit_lpmf", fixed = TRUE)
  # multi() takes integer counts; the assembled y_unit must be int.
  expect_match(sc, "array[Kg] int y_unit", fixed = TRUE)
  # Mode-2 hard identification (see diri test for rationale).
  expect_match(sc, "vector[Kg] mu_unit = mu[idx] - mu[idx[1]];",
               fixed = TRUE)
  expect_false(grepl("mu_unit_sums", sc, fixed = TRUE))
})

test_that("prepare_closure_unit_family() groups multinomial rows by site", {
  fam <- multi()
  dat <- make_multi_long_data(n_sites = 5L, n_species = 4L)
  fam_prep <- mvgam:::prepare_closure_unit_family(
    fam, dat, response_var = "y",
    has_obs_covariates = FALSE, has_det_covariates = FALSE
  )
  sv <- attr(fam_prep, "mvgam_stanvars", exact = TRUE)
  expect_false(is.null(sv))
  expect_identical(fam_prep$vars, c("N_unit", "n_rep", "visit_idx"))
  mf <- bf(y ~ env, family = fam_prep)
  sd <- brms::make_standata(mf, data = dat, stanvars = sv)
  expect_identical(sd$N_unit, 5L)
  expect_identical(sd$n_rep, rep(4L, 5L))
  expect_identical(dim(sd$visit_idx), c(5L, 4L))
})

# ------------------------------------------------------------
# categ(): family registration + Stan emission for categorical
# ------------------------------------------------------------

make_categ_long_data <- function(n_sites = 8L, n_categories = 4L,
                                  seed = 31L) {
  set.seed(seed)
  species_levels <- paste0("c", seq_len(n_categories))
  rows <- list()
  for (s in seq_len(n_sites)) {
    p <- exp(rnorm(n_categories, sd = 0.5))
    p <- p / sum(p)
    cat_obs <- sample.int(n_categories, 1L, prob = p)
    env_s <- rnorm(1L)
    for (k in seq_len(n_categories)) {
      rows[[length(rows) + 1L]] <- data.frame(
        series = factor(species_levels[k], levels = species_levels),
        time   = s,
        y      = as.integer(k == cat_obs),
        env    = env_s
      )
    }
  }
  do.call(rbind, rows)
}

test_that("categ() returns a custom family with the right tags", {
  fam <- categ()
  expect_s3_class(fam, "customfamily")
  expect_identical(fam$name, "categ")
  expect_identical(fam$dpars, "mu")
  expect_identical(fam$type, "int")
  expect_true(is_closure_unit_family(fam))
  expect_true(is_multi_response_family(fam))
  expect_true(is_simplex_response_family(fam))
  expect_true(isTRUE(
    attr(fam, "mvgam_binary_response", exact = TRUE)
  ))
})

test_that("categ() composes with validate_supported_family", {
  expect_invisible(validate_supported_family(categ()))
})

test_that("categ_stan_funs() emits the lpmf body with mu_unit[1] anchor", {
  sc <- mvgam:::categ_stan_funs()
  expect_match(sc, "real categ_lpmf(", fixed = TRUE)
  expect_match(sc, "categorical_logit_lpmf", fixed = TRUE)
  # One-hot scan to recover the scalar cat_code from Y_unit.
  expect_match(sc, "for (k in 1:Kg) {", fixed = TRUE)
  # Mode-2 hard identification (see diri test for rationale).
  expect_match(sc, "vector[Kg] mu_unit = mu[idx] - mu[idx[1]];",
               fixed = TRUE)
  expect_false(grepl("mu_unit_sums", sc, fixed = TRUE))
})

test_that("prepare_closure_unit_family() groups categorical rows by site", {
  fam <- categ()
  dat <- make_categ_long_data(n_sites = 5L, n_categories = 4L)
  fam_prep <- mvgam:::prepare_closure_unit_family(
    fam, dat, response_var = "y",
    has_obs_covariates = FALSE, has_det_covariates = FALSE
  )
  sv <- attr(fam_prep, "mvgam_stanvars", exact = TRUE)
  expect_false(is.null(sv))
  expect_identical(fam_prep$vars, c("N_unit", "n_rep", "visit_idx"))
  mf <- bf(y ~ env, family = fam_prep)
  sd <- brms::make_standata(mf, data = dat, stanvars = sv)
  expect_identical(sd$N_unit, 5L)
  expect_identical(sd$n_rep, rep(4L, 5L))
  expect_identical(dim(sd$visit_idx), c(5L, 4L))
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

# ------------------------------------------------------------
# mvn(): family registration + Stan emission for the
# multivariate normal closure-unit family.
# ------------------------------------------------------------

make_mvn_long_data <- function(n_sites = 6L, n_species = 4L,
                                seed = 41L) {
  set.seed(seed)
  species_levels <- paste0("y", seq_len(n_species))
  rows <- list()
  for (s in seq_len(n_sites)) {
    env_s <- rnorm(1L)
    eps <- rnorm(n_species)
    for (k in seq_len(n_species)) {
      rows[[length(rows) + 1L]] <- data.frame(
        series = factor(species_levels[k], levels = species_levels),
        time   = s,
        y      = eps[k] + 0.4 * env_s,
        env    = env_s
      )
    }
  }
  do.call(rbind, rows)
}

test_that("mvn() returns a custom family with the right tags", {
  fam <- mvn()
  expect_s3_class(fam, "customfamily")
  expect_identical(fam$name, "mvn")
  expect_identical(fam$dpars, "mu")
  expect_identical(fam$link, "identity")
  expect_identical(fam$type, "real")
  expect_false(fam$loop)
  expect_true(is_closure_unit_family(fam))
  expect_true(is_multi_response_family(fam))
  # NOT simplex: multi_normal_cholesky_lpdf is shift-sensitive so
  # the simplex identification machinery must be skipped.
  expect_false(is_simplex_response_family(fam))
  expect_identical(
    attr(fam, "mvgam_vars", exact = TRUE),
    c("N_unit", "n_rep", "visit_idx", "Psi")
  )
})

test_that("mvn() composes with validate_supported_family", {
  expect_invisible(validate_supported_family(mvn()))
})

test_that("mvn_stan_funs() emits a per-unit normal_lpdf using Psi as the SD", {
  # Conditional gllvm parameterisation: `Z[k, :] %*% lv[i, :]` is
  # added to `mu[i, k]` by the trend pipeline before the lpdf runs;
  # integrating out `lv ~ N(0, I)` recovers the marginal covariance
  # `Z Z' + diag(Psi^2)`. So the lpdf body just evaluates K
  # independent normals per closure unit.
  sc <- mvgam:::mvn_stan_funs()
  expect_match(sc, "real mvn_lpdf(", fixed = TRUE)
  expect_match(sc, "vector Psi)", fixed = TRUE)
  expect_match(sc, "vector[Kg] psi_unit = Psi[1:Kg];", fixed = TRUE)
  expect_match(
    sc, "normal_lpdf(y_unit | mu_unit, psi_unit)",
    fixed = TRUE
  )
  # mvn lpdf does NOT take Z (the loadings matrix) as an argument
  # or compute the Cholesky inside the function block; both are
  # consequences of the conditional parameterisation.
  expect_false(grepl("matrix Z", sc, fixed = TRUE))
  expect_false(grepl("cholesky_decompose", sc, fixed = TRUE))
  expect_false(grepl("multi_normal_cholesky_lpdf", sc, fixed = TRUE))
  # No simplex-specific shift removal here.
  expect_false(grepl("mu[idx[1]]", sc, fixed = TRUE))
  expect_false(grepl("mu_unit_sums", sc, fixed = TRUE))
})

test_that("make_mvn_stanvars() declares the SD-scale Psi parameter and prior", {
  dat <- make_mvn_long_data()
  arrays <- mvgam:::build_closure_unit_arrays(
    dat, response_var = "y",
    compute_y_max = FALSE,
    unit_grouping_vars = "time"
  )
  sv <- mvgam:::make_mvn_stanvars(arrays)
  param_sc <- extract_block_scode(sv, "parameters")
  model_sc <- extract_block_scode(sv, "model")
  # `K` (max_rep on the closure-unit arrays) is baked in as a
  # literal so the declaration does not depend on the trend
  # pipeline's `N_series_trend` symbol being in scope.
  expect_match(
    param_sc,
    "vector<lower=0>[4] Psi;",
    fixed = TRUE
  )
  expect_match(model_sc, "Psi ~ exponential(1);", fixed = TRUE)
})

test_that("prepare_closure_unit_family() groups mvn rows by site", {
  fam <- mvn()
  dat <- make_mvn_long_data(n_sites = 5L, n_species = 4L)
  fam_prep <- mvgam:::prepare_closure_unit_family(
    fam, dat, response_var = "y",
    has_obs_covariates = FALSE, has_det_covariates = FALSE
  )
  sv <- attr(fam_prep, "mvgam_stanvars", exact = TRUE)
  expect_false(is.null(sv))
  expect_identical(
    fam_prep$vars,
    c("N_unit", "n_rep", "visit_idx", "Psi")
  )
  # Verify the unit grouping by inspecting the standata round-trip.
  mf <- bf(y ~ env, family = fam_prep)
  sd <- brms::make_standata(mf, data = dat, stanvars = sv)
  expect_identical(sd$N_unit, 5L)
  expect_identical(sd$n_rep, rep(4L, 5L))
  expect_identical(dim(sd$visit_idx), c(5L, 4L))
})

# ------------------------------------------------------------
# mvt(): family registration + Stan emission for the heavy-tailed
# multivariate Student-t closure-unit family. Mirrors mvn(): the
# trend pipeline adds `Z[k, :] * lv[i, :]` to mu and lv ~ N(0, I),
# so the lpdf body is independent Student-t per row with scale
# Psi[k] and shared df nu. The marginal residual covariance is
# approximately Z Z' + diag(Psi^2 * nu / (nu - 2)).
# ------------------------------------------------------------

test_that("mvt() returns a custom family with the right tags", {
  fam <- mvt()
  expect_s3_class(fam, "customfamily")
  expect_identical(fam$name, "mvt")
  expect_identical(fam$dpars, "mu")
  expect_identical(fam$link, "identity")
  expect_identical(fam$type, "real")
  expect_false(fam$loop)
  expect_true(is_closure_unit_family(fam))
  expect_true(is_multi_response_family(fam))
  # NOT simplex: student_t_lpdf is shift-sensitive so the simplex
  # identification machinery must be skipped.
  expect_false(is_simplex_response_family(fam))
  expect_identical(
    attr(fam, "mvgam_vars", exact = TRUE),
    c("N_unit", "n_rep", "visit_idx", "Psi", "nu")
  )
})

test_that("mvt() composes with validate_supported_family", {
  expect_invisible(validate_supported_family(mvt()))
})

test_that("mvt_stan_funs() emits per-row student_t_lpdf with Psi and nu", {
  sc <- mvgam:::mvt_stan_funs()
  expect_match(sc, "real mvt_lpdf(", fixed = TRUE)
  expect_match(sc, "vector Psi,", fixed = TRUE)
  expect_match(sc, "real nu)", fixed = TRUE)
  expect_match(sc, "vector[Kg] psi_unit = Psi[1:Kg];", fixed = TRUE)
  expect_match(
    sc, "student_t_lpdf(y_unit | nu, mu_unit, psi_unit)",
    fixed = TRUE
  )
  # mvt does NOT take Z, does NOT compute a Cholesky, and does NOT
  # use the multivariate Student-t lpdf. The conditional gllvm
  # parameterisation reduces the joint K-vector likelihood to K
  # independent Student-t residuals.
  expect_false(grepl("matrix Z", sc, fixed = TRUE))
  expect_false(grepl("cholesky_decompose", sc, fixed = TRUE))
  expect_false(grepl("multi_student_t_lpdf", sc, fixed = TRUE))
  # No simplex-specific shift removal.
  expect_false(grepl("mu[idx[1]]", sc, fixed = TRUE))
  expect_false(grepl("mu_unit_sums", sc, fixed = TRUE))
})

test_that("make_mvt_stanvars() declares Psi, nu, and their priors", {
  dat <- make_mvn_long_data()
  arrays <- mvgam:::build_closure_unit_arrays(
    dat, response_var = "y",
    compute_y_max = FALSE,
    unit_grouping_vars = "time"
  )
  sv <- mvgam:::make_mvt_stanvars(arrays)
  param_sc <- extract_block_scode(sv, "parameters")
  model_sc <- extract_block_scode(sv, "model")
  # K (max_rep) is baked in as a literal so the Psi declaration is
  # self-contained.
  expect_match(
    param_sc,
    "vector<lower=0>[4] Psi;",
    fixed = TRUE
  )
  # nu has a hard floor at 2 so the marginal variance stays finite
  # under heavy-tailed posterior draws.
  expect_match(param_sc, "real<lower=2> nu;", fixed = TRUE)
  expect_match(model_sc, "Psi ~ exponential(1);", fixed = TRUE)
  expect_match(
    model_sc,
    "target += gamma_lpdf(nu - 2 | 2, 0.1);",
    fixed = TRUE
  )
})

test_that("prepare_closure_unit_family() wires mvt() vars and stanvars", {
  fam <- mvt()
  dat <- make_mvn_long_data(n_sites = 5L, n_species = 4L)
  fam_prep <- mvgam:::prepare_closure_unit_family(
    fam, dat, response_var = "y",
    has_obs_covariates = FALSE, has_det_covariates = FALSE
  )
  sv <- attr(fam_prep, "mvgam_stanvars", exact = TRUE)
  expect_false(is.null(sv))
  expect_identical(
    fam_prep$vars,
    c("N_unit", "n_rep", "visit_idx", "Psi", "nu")
  )
  mf <- bf(y ~ env, family = fam_prep)
  sd <- brms::make_standata(mf, data = dat, stanvars = sv)
  expect_identical(sd$N_unit, 5L)
  expect_identical(sd$n_rep, rep(4L, 5L))
  expect_identical(dim(sd$visit_idx), c(5L, 4L))
})

# ------------------------------------------------------------
# Mode-1 Z-column sum-to-zero is enforced HARD via Stan's
# `sum_to_zero_vector[K]` (Stan >= 2.36) in the parameters block.
# `generate_matrix_z_parameters()` emits `Z_cols` plus a
# transformed-parameters assembly to `Z` for simplex families, and
# the plain free `matrix Z` for non-simplex families. The
# `generate_factor_model()` model block carries NO soft sum-to-zero
# prior; the constraint lives at the parameter declaration.
# ------------------------------------------------------------


test_that("generate_matrix_z_parameters() emits sum_to_zero_vector for simplex families", {
  sv_simplex <- mvgam:::generate_matrix_z_parameters(
    is_factor_model = TRUE, n_lv = 2L, n_series = 4L,
    simplex = TRUE
  )
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

test_that("generate_matrix_z_parameters() emits plain free Z for non-simplex families", {
  sv <- mvgam:::generate_matrix_z_parameters(
    is_factor_model = TRUE, n_lv = 2L, n_series = 4L,
    simplex = FALSE
  )
  param_sc <- extract_block_scode(sv, "parameters")
  expect_match(
    param_sc,
    "matrix[N_series_trend, N_lv_trend] Z;",
    fixed = TRUE
  )
  expect_false(grepl("sum_to_zero_vector", param_sc, fixed = TRUE))
  expect_false(grepl("Z_cols", param_sc, fixed = TRUE))
})

test_that("generate_factor_model() does not emit a soft Z-column constraint", {
  for (fam in list(NULL, gaussian(), poisson(), nmix(), diri(),
                   multi(), categ())) {
    sv <- mvgam:::generate_factor_model(
      is_factor_model = TRUE, n_lv = 2L, family = fam
    )
    sc <- extract_block_scode(sv, "model")
    expect_false(
      grepl("sum(Z[, l]) ~ normal(0, 0.01)", sc, fixed = TRUE)
    )
  }
})

test_that("generate_factor_model() rejects non-family objects on `family`", {
  expect_error(
    mvgam:::generate_factor_model(
      is_factor_model = TRUE, n_lv = 2L, family = "diri"
    ),
    "family"
  )
})

# ------------------------------------------------------------
# default_simplex_population_priors() + the formula warning
# helper for K-shared-only obs formulas.
# ------------------------------------------------------------

test_that("default_simplex_population_priors() returns student_t for b + Intercept", {
  pr <- mvgam:::default_simplex_population_priors()
  expect_s3_class(pr, "brmsprior")
  expect_true(any(pr$class == "b"))
  expect_true(any(pr$class == "Intercept"))
  expect_true(all(grepl("student_t\\(3, 0, 2\\.5\\)", pr$prior)))
})

test_that("warn_simplex_obs_formula_lacks_species() detects K-shared-only forms", {
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

# ------------------------------------------------------------
# build_closure_unit_arrays()
# ------------------------------------------------------------

# Helper used across tests in this file.
make_nmix_data <- function(n_unit = 4, n_visit = 3, seed = 1) {
  set.seed(seed)
  n_total <- n_unit * n_visit
  data.frame(
    series = factor(rep(seq_len(n_unit), each = n_visit)),
    time   = rep(1L, n_total),
    visit  = rep(seq_len(n_visit), n_unit),
    y      = pmin(rpois(n_total, 5), 20L),
    cap    = rep(20L, n_total),
    elev   = rep(rnorm(n_unit), each = n_visit),
    tod    = stats::runif(n_total)
  )
}

test_that("build_closure_unit_arrays() returns correctly shaped arrays", {
  d <- make_nmix_data(n_unit = 4, n_visit = 3)
  arrs <- build_closure_unit_arrays(d, response_var = "y")
  expect_named(arrs, c(
    "N_unit", "n_rep", "K_max", "Y_max",
    "visit_idx", "max_rep", "unit_labels"
  ))
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

test_that("build_closure_unit_arrays() supports custom column names (predict path)", {
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

test_that("build_closure_unit_arrays() reflects changed cap when called on newdata", {
  d_fit <- make_nmix_data(n_unit = 3, n_visit = 2)
  arrs_fit <- build_closure_unit_arrays(d_fit, response_var = "y")
  expect_identical(arrs_fit$K_max, rep(20L, 3))

  # Same data, but cap raised to 50 in the prediction call.
  d_pred <- d_fit
  d_pred$cap <- 50L
  arrs_pred <- build_closure_unit_arrays(d_pred, response_var = "y")
  expect_identical(arrs_pred$K_max, rep(50L, 3))
  # Visit structure unchanged; K_max is the only thing that
  # responds to the user's edited cap column.
  expect_identical(arrs_pred$N_unit, arrs_fit$N_unit)
  expect_identical(arrs_pred$n_rep, arrs_fit$n_rep)
  expect_identical(arrs_pred$visit_idx, arrs_fit$visit_idx)
})

test_that("build_closure_unit_arrays() errors on missing columns", {
  d <- make_nmix_data()
  d$cap <- NULL
  expect_error(
    build_closure_unit_arrays(d, response_var = "y"),
    "Closure-unit families require column 'cap'"
  )
})

test_that("build_closure_unit_arrays() errors when cap varies within a unit", {
  d <- make_nmix_data(n_unit = 2, n_visit = 3)
  d$cap[1L] <- 30L  # row 1 in unit 1
  expect_error(
    build_closure_unit_arrays(d, response_var = "y"),
    "must be constant within a closure unit"
  )
})

test_that("build_closure_unit_arrays() skips unvisited occasions", {
  d <- make_nmix_data()
  d_na_y <- d
  d_na_y$y[1L] <- NA_integer_
  arrays <- build_closure_unit_arrays(d_na_y, response_var = "y")
  # The unvisited occasion leaves its unit one replicate shorter,
  # and the indices count positions among the rows brms keeps
  # rather than rows of the raw frame.
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

test_that("validate_closure_unit_data() accepts well-formed nmix data", {
  d <- make_nmix_data()
  expect_invisible(validate_closure_unit_data(
    d, response_var = "y", has_obs_covariates = TRUE
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
      d, response_var = "y", has_obs_covariates = TRUE
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
      d, response_var = "y", has_obs_covariates = TRUE
    ),
    "must be constant within a closure unit"
  )
})

test_that("validate_closure_unit_data() errors on non-integer counts", {
  d <- make_nmix_data()
  d$y <- d$y + 0.5
  expect_error(
    validate_closure_unit_data(
      d, response_var = "y", has_obs_covariates = TRUE
    ),
    "Non-integer values found in response"
  )
})

test_that("validate_closure_unit_data() errors on negative counts", {
  d <- make_nmix_data()
  d$y[1L] <- -1L
  expect_error(
    validate_closure_unit_data(
      d, response_var = "y", has_obs_covariates = TRUE
    ),
    "Negative counts found"
  )
})

test_that("validate_closure_unit_data() errors when all units single-visit with no covariates", {
  d <- make_nmix_data(n_unit = 5, n_visit = 1)
  # Count families (binary_response = FALSE) hard-error on this
  # configuration because lambda has unbounded support and the
  # lambda*p product is the only identified quantity (Solymos
  # et al. 2012, Dennis et al. 2015). Binary-response families
  # (occ) demote this to a warning per Royle and Dorazio 2008.
  expect_error(
    validate_closure_unit_data(
      d,
      response_var       = "y",
      has_obs_covariates = FALSE,
      has_det_covariates = FALSE
    ),
    "Closure-unit count family is non-identified"
  )
})

test_that("validate_closure_unit_data() accepts single-visit data when a covariate is supplied", {
  d <- make_nmix_data(n_unit = 5, n_visit = 1)
  # With at least one covariate in either layer the model is
  # identifiable from cross-unit shared structure.
  expect_invisible(validate_closure_unit_data(
    d,
    response_var       = "y",
    has_obs_covariates = TRUE,
    has_det_covariates = FALSE
  ))
})

# ------------------------------------------------------------
# validate_supported_family() admits customfamily objects
# ------------------------------------------------------------

test_that("validate_supported_family() admits nmix() and tweedie() customfamily objects", {
  expect_invisible(validate_supported_family(nmix()))
  expect_invisible(validate_supported_family(tweedie()))
})

# ------------------------------------------------------------
# Stan emission contract tests via brms make_stancode round-trip
# ------------------------------------------------------------
#
# These tests exercise the data-prep hook in
# `generate_stan_components_mvgam_formula()` to confirm that
# closure-unit arrays land in standata and the nmix lpdf
# function block lands in stancode with the expected signature.

test_that("stancode under nmix() includes the lpdf signature and data declarations", {
  d <- make_nmix_data(n_unit = 4, n_visit = 3)
  mf <- mvgam_formula(y ~ elev)
  sc <- as.character(stancode(mf, data = d, family = nmix()))
  # Function block: both overloaded signatures emitted.
  expect_match(sc, "real nmix_lpmf\\(\\s*array\\[\\] int y,", fixed = FALSE)
  expect_match(sc, "vector mu,", fixed = TRUE)
  expect_match(sc, "vector p,", fixed = TRUE)
  expect_match(sc, "real p,", fixed = TRUE)  # scalar broadcast entry point
  # Log-space ratio recurrence: per-unit baseline at K_min_g plus
  # the Horner accumulation inside the marginalisation loop. The
  # recurrence avoids the per-k binomial_logit_lpmf re-evaluation
  # that the naive log_sum_exp form pays.
  expect_match(sc, "poisson_log_lpmf(K_min_g | log_lam)", fixed = TRUE)
  expect_match(
    sc, "binomial_logit_lpmf(counts | K_min_g, lp_visits)",
    fixed = TRUE
  )
  # log1m_p is hoisted to the wrapper (task #230) so the partial
  # sum body reads `log1m_p[idx]` instead of recomputing log1m(p)
  # per chunk. The wrapper precomputes all three link-scale
  # vectors (log_mu, logit_p, log1m_p) before dispatching to
  # reduce_sum.
  expect_match(sc, "real log_ff = log_lam + sum(log1m_p[idx]);",
               fixed = TRUE)
  expect_match(sc, "vector[num_elements(p)] log1m_p = log1m(p);",
               fixed = TRUE)
  expect_match(sc, "for (i in 1 : possible_N)", fixed = TRUE)
  expect_match(
    sc,
    "log_prob_n = log_sum_exp(0, log_prob_n + log_ff + log_k_obs - log_N);",
    fixed = TRUE
  )
  # Threading: per-unit body lives in partial_sum_nmix_lpmf;
  # nmix_lpmf wrapper calls reduce_sum over closure units. When
  # stan_threads is compiled in (via `threads = N` on mvgam()),
  # TBB splits chunks across threads.
  expect_match(sc, "real partial_sum_nmix_lpmf(", fixed = TRUE)
  expect_match(sc, "reduce_sum(", fixed = TRUE)
  # stanc line-wraps the single-statement for-body onto two
  # lines, so assert on the two pieces independently.
  expect_match(sc, "array[N_unit] int g_seq;", fixed = TRUE)
  expect_match(sc, "g_seq[g] = g;", fixed = TRUE)
  # grainsize heuristic targets ~8 chunks (N_unit / 8 with a 1
  # floor) so single-threaded fits pay minimal dispatch overhead
  # AND multi-threaded fits see ~8 chunks across cores.
  expect_match(sc, "int grainsize = N_unit >= 8 ? N_unit / 8 : 1;",
               fixed = TRUE)
  # No remnants of the old vectorised log-sum-exp loop.
  expect_false(grepl("component_lps", sc, fixed = TRUE))
  expect_false(grepl("poisson_log_lpmf(k | log_lam)", sc, fixed = TRUE))
  # Data block: closure-unit arrays at unit length, not visit length.
  expect_match(sc, "int<lower=1> N_unit;", fixed = TRUE)
  expect_match(sc, "array[N_unit] int<lower=1> n_rep;", fixed = TRUE)
  expect_match(sc, "array[N_unit] int<lower=1> K_max;", fixed = TRUE)
  expect_match(sc, "array[N_unit] int<lower=0> Y_max;", fixed = TRUE)
  expect_match(sc, "array[N_unit, 3] int<lower=1> visit_idx;", fixed = TRUE)
  # Likelihood call wires the dpars + vint args correctly; the
  # trailing `log_n_lookup` argument is the cached log(N) vector
  # emitted in transformed data (task #316) so the inner ratio
  # loop indexes a vector instead of calling scalar log() per
  # iteration.
  expect_match(
    sc,
    "nmix_lpmf(Y | mu, p, N_unit, n_rep, K_max, Y_max, visit_idx, log_n_lookup)",
    fixed = TRUE
  )
  expect_match(sc, "int K_max_global = max(K_max);", fixed = TRUE)
  expect_match(sc, "vector[K_max_global] log_n_lookup;", fixed = TRUE)
  expect_match(sc, "log_n_lookup[n_lk] = log(n_lk);", fixed = TRUE)
  # Scalar-p case: p declared as a bounded probability so the
  # lpdf's logit(p) call is well-defined even without a
  # `p ~ ...` sub-formula.
  expect_match(sc, "real<lower=0, upper=1> p;", fixed = TRUE)
})

test_that("stancode under occ() emits partial_sum + reduce_sum scaffold", {
  d <- make_nmix_data(n_unit = 4, n_visit = 3)
  d$y <- as.integer(d$y > 0L)
  mf <- mvgam_formula(y ~ elev)
  sc <- as.character(stancode(mf, data = d, family = occ()))
  # Per-unit body factored into partial_sum_occ_lpmf; wrapper hoists
  # logit(mu) -> logit_psi and logit(p) -> logit_p once before
  # dispatching to reduce_sum so per-thread chunks reuse the cached
  # link-scale vectors.
  expect_match(sc, "real partial_sum_occ_lpmf(", fixed = TRUE)
  expect_match(sc, "vector[num_elements(mu)] logit_psi = logit(mu);",
               fixed = TRUE)
  expect_match(sc, "vector[num_elements(p)] logit_p = logit(p);",
               fixed = TRUE)
  # Per-unit marginalisation branches on whether anything was
  # detected: z = 1 certain when Y_max[g] >= 1, otherwise
  # log_sum_exp over {z = 0, z = 1}.
  expect_match(sc, "if (Y_max[g] >= 1)", fixed = TRUE)
  expect_match(sc, "log_sum_exp(loglik_z1, loglik_z0)", fixed = TRUE)
  # Threading: reduce_sum + grainsize heuristic shared with the
  # other closure-unit families.
  expect_match(sc, "reduce_sum(partial_sum_occ_lpmf,", fixed = TRUE)
  expect_match(sc, "array[N_unit] int g_seq;", fixed = TRUE)
  expect_match(sc, "g_seq[g] = g;", fixed = TRUE)
  expect_match(sc, "int grainsize = N_unit >= 8 ? N_unit / 8 : 1;",
               fixed = TRUE)
  # All four signature overloads (vec/vec, vec/scalar, scalar/vec,
  # scalar/scalar) must land so the brms emission resolves regardless
  # of whether mu / p are dpars or constants.
  expect_match(sc, "real occ_lpmf(array[] int y, vector mu, vector p, int N_unit,",
               fixed = TRUE)
  expect_match(sc, "real occ_lpmf(array[] int y, vector mu, real p, int N_unit,",
               fixed = TRUE)
  expect_match(sc, "real occ_lpmf(array[] int y, real mu, vector p, int N_unit,",
               fixed = TRUE)
  expect_match(sc, "real occ_lpmf(array[] int y, real mu, real p, int N_unit,",
               fixed = TRUE)
  # Likelihood call wires the standata args correctly.
  expect_match(
    sc,
    "occ_lpmf(Y | mu, p, N_unit, n_rep, Y_max, visit_idx)",
    fixed = TRUE
  )
  # Y_max is the per-unit detection indicator; bounded {0, 1}.
  expect_match(sc, "array[N_unit] int<lower=0, upper=1> Y_max;",
               fixed = TRUE)
  # occ() must NOT carry log_n_lookup (no log(int) in the body).
  expect_false(grepl("log_n_lookup", sc, fixed = TRUE))
})

test_that("stancode under nmix('royle_nichols') emits partial_sum + reduce_sum scaffold", {
  d <- make_nmix_data(n_unit = 4, n_visit = 3)
  d$y <- as.integer(d$y > 0L)
  mf <- mvgam_formula(y ~ elev)
  sc <- as.character(stancode(mf, data = d, family = nmix("royle_nichols")))
  # Per-unit body factored into partial_sum_nmix_royle_nichols_lpmf;
  # wrapper hoists log(mu) -> log_mu and log1m(p) -> log_1m_r once
  # before dispatching to reduce_sum.
  expect_match(sc, "real partial_sum_nmix_royle_nichols_lpmf(",
               fixed = TRUE)
  expect_match(sc, "vector[num_elements(mu)] log_mu = log(mu);",
               fixed = TRUE)
  expect_match(sc, "vector[num_elements(p)] log_1m_r = log1m(p);",
               fixed = TRUE)
  # Closure constraint: k < cmax pre-filled with -Inf so log_sum_exp
  # over the K_max marginalisation cells skips infeasible states.
  expect_match(sc, "for (k in 0 : (cmax - 1))", fixed = TRUE)
  expect_match(sc, "component_lps[k + 1] = negative_infinity();",
               fixed = TRUE)
  # Inner k loop: k * sum_non_det_log_1m_r is the closed-form
  # non-detection contribution; dot_product(counts_v, log1m_exp(k * log_1m_r_v))
  # is the per-visit at-least-one-detection contribution.
  expect_match(sc, "k * sum_non_det_log_1m_r", fixed = TRUE)
  expect_match(sc, "log1m_exp(k * log_1m_r_v)", fixed = TRUE)
  # Threading: reduce_sum + grainsize heuristic.
  expect_match(sc, "reduce_sum(partial_sum_nmix_royle_nichols_lpmf,",
               fixed = TRUE)
  expect_match(sc, "int grainsize = N_unit >= 8 ? N_unit / 8 : 1;",
               fixed = TRUE)
  # Likelihood call.
  expect_match(
    sc,
    "nmix_royle_nichols_lpmf(Y | mu, p, N_unit, n_rep, K_max, Y_max, visit_idx)",
    fixed = TRUE
  )
  # Data: K_max alongside the binary Y_max indicator.
  expect_match(sc, "array[N_unit] int<lower=1> K_max;", fixed = TRUE)
  expect_match(sc, "array[N_unit] int<lower=0, upper=1> Y_max;",
               fixed = TRUE)
  # RN has no log(int) in the inner loop, so log_n_lookup must NOT
  # be emitted for this family.
  expect_false(grepl("log_n_lookup", sc, fixed = TRUE))
  # Haines (2016) closed-form fast-path for the all-zero detection
  # case. cmax == 0 collapses sum_{N=0}^infty Poisson(N|lambda) *
  # prod_t (1-r_t)^N to exp(lambda * (prod_t (1-r_t) - 1)) via the
  # Poisson MGF -- exact, scalar, no log_sum_exp, no K_max
  # truncation bias on the dominant all-zero units.
  expect_match(sc, "if (cmax == 0)", fixed = TRUE)
  expect_match(sc, "real log_z = sum(log_1m_r_v);", fixed = TRUE)
  expect_match(sc, "lp += exp(log_lam) * (exp(log_z) - 1.0);",
               fixed = TRUE)
})

test_that("RN Haines closed form (Y_max = 0) matches brute-force marginalisation to round-off", {
  # Pin the Poisson-MGF identity used in the Stan code at machine
  # precision. The closed form `lp = lambda * (z - 1)` with
  # `z = prod_t (1 - r_t)` must agree with the truncated sum
  # `sum_{k=0}^{K_max} Poisson(k|lambda) * z^k` in the limit
  # K_max -> infty. We pick K_max = 200 (well above all tested
  # lambdas) so the truncation error is below double precision and
  # any deviation > 1e-12 surfaces as a bug in the Stan emission.
  closed_form <- function(lambda, r_t) {
    z <- prod(1 - r_t)
    lambda * (z - 1)
  }
  brute_force_log <- function(lambda, r_t, K_max) {
    z <- prod(1 - r_t)
    k_vals <- 0:K_max
    log_terms <- stats::dpois(k_vals, lambda, log = TRUE) + k_vals * log(z)
    m <- max(log_terms)
    m + log(sum(exp(log_terms - m)))
  }
  # Span the typical RN parameter space: low / moderate / high
  # encounter rate per visit; 2 / 5 / 10 visits; lambda in
  # {0.1, 1, 5, 15, 30}. All combinations must agree to round-off.
  scenarios <- expand.grid(
    lambda = c(0.1, 1, 5, 15, 30),
    n_visit = c(2L, 5L, 10L),
    r = c(0.05, 0.3, 0.7)
  )
  for (i in seq_len(nrow(scenarios))) {
    s <- scenarios[i, ]
    r_t <- rep(s$r, s$n_visit)
    closed <- closed_form(s$lambda, r_t)
    brute  <- brute_force_log(s$lambda, r_t, 200L)
    expect_equal(closed, brute, tolerance = 1e-12)
  }
})

test_that("stancode under nmix('poisson_poisson') emits partial_sum + log_n_lookup + reduce_sum", {
  d <- make_nmix_data(n_unit = 4, n_visit = 3)
  mf <- mvgam_formula(y ~ elev)
  sc <- as.character(stancode(mf, data = d, family = nmix("poisson_poisson")))
  # Per-unit body factored into partial_sum_nmix_poisson_poisson_lpmf;
  # wrapper hoists log(mu) -> log_mu and log(p) -> log_p once. The
  # raw p vector is also threaded through because the inner cell
  # uses k * sum(p_v) as the Poisson rate aggregate.
  expect_match(sc, "real partial_sum_nmix_poisson_poisson_lpmf(",
               fixed = TRUE)
  expect_match(sc, "vector[num_elements(mu)] log_mu = log(mu);",
               fixed = TRUE)
  expect_match(sc, "vector[num_elements(p)] log_p = log(p);",
               fixed = TRUE)
  # Inner k loop uses log_n_lookup[k] instead of scalar log(k) so
  # the AD tape skips the per-iter math op (task #316 pattern).
  expect_match(sc, "log_n_lookup[k] * sum_counts", fixed = TRUE)
  # k = 0 only feasible when no detections; flip to -Inf otherwise
  # so log_sum_exp over the full K range marginalises cleanly.
  expect_match(sc, "if (any_detection)", fixed = TRUE)
  expect_match(sc, "component_lps[1] = negative_infinity();",
               fixed = TRUE)
  # Threading: reduce_sum, grainsize heuristic, log_n_lookup as a
  # tail-position argument matching the partial_sum signature.
  expect_match(sc, "reduce_sum(partial_sum_nmix_poisson_poisson_lpmf,",
               fixed = TRUE)
  expect_match(sc, "int grainsize = N_unit >= 8 ? N_unit / 8 : 1;",
               fixed = TRUE)
  # Transformed data: log_n_lookup built once over K_max_global so
  # the lpmf indexes a cached vector instead of recomputing log(k).
  expect_match(sc, "int K_max_global = max(K_max);", fixed = TRUE)
  expect_match(sc, "vector[K_max_global] log_n_lookup;", fixed = TRUE)
  expect_match(sc, "log_n_lookup[n_lk] = log(n_lk);", fixed = TRUE)
  # Likelihood call: log_n_lookup + k_start_ppm tail args present.
  expect_match(
    sc,
    paste0(
      "nmix_poisson_poisson_lpmf(Y | mu, p, N_unit, n_rep, K_max, ",
      "Y_max, visit_idx, log_n_lookup, k_start_ppm)"
    ),
    fixed = TRUE
  )
  # Data: K_max and unbounded Y_max (counts).
  expect_match(sc, "array[N_unit] int<lower=1> K_max;", fixed = TRUE)
  expect_match(sc, "array[N_unit] int<lower=0> Y_max;", fixed = TRUE)
  # Per-unit Poisson-tail lower bound on the latent-N loop. The
  # lpmf reads k_start_ppm[g] as the inner-loop start; skipping
  # cells below it costs negligible mass (see make_nmix_poisson_
  # poisson_stanvars derivation).
  expect_match(sc, "array[N_unit] int<lower=1> k_start_ppm;",
               fixed = TRUE)
  expect_match(sc, "int kg_lo = k_start_ppm[g];", fixed = TRUE)
  expect_match(sc, "for (k in kg_lo : Kg)", fixed = TRUE)
})

test_that("nmix('poisson_poisson') k_start_ppm bound collapses to 1 for low-count units", {
  # Low Y_max stays at k_start = 1 (no skipped cells); high Y_max
  # gets a Poisson-tail-derived lower bound (~ Y_max - 7*sqrt(Y_max),
  # floor 1). Asserts the standata column matches the closed form.
  d <- make_nmix_data(n_unit = 4, n_visit = 3)
  # Force a known Y_max profile by overriding y. Per-unit max
  # detection counts: {0, 5, 100, 500}. The data prep computes
  # Y_max[g] = max(y) over visits in unit g. Raise cap so the
  # closure-unit validator (K_max >= Y_max) is satisfied.
  d$y <- as.integer(c(
    0, 0, 0,
    1, 5, 4,
    80, 100, 95,
    490, 500, 480
  ))
  d$cap <- 600L
  mf <- mvgam_formula(y ~ 1)
  sd <- standata(mf, data = d, family = nmix("poisson_poisson"))
  y_max_expected <- c(0L, 5L, 100L, 500L)
  expect_identical(as.integer(sd$Y_max), y_max_expected)
  # k_start_ppm[g] = max(1, Y_max[g] - 7 * ceil(sqrt(Y_max[g]))).
  offset <- 7L * as.integer(ceiling(sqrt(y_max_expected)))
  expected <- pmax(1L, y_max_expected - offset)
  # Sanity-check the closed-form math: Y_max <= 49 yields a 1 floor
  # (7*sqrt(Y_max) >= Y_max for Y_max <= 49); Y_max = 100 yields 30
  # (offset = 70); Y_max = 500 yields 339 (offset = 161).
  expect_identical(expected, c(1L, 1L, 30L, 339L))
  expect_identical(as.integer(sd$k_start_ppm), expected)
})

test_that("nmix log-space recurrence matches brute-force log-sum-exp on canonical cases", {
  # Independent R-side implementation of the Stan log-space ratio
  # recurrence in nmix_stan_funs(). Asserting both forms agree to
  # machine precision protects the Stan emission against silent
  # math regressions on the marginalisation algorithm. Cases cover
  # the corners worth defending against (high p with y near N,
  # large possible_N, single-visit, tiny lambda, K_max == K_min
  # no-op, all-zero detection history).
  brute_force <- function(y_visits, lambda, p, K_max) {
    K_min <- max(y_visits)
    if (length(p) == 1L) p <- rep(p, length(y_visits))
    k_vals <- K_min:K_max
    log_terms <- vapply(k_vals, function(k) {
      stats::dpois(k, lambda, log = TRUE) +
        sum(stats::dbinom(y_visits, k, p, log = TRUE))
    }, numeric(1L))
    m <- max(log_terms)
    m + log(sum(exp(log_terms - m)))
  }
  recurrence_log <- function(y_visits, lambda, p, K_max) {
    K_min <- max(y_visits)
    possible_N <- K_max - K_min
    V <- length(y_visits)
    if (length(p) == 1L) p <- rep(p, V)
    log_lam <- log(lambda)
    log_ff <- log_lam + sum(log1p(-p))
    log_prob_n <- 0
    if (possible_N > 0L) {
      for (i in seq_len(possible_N)) {
        N <- K_max - i + 1
        log_N <- log(N)
        log_k_obs <- sum(log_N - log(N - y_visits))
        a <- 0
        b <- log_prob_n + log_ff + log_k_obs - log_N
        mab <- max(a, b)
        log_prob_n <- mab + log(exp(a - mab) + exp(b - mab))
      }
    }
    baseline <- stats::dpois(K_min, lambda, log = TRUE) +
      sum(stats::dbinom(y_visits, K_min, p, log = TRUE))
    baseline + log_prob_n
  }
  cases <- list(
    list(y = c(0L, 0L, 0L),         lam = 1.5,  p = 0.5,  K = 20L),
    list(y = c(2L, 3L, 1L),         lam = 5,    p = 0.6,  K = 30L),
    list(y = c(8L, 9L, 10L),        lam = 30,   p = 0.4,  K = 50L),
    list(y = c(1L),                 lam = 10,   p = 0.3,  K = 25L),
    list(y = c(0L, 0L, 0L, 0L, 0L), lam = 50,   p = 0.95, K = 100L),
    list(y = c(5L),                 lam = 0.01, p = 0.5,  K = 10L),
    list(y = c(2L, 2L, 2L),         lam = 2,    p = 0.5,  K = 2L)
  )
  for (cs in cases) {
    expect_equal(
      recurrence_log(cs$y, cs$lam, cs$p, cs$K),
      brute_force(cs$y, cs$lam, cs$p, cs$K),
      tolerance = 1e-12
    )
  }
})

test_that("standata under nmix() carries the closure-unit arrays with correct values", {
  d <- make_nmix_data(n_unit = 5, n_visit = 2, seed = 7)
  mf <- mvgam_formula(y ~ elev)
  sd <- standata(mf, data = d, family = nmix())
  arrs <- build_closure_unit_arrays(d, response_var = "y")
  expect_identical(as.integer(sd$N_unit), arrs$N_unit)
  expect_identical(as.integer(sd$n_rep),  arrs$n_rep)
  expect_identical(as.integer(sd$K_max),  arrs$K_max)
  expect_identical(as.integer(sd$Y_max),  arrs$Y_max)
  expect_equal(dim(sd$visit_idx), c(arrs$N_unit, arrs$max_rep))
  expect_equal(as.integer(sd$visit_idx), as.integer(arrs$visit_idx))
})

test_that("standata K_max updates when newdata carries a different cap column", {
  d_fit  <- make_nmix_data(n_unit = 4, n_visit = 3)
  d_pred <- d_fit
  d_pred$cap <- 99L
  mf <- mvgam_formula(y ~ elev)
  sd_fit  <- standata(mf, data = d_fit,  family = nmix())
  sd_pred <- standata(mf, data = d_pred, family = nmix())
  expect_identical(as.integer(sd_fit$K_max),  rep(20L, 4))
  expect_identical(as.integer(sd_pred$K_max), rep(99L, 4))
  # Visit structure is identical; only K_max responds to the
  # edited cap column.
  expect_identical(sd_fit$N_unit, sd_pred$N_unit)
  expect_identical(sd_fit$n_rep,  sd_pred$n_rep)
  expect_identical(sd_fit$visit_idx, sd_pred$visit_idx)
})

test_that("stancode under nmix() emits vector-p path when a detection sub-formula is supplied", {
  d <- make_nmix_data(n_unit = 4, n_visit = 3)
  d$tod <- stats::runif(nrow(d))
  mf <- mvgam_formula(brms::bf(y ~ elev, p ~ tod))
  sc <- as.character(stancode(mf, data = d, family = nmix()))
  # brms emits a vector p with inv_logit applied when the
  # sub-formula supplies a design matrix for the detection dpar.
  expect_match(sc, "vector[N] p", fixed = TRUE)
  expect_match(sc, "p = inv_logit(p)", fixed = TRUE)
  # b_p coefficients become available to the user.
  expect_match(sc, "Intercept_p", fixed = TRUE)
  expect_match(sc, "Xc_p", fixed = TRUE)
})

# R-side prediction surface tests (log_lik, posterior_epred,
# posterior_predict, predict(latent_N), predict(detection),
# predict(variance)) for the PB nmix family live in
# tests/local/nmix_fitting.R because they compile Stan and
# sample short HMC chains. The cheap contract coverage stays
# here.

# ------------------------------------------------------------
# how_to_cite() coverage for nmix
# ------------------------------------------------------------

test_that("how_to_cite reference_db includes the four nmix entries", {
  db <- reference_db()
  expected <- c(
    "royle_nmix_2004",
    "dennis_nmix_2015",
    "kery_nmix_2018",
    "knape_overdispersion_2018"
  )
  expect_true(all(expected %in% names(db)))
  for (key in expected) {
    expect_true(nzchar(db[[key]]$text))
    expect_true(nzchar(db[[key]]$bibtex))
  }
})

test_that("uses_nmix_family() predicate distinguishes the family", {
  expect_false(uses_nmix_family(NULL))
  expect_false(uses_nmix_family(list(family = gaussian())))
  expect_true(uses_nmix_family(list(family = nmix())))
  expect_false(uses_nmix_family(list(family = tweedie())))
  # Royle-Nichols is its own predicate; the PB predicate excludes it
  # so the citation rule picks the variant-specific reference set.
  expect_false(
    uses_nmix_family(list(family = nmix("royle_nichols")))
  )
})

test_that("uses_nmix_royle_nichols_family() predicate distinguishes the RN variant", {
  expect_false(uses_nmix_royle_nichols_family(NULL))
  expect_false(uses_nmix_royle_nichols_family(list(family = gaussian())))
  expect_false(uses_nmix_royle_nichols_family(list(family = nmix())))
  expect_true(
    uses_nmix_royle_nichols_family(list(family = nmix("royle_nichols")))
  )
})

test_that("how_to_cite reference_db carries the Royle-Nichols 2003 entry", {
  db <- mvgam:::reference_db()
  expect_true("royle_nichols_2003" %in% names(db))
  rn <- db[["royle_nichols_2003"]]
  expect_true(grepl("Royle JA and Nichols JD", rn$text))
  expect_true(grepl("royle2003abundance", rn$bibtex))
})

# ------------------------------------------------------------
# nmix("royle_nichols") — Stan emission, dispatcher, recovery
# ------------------------------------------------------------

test_that("nmix('royle_nichols') constructor exposes the RN family name and binary-response flag", {
  fam <- nmix("royle_nichols")
  expect_identical(fam$name, "nmix_royle_nichols")
  expect_true(isTRUE(attr(fam, "mvgam_closure_unit", exact = TRUE)))
  expect_identical(attr(fam, "mvgam_nmix_type", exact = TRUE),
                   "royle_nichols")
  expect_true(isTRUE(attr(fam, "mvgam_binary_response",
                          exact = TRUE)))
  # RN defaults K_max to 25 to match unmarked::occuRN. Users still
  # override via a 'cap' column when latent_N_saturation() flags
  # truncation bias.
  expect_identical(
    attr(fam, "mvgam_default_cap", exact = TRUE), 25L
  )
  expect_identical(attr(fam, "mvgam_predict_types", exact = TRUE),
                   c("latent_state", "detection"))
})

# Stan emission + end-to-end + smooth-r + RE + smooth-on-state
# tests for the RN nmix variant live in tests/local/nmix_fitting.R
# because they compile Stan models. The constructor / predicate
# coverage stays in this file (above).

# ------------------------------------------------------------
# nmix("poisson_poisson"): constructor + identifiability warn
# ------------------------------------------------------------

test_that("nmix('poisson_poisson') constructor exposes the PPM family name and count-response config", {
  fam <- nmix("poisson_poisson")
  expect_identical(fam$name, "nmix_poisson_poisson")
  expect_true(isTRUE(attr(fam, "mvgam_closure_unit", exact = TRUE)))
  expect_identical(attr(fam, "mvgam_nmix_type", exact = TRUE),
                   "poisson_poisson")
  # PPM accepts arbitrary counts so the binary-response check is OFF.
  expect_false(isTRUE(attr(fam, "mvgam_binary_response",
                           exact = TRUE)))
  expect_null(attr(fam, "mvgam_default_cap", exact = TRUE))
  expect_identical(attr(fam, "mvgam_predict_types", exact = TRUE),
                   c("latent_state", "detection"))
  # Log link on p (not logit) so the rate stays on positive reals.
  expect_identical(fam$link_p, "log")
  expect_true(is.na(fam$ub[2L]))
})

# Stan emission + end-to-end + smooth-p tests for the PPM nmix
# variant live in tests/local/nmix_fitting.R because they compile
# Stan models. The constructor / predicate / how_to_cite contract
# coverage stays in this file (above and below).

test_that("nmix('poisson_poisson') intercept-only spec runs through prepare_closure_unit_family()", {
  set.seed(99)
  n_unit <- 15L; n_visit <- 3L
  # 3 visits per closure unit (same series, same time across the
  # n_visit rows) so the validator's "every unit single visit + no
  # covariates" hard-error does not fire.
  d <- data.frame(
    series = factor(rep(seq_len(n_unit), each = n_visit)),
    time   = rep(1L, n_unit * n_visit),
    y      = rpois(n_unit * n_visit, 2),
    cap    = rep(20L, n_unit * n_visit)
  )
  fam <- nmix("poisson_poisson")
  # The intercept-only identifiability warn fires from
  # `prepare_closure_unit_family()` via a single `rlang::warn(...,
  # .frequency = "once")` call (R/families.R). Its emission is
  # covered by direct inspection of that call site, not a testthat
  # assertion, because rlang's once-per-session frequency-id cache
  # makes the warning unreliable to catch across runs. The test
  # here only checks that the intercept-only spec routes cleanly
  # through the family preparation.
  fam_prep <- suppressWarnings(
    prepare_closure_unit_family(
      fam,
      data = d,
      response_var = "y",
      has_obs_covariates = FALSE,
      has_det_covariates = FALSE
    )
  )
  expect_identical(fam_prep$name, "nmix_poisson_poisson")
  expect_false(is.null(attr(fam_prep, "mvgam_stanvars", exact = TRUE)))
})

test_that("uses_nmix_poisson_poisson_family() predicate distinguishes the PPM variant", {
  expect_false(uses_nmix_poisson_poisson_family(NULL))
  expect_false(uses_nmix_poisson_poisson_family(list(family = gaussian())))
  expect_false(uses_nmix_poisson_poisson_family(list(family = nmix())))
  expect_false(
    uses_nmix_poisson_poisson_family(list(family = nmix("royle_nichols")))
  )
  expect_true(
    uses_nmix_poisson_poisson_family(list(family = nmix("poisson_poisson")))
  )
})

test_that("how_to_cite reference_db carries the Neyman 1939 entry", {
  db <- mvgam:::reference_db()
  expect_true("neyman_type_a_1939" %in% names(db))
  ny <- db[["neyman_type_a_1939"]]
  expect_true(grepl("Neyman J", ny$text))
  expect_true(grepl("neyman1939contagious", ny$bibtex))
})


test_that("nmix('royle_nichols') sets the auto-default K_max attribute", {
  fam <- nmix("royle_nichols")
  expect_equal(
    attr(fam, "mvgam_default_cap", exact = TRUE), 25L
  )
  # PB and PPM have no scalar default_cap; they auto-compute
  # K_max[g] = max(y in g) + 100 via the buffer attr instead.
  expect_null(attr(nmix(),
                    "mvgam_default_cap", exact = TRUE))
  expect_null(attr(nmix("poisson_poisson"),
                    "mvgam_default_cap", exact = TRUE))
  expect_identical(
    attr(nmix(),
          "mvgam_default_cap_buffer", exact = TRUE), 100L
  )
  expect_identical(
    attr(nmix("poisson_poisson"),
          "mvgam_default_cap_buffer", exact = TRUE), 100L
  )
  expect_null(attr(nmix("royle_nichols"),
                    "mvgam_default_cap_buffer", exact = TRUE))
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

test_that("occ() default has no mvgam_unit_grouping attr", {
  expect_null(closure_unit_grouping(occ()))
})

test_that("occ(multi_season = TRUE) sets mvgam_unit_grouping = c('series','site','time')", {
  expect_identical(
    closure_unit_grouping(occ(multi_season = TRUE)),
    c("series", "site", "time")
  )
})

test_that("nmix() default has no mvgam_unit_grouping attr (all variants)", {
  expect_null(closure_unit_grouping(nmix()))
  expect_null(closure_unit_grouping(nmix("royle_nichols")))
  expect_null(closure_unit_grouping(nmix("poisson_poisson")))
})

test_that("nmix(multi_season = TRUE) sets the 3-axis grouping (all variants)", {
  expect_identical(
    closure_unit_grouping(nmix(multi_season = TRUE)),
    c("series", "site", "time")
  )
  expect_identical(
    closure_unit_grouping(nmix("royle_nichols", multi_season = TRUE)),
    c("series", "site", "time")
  )
  expect_identical(
    closure_unit_grouping(nmix("poisson_poisson", multi_season = TRUE)),
    c("series", "site", "time")
  )
})

test_that("prepare_closure_unit_family() routes multi_season grouping into standata N_unit", {
  # Build a 3-axis closure-unit dataset: 2 species x 3 sites x 2
  # seasons x 2 visits = 24 rows. Single-season grouping (series,
  # time) collapses across sites and yields 2 * 2 = 4 closure
  # units; multi-season grouping (series, site, time) yields the
  # full 2 * 3 * 2 = 12 units. Asserting the standata N_unit on
  # the same data, switching only the family arg, pins both
  # branches of prepare_closure_unit_family() in one test.
  d <- expand.grid(
    series = factor(c("sp1", "sp2")),
    site   = factor(seq_len(3L)),
    time   = seq_len(2L),
    visit  = seq_len(2L),
    KEEP.OUT.ATTRS = FALSE
  )
  d$y <- 0L
  mf <- mvgam_formula(y ~ 1)
  sd_single <- standata(mf, data = d, family = occ())
  sd_multi  <- standata(mf, data = d, family = occ(multi_season = TRUE))
  # Single-season: closure units = (series, time) = 2 * 2 = 4.
  expect_identical(as.integer(sd_single$N_unit), 4L)
  # Multi-season: closure units = (series, site, time) = 12.
  expect_identical(as.integer(sd_multi$N_unit), 12L)
})

test_that("occ(multi_season = TRUE) rejects data without a 'site' column", {
  d <- data.frame(
    series = factor(c("sp1", "sp1", "sp2", "sp2")),
    time   = c(1L, 2L, 1L, 2L),
    visit  = c(1L, 1L, 1L, 1L),
    y      = 0L
  )
  mf <- mvgam_formula(y ~ 1)
  expect_error(
    standata(mf, data = d, family = occ(multi_season = TRUE)),
    "'site'"
  )
})
