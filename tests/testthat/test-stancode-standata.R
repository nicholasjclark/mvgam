# stancode and standata Tests
#
# Integrated tests for stancode.mvgam_formula() and
# standata.mvgam_formula()

# Test Data Setup ----

#' Create standardized test datasets for stancode/standata function testing
#' @noRd
setup_stan_test_data <- function() {
  set.seed(42)
  n_time <- 24
  n_series <- 4

  # Simple univariate dataset
  univariate <- data.frame(
    time = 1:n_time,
    series = factor(rep("series1", n_time)),
    y = rpois(n_time, lambda = 5),
    x = rnorm(n_time),
    temperature = rnorm(n_time, mean = 15, sd = 3),
    site = factor(rep(c("A", "B", "C"), length.out = n_time))
  )

  # Multivariate dataset with balanced design. habitat is assigned
  # deterministically per series so it can be used as a hierarchical
  # grouping variable (constant within each series). Trend-level
  # covariates (x, presence) are generated per (time, habitat) so they
  # satisfy the trend invariance check used by hierarchical trends.
  series_levels <- paste0("series", 1:n_series)
  series_to_habitat <- setNames(
    rep(c("forest", "grassland"), length.out = n_series),
    series_levels
  )
  series_col <- factor(rep(series_levels, each = n_time),
                        levels = series_levels)
  habitat_col <- factor(series_to_habitat[as.character(series_col)],
                         levels = c("forest", "grassland"))
  th_grid <- expand.grid(
    time = seq_len(n_time),
    habitat = factor(c("forest", "grassland"))
  )
  th_grid$x <- rnorm(nrow(th_grid))
  th_grid$presence <- rbinom(nrow(th_grid), 1, 0.7)
  th_lookup <- setNames(
    seq_len(nrow(th_grid)),
    paste(th_grid$time, th_grid$habitat, sep = "_")
  )
  obs_keys <- paste(rep(seq_len(n_time), n_series),
                     as.character(habitat_col), sep = "_")
  obs_idx <- th_lookup[obs_keys]
  multivariate <- data.frame(
    time = rep(seq_len(n_time), n_series),
    series = series_col,
    count = rpois(n_time * n_series, lambda = 4),
    biomass = rlnorm(n_time * n_series, meanlog = 1, sdlog = 0.5),
    presence = th_grid$presence[obs_idx],
    x = th_grid$x[obs_idx],
    habitat = habitat_col
  )

  # Dataset with missing values
  with_missings <- univariate
  with_missings$y[c(3, 7, 15)] <- NA

  list(
    univariate = univariate,
    multivariate = multivariate,
    with_missings = with_missings
  )
}

# The trend programs the shape tests assert on, one per shape, each
# built once per run of this file. A shape test asserts what
# distinguishes its program. The skeleton every program shares is
# asserted once, by "every trend program carries the shared skeleton".
trend_shapes <- list(
  rw = list(resp = NULL, args = function() list(
    mvgam_formula(y ~ x, trend_formula = ~ RW()),
    data = setup_stan_test_data()$univariate, family = poisson()
  )),
  ar_seasonal = list(resp = NULL, args = function() list(
    mvgam_formula(y ~ x, trend_formula = ~ AR(p = c(1, 12))),
    data = setup_stan_test_data()$univariate, family = negbinomial()
  )),
  arma = list(resp = NULL, args = function() list(
    mvgam_formula(y ~ x, trend_formula = ~ AR(p = c(2, 4), ma = TRUE)),
    data = setup_stan_test_data()$univariate, family = poisson()
  )),
  varma = list(resp = c("count", "biomass"), args = function() list(
    mvgam_formula(
      bf(mvbind(count, biomass) ~ t2(x, time)) + set_rescor(FALSE),
      trend_formula = ~ presence + VAR(p = 2, ma = TRUE)
    ),
    data = setup_stan_test_data()$multivariate
  )),
  factor_ar = list(resp = c("count", "presence", "biomass"),
                   args = function() list(
    mvgam_formula(
      formula = bf(count ~ x, family = poisson()) +
        bf(presence ~ x, family = bernoulli()) +
        bf(biomass ~ x, family = Gamma(link = "log")),
      trend_formula = ~ -1 + AR(p = 1, n_lv = 2, cor = TRUE)
    ),
    data = setup_stan_test_data()$multivariate
  )),
  zmvn_factor = list(resp = NULL, args = function() list(
    mvgam_formula(biomass ~ 1, trend_formula = ~ x + ZMVN(n_lv = 2)),
    data = setup_stan_test_data()$multivariate, family = lognormal()
  )),
  hier_zmvn = list(resp = NULL, args = function() list(
    mvgam_formula(
      biomass ~ 1,
      trend_formula = ~ x + (x | habitat) + ZMVN(gr = habitat)
    ),
    data = setup_stan_test_data()$multivariate, family = lognormal(),
    prior = brms::prior("beta(5, 5)", class = "alpha_cor_trend")
  )),
  hier_var = list(resp = NULL, args = function() list(
    mvgam_formula(count ~ 1 + x,
                  trend_formula = ~ 1 + VAR(p = 1, gr = habitat)),
    data = setup_stan_test_data()$multivariate, family = poisson()
  )),
  hier_varma = list(resp = NULL, args = function() list(
    mvgam_formula(count ~ 1 + x,
                  trend_formula = ~ 1 + VAR(p = 1, ma = TRUE, gr = habitat)),
    data = setup_stan_test_data()$multivariate, family = poisson()
  )),
  # Irregular times on one series, which lets `CAR()` take trend
  # covariates.
  car = list(resp = NULL, args = function() {
    set.seed(42)
    n_time <- 20
    site <- rep(c("A", "B", "C"), length.out = n_time)
    data <- data.frame(
      time = cumsum(c(1, rexp(n_time - 1, rate = 0.8))),
      series = factor(rep("series1", n_time)),
      y = rpois(n_time, lambda = 3),
      income = ordered(sample(1:5, n_time, replace = TRUE)),
      site = factor(site),
      plot = factor(paste0(site, "_", rep(1:2, length.out = n_time)))
    )
    list(
      mvgam_formula(y ~ (1 | site) + (1 | plot),
                    trend_formula = ~ mo(income) + CAR()),
      data = data, family = poisson()
    )
  }),
  pw = list(resp = NULL, args = function() {
    data <- setup_stan_test_data()$univariate
    data$cap <- 16
    list(
      mvgam_formula(y ~ x, trend_formula = ~ PW(n_changepoints = 10)),
      data = data, family = poisson()
    )
  }),
  # `by = lv_axis()` runs the trend design on the factor grain
  by_lv = list(resp = NULL, grain = "lv", args = function() {
    # The factor grain takes one covariate value per time
    data <- setup_stan_test_data()$multivariate
    data$x <- sin(data$time)
    list(
      mvgam_formula(count ~ 1,
                    trend_formula = ~ s(x, by = lv_axis()) +
                      AR(p = 1, n_lv = 2)),
      data = data, family = poisson()
    )
  }),
  distributional = list(resp = NULL, args = function() list(
    mvgam_formula(bf(y ~ x, sigma ~ temperature), trend_formula = ~ RW()),
    data = setup_stan_test_data()$univariate, family = gaussian()
  )),
  shared_rw = list(resp = c("count", "biomass"), args = function() {
    data <- setup_stan_test_data()$multivariate
    data$log_baseline_count <- log(runif(nrow(data), min = 2, max = 5))
    data$log_baseline_biomass <- log(runif(nrow(data), min = 1, max = 3))
    list(
      mvgam_formula(
        bf(mvbind(count, biomass) ~ x + offset(log_baseline_count) +
             offset(log_baseline_biomass)) + set_rescor(FALSE),
        trend_formula = ~ RW(cor = TRUE)
      ),
      data = data
    )
  })
)

trend_shape_cache <- new.env()

# The program for one shape. `validate = TRUE` has stanc parse it,
# which also settles that its braces balance.
trend_shape_code <- function(name) {
  if (is.null(trend_shape_cache[[name]])) {
    trend_shape_cache[[name]] <- do.call(
      stancode, c(trend_shapes[[name]]$args(), list(validate = TRUE))
    )
  }
  trend_shape_cache[[name]]
}

# The statements every trend program declares, with the mapping
# arrays named per response on a multivariate program. The trend
# design runs per series, or per factor under `by = lv_axis()`, where
# each factor's mean joins its state inside the loadings product.
trend_skeleton_lines <- function(resp, grain = "series") {
  suffixes <- if (is.null(resp)) "" else paste0("_", resp)
  grain_lines <- if (identical(grain, "lv")) {
    c(
      "array[N_time_trend, N_lv_trend] int times_trend;",
      "lv_mu_trend[ : , k] += mu_trend[times_trend[ : , k]];",
      "trend = lv_mu_trend * Z';"
    )
  } else {
    c(
      "array[N_time_trend, N_series_trend] int times_trend;",
      "matrix[N_time_trend, N_series_trend] trend ="
    )
  }
  c(
    grain_lines,
    "int<lower=1> N_trend;",
    "int<lower=1> N_series_trend;",
    "int<lower=1> N_lv_trend;",
    "matrix[N_time_trend, N_lv_trend] lv_trend;",
    "real lprior = 0;",
    "target += lprior;",
    paste0("array[N", suffixes, "] int<lower=1, upper=N_time_trend> ",
           "obs_trend_time", suffixes, ";"),
    paste0("array[N", suffixes, "] int<lower=1, upper=N_series_trend> ",
           "obs_trend_series", suffixes, ";"),
    paste0("array[N", suffixes, "] int obs_trend_idx", suffixes, ";"),
    # A univariate predictor takes the trend at its own row's cell, in
    # a statement on `mu` or inside the intercept of a GLM call
    if (is.null(resp)) {
      "to_vector(trend)[obs_trend_idx]"
    }
  )
}

# stancode Tests ----

test_that("an RW program steps its states and drops the trend's sigma", {
  # The class of a program without a trend, which takes another route
  expect_identical(
    class(stancode(mvgam_formula(y ~ x), data = setup_stan_test_data()$univariate,
                   family = poisson(), validate = FALSE)),
    c("mvgamstancode", "stancode", "character")
  )

  code_with_trend <- trend_shape_code("rw")
  # `~ RW()` has no predictors and declares no trend design
  expect_false(grepl("X_trend", code_with_trend, fixed = TRUE))
  # The residual scale brms gives the trend submodel is not mvgam's
  expect_false(grepl("real<lower=0> sigma;", code_with_trend, fixed = TRUE))
  expect_true(stan_pattern("scaled_innovations_trend = diag_post_multiply(innovations_trend, sigma_trend);", code_with_trend, fixed = TRUE))
  expect_true(stan_pattern("lv_trend\\[1,\\s*:\\s*\\] = scaled_innovations_trend\\[1,\\s*:\\s*\\]", code_with_trend))
  expect_true(stan_pattern("lv_trend\\[i, : \\] = lv_trend\\[i - 1, : \\] \\+ scaled_innovations_trend\\[i, : \\]", code_with_trend))
})

test_that("stancode generates correct AR(p = c(1, 12)) seasonal model with negative binomial family", {
  code_with_trend <- trend_shape_code("ar_seasonal")

  # Each lag is declared. No lag between them is.
  expect_true(stan_pattern("vector<lower=-1,upper=1>\\[N_lv_trend\\] ar1_trend;", code_with_trend))
  expect_true(stan_pattern("vector<lower=-1,upper=1>\\[N_lv_trend\\] ar12_trend;", code_with_trend))
  expect_false(grepl("ar2_trend", code_with_trend, fixed = TRUE))

  # The first 12 states are innovations. The recursion starts at 13.
  expect_true(stan_pattern("for \\(i in 1:12\\)", code_with_trend))
  expect_true(stan_pattern("lv_trend\\[i, :\\] = scaled_innovations_trend\\[i, :\\];", code_with_trend))
  expect_true(stan_pattern("for \\(i in 13:N_time_trend\\)", code_with_trend))
  expect_true(stan_pattern(paste0(
    "lv_trend[i, j] = ar1_trend[j] * lv_trend[i - 1, j] ",
    "+ ar12_trend[j] * lv_trend[i - 12, j] ",
    "+ scaled_innovations_trend[i, j];"
  ), code_with_trend, fixed = TRUE))
  expect_true(stan_pattern("ar12_trend ~ normal", code_with_trend))
  expect_true(stan_pattern("neg_binomial_2_log_glm_lpmf", code_with_trend))
})

test_that("stancode generates correct AR(p = c(2, 4), ma = TRUE) ARMA model structure", {
  code_with_trend <- trend_shape_code("arma")

  # Lags 2 and 4 are declared. Lag 1 is absent.
  expect_true(stan_pattern("vector<lower=-1,upper=1>\\[N_lv_trend\\] ar2_trend;", code_with_trend))
  expect_true(stan_pattern("vector<lower=-1,upper=1>\\[N_lv_trend\\] ar4_trend;", code_with_trend))
  expect_false(grepl("ar1_trend", code_with_trend, fixed = TRUE))
  expect_true(stan_pattern("vector<lower=-1,upper=1>\\[N_lv_trend\\] theta1_trend;", code_with_trend))

  # MA innovations matrix should be created from scaled innovations
  expect_true(stan_pattern("matrix\\[N_time_trend, N_lv_trend\\] ma_innovations_trend = scaled_innovations_trend;", code_with_trend))

  # A first-order moving average weights the previous innovation, one
  # column-scaling product over every occasion. Weighting the previous
  # filtered value accumulates every earlier innovation geometrically,
  # which gives an autoregression.
  expect_true(stan_pattern(
    paste0("ma_innovations_trend[2:N_time_trend] += diag_post_multiply(",
           "scaled_innovations_trend[1:(N_time_trend - 1)], theta1_trend);"),
    code_with_trend, fixed = TRUE
  ))
  expect_false(grepl("ma_innovations_trend[1:", code_with_trend,
                     fixed = TRUE))

  # Initialization: First 4 time points should use MA innovations
  expect_true(stan_pattern("for \\(i in 1:4\\)", code_with_trend))
  expect_true(stan_pattern("lv_trend\\[i, :\\] = ma_innovations_trend\\[i, :\\];", code_with_trend))

  # AR dynamics: Should start from time point 5 and use ma_innovations_trend
  expect_true(stan_pattern("for \\(i in 5:N_time_trend\\)", code_with_trend))

  # The ARMA equation sums both AR lags and the MA innovation
  expect_true(stan_pattern(paste0(
    "lv_trend[i, j] = ar2_trend[j] * lv_trend[i - 2, j] ",
    "+ ar4_trend[j] * lv_trend[i - 4, j] + ma_innovations_trend[i, j];"
  ), code_with_trend, fixed = TRUE))

  expect_true(stan_pattern("theta1_trend ~ normal", code_with_trend))
})

test_that("a VARMA(2, 1) program maps its coefficients to stationarity", {
  code_with_trend <- trend_shape_code("varma")
  line <- function(x) stan_pattern(x, code_with_trend, fixed = TRUE)

  # The Heaps (2023) transform and its unconstrained inputs
  expect_true(line("matrix partial_autocorrelation(matrix P_real)"))
  expect_true(line(
    "array[N_lags_trend] matrix[N_lv_trend, N_lv_trend] A_trend;"
  ))
  expect_true(line("array[1] matrix[N_lv_trend, N_lv_trend] D_trend;"))
  expect_true(line("array[2] vector[N_lags_trend] Amu_trend;"))
  expect_true(line("array[2] vector<lower=0>[N_lags_trend] Aomega_trend;"))
  expect_true(line("array[2] vector[1] Dmu_trend;"))
  expect_true(line("array[2] vector<lower=0>[1] Domega_trend;"))
  expect_true(line("result_var = stationary_map(A_trend, Sigma_trend);"))
  expect_true(line("result_ma = stationary_map(D_trend, Sigma_trend);"))
  expect_true(line(paste0(
    "Omega_trend = varma_stationary_joint(varma_autocov(Sigma_trend,",
    " Phi_trend, Theta_trend[1]), Sigma_trend);"
  )))
  expect_true(stan_pattern("A_trend\\[.*\\] ~ ", code_with_trend))
  expect_true(stan_pattern("D_trend\\[.*\\] ~ ", code_with_trend))

  # The innovation scale and correlation, each declared once
  expect_identical(
    stan_match_count(code_with_trend,
                     "vector<lower=0>\\[N_lv_trend\\] sigma_trend;"), 1L)
  expect_identical(
    stan_match_count(code_with_trend,
                     "cholesky_factor_corr\\[N_lv_trend\\] L_Omega_trend;"),
    1L)
  expect_true(line(paste0(
    "cov_matrix[N_lv_trend] Sigma_trend = ",
    "multiply_lower_tri_self_transpose(L_Sigma_trend);"
  )))

  # `~ presence + VAR()` has no intercept. Its design enters mu_trend.
  expect_true(line("mu_trend += X_trend * b_trend;"))

  # The first p + 1 states come from the joint stationary law.
  # `Omega_trend` orders `init_trend` most recent first, which puts the
  # lag-i state at occasion t <= i in block i - t + 1. An index ignoring
  # t hands the likelihood one block for two occasions once p >= 2.
  expect_true(line(paste0(
    "target += multi_normal_cholesky_lpdf(init_trend | ",
    "mu_init_trend, cholesky_decompose(Omega_trend));"
  )))
  expect_true(line("int init_idx = i - t + 1;"))
  expect_true(line(paste0(
    "vector[N_lv_trend] ma_init_trend = init_trend[",
    "(N_lags_trend * N_lv_trend + 1):((N_lags_trend + 1) * N_lv_trend)];"
  )))

  # The MA term: the error at t - 1 is the state less its mean
  expect_true(line("mu_lv_trend[1] += (Theta_trend[1] * ma_init_trend)';"))
  expect_true(line(paste0(
    "mu_lv_trend[t] += (lv_trend[t - 1] - mu_lv_trend[t - 1])",
    " * Theta_trend[1]';"
  )))
  # Every state given its conditional mean, under one factorisation
  expect_true(line(
    "mdivide_left_tri_low(L_Sigma_trend, (lv_trend - mu_lv_trend)')"
  ))

  # Each response adds the trend through its own cell index, after
  # the smooths that build its predictor
  expect_true(line(
    "s_count_1_3;mu_count+=to_vector(trend)[obs_trend_idx_count];"
  ))
  expect_true(line(
    "s_biomass_1_3;mu_biomass+=to_vector(trend)[obs_trend_idx_biomass];"
  ))
})

test_that("a factor AR program fixes the factor scale and keeps its labels", {
  code_with_trend <- trend_shape_code("factor_ar")
  line <- function(x) stan_pattern(x, code_with_trend, fixed = TRUE)

  # Sampled loadings absorb the factors' scale and correlation, which
  # the program fixes at 1 and the identity. Unit-scale innovations
  # need no Cholesky product and carry no priors.
  expect_true(line(
    "vector<lower=0>[N_lv_trend] sigma_trend = rep_vector(1.0, N_lv_trend);"
  ))
  expect_true(line(paste0(
    "cholesky_factor_corr[N_lv_trend] L_Omega_trend = ",
    "identity_matrix(N_lv_trend);"
  )))
  expect_false(grepl("L_Sigma_trend", code_with_trend, fixed = TRUE))
  expect_false(stan_pattern("sigma_trend ~", code_with_trend))
  expect_false(stan_pattern("L_Omega_trend ~", code_with_trend))
  expect_true(stan_pattern("matrix\\[N_series_trend, N_lv_trend\\] Z;",
                           code_with_trend))
  expect_true(stan_pattern("to_vector\\(Z\\) ~ student_t\\(3, 0, 0.5\\);",
                           code_with_trend))
  expect_true(line(paste0(
    "lv_trend[i, j] = ar1_trend[j] * lv_trend[i - 1, j] ",
    "+ scaled_innovations_trend[i, j];"
  )))
  # Unit, uncorrelated factors start each at its own stationary law,
  # with no joint Cholesky factor to take
  expect_true(line(paste0(
    "lv_trend[1] = scaled_innovations_trend[1] ./ ",
    "sqrt(1 - square(ar1_trend))';"
  )))
  expect_false(grepl("cholesky_decompose", code_with_trend, fixed = TRUE))
  # AR factors take their own coefficients and are relabelled after
  # sampling. A rotation would mix them.
  expect_false(grepl("qr_thin", code_with_trend, fixed = TRUE))
  # `~ -1 + AR()` declares no trend design
  expect_false(grepl("X_trend", code_with_trend, fixed = TRUE))

  # The trend joins each GLM call's intercept. Biomass adds it before
  # its log link.
  expect_true(line(paste0(
    "target += poisson_log_glm_lpmf(Y_count | Xc_count, Intercept_count + ",
    "to_vector(trend)[obs_trend_idx_count], b_count);"
  )))
  expect_true(line(paste0(
    "target += bernoulli_logit_glm_lpmf(Y_presence | Xc_presence, ",
    "Intercept_presence + to_vector(trend)[obs_trend_idx_presence], ",
    "b_presence);"
  )))
  expect_true(line(paste0(
    "mu_biomass += to_vector(trend)[obs_trend_idx_biomass];",
    "mu_biomass = exp(mu_biomass);"
  )))
})

test_that("a ZMVN factor program rotates its loadings after sampling", {
  code_with_trend <- trend_shape_code("zmvn_factor")
  line <- function(x) stan_pattern(x, code_with_trend, fixed = TRUE)

  expect_true(line(
    "vector<lower=0>[N_lv_trend] sigma_trend = rep_vector(1.0, N_lv_trend);"
  ))
  expect_false(stan_pattern("sigma_trend ~", code_with_trend))
  expect_true(line("lv_trend = scaled_innovations_trend;"))
  # The covariate enters the trend design, which takes no centring
  expect_true(line("mu_trend += X_trend * b_trend;"))
  expect_false(grepl("Xc_trend", code_with_trend, fixed = TRUE))
  # Post-hoc QR identification in generated quantities
  expect_true(line(
    "matrix[N_series_trend, N_lv_trend] Z_tilde = qr_thin_R(Z')';"
  ))
  expect_true(line("lv_trend_tilde = lv_trend * Q';"))

  # `b_trend` keeps the flat brms default, so the program places no
  # prior on it in either spelling
  expect_length(stan_prior_on(code_with_trend, "b_trend"), 0L)
})

test_that("a grouped ZMVN program blends each group's correlations", {
  # The registry gives this shape a beta(5, 5) prior on
  # `alpha_cor_trend`
  code_with_trend <- trend_shape_code("hier_zmvn")
  line <- function(x) stan_pattern(x, code_with_trend, fixed = TRUE)

  expect_true(line("array[N_series_trend] int<lower=1> group_inds_trend;"))
  expect_true(line("matrix combine_cholesky("))
  expect_true(line(paste0(
    "L_Omega_group_trend[g_idx] = combine_cholesky(L_Omega_global_trend, ",
    "L_deviation_group_trend[g_idx], alpha_cor_trend);"
  )))
  expect_true(line(paste0(
    "L_group_trend[g_idx] = diag_pre_multiply(sigma_group_trend[g_idx], ",
    "L_Omega_group_trend[g_idx]);"
  )))
  expect_true(line(paste0(
    "Sigma_group_trend[g_idx] = ",
    "multiply_lower_tri_self_transpose(L_group_trend[g_idx]);"
  )))
  expect_identical(stan_prior_on(code_with_trend, "alpha_cor_trend"),
                   "beta(5, 5)")
  expect_identical(stan_prior_on(code_with_trend, "L_Omega_global_trend"),
                   "lkj_corr_cholesky(1)")
  expect_true(stan_pattern(paste0(
    "for \\(g_idx in 1:N_groups_trend\\) \\{",
    "target \\+= lkj_corr_cholesky_lpdf\\(",
    "L_deviation_group_trend\\[g_idx\\] \\| 6\\); \\}"
  ), code_with_trend))
  expect_true(line(
    "exponential_lpdf(to_vector(sigma_group_trend[g_idx]) | 2);"
  ))

  # The trend's group-level effect and its fixed effects both reach
  # mu_trend, even where brms hides the fixed effects inside a GLM call
  expect_true(line("r_1_trend = scale_r_cor(z_1_trend, sd_1_trend, L_1_trend);"))
  expect_true(line("mu_trend += X_trend * b_trend"))
  expect_length(stan_prior_on(code_with_trend, "b_trend"), 0L)
})

test_that("a grouped RW fills its innovations before stepping them", {
  # The RW generator takes the hierarchical support AR and ZMVN take.
  # Without it the RW recurrence uses an unassigned
  # `scaled_innovations_trend`, which gives NaN at initialisation.
  code <- stancode(mvgam_formula(count ~ 1, trend_formula = ~ RW(gr = habitat)),
                   data = setup_stan_test_data()$multivariate,
                   family = poisson(), validate = FALSE)
  # Each group's innovations take its own Cholesky factor at its member
  # columns. The flat branch is absent.
  expect_true(stan_pattern(
    paste0("scaled_innovations_trend[:, group_members_trend[g_idx]]",
           " = innovations_trend[:, group_members_trend[g_idx]]",
           " * L_group_trend[g_idx]';"),
    code, fixed = TRUE))
  expect_false(grepl("diag_post_multiply(innovations_trend", code,
                     fixed = TRUE))
  expect_true(stan_pattern(
    "lv_trend\\[i,\\s*:\\s*\\] = lv_trend\\[i - 1,\\s*:\\s*\\]\\s*\\+ scaled_innovations_trend\\[i,\\s*:\\s*\\]",
    code))
})

test_that("a grouped VAR places each group's blocks at its member series", {
  code <- trend_shape_code("hier_var")
  # Each group's lags map jointly to stationary coefficients and
  # autocovariances. Every block lands at the group's members, in the
  # ascending order the innovations and the stationary start take.
  expect_true(stan_pattern(
    "stationary_map(A_group_trend[g_idx], Sigma_group_trend[g_idx]);",
    code, fixed = TRUE
  ))
  for (line in c(
    "Sigma_trend[members, members] = Sigma_group_trend[g_idx];",
    "L_Sigma_trend[members, members] = L_group_trend[g_idx];",
    "Phi_trend[lag][members, members] = result_group[1, lag];",
    "Gamma_trend[lag][members, members] = result_group[2, lag];",
    "Omega_trend = var_stationary_joint(Gamma_trend);"
  )) {
    expect_true(stan_pattern(line, code, fixed = TRUE), label = line)
  }
  # The default priors of the grouped correlations and scales
  expect_true(stan_pattern("alpha_cor_trend ~ beta\\(3, 2\\);", code))
  expect_true(stan_pattern(paste0(
    "exponential_lpdf\\(to_vector\\(sigma_group_trend",
    "\\[g_idx\\]\\) \\| 2\\);"
  ), code))

  # A grouped VARMA maps each group's MA matrix against the group's
  # covariance. Each group's autocovariances come from its own solve.
  # Both land at the members, which keeps the groups apart in the MA
  # term and in the start.
  varma <- trend_shape_code("hier_varma")
  for (line in c(
    "stationary_map(D_group_trend[g_idx], Sigma_group_trend[g_idx]);",
    "Theta_trend[1][members, members] = -result_ma[1, 1];",
    paste0("varma_autocov(Sigma_group_trend[g_idx], result_group[1],",
           " -result_ma[1, 1]);"),
    "Gamma_trend[lag][members, members] = gamma_group[lag];",
    "Omega_trend = varma_stationary_joint(Gamma_trend, Sigma_trend);",
    "offdiag_elements(D_group_trend[g_idx, lag])"
  )) {
    expect_true(stan_pattern(line, varma, fixed = TRUE), label = line)
  }
  expect_false(grepl("D_trend", varma, fixed = TRUE))
})


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

# The declarations a trend's program writes in the legacy
# `int name[N];` form. Stan >= 2.32 (cmdstanr default) rejects it.
# `rstan::stanc()` accepts it with a deprecation warning, which lets a
# parse under rstan pass a program that fails at sample time. Comment
# lines (`//`) are left out of the sweep.
legacy_array_declarations <- function(trend_formulas) {
  data <- setup_stan_test_data()$multivariate
  legacy_decl <- paste0(
    "(int|real|matrix|vector|cov_matrix|corr_matrix|",
    "cholesky_factor_corr|cholesky_factor_cov|simplex|",
    "unit_vector|ordered|positive_ordered)\\s+",
    "[A-Za-z_][A-Za-z0-9_]*\\["
  )
  unlist(lapply(trend_formulas, function(tf) {
    mf <- mvgam_formula(count ~ 1 + x, trend_formula = tf)
    code <- as.character(stancode(mf, data = data, family = poisson(),
                                  validate = FALSE))
    lines <- strsplit(code, "\n", fixed = TRUE)[[1]]
    grep(legacy_decl, lines[!grepl("^\\s*//", lines)], value = TRUE)
  }))
}

test_that("grouped trend programs use modern Stan array syntax", {
  expect_identical(legacy_array_declarations(list(
    ~ 1 + VAR(p = 1, gr = habitat, cor = TRUE),
    ~ 1 + AR(p = 1, gr = habitat, cor = TRUE)
  )), character(0))
})

test_that("every other trend program uses modern Stan array syntax", {
  skip_on_cran()
  expect_identical(legacy_array_declarations(list(
    ~ 1 + VAR(p = 1, cor = TRUE),
    ~ 1 + AR(p = 1, cor = TRUE),
    ~ 1 + RW(cor = TRUE),
    ~ 1 + ZMVN()
  )), character(0))
})


test_that("user priors on array-shaped VAR hyperparameters emit per-lag", {
  # Guards user-prior routing through `generate_var_trend_stanvars()`:
  #   * `Amu_trend` / `Aomega_trend` are declared `array[2] vector[lags]`;
  #     a user override must be emitted inside the `for (lag in 1:2)` loop
  #     and must NOT also appear at top level via the centralized
  #     `var_centralized_priors` block (which would emit a scalar
  #     `Amu_trend ~ ...;` Stan rejects).
  #   * `L_Omega_global_trend` and `L_deviation_group_trend` must route
  #     any user override through `generate_hierarchical_correlation_model`
  #     rather than a hardcoded LKJ prior that would discard it.
  data <- setup_stan_test_data()$multivariate
  mf <- mvgam_formula(
    count ~ 1 + x,
    trend_formula = ~ VAR(p = 1, gr = habitat, cor = TRUE)
  )
  custom <- c(
    brms::prior(normal(0, 0.3), class = "Amu_trend"),
    brms::prior(gamma(2, 0.5), class = "Aomega_trend"),
    brms::prior(lkj_corr_cholesky(2), class = "L_Omega_global_trend"),
    brms::prior(lkj_corr_cholesky(4), class = "L_deviation_group_trend")
  )
  code <- as.character(stancode(
    mf, data = data, family = poisson(), prior = custom, validate = TRUE
  ))
  lines <- strsplit(code, "\n", fixed = TRUE)[[1]]
  code_only <- lines[!grepl("^\\s*//", lines)]

  amu_lines <- grep("Amu_trend\\[k\\]\\s*[~|]", code_only, value = TRUE)
  expect_equal(length(amu_lines), 1L)
  expect_match(amu_lines, "Amu_trend\\[k\\]\\s*[~|]\\s*(normal\\()?0,\\s*0.3")

  aomega_lines <- grep("Aomega_trend\\[k\\]\\s*[~|]", code_only, value = TRUE)
  expect_equal(length(aomega_lines), 1L)
  expect_match(aomega_lines, "Aomega_trend\\[k\\]\\s*[~|]\\s*(gamma\\()?2,\\s*0.5")

  l_global_lines <- grep("L_Omega_global_trend\\s*[~|]", code_only, value = TRUE)
  expect_equal(length(l_global_lines), 1L)
  expect_match(l_global_lines,
                 "lkj_corr_cholesky\\(2\\)|lkj_corr_cholesky_lpdf\\(.*\\| ?2\\)")

  l_dev_lines <- grep("L_deviation_group_trend\\[g_idx\\]\\s*[~|]",
                      code_only, value = TRUE)
  expect_equal(length(l_dev_lines), 1L)
  expect_match(l_dev_lines,
                 "lkj_corr_cholesky\\(4\\)|lkj_corr_cholesky_lpdf\\(.*\\| ?4\\)")
})


test_that("user priors on array-shaped VARMA MA hyperparameters emit", {
  # `Dmu_trend` / `Domega_trend` are declared `array[2]
  # vector[ma_lags]`; the user override has to reach the statement on
  # each array element. Compile is verified via `validate = TRUE`.
  data <- setup_stan_test_data()$multivariate
  mf <- mvgam_formula(
    count ~ 1 + x,
    trend_formula = ~ VAR(p = 1, ma = TRUE)
  )
  custom <- c(
    brms::prior(normal(0, 0.4), class = "Dmu_trend"),
    brms::prior(gamma(3, 0.75), class = "Domega_trend")
  )
  code <- as.character(stancode(
    mf, data = data, family = poisson(),
    prior = custom, validate = TRUE
  ))
  lines <- strsplit(code, "\n", fixed = TRUE)[[1]]
  code_only <- lines[!grepl("^\\s*//", lines)]

  # Either spelling: `Dmu_trend[k] ~ dist(...)` or the density call
  # `dist_lpdf(Dmu_trend[k] | ...)`.
  expect_match(code_only, "Dmu_trend\\[k\\].*0, ?0.4", all = FALSE)
  expect_match(code_only, "Domega_trend\\[k\\].*3, ?0.75", all = FALSE)
})


test_that("default priors on array-shaped VAR hyperparameters still emit", {
  data <- setup_stan_test_data()$multivariate
  mf <- mvgam_formula(
    count ~ 1 + x,
    trend_formula = ~ VAR(p = 1, gr = habitat, cor = TRUE)
  )
  code <- as.character(stancode(
    mf, data = data, family = poisson(), validate = TRUE
  ))
  lines <- strsplit(code, "\n", fixed = TRUE)[[1]]
  code_only <- lines[!grepl("^\\s*//", lines)]

  # Without overrides the package defaults still surface, once each.
  amu_lines <- grep("Amu_trend\\[k\\]\\s*[~|]", code_only, value = TRUE)
  expect_equal(length(amu_lines), 1L)
  expect_match(amu_lines, paste0(
    "Amu_trend\\[k\\]\\s*[~|]\\s*(normal\\()?0,",
    "\\s*sqrt\\(0.455\\)"
  ))

  aomega_lines <- grep("Aomega_trend\\[k\\]\\s*[~|]", code_only, value = TRUE)
  expect_equal(length(aomega_lines), 1L)
  expect_match(aomega_lines, paste0(
    "Aomega_trend\\[k\\]\\s*[~|]\\s*(gamma\\()?1.365,",
    "\\s*0.071175"
  ))

  l_global_lines <- grep("L_Omega_global_trend\\s*[~|]", code_only, value = TRUE)
  expect_equal(length(l_global_lines), 1L)
  expect_match(l_global_lines,
                 "lkj_corr_cholesky\\(1\\)|lkj_corr_cholesky_lpdf\\(.*\\| ?1\\)")

  l_dev_lines <- grep("L_deviation_group_trend\\[g_idx\\]\\s*[~|]",
                      code_only, value = TRUE)
  expect_equal(length(l_dev_lines), 1L)
  expect_match(l_dev_lines,
                 "lkj_corr_cholesky\\(6\\)|lkj_corr_cholesky_lpdf\\(.*\\| ?6\\)")
})


test_that("AR(p=1) latent state at t=1 uses stationary marginal init", {
  # The first state takes the AR(1) stationary marginal
  # Normal(0, sigma / sqrt(1 - ar1^2)). The t = 1 innovation alone
  # would give it the innovation variance.
  data <- setup_stan_test_data()$multivariate
  mf <- mvgam_formula(count ~ 1 + x, trend_formula = ~ AR(p = 1))
  code <- as.character(stancode(
    mf, data = data, family = poisson(), validate = TRUE
  ))
  expect_true(stan_pattern(
    "lv_trend[1] = scaled_innovations_trend[1] ./ sqrt(1 - square(ar1_trend))';",
    code, fixed = TRUE
  ))
})


test_that("a contiguous AR(p>1) starts at its stationary distribution", {
  # The first p states are drawn from their joint stationary
  # distribution by `ar_stationary_init()`, which builds them from
  # the partial autocorrelations through the Levinson-Durbin
  # recursion. The marginal variance is sigma^2 / prod(1 - pacf^2),
  # and conditioning on m earlier states multiplies it by
  # prod_{k<=m}(1 - pacf_k^2). The per-series divisor and the
  # correlated form each belong to a different program.
  data <- setup_stan_test_data()$multivariate
  mf <- mvgam_formula(count ~ 1 + x, trend_formula = ~ AR(p = 2))
  code <- as.character(stancode(
    mf, data = data, family = poisson(), validate = TRUE
  ))
  lines <- strsplit(code, "\n", fixed = TRUE)[[1]]
  expect_false(any(grepl(
    "sqrt\\(1\\s*-\\s*square\\(ar1_trend", lines
  )))
  expect_false(any(grepl("ar_diag_joint_var", lines, fixed = TRUE)))
  expect_true(any(grepl("lv_trend\\[1\\s*:\\s*2,\\s*j\\]", lines)))
  expect_true(any(grepl("ar_stationary_init\\(pacf_j", lines)))
})


test_that("an ARMA term starts at its stationary pair", {
  # A stationary ARMA(1, 1) has `cov(lv_1, eps_1) = Sigma`, which
  # makes the conditional mean of `lv_1` given `eps_1` the innovation
  # itself and the conditional standard deviation
  # `(ar1 + theta1) * sigma / sqrt(1 - ar1^2)`. `eps_1` reaches the
  # t = 2 step again through `ma_innovations_trend`, asserted by the
  # sparse-lag ARMA block earlier in this file. That reuse fixes
  # `eps_1` and leaves the spread needing variates of its own.
  data <- setup_stan_test_data()$multivariate
  mf <- mvgam_formula(count ~ 1 + x,
                      trend_formula = ~ AR(p = 1, ma = TRUE))
  code <- as.character(stancode(
    mf, data = data, family = poisson(), validate = TRUE
  ))
  expect_true(stan_pattern("vector[N_lv_trend] init_innovations_trend;",
                          code, fixed = TRUE))
  expect_true(stan_pattern("std_normal_lpdf(init_innovations_trend)",
                          code, fixed = TRUE))
  expect_true(stan_pattern(
    paste0("lv_trend[1]=scaled_innovations_trend[1]",
           "+((ar1_trend+theta1_trend).*sigma_trend",
           "./sqrt(1-square(ar1_trend)).*init_innovations_trend)';"),
    code, fixed = TRUE
  ))
})


# The starts below share `joint_init_stanblock()` and differ in the set
# of series one covariance spans. Each test pins the pieces its shape
# alone exercises. `tests/local/test-stationary-start-functions.R`
# checks the distribution the emitted design implies against a Neumann
# sum.
ar_start_code <- function(trend_formula) {
  data <- setup_stan_test_data()$multivariate
  as.character(stancode(
    mvgam_formula(count ~ 1 + x, trend_formula = trend_formula),
    data = data, family = poisson(), validate = TRUE
  ))
}


test_that("an ARMA above one lag filters a stationary AR(p) path", {
  # `lv_t = x_t + theta x_{t-1}` for an AR(p) path `x` whose states
  # `x_1, x_0` are drawn jointly. `x_0` takes the extra variate and
  # `x_2` steps from both with the innovation the t = 3 step reuses.
  code <- ar_start_code(~ AR(p = 2, ma = TRUE))
  expect_true(stan_pattern(
    "z_init[2:2] = init_innovations_trend[{j_ser}];", code, fixed = TRUE
  ))
  expect_true(stan_pattern(
    paste0("lv_trend[2, {j_ser}] = (scaled_innovations_trend[2, {j_ser}]'",
           " + phi_init[1] .* state_init[1:1]",
           " + phi_init[2] .* state_init[2:2]",
           " + theta1_trend[{j_ser}] .* state_init[1:1])';"),
    code, fixed = TRUE
  ))
  expect_true(stan_pattern(
    paste0("lv_trend[1, {j_ser}] = (state_init[1:1]",
           " + theta1_trend[{j_ser}] .* state_init[2:2])';"),
    code, fixed = TRUE
  ))
  # The diagonal coefficients let the pairwise solve replace the dense
  # solve of side `(p m)^2` a VARMA takes. The program leaves it out.
  expect_false(stan_pattern("varma_autocov(", code, fixed = TRUE))
})


test_that("a sparse lag set with a moving average keeps the raw start", {
  # The scope boundary. A sparse set bounds its coefficients one at a
  # time while the joint start needs joint stationarity. Such a set
  # keeps the raw start, and the extra variates are absent with it.
  code <- ar_start_code(~ AR(p = c(1, 3), ma = TRUE))
  expect_false(stan_pattern("init_innovations_trend", code, fixed = TRUE))
  expect_false(stan_pattern("state_init", code, fixed = TRUE))
  expect_true(stan_pattern("lv_trend[i, :] = ma_innovations_trend[i, :];",
                           code, fixed = TRUE))
})


test_that("a correlated AR(1) starts at the joint stationary covariance", {
  # The stationary covariance of a correlated AR(1) solves
  # Gamma = A Gamma A' + Sigma with A = diag(ar1), giving entries
  # Gamma[a, b] = Sigma[a, b] / (1 - ar1[a] * ar1[b]). The per-series
  # divisor `1 / sqrt(1 - ar1^2)` agrees with it on the diagonal alone
  # and belongs to the independent-innovation program.
  code <- ar_start_code(~ AR(p = 1, cor = TRUE))
  expect_true(stan_pattern(
    paste0("state_init = cholesky_decompose(Sigma_trend",
           " ./ (1 - phi_init[1] * phi_init[1]')) * z_init;"),
    code, fixed = TRUE
  ))
  expect_true(stan_pattern("lv_trend[1, :] = state_init[1:N_lv_trend]';",
                           code, fixed = TRUE))
  expect_false(stan_pattern("sqrt\\(1-square\\(ar1_trend", code))
})


test_that("a grouped ARMA(1, 1) conditions on its first innovation", {
  # At one lag the filter reduces to `lv_1 = eps_1 + (ar1 + theta1) x_0`
  # over the members of one group, with `x_0` at the group's AR(1)
  # stationary covariance. `members` is the ascending order that
  # `scaled_innovations_trend` fills, which keeps position k on one
  # series in both.
  code <- ar_start_code(~ AR(p = 1, gr = habitat, ma = TRUE))
  expect_true(stan_pattern(
    "array[N_subgroups_trend] int members = group_members_trend[g_idx];",
    code, fixed = TRUE
  ))
  expect_true(stan_pattern(
    paste0("state_init = cholesky_decompose(Sigma_group_trend[g_idx]",
           " ./ (1 - phi_init[1] * phi_init[1]')) * z_init;"),
    code, fixed = TRUE
  ))
  expect_true(stan_pattern(
    "z_init[1:N_subgroups_trend] = init_innovations_trend[members];",
    code, fixed = TRUE
  ))
  expect_true(stan_pattern(
    paste0("lv_trend[1, members] = (scaled_innovations_trend[1, members]'",
           " + phi_init[1] .* state_init[1:N_subgroups_trend]",
           " + theta1_trend[members] .* state_init[1:N_subgroups_trend])';"),
    code, fixed = TRUE
  ))
  # One lag needs no companion solve.
  expect_false(stan_pattern("ar_diag_joint_var", code, fixed = TRUE))
})


test_that("a correlated AR(p > 1) starts at its joint stationary covariance", {
  # A correlated AR(p > 1) has no closed form for its joint
  # stationary covariance. Its coefficients are diagonal, and
  # `ar_diag_joint_var()` solves each pair of series on its own.
  #
  # Block i of the drawn state is y_{t-i+1}, descending in time.
  # Block i fills `lv_trend[p - i + 1]` to match. Mapping block i
  # onto `lv_trend[i]` would fit the time-reversed process, whose
  # off-diagonal blocks are transposed.
  code <- ar_start_code(~ AR(p = 2, cor = TRUE))
  expect_true(stan_pattern("phi_init[2] = ar2_trend;", code, fixed = TRUE))
  expect_true(stan_pattern(
    "cholesky_decompose(ar_diag_joint_var(Sigma_trend, phi_init))",
    code, fixed = TRUE
  ))
  expect_true(stan_pattern("lv_trend[2, :] = state_init[1:N_lv_trend]';",
                           code, fixed = TRUE))
  expect_true(stan_pattern(
    "lv_trend[1, :] = state_init[N_lv_trend + 1:2 * N_lv_trend]';",
    code, fixed = TRUE
  ))
  # The dense solve of side `(p m)^2` a VARMA takes is left out. The
  # independent AR(p) form belongs to another program.
  expect_false(stan_pattern("varma_autocov(", code, fixed = TRUE))
  expect_false(stan_pattern("ar_stationary_init", code, fixed = TRUE))
})


test_that("a grouped AR(p > 1) starts at its group joint covariance", {
  # The same solve, taken over the series of one group, with
  # `Sigma_group_trend[g_idx]` as the innovation covariance.
  code <- ar_start_code(~ AR(p = 2, gr = habitat))
  expect_true(stan_pattern("phi_init[1] = ar1_trend[members];", code,
                           fixed = TRUE))
  expect_true(stan_pattern(
    "ar_diag_joint_var(Sigma_group_trend[g_idx], phi_init)",
    code, fixed = TRUE
  ))
  expect_true(stan_pattern(
    paste0("lv_trend[1, members] = state_init[N_subgroups_trend + 1:",
           "2 * N_subgroups_trend]';"),
    code, fixed = TRUE
  ))
})


test_that("threads + trend + brms-native family compiles serially with one warning", {
  # brms threading moves both the `mu` declaration and every
  # `mu += ...` / `mu[n] = ...` assignment into
  # `partial_log_lik_lpmf` inside `functions {}`; mvgam's obs-side
  # trend injector at R/stan_assembly.R:1346 / :1571 only searches
  # `model {}` and so cannot find the assignment it needs to splice
  # the trend addition into. The combination compiles serially and
  # warns on every call.
  data <- setup_stan_test_data()$multivariate
  mf <- mvgam_formula(count ~ 1 + x, trend_formula = ~ AR(p = 1))

  expect_warning(
    code <- as.character(stancode(
      mf, data = data, family = poisson(),
      threads = 2L, validate = FALSE
    )),
    "is not supported for a brms family"
  )
  expect_equal(
    sum(grepl("reduce_sum", strsplit(code, "\n", fixed = TRUE)[[1]])),
    0L
  )
})


test_that("silent = 2 keeps brms's announcements quiet", {
  # brms announces the `rescor` it settles for a multivariate formula
  # with `message()`. The mock fit and both regenerations run it, and
  # `silent = 2` turns messages off.
  data <- setup_stan_test_data()$multivariate
  mf <- mvgam_formula(
    bf(count ~ x, family = poisson()) + bf(presence ~ x, family = bernoulli()),
    trend_formula = ~ AR(p = 1)
  )
  expect_message(stancode(mf, data = data, silent = 1,
                          validate = FALSE), "rescor")
  expect_no_message(stancode(mf, data = data, silent = 2, validate = FALSE))
})


test_that("closure-unit + threads still emits mvgam reduce_sum, no brms wrapper", {
  # Regression guard for the second threading bug: brms's
  # `partial_log_lik_lpmf` cannot see the transformed-data args
  # mvgam's closure-unit lpmfs need (`visit_idx`, `log_n_lookup`),
  # so we must suppress brms's wrapper for closure-unit families
  # while preserving mvgam's own `partial_sum_<family>_lpmf` +
  # `reduce_sum` call. `cpp_options$stan_threads = TRUE` is enabled
  # by `mvgam_single()` on a separate code path, so the inner
  # `reduce_sum` still parallelises at fit time.
  set.seed(1)
  d <- data.frame(
    series = factor(rep(seq_len(6), each = 3)),
    time   = rep(1L, 18),
    visit  = rep(1:3, 6),
    y      = pmin(rpois(18, 5), 20L),
    cap    = rep(20L, 18),
    elev   = rep(rnorm(6), each = 3)
  )
  mf <- mvgam_formula(y ~ elev)
  code <- as.character(stancode(
    mf, data = d, family = nmix("poisson_binomial"),
    threads = 2L, validate = TRUE
  ))
  lines <- strsplit(code, "\n", fixed = TRUE)[[1]]
  expect_equal(sum(grepl("partial_log_lik_lpmf", lines)), 0L)
  expect_true(any(grepl("partial_sum_nmix_lpmf", lines)))
  expect_true(any(grepl(
    "reduce_sum\\(\\s*partial_sum_nmix_lpmf,", lines
  )))
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


test_that("a CAR trend with nested RE and mo() steps over its gaps", {
  code_with_trend <- trend_shape_code("car")

  # The monotonic effect enters the trend predictor through its simplex
  expect_true(stan_pattern(
    "mu_trend[n] += (bsp_trend[1]) * mo(simo_1_trend, Xmo_1_trend[n])",
    code_with_trend, fixed = TRUE
  ))
  expect_true(stan_pattern(
    "lprior += dirichlet_lpdf(simo_1_trend | con_simo_1_trend);",
    code_with_trend, fixed = TRUE
  ))

  # Both nested intercepts and the trend reach the observation
  # predictor
  expect_true(stan_pattern(
    "mu[n] += r_1_1[J_1[n]] * Z_1_1[n] + r_2_1[J_2[n]] * Z_2_1[n]",
    code_with_trend, fixed = TRUE
  ))
  expect_true(stan_pattern(
    "mu += to_vector(trend)[obs_trend_idx];",
    code_with_trend, fixed = TRUE
  ))

  # The first state at the stationary marginal of a unit gap
  expect_true(stan_pattern(
    "lv_trend[1] = scaled_innovations_trend[1] ./ sqrt(1 - square(ar1_trend))';",
    code_with_trend, fixed = TRUE
  ))

  # The gap scales the decay and the innovation together, the step
  # `car1_recursC()` takes in a forecast
  expect_true(stan_pattern("real gap = gap_trend[gap_index_trend[i]];",
                           code_with_trend, fixed = TRUE))
  expect_true(stan_pattern(paste0(
    "lv_trend[i, j] = pow(ar1_trend[j], gap) * lv_trend[i - 1, j] ",
    "+ scaled_innovations_trend[i, j] ",
    "* sqrt((1 - pow(ar1_trend[j], 2 * gap)) / (1 - square(ar1_trend[j])));"
  ), code_with_trend, fixed = TRUE))

  # Each CAR parameter carries its default prior
  expect_equal(stan_prior_on(code_with_trend, "ar1_trend"), "normal(0, 0.5)")
  expect_equal(stan_prior_on(code_with_trend, "sigma_trend"),
               "exponential(2)")
  expect_true(stan_pattern(
    stan_prior_line("to_vector(innovations_trend)", "std_normal()"),
    code_with_trend, fixed = TRUE
  ))

  # No correlation without `cor = TRUE`
  expect_false(grepl("L_Omega_trend", code_with_trend, fixed = TRUE))
  expect_false(grepl("Sigma_trend", code_with_trend, fixed = TRUE))
})

test_that("CAR(cor = TRUE) correlates the diffusion and scales its gaps", {
  times <- c(10, 14, 16, 26, 28, 30)
  d <- expand.grid(time = times, series = c("a", "b", "c"),
                   stringsAsFactors = FALSE)
  d$series <- factor(d$series)
  d$y <- seq_len(nrow(d)) %% 5
  mf <- mvgam_formula(y ~ 1, trend_formula = ~ CAR(cor = TRUE))
  code <- stancode(mf, data = d, family = poisson(), validate = FALSE)

  # The correlation is that of the shocks over an instant, and the
  # unit-gap covariance follows from it
  expect_true(stan_pattern("matrix car_unit_coherence\\(vector ar\\)", code))
  expect_true(stan_pattern("cholesky_factor_corr\\[N_lv_trend\\] L_Omega_trend;",
                           code))
  expect_true(stan_pattern(
    paste0("cov_matrix\\[N_lv_trend\\] Sigma_trend = ",
           "multiply_lower_tri_self_transpose\\(diag_pre_multiply\\(",
           "sigma_trend, L_Omega_trend\\)\\) \\.\\* ",
           "car_unit_coherence\\(ar1_trend\\);"),
    code
  ))
  expect_true(stan_pattern(
    "Gamma_trend \\.\\* -expm1\\(gap_trend\\[g\\] \\* log_ar_cross_trend\\)",
    code
  ))
  expect_true(stan_pattern(
    "lkj_corr_cholesky_lpdf\\(L_Omega_trend \\| 2\\)", code
  ))

  # Gaps are in units of the median gap, one entry per distinct gap
  sdata <- standata(mf, data = d, family = poisson(), validate = FALSE)
  expect_equal(as.numeric(sdata$gap_trend[sdata$gap_index_trend][-1L]),
               diff(times) / 2)
  expect_identical(as.integer(sdata$N_gaps_trend), 3L)
  # The same series in another time unit gives the same data
  d_scaled <- transform(d, time = time * 7.3)
  expect_equal(standata(mf, data = d_scaled, family = poisson(),
                        validate = FALSE)$gap_trend,
               sdata$gap_trend)

  prior_classes <- get_prior(mf, data = d, family = poisson())$class
  expect_true("L_Omega_trend" %in% prior_classes)
})

test_that("stancode handles different observation families", {
  # Each family's GLM likelihood takes the trend in its intercept
  data <- setup_stan_test_data()$univariate
  mf <- mvgam_formula(y ~ x, trend_formula = ~ RW())
  alpha <- "Xc, Intercept + to_vector(trend)[obs_trend_idx], b"
  cases <- list(
    list(family = poisson(), y = data$y,
         call = paste0("poisson_log_glm_lpmf(Y | ", alpha, ");")),
    list(family = gaussian(), y = rnorm(nrow(data)),
         call = paste0("normal_id_glm_lpdf(Y | ", alpha, ", sigma);")),
    list(family = bernoulli(), y = rbinom(nrow(data), size = 1, prob = 0.3),
         call = paste0("bernoulli_logit_glm_lpmf(Y | ", alpha, ");"))
  )
  for (case in cases) {
    test_data <- data
    test_data$y <- case$y
    code <- stancode(mf, data = test_data, family = case$family,
                     validate = FALSE)
    expect_true(stan_pattern(case$call, code, fixed = TRUE),
                label = case$family$family)
  }
})

test_that("the trend joins mu once, after brms builds it", {
  code <- stancode(mvgam_formula(y ~ s(x), trend_formula = ~ AR(p = 1)),
                   data = setup_stan_test_data()$univariate,
                   family = poisson(), validate = FALSE)
  code_lines <- strsplit(code, "\n", fixed = TRUE)[[1]]
  block_lines <- function(header) {
    start <- grep(paste0("^\\s*", header, "\\s*\\{"), code_lines)
    code_lines[start:mvgam:::find_matching_closing_brace(code_lines, start)]
  }
  model_lines <- block_lines("model")

  # In the model block, after the terms brms adds to mu and before the
  # likelihood statement. Nothing in transformed parameters takes it.
  gather <- "mu += to_vector(trend)[obs_trend_idx];"
  injection <- which(trimws(model_lines) == gather)
  expect_length(injection, 1L)
  expect_false(any(grepl("to_vector(trend)",
                         block_lines("transformed parameters"), fixed = TRUE)))
  builds <- grep("^\\s*mu\\s*\\+=", model_lines)
  expect_gt(injection, max(setdiff(builds, injection)))
  expect_lt(injection, min(grep("target\\s*\\+=.*_lp[dm]f\\s*\\(",
                                model_lines)))
})

test_that("a trend term is computed once per iteration", {
  # brms declares a group-level effect and a smooth coefficient in the
  # trend model's transformed parameters and assigns each below its
  # declaration, and mu_trend's construction needs the pair. A program
  # emitting the assignment twice parses and samples correctly, and
  # computes the same value a second time every iteration.
  data <- setup_stan_test_data()$univariate

  shapes <- list(
    list(trend = ~ (1 | site) + AR(p = 1), term = "r_1_1_trend"),
    list(trend = ~ s(x, k = 5) + AR(p = 1), term = "s_1_1_trend"),
    list(trend = ~ x + s(temperature, k = 5) + (1 | site) + AR(p = 1),
         term = "r_1_1_trend")
  )

  for (shape in shapes) {
    code <- stancode(
      mvgam_formula(y ~ 1, trend_formula = shape$trend),
      data = data, family = poisson(), validate = TRUE
    )
    statements <- gsub("\\s+", " ", trimws(
      strsplit(code, "\n", fixed = TRUE)[[1]]
    ))
    assignments <- statements[
      grepl("^[A-Za-z_][A-Za-z0-9_]*(\\[[^]]*\\])? \\+?=", statements)
    ]

    # Two blocks emitting one statement is what this catches, and
    # anyDuplicated names the first repeat rather than a count
    expect_identical(anyDuplicated(assignments), 0L)

    # A duplicate check passes when the term is dropped altogether, so
    # the term is also asserted present, exactly once
    assigned <- grepl(paste0("^", shape$term, " ="), statements)
    expect_identical(sum(assigned), 1L)
  }
})

test_that("an offset keeps the GLM's own coefficients, response by response", {
  # An offset makes brms pass its declared `mu` as the GLM intercept.
  # The trend adds to that vector and `X * b` stays in the call. The
  # call of a response without an offset takes the trend in its
  # intercept argument.
  uni_data <- setup_stan_test_data()$univariate
  uni_data$off <- log(runif(nrow(uni_data), 1, 3))
  uni <- stancode(
    mvgam_formula(y ~ x + offset(off), trend_formula = ~ RW()),
    data = uni_data, family = poisson(), validate = FALSE
  )
  expect_true(stan_pattern("poisson_log_glm_lpmf(Y | Xc, mu, b)", uni,
                           fixed = TRUE))

  data <- setup_stan_test_data()$multivariate
  data$off <- log(runif(nrow(data), 1, 3))
  mixed <- stancode(
    mvgam_formula(
      bf(count ~ x + offset(off), family = poisson()) +
        bf(biomass ~ x, family = gaussian()) + set_rescor(FALSE),
      trend_formula = ~ RW()
    ),
    data = data, validate = FALSE
  )
  expect_true(stan_pattern(
    "poisson_log_glm_lpmf(Y_count | Xc_count, mu_count, b_count)", mixed,
    fixed = TRUE
  ))
  expect_true(stan_pattern(paste0(
    "normal_id_glm_lpdf(Y_biomass | Xc_biomass, Intercept_biomass + ",
    "to_vector(trend)[obs_trend_idx_biomass], b_biomass, sigma_biomass)"
  ), mixed, fixed = TRUE))
})

test_that("a shared correlated RW enters every response with its offset", {
  code_shared <- trend_shape_code("shared_rw")

  # Identity loadings for a non-factor model
  expect_true(stan_pattern(
    "Z = diag_matrix(rep_vector(1.0, N_lv_trend));", code_shared,
    fixed = TRUE
  ))

  # `~ RW(cor = TRUE)` contributes no population-level term. The
  # program declares no intercept and no `mu_trend`.
  expect_false(grepl("mu_trend", code_shared, fixed = TRUE))
  expect_false(grepl("Intercept_trend", code_shared, fixed = TRUE))

  # One Cholesky factor gives both the reported covariance and the
  # scaled innovations
  expect_true(stan_pattern(
    "Sigma_trend = multiply_lower_tri_self_transpose(L_Sigma_trend);",
    code_shared, fixed = TRUE
  ))
  expect_true(stan_pattern(
    "scaled_innovations_trend = innovations_trend * L_Sigma_trend';",
    code_shared, fixed = TRUE
  ))

  # RW latent states from the correlated innovations
  expect_true(stan_pattern(
    "lv_trend\\[1,\\s*:\\s*\\] = scaled_innovations_trend\\[1,\\s*:\\s*\\]",
    code_shared
  ))
  expect_true(stan_pattern(
    paste0("lv_trend\\[i,\\s*:\\s*\\] = lv_trend\\[i - 1,\\s*:\\s*\\]",
           ".*\\+.*scaled_innovations_trend\\[i,\\s*:\\s*\\];"),
    code_shared
  ))

  # One trend matrix, which both responses index after their offsets
  expect_true(stan_pattern("mu_count += Intercept_count + offsets_count;",
                           code_shared, fixed = TRUE))
  expect_true(stan_pattern(
    "mu_biomass += Intercept_biomass + offsets_biomass;", code_shared,
    fixed = TRUE
  ))
  expect_true(stan_pattern(
    "mu_count += to_vector(trend)[obs_trend_idx_count];", code_shared,
    fixed = TRUE
  ))
  expect_true(stan_pattern(
    "mu_biomass += to_vector(trend)[obs_trend_idx_biomass];", code_shared,
    fixed = TRUE
  ))

  # brms passes each offset-carrying predictor as the GLM intercept,
  # and the call keeps its design matrix and coefficients. A call
  # rewritten to take `to_matrix(mu_count)` dropped `Xc_count * b_count`
  # and left `b_count` under its flat prior.
  expect_true(stan_pattern(
    "normal_id_glm_lpdf(Y_count | Xc_count, mu_count, b_count, sigma_count)",
    code_shared, fixed = TRUE
  ))
  expect_true(stan_pattern(
    paste0("normal_id_glm_lpdf(Y_biomass | Xc_biomass, mu_biomass, ",
           "b_biomass, sigma_biomass)"),
    code_shared, fixed = TRUE
  ))

  # Trend priors
  expect_equal(stan_prior_on(code_shared, "sigma_trend"), "exponential(2)")
  expect_true(stan_pattern("lkj_corr_cholesky_lpdf(L_Omega_trend",
                           code_shared, fixed = TRUE))
  expect_true(stan_pattern(
    stan_prior_line("to_vector(innovations_trend)", "std_normal()"),
    code_shared, fixed = TRUE
  ))
})

test_that("stancode integrates custom priors correctly", {
  data <- setup_stan_test_data()$univariate
  mf <- mvgam_formula(y ~ x, trend_formula = ~ AR(p = 1))

  # Custom priors using brms syntax
  custom_priors <- brms::prior("normal(0, 0.75)", class = "ar1_trend") +
                   brms::prior("exponential(7)", class = "sigma_trend")

  code_with_priors <- stancode(mf, data = data, family = poisson(),
                               prior = custom_priors, validate = FALSE)
  code_default <- stancode(mf, data = data, family = poisson(),
                           validate = FALSE)

  # Each custom prior reaches the parameter it names. Asserting the
  # distribution text alone would pass on a program that put it on
  # the wrong parameter, and would miss it entirely once the
  # statement is written as a density call rather than a tilde.
  expect_equal(stan_prior_on(code_with_priors, "ar1_trend"),
                 "normal(0, 0.75)")
  expect_equal(stan_prior_on(code_with_priors, "sigma_trend"),
                 "exponential(7)")
  # The defaults they displaced are gone.
  expect_equal(stan_prior_on(code_default, "ar1_trend"),
                 "normal(0, 0.5)")
  expect_equal(stan_prior_on(code_default, "sigma_trend"),
                 "exponential(2)")
})

test_that("stancode validates input parameters", {
  data <- setup_stan_test_data()$univariate

  # Invalid formula object
  expect_error(
    stancode("banana", data = data),
    "invalid formula.*not a call"
  )

  # Invalid data
  mf <- mvgam_formula(y ~ x)
  expect_error(
    stancode(mf, data = "not_a_dataframe", validate = FALSE)
  )

  # Missing data parameter
  expect_error(
    stancode(mf, family = poisson()),
    "data"
  )
})

# standata Tests ----

test_that("standata.mvgam_formula returns proper list structure", {
  data <- setup_stan_test_data()$univariate
  mf <- mvgam_formula(y ~ x, trend_formula = ~ RW())

  standata_result <- standata(mf, data = data, family = poisson(),
                              validate = FALSE)

  # The response, one entry per observation
  expect_identical(length(standata_result$Y), as.integer(standata_result$N))

  # Mapping arrays should have correct dimensions
  expect_equal(length(standata_result$obs_trend_time), standata_result$N)
  expect_equal(length(standata_result$obs_trend_series), standata_result$N)

  # The series index and the time index, row for row. A `>= 1` bound
  # passed on a permuted map, on a reversed one, and on one putting
  # every row on series 1.
  expect_identical(as.integer(standata_result$obs_trend_series),
                   as.integer(data$series))
  expect_identical(as.integer(standata_result$obs_trend_time),
                   match(data$time, sort(unique(data$time))))
})

test_that("standata processes missing data correctly", {
  data <- setup_stan_test_data()$with_missings
  mf <- mvgam_formula(y ~ x, trend_formula = ~ RW())

  # brms reports the rows it drops
  standata_result <- withCallingHandlers(
    standata(mf, data = data, family = poisson(), validate = FALSE),
    warning = function(w) {
      if (grepl("Rows containing NAs", conditionMessage(w))) {
        invokeRestart("muffleWarning")
      }
    }
  )

  # The three missing responses are dropped, and the mapping arrays
  # follow the observed rows
  expect_identical(as.integer(standata_result$N), sum(!is.na(data$y)))
  expect_equal(length(standata_result$obs_trend_time), standata_result$N)
  expect_equal(length(standata_result$obs_trend_series), standata_result$N)
})

test_that("standata sizes the trend on the data's own axes", {
  # Every time on one series, under each of three trends
  data <- setup_stan_test_data()$univariate
  for (tf in list(~ RW(), ~ AR(p = 1), ~ VAR(p = 1))) {
    sd <- standata(mvgam_formula(y ~ x, trend_formula = tf), data = data,
                   family = poisson(), validate = FALSE)
    lab <- deparse(tf)
    expect_identical(as.integer(sd$N_time_trend), nrow(data), label = lab)
    expect_identical(as.integer(sd$N_series_trend), 1L, label = lab)
    expect_identical(dim(sd$times_trend), c(nrow(data), 1L), label = lab)
  }
})

test_that("standata refuses irregular times for a discrete-time trend", {
  data <- setup_stan_test_data()$univariate
  data$time <- c(0, 1, 3, 4:nrow(data))
  expect_error(
    standata(mvgam_formula(y ~ x, trend_formula = ~ RW()), data = data,
             family = poisson(), validate = FALSE),
    "Gaps between times range from 1 to 2"
  )
})

# Integration Tests ----

test_that("stancode and standata are consistent", {
  # Every name the data block declares is a name the data supplies
  data <- setup_stan_test_data()$univariate
  mf <- mvgam_formula(y ~ s(x), trend_formula = ~ AR(p = 1))
  out <- mvgam_stan_setup(mf, data = data, family = poisson())

  block <- mvgam:::extract_stan_block_content(out$code, "data")
  decls <- sub(";.*$", "", trimws(strsplit(block, "\n", fixed = TRUE)[[1]]))
  decls <- decls[nzchar(decls) & !startsWith(decls, "//")]
  declared <- vapply(strsplit(decls, "[[:space:]]+"),
                     function(z) z[length(z)], character(1))
  expect_gt(length(declared), 0L)
  expect_identical(setdiff(declared, names(out$data)), character(0))
})

test_that("stan functions work with complex model specifications", {
  data <- setup_stan_test_data()$multivariate
  mf <- mvgam_formula(
    bf(mvbind(count, biomass) ~ s(x, by = habitat) + habitat) +
      set_rescor(FALSE),
    trend_formula = ~ AR(p = 1, cor = TRUE, n_lv = 2)
  )
  out <- mvgam_stan_setup(
    mf, data = data,
    prior = brms::prior("normal(0, 1)", class = "ar1_trend")
  )
  # The user's prior reaches the factor AR coefficients, and the data
  # carries the two factors asked for
  expect_identical(stan_prior_on(out$code, "ar1_trend"), "normal(0, 1)")
  expect_identical(as.integer(out$data$N_lv_trend), 2L)
})

# Edge Cases and Error Handling ----

test_that("stan functions handle edge cases gracefully", {
  # Three occasions are enough for a trend
  small_data <- data.frame(
    time = 1:3,
    series = factor(rep("s1", 3)),
    y = c(1, 2, 1),
    x = c(0.1, 0.2, 0.3)
  )
  mf <- mvgam_formula(y ~ 1, trend_formula = ~ RW())
  out <- mvgam_stan_setup(mf, data = small_data, family = poisson())
  expect_identical(as.integer(out$data$N), 3L)
  expect_identical(as.integer(out$data$N_time_trend), 3L)
})

test_that("stan functions provide informative error messages", {
  data <- setup_stan_test_data()$univariate
  bad_data <- data[, c("y", "x")]
  mf <- mvgam_formula(y ~ x, trend_formula = ~ RW())
  expect_error(stancode(mf, data = bad_data, family = poisson(),
                        validate = FALSE),
               "Column 'time' is absent from the data")
  expect_error(standata(mf, data = bad_data, family = poisson(),
                        validate = FALSE),
               "Column 'time' is absent from the data")
})

test_that("PW samples its growth on the span of the series", {
  # 24 time points
  code <- trend_shape_code("pw")
  line <- function(x) stan_pattern(x, code, fixed = TRUE)

  # Time runs in steps, and each parameter is sampled in units of its
  # default prior scale
  expect_true(line(paste0("vector[N_time_trend] time_trend = ",
                          "linspaced_vector(N_time_trend, 1, N_time_trend);")))
  expect_true(line(
    "vector<multiplier=rate_scale_trend>[N_lv_trend] k_trend;"
  ))
  expect_true(line(paste0("matrix<multiplier=delta_scale_trend>",
                          "[N_change_trend, N_lv_trend] delta_trend;")))
  expect_true(line(paste0("target += normal_lpdf(k_trend | 0, ",
                          signif(2 / 23, 4), ");")))
  expect_true(line(paste0(
    "target += double_exponential_lpdf(to_vector(delta_trend) | 0, ",
    signif(0.3 / 24, 4), ");"
  )))
  # The linear form takes its level from the observation formula
  expect_false(grepl("m_trend", code, fixed = TRUE))
  expect_false(grepl("logistic_trend", code, fixed = TRUE))

  # The user's changepoint scale sets the prior and the sampling unit
  pw_data <- setup_stan_test_data()$univariate
  scaled <- mvgam_formula(
    y ~ x, trend_formula = ~ PW(n_changepoints = 10, changepoint_scale = 0.05)
  )
  expect_identical(
    standata(scaled, data = pw_data, family = poisson(),
             validate = FALSE)$delta_scale_trend,
    signif(0.05 / 24, 4)
  )
  expect_true(stan_pattern(
    paste0("target += double_exponential_lpdf(to_vector(delta_trend) | 0, ",
           signif(0.05 / 24, 4), ");"),
    stancode(scaled, data = pw_data, family = poisson(),
             validate = FALSE), fixed = TRUE
  ))

  # The logistic inflection is centred on the middle of the series
  pw_data$cap <- 16
  logistic_code <- stancode(
    mvgam_formula(
      y ~ x, trend_formula = ~ PW(n_changepoints = 10, growth = "logistic")
    ),
    data = pw_data, family = poisson(), validate = FALSE
  )
  expect_true(stan_pattern(
    paste0("vector<offset=0.5 * (N_time_trend + 1), ",
           "multiplier=0.5 * N_time_trend>[N_lv_trend] m_trend;"),
    logistic_code, fixed = TRUE
  ))
  expect_true(stan_pattern("target += normal_lpdf(m_trend | 12.5, 12);",
                           logistic_code, fixed = TRUE))
  expect_false(grepl("linear_trend", logistic_code, fixed = TRUE))

  # Column `s` of the cap is series `s` of the trend. A factor that
  # keeps an unused level, as `subset()` leaves one, numbered the cap
  # columns by level and refused a frame with every cell supplied.
  two <- data.frame(
    y = rpois(40, 5), time = rep(1:20, 2),
    series = factor(rep(c("a", "b"), each = 20), levels = c("z", "a", "b")),
    cap = rep(c(30, 60), each = 20)
  )
  cap_sd <- standata(
    mvgam_formula(y ~ 1, trend_formula = ~ PW(growth = "logistic")),
    data = two, family = poisson(), validate = FALSE
  )
  expect_equal(exp(cap_sd$cap_trend[1, ]), c(30, 60))
})

test_that("in a distributional model the trend reaches mu alone", {
  # A single response with a distributional parameter is a univariate
  # model, with the unsuffixed mapping arrays
  code_distributional <- trend_shape_code("distributional")
  expect_false(grepl("obs_trend_time_y", code_distributional, fixed = TRUE))
  # The trend joins mu alone. Sigma is built from its own predictor.
  expect_identical(
    stan_match_count(code_distributional, "to_vector\\(trend\\)"), 1L
  )
  expect_true(stan_pattern("mu += to_vector(trend)[obs_trend_idx];",
                           code_distributional, fixed = TRUE))
  expect_true(stan_pattern("sigma \\+= Intercept_sigma \\+ Xc_sigma \\* b_sigma;", code_distributional))
  expect_true(stan_pattern("sigma = exp\\(sigma\\);", code_distributional))
})


test_that("every trend program carries the shared skeleton", {
  # Each shape test above asserts what distinguishes its program. The
  # statements below are the ones every program shares, whatever the
  # trend and whatever the observation family. A change to them fails
  # here, once per shape it reaches.
  for (nm in names(trend_shapes)) {
    code <- trend_shape_code(nm)
    expect_identical(
      class(code), c("mvgamstancode", "stancode", "character"),
      label = paste(nm, "class")
    )
    for (blk in c("functions", STAN_BLOCKS)) {
      expect_identical(stan_block_count(code, blk), 1L,
                       label = paste(nm, blk, "block count"))
    }
    shape <- trend_shapes[[nm]]
    for (line in trend_skeleton_lines(shape$resp,
                                      shape$grain %||% "series")) {
      expect_true(stan_pattern(line, code, fixed = TRUE),
                  label = paste(nm, line))
    }
  }
})

test_that("the trend assembly skips an identity Z and a zero mu_trend", {
  # The four series-grain cases. A factor model keeps the product with
  # its loadings. Trend formula terms keep the gather of `mu_trend`.
  cases <- list(
    rw = c(product = FALSE, gather = FALSE),
    varma = c(product = FALSE, gather = TRUE),
    factor_ar = c(product = TRUE, gather = FALSE),
    zmvn_factor = c(product = TRUE, gather = TRUE)
  )
  for (nm in names(cases)) {
    code <- trend_shape_code(nm)
    expect_identical(
      c(product = stan_pattern("trend = lv_trend * Z';", code, fixed = TRUE),
        gather = stan_pattern("trend[ : , s] += mu_trend[times_trend[ : , s]];",
                              code, fixed = TRUE)),
      cases[[nm]],
      label = nm
    )
  }
})

# Unsupported Family Validation Tests ----

test_that("multi-category families are blocked with informative error", {
  data <- setup_stan_test_data()$univariate

  # Create a simple formula for testing
mf <- mvgam_formula(y ~ x)

  # Categorical data take the mvgam wrapper `categ()`
  expect_error(
    stancode(mf, data = data, family = categorical(), validate = FALSE),
    regexp = "categorical.*not supported",
    ignore.case = TRUE
  )
  expect_error(stancode(mf, data = data, family = categorical(),
                        validate = FALSE), "categ()",
               fixed = TRUE)

  # Test multinomial family is blocked
  expect_error(
    stancode(mf, data = data, family = multinomial(), validate = FALSE),
    regexp = "multinomial.*not supported",
    ignore.case = TRUE
  )

  # Test dirichlet family is blocked
  expect_error(
    stancode(mf, data = data, family = dirichlet(), validate = FALSE),
    regexp = "dirichlet.*not supported",
    ignore.case = TRUE
  )

  # Test logistic_normal family is blocked
  expect_error(
    stancode(mf, data = data, family = logistic_normal(), validate = FALSE),
    regexp = "logistic_normal.*not supported",
    ignore.case = TRUE
  )
})

test_that("ordinal families remain supported", {
  data <- setup_stan_test_data()$univariate

  # Create ordered factor response for ordinal models
  data$y_ord <- ordered(cut(data$y, breaks = 4, labels = c("low", "med", "high", "vhigh")))
  mf <- mvgam_formula(y_ord ~ x)

  # Cumulative family should work (ordinal uses 2D linpred)
  expect_no_error(
    stancode(mf, data = data, family = cumulative(), validate = FALSE)
  )

  # Sequential ratio family should work
  expect_no_error(
    stancode(mf, data = data, family = sratio(), validate = FALSE)
  )

  # Continuation ratio family should work
  expect_no_error(
    stancode(mf, data = data, family = cratio(), validate = FALSE)
  )

  # Adjacent category family should work
  expect_no_error(
    stancode(mf, data = data, family = acat(), validate = FALSE)
  )
})

test_that("a hurdle likelihood loop takes the trend before it starts", {
  code <- stancode(mvgam_formula(y ~ x, trend_formula = ~ AR()),
                   data = setup_stan_test_data()$univariate,
                   family = hurdle_poisson(), validate = TRUE)
  flat <- gsub("\\s+", "", code)
  gather_at <- regexpr("mu+=to_vector(trend)[obs_trend_idx];", flat,
                       fixed = TRUE)
  expect_gt(gather_at, 0L)
  expect_lt(gather_at, regexpr("target+=hurdle_poisson_log_lpmf(", flat,
                               fixed = TRUE))
})

test_that("trend formula with covariate on multi-series univariate produces X_trend matching N_trend", {
  # Regression test for the N_trend / N_time_trend split (closes the
  # silent X_trend dim mismatch where trend_data had n_time * n_series
  # rows but N_trend was forced to n_time).
  set.seed(7)
  n_time <- 6
  n_series <- 4
  d <- data.frame(
    time = rep(seq_len(n_time), n_series),
    series = factor(rep(paste0("s", seq_len(n_series)), each = n_time)),
    x = stats::rnorm(n_time * n_series),
    y = stats::rpois(n_time * n_series, lambda = 5)
  )

  sd <- standata(
    mvgam_formula(y ~ 1, trend_formula = ~ x + AR(p = 1)),
    data = d, family = poisson(), validate = FALSE
  )

  # N_trend keeps brms's natural nrow(trend_data) so the trend-level
  # design matrix has one row per (time, series). N_time_trend is the
  # independent time axis used by the latent dynamics.
  expect_equal(sd$N_trend, n_time * n_series)
  expect_equal(sd$N_time_trend, n_time)
  expect_equal(sd$N_series_trend, n_series)

  # X_trend dims must match N_trend. Before the layout split this was
  # 20 vs 5, causing "dims declared=(5,1); dims found=(20,1)" at Stan
  # init.
  expect_equal(nrow(sd$X_trend), sd$N_trend)
  expect_equal(ncol(sd$X_trend), 1L)

  # times_trend maps each (time index i, series index s) to a unique
  # row of mu_trend; per-series case fills (i - 1) * n_series + s.
  expect_equal(dim(sd$times_trend), c(n_time, n_series))
  expect_equal(length(unique(as.integer(sd$times_trend))), sd$N_trend)
  expect_equal(sd$times_trend[1, ], seq_len(n_series))
  expect_equal(sd$times_trend[2, ], n_series + seq_len(n_series))

  # stanc accepts the assembled program
  expect_no_error(
    stancode(
      mvgam_formula(y ~ 1, trend_formula = ~ x + AR(p = 1)),
      data = d, family = poisson(), validate = TRUE
    )
  )
})

test_that("ZMVN(gr = X) without explicit subgr auto-fills subgr to series", {
  # The codegen path always treated `series` as the implicit subgr
  # when only `gr` was supplied; validate_grouping_arguments now
  # mirrors that by auto-filling subgr = "series". Smoke-checks the
  # resulting Stan code carries the hierarchical structure.
  data <- setup_stan_test_data()$multivariate
  mf <- mvgam_formula(
    biomass ~ 1,
    trend_formula = ~ x + (x | habitat) + ZMVN(gr = habitat)
  )
  sd <- standata(mf, data = data, family = lognormal(), validate = FALSE)
  # Four series in two habitats
  expect_identical(as.integer(sd$N_groups_trend), 2L)
  expect_identical(as.integer(sd$N_subgroups_trend), 2L)
})

test_that("subgr without gr is rejected with clear error", {
  # The auto-fill is one-directional: subgr cannot be supplied
  # without gr because we have no inferred main grouping variable.
  data <- setup_stan_test_data()$multivariate
  expect_error(
    standata(
      mvgam_formula(biomass ~ 1,
                    trend_formula = ~ ZMVN(subgr = habitat)),
      data = data, family = lognormal(), validate = FALSE
    ),
    regexp = "'subgr = habitat' requires a main grouping 'gr'"
  )
})


# ---- Fixed-Z (trend_map) Stan emission contract -------------------

test_that("a trend_map matrix moves Z into the data block", {
  # Dense user-supplied Z (4 series, 2 factors, custom weights).
  # Constructor-level path: the trend formula's environment is
  # captured so `Z_user` resolves at evaluation time.
  data <- setup_stan_test_data()$multivariate
  Z_user <- matrix(
    c(1.0, 0.0,
      0.7, 0.3,
      0.0, 1.0,
      0.5, 0.5),
    nrow = 4L, ncol = 2L, byrow = TRUE
  )
  mf <- mvgam_formula(
    count ~ x,
    trend_formula = ~ -1 + AR(p = 1, trend_map = Z_user, cor = TRUE)
  )
  out <- mvgam_stan_setup(mf, data = data, family = poisson(),
                           validate = FALSE)
  code <- out$code
  sd <- out$data

  # Z declared in the data block
  expect_true(stan_pattern("matrix\\[N_series_trend, N_lv_trend\\] Z;", code))

  # Standata carries the matrix verbatim.
  expect_equal(dim(sd$Z), c(4L, 2L))
  expect_equal(unname(sd$Z), unname(Z_user))

  # Innovations are still factor-sized (n_lv = 2 < n_series = 4).
  expect_true(stan_pattern(
    "matrix\\[N_time_trend, N_lv_trend\\] innovations_trend;",
    code
  ))
  expect_equal(sd$N_lv_trend, 2L)
})

test_that("trend_map 'shared' yields single-column Z in data", {
  data <- setup_stan_test_data()$multivariate
  mf <- mvgam_formula(
    count ~ 1,
    trend_formula = ~ -1 + RW(trend_map = "shared")
  )
  out <- mvgam_stan_setup(mf, data = data, family = poisson(),
                           validate = FALSE)
  code <- out$code
  sd <- out$data

  expect_true(stan_pattern("matrix\\[N_series_trend, N_lv_trend\\] Z;", code))
  expect_equal(sd$N_lv_trend, 1L)
  expect_equal(dim(sd$Z), c(4L, 1L))
  expect_true(all(sd$Z == 1))
})

test_that("trend_map data.frame builds binary Z aligned to series", {
  data <- setup_stan_test_data()$multivariate
  tm <- data.frame(
    series = paste0("series", 1:4),
    trend = c(1, 1, 2, 2)
  )
  mf <- mvgam_formula(
    count ~ 1,
    trend_formula = ~ -1 + AR(p = 1, trend_map = tm)
  )
  out <- mvgam_stan_setup(mf, data = data, family = poisson(),
                           validate = FALSE)
  code <- out$code
  sd <- out$data

  expected_Z <- matrix(c(1, 0,
                         1, 0,
                         0, 1,
                         0, 1),
                       nrow = 4L, ncol = 2L, byrow = TRUE)
  expect_equal(unname(sd$Z), expected_Z)
})

test_that("trend_map 'identity' emits Z as data (not tdata default)", {
  # Even when n_lv == n_series, an explicit trend_map keeps Z in
  # the data block, so the loadings every post-fit surface reads
  # are the ones the user wrote rather than a sampled rotation of
  # them.
  data <- setup_stan_test_data()$multivariate
  mf <- mvgam_formula(
    count ~ 1,
    trend_formula = ~ -1 + RW(trend_map = "identity")
  )
  out <- mvgam_stan_setup(mf, data = data, family = poisson(),
                           validate = FALSE)
  code <- out$code
  sd <- out$data

  expect_true(stan_pattern("matrix\\[N_series_trend, N_lv_trend\\] Z;", code))
  # No default diagonal Z assignment in tdata.
  expect_false(grepl(
    "Z = diag_matrix\\(rep_vector\\(1.0, N_lv_trend\\)\\)", code
  ))
  expect_equal(unname(sd$Z), diag(1, 4L))
})

test_that("trend_map conflict with explicit n_lv errors at fit time", {
  data <- setup_stan_test_data()$multivariate
  Z_user <- matrix(c(1, 0,
                     1, 1,
                     0, 1,
                     1, 0),
                   nrow = 4L, ncol = 2L, byrow = TRUE)
  mf <- mvgam_formula(
    count ~ 1,
    trend_formula = ~ -1 + AR(p = 1, n_lv = 3, trend_map = Z_user)
  )
  expect_error(
    stancode(mf, data = data, family = poisson(), validate = FALSE),
    "give different numbers of factors"
  )
})


# ---- Partial Z (NA = sampled) Stan emission contract ----------

test_that("trend_map with NA emits Z_template + Z_is_free + Z_free_vec", {
  # Partial Z: some entries fixed, some sampled (NA in matrix).
  data <- setup_stan_test_data()$multivariate
  Z_user <- matrix(
    c(1.0, 0.0,
      NA_real_, NA_real_,
      0.0, 1.0,
      NA_real_, NA_real_),
    nrow = 4L, ncol = 2L, byrow = TRUE
  )
  mf <- mvgam_formula(
    count ~ x,
    trend_formula = ~ -1 + AR(p = 1, trend_map = Z_user, cor = TRUE)
  )
  out <- mvgam_stan_setup(mf, data = data, family = poisson(),
                           validate = FALSE)
  code <- out$code
  sd <- out$data

  # Z_template in data with NAs replaced by 0.
  expect_true(stan_pattern(
    "matrix\\[N_series_trend, N_lv_trend\\] Z_template;", code
  ))
  # Z_is_free integer mask in data.
  expect_true(stan_pattern(
    paste0(
      "array\\[N_series_trend, N_lv_trend\\] ",
      "int<lower=0, upper=1> Z_is_free;"
    ),
    code
  ))
  # Free-entry vector parameter.
  expect_true(stan_pattern("vector\\[N_free_Z\\] Z_free_vec;", code))
  # Assembly loop in transformed parameters.
  expect_true(stan_pattern("Z\\[i, j\\] = Z_free_vec\\[idx\\];", code))
  expect_true(stan_pattern("Z\\[i, j\\] = Z_template\\[i, j\\];", code))
  # Prior on the free vector only, taken from the `Z` row of the
  # prior table. The statement is compared literally, since
  # `stan_pattern()` escapes a pattern and also accepts a tilde
  # spelling.
  expect_true(grepl(
    stan_prior_line("Z_free_vec", "student_t(3, 0, 0.5)"),
    paste(as.character(code), collapse = "\n"), fixed = TRUE
  ))
  # A user prior on class `Z` reaches the free entries
  code_user <- stancode(
    mf, data = data, family = poisson(), validate = FALSE,
    prior = brms::prior(normal(0, 2), class = "Z")
  )
  expect_true(grepl(
    stan_prior_line("Z_free_vec", "normal(0, 2)"),
    paste(as.character(code_user), collapse = "\n"), fixed = TRUE
  ))
  # User-supplied loadings bypass the QR identification path.
  expect_false(grepl("Z_tilde", code, fixed = TRUE))
  expect_false(grepl("qr_thin_R", code, fixed = TRUE))
  # Standata carries the template (NAs -> 0), mask, and count.
  expect_equal(dim(sd$Z_template), c(4L, 2L))
  expect_equal(sum(sd$Z_template == 0), 6L)  # 4 NAs + 2 fixed-0
  expect_equal(dim(sd$Z_is_free), c(4L, 2L))
  expect_equal(sd$N_free_Z, 4L)
})

test_that("fully-fixed Z is preserved (no partial-Z stanvars emitted)", {
  # When trend_map has no NAs, the fully-fixed code path stays in
  # effect, emitting no Z_template or Z_free_vec.
  data <- setup_stan_test_data()$multivariate
  Z_user <- matrix(c(1, 0, 0.5, 0.5, 0, 1, 0.3, 0.7),
                    nrow = 4L, ncol = 2L, byrow = TRUE)
  mf <- mvgam_formula(
    count ~ 1,
    trend_formula = ~ -1 + AR(p = 1, trend_map = Z_user)
  )
  code <- stancode(mf, data = data, family = poisson(),
                   validate = FALSE)
  expect_false(grepl("Z_template", code, fixed = TRUE))
  expect_false(grepl("Z_is_free", code, fixed = TRUE))
  expect_false(grepl("Z_free_vec", code, fixed = TRUE))
  # User-supplied loadings bypass the QR identification path.
  expect_false(grepl("Z_tilde", code, fixed = TRUE))
  expect_false(grepl("qr_thin_R", code, fixed = TRUE))
})


# Helper: 4-series factor-model fixture used by the
# loadings_prior contract tests below. Returns a list with the
# fixed pieces (data, mvgam_formula, features, distance matrix)
# every contract test re-uses.
loadings_prior_fixture <- function() {
  set.seed(1L)
  n_series <- 4L
  series_levels <- paste0("s", seq_len(n_series))
  data <- data.frame(
    series = factor(rep(series_levels, each = 10L)),
    time = rep(1:10, n_series),
    y = rpois(40L, 2)
  )
  features <- data.frame(
    series = series_levels,
    trait = rnorm(n_series)
  )
  d_phylo <- as.matrix(stats::dist(rnorm(n_series)))
  mf <- mvgam_formula(
    y ~ 1, trend_formula = ~ AR(p = 1, n_lv = 2)
  )
  list(
    data = data, features = features, d_phylo = d_phylo, mf = mf,
    series_levels = series_levels
  )
}


test_that("loadings_prior with features only emits ARD prior on Z", {
  fx <- loadings_prior_fixture()
  code <- stancode(
    fx$mf, data = fx$data, family = poisson(),
    data2 = list(features = fx$features),
    loadings_prior = list(features = "features")
  )
  sc <- as.character(code)
  expect_match(sc, "int<lower=1>\\s+N_features_trend\\s*;")
  expect_match(
    sc,
    "array\\[N_features_trend\\]\\s+real<lower=0>\\s+theta_features\\s*;"
  )
  expect_match(sc, "gp_exponential_cov", fixed = TRUE)
  expect_match(sc, "cholesky_decompose", fixed = TRUE)
  expect_match(sc, "multi_normal_cholesky", fixed = TRUE)
  # The length-scale is a settable prior, so it is written as a
  # sampling statement like every other one mvgam emits.
  expect_true(stan_pattern("theta_features ~ lognormal(0, 1)", sc))
  # The structured prior replaces the default iid one, so nothing
  # student_t reaches `Z`. The default program carries that row, so
  # the emptiness here is the structured prior displacing it rather
  # than a pattern that no longer matches anything.
  expect_length(stan_prior_on(sc, "Z"), 0L)
})


test_that("loadings_prior with distances only emits exponential decay prior", {
  fx <- loadings_prior_fixture()
  code <- stancode(
    fx$mf, data = fx$data, family = poisson(),
    data2 = list(phylo = fx$d_phylo),
    loadings_prior = list(distances = "phylo")
  )
  sc <- as.character(code)
  expect_match(
    sc,
    "matrix<lower=0>\\[N_series_trend,\\s*N_series_trend\\]\\s+dist_phylo\\s*;"
  )
  expect_match(sc, "real<lower=0>\\s+theta_dist_phylo\\s*;")
  expect_match(sc, "exp\\(-dist_phylo")
  expect_false(grepl("gp_exponential_cov", sc, fixed = TRUE))
  expect_false(grepl("row_features", sc, fixed = TRUE))
})


test_that("loadings_prior combines features and distances multiplicatively, threads standata", {
  fx <- loadings_prior_fixture()
  out <- mvgam_stan_setup(
    fx$mf, data = fx$data, family = poisson(),
    data2 = list(features = fx$features, phylo = fx$d_phylo),
    loadings_prior = list(
      features = "features", distances = "phylo"
    )
  )
  sc <- as.character(out$code)
  expect_match(sc, "gp_exponential_cov", fixed = TRUE)
  expect_match(sc, "dist_phylo", fixed = TRUE)
  # Standata carries both feature and distance threads.
  expect_equal(out$data$N_features_trend, 1L)
  expect_equal(dim(out$data$row_features), c(4L, 1L))
  expect_equal(dim(out$data$dist_phylo), c(4L, 4L))
  expect_equal(max(out$data$dist_phylo), 1)
})


test_that("loadings_prior with column_shrinkage = 'mgp' emits MGP machinery", {
  fx <- loadings_prior_fixture()
  code <- stancode(
    fx$mf, data = fx$data, family = poisson(),
    data2 = list(phylo = fx$d_phylo),
    loadings_prior = list(
      distances = "phylo",
      column_shrinkage = "mgp",
      mgp_a1 = 2, mgp_a2 = 6
    )
  )
  sc <- as.character(code)
  expect_match(sc, "varrho_inv", fixed = TRUE)
  expect_match(sc, "Psi_diag", fixed = TRUE)
  expect_match(sc, "inv_gamma\\(mgp_a1|inv_gamma_lpdf\\([^|]*\\| ?mgp_a1")
  expect_match(sc, "sqrt\\(Psi_diag")
})


test_that("default factor model still emits the iid student_t default", {
  fx <- loadings_prior_fixture()
  code <- stancode(fx$mf, data = fx$data, family = poisson())
  sc <- as.character(code)
  expect_match(sc, paste0("to_vector\\(Z\\)\\s*~\\s*student_t",
                            "|student_t_lpdf\\(to_vector\\(Z\\)"))
  expect_false(grepl("Phi_loadings", sc, fixed = TRUE))
  expect_false(grepl("gp_exponential_cov", sc, fixed = TRUE))
})


test_that("loadings_prior errors when combined with a partial trend_map", {
  fx <- loadings_prior_fixture()
  # Each row carries at least one finite entry and at least one
  # NA so the partial-Z validator accepts the matrix and the
  # loadings-prior compatibility check is what fires.
  Z_partial <- matrix(c(1, NA, NA, 1, 1, NA, NA, 1),
                       nrow = 4L, ncol = 2L, byrow = TRUE)
  mf <- mvgam_formula(
    y ~ 1,
    trend_formula = ~ AR(p = 1, trend_map = Z_partial)
  )
  expect_error(
    stancode(
      mf, data = fx$data, family = poisson(),
      data2 = list(phylo = fx$d_phylo),
      loadings_prior = list(distances = "phylo"), validate = FALSE
    ),
    "fixes 4 of 8 entries"
  )
})


test_that("loadings_prior errors when combined with a fully-fixed trend_map", {
  fx <- loadings_prior_fixture()
  Z_fixed <- matrix(c(1, 0, 0.5, 0.5, 0, 1, 0.3, 0.7),
                     nrow = 4L, ncol = 2L, byrow = TRUE)
  mf <- mvgam_formula(
    y ~ 1,
    trend_formula = ~ AR(p = 1, trend_map = Z_fixed)
  )
  expect_error(
    stancode(
      mf, data = fx$data, family = poisson(),
      data2 = list(phylo = fx$d_phylo),
      loadings_prior = list(distances = "phylo"), validate = FALSE
    ),
    "fixes 8 of 8 entries"
  )
})


# The trend classes `get_prior()` offers that the program never
# declares, one entry per offending spec. A prior set on such a class
# reaches nothing. The user took the class name from the table.
undeclared_trend_offers <- function(specs) {
  data <- setup_stan_test_data()$multivariate
  declared <- function(code) {
    lines <- strsplit(code, "\n", fixed = TRUE)[[1]]
    i0 <- grep("^parameters \\{", lines)
    i1 <- grep("^transformed parameters \\{|^model \\{", lines)
    i1 <- i1[i1 > i0][1L]
    ids <- sub(";.*$", "", trimws(lines[(i0 + 1L):(i1 - 1L)]))
    ids <- vapply(strsplit(ids, "[[:space:]]+"), function(z) {
      if (length(z) == 0L) "" else z[length(z)]
    }, character(1))
    ids[nzchar(ids) & !grepl("[{}]", ids)]
  }
  offenders <- character(0)
  for (tf in specs) {
    mf <- mvgam_formula(count ~ 1 + x, trend_formula = tf)
    code <- as.character(stancode(mf, data = data, family = poisson(),
                                  validate = FALSE))
    gp <- get_prior(mf, data = data, family = poisson())
    offered <- unique(gp$class[grepl("_trend$|^Z$", gp$class)])
    absent <- setdiff(offered, declared(code))
    if (length(absent)) {
      offenders <- c(offenders, paste0(
        deparse(tf[[2L]]), ": ", paste(absent, collapse = ", ")
      ))
    }
  }
  offenders
}

test_that("get_prior offers only trend classes the program declares", {
  # Grouping is where the two drift apart: the generated program
  # swaps `sigma_trend` and `L_Omega_trend` for `sigma_group_trend`
  # with the global and deviation blocks. A fixed `trend_map` hands
  # the loadings to Stan as data, where a prior on that class would
  # reach nothing.
  Z_fixed <- matrix(c(1, 0, 0, 1, 1, 1, 0, 1),
                    nrow = 4L, ncol = 2L, byrow = TRUE)
  rownames(Z_fixed) <- paste0("series", 1:4)
  expect_identical(undeclared_trend_offers(list(
    ~ 1 + AR(p = 1, gr = habitat),
    ~ 1 + ZMVN(gr = habitat),
    ~ 1 + AR(p = 1, gr = habitat, df = NA),
    ~ 1 + AR(p = 1, n_lv = 2),
    ~ 1 + AR(p = 1, trend_map = Z_fixed)
  )), character(0))
})

test_that("get_prior offers only declared classes on every other trend", {
  skip_on_cran()
  expect_identical(undeclared_trend_offers(list(
    ~ 1 + AR(p = 1),
    ~ 1 + AR(p = 1, cor = TRUE),
    ~ 1 + AR(p = 2, coef_sharing = "shared"),
    ~ 1 + AR(p = 2, coef_sharing = "hierarchical"),
    ~ 1 + RW(gr = habitat),
    ~ 1 + AR(p = 1, df = NA)
  )), character(0))
})


# ---- AR() prior + coef_sharing surface (Step 3) -------------------

# Shared fixture for the four valid combinations + the
# constructor-side rejections. Small panel because we only
# inspect the emitted stancode, not the posterior.
ar_step3_fixture <- function() {
  set.seed(1L)
  data.frame(
    y = rnorm(80L),
    time = rep(1:20L, 4L),
    series = factor(rep(paste0("s", 1:4), each = 20L))
  )
}

ar_step3_stancode <- function(tf) {
  dat <- ar_step3_fixture()
  mf <- mvgam_formula(y ~ 1, trend_formula = tf)
  paste(unlist(stancode(mf, data = dat, validate = FALSE)), collapse = "\n")
}

# brms reformats `<lower=a,upper=b>` to `<lower=a, upper=b>`
# in its emitted Stan source. The regex patterns below use
# `\\s*` between comma-separated arguments so they survive
# either spacing.

test_that("AR(p = 2, none) samples partial autocorrelations", {
  sc <- ar_step3_stancode(~ AR(p = 2))
  # The sampled quantity carries the stationarity bound. Any pair
  # of partial autocorrelations in (-1, 1) gives a stationary
  # coefficient pair.
  expect_true(grepl(
    "vector<lower=-1,\\s*upper=1>\\[N_lv_trend\\] ar1_pacf_trend;", sc
  ))
  expect_true(grepl(
    "vector<lower=-1,\\s*upper=1>\\[N_lv_trend\\] ar2_pacf_trend;", sc
  ))
  # `ar{lag}_trend` holds the coefficient under either
  # parameterisation, which keeps every post-fit reader on one
  # quantity.
  expect_true(grepl("vector\\[N_lv_trend\\] ar1_trend;", sc))
  expect_true(grepl("vector\\[N_lv_trend\\] ar2_trend;", sc))
  expect_true(grepl("ar_pacf_to_coef", sc, fixed = TRUE))
  expect_false(grepl("shared_ar1", sc))
  expect_false(grepl("mu_ar1_trend\\b", sc))
})

test_that("AR(p = 1) and a sparse lag set keep the plain coefficient", {
  # One lag needs no recursion: the declared interval is already
  # the stationary region. A sparse lag set fixes its intermediate
  # coefficients at zero, a constraint the recursion cannot state.
  for (tf in list(~ AR(p = 1), ~ AR(p = c(2, 4)))) {
    expect_false(grepl("pacf", ar_step3_stancode(tf), fixed = TRUE))
  }
  expect_true(grepl(
    "vector<lower=-1,\\s*upper=1>\\[N_lv_trend\\] ar1_trend;",
    ar_step3_stancode(~ AR(p = 1))
  ))
  expect_true(grepl(
    "vector<lower=-1,\\s*upper=1>\\[N_lv_trend\\] ar4_trend;",
    ar_step3_stancode(~ AR(p = c(2, 4)))
  ))
})

test_that("AR(default + shared) emits one partial autocorrelation per lag", {
  sc <- ar_step3_stancode(~ AR(p = 2, coef_sharing = "shared"))
  # The `_trend` suffix places the sampled scalar on the trend side
  # for `is_trend_parameter()`, which the parameter taxonomy and
  # `tidy()` both consult.
  expect_true(grepl(
    "vector<lower=-1,\\s*upper=1>\\[1\\] shared_ar1_pacf_trend;", sc
  ))
  expect_true(grepl(
    "vector<lower=-1,\\s*upper=1>\\[1\\] shared_ar2_pacf_trend;", sc
  ))
  expect_true(grepl(
    paste0("ar1_pacf_trend = rep_vector\\(shared_ar1_pacf_trend\\[1\\],",
           "\\s*N_lv_trend\\);"), sc
  ))
  expect_true(grepl(
    stan_prior_line("shared_ar1_pacf_trend", "normal(0, 0.5)"), sc,
    fixed = TRUE
  ))
  expect_false(grepl(
    "vector<lower=-1,\\s*upper=1>\\[N_lv_trend\\] ar1_trend;", sc
  ))
})

test_that("AR(default + hierarchical) pools the partial autocorrelation", {
  sc <- ar_step3_stancode(~ AR(p = 2, coef_sharing = "hierarchical"))
  expect_true(grepl(
    "real<lower=-1,\\s*upper=1> mu_ar1_pacf_trend;", sc
  ))
  expect_true(grepl("real<lower=0> sigma_ar1_pacf_trend;", sc))
  expect_true(grepl(
    "vector<lower=-1,\\s*upper=1>\\[N_lv_trend\\] ar1_pacf_trend;", sc
  ))
  expect_true(grepl(
    stan_prior_line("mu_ar1_pacf_trend", "normal(0, 0.5)"), sc,
    fixed = TRUE
  ))
  expect_true(grepl(
    stan_prior_line("sigma_ar1_pacf_trend", "exponential(2)"),
    sc, fixed = TRUE
  ))
  expect_true(grepl(
    stan_prior_line("ar1_pacf_trend",
                      "normal(mu_ar1_pacf_trend, sigma_ar1_pacf_trend)"),
    sc, fixed = TRUE
  ))
  # The population density is truncated to the bounds of each series'
  # partial autocorrelation. Its normaliser moves with the
  # hyperparameters, and leaving it out biases them.
  expect_true(stan_pattern(
    paste0("target += -N_lv_trend * log_diff_exp(normal_lcdf(1 | ",
           "mu_ar1_pacf_trend, sigma_ar1_pacf_trend), normal_lcdf(-1 | ",
           "mu_ar1_pacf_trend, sigma_ar1_pacf_trend));"),
    sc, fixed = TRUE
  ))
  # Pooling the partial autocorrelation keeps the derived
  # coefficient stationary for every group.
  expect_true(grepl("vector\\[N_lv_trend\\] ar1_trend;", sc))
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

test_that("make_stancode / make_standata dispatch on mvgam_formula without brms::", {
  dat <- ar_step3_fixture()
  mf <- mvgam_formula(y ~ 1, trend_formula = ~ AR(p = 2))
  sc <- make_stancode(mf, data = dat)
  expect_s3_class(sc, "mvgamstancode")
  expect_true(grepl("ar1_trend", as.character(sc)))

  sd <- make_standata(mf, data = dat)
  expect_true(all(c("N", "Y", "N_trend") %in% names(sd)))
})

test_that("default_prior() and get_prior() agree on an mvgam_formula", {
  dat <- ar_step3_fixture()
  mf <- mvgam_formula(y ~ 1, trend_formula = ~ AR(p = 1))
  # The brms default method refuses an mvgam_formula as a nested
  # formula. The mvgam method returns the get_prior() table.
  expect_identical(default_prior(mf, data = dat),
                   get_prior(mf, data = dat))
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

# ---------------------------------------------------------------------------
# Deprecated `run_model = FALSE` short-circuit. Skips Stan parse /
# compile / sampling and returns a stub mvgam object whose stancode +
# standata slots are populated but $fit is NULL. The deprecation
# pointer goes to stancode() / standata() on an mvgam_formula(),
# which is what these tests exercise alongside.
# ---------------------------------------------------------------------------

run_model_test_data <- function(n = 24L) {
  set.seed(42L)
  data.frame(
    time = seq_len(n),
    series = factor(rep("series1", n)),
    y = rpois(n, lambda = 5),
    x = rnorm(n)
  )
}

test_that("run_model = FALSE returns the program stancode() writes", {
  data <- run_model_test_data()
  mod <- mvgam(y ~ x, trend_formula = ~ AR(p = 1L), data = data,
               family = poisson(), run_model = FALSE, validate = FALSE)
  expect_s3_class(mod, "mvgam_prefit")
  expect_null(mod$fit)
  expect_equal(mod$standata$N, nrow(data))
  # The forecast and prediction context
  expect_identical(mod$trend_metadata$trend_type, "AR")
  # The deprecation message points at stancode(), which gives the same
  # program
  code_direct <- stancode(mvgam_formula(y ~ x, trend_formula = ~ AR(p = 1L)),
                          data = data, family = poisson(), validate = FALSE)
  norm <- function(x) gsub("\\s+", "", as.character(x))
  expect_equal(norm(mod$stancode), norm(code_direct))
})

test_that("run_model = FALSE threads through jsdgam() too", {
  set.seed(11L)
  dat <- expand.grid(
    time = seq_len(12L),
    species = factor(paste0("sp", 1:3))
  )
  dat$y <- rpois(nrow(dat), 2)
  mod <- jsdgam(
    formula = y ~ 1, factor_formula = ~ -1,
    data = dat, unit = time, species = species,
    family = poisson(), n_lv = 2L,
    run_model = FALSE, silent = 2, validate = FALSE
  )
  expect_s3_class(mod, "jsdgam")
  expect_s3_class(mod, "mvgam")
  expect_null(mod$fit)
  expect_equal(mod$standata$N_lv_trend, 2L)
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

test_that("threads = N forwards brms partial_log_lik into mvgam stancode", {
  # Threading was being dropped at the mvgam mock-fit setup; only
  # closure-unit families threaded (via mvgam's own partial_sum
  # stanvars). The plumbing fix forwards `threads = N` (when N > 1)
  # so brms emits its partial_log_lik_lpmf for brms-native fits.
  set.seed(1L)
  n <- 50L
  dat <- data.frame(
    y      = rnorm(n),
    x      = rnorm(n),
    grp    = factor(sample(letters[1:6], n, replace = TRUE)),
    time   = seq_len(n),
    series = factor(rep("s1", n))
  )
  mf <- mvgam_formula(y ~ x + (1 | grp))
  sc_default <- SW(stancode(mf, data = dat, family = gaussian(),
                            validate = FALSE))
  sc_one     <- SW(stancode(mf, data = dat, family = gaussian(),
                              threads = 1L, validate = FALSE))
  sc_four    <- SW(stancode(mf, data = dat, family = gaussian(),
                              threads = 4L, validate = FALSE))
  expect_false(grepl("partial_log_lik_lpmf", sc_default))
  expect_false(grepl("partial_log_lik_lpmf", sc_one))
  expect_true(grepl("partial_log_lik_lpmf", sc_four))
  expect_true(grepl("reduce_sum", sc_four))
})


# ---- Conway-Maxwell-Binomial family contract --------------------

test_that("com_binomial() emits expected Stan lpmf + lookup table", {
  # Asserts that:
  # 1. The custom lpmf function is declared in the Stan functions
  #    block with the (int y, real mu, real nu, int T, data vector
  #    lfact) signature
  # 2. `trials[n]` (the denominator) is referenced in the model
  #    block
  # 3. The transformed-data log-factorial table is precomputed once
  #    per fit, as a vector rather than the square array of binomial
  #    coefficients an earlier implementation built
  # 4. The lpmf reads that table rather than calling `lchoose()` per
  #    row, and bounds the normalising sum instead of enumerating
  #    every one of the T + 1 outcomes
  set.seed(0)
  dat <- data.frame(
    y = rbinom(30, size = 5L, prob = 0.4),
    trials = 5L,
    x = rnorm(30),
    series = factor("s1"),
    time = 1:30
  )
  mf <- mvgam_formula(bf(y | trials(trials) ~ x))
  sc <- as.character(stancode(
    mf, data = dat, family = com_binomial(), validate = TRUE
  ))
  expect_true(grepl(
    "real com_binomial_lpmf\\(int y, real mu, real nu, int T",
    sc
  ))
  expect_true(grepl("trials\\[n\\]", sc))
  expect_true(grepl("data vector lfact", sc))
  expect_true(grepl(
    "vector\\[max_com_binomial_T \\+ 1\\] lfact_com_binomial", sc
  ))
  expect_false(grepl("lchoose", sc))
  # The normaliser is bounded rather than enumerated, so the helper
  # that locates the mode and the weight function are both emitted.
  expect_true(grepl("com_binomial_mode\\(T, nu, theta\\)", sc))
  expect_true(grepl("com_binomial_lw\\(", sc))
})


test_that("com_binomial() default prior on nu is normal(1, 1)", {
  # `nu` is the natural parameter of the COM-Binomial exponential
  # family, so a normal prior on it regularises as one on a
  # regression coefficient does. It is centred on independence
  # (`nu = 1`) and scaled to the range the data can resolve, the
  # response saturating outside roughly `nu` in `(-1, 4)`. Confirm
  # the injection emits this prior on the nu class along with the
  # truncation correction for the `lb = -5` lower bound.
  set.seed(0)
  dat <- data.frame(
    y = rbinom(30, size = 5L, prob = 0.4),
    trials = 5L,
    x = rnorm(30),
    series = factor("s1"),
    time = 1:30
  )
  mf <- mvgam_formula(bf(y | trials(trials) ~ x))
  sc <- as.character(stancode(
    mf, data = dat, family = com_binomial(), validate = FALSE
  ))
  expect_true(grepl(
    "lprior \\+= normal_lpdf\\(nu \\| 1, 1\\)", sc
  ))
  expect_true(grepl("real<lower=-5> nu", sc))
})


test_that("com_binomial() refuses non-logit links", {
  expect_error(com_binomial(link = "probit"), "{'logit'}", fixed = TRUE)
})


test_that("com_binomial() requires a `trials()` addition term", {
  # The lpmf references `trials[n]`, which brms emits only for a
  # response carrying `trials()`. Without it stanc would fail on an
  # undefined variable, so the omission is named up front.
  dat <- data.frame(
    y = rbinom(30, size = 5L, prob = 0.4),
    trials = 5L,
    x = rnorm(30),
    series = factor("s1"),
    time = 1:30
  )
  expect_error(
    stancode(mvgam_formula(bf(y ~ x)), data = dat,
             family = com_binomial(), validate = FALSE),
    "trials"
  )
  # With the term present, brms sizes `trials` to the rows it kept
  # rather than to the raw frame, which is what makes NA responses
  # usable with a latent trend.
  dat$y[1:5] <- NA
  # brms announces the dropped rows; the point of the test is that
  # `trials` follows them instead of keeping the raw frame's length.
  expect_warning(
    sd <- standata(mvgam_formula(bf(y | trials(trials) ~ x)), data = dat,
                   family = com_binomial(), validate = FALSE),
    "Rows containing NAs"
  )
  expect_identical(length(sd$trials), sd$N)
  expect_identical(sd$N, 25L)
})


test_that("multi-response fits with NA preserve per-response valid rows", {
  # brms's mvbf listwise-deletes rows with NA in ANY response.
  # mvgam overrides this so each
  # response keeps its own non-NA rows and contributes to the
  # shared latent state at every time point it was observed. The
  # `expand_per_response_standata` helper substitutes per-arm
  # data arrays into the combined standata.
  set.seed(1)
  T_ <- 20L
  dat <- data.frame(
    time   = seq_len(T_),
    series = factor(rep("a", T_)),
    count   = rpois(T_, 5),
    biomass = rgamma(T_, 2, 0.5),
    camera  = rbinom(T_, 1, 0.4)
  )
  dat$count[c(5L, 11L)]              <- NA_integer_
  dat$biomass[c(8L, 14L)]            <- NA_real_
  dat$camera[c(3L, 7L, 12L, 18L)]    <- NA_integer_

  f <- bf(count   ~ 1, family = poisson()) +
       bf(biomass ~ 1, family = Gamma(link = "log")) +
       bf(camera  ~ 1, family = bernoulli(link = "logit"))

  mf <- mvgam_formula(f, trend_formula = ~ AR(p = 1))
  # brms emits one "Rows containing NAs were excluded" warning
  # per bf() arm; mvgam's per-arm expansion call emits more.
  # All are expected.
  sd <- withCallingHandlers(
    standata(mf, data = dat, validate = FALSE),
    warning = function(w) {
      if (grepl("Rows containing NAs", conditionMessage(w))) {
        invokeRestart("muffleWarning")
      }
    }
  )

  # Per-response N_<resp> should equal that response's own
  # non-NA row count, NOT the listwise intersection.
  for (r in c("count", "biomass", "camera")) {
    expected <- sum(!is.na(dat[[r]]))
    expect_identical(as.integer(sd[[paste0("N_", r)]]),
                     as.integer(expected))
    expect_identical(length(sd[[paste0("Y_", r)]]), expected)
    expect_identical(length(sd[[paste0("obs_trend_time_", r)]]),
                     expected)
    expect_identical(length(sd[[paste0("obs_trend_series_", r)]]),
                     expected)
  }
  # The per-response N values should differ because each response
  # has different NA rows.
  expect_false(sd$N_count == sd$N_biomass &&
                 sd$N_biomass == sd$N_camera)
})


test_that("each response keeps its own rows whatever its column is called", {
  # brms keys a response by its column with every `_` and `.` taken
  # out, so `y_1` is `N_y1`. The per-response rebuild looked for
  # `N_y_1`, found nothing to replace and left every response on the
  # rows all of them observed. A `trials()` response stopped it
  # outright, and a response taking the family given beside the
  # formula was rebuilt as gaussian.
  set.seed(3)
  T_ <- 24L
  dat <- data.frame(
    time = seq_len(T_), series = factor("a"),
    y_1 = rpois(T_, 5), y.2 = rgamma(T_, 2, 0.5),
    k = rbinom(T_, 10, 0.4), n = 10L, z = rpois(T_, 3)
  )
  dat$y_1[c(3L, 9L)] <- NA
  dat$y.2[c(5L, 14L, 20L)] <- NA
  dat$k[11L] <- NA
  mf <- mvgam_formula(
    bf(y_1 ~ 1, family = poisson()) +
      bf(y.2 ~ 1, family = Gamma(link = "log")) +
      bf(k | trials(n) ~ 1, family = binomial()) +
      bf(z ~ 1),
    trend_formula = ~ AR(p = 1)
  )
  sd <- withCallingHandlers(
    standata(mf, data = dat, family = poisson(), validate = FALSE),
    warning = function(w) {
      if (grepl("Rows containing NAs", conditionMessage(w))) {
        invokeRestart("muffleWarning")
      }
    }
  )
  columns <- c(y1 = "y_1", y2 = "y.2", k = "k", z = "z")
  for (key in names(columns)) {
    expected <- sum(!is.na(dat[[columns[[key]]]]))
    expect_identical(as.integer(sd[[paste0("N_", key)]]), expected)
    expect_identical(length(sd[[paste0("Y_", key)]]), expected)
  }
  expect_identical(as.integer(sd$trials_k), rep(10L, T_ - 1L))
  # `z` names no family and takes the poisson given beside the
  # formula, whose response brms stores as integers.
  expect_true(is.integer(sd$Y_z))
})


test_that("a family written inside bf() is the model's family", {
  # brms reads a family written inside `bf()` over the one given
  # beside the formula. The likelihood followed it, while the fit
  # recorded the default gaussian, so every post-fit method read the
  # wrong family, and a custom family there never had its Stan
  # functions added.
  set.seed(4)
  d <- data.frame(time = 1:30, series = factor("a"),
                  cnt = rpois(30, 4), pos = rgamma(30, 2, 1))
  pf <- mvgam(bf(cnt ~ 1, family = poisson()), data = d, run_model = FALSE,
              validate = FALSE)
  expect_identical(family(pf)$family, "poisson")
  expect_identical(pf$family$family, "poisson")
  code <- stancode(mvgam_formula(bf(pos ~ 1, family = tweedie())), data = d,
                   validate = FALSE)
  expect_match(code, "target += tweedie_lpdf(Y | mu, mphi, mtheta, M);",
               fixed = TRUE)
  expect_match(code, "int<lower=1> M;", fixed = TRUE)
  # The same model spelled with the family beside the formula.
  expect_identical(
    code,
    stancode(mvgam_formula(pos ~ 1), data = d, family = tweedie(),
             validate = FALSE)
  )
})


test_that("a family is read in every spelling a model accepts", {
  set.seed(4)
  d <- data.frame(time = 1:30, series = factor("a"),
                  cnt = rpois(30, 4), pos = rgamma(30, 2, 1))
  by_call <- stancode(mvgam_formula(cnt ~ 1), data = d, family = poisson(),
                      validate = FALSE)
  expect_identical(
    stancode(mvgam_formula(cnt ~ 1), data = d, family = poisson,
             validate = FALSE),
    by_call
  )
  expect_identical(
    stancode(mvgam_formula(cnt ~ 1), data = d, family = "poisson",
             validate = FALSE),
    by_call
  )
  # mvgam's own families are built by their constructors, which brms
  # cannot name.
  expect_identical(
    stancode(mvgam_formula(pos ~ 1), data = d, family = "tweedie",
             validate = FALSE),
    stancode(mvgam_formula(pos ~ 1), data = d, family = tweedie(),
             validate = FALSE)
  )
  expect_error(
    stancode(mvgam_formula(pos ~ 1), data = d,
             family = c("tweedie", "log"), validate = FALSE),
    "A link cannot be given with the family name 'tweedie'",
    fixed = TRUE
  )
  # A family inside `bf()` meets the same checks as one beside it.
  expect_error(
    stancode(mvgam_formula(bf(cnt ~ 1, family = categorical())),
             data = transform(d, cnt = factor(cnt %% 3)), validate = FALSE),
    "Use categ()", fixed = TRUE
  )
})


test_that("each response of a multivariate model gets its family's code", {
  # Stan functions, data and default priors were read off the family
  # given beside the formula, so a custom family named by one response
  # left its likelihood calling a function the program never declared.
  set.seed(5)
  d <- data.frame(time = 1:30, series = factor("a"),
                  pos = rgamma(30, 2, 1), cnt = rpois(30, 4),
                  k = rbinom(30, 10, 0.4), n = 10L, y = rbinom(30, 1, 0.5))
  f <- bf(pos ~ 1, family = tweedie()) +
    bf(cnt ~ 1, family = beta_nb()) +
    bf(k | trials(n) ~ 1, family = com_binomial()) + set_rescor(FALSE)
  code <- stancode(mvgam_formula(f), data = d, validate = FALSE)
  expect_match(code, "real tweedie_lpdf(", fixed = TRUE)
  expect_match(code, "real beta_nb_lpmf(", fixed = TRUE)
  expect_match(code, "int max_com_binomial_T = max(trials_k);", fixed = TRUE)
  expect_match(code, "gamma_lpdf(shape_cnt | 2, 0.5)", fixed = TRUE)
  expect_match(code, "normal_lpdf(nu_k | 1, 1)", fixed = TRUE)
  # A closure-unit family lays its data out by unit, which one
  # response among several cannot have.
  expect_error(
    stancode(mvgam_formula(bf(y ~ 1, family = occ()) + bf(cnt ~ 1)),
             data = d, validate = FALSE),
    "'occ()' cannot be one response of a multivariate model", fixed = TRUE
  )
  expect_error(
    stancode(mvgam_formula(bf(k ~ 1, family = com_binomial()) + bf(cnt ~ 1)),
             data = d, validate = FALSE),
    "needs its number of trials"
  )
})


test_that("multi-response fits with no NAs hit the fast no-op path", {
  # When no response has NAs, brms's listwise-deleted standata is
  # already per-response-correct and `expand_per_response_standata`
  # returns it unchanged after a fast no-op check.
  set.seed(1)
  T_ <- 20L
  dat <- data.frame(
    time   = seq_len(T_),
    series = factor(rep("a", T_)),
    count   = rpois(T_, 5),
    biomass = rgamma(T_, 2, 0.5)
  )

  mf <- mvgam_formula(
    bf(count ~ 1, family = poisson()) +
      bf(biomass ~ 1, family = Gamma(link = "log")),
    trend_formula = ~ AR(p = 1)
  )
  sd <- withCallingHandlers(
    standata(mf, data = dat, validate = FALSE),
    warning = function(w) {
      if (grepl("Rows containing NAs", conditionMessage(w))) {
        invokeRestart("muffleWarning")
      }
    }
  )
  expect_identical(as.integer(sd$N_count),   T_)
  expect_identical(as.integer(sd$N_biomass), T_)
})


test_that("per-arm standata expansion handles smooths, ranef and dpar", {
  # Verifies the trailing-then-mid key stripper in
  # `map_combined_key_to_single` correctly recovers per-arm values
  # under every per-key naming pattern brms emits.
  set.seed(1)
  T_ <- 30L
  dat <- data.frame(
    time     = seq_len(T_),
    series   = factor(rep("a", T_)),
    g        = factor(rep(c("g1", "g2", "g3"), each = T_ / 3L)),
    x        = rnorm(T_),
    z        = rnorm(T_),
    m        = rep(1:12, length.out = T_),
    count    = rpois(T_, 5),
    biomass  = rgamma(T_, 2, 0.5),
    camera   = rbinom(T_, 1, 0.4)
  )
  dat$count[c(2L, 5L)]            <- NA_integer_
  dat$biomass[c(8L, 11L, 17L)]    <- NA_real_
  dat$camera[c(3L, 7L, 12L, 18L)] <- NA_integer_

  mf <- mvgam_formula(
    bf(count   ~ x + (1|g),               family = poisson()) +
      bf(biomass ~ s(m, k = 5), shape ~ z, family = Gamma(link = "log")) +
      bf(camera  ~ x,                       family = bernoulli(link = "logit")),
    trend_formula = ~ AR(p = 1)
  )
  sd <- withCallingHandlers(
    standata(mf, data = dat, validate = FALSE),
    warning = function(w) {
      if (grepl("Rows containing NAs", conditionMessage(w))) {
        invokeRestart("muffleWarning")
      }
    }
  )
  # Every per-response array reaches its expected per-arm length.
  for (r in c("count", "biomass", "camera")) {
    expected <- sum(!is.na(dat[[r]]))
    expect_identical(as.integer(sd[[paste0("N_", r)]]),
                     as.integer(expected))
    expect_identical(length(sd[[paste0("Y_", r)]]), expected)
  }
  # Ranef Z and J arrays for count (mid-position then trailing
  # suffix patterns) should match N_count.
  expect_identical(length(sd$J_1_count),   as.integer(sd$N_count))
  expect_identical(nrow(sd$Z_1_count_1),   as.integer(sd$N_count))
  # Smooth Zs array for biomass (mid-position suffix).
  expect_identical(nrow(sd$Zs_biomass_1_1),
                   as.integer(sd$N_biomass))
  # Dpar `shape ~ z` design matrix for biomass has trailing
  # `_biomass` suffix after the dpar prefix `_shape`.
  expect_identical(nrow(sd$X_shape_biomass),
                   as.integer(sd$N_biomass))
})


test_that("a modelled dpar does not collide with mvgam's injected default prior", {
  # mvgam injects a class-level default for `nu` (com_binomial) and for
  # `shape` / `mtail` (beta_nb). When the user gives that parameter its
  # own sub-formula, brms replaces the scalar with a design matrix, so
  # a prior aimed at the scalar class matches nothing and brms rejects
  # the whole prior set. The injected row has to be dropped first.
  set.seed(4)
  dat <- data.frame(
    y = rbinom(40, size = 10, prob = 0.5),
    trials = 10L,
    site = factor(rep(c("a", "b"), each = 20)),
    time = 1:40,
    series = factor(rep("series1", 40))
  )
  sc <- paste(unlist(stancode(
    mvgam_formula(brms::bf(y | trials(trials) ~ 1, nu ~ site)),
    data = dat, family = com_binomial(), backend = "cmdstanr", validate = FALSE
  )), collapse = "\n")
  expect_true(grepl("b_nu", sc, fixed = TRUE))
  # nu is identity-linked, so its default belongs on the intercept once
  # nu is modelled. Without the move brms falls back to a positive-only
  # gamma on a parameter that may go negative, and warns.
  expect_true(grepl("normal_lpdf(Intercept_nu | 1, 1)", sc, fixed = TRUE))
  expect_false(grepl("gamma_lpdf(Intercept_nu", sc, fixed = TRUE))

  # and the scalar default is still emitted when nu is not modelled
  sc_plain <- paste(unlist(stancode(
    mvgam_formula(y | trials(trials) ~ 1),
    data = dat, family = com_binomial(), backend = "cmdstanr", validate = FALSE
  )), collapse = "\n")
  expect_true(grepl("normal_lpdf(nu | 1, 1)", sc_plain, fixed = TRUE))
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

# `time` and `series` are the grouping columns of the trend frame.
# Selecting them in `across()` failed for any trend formula naming
# time.
time_covariate_frames <- function() {
  set.seed(7)
  uni <- data.frame(y = rpois(120, 5), x = rnorm(120), time = 1:120,
                    series = factor("series1"))
  multi <- expand.grid(time = 1:50, series = factor(paste0("s", 1:4)))
  multi$y <- rpois(nrow(multi), 5)
  multi$y2 <- rpois(nrow(multi), 8)
  renamed <- data.frame(y = rpois(120, 5), year = 1:120,
                        site = factor("a"))
  list(uni = uni, multi = multi, renamed = renamed)
}
time_trend_code <- function(tf, data, f = y ~ 1) {
  stancode(mvgam_formula(f, trend_formula = tf), data = data,
           family = poisson(), validate = FALSE)
}

test_that("trend_formula takes time as a covariate", {
  d <- time_covariate_frames()
  smooth <- "sds_[0-9]+_trend"
  expect_match(time_trend_code(~ s(time) + AR(p = 1), d$uni), smooth)
  expect_match(time_trend_code(
    ~ s(time, by = lv_axis()) + AR(p = 1, n_lv = 2), d$multi
  ), smooth)
  # A column named `year` is an ordinary covariate
  expect_match(time_trend_code(
    ~ s(year) + AR(p = 1, time = year, series = site), d$renamed
  ), smooth)
  # A trend without covariates builds no design
  bare <- time_trend_code(~ AR(p = 1), d$uni)
  expect_false(grepl("sds_", bare, fixed = TRUE))
  expect_false(grepl("b_trend", bare, fixed = TRUE))
})

test_that("trend_formula takes time as a covariate on every other path", {
  skip_on_cran()
  d <- time_covariate_frames()
  smooth <- "sds_[0-9]+_trend"
  expect_match(time_trend_code(~ time + AR(p = 1), d$uni), "b_trend",
               fixed = TRUE)
  expect_match(time_trend_code(~ s(x) + AR(p = 1), d$uni), smooth)
  expect_match(time_trend_code(~ s(time) + AR(p = 1), d$multi,
                               brms::mvbind(y, y2) ~ 1), smooth)
  sd_gp <- standata(
    mvgam_formula(y ~ 1, trend_formula = ~ gp(time, k = 20) + AR(p = 1)),
    data = d$uni, family = poisson(), validate = FALSE)
  expect_true(any(grepl("^Xgp_.*_trend$", names(sd_gp))))
})

test_that("NA responses shrink the likelihood but not the trend grid", {
  # mvgam keeps the latent process on the full time grid and lets
  # the likelihood run over observed rows only. Anything carried
  # per observation must therefore follow brms's retained rows.
  # `com_binomial()` once packed its denominator from the raw
  # frame instead, so Stan received more values than there were
  # observations and every chain died at data-init.
  #
  # Three series over twenty times means the raw row count (60)
  # matches neither `N` nor the grid, so a leftover raw-length
  # array cannot hide behind a coincidence.
  set.seed(2)
  n_time <- 20L
  dat <- expand.grid(time = seq_len(n_time),
                     series = factor(paste0("s", 1:3)))
  dat$x <- rnorm(nrow(dat))
  dat$trials <- pmax(rpois(nrow(dat), 20L), 1L)

  cases <- list(
    list(tag = "poisson", family = poisson(),
         y = rpois(nrow(dat), 5), formula = y ~ x),
    list(tag = "tweedie", family = tweedie(),
         y = rgamma(nrow(dat), 2, 1), formula = y ~ x),
    list(tag = "beta_nb", family = beta_nb(),
         y = rnbinom(nrow(dat), mu = 5, size = 2), formula = y ~ x),
    list(tag = "com_binomial", family = com_binomial(),
         y = rbinom(nrow(dat), dat$trials, 0.5),
         formula = bf(y | trials(trials) ~ x))
  )

  for (case in cases) {
    d <- dat
    d$y <- case$y
    d$y[c(3L, 17L, 28L, 41L, 55L)] <- NA
    mf <- mvgam_formula(case$formula, trend_formula = ~ AR(p = 1))
    # brms reports the rows it dropped, once per internal pass.
    sd <- withCallingHandlers(
      standata(mf, data = d, family = case$family, backend = "cmdstanr",
               validate = FALSE),
      warning = function(w) {
        if (grepl("Rows containing NAs", conditionMessage(w))) {
          invokeRestart("muffleWarning")
        }
      }
    )
    # The likelihood sees only observed rows ...
    expect_identical(sd$N, sum(!is.na(d$y)))
    # ... while the trend still spans every timepoint.
    expect_identical(sd$N_time_trend, n_time)
    # No data array may still be sized to the raw frame.
    raw_length <- vapply(
      sd,
      function(v) is.atomic(v) && is.null(dim(v)) && length(v) == nrow(d),
      logical(1)
    )
    expect_identical(names(sd)[raw_length], character(0))
  }
})

test_that("closure-unit families keep their unit arrays over observed visits", {
  # A missing response is a visit that did not happen, which is the
  # normal case in repeat-visit survey designs. The per-unit arrays
  # index brms's retained rows, so they must cover the observed
  # visits exactly: `sum(n_rep)` is the likelihood's row count and
  # no `visit_idx` entry may point past it.
  sim <- sim_closure_unit_data(family = occ(), type = 1L,
                               n_sites = 8L, n_visits = 4L, seed = 3L)
  dat <- sim$data_train
  dat$y[seq(2L, nrow(dat), by = 7L)] <- NA_integer_
  sd <- expect_warning(
    standata(mvgam_formula(y ~ 1), data = dat, family = occ(),
             validate = FALSE),
    "Rows containing NAs"
  )
  expect_identical(as.integer(sd$N), sum(!is.na(dat$y)))
  expect_identical(sum(sd$n_rep), as.integer(sd$N))
  expect_true(max(sd$visit_idx) <= sd$N)
  expect_true(all(sd$Y_max <= sd$K_max))

  # A unit whose visits are all missing carries nothing about
  # detection and is dropped rather than left to index an empty
  # visit list in the Stan loop.
  unit <- paste(dat$series, dat$time)
  blanked <- dat
  blanked$y[unit == unique(unit)[2L]] <- NA_integer_
  sd_dropped <- expect_warning(
    standata(mvgam_formula(y ~ 1), data = blanked, family = occ(),
             validate = FALSE),
    "Rows containing NAs"
  )
  expect_identical(as.integer(sd_dropped$N_unit),
                   as.integer(sd$N_unit) - 1L)
  expect_identical(sum(sd_dropped$n_rep), as.integer(sd_dropped$N))
})

test_that("com_binomial() accepts a constant denominator", {
  # `trials(30)` is the brms spelling for a fixed number of trials.
  # Taking the denominator from the addition term rather than a
  # hardcoded column means no `trials` column is needed.
  set.seed(7)
  dat <- data.frame(
    y = rbinom(30, size = 30L, prob = 0.5),
    x = rnorm(30), series = factor("s1"), time = 1:30
  )
  expect_false("trials" %in% names(dat))
  sd <- standata(mvgam_formula(bf(y | trials(30) ~ x)), data = dat,
                 family = com_binomial(), validate = FALSE)
  expect_true(all(sd$trials == 30L))
  expect_identical(length(sd$trials), sd$N)
})


# ---- brms code-generation options ---------------------------------
# `knots`, `sample_prior`, `drop_unused_levels` and `normalize` are
# documented arguments that reach brms three times over: once when the
# mock fit is built, once when the Stan program is regenerated and once
# when the Stan data is. Each of these pins one of those journeys.

codegen_test_data <- function(n_time = 24L, n_series = 2L) {
  set.seed(11)
  out <- data.frame(
    time = rep(seq_len(n_time), n_series),
    series = factor(rep(paste0("s", seq_len(n_series)), each = n_time)),
    elev = runif(n_time * n_series, 0, 100)
  )
  out$y <- rpois(nrow(out), 5)
  out
}

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

test_that("knots reach the basis on both formulas", {
  dat <- codegen_test_data()
  kn <- list(elev = seq(0, 100, length.out = 5))

  obs <- mvgam_formula(y ~ s(elev, bs = "cr", k = 5),
                       trend_formula = ~ AR(p = 1))
  sd_default <- standata(obs, data = dat, family = poisson(), validate = FALSE)
  sd_knots <- standata(obs, data = dat, family = poisson(), knots = kn,
                       validate = FALSE)
  expect_false(isTRUE(all.equal(sd_default$Xs, sd_knots$Xs)))
  expect_false(isTRUE(all.equal(sd_default$Zs_1_1, sd_knots$Zs_1_1)))

  # The basis has to match what brms builds from the same knots, not
  # merely differ from the default.
  brms_knots <- brms::make_standata(y ~ s(elev, bs = "cr", k = 5),
                                    data = dat, family = poisson(),
                                    knots = kn)
  expect_equal(sd_knots$Xs, brms_knots$Xs, ignore_attr = TRUE)

  trend <- mvgam_formula(y ~ 1,
                         trend_formula = ~ s(elev, bs = "cr", k = 5) +
                           AR(p = 1))
  td_default <- standata(trend, data = dat, family = poisson(),
                         validate = FALSE)
  td_knots <- standata(trend, data = dat, family = poisson(), knots = kn,
                       validate = FALSE)
  expect_false(isTRUE(all.equal(td_default$Xs_trend, td_knots$Xs_trend)))
})

test_that("a knot count that disagrees with k is refused", {
  # mgcv raises this, which is only reachable once the knots arrive.
  dat <- codegen_test_data()
  expect_error(
    standata(mvgam_formula(y ~ s(elev, bs = "cr", k = 5),
                           trend_formula = ~ AR(p = 1)),
              data = dat, family = poisson(),
              knots = list(elev = seq(0, 100, length.out = 6)),
              validate = FALSE),
    "knots"
  )
})

test_that("drop_unused_levels decides whether an empty level is priced", {
  dat <- codegen_test_data()
  dat$grp <- factor(rep(c("a", "b"), length.out = nrow(dat)),
                    levels = c("a", "b", "c"))
  mf <- mvgam_formula(y ~ grp, trend_formula = ~ AR(p = 1))

  expect_identical(
    as.integer(standata(mf, data = dat, family = poisson(),
                        validate = FALSE)$K), 2L
  )
  # The prior table is keyed by the same coefficients, so it has to be
  # merged under the same setting: merging under the default leaves no
  # row for `b_grpc` and brms rejects the whole table.
  kept <- standata(mf, data = dat, family = poisson(),
                   drop_unused_levels = FALSE, validate = FALSE)
  expect_identical(as.integer(kept$K), 3L)
})

test_that("sample_prior moves the data, not the program", {
  dat <- codegen_test_data()
  mf <- mvgam_formula(y ~ 1, trend_formula = ~ AR(p = 1))

  # brms gates the likelihood on `prior_only`, which is Stan data, so
  # "only" changes the standata and leaves the program untouched.
  expect_identical(
    as.integer(standata(mf, data = dat, family = poisson(),
                        validate = FALSE)$prior_only), 0L
  )
  expect_identical(
    as.integer(standata(mf, data = dat, family = poisson(),
                        sample_prior = "only", validate = FALSE)$prior_only), 1L
  )
  expect_identical(
    stancode(mf, data = dat, family = poisson(), silent = 2L, validate = FALSE),
    stancode(mf, data = dat, family = poisson(), sample_prior = "only",
             silent = 2L, validate = FALSE)
  )

  # "yes" keeps the likelihood and adds draws from the prior alongside.
  expect_true(grepl(
    "prior_Intercept",
    stancode(mf, data = dat, family = poisson(), sample_prior = "yes",
             silent = 2L, validate = FALSE)
  ))
})

test_that("normalize = FALSE emits the unnormalised densities", {
  dat <- codegen_test_data()
  mf <- mvgam_formula(y ~ 1, trend_formula = ~ AR(p = 1))
  unnormalised <- stancode(mf, data = dat, family = poisson(),
                           normalize = FALSE, silent = 2L, validate = FALSE)
  expect_true(grepl("_lup[dm]f", unnormalised))
  expect_false(identical(
    unnormalised,
    stancode(mf, data = dat, family = poisson(), silent = 2L, validate = FALSE)
  ))
  # The trend's own `sigma` prior is mvgam's to set, so brms's copy is
  # dropped under either spelling. Leaving the unnormalised spelling
  # unrecognised left `student_t_lupdf(sigma | ...)` in the program,
  # where the renamer then suffixed the function instead of the
  # parameter and Stan rejected it.
  expect_false(grepl("student_t_lupdf_trend", unnormalised))
})

test_that("the reserved-word list carries both density spellings", {
  reserved <- mvgam:::get_stan_reserved_words()
  normalised <- grep("_lp[dm]f$", reserved, value = TRUE)
  expect_true(length(normalised) > 0)
  expect_true(all(
    sub("_lp([dm])f$", "_lup\\1f", normalised) %in% reserved
  ))
})

test_that("save_model writes the assembled program", {
  dat <- codegen_test_data()
  mf <- mvgam_formula(y ~ 1, trend_formula = ~ AR(p = 1))
  path <- withr::local_tempfile(fileext = ".stan")

  code <- stancode(mf, data = dat, family = poisson(), save_model = path,
                   silent = 2L, validate = FALSE)
  # What lands on disk is mvgam's program, not the brms one it starts
  # from
  expect_identical(paste(readLines(path), collapse = "\n"),
                   as.character(code))
})

test_that("save_model refuses a directory that does not exist", {
  dat <- codegen_test_data()
  expect_error(
    stancode(mvgam_formula(y ~ 1, trend_formula = ~ AR(p = 1)),
              data = dat, family = poisson(),
              save_model = file.path(tempdir(), "no_such_dir", "m.stan"),
              validate = FALSE),
    "not exist"
  )
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
             stan_funs = "real f(real x) { return x; }", validate = FALSE),
    "stanvars"
  )

  # The supported spelling still works.
  expect_true(grepl(
    "csr_matrix_times_vector",
    stancode(mvgam_formula(bf(y ~ elev, sparse = TRUE),
                           trend_formula = ~ AR(p = 1)),
              data = dat, family = poisson(), silent = 2L, validate = FALSE)
  ))
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

test_that("the options reach a fit with no trend formula", {
  # A trend-free fit takes its program and data straight off the mock
  # brms fit, never through the assembly stage, so it is a separate
  # journey for the options to make.
  dat <- codegen_test_data()
  kn <- list(elev = seq(0, 100, length.out = 5))
  mf <- mvgam_formula(y ~ s(elev, bs = "cr", k = 5))

  sd_knots <- standata(mf, data = dat, family = poisson(), knots = kn,
                       validate = FALSE)
  expect_false(isTRUE(all.equal(
    standata(mf, data = dat, family = poisson(),
             validate = FALSE)$Xs, sd_knots$Xs
  )))
  expect_equal(
    sd_knots$Xs,
    brms::make_standata(y ~ s(elev, bs = "cr", k = 5), data = dat,
                        family = poisson(), knots = kn)$Xs,
    ignore_attr = TRUE
  )
  expect_true(grepl(
    "_lup[dm]f",
    stancode(mf, data = dat, family = poisson(), normalize = FALSE,
             silent = 2L, validate = FALSE)
  ))
})

test_that("a knots fit carries its basis into newdata predictions", {
  # `posterior_smooths()` and `conditional_smooths()` rebuild their
  # design matrices with `brms::standata()` on the stored brms fit. If
  # the knots never reached that fit, a prediction grid would be built
  # on a default basis and the smooth would be read off the wrong
  # coefficients without anything erroring.
  dat <- codegen_test_data()
  kn <- list(elev = seq(0, 100, length.out = 5))
  form <- mvgam_formula(y ~ s(elev, bs = "cr", k = 5),
                        trend_formula = ~ AR(p = 1))

  with_knots <- mvgam:::build_stan_components(
    formula = form, data = dat, family = poisson(), knots = kn,
    validate = FALSE, silent = 2L
  )
  without <- mvgam:::build_stan_components(
    formula = form, data = dat, family = poisson(),
    validate = FALSE, silent = 2L
  )
  expect_identical(with_knots$obs_setup$codegen$knots, kn)
  expect_identical(with_knots$trend_setup$codegen,
                   with_knots$obs_setup$codegen)

  grid <- data.frame(
    elev = seq(5, 95, length.out = 12), time = 1:12,
    series = factor("s1", levels = levels(dat$series))
  )
  regen <- function(components) {
    brms::standata(components$obs_setup$brmsfit, newdata = grid,
                   check_response = FALSE, internal = TRUE)$Xs
  }
  expect_false(isTRUE(all.equal(regen(with_knots), regen(without))))
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

test_that("get_prior reports the trend model that gets fitted", {
  # The observation side takes these through the dots brms reads; the
  # trend side is assembled by mvgam and has to be given them too, or a
  # user cannot set a prior on a coefficient the fit will estimate.
  dat <- codegen_test_data()
  dat$grp <- factor(rep(c("a", "b"), length.out = nrow(dat)),
                    levels = c("a", "b", "c"))
  mf <- mvgam_formula(y ~ 1, trend_formula = ~ grp + AR(p = 1))

  trend_coefs <- function(...) {
    tbl <- get_prior(mf, data = dat, family = poisson(), ...)
    sort(tbl$coef[tbl$class == "b_trend" & nzchar(tbl$coef)])
  }
  # The trend formula carries no intercept, so every observed level
  # earns a coefficient and the unused one appears only when it is kept.
  expect_identical(trend_coefs(), c("grpa", "grpb"))
  expect_identical(trend_coefs(drop_unused_levels = FALSE),
                   c("grpa", "grpb", "grpc"))
})

test_that("a shrinkage prior gets the Stan data it declares", {
  # `horseshoe()` declares data of its own. The program was generated
  # with the prior and the data without it, so the fit declared six
  # variables nothing supplied.
  dat <- codegen_test_data()
  dat$x1 <- rnorm(nrow(dat))
  dat$x2 <- rnorm(nrow(dat))
  mf <- mvgam_formula(y ~ x1 + x2, trend_formula = ~ AR(p = 1))
  hs <- brms::prior(horseshoe(1), class = "b")

  code <- stancode(mf, data = dat, family = poisson(), prior = hs,
                   silent = 2L, validate = FALSE)
  sdata <- standata(mf, data = dat, family = poisson(), prior = hs,
                    silent = 2L, validate = FALSE)
  declared <- unique(unlist(regmatches(
    code, gregexpr("\\b(hs_[a-z_]+|Kscales)\\b", code)
  )))
  supplied <- names(sdata)
  # Every name the data block reads has to be a name the data carries.
  expect_true(all(
    intersect(declared, c("hs_df", "hs_df_global", "hs_df_slab",
                          "hs_scale_global", "hs_scale_slab", "Kscales")) %in%
      supplied
  ))
})

test_that("a shrinkage prior on the trend keeps its own scale", {
  # `horseshoe()` scales its global parameter by the family's residual
  # standard deviation. The trend submodel reaches brms as a gaussian
  # whose residual scale mvgam takes back out, so that factor named a
  # parameter the program does not declare, and under a gaussian
  # observation family it resolved to the observation model's `sigma`.
  dat <- codegen_test_data()
  dat$x1 <- rnorm(nrow(dat))
  dat$x2 <- rnorm(nrow(dat))
  mf <- mvgam_formula(y ~ 1, trend_formula = ~ x1 + x2 + AR(p = 1))

  for (shrinkage in c("horseshoe(1)", "R2D2(0.5, 2)")) {
    # A poisson model declares no `sigma`, so the parse catches the
    # reference. `validate = TRUE` is the assertion.
    stancode(mf, data = dat, family = poisson(), silent = 2L,
             validate = TRUE,
             prior = brms::set_prior(shrinkage, class = "b_trend"))

    # A gaussian model declares one, and the program compiled with the
    # trend's prior scaled by the observation model's residual scale.
    code <- stancode(mf, data = dat, family = gaussian(), silent = 2L,
                     validate = TRUE,
                     prior = brms::set_prior(shrinkage, class = "b_trend"))
    scaled <- grep("hs_global_trend|R2D2_tau2_trend",
                   strsplit(code, "\n", fixed = TRUE)[[1]], value = TRUE)
    expect_false(any(grepl("\\bsigma\\b", scaled)))
  }

  # Asking for the scaling names what the trend does not have
  expect_error(
    stancode(mf, data = dat, family = poisson(), silent = 2L,
             prior = brms::set_prior("horseshoe(1, autoscale = TRUE)",
                                     class = "b_trend"), validate = FALSE),
    "autoscale"
  )
})

test_that("normalize = FALSE still injects the trend into the GLM call", {
  # Under `normalize = FALSE` brms writes `*_glm_lupmf` calls. GLM
  # detection once recognised only the normalised spelling. The trend
  # was then computed and never used, or the assembly aborted. Each
  # formula holds a predictor. brms writes an intercept-only formula as
  # a plain density.
  dat <- codegen_test_data()
  dat$z <- rnorm(nrow(dat))
  dat$bin <- rbinom(nrow(dat), 1L, 0.5)
  dat$cont <- rnorm(nrow(dat))

  cases <- list(
    list(resp = "y", family = poisson(), density = "poisson_log_glm_lupmf"),
    list(resp = "y", family = brms::negbinomial(),
         density = "neg_binomial_2_log_glm_lupmf"),
    list(resp = "cont", family = gaussian(),
         density = "normal_id_glm_lupdf"),
    list(resp = "bin", family = brms::bernoulli(),
         density = "bernoulli_logit_glm_lupmf")
  )
  for (case in cases) {
    mf <- mvgam_formula(
      stats::as.formula(paste0(case$resp, " ~ elev + z")),
      trend_formula = ~ AR(p = 1)
    )
    code <- stancode(mf, data = dat, family = case$family,
                     normalize = FALSE, silent = 2L, validate = FALSE)
    # The rewritten call keeps the unnormalised spelling and takes the
    # trend in its intercept. A normalised spelling would quietly undo
    # `normalize = FALSE`.
    expect_true(stan_pattern(paste0(
      case$density, "(Y | Xc, Intercept + to_vector(trend)[obs_trend_idx]"
    ), code, fixed = TRUE), label = paste("trend in the call for",
                                           case$family$family))
  }
})

test_that("an ordinal GLM keeps its own argument layout", {
  # `ordered_logistic_glm` takes `(x, beta, cutpoints)` and carries no
  # intercept for the trend to join. The program builds `mu` with the
  # trend and scores it with the vectorised `ordered_logistic`. A
  # parser applying the `(x, alpha, beta)` layout of the other families
  # would multiply the design matrix by the cutpoints. `stancode()`
  # parses what it emits. The assertions fix each argument's place.
  dat <- codegen_test_data()
  dat$z <- rnorm(nrow(dat))
  dat$ord <- factor(sample(1:3, nrow(dat), replace = TRUE), ordered = TRUE)
  mf <- mvgam_formula(ord ~ elev + z, trend_formula = ~ AR(p = 1))

  code <- stancode(mf, data = dat, family = brms::cumulative(), silent = 2L,
                   validate = FALSE)

  # The cutpoints stay in the likelihood call. The coefficients move
  # to the predictor the program builds.
  expect_true(grepl("ordered_logistic_lpmf(Y | mu, Intercept)", code,
                    fixed = TRUE))
  expect_false(grepl("_glm_", code, fixed = TRUE))
  expect_true(grepl("vector[N] mu = Xc * b;", code, fixed = TRUE))
  expect_false(grepl("Intercept + to_vector(trend)", code, fixed = TRUE))
  expect_true(grepl("mu += to_vector(trend)[obs_trend_idx];", code,
                    fixed = TRUE))
})

# One series of forty occasions, with a Gaussian partner response.
injection_frame <- function() {
  set.seed(11)
  n <- 40L
  d <- data.frame(
    time = seq_len(n), series = factor("s1"), x = rnorm(n),
    e = runif(n, 1, 2), tr = rep(10L, n), yh = rnorm(n), yg = rnorm(n),
    yB01 = runif(n, 0.1, 0.9), yp = rpois(n, 3), yk = rbinom(n, 10, 0.4)
  )
  d$yB01[1:4] <- c(0, 1, 0, 1)
  d
}

test_that("each response takes its trend before its inverse link", {
  # brms applies these inverse links to the whole vector once the
  # predictor is built. The trend joins on the link scale, before that
  # statement and outside the likelihood loop.
  d <- injection_frame()
  arms <- list(
    yB01 = bf(yB01 ~ x, family = zero_one_inflated_beta()),
    yp = bf(yp ~ x, family = beta_nb()),
    yk = bf(yk | trials(tr) ~ x, family = com_binomial())
  )
  for (resp in names(arms)) {
    code <- stancode(
      mvgam_formula(arms[[resp]] + bf(yh ~ 1) + set_rescor(FALSE),
                    trend_formula = ~ AR()),
      data = d, validate = TRUE
    )
    lines <- trimws(strsplit(code, "\n", fixed = TRUE)[[1]])
    trend_at <- which(lines == mvgam:::trend_addition_lines(resp, ""))
    link_at <- grep(paste0("^mu_", resp, " = (inv_logit|exp)\\(mu_",
                           resp, "\\);$"), lines)
    expect_length(trend_at, 1L)
    expect_length(link_at, 1L)
    expect_lt(trend_at, link_at)
  }
})

test_that("a nonlinear response of a multivariate model takes its trend", {
  d <- injection_frame()
  code <- stancode(
    mvgam_formula(
      bf(yg ~ a + b * x, a ~ 1, b ~ 1, nl = TRUE) + bf(yh ~ 1) +
        set_rescor(FALSE),
      trend_formula = ~ AR()
    ),
    data = d, validate = TRUE
  )
  lines <- trimws(strsplit(code, "\n", fixed = TRUE)[[1]])
  built_at <- grep("^mu_yg\\[n\\] = \\(nlp_yg_a", lines)
  trend_at <- which(lines == mvgam:::trend_addition_lines("yg", ""))
  expect_length(built_at, 1L)
  expect_length(trend_at, 1L)
  expect_gt(trend_at, built_at)
})

test_that("an offset-only formula keeps its offset", {
  # The empty-formula placeholder joins the right-hand side, and the
  # model keeps the offset as a term of its own.
  d <- injection_frame()
  uni <- standata(
    mvgam_formula(yp ~ 0 + offset(log(e)), trend_formula = ~ AR()),
    data = d, family = poisson(), validate = FALSE
  )
  expect_equal(as.numeric(uni$offsets), log(d$e))
  multi <- standata(
    mvgam_formula(
      bf(yg ~ 0 + offset(log(e))) + bf(yh ~ 1) + set_rescor(FALSE),
      trend_formula = ~ AR()
    ),
    data = d, validate = FALSE
  )
  expect_equal(as.numeric(multi$offsets_yg), log(d$e))
})

test_that("a mixture family is refused for every response", {
  d <- injection_frame()
  mix <- suppressMessages(brms::mixture(gaussian, gaussian))
  expect_error(
    stancode(mvgam_formula(yg ~ x, trend_formula = ~ AR()),
             data = d, family = mix, validate = FALSE),
    "Mixture families are not supported"
  )
  expect_error(
    stancode(
      mvgam_formula(bf(yg ~ x, family = mix) + bf(yh ~ 1) +
                      set_rescor(FALSE)),
      data = d, validate = FALSE
    ),
    "Mixture families are not supported"
  )
})

test_that("the two code-generation entry points agree", {
  # `stancode()` and `standata()` reach the same generator, so an
  # argument on one and not the other means the pair describes
  # different models. `standata()` accepted five of these through its
  # dots while advertising none of them, and restated defaults drift
  # apart silently because nothing compares them.
  sc <- formals(mvgam:::stancode.mvgam_formula)
  sd <- formals(mvgam:::standata.mvgam_formula)
  expect_setequal(names(sc), names(sd))

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


test_that("group_inds_trend is ordered by the trend's series axis", {
  # Stan reads `group_inds_trend[s]` against `s`, the trend matrix's
  # own series index, and `obs_trend_series` records which column each
  # row was given. Deriving the order a second time by walking the
  # data answers a different question whenever the rows are not sorted
  # by series, and the two answers are a permutation of one another,
  # so the program still compiles and the wrong series are correlated
  # together. The supplied `series` column here is built the way
  # `interaction()` builds one by default, whose level order differs
  # from the order the rows present.
  n_t <- 6L
  d <- data.frame(
    region = factor(rep(rep(c("r1", "r2"), each = 3L), times = n_t)),
    species = factor(rep(rep(c("sp1", "sp2", "sp3"), times = 2L),
                         times = n_t)),
    time = rep(seq_len(n_t), each = 6L)
  )
  d$series <- interaction(d$region, d$species, drop = TRUE)
  set.seed(2L)
  d$y <- rpois(nrow(d), 3)

  f <- mvgam_formula(
    y ~ 1,
    trend_formula = ~ AR(p = 1, gr = region, subgr = species, cor = TRUE)
  )
  sd <- SW(standata(f, data = d, family = poisson(), validate = FALSE))

  # The group of whichever series occupies each trend column.
  s_idx <- as.integer(sd$obs_trend_series)
  gr <- as.character(d$region)
  occupant <- vapply(seq_len(sd$N_series_trend),
                     function(k) gr[which(s_idx == k)[1L]], character(1))
  expect_identical(
    as.integer(sd$group_inds_trend),
    match(occupant, sort(unique(gr)))
  )
})


test_that("obs_trend_time runs in time order, not row order", {
  # The index is what the trend steps along, so it has to follow the
  # clock rather than the order the rows arrived in. A frame grouped
  # by series rather than by date otherwise gives its first series'
  # earliest time index 1 and leaves the real first time in the
  # middle, and the mapping stays a bijection so nothing downstream
  # can notice.
  n_t <- 8L
  d <- data.frame(
    time = rep(seq_len(n_t), each = 2L),
    series = factor(rep(c("a", "b"), times = n_t))
  )
  set.seed(5L)
  d$y <- rpois(nrow(d), 3)
  f <- mvgam_formula(y ~ 1, trend_formula = ~ AR(p = 1))

  sd_sorted <- standata(f, data = d, family = poisson(), validate = FALSE)
  expect_identical(as.integer(sd_sorted$obs_trend_time),
                   as.integer(d$time))

  # The same rows in another order describe the same series, so every
  # row has to keep the time index it had.
  set.seed(9L)
  shuffled <- d[sample(nrow(d)), , drop = FALSE]
  sd_shuf <- standata(f, data = shuffled, family = poisson(), validate = FALSE)
  expect_identical(as.integer(sd_shuf$obs_trend_time),
                   as.integer(shuffled$time))
})


test_that("each response on a wide frame gets its own latent series", {
  # A `brms::mvbf()` frame carries one row per time and one column
  # per response, so the series an observation belongs to is a
  # property of the (row, response) pair rather than of the row. Every
  # row of `count` sits on the `count` series and every row of `seen`
  # on the `seen` series, and the two must be different columns of the
  # trend matrix or the per-response families share one state and the
  # model is not the one the formula asks for.
  set.seed(1)
  T_ <- 20L
  dat <- data.frame(
    time  = seq_len(T_),
    count = rpois(T_, 5),
    seen  = rbinom(T_, 1, 0.5)
  )
  f <- bf(count ~ 1, family = poisson()) +
       bf(seen  ~ 1, family = bernoulli()) +
       set_rescor(FALSE)

  sd <- standata(
    mvgam_formula(f, trend_formula = ~ AR(p = 1)),
    data = dat, silent = 2L, validate = FALSE
  )

  expect_identical(as.integer(sd$N_series_trend), 2L)
  # Constant within a response, and a different constant per response.
  expect_identical(unique(sd$obs_trend_series_count), 1L)
  expect_identical(unique(sd$obs_trend_series_seen), 2L)
  # Both responses observe every time point.
  expect_identical(sd$obs_trend_time_count, seq_len(T_))
  expect_identical(sd$obs_trend_time_seen, seq_len(T_))
})

test_that("a wide frame does not require rows divisible by responses", {
  # The row count of a wide frame is the number of time points and has
  # no arithmetic relationship to the number of responses, so refusing
  # a frame whose rows do not divide by the response count rejects
  # ordinary data. Twenty times and three responses is the case that
  # stopped.
  set.seed(2)
  T_ <- 20L
  dat <- data.frame(
    time    = seq_len(T_),
    count   = rpois(T_, 5),
    seen    = rbinom(T_, 1, 0.5),
    biomass = rgamma(T_, 2, 0.5)
  )
  f <- bf(count   ~ 1, family = poisson()) +
       bf(seen    ~ 1, family = bernoulli()) +
       bf(biomass ~ 1, family = Gamma(link = "log")) +
       set_rescor(FALSE)

  sd <- standata(
    mvgam_formula(f, trend_formula = ~ AR(p = 1)),
    data = dat, silent = 2L, validate = FALSE
  )

  expect_identical(as.integer(sd$N_series_trend), 3L)
  series_of <- c(
    unique(sd$obs_trend_series_count),
    unique(sd$obs_trend_series_seen),
    unique(sd$obs_trend_series_biomass)
  )
  # One series each, and three distinct ones between them.
  expect_length(series_of, 3L)
  expect_setequal(series_of, 1:3)
})

test_that("an explicit series column supersedes the response axis", {
  # A user who supplies a series column is saying the responses are
  # measurements of one unit and share a latent state. That reading
  # has to keep winning: it is what a joint model over one site means,
  # and collapsing it to one series per response would silently fit a
  # different model.
  set.seed(3)
  T_ <- 20L
  dat <- data.frame(
    time   = seq_len(T_),
    series = factor(rep("site_a", T_)),
    count  = rpois(T_, 5),
    seen   = rbinom(T_, 1, 0.5)
  )
  f <- bf(count ~ 1, family = poisson()) +
       bf(seen  ~ 1, family = bernoulli()) +
       set_rescor(FALSE)

  sd <- standata(
    mvgam_formula(f, trend_formula = ~ AR(p = 1)),
    data = dat, silent = 2L, validate = FALSE
  )

  expect_identical(as.integer(sd$N_series_trend), 1L)
  expect_identical(unique(sd$obs_trend_series_count), 1L)
  expect_identical(unique(sd$obs_trend_series_seen), 1L)
})

test_that("the wide-frame trend design is indexed at time level", {
  # A wide frame holds one value per time in a covariate column, so a
  # trend covariate cannot vary by response and the trend-side design
  # carries one row per time. `times_trend[i, s]` therefore reads the
  # same design row for every series at time `i`, and `N_trend` counts
  # times rather than (time, series) cells. Emitting a cell-level
  # index against a time-level design would read past the end of it
  # as soon as a trend covariate is present.
  set.seed(4)
  T_ <- 20L
  dat <- data.frame(
    time  = seq_len(T_),
    env   = rnorm(T_),
    count = rpois(T_, 5),
    seen  = rbinom(T_, 1, 0.5)
  )
  f <- bf(count ~ 1, family = poisson()) +
       bf(seen  ~ 1, family = bernoulli()) +
       set_rescor(FALSE)

  sd <- standata(
    mvgam_formula(f, trend_formula = ~ env + AR(p = 1)),
    data = dat, silent = 2L, validate = FALSE
  )

  expect_identical(as.integer(sd$N_trend), T_)
  expect_identical(as.integer(sd$N_series_trend), 2L)
  expect_identical(dim(sd$times_trend), c(T_, 2L))
  # Every series reads the same design row at a given time.
  expect_identical(sd$times_trend[, 1], sd$times_trend[, 2])
  # And the design holds the covariate in time order.
  expect_equal(
    as.numeric(sd$X_trend[, ncol(sd$X_trend)]), dat$env,
    tolerance = 1e-8
  )
})

test_that("every response's GLM likelihood takes its trend", {
  # brms folds a response's predictor into its GLM call. The trend
  # reaches the likelihood only once that call is rewritten. The
  # rewrite once matched response keys of letters alone: `y1` and `y2`
  # kept their original calls. The model then compiled and sampled with
  # the trend computed and never used. A response formula without an
  # intercept passes the trend alone as the intercept argument.
  set.seed(5)
  T_ <- 20L
  dat <- data.frame(
    time = seq_len(T_), x = rnorm(T_),
    y1 = rpois(T_, 5), y2 = rpois(T_, 3)
  )
  gather <- function(r) paste0("to_vector(trend)[obs_trend_idx_", r, "]")
  y1_call <- paste0("poisson_log_glm_lpmf(Y_y1 | Xc_y1, Intercept_y1 + ",
                    gather("y1"), ", b_y1);")
  forms <- list(
    list(f = brms::mvbf(y1 ~ x, y2 ~ x, rescor = FALSE),
         y2_call = paste0("poisson_log_glm_lpmf(Y_y2 | Xc_y2, ",
                          "Intercept_y2 + ", gather("y2"), ", b_y2);")),
    list(f = brms::mvbf(y1 ~ x, y2 ~ 0 + x, rescor = FALSE),
         y2_call = paste0("poisson_log_glm_lpmf(Y_y2 | X_y2, ",
                          gather("y2"), ", b_y2);"))
  )
  for (form in forms) {
    pf <- mvgam(form$f, trend_formula = ~ AR(p = 1), data = dat,
                family = poisson(), run_model = FALSE, validate = FALSE)
    code <- stancode(pf)
    expect_true(stan_pattern(y1_call, code, fixed = TRUE))
    expect_true(stan_pattern(form$y2_call, code, fixed = TRUE))
    expect_identical(stan_match_count(code, "_glm_lpmf"), 2L)
  }
})


test_that("the stacked design names the columns the two sides share", {
  # A term written into both formulas moves the same fitted values
  # from either side. The observation and trend designs stacked on
  # one row per observation give the columns one likelihood sees at
  # once, and the rank of that matrix states whether the two
  # coefficients are separately identified.
  set.seed(9)
  nt <- 24
  d <- expand.grid(
    time = seq_len(nt), series = factor(c("a", "b", "c")),
    stringsAsFactors = TRUE
  )
  d$x <- rnorm(nrow(d))
  d$env <- rnorm(nrow(d))
  d$y <- rpois(nrow(d), 6)

  shared <- function(obs, tr, family = poisson()) {
    pf <- mvgam(obs, trend_formula = tr, data = d, family = family,
                run_model = FALSE, validate = FALSE)
    intercept <- "Intercept_trend" %in% brms_declared_params(stancode(pf))
    m <- stacked_design_matrix(standata(pf), intercept)
    if (is.null(m)) NULL else confounded_columns(m)
  }

  # A per-series latent level against an observation intercept.
  expect_match(shared(y ~ 1, ~ lv_axis() + AR(p = 1)), "^X_trend:series")

  # The same covariate on both sides.
  expect_identical(shared(y ~ x, ~ x + AR(p = 1)), "X_trend:x")

  # A trend intercept against the observation intercept. The trend
  # formula holds no design matrix to stack.
  expect_identical(shared(y ~ 1, ~ 1 + AR(p = 1)), "Intercept_trend")

  # Different covariates are separately identified.
  expect_length(shared(y ~ x, ~ env + AR(p = 1)), 0L)

  # A distributional parameter's design enters its own linear
  # predictor. Its intercept repeats the mean's direction alone.
  expect_length(
    shared(bf(y ~ x, sigma ~ series), ~ env + AR(p = 1), gaussian()), 0L
  )

  # The placeholder for an empty observation formula is a column of
  # zeros pinned at zero, and shares a direction with nothing.
  expect_length(shared(y ~ 0, ~ lv_axis() + AR(p = 1)), 0L)

  # A smooth repeated across the two formulas shares its whole basis.
  # A check over the parametric designs alone reports this pairing at
  # full rank.
  expect_true(any(grepl(
    "^Zs_1_1_trend", shared(y ~ s(env, k = 5), ~ s(env, k = 5) + AR(p = 1))
  )))

  # A trend carrying no design of its own gives nothing to compare.
  expect_null(shared(y ~ x, ~ AR(p = 1)))
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


test_that("a series missing a time is refused, and CAR completes it", {
  d <- data.frame(y = rpois(30, 3), time = rep(1:10, 3),
                  series = factor(rep(c("a", "b", "c"), each = 10)))
  ragged <- d[!(d$series == "b" & d$time == 4), ]
  err <- expect_error(
    stancode(mvgam_formula(y ~ 1, ~ AR()), data = ragged,
             family = poisson(), validate = FALSE),
    "must share one time grid"
  )
  msg <- conditionMessage(err)
  expect_match(msg, "'b'", fixed = TRUE)
  expect_match(msg, "'CAR()' takes series observed at their own times",
               fixed = TRUE)

  # A CAR trend holds a state for every series at every time. The
  # frame with the row absent and the frame with the row present and
  # its response missing are one model.
  car <- mvgam_formula(y ~ 1, ~ CAR(cor = TRUE))
  padded <- d
  padded$y[padded$series == "b" & padded$time == 4] <- NA
  sd_ragged <- standata(car, data = ragged, family = poisson(),
                        validate = FALSE)
  sd_padded <- suppressWarnings(
    standata(car, data = padded, family = poisson(), validate = FALSE)
  )
  expect_identical(sd_ragged, sd_padded)
  expect_identical(as.integer(sd_ragged$N_trend), 30L)
  expect_identical(as.integer(sd_ragged$N), 29L)

  # A multivariate t draws one mixing scale per time, and the times of
  # one series would then change the trend of another
  expect_error(
    stancode(mvgam_formula(y ~ 1, ~ CAR(df = 5)), data = ragged,
             family = poisson(), validate = FALSE),
    "'df' is not supported for series observed at their own times"
  )
  expect_no_error(suppressWarnings(
    stancode(mvgam_formula(y ~ 1, ~ CAR(df = 5)), data = padded,
             family = poisson(), validate = FALSE)
  ))
})

test_that("CAR measures time in gaps between a series' observations", {
  # Three series observed every two units, each offset from the
  # others. The union of their times has gaps of 0.01, and the unit
  # stays the gap one series is observed at.
  base <- seq(2, 20, by = 2)
  d <- data.frame(
    time = c(base, base + 0.01, base + 0.02),
    series = factor(rep(c("a", "b", "c"), each = length(base))),
    y = rep(1:5, 6)
  )
  sdata <- standata(mvgam_formula(y ~ 1, ~ CAR(cor = TRUE)), data = d,
                    family = poisson(), validate = FALSE)
  expect_equal(sort(as.numeric(sdata$gap_trend)), c(0.005, 0.99, 1))
})
