# Contract tests for trend Stan generators. Locks the
# scalar-vs-vector `p` semantics in AR and VAR.
#
# Semantics (intended):
#   AR(p = 3)        -> CONSECUTIVE lags 1, 2, 3
#                       (declares ar1_trend, ar2_trend, ar3_trend)
#   AR(p = c(2, 4))  -> SPARSE: only lags 2 and 4
#                       (declares ar2_trend, ar4_trend; no ar1)
#   VAR(p = 3)       -> CONSECUTIVE lags 1, 2, 3
#                       (declares array[3] A_raw_trend matrices)
#   VAR(p = c(2, 4)) -> SPARSE: only lags 2 and 4
#                       (declares array[2] A_raw_trend matrices
#                        plus an `active_lags_trend` data array)
#
# Tests build an `mvgam_formula` and call `stancode()` with
# `validate = FALSE` so no Stan compilation runs. Assertions
# use whitespace-tolerant literal substring matches against the
# generated Stan source.


# Whitespace-tolerant literal substring matcher. Tests assert
# that a specific Stan declaration or dynamics expression is
# present; brackets / angle brackets / parentheses come along
# verbatim. Stripping whitespace from both sides handles
# formatter differences; `fixed = TRUE` sidesteps regex
# escaping entirely.
stan_has <- function(code, snippet) {
  remove_ws <- function(text) gsub("\\s+", "", text)
  grepl(remove_ws(snippet), remove_ws(code), fixed = TRUE)
}


build_test_data <- function() {
  set.seed(42L)
  data.frame(
    time = 1:24,
    series = factor(rep("series1", 24)),
    y = stats::rpois(24, lambda = 5),
    x = stats::rnorm(24)
  )
}


get_trend_stancode <- function(form, dat = NULL,
                                family = poisson()) {
  if (is.null(dat)) dat <- build_test_data()
  mf <- mvgam_formula(y ~ x, trend_formula = form)
  stancode(mf, data = dat, family = family, validate = FALSE)
}


# ----- AR scalar p (intended: CONSECUTIVE lags 1..p) ------------

test_that("AR(p = 1) declares ar1_trend only", {
  code <- get_trend_stancode(~ AR(p = 1))
  expect_true(stan_has(code, "[N_lv_trend] ar1_trend;"))
  expect_false(stan_has(code, "ar2_trend"))
  expect_false(stan_has(code, "ar3_trend"))
})

test_that("AR(p = 2) declares ar1_trend AND ar2_trend", {
  code <- get_trend_stancode(~ AR(p = 2))
  expect_true(stan_has(code, "[N_lv_trend] ar1_trend;"))
  expect_true(stan_has(code, "[N_lv_trend] ar2_trend;"))
  expect_false(stan_has(code, "ar3_trend"))
  # Dynamics must reference both lag-1 AND lag-2 of lv_trend.
  expect_true(stan_has(code, "lv_trend[i - 1, j]"))
  expect_true(stan_has(code, "lv_trend[i - 2, j]"))
})

test_that("AR(p = 3) declares ar1_trend, ar2_trend, ar3_trend", {
  code <- get_trend_stancode(~ AR(p = 3))
  expect_true(stan_has(code, "[N_lv_trend] ar1_trend;"))
  expect_true(stan_has(code, "[N_lv_trend] ar2_trend;"))
  expect_true(stan_has(code, "[N_lv_trend] ar3_trend;"))
  expect_true(stan_has(code, "lv_trend[i - 3, j]"))
})


# ----- AR sparse-lag p (intended: ONLY listed lags) -------------

test_that("AR(p = c(2, 4)) declares ar2_trend and ar4_trend only", {
  code <- get_trend_stancode(~ AR(p = c(2, 4)))
  expect_true(stan_has(code, "[N_lv_trend] ar2_trend;"))
  expect_true(stan_has(code, "[N_lv_trend] ar4_trend;"))
  expect_false(stan_has(code, "ar1_trend"))
  expect_false(stan_has(code, "ar3_trend"))
  expect_true(stan_has(code, "lv_trend[i - 2, j]"))
  expect_true(stan_has(code, "lv_trend[i - 4, j]"))
})


# ----- initial_joint_var() out-of-bounds guard ------------------

test_that("initial_joint_var guards the q=0 (pure VAR) case", {
  # Pure VAR (no MA) has q = 0. The companion_var matrix is then
  # sized (p + q) * m = p * m, so the MA-block writes at indices
  # (p * m + 1):((p + 1) * m) are out of bounds. The Stan
  # function must wrap those writes in `if (q > 0)`. Without this
  # guard every pure VAR fit crashed at runtime with an
  # 'accessing element out of range' exception.
  code <- get_trend_stancode(~ VAR(p = 1))
  expect_true(stan_has(code, "if (q > 0)"))
  expect_true(stan_has(code, "companion_var[1:m, 1:m] = Sigma;"))
})


# ----- VAR scalar p (intended: CONSECUTIVE lags 1..p) ------------

test_that("VAR(p = 1) declares array[1] A_raw_trend", {
  code <- get_trend_stancode(~ VAR(p = 1))
  expect_true(stan_has(
    code, "array[1] matrix[N_lv_trend, N_lv_trend] A_raw_trend"
  ))
  expect_true(stan_has(code, "[N_lv_trend] sigma_trend;"))
})

test_that("VAR(p = 2) declares array[2] A_raw_trend", {
  code <- get_trend_stancode(~ VAR(p = 2))
  expect_true(stan_has(
    code, "array[2] matrix[N_lv_trend, N_lv_trend] A_raw_trend"
  ))
})


# ----- VAR sparse-lag p (currently rejected) --------------------

test_that("VAR(p = c(2, 4)) errors with an informative message", {
  # Sparse-lag VAR is not yet supported: the Heaps-2022
  # stationary joint-distribution initialisation assumes a
  # consecutive companion-form structure, and deriving the
  # sparse-companion stationary covariance is a separate piece
  # of work. The constructor rejects vector 'p' with a clear
  # message that points users at AR(p = c(...)) for the
  # univariate sparse-lag case.
  expect_error(
    VAR(p = c(2, 4)),
    "Sparse-lag VAR"
  )
})


# ---- heavy-tailed trend innovations (df) ---------------------------

tdf_data <- function() {
  set.seed(1)
  d <- expand.grid(time = 1:60, series = factor(paste0("s", 1:3)))
  d$y <- rpois(nrow(d), 5)
  d
}

tdf_code <- function(tf) {
  paste(unlist(stancode(
    mvgam_formula(y ~ 1, trend_formula = tf), data = tdf_data(),
    family = poisson(), backend = "cmdstanr"
  )), collapse = "\n")
}

test_that("is_gaussian_df() treats Inf and NULL as Gaussian", {
  # A t with infinite degrees of freedom is a normal, so Inf is the
  # exact spelling of the default rather than a sentinel value.
  expect_true(mvgam:::is_gaussian_df(Inf))
  expect_true(mvgam:::is_gaussian_df(NULL))
  expect_false(mvgam:::is_gaussian_df(NA))
  expect_false(mvgam:::is_gaussian_df(5))
})

test_that("innovation_sampling_code() emits normal or multivariate t", {
  expect_identical(
    mvgam:::innovation_sampling_code("N_lv_trend", Inf),
    "to_vector(innovations_trend) ~ std_normal();"
  )
  est <- paste(mvgam:::innovation_sampling_code("N_lv_trend", NA),
               collapse = "\n")
  expect_true(grepl("multi_student_t_cholesky(nu_trend,", est, fixed = TRUE))
  fixed <- paste(mvgam:::innovation_sampling_code("N_lv_trend", 7),
                 collapse = "\n")
  expect_true(grepl("multi_student_t_cholesky(7,", fixed, fixed = TRUE))
  # The identity scale is deliberate: the correlation transform stays in
  # transformed parameters, and a linear map of a multivariate t is a
  # multivariate t with the mapped scale matrix.
  expect_true(grepl("identity_matrix(", est, fixed = TRUE))
})

test_that("nu_trend is declared and given a prior only when estimated", {
  expect_null(mvgam:::nu_trend_stanvars(Inf))
  expect_null(mvgam:::nu_trend_stanvars(7))
  sv <- mvgam:::nu_trend_stanvars(NA)
  blocks <- vapply(sv, function(x) x$block, character(1))
  expect_setequal(blocks, c("parameters", "model"))
})

test_that("df = Inf leaves the generated Stan code unchanged", {
  # The default must be byte-identical to the Gaussian model, otherwise
  # every existing fit changes.
  expect_identical(tdf_code(~ AR(p = 1)), tdf_code(~ AR(p = 1, df = Inf)))
  expect_true(grepl("to_vector(innovations_trend) ~ std_normal();",
                    tdf_code(~ AR(p = 1)), fixed = TRUE))
  expect_false(grepl("nu_trend", tdf_code(~ AR(p = 1)), fixed = TRUE))
})

test_that("df is honoured across every trend type that has innovations", {
  for (tf in list(~ AR(p = 1, df = NA), ~ RW(df = NA), ~ ZMVN(df = NA),
                  ~ CAR(df = NA), ~ AR(p = 1, cor = TRUE, df = NA))) {
    sc <- tdf_code(tf)
    expect_true(grepl("multi_student_t_cholesky", sc, fixed = TRUE))
    expect_true(grepl("real<lower=2> nu_trend", sc, fixed = TRUE))
    expect_true(grepl("nu_trend ~ gamma(4, 0.3)", sc, fixed = TRUE))
  }
})

test_that("a fixed df needs no estimated parameter", {
  sc <- tdf_code(~ AR(p = 1, df = 7))
  expect_true(grepl("multi_student_t_cholesky(7,", sc, fixed = TRUE))
  expect_false(grepl("real<lower=2> nu_trend", sc, fixed = TRUE))
})

test_that("nu_trend is monitored exactly when the Stan code declares it", {
  # get_prior() reads the monitor list while stancode() reads the
  # stanvar generator. Were the two to disagree, get_prior() would
  # offer a prior for a parameter the model never declares.
  cases <- list(
    list(tf = ~ AR(p = 1), spec = AR(p = 1)),
    list(tf = ~ AR(p = 1, df = NA), spec = AR(p = 1, df = NA)),
    list(tf = ~ AR(p = 1, df = 7), spec = AR(p = 1, df = 7)),
    list(tf = ~ RW(df = NA), spec = RW(df = NA)),
    list(tf = ~ VAR(), spec = VAR())
  )
  for (case in cases) {
    monitored <- "nu_trend" %in% mvgam:::generate_monitor_params(case$spec)
    declared <- grepl("real<lower=2> nu_trend", tdf_code(case$tf),
                      fixed = TRUE)
    expect_identical(monitored, declared)
  }
})

test_that("one registry entry drives both stancode and get_prior", {
  # Two independent copies of the default would drift apart without any
  # test noticing, so pin both surfaces to the registry string itself.
  default <- mvgam:::common_trend_priors$nu_trend$default
  expect_true(grepl(paste0("nu_trend ~ ", default),
                    tdf_code(~ AR(p = 1, df = NA)), fixed = TRUE))
  prior_tab <- get_prior(
    mvgam_formula(y ~ 1, trend_formula = ~ AR(p = 1, df = NA)),
    data = tdf_data(), family = poisson()
  )
  expect_identical(prior_tab$prior[prior_tab$class == "nu_trend"], default)
})

test_that("assert_trend_df() rejects values with no finite variance", {
  # The AR stationary initialisation divides by sqrt(1 - phi^2), which
  # presumes a finite second moment; a t has one only above 2.
  expect_error(AR(df = 2), "greater than 2")
  expect_error(AR(df = 1.5), "greater than 2")
  expect_error(AR(df = -Inf), "positive")
  expect_error(AR(df = "a"), "single number")
  expect_error(AR(df = c(3, 4)), "single number")
  expect_identical(AR(df = Inf)$df, Inf)
  expect_identical(AR(df = NA)$df, NA_real_)
  expect_identical(AR(df = 7)$df, 7)
})

test_that("trend types without innovations do not accept df", {
  # VAR samples its states directly and piecewise trends are
  # deterministic, so neither has innovations to make heavy-tailed.
  expect_error(VAR(df = 5), "unused argument")
  expect_error(PW(df = 5), "unused argument")
})

test_that("draw_trend_innovations() matches the Stan innovation distribution", {
  set.seed(9)
  g <- mvgam:::draw_trend_innovations(20000L, 3L, Inf)
  expect_equal(var(g[, 1]), 1, tolerance = 0.05)

  t5 <- mvgam:::draw_trend_innovations(50000L, 3L, 5)
  # variance of a standard t is df / (df - 2)
  expect_equal(var(t5[, 1]), 5 / 3, tolerance = 0.15)

  # Series are uncorrelated but share the mixing value, so a shock
  # reaches all of them at once. Element-wise student_t() would leave
  # the absolute values independent.
  expect_equal(cor(t5[, 1], t5[, 2]), 0, tolerance = 0.05)
  expect_true(cor(abs(t5[, 1]), abs(t5[, 2])) > 0.1)
  expect_true(cor(abs(g[, 1]), abs(g[, 2])) < 0.05)
})

test_that("draw_trend_innovations() validates its arguments", {
  expect_error(mvgam:::draw_trend_innovations(10L, 2L, 2), "df")
  expect_error(mvgam:::draw_trend_innovations(10L, 0L, Inf), "n_series")
})
