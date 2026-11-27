# The tests of test-trend-stancode-shape.R that assemble a model or run for
# 0.2 s or more. CRAN skips this file. CI and local runs take it.
skip_on_cran()
# Contract tests for trend Stan generators. Locks the
# scalar-vs-vector `p` semantics in AR and VAR.
#
# Semantics (intended):
#   AR(p = 3)        -> CONSECUTIVE lags 1, 2, 3
#                       (declares ar1_trend, ar2_trend, ar3_trend)
#   AR(p = c(2, 4))  -> SPARSE: only lags 2 and 4
#                       (declares ar2_trend, ar4_trend; no ar1)
#   VAR(p = 3)       -> CONSECUTIVE lags 1, 2, 3
#                       (declares array[3] A_trend matrices)
#   VAR(p = c(2, 4)) -> SPARSE: only lags 2 and 4
#                       (declares array[2] A_trend matrices
#                        plus an `active_lags_trend` data array)
#
# Tests build an `mvgam_formula` and call `stancode()` with
# `validate = FALSE` so no Stan compilation runs. Assertions
# use whitespace-tolerant literal substring matches against the
# generated Stan source.


# A literal snippet of Stan code, whitespace ignored
stan_has <- function(code, snippet) {
  stan_pattern(snippet, code, fixed = TRUE)
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

test_that("AR(p = 3) declares ar1_trend, ar2_trend, ar3_trend", {
  code <- get_trend_stancode(~ AR(p = 3))
  # Each coefficient multiplies the state at its own lag
  for (lag in 1:3) {
    expect_true(stan_has(code, paste0("[N_lv_trend] ar", lag, "_trend;")))
    expect_true(stan_has(
      code, paste0("ar", lag, "_trend[j] * lv_trend[i - ", lag, ", j]")
    ))
  }
})


# ----- AR sparse-lag p (intended: ONLY listed lags) -------------

test_that("AR(p = c(2, 4)) declares ar2_trend and ar4_trend only", {
  code <- get_trend_stancode(~ AR(p = c(2, 4)))
  expect_true(stan_has(code, "[N_lv_trend] ar2_trend;"))
  expect_true(stan_has(code, "[N_lv_trend] ar4_trend;"))
  expect_false(stan_has(code, "ar1_trend"))
  expect_false(stan_has(code, "ar3_trend"))
  expect_true(stan_has(code, "ar2_trend[j] * lv_trend[i - 2, j]"))
  expect_true(stan_has(code, "ar4_trend[j] * lv_trend[i - 4, j]"))
})


# ----- VAR scalar p (intended: CONSECUTIVE lags 1..p) ------------

test_that("VAR(p = 2) declares two lag matrices and a stationary start", {
  code <- get_trend_stancode(~ VAR(p = 2))
  expect_true(stan_has(
    code, "array[N_lags_trend] matrix[N_lv_trend, N_lv_trend] A_trend"
  ))
  sd <- standata(mvgam_formula(y ~ x, trend_formula = ~ VAR(p = 2)),
                 data = build_test_data(), family = poisson())
  expect_identical(as.integer(sd$N_lags_trend), 2L)
  # The Lyapunov solve has side (p m)^2 at every gradient. The map
  # that gives the coefficients also gives the autocovariances. Only
  # a VARMA, whose MA term changes the autocovariances, needs the solve.
  expect_true(stan_has(
    code, "Omega_trend = var_stationary_joint(result_var[2]);"
  ))
  expect_false(stan_has(code, "varma_autocov("))
})


# ---- heavy-tailed trend innovations (df) ---------------------------

tdf_data <- function() {
  set.seed(1)
  d <- expand.grid(time = 1:60, series = factor(paste0("s", 1:3)))
  d$y <- rpois(nrow(d), 5)
  d
}

# Each program is built and checked by stanc once
tdf_cache <- new.env(parent = emptyenv())
tdf_code <- function(tf) {
  key <- deparse1(tf)
  if (is.null(tdf_cache[[key]])) {
    tdf_cache[[key]] <- paste(unlist(stancode(
      mvgam_formula(y ~ 1, trend_formula = tf), data = tdf_data(),
      family = poisson(), backend = "cmdstanr"
    )), collapse = "\n")
  }
  tdf_cache[[key]]
}

nu_monitored <- function(tf) {
  spec <- mvgam:::parse_trend_formula(tf)$trend_model
  "nu_trend" %in% mvgam:::generate_monitor_params(spec)
}

test_that("df = Inf leaves the generated Stan code unchanged", {
  skip_if_not_installed("cmdstanr")
  # The default must be byte-identical to the Gaussian model, otherwise
  # every existing fit changes.
  gauss <- tdf_code(~ AR(p = 1))
  expect_identical(gauss, tdf_code(~ AR(p = 1, df = Inf)))
  expect_true(grepl(
    stan_prior_line("to_vector(innovations_trend)", "std_normal()"),
    gauss, fixed = TRUE
  ))
  expect_false(grepl("nu_trend", gauss, fixed = TRUE))
  expect_false(nu_monitored(~ AR(p = 1)))
})

test_that("df is honoured across every trend type that has innovations", {
  skip_if_not_installed("cmdstanr")
  # get_prior() takes the monitor list and stancode() the stanvar
  # generator. Both have to name `nu_trend`, under the registry default.
  default <- mvgam:::common_trend_priors$nu_trend$default
  for (tf in list(~ AR(p = 1, df = NA), ~ RW(df = NA), ~ ZMVN(df = NA),
                  ~ CAR(df = NA), ~ AR(p = 1, cor = TRUE, df = NA))) {
    sc <- tdf_code(tf)
    expect_true(grepl("multi_student_t_cholesky", sc, fixed = TRUE))
    expect_true(grepl("real<lower=2> nu_trend", sc, fixed = TRUE))
    expect_true(grepl(stan_prior_line("nu_trend", default), sc,
                      fixed = TRUE))
    expect_true(nu_monitored(tf))
  }
})

test_that("a fixed df needs no estimated parameter", {
  skip_if_not_installed("cmdstanr")
  sc <- tdf_code(~ AR(p = 1, df = 7))
  expect_true(grepl("multi_student_t_cholesky_lpdf", sc, fixed = TRUE))
  expect_true(grepl("| 7,", sc, fixed = TRUE))
  expect_false(grepl("real<lower=2> nu_trend", sc, fixed = TRUE))
  expect_false(nu_monitored(~ AR(p = 1, df = 7)))
})

test_that("a reported bound is the bound Stan declares", {
  # A bound is stated in the registry, in the type fallback and in any
  # per-trend resolver. The whole table is compared with the
  # declarations, every parameter of every form.
  declared_bounds <- function(code) {
    lines <- unlist(strsplit(code, "\n"))
    pat <- paste0("^\\s*(vector|real|array[^ ]*)\\s*",
                  "<([^>]*)>\\s*(\\[[^]]*\\])?\\s*",
                  "([A-Za-z_][A-Za-z0-9_]*)")
    out <- list()
    for (m in regmatches(lines, regexec(pat, lines))) {
      if (!length(m)) next
      cons <- m[3L]
      out[[m[5L]]] <- c(
        if (grepl("lower", cons)) {
          trimws(sub(".*lower\\s*=\\s*([^,>]*).*", "\\1", cons))
        } else "",
        if (grepl("upper", cons)) {
          trimws(sub(".*upper\\s*=\\s*([^,>]*).*", "\\1", cons))
        } else ""
      )
    }
    out
  }

  forms <- list(~ AR(p = 2), ~ AR(p = 1, ma = TRUE), ~ RW(ma = TRUE),
                ~ CAR(), ~ VAR(p = 1), ~ AR(p = 1, cor = TRUE))
  dat <- build_test_data()
  for (form in forms) {
    mf <- mvgam_formula(y ~ x, trend_formula = form)
    tab <- get_prior(mf, data = dat, family = poisson())
    stan <- declared_bounds(
      stancode(mf, data = dat, family = poisson(), validate = FALSE)
    )
    for (i in seq_len(nrow(tab))) {
      cls <- tab$class[i]
      if (!nzchar(cls) || is.null(stan[[cls]])) next
      reported <- c(tab$lb[i] %||% "", tab$ub[i] %||% "")
      reported[is.na(reported)] <- ""
      expect_identical(reported, stan[[cls]])
    }
  }
})
