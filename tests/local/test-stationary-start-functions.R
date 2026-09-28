# The Stan code an AR() trend starts its latent states with, compiled
# on its own and checked against the definition.
#
# Correlated or grouped innovations at any order, and a moving-average
# term above one lag, start through `joint_init_stanblock()`. It draws
# the `p` most recent states of a stationary AR(p) path from the
# covariance `ar_diag_joint_var()` returns, and with a moving-average
# term filters that path. The coefficients are diagonal, which lets
# `ar_diag_joint_var()` solve each pair of series on its own.
#
# A fitted model tests this code on one shape, through whatever draws
# the sampler visited. Compiled here, the emitted blocks meet random
# shapes across series counts, orders and signs, and the near-unit and
# cancelling corners, at a cost of one compile.
#
# Ground truth is the Neumann sum `Omega = sum_k A^k Q A'^k` over the
# full companion, `neumann_omega()` in helper-local.R. It calls nothing
# from the package.
#
# Run with:
#   testthat::test_file("tests/local/test-stationary-start-functions.R")

suppressMessages({
  devtools::load_all(".", quiet = TRUE)
  library(testthat)
})


# -- The program ---------------------------------------------------------

# Each start below wraps the block the generator emits in a function
# of the coefficients, the innovation covariance and the standard
# variates. It returns the states `lv_1, ..., lv_p` and, in row p + 1,
# the scaled innovation at t = p, the one a moving-average step at
# t = p + 1 reuses. The block is linear in the variates, and the
# function is evaluated at each unit variate to recover that map.
#
# `joint_init_stanblock()` takes three sets of series: every series
# (`":"`), the members of one group, and each series alone. Independent
# innovations take the scalar closed form at one lag (`"scalar"`) and
# `ar_stationary_init()` above one lag without a moving-average term
# (`"pacf"`), whose arguments are partial autocorrelations.
start_function <- function(name, p, has_ma, set) {
  stem <- if (set == "pacf") "_pacf_trend" else "_trend"
  ar_args <- paste0("vector ar", seq_len(p), stem, collapse = ", ")
  block <- switch(set,
    scalar = scalar_init_stanblock(has_ma),
    pacf = pacf_init_stanblock(p),
    all = joint_init_stanblock(p, "N_lv_trend", "Sigma_trend", ":",
                               has_ma),
    group = joint_init_stanblock(
      p, "size(members)", "Sigma_trend[members, members]", "members",
      has_ma
    ),
    single = joint_init_stanblock(
      p, "1", "rep_matrix(square(sigma_trend[j_ser]), 1, 1)", "{j_ser}",
      has_ma, open = "      for (j_ser in 1:N_lv_trend) {\n"
    )
  )
  paste0(
    "  matrix ", name, "(matrix Sigma_trend, ", ar_args,
    ", vector theta1_trend, matrix innovations_trend,\n",
    "      vector init_innovations_trend",
    if (set == "group") ", array[] int members", ") {\n",
    "    int N_lv_trend = rows(Sigma_trend);\n",
    "    vector[N_lv_trend] sigma_trend = sqrt(diagonal(Sigma_trend));\n",
    "    matrix[", p, ", N_lv_trend] scaled_innovations_trend\n",
    "      = innovations_trend * cholesky_decompose(Sigma_trend)';\n",
    "    matrix[", p + 1L, ", N_lv_trend] lv_trend\n",
    "      = rep_matrix(0, ", p + 1L, ", N_lv_trend);\n",
    block, "\n",
    "    lv_trend[", p + 1L, "] = scaled_innovations_trend[", p, "];\n",
    "    return lv_trend;\n",
    "  }\n"
  )
}

switch_set <- function(set, p, has_ma) {
  ifelse(set == "scalar", p == 1L,
    ifelse(set == "pacf", p > 1L & !has_ma,
      ifelse(set == "single", p > 1L & has_ma, TRUE)))
}

starts <- expand.grid(p = 1:3, has_ma = c(FALSE, TRUE),
                      set = c("all", "group", "single", "scalar", "pacf"),
                      stringsAsFactors = FALSE)
# The shapes each generator branch emits for, the ones
# `generate_ar_trend_stanvars()` routes there.
starts <- starts[with(starts, switch_set(set, p, has_ma)), ]
starts$name <- sprintf("start_p%d_%s_%s", starts$p,
                       ifelse(starts$has_ma, "ma", "ar"), starts$set)

sv <- c(stationary_joint_functions_stanvar(kernel = "ar"),
        ar_pacf_functions_stanvar(with_init = TRUE))
stan_file <- cmdstanr::write_stan_file(paste0(
  "functions {\n", sv[[1]]$scode, "\n", sv[[2]]$scode, "\n",
  paste(unlist(Map(start_function, starts$name, starts$p, starts$has_ma,
                   starts$set)), collapse = "\n"),
  "}\n"
))
mod <- cmdstanr::cmdstan_model(stan_file, compile_standalone = TRUE,
                               quiet = TRUE)
ar_diag_joint_var <- mod$functions$ar_diag_joint_var


# -- Random inputs -------------------------------------------------------

# Stationary coefficients per series, from partial autocorrelations
# through the Levinson-Durbin recursion.
pacf_to_coef <- function(r) {
  phi <- r[1L]
  for (k in seq_along(r)[-1L]) phi <- c(phi - r[k] * rev(phi), r[k])
  phi
}

# `pacf` is one row per lag and one column per series.
with_pacf <- function(sh, pacf) {
  coefs <- apply(pacf, 2L, pacf_to_coef)
  coefs <- matrix(coefs, nrow = nrow(pacf))
  sh$pacf <- lapply(seq_len(nrow(pacf)), function(k) pacf[k, ])
  sh$phi <- lapply(seq_len(nrow(pacf)), function(k) coefs[k, ])
  sh
}

random_shape <- function(m, p) {
  B <- matrix(stats::rnorm(m * m), m, m)
  with_pacf(
    list(Sigma = crossprod(B) + diag(m),
         theta = stats::runif(m, -0.9, 0.9)),
    matrix(stats::runif(p * m, -0.9, 0.9), p, m)
  )
}

set.seed(8812L)
shapes <- lapply(1:24, function(i) {
  random_shape(sample(1:5, 1L), sample(1:4, 1L))
})
# A leading partial autocorrelation of 0.99 puts the process a step
# from a unit root, where the stationary variance runs to many times
# the innovation variance and the solve is least well conditioned.
near_unit <- with_pacf(random_shape(3L, 2L),
                       rbind(c(0.99, -0.99, 0.95), 0.5))
# `theta = -ar1` at one lag cancels the two polynomials, and the path
# is the innovation sequence itself. The AR(1) covariance factored in
# the start stays positive definite there.
cancelling <- random_shape(3L, 1L)
cancelling$theta <- -cancelling$phi[[1L]]

# The near-unit shape carries variances above 200, and the solve and
# the sum each lose about two digits of relative precision there. A
# stored draw holds six significant digits, and every construction
# error the separation checks below show moves the covariance by more
# than 0.1.
tol <- 1e-8


# -- The joint covariance ------------------------------------------------

test_that("ar_diag_joint_var matches the full companion's Neumann sum", {
  for (sh in c(Filter(function(s) length(s$phi) > 1L, shapes),
               list(near_unit))) {
    expect_equal(ar_diag_joint_var(sh$Sigma, sh$phi),
                 neumann_omega(sh$Sigma, sh$phi), tolerance = tol)
  }
})


test_that("the pairwise solve places each companion on its own side", {
  # The error the pairwise layout invites: `X = A_b X A_a' + Q` in
  # place of `X = A_a X A_b' + Q`, which transposes every cross-series
  # block. A single series cannot show it, and neither can two series
  # sharing their coefficients. The first shape with three or more
  # series and two or more lags separates the two by more than 0.1,
  # which shows the block above tests the orientation.
  sh <- Filter(function(s) nrow(s$Sigma) >= 3L && length(s$phi) >= 2L,
               shapes)[[1L]]
  m <- nrow(sh$Sigma)
  p <- length(sh$phi)
  comp <- function(a) {
    A <- matrix(0, p, p)
    A[1L, ] <- vapply(sh$phi, `[`, numeric(1), a)
    for (k in seq_len(p)[-1L]) A[k, k - 1L] <- 1
    A
  }
  Q <- matrix(0, p, p)
  Q[1L, 1L] <- 1
  swapped <- matrix(0, p * m, p * m)
  for (a in seq_len(m)) {
    for (b in seq_len(m)) {
      X <- matrix(solve(diag(p * p) - kronecker(comp(a), comp(b)),
                        as.vector(Q)), p, p)
      for (i in seq_len(p)) {
        for (j in seq_len(p)) {
          swapped[(i - 1L) * m + a, (j - 1L) * m + b] <-
            sh$Sigma[a, b] * X[i, j]
        }
      }
    }
  }
  expect_gt(max(abs(swapped - neumann_omega(sh$Sigma, sh$phi))), 0.1)
})


# -- The emitted start ---------------------------------------------------

# The covariance of the companion state at t = p that one start
# implies: `(lv_p, ..., lv_1)`, with `eps_p` appended under a
# moving-average term. Column k of the map is the start evaluated at
# the k-th unit variate, taken over innovation rows 1 to p and then
# `init_innovations_trend`.
implied_state_cov <- function(start, sh, members) {
  m <- nrow(sh$Sigma)
  p <- start$p
  fn <- mod$functions[[start$name]]
  n_var <- (p + 1L) * m
  rows <- c(rev(seq_len(p)), if (start$has_ma) p + 1L)
  cols <- vapply(seq_len(n_var), function(k) {
    u <- replace(numeric(n_var), k, 1)
    args <- c(
      list(sh$Sigma), if (start$set == "pacf") sh$pacf else sh$phi,
      list(sh$theta, matrix(u[seq_len(p * m)], p, m, byrow = TRUE),
           u[p * m + seq_len(m)])
    )
    if (start$set == "group") args <- c(args, list(members))
    out <- do.call(fn, args)
    as.vector(t(out[rows, members, drop = FALSE]))
  }, numeric(length(rows) * length(members)))
  tcrossprod(matrix(cols, ncol = n_var))
}

test_that("every emitted start holds the stationary law of its state", {
  # One state at t = p fixes every later occasion, which the recursion
  # generates from it. The grouped start takes the first and third of
  # three series, leaving a gap in the set. The single-series, scalar
  # and pacf starts model independent innovations, and their truth
  # takes the diagonal of the covariance.
  checked <- character(0)
  for (i in seq_len(nrow(starts))) {
    start <- starts[i, ]
    pool <- c(shapes, list(near_unit), if (start$p == 1L) list(cancelling))
    for (sh in Filter(function(s) length(s$phi) == start$p, pool)) {
      m <- nrow(sh$Sigma)
      if (start$set == "group" && m < 3L) next
      if (start$set %in% c("single", "scalar", "pacf")) {
        sh$Sigma <- diag(diag(sh$Sigma), m)
      }
      members <- if (start$set == "group") c(1L, 3L) else seq_len(m)
      theta <- if (start$has_ma) sh$theta[members]
      want <- neumann_omega(sh$Sigma[members, members, drop = FALSE],
                            lapply(sh$phi, `[`, members), theta)
      expect_equal(implied_state_cov(start, sh, members), want,
                   tolerance = tol)
      checked <- c(checked, start$name)
    }
  }
  # Every start meets at least one shape.
  expect_setequal(checked, starts$name)
})


test_that("the design check separates the filter from its neighbours", {
  # A start that took the moving-average term at the wrong lag, or
  # left it out, would settle somewhere else. Dropping `theta` from
  # the truth moves the covariance of a moving-average start by more
  # than 0.1, which shows the assertion above tests the filter.
  start <- starts[starts$name == "start_p2_ma_all", ]
  sh <- Filter(function(s) length(s$phi) == 2L, shapes)[[1L]]
  members <- seq_len(nrow(sh$Sigma))
  pm <- 2L * length(members)
  got <- implied_state_cov(start, sh, members)[seq_len(pm), seq_len(pm)]
  expect_gt(max(abs(got - neumann_omega(sh$Sigma, sh$phi))), 0.1)
})


test_that("a joint start holds its law a step from a unit root", {
  # A leading partial autocorrelation of 0.9999 puts the spectral radius
  # at 0.99997, and the stationary variance above 10^4 times the
  # innovation variance. The covariance factored there is the least well
  # conditioned a stationary draw reaches, and a Cholesky failure would
  # reject the draw. The sum needs 5 x 10^5 terms to settle, and the
  # solve loses about four digits of relative precision.
  sh <- with_pacf(random_shape(3L, 2L), rbind(c(0.9999, 0.999, -0.9999),
                                              0.5))
  start <- starts[starts$name == "start_p2_ar_all", ]
  members <- seq_len(3L)
  want <- neumann_omega(sh$Sigma, sh$phi, n_terms = 500000L)
  got <- implied_state_cov(start, sh, members)
  expect_equal(got, want, tolerance = 1e-6)
})


cat("\nDone.\n")
