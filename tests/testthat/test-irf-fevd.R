## Coverage for irf.mvgam / fevd.mvgam / stability.mvgam. Three layers
## of test:
##   1. Trend-type gate (`assert_var_trend`) on a non-VAR mock.
##   2. Posterior extractor (`extract_var_posterior`) on a mock
##      `posterior::draws_matrix` with the right Stan column names.
##   3. Math helpers (`var_phi`, `var_psi`, `var_fecov`, `gen_irf`,
##      `gen_fevd`) on hand-built A / Sigma where the answer is
##      analytically known.
##
## Fitted coverage is in `tests/local/test-trend-var.R` and
## `tests/local/test-factor-var.R`.

# ---- Build a minimal mvgam stub backed by a draws_matrix ----------

# `extract_var_posterior` only needs (i) object$fit to be something
# `posterior::as_draws_matrix` accepts and (ii) the trend type and the
# Stan data to resolve K. A draws_matrix dispatches through the
# posterior generic as the identity, so we can hand one in directly
# without going through Stan.
build_var_mock <- function(K = 3L, ndraws = 50L,
                           trend_type = "VAR", seed = 1L) {
  set.seed(seed)
  cols <- character(0L)
  for (i in seq_len(K)) for (j in seq_len(K)) {
    cols <- c(cols, sprintf("Phi_trend[1,%d,%d]", i, j))
  }
  for (i in seq_len(K)) for (j in seq_len(K)) {
    cols <- c(cols, sprintf("Sigma_trend[%d,%d]", i, j))
  }
  vals <- matrix(rnorm(ndraws * length(cols), sd = 0.1),
                 nrow = ndraws, ncol = length(cols),
                 dimnames = list(NULL, cols))
  # Force the diagonal of Sigma_trend to be positive so the math
  # helpers (chol, eigen, kronecker inverse) stay numerically sane.
  for (i in seq_len(K)) {
    sig_d <- sprintf("Sigma_trend[%d,%d]", i, i)
    vals[, sig_d] <- abs(vals[, sig_d]) + 0.5
  }
  draws <- posterior::as_draws_matrix(vals)

  structure(
    list(
      fit = draws,
      trend_metadata = list(trend_type = trend_type),
      standata = list(N_lv_trend = K, N_series_trend = K)
    ),
    class = "mvgam"
  )
}

# The math kernels are reached only through `var_draw_surfaces()`,
# which hands them the process labels beside the matrices. A stub
# omitting those describes an object no caller builds.
var_kernel_input <- function(K, A, Sigma = diag(K),
                             D = array(0, c(K, K, 0L)),
                             labels = paste0("s", seq_len(K))) {
  list(K = K, A = array(A, c(K, K, length(A) / K^2)), D = D,
       Sigma = Sigma, labels = labels)
}

# ---- Gate ----------------------------------------------------------

test_that("assert_var_trend() rejects non-VAR fits with a clear pointer", {
  fake <- structure(
    list(trend_metadata = list(trend_type = "RW")),
    class = "mvgam"
  )
  expect_error(
    assert_var_trend(fake, surface = "irf()"),
    "requires a VAR latent trend"
  )
  expect_error(
    assert_var_trend(fake, surface = "irf()"),
    "trend type is 'RW'"
  )
  expect_true(assert_var_trend(build_var_mock(K = 2L, ndraws = 2L),
                               surface = "irf()"))
})

# ---- Extractor -----------------------------------------------------

test_that("extract_var_posterior() reads the correct column for each (i, j)", {
  # Stamp known values into Phi_trend[1,2,3] and Sigma_trend[1,1] so we
  # can verify the indexing convention round-trips correctly.
  mock <- build_var_mock(K = 3L, ndraws = 4L)
  mock$fit[1L, "Phi_trend[1,2,3]"] <- 0.7
  mock$fit[2L, "Sigma_trend[1,1]"] <- 1.25
  vp <- extract_var_posterior(mock)
  expect_equal(vp$A[1L, 2L, 3L, 1L], 0.7)
  expect_equal(vp$Sigma[2L, 1L, 1L], 1.25)
})

test_that(
  "extract_var_posterior() errors on a missing column", {
  mock <- build_var_mock(K = 2L, ndraws = 3L)
  drop_col <- "Phi_trend[1,1,1]"
  mock$fit <- mock$fit[, setdiff(colnames(mock$fit), drop_col), drop = FALSE]
  # The refusal names the cell the posterior lacks.
  expect_error(
    extract_var_posterior(mock),
    "Missing: Phi_trend\\[1,1,1\\]"
  )
})

# ---- Math helpers --------------------------------------------------

test_that("var_phi() recovers A^k for diagonal A", {
  K <- 2L
  A <- diag(c(0.5, 0.3))
  x <- var_kernel_input(K, A)
  Phi <- var_phi(x, h = 3L)
  # Phi[, , k+1] = A^k for the i.i.d. case with diagonal A.
  expect_equal(Phi[, , 1L], diag(K))
  expect_equal(Phi[, , 2L], A)
  expect_equal(Phi[, , 3L], A %*% A)
  expect_equal(Phi[, , 4L], A %*% A %*% A)
})

test_that("var_phi() matches a VAR(2) companion and a VARMA impulse", {
  K <- 2L
  set.seed(2L)
  A <- array(stats::rnorm(K * K * 2L, sd = 0.25), c(K, K, 2L))
  D <- array(stats::rnorm(K * K, sd = 0.3), c(K, K, 1L))
  # A VAR(2)'s weights are the top-left block of the companion powers.
  comp <- rbind(cbind(A[, , 1L], A[, , 2L]), cbind(diag(K), 0 * diag(K)))
  Phi <- var_phi(var_kernel_input(K, A), h = 3L)
  expect_equal(Phi[, , 4L], (comp %*% comp %*% comp)[1:K, 1:K])
  # A VARMA(2, 1)'s first weight adds the moving-average matrix.
  Phi_ma <- var_phi(var_kernel_input(K, A, D = D), h = 2L)
  expect_equal(Phi_ma[, , 2L], A[, , 1L] + D[, , 1L])
  expect_equal(Phi_ma[, , 3L],
               A[, , 1L] %*% Phi_ma[, , 2L] + A[, , 2L])
})

test_that("gen_fevd() rows sum to 1 across the columns for each horizon", {
  K <- 3L
  set.seed(7L)
  # Random stable A by shrinking eigenvalues into the unit circle.
  A_raw <- matrix(stats::rnorm(K * K, sd = 0.4), K, K)
  ev <- max(abs(eigen(A_raw)$values))
  A <- A_raw / (1.2 * ev)
  Sigma <- crossprod(matrix(stats::rnorm(K * K), K, K)) + diag(K)
  x <- var_kernel_input(K, A, Sigma)
  fevd <- gen_fevd(x, h = 5L)
  for (i in seq_len(K)) {
    # For each "responding" series i, the contributions across all K
    # shock origins should sum to 1 at every horizon.
    row_sums <- rowSums(fevd[[i]])
    expect_equal(row_sums, rep(1.0, 5L))
  }
  # At the first horizon a response's shares are the squared row of
  # the lower Cholesky factor of Sigma, normalised
  P <- t(chol(Sigma))
  for (i in seq_len(K)) {
    expect_equal(unname(fevd[[i]][1L, ]), P[i, ]^2 / sum(P[i, ]^2))
  }
})

test_that("the generalised IRF is the scaled column of Sigma, propagated", {
  # Pesaran and Shin (1998): a shock to j moves the system by
  # `Sigma[, j] / sqrt(Sigma[j, j])` and then by A times that. Each
  # list element is one shock, with horizons in rows and responses in
  # columns.
  K <- 2L
  A <- matrix(c(0.4, 0.1, 0.05, 0.3), K, K)
  Sigma <- matrix(c(1, 0.4, 0.4, 2), K, K)
  x <- var_kernel_input(K, A, Sigma)
  irf_gen <- gen_irf(x, h = 3L, cumulative = FALSE, orthogonal = FALSE)
  for (j in seq_len(K)) {
    impact <- Sigma[, j] / sqrt(Sigma[j, j])
    expect_equal(unname(irf_gen[[j]][1L, ]), impact)
    expect_equal(unname(irf_gen[[j]][2L, ]), as.numeric(A %*% impact))
  }
})

test_that("orthogonalised IRF at horizon 1 equals P %*% e_j", {
  # For A = 0 and diagonal Sigma the correct OIRF at horizon 1
  # from shock j to response i is P[i, j], where P is the lower
  # Cholesky factor of Sigma. This is the Sims (1980) /
  # Lutkepohl (2007) definition and would not hold if the
  # Cholesky factor were applied twice on the right-hand side.
  K <- 2L
  A <- matrix(0, K, K)
  Sigma <- diag(c(1, 4))
  x <- var_kernel_input(K, A, Sigma)
  irf_orth <- gen_irf(x, h = 1L, cumulative = FALSE, orthogonal = TRUE)
  P <- t(chol(Sigma))
  expect_equal(as.numeric(irf_orth[[1L]][1L, ]), as.numeric(P[, 1L]))
  expect_equal(as.numeric(irf_orth[[2L]][1L, ]), as.numeric(P[, 2L]))
})

test_that("var_fecov() accumulates the propagated innovation covariance", {
  K <- 2L
  A <- matrix(c(0.3, 0.05, 0.1, 0.2), K, K)
  Sigma <- diag(K) + 0.3
  msey <- var_fecov(var_kernel_input(K, A, Sigma), h = 3L)
  expect_equal(msey[, , 1L], Sigma)
  expect_equal(msey[, , 2L], Sigma + A %*% Sigma %*% t(A))
})


# ---- posterior_transition_matrix ------------------------------------
# Driven on the same draws_matrix mock as the other VAR surfaces, so
# the argument surface and the summary shape are covered without
# sampling. End-to-end coverage is in tests/local/postfit_sweep.R.

# `finalise_transition_matrix()` reports an effective sample size per
# cell, and `posterior::ess_basic()` warns when it has to cap one.
# Independent draws invite that cap, which no real chain would, so
# the mock is passed through an AR(1) filter first to give it the
# serial correlation a posterior actually has.
correlate_draws <- function(mock, rho = 0.5) {
  m <- as.matrix(mock$fit)
  for (j in seq_len(ncol(m))) {
    m[, j] <- as.numeric(
      stats::filter(m[, j], rho, method = "recursive")
    )
  }
  mock$fit <- posterior::as_draws_matrix(m)
  mock
}

test_that("posterior_transition_matrix(summary = FALSE) keeps the draws", {
  mock <- correlate_draws(build_var_mock(K = 3L, ndraws = 200L))
  arr <- posterior_transition_matrix(mock, summary = FALSE)
  expect_equal(dim(arr), c(200L, 3L, 3L))
  # The summarised estimate is the posterior mean of those draws.
  out <- posterior_transition_matrix(mock)
  expect_equal(out$A, apply(arr, c(2, 3), mean), ignore_attr = TRUE)
  # And the median under `robust`.
  rob <- posterior_transition_matrix(mock, robust = TRUE)
  expect_equal(rob$A, apply(arr, c(2, 3), stats::median),
               ignore_attr = TRUE)
  # The exceedance probability is the share of draws above zero. A
  # [0, 1] bound on a mean of indicators excludes no result.
  expect_equal(out$prob_positive, apply(arr > 0, c(2, 3), mean),
               ignore_attr = TRUE)

  # The heatmap puts the lagged process (column) across and outlines
  # the one cell whose interval excludes zero, row 1 and column 2.
  out$A_lower[] <- -1
  out$A_upper[] <- 1
  out$A_lower[1L, 2L] <- 0.1
  p <- plot(out)
  expect_identical(rlang::as_label(p$mapping$x), "Var2")
  # The columns are labelled by the matrix's own lag
  out$lag <- 2L
  expect_identical(plot(out)$labels$x, "Process at t - 2")
  outlined <- p$layers[[2L]]$data
  expect_identical(
    as.character(c(outlined$Var1, outlined$Var2)),
    c(rownames(out$A)[1L], colnames(out$A)[2L])
  )
})


test_that("posterior_transition_matrix() takes one groups argument", {
  # A non-hierarchical fit refuses `groups`
  mock <- correlate_draws(build_var_mock(K = 3L, ndraws = 200L))
  expect_error(
    posterior_transition_matrix(mock, groups = "all"),
    "only applies to hierarchical VAR fits"
  )
})


# ---- Process labels -------------------------------------------------

test_that("var_process_labels names the series where they are the processes", {
  # A VAR over the observed series names its processes by the series
  # axis, and a reader interprets a shock without the internal order.
  mock <- build_var_mock(K = 3L, ndraws = 5L)
  expect_identical(var_process_labels(mock, 3L),
                   c("Process_1", "Process_2", "Process_3"))

  named <- mock
  named$trend_metadata$axes <- list(
    series = list(levels = c("willow", "ash", "rowan"), n = 3L)
  )
  expect_identical(var_process_labels(named, 3L),
                   c("willow", "ash", "rowan"))

  # A series count disagreeing with the VAR dimension is no labelling
  # of it, and the index answers in place of a recycled name.
  short <- named
  short$trend_metadata$axes$series$levels <- c("willow", "ash")
  expect_identical(var_process_labels(short, 3L),
                   c("Process_1", "Process_2", "Process_3"))
})


test_that("the VAR posterior carries one label per process", {
  mock <- build_var_mock(K = 3L, ndraws = 5L)
  vp <- extract_var_posterior(mock)
  # The mock carries no series names, and the labels take the index
  # form the test above pins for var_process_labels().
  expect_identical(vp$labels, c("Process_1", "Process_2", "Process_3"))
  # Both kernels read the labels off the posterior, so a surface
  # cannot spell them a second way.
  irf_gen <- gen_irf(
    var_kernel_input(3L, matrix(0.1, 3L, 3L), labels = vp$labels), h = 2L
  )
  expect_identical(names(irf_gen), vp$labels)
  fevd_gen <- gen_fevd(
    var_kernel_input(3L, matrix(0.1, 3L, 3L), labels = vp$labels), h = 2L
  )
  expect_identical(names(fevd_gen), vp$labels)
})


test_that("the variance decomposition plot keeps the processes in order", {
  # The legend, the colours and the panels follow the fit's order
  labels <- c("zeta", "alpha", "mu")
  draws <- lapply(1:2, function(i) {
    gen_fevd(var_kernel_input(3L, matrix(0.1 * i, 3L, 3L),
                              labels = labels), h = 2L)
  })
  class(draws) <- "mvgam_fevd"
  p <- plot(draws)
  expect_identical(levels(p$data$Series), labels)
  expect_identical(levels(p$data$target), labels)
  # A subset keeps the same relative order.
  p_sub <- plot(draws, series = c(3L, 1L), contributing = c(2L, 3L))
  expect_identical(levels(p_sub$data$Series), c("alpha", "mu"))
  expect_identical(levels(p_sub$data$target), c("zeta", "mu"))
  # Names select the same processes as positions, in the fit's order.
  p_named <- plot(draws, series = c("mu", "zeta"),
                  contributing = c("mu", "alpha"))
  expect_identical(p_named$data, p_sub$data)
  expect_error(plot(draws, series = "beta"), "Unknown: beta")
})


test_that("impulse response draws and summary keep the processes in order", {
  # Both forms follow the fit's order and label a pair alike
  labels <- c("zeta", "alpha", "mu")
  draws <- lapply(1:2, function(i) {
    gen_irf(var_kernel_input(3L, matrix(0.1 * i, 3L, 3L),
                             labels = labels),
            h = 2L, cumulative = FALSE, orthogonal = FALSE)
  })
  class(draws) <- "mvgam_irf"
  attr(draws, "irf_type") <- "Generalized"
  pairs <- paste0("alpha -> ", labels)
  p <- plot(draws, series = 2L)
  expect_identical(levels(p$layers[[1L]]$data$series), pairs)
  expect_identical(p$labels$y, "Generalized impulse response")
  expect_identical(unique(summary(draws)$shock)[4:6], pairs)
  # A shock and its responses can be named on both forms.
  p_named <- plot(draws, series = "alpha", responses = c("mu", "zeta"))
  expect_identical(levels(p_named$layers[[1L]]$data$series), pairs[c(1, 3)])
  irf_sum <- as_var_surface_summary(summary(draws), "mvgam_irf_summary",
                                    process_labels = labels)
  p_sum <- plot(irf_sum, series = "alpha", responses = c("mu", "zeta"))
  expect_identical(levels(p_sum$data$shock), pairs[c(1, 3)])
})


test_that("a stability metric that is not there is refused by name", {
  # A misspelt metric is refused by name
  set.seed(5L)
  x <- data.frame(reactivity = rnorm(20), mean_return_rate = runif(20),
                  var_return_rate = runif(20))
  class(x) <- c("mvgam_stability", class(x))
  expect_error(plot(x, variables = c("reactivity", "reactvity")),
               "'reactvity'")
  p <- plot(x, variables = c("var_return_rate", "reactivity"))
  expect_identical(levels(p$data$metric),
                   c("var_return_rate", "reactivity"))
  # Panels carry display names
  strips <- p$facet$params$labeller(
    data.frame(metric = c("var_return_rate", "reactivity"))
  )
  expect_identical(unname(strips[[1L]]),
                   c("Variance return rate", "Reactivity"))
})


test_that("a hierarchical VAR's stability is summarised panel by panel", {
  # Two panels of two processes. The cells between panels are the
  # structural zeros of a hierarchical VAR.
  mock <- build_var_mock(K = 4L, ndraws = 20L)
  m <- as.matrix(mock$fit)
  cross <- expand.grid(i = 1:4, j = 1:4)
  cross <- cross[(cross$i <= 2L) != (cross$j <= 2L), ]
  m[, c(sprintf("Phi_trend[1,%d,%d]", cross$i, cross$j),
        sprintf("Sigma_trend[%d,%d]", cross$i, cross$j))] <- 0
  mock$fit <- posterior::as_draws_matrix(m)
  mock$standata <- c(mock$standata, list(
    N_groups_trend = 2L, N_subgroups_trend = 2L,
    group_inds_trend = c(1L, 1L, 2L, 2L)
  ))
  mock$trend_metadata$axes <- list(
    group_levels = list(gr = c("north", "south"))
  )

  draws <- stability(mock, summary = FALSE)
  expect_identical(levels(draws$group), c("north", "south"))
  # Each panel's metrics are those of its own block of the matrices
  block <- function(name, d) {
    matrix(m[d, sprintf(name, rep(3:4, 2L), rep(3:4, each = 2L))], 2L)
  }
  expect_equal(
    unlist(draws[draws$group == "south", -1L][5L, ]),
    unlist(stability_metrics(block("Phi_trend[1,%d,%d]", 5L),
                             block("Sigma_trend[%d,%d]", 5L)))
  )
  south <- stability(mock, groups = "south")
  expect_identical(as.character(unique(south$group)), "south")
  # Panels are rows of a median and interval plot
  p <- plot(stability(mock))
  expect_s3_class(p$layers[[2L]]$geom, "GeomPointrange")
  expect_identical(levels(p$data$row), c("south", "north"))
  expect_error(stability(build_var_mock(K = 2L), groups = 1L),
               "only applies to hierarchical VAR fits")
})


test_that("the stability metrics match their closed forms", {
  B <- matrix(c(0.5, 0.2, 0, -0.1, 0.4, 0.1, 0.05, 0, 0.3), 3L)
  Sigma <- matrix(c(1, 0.3, 0.1, 0.3, 0.8, 0.2, 0.1, 0.2, 0.5), 3L)
  # The doubling solver agrees with the Kronecker solve of
  # vec(X) = (I - B %x% B)^-1 vec(Sigma)
  kron <- matrix(solve(diag(9L) - B %x% B, c(Sigma)), 3L)
  expect_equal(solve_dlyap(B, Sigma), kron, tolerance = 1e-10)
  m <- stability_metrics(B, Sigma)
  expect_equal(m$prop_int, det(B)^2)
  expect_equal(m$reactivity, log(max(svd(B)$d)))
  expect_equal(m$var_return_rate,
               max(Mod(eigen(B %x% B, only.values = TRUE)$values)))
})
