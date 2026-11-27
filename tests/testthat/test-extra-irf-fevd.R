# The tests of test-irf-fevd.R that assemble a model or run for
# 0.2 s or more. CRAN skips this file. CI and local runs take it.
skip_on_cran()

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
  # the one cell whose sign probability passes 0.95, row 1 and column 2
  out$prob_nonzero[] <- 0.6
  out$prob_nonzero[1L, 2L] <- 0.99
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
