# plot_factors tests: mocked mvgam object with fake LV draws so
# CI does not require a real Stan fit.

# Build a stand-in mvgam object with an `lv_trend[t,k]` posterior
# block embedded in object$fit (a list with the minimum surface
# posterior::as_draws_matrix needs).
.make_lv_obj <- function(
  n_lv = 2L,
  n_time = 30L,
  n_series = 4L,
  ndraws = 200L,
  factor_sds = c(0.05, 1.0),
  loading_scales = NULL,
  seed = 1L
) {
  stopifnot(length(factor_sds) == n_lv)
  set.seed(seed)
  # Per-factor draws: low-sd factors mimic shrinkage-suppressed
  # paths; high-sd factors mimic dynamic ones.
  lv_draws <- lapply(seq_len(n_lv), function(k) {
    matrix(
      rnorm(ndraws * n_time, sd = factor_sds[k]),
      nrow = ndraws, ncol = n_time
    )
  })
  col_names <- character(0L)
  draws_cols <- list()
  for (t in seq_len(n_time)) {
    for (k in seq_len(n_lv)) {
      col_names <- c(col_names, paste0("lv_trend[", t, ",", k, "]"))
      draws_cols <- c(draws_cols, list(lv_draws[[k]][, t]))
    }
  }
  # Stan stores Z column-major: Z[1,1], Z[2,1], ..., Z[N,1],
  # Z[1,2], Z[2,2], ... — mirror that here so extract_Z_loadings
  # parses correctly.
  if (is.null(loading_scales)) {
    loading_scales <- rep(1, n_lv)
  }
  for (k in seq_len(n_lv)) {
    for (s in seq_len(n_series)) {
      col_names <- c(col_names, paste0("Z[", s, ",", k, "]"))
      draws_cols <- c(
        draws_cols,
        list(rnorm(ndraws, mean = 0, sd = loading_scales[k]))
      )
    }
  }
  draws_mat <- do.call(cbind, draws_cols)
  colnames(draws_mat) <- col_names
  draws_mat <- posterior::as_draws_matrix(draws_mat)

  fake_fit <- list()
  attr(fake_fit, "fake_draws") <- draws_mat
  class(fake_fit) <- c("fake_brmsfit")
  structure(
    list(
      fit = fake_fit,
      use_lv = TRUE,
      trend_metadata = list(n_lv = n_lv),
      mv_spec = list(
        trend_specs = structure(
          list(n_lv = n_lv, gr = NA_character_),
          class = "mvgam_trend"
        )
      ),
      obs_data = list(
        series = factor(rep(
          paste0("s", seq_len(n_series)), length.out = n_time
        ))
      )
    ),
    class = c("mvgam")
  )
}

# Route posterior::as_draws_matrix() on our fake_brmsfit to the
# stashed matrix.
as_draws_matrix.fake_brmsfit <- function(x, ...) {
  attr(x, "fake_draws")
}

test_that("plot_factors.mvgam returns a ggplot with contribution attr", {
  registerS3method(
    "as_draws_matrix", "fake_brmsfit", as_draws_matrix.fake_brmsfit,
    envir = .GlobalEnv
  )
  obj <- .make_lv_obj()
  p <- mvgam:::plot_factors(obj)
  expect_ggplot(p)
  contrib <- attr(p, "contribution")
  expect_true(is.data.frame(contrib))
  expect_equal(nrow(contrib), 2L)
  expect_equal(
    sort(colnames(contrib)),
    c("Factor", "Lower", "PathVarShare", "Upper")
  )
  expect_equal(sum(contrib$PathVarShare), 1, tolerance = 1e-8)
  expect_true(all(contrib$Lower <= contrib$PathVarShare))
  expect_true(all(contrib$Upper >= contrib$PathVarShare))
})

test_that("low-sd factor receives small contribution share", {
  registerS3method(
    "as_draws_matrix", "fake_brmsfit", as_draws_matrix.fake_brmsfit,
    envir = .GlobalEnv
  )
  obj <- .make_lv_obj(factor_sds = c(0.02, 2.0), seed = 2L)
  p <- mvgam:::plot_factors(obj)
  contrib <- attr(p, "contribution")
  expect_lt(
    contrib$PathVarShare[contrib$Factor == "Factor 1"],
    contrib$PathVarShare[contrib$Factor == "Factor 2"]
  )
})

test_that("Z_arr = NULL fallback reduces to bare path-variance partition", {
  registerS3method(
    "as_draws_matrix", "fake_brmsfit", as_draws_matrix.fake_brmsfit,
    envir = .GlobalEnv
  )
  obj <- .make_lv_obj(factor_sds = c(0.02, 2.0), seed = 11L)
  testthat::local_mocked_bindings(
    extract_factor_loadings_array = function(...) NULL
  )
  p <- mvgam:::plot_factors(obj)
  contrib <- attr(p, "contribution")
  # Bare path variance still favours the wider-amplitude factor,
  # so factor 2 (sd = 2.0) dominates factor 1 (sd = 0.02).
  expect_equal(sum(contrib$PathVarShare), 1, tolerance = 1e-8)
  expect_lt(
    contrib$PathVarShare[contrib$Factor == "Factor 1"],
    contrib$PathVarShare[contrib$Factor == "Factor 2"]
  )
})

test_that("loading scale shifts the contribution share", {
  registerS3method(
    "as_draws_matrix", "fake_brmsfit", as_draws_matrix.fake_brmsfit,
    envir = .GlobalEnv
  )
  # Same path SDs (both = 1), but factor 1 gets large loadings
  # and factor 2 gets tiny loadings. The loading-weighted metric
  # should award most of the share to factor 1.
  obj <- .make_lv_obj(
    factor_sds = c(1.0, 1.0),
    loading_scales = c(5.0, 0.05),
    seed = 7L
  )
  p <- mvgam:::plot_factors(obj)
  contrib <- attr(p, "contribution")
  expect_gt(
    contrib$PathVarShare[contrib$Factor == "Factor 1"],
    0.9
  )
})

test_that("non-LV model raises an informative error", {
  obj <- list(
    fit = list(),
    mv_spec = list(trend_specs = structure(list(),
                                           class = "mvgam_trend"))
  )
  class(obj) <- "mvgam"
  expect_error(mvgam:::plot_factors(obj), "latent dynamic factors")
})

test_that("label_contribution toggles the percentage facet labels", {
  registerS3method(
    "as_draws_matrix", "fake_brmsfit", as_draws_matrix.fake_brmsfit,
    envir = .GlobalEnv
  )
  obj <- .make_lv_obj(factor_sds = c(0.02, 2.0), seed = 3L)
  # Label strings are inside the labeller; pull them out by
  # applying the labeller to the data values.
  p_pct <- mvgam:::plot_factors(obj, label_contribution = TRUE)
  p_bare <- mvgam:::plot_factors(obj, label_contribution = FALSE)
  labels_pct <- p_pct$facet$params$labeller(
    list(series = c("Factor 1", "Factor 2"))
  )$series
  labels_bare <- p_bare$facet$params$labeller(
    list(series = c("Factor 1", "Factor 2"))
  )$series
  expect_true(all(grepl("\\([0-9.]+%\\)$", labels_pct)))
  expect_false(any(grepl("%", labels_bare)))
})

test_that("ribbon band count matches probs length per factor", {
  registerS3method(
    "as_draws_matrix", "fake_brmsfit", as_draws_matrix.fake_brmsfit,
    envir = .GlobalEnv
  )
  obj <- .make_lv_obj(n_lv = 2L)
  p <- mvgam:::plot_factors(obj, probs = c(0.5, 0.95))
  ribbon_count <- sum(vapply(
    p$layers,
    function(l) inherits(l$geom, "GeomRibbon"),
    logical(1L)
  ))
  # 2 probs x 2 factors = 4 ribbon layers.
  expect_equal(ribbon_count, 4L)
})
