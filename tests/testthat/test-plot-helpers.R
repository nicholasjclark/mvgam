# The helpers every plot method builds from. Nothing is rendered.

# Fixed-seed (n_draws x n_time) draws matrix used across tests.
.make_draws_mat <- function(seed = 1L, n_draws = 200L, n_time = 25L) {
  set.seed(seed)
  matrix(rnorm(n_draws * n_time), nrow = n_draws, ncol = n_time)
}

test_that("mvgam_palette returns the bayesplot scheme vector", {
  red <- mvgam:::mvgam_palette("red")
  expect_equal(red[c(1L, 5L)], c("#DCBCBC", "#8F2727"))
})

test_that("with_color_scheme restores the prior scheme on exit", {
  local_color_scheme("blue")
  out <- mvgam:::with_color_scheme(mvgam:::mvgam_palette())
  expect_equal(out[1L], "#DCBCBC")
  prior <- attr(bayesplot::color_scheme_get(), "scheme_name")
  expect_equal(prior, "blue")
})

test_that("with_color_scheme restores on expression error", {
  local_color_scheme("red")
  expect_error(
    mvgam:::with_color_scheme(stop("boom"), "blue")
  )
  prior <- attr(bayesplot::color_scheme_get(), "scheme_name")
  expect_equal(prior, "red")
})

test_that("mvgam_theme sets the strips and font size of every figure", {
  th <- mvgam:::mvgam_theme(base_size = 14)
  expect_identical(th$strip.background$fill, "grey85")
  expect_identical(th$axis.title$size, 14)
})

test_that("mvgam_band_fills shapes match n", {
  fills <- mvgam:::mvgam_band_fills(3L, palette = letters[1:6])
  expect_equal(fills, letters[1:3])
  expect_error(
    mvgam:::mvgam_band_fills(7L, palette = letters[1:6])
  )
})

test_that("mvgam_band_layer returns one ribbon per probability", {
  mat <- .make_draws_mat()
  layers <- mvgam:::mvgam_band_layer(
    draws_mat = mat,
    times = seq_len(ncol(mat)),
    probs = c(0.5, 0.8, 0.95)
  )
  expect_length(layers, 3L)
  # The 95% band is drawn first, in the lightest palette entry
  outer_fill <- layers[[1L]]$aes_params$fill
  inner_fill <- layers[[3L]]$aes_params$fill
  expect_equal(outer_fill, mvgam:::mvgam_palette()[1L])
  expect_equal(inner_fill, mvgam:::mvgam_palette()[3L])
})

test_that("mvgam_band_layer quantile bounds match apply(quantile)", {
  mat <- .make_draws_mat()
  times <- seq_len(ncol(mat))
  layers <- mvgam:::mvgam_band_layer(
    draws_mat = mat, times = times, probs = 0.95
  )
  expected_lo <- apply(mat, 2L, quantile, probs = 0.025)
  expected_hi <- apply(mat, 2L, quantile, probs = 0.975)
  layer_df <- layers[[1L]]$data
  expect_equal(layer_df$lower, unname(expected_lo))
  expect_equal(layer_df$upper, unname(expected_hi))
  expect_equal(layer_df$time, times)
})

test_that("mvgam_band_layer rejects duplicate probs", {
  mat <- .make_draws_mat()
  expect_error(
    mvgam:::mvgam_band_layer(
      mat, seq_len(ncol(mat)), probs = c(0.5, 0.5, 0.8)
    ),
    "duplicated"
  )
})

test_that("mvgam_band_layer rejects mismatched times", {
  mat <- .make_draws_mat()
  expect_error(
    mvgam:::mvgam_band_layer(mat, seq_len(ncol(mat) + 1L))
  )
})

test_that("mvgam_median_layer plots the per-column median", {
  mat <- .make_draws_mat()
  times <- seq_len(ncol(mat))
  layer <- mvgam:::mvgam_median_layer(mat, times)
  expect_equal(layer$data$med, unname(apply(mat, 2L, median)))
  expect_equal(layer$aes_params$colour, mvgam:::mvgam_palette()[5L])
})

test_that("mvgam_obs_layer drops NAs and emits two point layers", {
  layers <- mvgam:::mvgam_obs_layer(
    times = 1:5, y = c(1, NA, 3, NA, 5)
  )
  expect_length(layers, 2L)
  expect_equal(nrow(layers[[1L]]$data), 3L)
  expect_equal(nrow(layers[[2L]]$data), 3L)
})

test_that("mvgam_obs_layer returns empty list when all NA", {
  out <- mvgam:::mvgam_obs_layer(
    times = 1:3, y = c(NA_real_, NA_real_, NA_real_)
  )
  expect_equal(out, list())
})

test_that("mvgam_cut_layer is a no-op when t_cut is NULL or NA", {
  expect_null(mvgam:::mvgam_cut_layer(NULL))
  expect_null(mvgam:::mvgam_cut_layer(NA_real_))
  expect_null(mvgam:::mvgam_cut_layer(integer(0L)))
  expect_equal(mvgam:::mvgam_cut_layer(t_cut = 42)$data$xintercept, 42)
})

test_that("plot_helpers compose into a valid ggplot", {
  mat <- .make_draws_mat()
  times <- seq_len(ncol(mat))
  p <- ggplot2::ggplot() +
    mvgam:::mvgam_band_layer(mat, times, probs = c(0.5, 0.95)) +
    mvgam:::mvgam_median_layer(mat, times) +
    mvgam:::mvgam_obs_layer(times = times[c(5, 10)], y = c(1, -1)) +
    mvgam:::mvgam_cut_layer(t_cut = 15) +
    mvgam:::mvgam_theme()
  # 2 band ribbons + 1 median line + 2 obs points + 1 cut line.
  expect_equal(length(p$layers), 6L)
})


# ---- resolve_factor_loadings (fixed Z vs sampled Z) ----

test_that("resolve_factor_loadings broadcasts a fixed Z across draws", {
  fixed_Z <- matrix(c(1.0, 0.0,
                       0.5, 0.5,
                       0.0, 1.0,
                       0.3, 0.7),
                     nrow = 4L, ncol = 2L, byrow = TRUE)
  draws_mat <- matrix(0, nrow = 7L, ncol = 1L)
  out <- mvgam:::resolve_factor_loadings(
    fixed_Z = fixed_Z, draws_mat = draws_mat,
    n_series = 4L, n_lv = 2L
  )
  # Every draw broadcasts the same matrix. A wrong shape fails here
  # before any dimension check would.
  for (d in seq_len(7L)) {
    expect_equal(out[d, , ], unname(fixed_Z))
  }
})

test_that("resolve_factor_loadings parses Z[i,j] when no fixed_Z", {
  # Synthesise a draws matrix with Z[i, j] columns in Stan
  # column-major order.
  ndraws <- 5L
  n_series <- 3L
  n_lv <- 2L
  cols <- character(0)
  for (k in seq_len(n_lv)) {
    for (s in seq_len(n_series)) {
      cols <- c(cols, sprintf("Z[%d,%d]", s, k))
    }
  }
  draws_mat <- matrix(seq_len(ndraws * length(cols)),
                       nrow = ndraws,
                       dimnames = list(NULL, cols))
  out <- mvgam:::resolve_factor_loadings(
    draws_mat = draws_mat,
    n_series = n_series, n_lv = n_lv
  )
  # Every (series, factor) cell. Two corners left the interior free,
  # and swapping series 2 with series 3 passed them both.
  for (s in seq_len(n_series)) {
    for (k in seq_len(n_lv)) {
      expect_equal(out[, s, k],
                   unname(draws_mat[, sprintf("Z[%d,%d]", s, k)]))
    }
  }
})

test_that("resolve_factor_loadings errors on incomplete inputs", {
  expect_error(
    mvgam:::resolve_factor_loadings(),
    "Must be of type 'single integerish value'"
  )
  expect_error(
    mvgam:::resolve_factor_loadings(n_series = 3L),
    "Must be of type 'single integerish value'"
  )
})


# One theme and one palette. Every figure takes `mvgam_theme()` and
# its colours from the scheme the plot method sets, with no theme or
# scheme colour written at a call site.

test_that("mvgam_colour reads a scheme entry by its role", {
  local_color_scheme("red")
  expect_equal(mvgam:::mvgam_colour("light"), "#DCBCBC")
  expect_equal(mvgam:::mvgam_colour("dark_highlight"), "#7C0000")
  # Explicit scheme wins over the active one.
  expect_equal(mvgam:::mvgam_colour("light", scheme = "blue"), "#d1e1ec")
  # And the active scheme is what an unqualified call follows.
  bayesplot::color_scheme_set("blue")
  expect_equal(mvgam:::mvgam_colour("light"), "#d1e1ec")
  expect_error(mvgam:::mvgam_colour("darkest"), "role")
})

test_that("no plot applies a ggplot2 theme of its own", {
  # `mvgam_theme()` is built on `theme_classic()`, so the one call
  # inside it is the only one allowed.
  r_files <- list.files(
    test_path("..", "..", "R"), pattern = "\\.R$", full.names = TRUE
  )
  if (!length(r_files)) {
    r_files <- list.files("R", pattern = "\\.R$", full.names = TRUE)
  }
  offenders <- character(0)
  for (f in r_files) {
    lines <- readLines(f, warn = FALSE)
    code <- grep("^\\s*#", lines, value = TRUE, invert = TRUE)
    hits <- grep("ggplot2::theme_[a-z]+\\(", code, value = TRUE)
    if (length(hits) && basename(f) != "plot_helpers.R") {
      offenders <- c(offenders, paste0(basename(f), ": ", trimws(hits)))
    }
  }
  expect_equal(offenders, character(0))
})

test_that("no plot writes a scheme colour as a literal", {
  # Every hex the bayesplot schemes carry. A colour comes from the
  # scheme by its role.
  schemes <- c("red", "blue", "green", "purple", "teal", "gray",
               "yellow", "orange")
  scheme_hex <- unique(toupper(unlist(lapply(
    schemes, function(s) unname(unlist(bayesplot::color_scheme_get(s)))
  ))))
  r_files <- list.files(
    test_path("..", "..", "R"), pattern = "\\.R$", full.names = TRUE
  )
  if (!length(r_files)) {
    r_files <- list.files("R", pattern = "\\.R$", full.names = TRUE)
  }
  offenders <- character(0)
  for (f in r_files) {
    if (basename(f) == "plot_helpers.R") next
    lines <- readLines(f, warn = FALSE)
    code <- grep("^\\s*#", lines, value = TRUE, invert = TRUE)
    for (hx in scheme_hex) {
      if (any(grepl(hx, toupper(code), fixed = TRUE))) {
        offenders <- c(offenders, paste0(basename(f), ": ", hx))
      }
    }
  }
  expect_equal(offenders, character(0))
})


test_that("legend breaks stay readable when limits come from data", {
  # A precision heatmap scales to its own entries. Its breaks are
  # rounded.
  expect_equal(
    mvgam:::pretty_symmetric_breaks(c(-1, 1)),
    c(-1, -0.5, 0, 0.5, 1)
  )
  # The endpoints stay inside the limits. ggplot2 drops a break
  # rounded past them.
  expect_equal(
    mvgam:::pretty_symmetric_breaks(c(-0.5764995, 0.5764995)),
    c(-0.57, -0.29, 0, 0.29, 0.57)
  )
  # A wide range keeps its magnitude rather than collapsing to zero.
  expect_equal(
    mvgam:::pretty_symmetric_breaks(c(-12.3, 12.3)),
    c(-12.3, -6.2, 0, 6.2, 12.3)
  )
  # And a narrow one keeps enough digits to stay distinct.
  narrow <- mvgam:::pretty_symmetric_breaks(c(-0.004, 0.004))
  expect_length(unique(narrow), 5L)
})


test_that("the plotting default holds the parameters a reader reads", {
  # `mcmc_plot()` and `pairs()` take their default from the parameter
  # taxonomy. It holds each response's family parameters, such as
  # `shape_pos`, under the names `as.array()` returns.
  raw <- c(
    "b_Intercept", "Intercept", "b_x", "bs_sx_1", "simo_mo1[1]",
    "shape_pos", "shape_cnt", "sds_1[1]", "sdgp_1[1]", "lscale_1[1]",
    "zgp_1[1]", "s_1_1[1]", "zs_1_1[1]", "sd_g__Intercept",
    "cor_g__Intercept__x", "r_g[a,Intercept]", "Z[1,1]",
    "Z_tilde[1,1]", "ar1_trend[1]", "b_Intercept_trend",
    "trend[1,1]", "lv_trend[1,1]", "lp__"
  )
  stub <- structure(
    list(fit = posterior::as_draws_matrix(matrix(
      0, nrow = 2L, ncol = length(raw), dimnames = list(NULL, raw)
    ))),
    class = "mvgam"
  )
  # The rotated loadings are drawn and the raw `Z` is not, as in
  # `variables()` and `summary()`.
  expect_setequal(
    mvgam:::default_plot_variables(stub),
    c("b_Intercept", "b_x", "bs_sx_1", "simo_mo1[1]", "shape_pos",
      "shape_cnt", "sds_1[1]", "sdgp_1[1]", "lscale_1[1]",
      "sd_g__Intercept", "cor_g__Intercept__x", "Z_tilde[1,1]",
      "ar1_trend[1]", "b_Intercept_trend")
  )
})
