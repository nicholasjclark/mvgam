# Tests for the input-array pivot helpers `pivot_detection_array()`
# and `pivot_species_matrix()`. Three layers:
#   1. Layout dispatch: 2D matrix, 3D array, 4D array (multi-season),
#      and named list of matrices (unmarkedFrameOccuMulti) all produce
#      a correctly-shaped long-format data.frame.
#   2. Covariate broadcasting: site_covs, season_covs,
#      site_season_covs, obs_covs (both list and long-data.frame
#      forms) broadcast across the (site, season, visit, species)
#      grid as expected.
#   3. Downstream compatibility: the pivoted frames pass through
#      mvgam's own validators (`build_closure_unit_arrays()`,
#      `mvgam_formula()` parse, `stancode()` codegen with
#      `family = occ()` / `family = nmix()`).

# 1. Layout dispatch ----------------------------------------------------

test_that("2D matrix y pivots to (time, visit, y) long format", {
  set.seed(1L)
  y <- matrix(rbinom(20L, 1L, 0.4), nrow = 5L, ncol = 4L)
  out <- pivot_detection_array(y)
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 5L * 4L)
  expect_true(all(c("site", "visit", "time", "y") %in% colnames(out)))
  # Single-season, single-species drops series and season columns.
  expect_false("series" %in% colnames(out))
  expect_false("season" %in% colnames(out))
  # time == site for single-season.
  expect_equal(out$time, out$site)
})

test_that("3D array y pivots to (series, site, visit, y) long format", {
  set.seed(2L)
  y <- array(rbinom(60L, 1L, 0.4), dim = c(3L, 5L, 4L))
  out <- pivot_detection_array(
    y, species = c("sp1", "sp2", "sp3")
  )
  expect_equal(nrow(out), 3L * 5L * 4L)
  expect_true("series" %in% colnames(out))
  expect_false("season" %in% colnames(out))
  expect_equal(levels(out$series), c("sp1", "sp2", "sp3"))
  # time still equals site for single-season multi-species.
  expect_equal(out$time, out$site)
})

test_that("3D array y picks up species names from dimnames", {
  set.seed(3L)
  y <- array(rbinom(60L, 1L, 0.4), dim = c(3L, 5L, 4L),
              dimnames = list(c("alpha", "beta", "gamma"),
                              NULL, NULL))
  out <- pivot_detection_array(y)
  expect_equal(levels(out$series), c("alpha", "beta", "gamma"))
})

test_that("4D array y emits fused time and side-car season", {
  set.seed(4L)
  N <- 3L; J <- 5L; T_ <- 2L; K <- 4L
  y <- array(rbinom(N * J * T_ * K, 1L, 0.4),
             dim = c(N, J, T_, K))
  out <- pivot_detection_array(
    y, species = c("sp1", "sp2", "sp3")
  )
  expect_equal(nrow(out), N * J * T_ * K)
  expect_true(all(c("series", "site", "season", "visit", "time", "y")
                  %in% colnames(out)))
  # `time = (site - 1) * T + season` is the closure-unit identifier.
  expect_equal(out$time, (out$site - 1L) * T_ + out$season)
  # Each (site, season) pair is exactly one unique `time` value.
  uniq <- unique(out[, c("site", "season", "time")])
  expect_equal(nrow(uniq), J * T_)
  expect_equal(length(unique(uniq$time)), J * T_)
})

test_that("4D array with multi_season = 'hierarchical' emits time = season", {
  set.seed(4L)
  N <- 3L; J <- 5L; T_ <- 4L; K <- 3L
  y <- array(rbinom(N * J * T_ * K, 1L, 0.4),
             dim = c(N, J, T_, K))
  out <- pivot_detection_array(
    y, species = c("sp1", "sp2", "sp3"),
    multi_season = "hierarchical"
  )
  expect_equal(nrow(out), N * J * T_ * K)
  # `time = season` rather than the fused encoding. `site` is
  # retained as a side-car covariate for use in obs_formula.
  expect_equal(out$time, out$season)
  expect_true("site" %in% colnames(out))
  # Distinct `time` values match the number of seasons, not
  # (site, season) pairs.
  expect_equal(length(unique(out$time)), T_)
  # Closure-unit grouping (series, site, time): one unit per
  # (species, site, season).
  arrays <- mvgam:::build_closure_unit_arrays(
    out, response_var = "y", compute_y_max = FALSE,
    unit_grouping_vars = c("series", "site", "time")
  )
  expect_equal(arrays$N_unit, N * J * T_)
  expect_true(all(arrays$n_rep == K))
})

test_that("multi_season is ignored for single-season inputs", {
  # 2D and 3D inputs have only one season; both modes should give
  # identical output.
  set.seed(4L)
  y3 <- array(rbinom(60L, 1L, 0.4), dim = c(3L, 5L, 4L))
  out_default <- pivot_detection_array(
    y3, species = c("sp1", "sp2", "sp3")
  )
  out_hier <- pivot_detection_array(
    y3, species = c("sp1", "sp2", "sp3"),
    multi_season = "hierarchical"
  )
  expect_identical(out_default, out_hier)
})

test_that("invalid multi_season choice errors via match.arg", {
  y <- matrix(rbinom(20L, 1L, 0.4), 5L, 4L)
  expect_error(
    pivot_detection_array(y, multi_season = "bogus"),
    "should be one of"
  )
})

test_that("named list of matrices is OccuMulti-style multi-species", {
  set.seed(5L)
  y <- list(
    sp1 = matrix(rbinom(20L, 1L, 0.4), 5L, 4L),
    sp2 = matrix(rbinom(20L, 1L, 0.4), 5L, 4L)
  )
  out <- pivot_detection_array(y)
  expect_equal(nrow(out), 2L * 5L * 4L)
  expect_equal(levels(out$series), c("sp1", "sp2"))
})

test_that("list y errors when entries are unnamed", {
  y <- list(
    matrix(rbinom(20L, 1L, 0.4), 5L, 4L),
    matrix(rbinom(20L, 1L, 0.4), 5L, 4L)
  )
  expect_error(pivot_detection_array(y), "fully named")
})

test_that("unsupported y layout errors cleanly", {
  # 5D array isn't a recognised shape.
  y <- array(0, dim = c(2L, 3L, 4L, 5L, 6L))
  expect_error(pivot_detection_array(y), "unsupported layout")
})

test_that("NA cells drop from the output", {
  y <- matrix(c(1, 0, NA, 1,
                1, 1, 0,  0,
                NA, NA, NA, NA),
              nrow = 3L, byrow = TRUE)
  out <- pivot_detection_array(y)
  expect_equal(nrow(out), sum(!is.na(y)))
  expect_false(any(is.na(out$y)))
})

# 2. Covariate broadcasting --------------------------------------------

test_that("site_covs broadcast across visits + species", {
  set.seed(6L)
  J <- 5L; K <- 3L; N <- 2L
  y <- array(rbinom(N * J * K, 1L, 0.4), dim = c(N, J, K))
  sc <- data.frame(env = rnorm(J), habitat = letters[seq_len(J)])
  out <- pivot_detection_array(y, site_covs = sc)
  expect_equal(nrow(out), N * J * K)
  # Per-site env value is identical across the J replicate rows.
  for (i in seq_len(J)) {
    expect_equal(
      length(unique(out$env[out$site == i])), 1L
    )
    expect_equal(out$env[out$site == i][1L], sc$env[i])
  }
})

test_that("obs_covs as list broadcasts across (site, visit)", {
  set.seed(7L)
  J <- 5L; K <- 3L; N <- 2L
  y <- array(rbinom(N * J * K, 1L, 0.4), dim = c(N, J, K))
  obs <- list(temp = matrix(rnorm(J * K), J, K))
  out <- pivot_detection_array(y, obs_covs = obs)
  # Check one (site, visit, species) cell matches the source matrix.
  one <- out[out$site == 2L & out$visit == 3L & out$series == "sp1", ]
  expect_equal(one$temp, obs$temp[2L, 3L])
})

test_that("obs_covs as long-format data.frame matches unmarked layout", {
  set.seed(8L)
  J <- 4L; K <- 3L
  y <- matrix(rbinom(J * K, 1L, 0.4), J, K)
  # unmarked stores obsCovs site-major + visit-fastest:
  # row1.visit1, row1.visit2, ..., row1.visitK, row2.visit1, ...
  obs_df <- data.frame(
    temp = as.vector(t(matrix(rnorm(J * K), J, K)))
  )
  # Capture the matrix that produced obs_df so we can check
  # round-trip recovery.
  src <- matrix(obs_df$temp, J, K, byrow = TRUE)
  out <- pivot_detection_array(y, obs_covs = obs_df)
  one <- out[out$site == 3L & out$visit == 2L, ]
  expect_equal(one$temp, src[3L, 2L])
})

test_that("multi-season covariate args broadcast correctly", {
  set.seed(9L)
  N <- 2L; J <- 4L; T_ <- 3L; K <- 2L
  y <- array(rbinom(N * J * T_ * K, 1L, 0.4),
              dim = c(N, J, T_, K))
  sc <- data.frame(elev = rnorm(J))
  cc <- data.frame(year = seq_len(T_))
  ssc <- list(
    habitat_score = matrix(rnorm(J * T_), J, T_)
  )
  obs <- list(
    effort = array(rnorm(J * T_ * K), dim = c(J, T_, K))
  )
  out <- pivot_detection_array(
    y, site_covs = sc, season_covs = cc,
    site_season_covs = ssc, obs_covs = obs
  )
  expect_equal(nrow(out), N * J * T_ * K)
  # Check broadcasts on one cell.
  row1 <- out[out$series == "sp1" & out$site == 2L &
                out$season == 3L & out$visit == 1L, ]
  expect_equal(row1$elev,           sc$elev[2L])
  expect_equal(row1$year,           cc$year[3L])
  expect_equal(row1$habitat_score,  ssc$habitat_score[2L, 3L])
  expect_equal(row1$effort,         obs$effort[2L, 3L, 1L])
})

test_that("reserved-name overlap on covariate args is rejected", {
  y <- matrix(rbinom(20L, 1L, 0.4), 5L, 4L)
  bad_site_covs <- data.frame(time = rnorm(5L))
  expect_error(
    pivot_detection_array(y, site_covs = bad_site_covs),
    "reserved output columns"
  )
})

# 3. Downstream compatibility ------------------------------------------

test_that("3D pivot output passes through build_closure_unit_arrays", {
  set.seed(10L)
  N <- 3L; J <- 6L; K <- 4L
  y <- array(rbinom(N * J * K, 1L, 0.4), dim = c(N, J, K))
  long <- pivot_detection_array(y, species = paste0("sp", 1:N))
  # `build_closure_unit_arrays()` is mvgam's internal closure-unit
  # validator + index builder. Default grouping is (series, time)
  # for closure-unit families; long$series + long$time should give
  # N * J unique closure units, each with K visits.
  arrays <- mvgam:::build_closure_unit_arrays(
    long, response_var = "y", compute_y_max = FALSE,
    unit_grouping_vars = c("series", "time")
  )
  expect_equal(arrays$N_unit, N * J)
  expect_true(all(arrays$n_rep == K))
})

test_that("4D multi-season pivot output passes through build_closure_unit_arrays", {
  set.seed(11L)
  N <- 3L; J <- 5L; T_ <- 2L; K <- 4L
  y <- array(rbinom(N * J * T_ * K, 1L, 0.4),
              dim = c(N, J, T_, K))
  long <- pivot_detection_array(y, species = paste0("sp", 1:N))
  arrays <- mvgam:::build_closure_unit_arrays(
    long, response_var = "y", compute_y_max = FALSE,
    unit_grouping_vars = c("series", "time")
  )
  # Multi-season: closure units = N species * J sites * T seasons.
  expect_equal(arrays$N_unit, N * J * T_)
  expect_true(all(arrays$n_rep == K))
})

test_that("real spOccupancy `hbef2015` pivots cleanly + closure-unit validates", {
  testthat::skip_if_not_installed("spOccupancy")
  env <- new.env()
  data("hbef2015", package = "spOccupancy", envir = env)
  hbef <- env$hbef2015
  long <- pivot_detection_array(
    y         = hbef$y,
    site_covs = as.data.frame(hbef$occ.covs),
    obs_covs  = hbef$det.covs
  )
  N <- dim(hbef$y)[1L]
  J <- dim(hbef$y)[2L]
  K <- dim(hbef$y)[3L]
  expect_equal(nrow(long), N * J * K - sum(is.na(hbef$y)))
  expect_equal(levels(long$series), dimnames(hbef$y)[[1L]])
  expect_true(all(c("Elevation", "day", "tod") %in% colnames(long)))

  arrays <- mvgam:::build_closure_unit_arrays(
    long, response_var = "y", compute_y_max = FALSE,
    unit_grouping_vars = c("series", "time")
  )
  expect_equal(arrays$N_unit, N * J)
  expect_true(all(arrays$n_rep <= K))
  expect_true(all(arrays$n_rep >= 1L))
})

test_that("2D single-species pivot composes with mvgam(family = occ())", {
  set.seed(12L)
  J <- 12L; K <- 3L
  y <- matrix(rbinom(J * K, 1L, 0.5), J, K)
  sc <- data.frame(env = rnorm(J))
  long <- pivot_detection_array(
    y, site_covs = sc, series_col = "series", site_col = "time"
  )
  # mvgam single-species: needs `time` + `series` columns. The
  # helper drops `series` for single-species; mvgam adds it as a
  # constant level downstream. Force a single series here so the
  # closure-unit codegen path can resolve.
  long$series <- factor("only")
  long$time <- long$time  # already a column
  expect_no_error(
    mvgam_formula(y ~ 1)
  )
  # Compile via stancode (no fit) to confirm the long-format frame
  # threads through the codegen path.
  mf <- mvgam_formula(y ~ 1)
  expect_no_error(
    suppressWarnings(stancode(
      mf, data = long, family = occ(), validate = FALSE
    ))
  )
})
