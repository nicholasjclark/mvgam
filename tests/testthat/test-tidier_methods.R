context("tidier methods")

# `tidy()` tests
test_that("`tidy()` snapshot value of `mvgam_example1`", {
  local_edition(3)
  expect_snapshot_value(tidy.mvgam(mvgam_example1), style = "json2")
})

test_that("`tidy()` snapshot value of `mvgam_example2`", {
  local_edition(3)
  expect_snapshot_value(tidy.mvgam(mvgam_example2), style = "json2")
})

test_that("`tidy()` snapshot value of `mvgam_example3`", {
  local_edition(3)
  expect_snapshot_value(tidy.mvgam(mvgam_example3), style = "json2")
})

test_that("`tidy()` snapshot value of `mvgam_example4`", {
  local_edition(3)
  expect_snapshot_value(tidy.mvgam(mvgam_example4), style = "json2")
})

test_that("`tidy()` snapshot value of `mvgam_example6`", {
  local_edition(3)
  testthat::skip_on_cran()

  # Hierarchical dynamics example adapted from RW documentation example.
  # The difference is that this uses 4 species rather than 3.
  simdat1 <- sim_mvgam(trend_model = VAR(cor = TRUE),
                       prop_trend = 0.95,
                       n_series = 4,
                       mu = c(1, 2, 3, 4))
  simdat2 <- sim_mvgam(trend_model = VAR(cor = TRUE),
                       prop_trend = 0.95,
                       n_series = 4,
                       mu = c(1, 2, 3, 4))
  simdat3 <- sim_mvgam(trend_model = VAR(cor = TRUE),
                       prop_trend = 0.95,
                       n_series = 4,
                       mu = c(1, 2, 3, 4))

  simdat_all <- rbind(simdat1$data_train %>%
                        dplyr::mutate(region = 'qld'),
                      simdat2$data_train %>%
                        dplyr::mutate(region = 'nsw'),
                      simdat3$data_train %>%
                        dplyr::mutate(region = 'vic')) %>%
    dplyr::mutate(species = gsub('series', 'species', series),
                  species = as.factor(species),
                  region = as.factor(region)) %>%
    dplyr::arrange(series, time) %>%
    dplyr::select(-series)

  mvgam_example6 <- suppressWarnings(mvgam(formula = y ~ species,
                          trend_model = AR(gr = region, subgr = species),
                          data = simdat_all))

  tidyout = tidy.mvgam(mvgam_example6)

  expect_equal(dim(tidyout), c(65, 7))
  expect_equal(colnames(tidyout),
               c("parameter", "type", "mean", "sd", "2.5%", "50%", "97.5%"))
  expect_snapshot_value(tidyout[c("parameter", "type")], style = "json2")
})

# `augment()` tests
test_that("augment doesn't error", {
  expect_no_error(augment(mvgam:::mvgam_example1))
  expect_no_error(augment(mvgam:::mvgam_example4))
})

test_that("augment returns correct types", {
  out1 <- augment(mvgam:::mvgam_example1)
  out4 <- augment(mvgam:::mvgam_example4)

  expect_equal(class(out1)[[1]], "tbl_df")
  expect_equal(class(out4), "list")

  # Lengths of augment output and of obs data should be equal
  expect_equal(NROW(out1), NROW(mvgam:::mvgam_example1$obs_data))
  expect_equal(length(out4$y), length(mvgam:::mvgam_example4$obs_data$y))

  # NAs in obs data should equal NAs in residuals
  expect_true(all(
    which(is.na(mvgam:::mvgam_example1$obs_data$y)) %in%
      which(is.na(out1$.resid))
  ))
})
