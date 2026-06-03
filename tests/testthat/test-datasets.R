# Smoke tests for the stock datasets shipped under `data/`. Each
# test loads the dataset, checks the column names + classes, and
# asserts the basic hierarchical / state-space structure the
# documentation promises. Numerical-recovery tests against an
# mvgam fit live in `tests/local/`.


test_that("birdsong loads with expected structure", {
  data("birdsong")
  expect_s3_class(birdsong, "data.frame")
  expect_identical(
    colnames(birdsong),
    c("series", "time", "y", "species", "week",
      "week_in_year", "count")
  )
  expect_s3_class(birdsong$series, "factor")
  expect_s3_class(birdsong$species, "factor")
  expect_identical(nlevels(birdsong$species), 4L)
  expect_identical(nrow(birdsong), 320L)
  expect_true(all(birdsong$y >= 0L))
  expect_true(all(birdsong$y == round(birdsong$y)))
  expect_identical(max(birdsong$week), 80L)
  expect_true(all(birdsong$week_in_year >= 1L &
                     birdsong$week_in_year <= 52L))
})


test_that("lake_chemistry loads with expected structure", {
  data("lake_chemistry")
  expect_s3_class(lake_chemistry, "data.frame")
  expect_identical(
    colnames(lake_chemistry),
    c("series", "time", "y", "lake", "month", "treated",
      "chemistry")
  )
  expect_s3_class(lake_chemistry$series, "factor")
  expect_s3_class(lake_chemistry$lake, "factor")
  expect_identical(nlevels(lake_chemistry$lake), 5L)
  expect_identical(nrow(lake_chemistry), 300L)
  expect_true(all(is.finite(lake_chemistry$y)))
  expect_true(all(lake_chemistry$treated %in% c(0L, 1L)))
  # Treatment switches on at month 31.
  expect_true(all(
    lake_chemistry$treated[lake_chemistry$month < 31L] == 0L
  ))
  expect_true(all(
    lake_chemistry$treated[lake_chemistry$month >= 31L] == 1L
  ))
})


test_that("coral_surveys loads with expected structure", {
  data("coral_surveys")
  expect_s3_class(coral_surveys, "data.frame")
  expect_identical(
    colnames(coral_surveys),
    c("series", "time", "y", "reef", "sst", "bleaching")
  )
  expect_s3_class(coral_surveys$series, "factor")
  expect_s3_class(coral_surveys$reef, "factor")
  expect_identical(nlevels(coral_surveys$reef), 3L)
  expect_identical(nrow(coral_surveys), 150L)
  expect_true(all(is.finite(coral_surveys$y)))
  expect_true(all(is.finite(coral_surveys$sst)))
  # Irregular sampling: there must be at least one gap > 1.
  reef_gaps <- tapply(
    coral_surveys$time, coral_surveys$reef,
    function(t) diff(sort(unique(t)))
  )
  expect_true(any(unlist(reef_gaps) > 1L))
})


test_that("portal_data still loads (regression guard)", {
  data("portal_data")
  expect_s3_class(portal_data, "data.frame")
  expect_true("time" %in% colnames(portal_data))
})
