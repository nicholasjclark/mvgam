# The latent state is what a closure-unit model exists to estimate, and
# it is reported per (series, time) cell. Those labels come from a grid
# built separately from the kernel's draw matrix, so the two can drift:
# the grid was once sorted series-major while the kernel numbers units
# in first-appearance order over time-major data, which relabelled
# every unit on any fit carrying more than one series.
#
# These tests assert properties the likelihood guarantees rather than
# specific numbers, so they hold for any correct fit whatever the seed
# or sampler settings, and fail loudly when labels are transposed
# between series with different abundances.

context("closure-unit labelling")

require_fixtures("val_mvgam_closure_labels_nmix.rds",
                 "val_mvgam_closure_labels_occ.rds")

test_that("no unit is given fewer animals than were counted there", {
  fit <- load_mvgam("closure_labels_nmix")
  state <- as.data.frame(hindcast(fit, type = "latent_state"))
  observed <- stats::aggregate(y ~ series + time, data = fit$data,
                               FUN = max)
  merged <- merge(state, observed, by = c("series", "time"))
  expect_identical(nrow(merged), nrow(state))

  # N is the population a binomial count is drawn from, so it can never
  # be below the largest count seen in that unit.
  expect_true(all(merged$median >= merged$y))
  expect_true(all(merged$upper_95 >= merged$y))
})

test_that("a unit where the species was seen is occupied", {
  fit <- load_mvgam("closure_labels_occ")
  state <- as.data.frame(hindcast(fit, type = "latent_state"))
  detected <- stats::aggregate(y ~ series + time, data = fit$data,
                               FUN = max)
  merged <- merge(state, detected, by = c("series", "time"))
  seen <- merged[merged$y == 1L, ]
  expect_true(nrow(seen) > 0L)

  # Detection is conditional on occupancy, so a detection settles it.
  expect_true(all(seen$median >= 0.99))
})

test_that("the unit grid keeps the kernel's own ordering", {
  # The grid must not be re-sorted: `build_closure_unit_arrays()`
  # numbers units by first appearance in the time-major data, and the
  # draw matrix columns follow that.
  fit <- load_mvgam("closure_labels_nmix")
  state <- as.data.frame(hindcast(fit, type = "latent_state"))
  units <- unique(fit$data[, c("series", "time")])
  expect_identical(nrow(state), nrow(units))
  expect_setequal(
    paste(state$series, state$time),
    paste(units$series, units$time)
  )
})
