# The summary and print methods of `species_effects()`, on an object
# built from known draws. The fits that compute the draws are driven
# in tests/local.

test_that("summary and print report each species and effect's own cell", {
  arr <- array(0, c(10L, 2L, 2L),
               dimnames = list(NULL, c("a", "b"), c("x", "z")))
  arr[, "a", "x"] <- 1
  arr[, "b", "x"] <- -1
  # Positive in 6 of 10 draws, short of the threshold
  arr[, "a", "z"] <- rep(c(1, -1), c(6L, 4L))
  arr[, "b", "z"] <- 2
  x <- structure(
    list(
      species = mvgam:::summarise_signed_array(
        arr, FALSE, c(0.025, 0.975), 0.95, dimnames(arr)[2:3]
      ),
      species_column = "species", prob_threshold = 0.95,
      probs = c(0.025, 0.975), draw_ids = 1:10
    ),
    class = "mvgam_species_effects"
  )
  s <- summary(x)
  expect_identical(paste(s$species, s$effect, s$Estimate, s$sig),
                   c("a x 1 TRUE", "b x -1 TRUE", "a z 0.2 FALSE",
                     "b z 2 TRUE"))
  # One supported species of each sign responds to x, one positive to z
  shown <- capture.output(print(x))
  expect_true(any(grepl("^ x\\s+1\\s+1\\s+2", shown)))
  expect_true(any(grepl("^ z\\s+1\\s+0\\s+2", shown)))
})
