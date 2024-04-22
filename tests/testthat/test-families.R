context("Tests for family functions")

test_that("distributions work correctly", {
  fam <- tweedie()
  expect_true(inherits(fam, 'family'))
  expect_true(inherits(fam, 'extended.family'))
  expect_true(fam$link == 'log')

  fam <- student_t()
  expect_true(inherits(fam, 'family'))
  expect_true(inherits(fam, 'extended.family'))
  expect_true(fam$link == 'identity')

  fam <- nmix()
  expect_true(inherits(fam, 'family'))
  expect_true(inherits(fam, 'extended.family'))
  expect_true(fam$link == 'log')
})
