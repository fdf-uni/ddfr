test_that("ddf throws error when support and probabilities don't have the same length", {
  expect_error(ddf(1:3, c(1 / 2, 1 / 2)), "Support and probabilities must have the same length")
})

test_that("ddf throws error when probabilities don't match criteria", {
  expect_error(ddf(1:3, c(1 / 2, 1 / 2, 1 / 2)), "probabilities have to sum up to approximately 1")
  expect_error(ddf(1:3, c(-1, 1 / 2, 3 / 2)), "probabilities have to be between 0 and 1")
})

test_that("ddf removes duplicates and non-support elements correctly", {
  expect_equal(ddf(c(1, 1), c(1 / 2, 1 / 2)), ddf(1, 1))
  expect_equal(ddf(c(1, 2), c(1, 0)), ddf(1, 1))
})

test_that("creation of new ddf objects sorts support and probabilities correctly", {
  # Use some unordered data
  dist <- ddf(c(1, 3, 0, 2, -1), c(1 / 7, 1 / 5, 1 / 3, 1 / 9, 67 / 315))
  expect_equal(supp(dist), c(-1, 0, 1, 2, 3))
  expect_equal(probs(dist), c(67 / 315, 1 / 3, 1 / 7, 1 / 9, 1 / 5))
})

test_that("setter for description works", {
  dist <- ddf(1:6, desc = "First description")
  expect_equal(desc(dist), "First description")
  desc(dist) <- "Second description"
  expect_equal(desc(dist), "Second description")
})

test_that("generic - works for `ddf` class", {
  # Check that support order is reversed
  expect_equal(supp(-ddf(1:3, c(1 / 6, 1 / 3, 1 / 2))), -3:-1)
  # Check that probabilities are in corresponding order
  expect_equal(probs(-ddf(1:3, c(1 / 6, 1 / 3, 1 / 2))), c(1 / 2, 1 / 3, 1 / 6))
})
