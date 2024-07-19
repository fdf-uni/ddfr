test_that("expected value gives same result as some formulas", {
  # Test variables
  N <- 20
  p <- 0.4
  K <- 10
  n <- 5
  # Actual tests
  expect_equal(expected_value(unif(N)), (N+1)/2)
  expect_equal(expected_value(bin(N, p)), N*p)
  expect_equal(expected_value(hypergeometric(N, K, n)), n*K/N)
  expect_equal(expected_value(negative_hypergeometric(N, K, n)), n*K/(N-K+1))
})

test_that("certain methods for medians and modes return vector of length 1", {
  dist <- unif(6)
  expect_length(medians(dist, "min"), 1)
  expect_length(medians(dist, "max"), 1)
  expect_length(medians(dist, "mean"), 1)
  expect_length(modes(dist, "min"), 1)
  expect_length(modes(dist, "max"), 1)
  expect_length(modes(dist, "mean"), 1)
})
