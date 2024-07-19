test_that("skewness gives same result as some formulas", {
  # Test variables
  N <- 20
  p <- 0.4
  K <- 10
  n <- 5
  # Actual tests
  expect_equal(skew(unif(N)), 0)
  expect_equal(skew(bin(N, p)), (1 - 2 * p) / sqrt(N * p * (1 - p)))
  expect_equal(
    skew(hypergeometric(N, K, n)),
    (N - 2 * K) * sqrt(N - 1) * (N - 2 * n) / (sqrt(n * K * (N - K) * (N - n)) * (N - 2))
  )
})

test_that("excess kurtosis gives same result as some formulas", {
  # Test variables
  N <- 20
  p <- 0.4
  K <- 10
  n <- 5
  # Actual tests
  expect_equal(excess_kurtosis(unif(N)), -6 / 5 * (N^2 + 1) / (N^2 - 1))
  expect_equal(excess_kurtosis(bin(N, p)), (1 - 6 * p * (1 - p)) / (N * p * (1 - p)))
  expect_equal(
    excess_kurtosis(hypergeometric(N, K, n)),
    ((N - 1) * N^2 * (N * (N + 1) - 6 * K * (N - K) - 6 * n * (N - n)) + 6 * n * K * (N - K) * (N - n) * (5 * N - 6)) /
      (n * K * (N - K) * (N - n) * (N - 2) * (N - 3))
  )
})
