test_that("variance gives same result as some formulas", {
  # Test variables
  N <- 20
  p <- 0.4
  K <- 10
  n <- 5
  # Actual tests
  expect_equal(variance(unif(N)), ((N - 1 + 1)^2 - 1) / 12)
  expect_equal(variance(bin(N, p)), N * p * (1 - p))
  expect_equal(
    variance(hypergeometric(N, K, n)),
    n * K / N * (N - K) / N * (N - n) / (N - 1)
  )
  expect_equal(
    variance(negative_hypergeometric(N, K, n)),
    n * ((N + 1) * K) / ((N - K + 1) * (N - K + 2)) * (1 - n / (N - K + 1))
  )
})
