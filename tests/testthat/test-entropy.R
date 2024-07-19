test_that("entropy gives same result as formula for uniform discrete distribution", {
  # Base 2
  expect_equal(entropy(unif(5)), log(5, 2))
  expect_equal(entropy(unif(10)), log(10, 2))
  expect_equal(entropy(unif(20)), log(20, 2))

  # Base e
  expect_equal(entropy(unif(5), exp(1)), log(5))
  expect_equal(entropy(unif(10), exp(1)), log(10))
  expect_equal(entropy(unif(20), exp(1)), log(20))
})
