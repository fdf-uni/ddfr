test_that("pmf gives correct value at elements in support", {
  expect_equal(pmf(unif(6))(3), 1 / 6)
  expect_equal(pmf(bin(20, 0.3))(5), choose(20, 5) * 0.3^5 * (1 - 0.3)^15)
})

test_that("pmf is zero at elements not in support", {
  expect_equal(pmf(unif(6))(3.5), 0)
})

test_that("cdf is zero for elements less than minimum of support", {
  expect_equal(cdf(unif(6))(0), 0)
  expect_equal(cdf(rademacher())(-1.5), 0)
})

test_that("cdf is one for elements larger than maximum of support", {
  expect_equal(cdf(unif(6))(7), 1)
  expect_equal(cdf(rademacher())(1.5), 1)
})
