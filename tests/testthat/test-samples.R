test_that("samples have the requested length", {
  dist <- unif(6)
  n <- 10
  expect_length(samples(dist, n), n)
})
