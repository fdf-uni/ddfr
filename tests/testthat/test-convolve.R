test_that("convolution works correctly for a uniform discrete distribution", {
  expect_equal(supp(conv(unif(6), unif(6))), 2:12)
  expect_equal(probs(conv(unif(6), unif(6))), c(
    1 / 36, 1 / 18, 1 / 12, 1 / 9, 5 / 36, 1 / 6, 5 / 36, 1 / 9, 1 / 12, 1 / 18, 1 / 36
  ))
  # Also check that generic * works
  expect_equal(supp(unif(6) * unif(6)), 2:12)
  expect_equal(probs(unif(6) * unif(6)), c(
    1 / 36, 1 / 18, 1 / 12, 1 / 9, 5 / 36, 1 / 6, 5 / 36, 1 / 9, 1 / 12, 1 / 18, 1 / 36
  ))
})

test_that("description works correctly for n-fold convolution", {
  expect_equal(
    desc(conv_n(rademacher(), 2)),
    "2-fold convolution of Rademacher distribution"
  )
  expect_equal(
    desc(conv_n(rademacher(), 2, "asdf")),
    "asdf"
  )
})

test_that("conv_n works correctly for `n` == 1 and `n` == 2", {
  # We set the name here for the results from conv_n such that equality is only
  # really tested for the supports and probabilities.
  expect_equal(conv_n(rademacher(), 1, "Rademacher distribution"), rademacher())
  # We can expect that `conv` works correctly here by the previous test
  expect_equal(conv_n(unif(6), 2, "A convolution"), conv(unif(6), unif(6)))
})

test_that("conv_n gives error when `n` is less than 1 or not an integer", {
  expect_error(conv_n(bin(20, 0.3), 0), "must be a positive integer")
  expect_error(conv_n(bin(20, 0.3), 2.5), "must be a positive integer")
})
