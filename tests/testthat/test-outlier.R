context("Outlier tests")

test_that("Test that outliers are correctly discarded", {
  x <- tibble::tibble(a = 1:50)

  # Discard top and botton 10%
  discarded <- discard_outliers(x, "a", 0.10)

  expect_equal(discarded$a, 6:45)
})
