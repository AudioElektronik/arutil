context("Removing duplicate rows")

test_that("Test that duplicate rows are deleted", {
  x <- tibble::tibble(x = c(1, 2), y = c('x', 'y'))
  y <- dplyr::bind_rows(x, x)

  expect_equal(remove_duplicate(y), x)
})


