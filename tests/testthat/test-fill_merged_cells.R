context("Filling merged cells")

test_that("Test that merged cells are filled with the previous cell value", {
  x <- c("A", NA, NA, "B", NA)
  expect_equal(fill_merged_cells(x), c("A", "A", "A", "B", "B"))
})
