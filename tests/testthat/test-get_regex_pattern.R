context("Extracting regex pattern")

test_that("Test that correct characters are extracted for regex patterns", {
  expect_equal(get_regex_pattern(c("John-1", "Jess-28", NA, "Jude"),
                                 "[A-Za-z]+-([0-9]+)"),
               c("1", "28", "", ""))
})
