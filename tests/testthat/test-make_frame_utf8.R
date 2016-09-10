context("Changing data.frame encoding to utf8")

test_that("Test that character columns encoding becomes utf8", {
  chars <- c("fa\xe7ile", "\xe7erez")
  Encoding(chars) <- "latin1"

  df <- tibble::tibble(x = chars, y = c(1, 2)) %>%
    make_frame_utf8()

  expect_equal(unique(Encoding(df$x)), "UTF-8")
})
