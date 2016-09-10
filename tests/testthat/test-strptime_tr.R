context("Parsing date")

test_that("Test that TR dates are parsed correctly", {
  format <- "%d %B %Y"
  expect_equal(strptime_tr("14 Ağustos 2020", format),
               strptime("14 August 2020",  format))
})

test_that("Test that system locale doesn't stay modified", {
  start_lc_time <- Sys.getlocale("LC_TIME")
  strptime_tr("14 Ağustos 2020", "%d %B %Y")
  end_lc_time <- Sys.getlocale("LC_TIME")

  expect_equal(start_lc_time, end_lc_time)
})
