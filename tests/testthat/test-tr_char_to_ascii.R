context("TR character translation")

test_that("Test that TR char translation is correct", {
  expect_equal(tr_char_to_ascii(c("Şekil", "Kağnı", "İzmir", NA)),
               c("Sekil", "Kagni", "Izmir", NA))
})
