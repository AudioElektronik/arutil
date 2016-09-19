context("İstanbul town conversion")

test_that("Tests that towns are correctly assigned to İstanbul parts", {
  x <- tibble::frame_data(
    ~town,       ~city,
    "Kadıköy",   "İstanbul",
    "Eyüp",      "İstanbul"
  )

  expect_equal(add_istanbul_part(x)$city,
               c("İstanbul (Anadolu)", "İstanbul (Avrupa)"))
})
