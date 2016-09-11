context("Status log generator")

test_that("Test that status update generator is measuring speed correctly", {
  update_fn <- status_update_fn_generator("Testing", 0, 10)
  Sys.sleep(0.1)
  expect_message(
    update_fn(1),
    paste0("Testing: Running for 00:00 - Remaining 00:00 - ",
           "Speed (10|9)\\.[0-9]/s - Completed % 10.0 - Count 1"))
})
