context("Test utility functions")

test_that("make_par_string does a reasonable thing", {
  test_list <- list(a = 1, b = 2.1, c = -1)
  output_string <- make_par_string(test_list)

  expect_equal(
    output_string,
    "a=1-b=2.1-c=-1"
  )

})