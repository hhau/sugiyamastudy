context("Test that the plotting function can plot object")
  
test_that("plot function can produce a plot", {
  numerator_density <- make_density_obj()
  denominator_density <- make_density_obj()

  ratio_obj <- make_ratio_obj(
    numerator_density,
    denominator_density,
    ratio_method = "clsif",
    lambda = 0.1
  )  

  res <- plot_ratio(ratio_obj)
  expect_s3_class(res, "ggplot")

})

test_that("plot function can write a pdf", {
  numerator_density <- make_density_obj()
  denominator_density <- make_density_obj()

  ratio_obj <- make_ratio_obj(
    numerator_density,
    denominator_density,
    ratio_method = "clsif",
    lambda = 0.1
  )  

  plot_directory <- tempdir()

  res <- plot_ratio(
    ratio_obj,
    plot_directory
  )

  pdf_file <- Sys.glob(file.path(
    plot_directory,
    "*.pdf"
  ))

  expect_true(
    length(pdf_file) == 1
  )

})