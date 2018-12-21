context("Test the object constructors")

library(sugiyamastudy)

test_that("density functions match", {
  val <- 0:10
  obj <- make_density_obj()
  expected <- dnorm(x = val, mean = 0, sd = 1)

  expect_equal(obj$density(x = val), expected)

})

test_that("ratio object is constructable", {
  numerator_density <- make_density_obj()
  denominator_density <- make_density_obj()

  ratio_obj <- make_ratio_obj(
    numerator_density,
    denominator_density,
    ratio_method = "clsif",
    lambda = 0.1
  )  

  expect_s3_class(ratio_obj, "ratio_sugi")

})

test_that("generator function produces output", {
  numerator_density <- make_density_obj()
  denominator_density <- make_density_obj()

  ratio_obj <- make_ratio_obj(
    numerator_density,
    denominator_density,
    ratio_method = "clsif",
    lambda = 0.1,
    keep_samples = TRUE
  )

  expect_equal(
    length(ratio_obj$numerator_density$samples),
    ratio_obj$numerator_density$sample_size
  )

})

test_that("generator function produces numeric output", {
  numerator_density <- make_density_obj()
  denominator_density <- make_density_obj()

  ratio_obj <- make_ratio_obj(
    numerator_density,
    denominator_density,
    ratio_method = "clsif",
    lambda = 0.1,
    keep_samples = TRUE
  )

  expect_true(
    is.numeric(ratio_obj$numerator_density$samples) ==
    is.numeric(ratio_obj$denominator_density$samples)
  )

})

test_that("NULL'ing samples deletes list element",{
  numerator_density <- make_density_obj()
  denominator_density <- make_density_obj()

  ratio_obj <- make_ratio_obj(
    numerator_density,
    denominator_density,
    ratio_method = "clsif",
    lambda = 0.1,
    keep_samples = FALSE
  )

  expect_null(
    ratio_obj$numerator_density$samples
  )

})