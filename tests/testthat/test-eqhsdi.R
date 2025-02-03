context("EQ-5D HSDI")

pre <- read.csv("../testdata/pre.csv")$x

test_that("eqhsdi five digit gives correct answer", {
  expect_equal(hsdi(pre, version="3L"), 0.56)
})

test_that("eqhsdi throws error", {
  expect_error(hsdi(pre, version="3L", ignore.invalid=FALSE))
  expect_error(hsdi(pre))
})

test_that("eqhsdi using version='Y' is deprecated", {
  rlang::local_options(lifecycle_verbosity = "error")
  expect_error(hsdi(pre, version="Y"))
})

test_that("eq5dhsdi using version='Y' still works", {
  rlang::local_options(lifecycle_verbosity = "quiet")
  expect_equal(hsdi(pre, version="Y"), 0.56)
})
