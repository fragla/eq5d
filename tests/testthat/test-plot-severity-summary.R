context("plot_severity_summary")

test_that("plot_severity_summary returns a ggplot object for LFS", {
  
  dat <- read.csv(
    system.file("extdata", "eq5d3l_example.csv", package = "eq5d")
  )
  
  p <- plot_severity_summary(
    data     = dat,
    country  = "UK",
    version  = "3L",
    type     = "TTO",
    severity = "LFS"
  )
  
  expect_s3_class(p, "ggplot")
})


test_that("plot_severity_summary returns a ggplot object for LSS", {
  
  dat <- read.csv(
    system.file("extdata", "eq5d3l_example.csv", package = "eq5d")
  )
  
  p <- plot_severity_summary(
    data     = dat,
    country  = "UK",
    version  = "3L",
    type     = "TTO",
    severity = "LSS"
  )
  
  expect_s3_class(p, "ggplot")
})


test_that("plot_severity_summary errors on invalid EQ-5D version", {
  
  dat <- read.csv(
    system.file("extdata", "eq5d3l_example.csv", package = "eq5d")
  )
  
  expect_error(
    plot_severity_summary(
      data     = dat,
      country  = "UK",
      version  = "2L",
      type     = "TTO",
      severity = "LFS"
    ),
    "EQ-5D version not one of"
  )
})


test_that("plot_severity_summary accepts custom tick width", {
  
  dat <- read.csv(
    system.file("extdata", "eq5d3l_example.csv", package = "eq5d")
  )
  
  p <- plot_severity_summary(
    data       = dat,
    country    = "UK",
    version    = "3L",
    type       = "TTO",
    severity   = "LFS",
    tick_width = 0.12
  )
  
  expect_s3_class(p, "ggplot")
})
