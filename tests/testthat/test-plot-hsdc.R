context("plot_hsdc")

test_that("plot_hsdc returns a ggplot object for ungrouped data", {
  
  dat <- read.csv(
    system.file("extdata", "eq5d3l_example.csv", package = "eq5d")
  )
  
  cf <- eq5dcf(dat, version = "3L")
  h  <- hsdi(dat, version = "3L")
  
  p <- plot_hsdc(cf, hsdi = h)
  
  expect_s3_class(p, "ggplot")
})


test_that("plot_hsdc supports grouped HSDC data", {
  
  dat <- read.csv(
    system.file("extdata", "eq5d3l_example.csv", package = "eq5d")
  )
  
  hsdc_grp <- make_hsdc_by_group(dat, group = "Group", version = "3L")
  hsdi_grp <- make_hsdi_by_group(dat, group = "Group", version = "3L")
  
  p <- plot_hsdc(
    data  = hsdc_grp,
    group = "Group",
    hsdi  = hsdi_grp
  )
  
  expect_s3_class(p, "ggplot")
})


test_that("plot_hsdc errors with missing required columns", {
  
  bad <- data.frame(x = 1:5, y = 1:5)
  
  expect_error(
    plot_hsdc(bad),
    "CumulativeProp"
  )
})