context("plot_descriptive")

test_that("plot_descriptive returns a ggplot object", {
  
  dat <- read.csv(
    system.file("extdata", "eq5d3l_example.csv", package = "eq5d")
  )
  
  dd <- descriptive_data(dat, version = "3L")
  p  <- plot_descriptive(dd)
  
  expect_s3_class(p, "ggplot")
})


test_that("plot_descriptive supports grouped descriptive data", {
  
  dat <- read.csv(
    system.file("extdata", "eq5d3l_example.csv", package = "eq5d")
  )
  
  dd_grp <- descriptive_data(dat, version = "3L", group = "Group")
  p      <- plot_descriptive(dd_grp)
  
  expect_s3_class(p, "ggplot")
})


test_that("plot_descriptive errors on invalid input", {
  
  bad <- data.frame(x = 1:5, y = 1:5)
  
  expect_error(
    plot_descriptive(bad),
    "descriptive_data"
  )
})


test_that("plot_descriptive accepts dimension label mapping", {
  
  dat <- read.csv(
    system.file("extdata", "eq5d3l_example.csv", package = "eq5d")
  )
  
  dd <- descriptive_data(dat, version = "3L")
  
  p <- plot_descriptive(
    dd,
    dimension_labels = c(
      MO = "Mobility",
      SC = "Self care",
      UA = "Usual activities",
      PD = "Pain & Discomfort",
      AD = "Anxiety & Depression"
    )
  )
  
  expect_s3_class(p, "ggplot")
})
