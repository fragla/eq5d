context("descriptive_data")

test_that("descriptive_data returns a data.frame with required columns", {
  
  dat <- read.csv(
    system.file("extdata", "eq5d3l_example.csv", package = "eq5d")
  )
  
  dd <- descriptive_data(dat, version = "3L")
  
  expect_s3_class(dd, "data.frame")
  expect_true(all(
    c("Dimension", "Level", "Value", "Metric") %in% names(dd)
  ))
})


test_that("descriptive_data supports counts and percentages", {
  
  dat <- read.csv(
    system.file("extdata", "eq5d3l_example.csv", package = "eq5d")
  )
  
  dd_pct <- descriptive_data(dat, version = "3L", metric = "percent")
  dd_cnt <- descriptive_data(dat, version = "3L", metric = "count")
  
  expect_true(all(dd_pct$Metric == "percent"))
  expect_true(all(dd_cnt$Metric == "count"))
})


test_that("descriptive_data supports grouping", {
  
  dat <- read.csv(
    system.file("extdata", "eq5d3l_example.csv", package = "eq5d")
  )
  
  dd_grp <- descriptive_data(dat, version = "3L", group = "Group")
  
  expect_true("Group" %in% names(dd_grp))
  expect_true(length(unique(dd_grp$Group)) > 1)
})


test_that("descriptive_data errors with missing grouping variable", {
  
  dat <- read.csv(
    system.file("extdata", "eq5d3l_example.csv", package = "eq5d")
  )
  
  expect_error(
    descriptive_data(dat, version = "3L", group = "NotAColumn"),
    "Grouping column not found"
  )
})
