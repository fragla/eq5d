test_that("percentage descriptive table is created correctly (ungrouped)", {
  
  dat <- read.csv(
    system.file("extdata", "eq5d3l_example.csv", package = "eq5d")
  )
  
  dd_pct <- descriptive_data(dat, version = "3L")
  
  tab <- table_descriptive(dd_pct, type = "percent")
  
  expect_true(is.data.frame(tab))
  expect_true("Level" %in% names(tab))
  expect_true(all(c("MO","SC","UA","PD","AD") %in% names(tab)))
  
  ## Percentages should sum to ~100 within each dimension
  expect_equal(sum(tab$MO), 100, tolerance = 1e-6)
  expect_equal(sum(tab$SC), 100, tolerance = 1e-6)
  
  ## No Total column for percentage tables
  expect_false("Total" %in% names(tab))
})


test_that("count descriptive table is created correctly (ungrouped)", {
  
  dat <- read.csv(
    system.file("extdata", "eq5d3l_example.csv", package = "eq5d")
  )
  
  dd_cnt <- descriptive_data(dat, version = "3L", metric = "count")
  
  tab <- table_descriptive(dd_cnt, type = "count")
  
  expect_true(is.data.frame(tab))
  expect_true("Total" %in% names(tab))
  
  ## With example data, total respondents should be consistent across dimensions
  expect_equal(
    sum(tab$MO),
    sum(tab$SC)
  )
})


test_that("grouped percentage tables are stratified by Group", {
  
  dat <- read.csv(
    system.file("extdata", "eq5d3l_example.csv", package = "eq5d")
  )
  
  dd_grp <- descriptive_data(dat, version = "3L", group = "Group")
  
  tabs <- table_descriptive(dd_grp, type = "percent")
  
  expect_type(tabs, "list")
  expect_true(all(c("Group1","Group2") %in% names(tabs)))
  
  ## Each group should produce its own table
  expect_true(is.data.frame(tabs$Group1))
  expect_true(is.data.frame(tabs$Group2))
  
  ## Percentages sum to 100 per dimension within each group
  expect_equal(sum(tabs$Group1$MO), 100, tolerance = 1e-6)
  expect_equal(sum(tabs$Group2$MO), 100, tolerance = 1e-6)
})


test_that("type mismatch errors clearly", {
  
  dat <- read.csv(
    system.file("extdata", "eq5d3l_example.csv", package = "eq5d")
  )
  
  dd_pct <- descriptive_data(dat, version = "3L")
  
  expect_error(
    table_descriptive(dd_pct, type = "count"),
    "descriptive_data contains metric"
  )
})
