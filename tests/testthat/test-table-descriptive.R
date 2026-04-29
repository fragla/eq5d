test_that("percentage descriptive table is created correctly (ungrouped)", {
  
  dat <- read.csv(
    system.file("extdata", "eq5d3l_example.csv", package = "eq5d")
  )
  
  dd_pct <- descriptive_data(dat, version = "3L")
  
  tab <- table_descriptive(dd_pct)
  
  expect_true(is.data.frame(tab))
  expect_true("Level" %in% names(tab))
  expect_true(all(c("MO","SC","UA","PD","AD") %in% names(tab)))
  
  ## Percentages should sum to ~100 within each dimension
  expect_equal(sum(tab$MO[tab$Level != "Total"]), 100, tolerance = 1e-6)
  expect_equal(sum(tab$SC[tab$Level != "Total"]), 100, tolerance = 1e-6)
  
  ## Total row for percentage tables
  expect_true("Total" %in% tab$Level)
})


test_that("count descriptive table is created correctly (ungrouped)", {
  
  dat <- read.csv(
    system.file("extdata", "eq5d3l_example.csv", package = "eq5d")
  )
  
  dd_cnt <- descriptive_data(dat, version = "3L", metric = "count")
  
  tab <- table_descriptive(dd_cnt)
  
  expect_true(is.data.frame(tab))
  expect_true("Total" %in% tab$Level)
  
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
  
  tabs <- table_descriptive(dd_grp, include_total = FALSE)
  
  expect_type(tabs, "list")
  expect_setequal(names(tabs), unique(dd_grp$Group))
  
  ## Each group should produce its own table
  expect_true(is.data.frame(tabs$Group1))
  expect_true(is.data.frame(tabs$Group2))
  
  ## Percentages sum to 100 per dimension within each group
  expect_equal(sum(tabs$Group1$MO), 100, tolerance = 1e-6)
  expect_equal(sum(tabs$Group2$MO), 100, tolerance = 1e-6)
})


test_that("table_descriptive errors when descriptive_data contains multiple metrics", {
  
  dat <- read.csv(
    system.file("extdata", "eq5d3l_example.csv", package = "eq5d")
  )

  ## Manually violate the invariant by duplicating data
  dd <- descriptive_data(dat, version = "3L", metric = "percent")
  dd$Metric <- c("percent", "count")[dd$Metric == "percent"]
  
  expect_error(
    table_descriptive(dd),
    "must contain exactly one metric"
  )
})
