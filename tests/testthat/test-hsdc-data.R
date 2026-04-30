test_that("make_hsdi_by_group computes one HSDI per group", {
  
  dat <- read.csv(
    system.file("extdata", "eq5d3l_example.csv", package = "eq5d")
  )
  
  res <- make_hsdi_by_group(dat, group = "Group", version = "3L")
  
  expect_true(is.numeric(res))
  expect_true(is.vector(res))
  expect_true(!is.null(names(res)))
  
  expect_setequal(
    names(res),
    unique(dat$Group)
  )
})


test_that("make_hsdi_by_group matches manual split computation", {
  
  dat <- read.csv(
    system.file("extdata", "eq5d3l_example.csv", package = "eq5d")
  )
  
  manual <- sapply(
    split(dat, dat$Group),
    function(d) {
      hsdi(d, version = "3L")
    }
  )
  
  helper <- make_hsdi_by_group(dat, group = "Group", version = "3L")
  
  expect_equal(helper, manual)
})
