context("EQ-5D Shannon Index")

pre <- read.csv("../testdata/pre.csv")$x

test_that("eqshannon five digit gives correct answer", {
  res1 <- shannon(pre, version="3L", by.dimension=TRUE, permutations=TRUE)
  res2 <- shannon(pre, version="3L", by.dimension=FALSE, permutations=TRUE)
  res3 <- shannon(pre, version="3L", by.dimension=FALSE, permutations=FALSE)
  expect_equal(unname(as.numeric(res1[res1$dimension == "MO", c("H","H.max","J")])), c(0.97, 1.58, 0.61))
  expect_equal(unname(as.numeric(res1[res1$dimension == "SC", c("H","H.max","J")])), c(1.09, 1.58, 0.68))
  expect_equal(unname(as.numeric(res1[res1$dimension == "UA", c("H","H.max","J")])), c(1.10, 1.58, 0.69))
  expect_equal(unname(as.numeric(res1[res1$dimension == "PD", c("H","H.max","J")])), c(1.14, 1.58, 0.72))
  expect_equal(unname(as.numeric(res1[res1$dimension == "AD", c("H","H.max","J")])), c(1.00, 1.58, 0.63))
  expect_equal(unname(as.numeric(res2[,c("H","H.max","J")])), c(4.33, 7.92, 0.55))
  expect_equal(unname(as.numeric(res3[,c("H","H.max","J")])), c(4.33, 4.81, 0.90))
})

test_that("eqshannon throws error", {
  expect_error(shannon(pre, version="3L", by.dimension=TRUE, permutations=TRUE, ignore.invalid=FALSE))
  expect_error(shannon(pre, version="3L", by.dimension=TRUE, permutations=FALSE, ignore.invalid=FALSE))
  expect_error(shannon(pre, by.dimension=FALSE, permutations=FALSE))
})

pre.df <- read.csv("../testdata/pre_df.csv")

test_that("eqshannon data.frame gives correct answer", {
  res4 <- shannon(pre.df, version="3L", by.dimension=TRUE, permutations=TRUE)
  res5 <- shannon(pre.df, version="3L", by.dimension=TRUE, permutations=FALSE)
  expect_equal(unname(as.numeric(res4[res4$dimension == "MO", c("H","H.max","J")])), c(0.97, 1.58, 0.61))
  expect_equal(unname(as.numeric(res4[res4$dimension == "SC", c("H","H.max","J")])), c(1.09, 1.58, 0.68))
  expect_equal(unname(as.numeric(res4[res4$dimension == "UA", c("H","H.max","J")])), c(1.10, 1.58, 0.69))
  expect_equal(unname(as.numeric(res4[res4$dimension == "PD", c("H","H.max","J")])), c(1.14, 1.58, 0.72))
  expect_equal(unname(as.numeric(res4[res4$dimension == "AD", c("H","H.max","J")])), c(1.00, 1.58, 0.63)) 
  expect_equal(unname(as.numeric(res5[res5$dimension == "MO", c("H","H.max","J")])), c(0.97, 1, 0.97))
  expect_equal(unname(as.numeric(res5[res5$dimension == "SC", c("H","H.max","J")])), c(1.09, 1.58, 0.68))
  expect_equal(unname(as.numeric(res5[res5$dimension == "UA", c("H","H.max","J")])), c(1.10, 1.58, 0.69))
  expect_equal(unname(as.numeric(res5[res5$dimension == "PD", c("H","H.max","J")])), c(1.14, 1.58, 0.72))
  expect_equal(unname(as.numeric(res5[res5$dimension == "AD", c("H","H.max","J")])), c(1.00, 1.00, 1.00)) 
  
})

test_that("eqshannon data.frame throws error", {
  expect_error(shannon(pre.df, version="3L", permutations=TRUE, dimensions=c("M0","SC","UA","PD","AD")))
  expect_error(shannon(pre.df, version="3L", permutations=TRUE, ignore.invalid=FALSE))
})

test_that("eqshannon using version='Y' is deprecated", {
  rlang::local_options(lifecycle_verbosity = "error")
  expect_error(shannon(pre, version="Y", by.dimension=TRUE, permutations=TRUE))
})

test_that("eq5dshannon using version='Y' still works", {
  rlang::local_options(lifecycle_verbosity = "quiet")
  res6 <- shannon(pre, version="Y", by.dimension=TRUE, permutations=TRUE)
  expect_equal(unname(as.numeric(res6[res6$dimension == "MO", c("H","H.max","J")])), c(0.97, 1.58, 0.61))
})
