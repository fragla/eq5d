context("EQ-5D Shannon Index")

pre <- read.csv("../testdata/pre.csv")$x

test_that("eqshannon five digit gives correct answer", {
  expect_equal(shannon(pre, version="3L", by.dimension=TRUE, permutations=TRUE)$MO, list(H=0.97, H.max=1.58, J=0.61))
  expect_equal(shannon(pre, version="3L", by.dimension=TRUE, permutations=TRUE)$SC, list(H=1.09, H.max=1.58, J=0.68))
  expect_equal(shannon(pre, version="3L", by.dimension=TRUE, permutations=TRUE)$UA, list(H=1.10, H.max=1.58, J=0.69))
  expect_equal(shannon(pre, version="3L", by.dimension=TRUE, permutations=TRUE)$PD, list(H=1.14, H.max=1.58, J=0.72))
  expect_equal(shannon(pre, version="3L", by.dimension=TRUE, permutations=TRUE)$AD, list(H=1.00, H.max=1.58, J=0.63))
  expect_equal(shannon(pre, version="3L", by.dimension=FALSE, permutations=TRUE), list(H=4.33, H.max=7.92, J=0.55))
  expect_equal(shannon(pre, version="3L", by.dimension=FALSE, permutations=FALSE), list(H=4.33, H.max=4.81, J=0.90))
})

test_that("eqshannon throws error", {
  expect_error(shannon(pre, version="3L", by.dimension=TRUE, permutations=TRUE, ignore.invalid=FALSE))
  expect_error(shannon(pre, version="3L", by.dimension=TRUE, permutations=FALSE, ignore.invalid=FALSE))
  expect_error(shannon(pre, by.dimension=FALSE, permutations=FALSE))
})

pre.df <- read.csv("../testdata/pre_df.csv")

test_that("eqshannon data.frame gives correct answer", {
  expect_equal(shannon(pre.df, version="3L", by.dimension=TRUE, permutations=TRUE)$MO, list(H=0.97, H.max=1.58, J=0.61))
  expect_equal(shannon(pre.df, version="3L", by.dimension=TRUE, permutations=TRUE)$SC, list(H=1.09, H.max=1.58, J=0.68))
  expect_equal(shannon(pre.df, version="3L", by.dimension=TRUE, permutations=TRUE)$UA, list(H=1.10, H.max=1.58, J=0.69))
  expect_equal(shannon(pre.df, version="3L", by.dimension=TRUE, permutations=TRUE)$PD, list(H=1.14, H.max=1.58, J=0.72))
  expect_equal(shannon(pre.df, version="3L", by.dimension=TRUE, permutations=TRUE)$AD, list(H=1.00, H.max=1.58, J=0.63)) 
  expect_equal(shannon(pre.df, version="3L", by.dimension=TRUE, permutations=FALSE)$MO, list(H=0.97, H.max=1, J=0.97))
  expect_equal(shannon(pre.df, version="3L", by.dimension=TRUE, permutations=FALSE)$SC, list(H=1.09, H.max=1.58, J=0.68))
  expect_equal(shannon(pre.df, version="3L", by.dimension=TRUE, permutations=FALSE)$UA, list(H=1.10, H.max=1.58, J=0.69))
  expect_equal(shannon(pre.df, version="3L", by.dimension=TRUE, permutations=FALSE)$PD, list(H=1.14, H.max=1.58, J=0.72))
  expect_equal(shannon(pre.df, version="3L", by.dimension=TRUE, permutations=FALSE)$AD, list(H=1.00, H.max=1.00, J=1.00)) 
  
})

test_that("eqshannon data.frame throws error", {
  expect_error(shannon(pre.df, version="3L", permutations=TRUE, dimensions=c("M0","SC","UA","PD","AD")))
  expect_error(shannon(pre.df, version="3L", permutations=TRUE, ignore.invalid=FALSE))
})
