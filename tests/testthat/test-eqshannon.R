context("EQ-5D Shannon Index")

pre <- read.csv("../testdata/pre.csv")$x

test_that("eqshannon five digit gives correct answer", {
  expect_equal(shannon(pre, version="3L", by.dimension=TRUE)$MO, list(H=0.97, H.max=1.58, J=0.61))
  expect_equal(shannon(pre, version="3L", by.dimension=TRUE)$SC, list(H=1.09, H.max=1.58, J=0.68))
  expect_equal(shannon(pre, version="3L", by.dimension=TRUE)$UA, list(H=1.10, H.max=1.58, J=0.69))
  expect_equal(shannon(pre, version="3L", by.dimension=TRUE)$PD, list(H=1.14, H.max=1.58, J=0.72))
  expect_equal(shannon(pre, version="3L", by.dimension=TRUE)$AD, list(H=1.00, H.max=1.58, J=0.63)) 
  expect_equal(shannon(pre, version="3L", by.dimension=FALSE), list(H=4.33, H.max=6.97, J=0.62))
})

test_that("eqshannon throws error", {
  expect_error(shannon(pre, version="3L", by.dimension=TRUE, ignore.invalid=FALSE))
})

pre.df <- read.csv("../testdata/pre_df.csv")

test_that("eqshannon data.frame gives correct answer", {
  expect_equal(shannon(pre.df, version="3L", by.dimension=TRUE)$MO, list(H=0.97, H.max=1.58, J=0.61))
  expect_equal(shannon(pre.df, version="3L", by.dimension=TRUE)$SC, list(H=1.09, H.max=1.58, J=0.68))
  expect_equal(shannon(pre.df, version="3L", by.dimension=TRUE)$UA, list(H=1.10, H.max=1.58, J=0.69))
  expect_equal(shannon(pre.df, version="3L", by.dimension=TRUE)$PD, list(H=1.14, H.max=1.58, J=0.72))
  expect_equal(shannon(pre.df, version="3L", by.dimension=TRUE)$AD, list(H=1.00, H.max=1.58, J=0.63)) 
})

test_that("eqshannon data.frame throws error", {
  expect_error(shannon(pre.df, version="3L", dimensions=c("M0","SC","UA","PD","AD")))
  expect_error(shannon(pre.df, version="3L", ignore.invalid=FALSE))
})
