context("EQ-5D PCHC")

pre <- read.csv("../testdata/pre.csv")$x
post <- read.csv("../testdata/post.csv")$x

test_that("eqpchc five digit gives correct answer", {
  expect_equal(ps(pre, post, version="3L", ignore.invalid=TRUE)$MO, 0.59)
  expect_equal(ps(pre, post, version="3L", ignore.invalid=TRUE)$SC, 0.62)
  expect_equal(ps(pre, post, version="3L", ignore.invalid=TRUE)$UA, 0.61)
  expect_equal(ps(pre, post, version="3L", ignore.invalid=TRUE)$PD, 0.66)
  expect_equal(ps(pre, post, version="3L", ignore.invalid=TRUE)$AD, 0.56)  
})

test_that("eqps throws error", {
  expect_error(ps(pre, post, version="3L", ignore.invalid=F))
})

pre.df <- read.csv("../testdata/pre_df.csv")
post.df <- read.csv("../testdata/post_df.csv")

test_that("eqpchc data.frame gives correct answer", {
  expect_equal(ps(pre, post, version="3L", ignore.invalid=TRUE)$MO, 0.59)
  expect_equal(ps(pre, post, version="3L", ignore.invalid=TRUE)$SC, 0.62)
  expect_equal(ps(pre, post, version="3L", ignore.invalid=TRUE)$UA, 0.61)
  expect_equal(ps(pre, post, version="3L", ignore.invalid=TRUE)$PD, 0.66)
  expect_equal(ps(pre, post, version="3L", ignore.invalid=TRUE)$AD, 0.56)  
})

# 
# test_that("eqpchc data.frame gives correct answer", {
#   expect_equal(pchc(pre.df, post.df, version="3L", no.problems=TRUE, totals=TRUE), res1)
#   expect_equal(pchc(pre.df, post.df, version="3L", no.problems=TRUE, totals=FALSE), res2)
#   expect_equal(pchc(pre.df, post.df, version="3L", no.problems=FALSE, totals=FALSE), res3)
#   expect_equal(pchc(pre.df, post.df, version="3L", no.problems=FALSE, totals=TRUE), res4)
# })
# 
# test_that("eqpchc data.frame throws error", {
#   expect_error(pchc(pre.df, post.df, version="3L", dimensions=c("M0","SC","UA","PD","AD")))
#   expect_error(pchc(pre.df, post.df, version="3L", ignore.invalid=FALSE))
#   expect_error(pchc(pre.df[-1,], post.df, version="3L", ignore.invalid=FALSE))
# })

