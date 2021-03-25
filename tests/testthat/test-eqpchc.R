context("EQ-5D PCHC")

pre <- read.csv("../testdata/pre.csv")$x
post <- read.csv("../testdata/post.csv")$x

res1 <- read.csv("../testdata/pchc_noprob_true_totals_true.csv", row.names=1, colClasses=c("character",rep("numeric",2)))
res2 <- read.csv("../testdata/pchc_noprob_true_totals_false.csv", row.names=1, colClasses=c("character",rep("numeric",2)))
res3 <- read.csv("../testdata/pchc_noprob_false_totals_false.csv", row.names=1, colClasses=c("character",rep("numeric",2)))
res4 <- read.csv("../testdata/pchc_noprob_false_totals_true.csv", row.names=1, colClasses=c("character",rep("numeric",2)))

res1.mo <- read.csv("../testdata/pchc_noprob_true_totals_true_dim_mo.csv", row.names=1, colClasses=c("character",rep("numeric",2)))
res2.mo <- read.csv("../testdata/pchc_noprob_true_totals_false_dim_mo.csv", row.names=1, colClasses=c("character",rep("numeric",2)))
res3.mo <- read.csv("../testdata/pchc_noprob_false_totals_false_dim_mo.csv", row.names=1, colClasses=c("character",rep("numeric",2)))
res4.mo <- read.csv("../testdata/pchc_noprob_false_totals_true_dim_mo.csv", row.names=1, colClasses=c("character",rep("numeric",2)))

test_that("eqpchc five digit gives correct answer", {
  expect_equal(pchc(pre, post, version="3L", no.problems=TRUE, totals=TRUE), res1)
  expect_equal(pchc(pre, post, version="3L", no.problems=TRUE, totals=FALSE), res2)
  expect_equal(pchc(pre, post, version="3L", no.problems=FALSE, totals=FALSE), res3)
  expect_equal(pchc(pre, post, version="3L", no.problems=FALSE, totals=TRUE), res4)
  expect_equal(pchc(pre, post, version="3L", no.problems=TRUE, totals=TRUE, by.dimension=TRUE)$MO, res1.mo)
  expect_equal(pchc(pre, post, version="3L", no.problems=TRUE, totals=FALSE, by.dimension=TRUE)$MO, res2.mo)
  expect_equal(pchc(pre, post, version="3L", no.problems=FALSE, totals=FALSE, by.dimension=TRUE)$MO, res3.mo)
  expect_equal(pchc(pre, post, version="3L", no.problems=FALSE, totals=TRUE, by.dimension=TRUE)$MO, res4.mo)
  expect_equal(pchc(pre, post, version="Y", no.problems=TRUE, totals=TRUE), res1)
})

test_that("eqpchc data.frame throws error", {
  expect_error(pchc(pre[-1,], post, version="3L", ignore.invalid=FALSE))
})

pre.df <- read.csv("../testdata/pre_df.csv")
post.df <- read.csv("../testdata/post_df.csv")

test_that("eqpchc data.frame gives correct answer", {
  expect_equal(pchc(pre.df, post.df, version="3L", no.problems=TRUE, totals=TRUE), res1)
  expect_equal(pchc(pre.df, post.df, version="3L", no.problems=TRUE, totals=FALSE), res2)
  expect_equal(pchc(pre.df, post.df, version="3L", no.problems=FALSE, totals=FALSE), res3)
  expect_equal(pchc(pre.df, post.df, version="3L", no.problems=FALSE, totals=TRUE), res4)
})

test_that("eqpchc data.frame throws error", {
  expect_error(pchc(pre.df, post.df, version="3L", dimensions=c("M0","SC","UA","PD","AD")))
  expect_error(pchc(pre.df, post.df, version="3L", ignore.invalid=FALSE))
  expect_error(pchc(pre.df[-1,], post.df, version="3L", ignore.invalid=FALSE))
})

