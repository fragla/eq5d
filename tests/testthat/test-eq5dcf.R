context("EQ-5D Cumulative Frequency")

dat <- read.csv("../testdata/pre.csv")$x
res <- read.csv("../testdata/eq5dcf.csv", colClasses=c("character",rep("numeric",4)))

test_that("eq5dcf five digit gives correct answer", {
  expect_equal(eq5dcf(dat, version="3L", ignore.invalid=TRUE), res)
  expect_equal(eq5dcf(dat, version="Y", ignore.invalid=TRUE), res)
})

test_that("eq5dcf five digit throws error", {
  expect_error(eq5dcf(dat, version="3L", ignore.invalid=FALSE))
  expect_error(eq5dcf(dat, version="Y", ignore.invalid=FALSE))
})

dat.df <- read.csv("../testdata/pre_df.csv")
dat.df.nam <- data.frame(Mob=dat.df$MO, SelCa=dat.df$SC, UsAct=dat.df$UA,PDis=dat.df$PD, AnxDep=dat.df$AD)

test_that("eq5dcf data.frame gives correct answer", {
  expect_equal(eq5dcf(dat.df, version="3L", ignore.invalid=TRUE), res)
  expect_equal(eq5dcf(dat.df.nam, version="3L", ignore.invalid=TRUE, dimensions=c("Mob","SelCa","UsAct","PDis","AnxDep")), res)
})

test_that("eq5dcf data.frame throws error", {
  expect_error(eq5dcf(dat.df, version="3L", ignore.invalid=FALSE))
  expect_error(eq5dcf(dat, version="3L", ignore.invalid=FALSE, dimensions=c("MO", "SC", "UA", "Pa", "AD")))
})

dat.mat <- as.matrix(dat.df)
test_that("eq5dcf data.frame gives correct answer", {
  expect_equal(eq5dcf(dat.mat, version="3L", ignore.invalid=TRUE), res)
})
