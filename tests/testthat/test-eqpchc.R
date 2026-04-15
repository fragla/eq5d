context("EQ-5D PCHC")

pre <- read.csv("../testdata/pre.csv")$x
post <- read.csv("../testdata/post.csv")$x

res1 <- read.csv("../testdata/pchc_noprob_true_totals_true.csv", row.names=1, colClasses=c("character",rep("numeric",2)))
res2 <- read.csv("../testdata/pchc_noprob_true_totals_false.csv", row.names=1, colClasses=c("character",rep("numeric",2)))
res3 <- read.csv("../testdata/pchc_noprob_false_totals_false.csv", row.names=1, colClasses=c("character",rep("numeric",2)))
res4 <- read.csv("../testdata/pchc_noprob_false_totals_true.csv", row.names=1, colClasses=c("character",rep("numeric",2)))
res5 <- c(NA, "Improve", "Worsen", "Worsen", "Worsen", "Improve")

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
  expect_equal(pchc(pre, post, version="Y3L", no.problems=TRUE, totals=TRUE), res1)
  expect_equal(pchc(pre[1:6], post[1:6], version="3L", no.problems=TRUE, totals=F, summary=F), res5)
})

test_that("eqpchc data.frame throws error", {
  expect_error(pchc(pre[-1,], post, version="3L", ignore.invalid=FALSE))
  expect_error(pchc(pre, post, no.problems=TRUE, totals=TRUE))
})

test_that("eqpchc data.frame throws warning", {
  expect_warning(pchc(pre, post, version="3L", no.problems=TRUE, totals=TRUE, summary=F))
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

test_that("eqpchc using version='Y' is deprecated", {
  rlang::local_options(lifecycle_verbosity = "error")
  expect_error(pchc(pre.df, post.df, version="Y", no.problems=TRUE, totals=TRUE))
})

test_that("eq5dpchc using version='Y' still works", {
  rlang::local_options(lifecycle_verbosity = "quiet")
  expect_equal(pchc(pre.df, post.df, version="Y", no.problems=TRUE, totals=TRUE), res1)
})

#### formula interface tests ####

test_that("pchc.formula(profile) matches wide-form", {
  df <- make_long_profile(pre, post)
  expect_equal(pchc(profile ~ visit | id, data=df, version="3L"), res1)
})

test_that("pchc.formula(dimensions) matches wide-form", {
  df <- make_long_dimensions(pre, post)
  expect_equal(pchc(MO + SC + UA + PD + AD ~ visit | id, data=df, version="3L"), res1)
})

test_that("pchc.formula(cbind) matches wide-form", {
  df <- make_long_dimensions(pre, post)
  expect_equal(pchc(cbind(MO,SC,UA,PD,AD) ~ visit | id, data=df, version="3L"), res1)
})

test_that("pchc.formula supports dimensions= mapping", {
  df <- make_long_dimensions(pre, post)
  names(df)[3:7] <- c("mob","self","use","pain","anx")
  
  expect_equal(
    pchc(
      mob + self + use + pain + anx ~ visit | id, data=df, 
      version="3L", dimensions=c(MO="mob", SC="self", UA="use", PD="pain", AD="anx")
    ),
    res1
  )
})

test_that("pchc.formula errors on missing dimension columns", {
  df <- make_long_dimensions(pre, post)
  df$MO <- NULL
  expect_error(pchc(MO + SC + UA + PD + AD ~ visit | id, data=df, version="3L"))
})

test_that("pchc.formula errors on invalid dimension mapping", {
  df <- make_long_dimensions(pre, post)
  expect_error(
    pchc(
      MO + SC + UA + PD + AD ~ visit | id, data=df, version="3L",
      dimensions=c(MO="wrong", SC="SC", UA="UA", PD="PD", AD="AD"))
  )
})

test_that("pchc.formula errors on >2 time levels", {
  df <- make_long_dimensions(pre, post)
  df$visit[1] <- "middle"  # create 3 levels
  expect_error(pchc(MO + SC + UA + PD + AD ~ visit | id, data=df, version="3L"))
})

test_that("pchc.formula summary=FALSE works", {
  df <- make_long_profile(pre[1:6], post[1:6])
  expect_equal(pchc(profile ~ visit | id, data=df, version="3L", no.problems=TRUE, totals=FALSE, summary=FALSE), res5)
})


test_that("pchc.formula(profile) errors with ignore.invalid=FALSE on bad profile", {
  bad <- pre
  bad[3] <- "99999"   # invalid 5-digit state
  df <- make_long_profile(bad, post)
  
  expect_error(pchc(profile ~ visit | id, data=df, version="3L", ignore.invalid=FALSE))
})

test_that("pchc.formula(dimensions) errors with ignore.invalid=FALSE on bad dimension", {
  df <- make_long_dimensions(pre, post)
  df$MO[5] <- 9   # invalid 3L level
  
  expect_error(pchc(MO + SC + UA + PD + AD ~ visit | id, data=df, version="3L", ignore.invalid=FALSE))
})
