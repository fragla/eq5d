context("EQ-5D Probability of Superiority")

pre <- read.csv("../testdata/pre.csv")$x
post <- read.csv("../testdata/post.csv")$x

test_that("eqps five digit gives correct answer", {
  expect_equal(ps(pre, post, version="3L", ignore.invalid=TRUE)$MO, 0.59)
  expect_equal(ps(pre, post, version="3L", ignore.invalid=TRUE)$SC, 0.62)
  expect_equal(ps(pre, post, version="3L", ignore.invalid=TRUE)$UA, 0.61)
  expect_equal(ps(pre, post, version="3L", ignore.invalid=TRUE)$PD, 0.66)
  expect_equal(ps(pre, post, version="3L", ignore.invalid=TRUE)$AD, 0.56)  
})

test_that("eqps throws error", {
  expect_error(ps(pre, post, version="3L", ignore.invalid=F))
  expect_error(ps(pre[-1,], post, version="3L", ignore.invalid=TRUE))
  expect_error(ps(pre, post, ignore.invalid=TRUE))
})

pre.df <- read.csv("../testdata/pre_df.csv")
post.df <- read.csv("../testdata/post_df.csv")

test_that("eqps data.frame gives correct answer", {
  expect_equal(ps(pre, post, version="3L", ignore.invalid=TRUE)$MO, 0.59)
  expect_equal(ps(pre, post, version="3L", ignore.invalid=TRUE)$SC, 0.62)
  expect_equal(ps(pre, post, version="3L", ignore.invalid=TRUE)$UA, 0.61)
  expect_equal(ps(pre, post, version="3L", ignore.invalid=TRUE)$PD, 0.66)
  expect_equal(ps(pre, post, version="3L", ignore.invalid=TRUE)$AD, 0.56)  
})

test_that("eqps data.frame throws error", {
  expect_error(ps(pre.df, post.df, version="3L", dimensions=c("M0","SC","UA","PD","AD")))
  expect_error(ps(pre.df, post.df, version="3L", ignore.invalid=FALSE))
  expect_error(ps(pre.df[-1,], post.df, version="3L", ignore.invalid=TRUE))
})


#### formula interface tests ####

test_that("ps.formula(profile) matches wide-form", {
  df <- make_long_profile(pre, post)
  expect_equal(
    ps(profile ~ visit | id, data=df, version="3L"),
    ps(pre, post, version="3L")
  )
})

test_that("ps.formula(dimensions) matches wide-form", {
  df <- make_long_dimensions(pre, post)
  expect_equal(
    ps(MO + SC + UA + PD + AD ~ visit | id, data=df, version="3L"),
    ps(pre, post, version="3L")
  )
})

test_that("ps.formula(cbind) matches wide-form", {
  df <- make_long_dimensions(pre, post)
  expect_equal(
    ps(cbind(MO,SC,UA,PD,AD) ~ visit | id, data=df, version="3L"),
    ps(pre, post, version="3L")
  )
})

test_that("ps.formula dimension mapping works", {
  df <- make_long_dimensions(pre, post)
  names(df)[3:7] <- c("mob","self","use","pain","anx")
  
  expect_equal(
    ps(
      mob + self + use + pain + anx ~ visit | id,
      data=df,
      version="3L",
      dimensions=c(MO="mob", SC="self", UA="use", PD="pain", AD="anx")
    ),
    ps(pre, post, version="3L")
  )
})

test_that("ps.formula errors on missing columns", {
  df <- make_long_dimensions(pre, post)
  df$MO <- NULL
  expect_error(
    ps(MO + SC + UA + PD + AD ~ visit | id, data=df, version="3L")
  )
})

test_that("ps.formula errors on invalid mapping", {
  df <- make_long_dimensions(pre, post)
  expect_error(
    ps(MO + SC + UA + PD + AD ~ visit | id, data=df,
       version="3L",
       dimensions=c(MO="wrong", SC="SC", UA="UA", PD="PD", AD="AD"))
  )
})


test_that("ps.formula(profile) errors with ignore.invalid=FALSE on bad profile", {
  bad <- pre
  bad[4] <- "99999"
  df <- make_long_profile(bad, post)
  
  expect_error(
    ps(profile ~ visit | id, data=df, version="3L", ignore.invalid=FALSE)
  )
})

test_that("ps.formula(dimensions) errors with ignore.invalid=FALSE on bad dimension", {
  df <- make_long_dimensions(pre, post)
  df$AD[7] <- 99   # invalid 3L level
  
  expect_error(
    ps(MO + SC + UA + PD + AD ~ visit | id,
       data=df, version="3L", ignore.invalid=FALSE)
  )
})


