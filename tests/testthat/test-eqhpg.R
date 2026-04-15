context("EQ-5D HPG")

pre <- read.csv("../testdata/pre.csv")$x
post <- read.csv("../testdata/post.csv")$x

pre.df <- read.csv("../testdata/pre_df.csv")
post.df <- read.csv("../testdata/post_df.csv")


res <- read.csv("../testdata/hpg_3l_uk_tto_no_problems_false.csv")

############################################################
# Wide-form tests
############################################################

test_that("eqhpg five digit gives correct answer", {
  expect_equal(hpg(pre, post, country = "UK", version="3L", type="TTO", no.problems = F), res)
})

test_that("eqhpg data.frame gives correct answer", {
  expect_equal(hpg(pre.df, post.df, country = "UK", version="3L", type="TTO", no.problems = F), res)
})

test_that("eqhpg data.frame throws error", {
  expect_error(hpg(pre.df, post, country = "UK", version="3L", type="TTO", ignore.invalid = FALSE))
  expect_error(hpg(pre.df, post.df, country = "UK", version="3L", type="TTO", dimensions=c("M0","SC","UA","PD","AD")))
  expect_error(hpg(pre.df[-1,], post.df, country = "UK", version="3L", type="TTO", ignore.invalid=TRUE))
  expect_error(hpg(pre, post, country = "UK", type="TTO", no.problems = F))
})

test_that("eq5dhpg using version='Y' is deprecated", {
  rlang::local_options(lifecycle_verbosity = "error")
  expect_error(hpg(pre, post, country = "Brazil", version="Y", type="CW", no.problems = F))
})

test_that("eq5d using version='Y' still works", {
  rlang::local_options(lifecycle_verbosity = "quiet")
  expect_equal(as.character(hpg(pre, post, country = "Brazil", version="Y", type="CW", no.problems = F)[2,]), c(158, 60, "Improve"))
})


############################################################
# Formula interface tests
############################################################

test_that("hpg.formula(profile ~ visit | id) matches wide-form", {
  df <- make_long_profile(pre, post)
  out <- hpg(profile ~ visit | id, data=df, country="UK", version="3L", type="TTO", no.problems=FALSE)
  expect_equal(out, res)
})

test_that("hpg.formula(dimensions) matches wide-form", {
  df <- make_long_dimensions(pre, post)
  out <- hpg(MO + SC + UA + PD + AD ~ visit | id, data=df, country="UK", version="3L", type="TTO", no.problems=FALSE)
  expect_equal(out, res)
})

test_that("hpg.formula(cbind) matches wide-form", {
  df <- make_long_dimensions(pre, post)
  out <- hpg(
    cbind(MO,SC,UA,PD,AD) ~ visit | id, data=df,
    country="UK", version="3L", type="TTO", no.problems=FALSE)
  expect_equal(out, res)
})

############################################################
# Formula with dimension mapping
############################################################

test_that("hpg.formula supports dimensions= mapping", {
  df <- make_long_dimensions(pre, post)
  names(df)[3:7] <- c("mob","self","use","pain","anx")
  
  out <- hpg(
    mob + self + use + pain + anx ~ visit | id,
    data=df,
    country="UK",
    version="3L",
    type="TTO",
    no.problems=FALSE,
    dimensions = c(MO="mob", SC="self", UA="use", PD="pain", AD="anx")
  )
  
  expect_equal(out, res)
})

############################################################
# Error conditions (formula)
############################################################

test_that("hpg.formula errors on missing EQ-5D columns", {
  df <- make_long_dimensions(pre, post)
  df$MO <- NULL
  expect_error(hpg(MO + SC + UA + PD + AD ~ visit | id, data=df, country="UK", version="3L", type="TTO"))
})

test_that("hpg.formula errors on invalid dimension mapping", {
  df <- make_long_dimensions(pre, post)
  expect_error(
    hpg(
      MO + SC + UA + PD + AD ~ visit | id, data=df, 
      country="UK", version="3L", type="TTO", dimensions=c(MO="wrong", SC="SC", UA="UA", PD="PD", AD="AD"))
  )
})

test_that("hpg.formula errors on >2 time levels", {
  df <- make_long_dimensions(pre, post)
  df$visit[1] <- "midline"
  expect_error(hpg(MO + SC + UA + PD + AD ~ visit | id, data=df, country="UK", version="3L", type="TTO"))
})

############################################################
# ignore.invalid = FALSE tests (formula)
############################################################

test_that("hpg.formula(profile) errors with ignore.invalid=FALSE", {
  bad <- pre
  bad[5] <- "99999"
  df <- make_long_profile(bad, post)
  expect_error(hpg(profile ~ visit | id, data=df, country="UK", version="3L", type="TTO", ignore.invalid=FALSE))
})

test_that("hpg.formula(dimensions) errors with ignore.invalid=FALSE", {
  df <- make_long_dimensions(pre, post)
  df$AD[3] <- 99
  expect_error(
    hpg(MO + SC + UA + PD + AD ~ visit | id, data=df,
        country="UK", version="3L", type="TTO", ignore.invalid=FALSE)
  )
})

############################################################
# pre.level / post.level override
############################################################

test_that("hpg.formula respects explicit pre.level / post.level", {
  df <- make_long_profile(pre, post)
  
  # Reverse order intentionally
  df$visit <- factor(rep(c("post","pre"), each=length(pre)),
                     levels=c("post","pre"))
  
  out <- hpg(
    profile ~ visit | id,
    data = df,
    country="UK",
    version="3L",
    type="TTO",
    pre.level="pre",
    post.level="post",
    no.problems=FALSE
  )
  
  expect_equal(out$Pre,  res$Post)
  expect_equal(out$Post, res$Pre)
})
