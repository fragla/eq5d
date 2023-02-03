context("EQ-5D HPG")

pre <- read.csv("../testdata/pre.csv")$x
post <- read.csv("../testdata/post.csv")$x

pre.df <- read.csv("../testdata/pre_df.csv")
post.df <- read.csv("../testdata/post_df.csv")


res <- read.csv("../testdata/hpg_3l_uk_tto_no_problems_false.csv")

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
