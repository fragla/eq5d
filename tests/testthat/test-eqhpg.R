context("EQ-5D HPG")

pre <- read.csv("../testdata/pre.csv")$x
post <- read.csv("../testdata/post.csv")$x

res <- read.csv("../testdata/hpg_3l_uk_tto_no_problems_false.csv")

test_that("eqhpg five digit gives correct answer", {
  expect_equal(hpg(pre, post, country = "UK", version="3L", type="TTO", no.problems = F), res)
})

test_that("eqhpg data.frame gives correct answer", {
  expect_equal(hpg(getDimensionsFromHealthStates(pre), getDimensionsFromHealthStates(post), country = "UK", version="3L", type="TTO", no.problems = F), res)
})

test_that("eqhpg data.frame throws error", {
  expect_error(hpg(pre, post, country = "UK", version="3L", type="TTO", ignore.invalid = FALSE))
})