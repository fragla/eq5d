context("EQ-5D LSS")

test_that("single lss gives correct answer", {
  expect_equal(lss(c(MO=1,SC=2,UA=3,PD=2,AD=1), version="3L"), 9)
  expect_equal(lss(c(MO=5,SC=5,UA=5,PD=5,AD=5), version="5L"), 25)
  expect_equal(lss(c(MO=5,SC=5,UA=6,PD=5,AD=5), version="5L", ignore.invalid=TRUE), NA)
})

test_that("five digit lss gives correct answer", {
  expect_equal(lss(12321, version="3L"), 9)
  expect_equal(lss(55555, version="5L"), 25)
  expect_equal(lss(c(11111,12345, 55555), version="5L"), c(5,15,25))
})

test.df <- data.frame(MO=c(1,2,3,4,5),SC=c(1,5,4,3,2),UA=c(1,5,2,3,1),PD=c(1,3,4,3,4),AD=c(1,2,1,2,1))

test_that("data.frame lss gives correct answer", {
  expect_equal(lss(test.df, version="5L"), c(5,17,14,15,13))
})

test_that("lss throws error for incorrect parameters", {
  expect_error(lss(12345, version="3L", ignore.invalid=FALSE))
  expect_error(lss(c(MO=1,SC=7,UA=3,PD=2,AD=1), version="3L", ignore.invalid=FALSE))
})
