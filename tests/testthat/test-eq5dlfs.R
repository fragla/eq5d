context("EQ-5D lfs")

test_that("single lfs gives correct answer", {
  expect_equal(lfs(c(MO=1,SC=2,UA=3,PD=2,AD=1), version="3L"), "221")
  expect_equal(lfs(c(MO=5,SC=5,UA=5,PD=5,AD=5), version="5L"), "00005")
  expect_equal(lfs(c(MO=5,SC=5,UA=6,PD=5,AD=5), version="5L", ignore.invalid=TRUE), NA)
})

test_that("five digit lfs gives correct answer", {
  expect_equal(lfs(12321, version="3L"), "221")
  expect_equal(lfs(55555, version="5L"), "00005")
  expect_equal(lfs(c(11111,12345, 55555), version="5L"), c("50000","11111","00005"))
})

test.df <- data.frame(MO=c(1,2,3,4,5),SC=c(1,5,4,3,2),UA=c(1,5,2,3,1),PD=c(1,3,4,3,4),AD=c(1,2,1,2,1))

test_that("data.frame lfs gives correct answer", {
  expect_equal(lfs(test.df, version="5L"), c("50000","02102","11120","01310","21011"))
})

test_that("lfs throws error for incorrect parameters", {
  expect_error(lfs(12345, version="3L", ignore.invalid=FALSE))
  expect_error(lfs(c(MO=1,SC=7,UA=3,PD=2,AD=1), version="3L", ignore.invalid=FALSE))
})
