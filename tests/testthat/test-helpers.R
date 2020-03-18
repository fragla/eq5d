context("EQ-5D helper functions")

test_that("getHealthStates gives correct answer", {
  expect_equal(getHealthStates("3L")[c(4,41,106,158,179)], c("11121","12222","21331","23322","31232"))
  expect_equal(getHealthStates("5L")[c(23,980,1122,2673,3022)], c("11153","23515","24552","52253","55152"))
})

test_that("getHealthStates gives returns an error", {
  expect_error(getHealthStates("10L"))
})

test_that("splitHealthStates gives correct answer", {
  expect_equal(splitHealthStates(c("12345", "54321")), data.frame(MO=c(1,5),SC=c(2,4),UA=c(3,3),PD=c(4,2),AD=c(5,1)))
})

test_that("splitHealthStates throws an error", {
  expect_error(splitHealthStates(c("12345", "54321"), ignore.invalid=FALSE, version="3L"))
  expect_error(splitHealthStates(c("12345", "S432l"), ignore.invalid=FALSE))
})