context("EQ-5D helper functions")

test_that("getHealthStates gives correct answer", {
  expect_equal(getHealthStates("3L")[c(4,41,106,158,179)], c("11121","12222","21331","23322","31232"))
  expect_equal(getHealthStates("5L")[c(23,980,1122,2673,3022)], c("11153","23515","24552","52253","55152"))
})

test_that("getHealthStates gives returns an error", {
  expect_error(getHealthStates("10L"))
})

test_that("getDimensionsFromHealthStates gives correct answer", {
  expect_equal(getDimensionsFromHealthStates(c("12345", "54321")), data.frame(MO=c(1,5),SC=c(2,4),UA=c(3,3),PD=c(4,2),AD=c(5,1)))
})

test_that("getDimensionsFromHealthStates throws an error", {
  expect_error(getDimensionsFromHealthStates(c("12345", "54321"), ignore.invalid=FALSE, version="3L"))
  expect_error(getDimensionsFromHealthStates(c("12345", "S432l"), ignore.invalid=FALSE))
})

scores.df <- data.frame(MO=c(1,2,5,2,1), SC=c(2,3,1,3,2), UA=c(3,1,2,NA,3), PD=c(1,1,1,1,1), AD=c(3,3,3,3,9))
test_that("getHealthStatesFromDimensions gives the correct answer", {
  expect_equal(getHealthStatesFromDimensions(scores.df, version="3L", ignore.invalid=TRUE), c("12313","23113",NA,NA,NA))
  expect_equal(getHealthStatesFromDimensions(scores.df, version="5L", ignore.invalid=TRUE), c("12313","23113","51213",NA,NA))
})

test_that("getHealthStatesFromDimensions throws an error", {
  expect_error(getHealthStatesFromDimensions(scores.df, version="5L", ignore.invalid=FALSE))
  expect_error(getHealthStatesFromDimensions(scores.df, version="5L", ignore.invalid=TRUE, dimensions=c("Mob","SC","UA","PD","AD")))
})

