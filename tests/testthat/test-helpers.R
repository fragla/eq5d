context("EQ-5D helper functions")

test_that("getHealthStates gives correct answer", {
  expect_equal(getHealthStates("3L")[c(4,41,106,158,179)], c("11121","12222","21331","23322","31232"))
  expect_equal(getHealthStates("5L")[c(23,980,1122,2673,3022)], c("11153","23515","24552","52253","55152"))
})

test_that("getHealthStates gives correct answer", {
  expect_error(getHealthStates("10L"))
})
