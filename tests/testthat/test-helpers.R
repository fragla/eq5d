context("EQ-5D helper functions")

test_that("get_all_health_states gives correct answer", {
  expect_equal(get_all_health_states("3L")[c(4,41,106,158,179)], c("11121","12222","21331","23322","31232"))
  expect_equal(get_all_health_states("5L")[c(23,980,1122,2673,3022)], c("11153","23515","24552","52253","55152"))
})

test_that("get_all_health_states gives returns an error", {
  expect_error(get_all_health_states("10L"))
})

test_that("get_dimensions_from_health_states gives correct answer", {
  expect_equal(get_dimensions_from_health_states(c("12345", "54321")), data.frame(MO=c(1,5),SC=c(2,4),UA=c(3,3),PD=c(4,2),AD=c(5,1)))
})

test_that("get_dimensions_from_health_states throws an error", {
  expect_error(get_dimensions_from_health_states(c("12345", "54321"), ignore.invalid=FALSE, version="3L"))
  expect_error(get_dimensions_from_health_states(c("12345", "S432l"), ignore.invalid=FALSE))
})

scores.df <- data.frame(MO=c(1,2,5,2,1), SC=c(2,3,1,3,2), UA=c(3,1,2,NA,3), PD=c(1,1,1,1,1), AD=c(3,3,3,3,9))
test_that("get_health_states_from_dimensions gives the correct answer", {
  expect_equal(get_health_states_from_dimensions(scores.df, version="3L", ignore.invalid=TRUE), c("12313","23113",NA,NA,NA))
  expect_equal(get_health_states_from_dimensions(scores.df, version="5L", ignore.invalid=TRUE), c("12313","23113","51213",NA,NA))
})

test_that("get_health_states_from_dimensions throws an error", {
  expect_error(get_health_states_from_dimensions(scores.df, version="5L", ignore.invalid=FALSE))
  expect_error(get_health_states_from_dimensions(scores.df, version="5L", ignore.invalid=TRUE, dimensions=c("Mob","SC","UA","PD","AD")))
})

