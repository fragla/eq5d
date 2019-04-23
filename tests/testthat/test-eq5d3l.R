context("EQ-5D-3L TTO")

test_that("EQ-5D TTO gives correct answer", {
  expect_equal(eq5d3l(c(Mobility=1,Care=2,Activity=3,Pain=2,Anxiety=1), "TTO", "UK"), 0.329)
  expect_equal(eq5d3l(c(Mobility=3,Care=2,Activity=1,Pain=2,Anxiety=3), "TTO", "Denmark"), -0.017)
  expect_equal(eq5d3l(c(Mobility=3,Care=3,Activity=3,Pain=3,Anxiety=3), "TTO", "Zimbabwe"), -0.145)
  expect_equal(eq5d3l(c(Mobility=3,Care=2,Activity=3,Pain=2,Anxiety=3), "TTO", "Germany"), 0.083)
  expect_equal(eq5d3l(c(Mobility=3,Care=3,Activity=3,Pain=2,Anxiety=3), "TTO", "USA"), 0.030)
})

context("EQ-5D-3L VAS")

test_that("EQ-5D VAS gives correct answer", {
  expect_equal(eq5d3l(c(Mobility=3,Care=2,Activity=3,Pain=1,Anxiety=1), "VAS", "Belgium"), 0.299)
  expect_equal(eq5d3l(c(Mobility=3,Care=2,Activity=1,Pain=3,Anxiety=1), "VAS", "Finland"), 0.361)
  expect_equal(eq5d3l(c(Mobility=3,Care=1,Activity=1,Pain=3,Anxiety=1), "VAS", "Germany"), 0.170)
  expect_equal(eq5d3l(c(Mobility=3,Care=1,Activity=1,Pain=3,Anxiety=3), "VAS", "Slovenia"), 0.052)
  expect_equal(eq5d3l(c(Mobility=3,Care=1,Activity=2,Pain=3,Anxiety=3), "VAS", "Europe"), 0.141)
  expect_equal(eq5d3l(c(Mobility=1,Care=1,Activity=1,Pain=1,Anxiety=1), "VAS", "UK"), 1)
})

context("EQ-5D-3L Incorrect params")

test_that("EQ-5D-3L throws error for incorrect parameters", {
  expect_error(eq5d(1,1,1,1,1,"3L", "XXX"))
  expect_error(eq5d(1,1,1,1,1,"10L", "TTO"))
  expect_error(eq5d(1,2,3,4,5,"3L", "VAS"))
  expect_error(eq5d(2,4,6,8,10,"5L", "VAS"))
  expect_error(eq5d(2,4,6,8,10,"5L", "VAS", "Jupiter"))
  expect_error(eq5d(1,2,3,4,5,"5L", "VAS", "UK"))
})
