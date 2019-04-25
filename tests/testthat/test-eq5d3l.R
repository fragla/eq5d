context("EQ-5D-3L TTO")

test_that("EQ-5D TTO gives correct answer", {
  expect_equal(eq5d3l(c(MO=1,SC=2,UA=3,PD=2,AD=1), "TTO", "UK"), 0.329)
  expect_equal(eq5d3l(c(MO=3,SC=2,UA=1,PD=2,AD=3), "TTO", "Denmark"), -0.017)
  expect_equal(eq5d3l(c(MO=3,SC=3,UA=3,PD=3,AD=3), "TTO", "Zimbabwe"), -0.145)
  expect_equal(eq5d3l(c(MO=3,SC=2,UA=3,PD=2,AD=3), "TTO", "Germany"), 0.083)
  expect_equal(eq5d3l(c(MO=3,SC=3,UA=3,PD=2,AD=3), "TTO", "USA"), 0.030)
})

context("EQ-5D-3L VAS")

test_that("EQ-5D VAS gives correct answer", {
  expect_equal(eq5d3l(c(MO=3,SC=2,UA=3,PD=1,AD=1), "VAS", "Belgium"), 0.299)
  expect_equal(eq5d3l(c(MO=3,SC=2,UA=1,PD=3,AD=1), "VAS", "Finland"), 0.361)
  expect_equal(eq5d3l(c(MO=3,SC=1,UA=1,PD=3,AD=1), "VAS", "Germany"), 0.170)
  expect_equal(eq5d3l(c(MO=3,SC=1,UA=1,PD=3,AD=3), "VAS", "Slovenia"), 0.052)
  expect_equal(eq5d3l(c(MO=3,SC=1,UA=2,PD=3,AD=3), "VAS", "Europe"), 0.141)
  expect_equal(eq5d3l(c(MO=1,SC=1,UA=1,PD=1,AD=1), "VAS", "UK"), 1)
})

context("EQ-5D-3L Incorrect params")

test_that("EQ-5D-3L throws error for incorrect parameters", {
  expect_error(eq5d(c(MO=1,SC=1,UA=1,PD=1,AD=1),"3L", "XXX"))
  expect_error(eq5d(c(MO=1,SC=1,UA=1,PD=1,AD=1),"10L", "TTO"))
  expect_error(eq5d(c(MO=1,SC=2,UA=3,PD=4,AD=5),"3L", "VAS"))
  expect_error(eq5d(c(MO=2,SC=4,UA=6,PD=8,AD=10),"5L", "VAS"))
  expect_error(eq5d(c(MO=2,SC=4,UA=6,PD=8,AD=10),"5L", "VAS", "Jupiter"))
  expect_error(eq5d(c(MO=1,SC=2,UA=3,PD=4,AD=5),"5L", "VAS", "UK"))
})
