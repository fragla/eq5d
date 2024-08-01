context("EQ-5D version mapping")

test_that("EQ-5D-5L to EQ-5D-3L age/sex mapping by dimensions gives correct answer", {
  expect_equal(eq5dmap(c(MO=1,SC=2,UA=3,PD=4,AD=5), "UK", "5L", 30, "female"), 0.067)
  expect_equal(eq5dmap(c(MO=2,SC=4,UA=1,PD=1,AD=2), "UK", "5L", 50, "female"), 0.761)
  expect_equal(eq5dmap(c(MO=2,SC=3,UA=4,PD=2,AD=1), "UK", "5L", 5, "male"), 0.609)
  expect_equal(eq5dmap(c(MO=2,SC=3,UA=4,PD=2,AD=1), "UK", "5L", 5, "male", digits = 8), 0.60885463)
})

test_that("EQ-5D-3L to EQ-5D-5L age/sex mapping by dimensions gives correct answer", {
  expect_equal(eq5dmap(c(MO=1,SC=2,UA=3,PD=2,AD=1), "UK", "3L", 30, "female"), 0.612)
  expect_equal(eq5dmap(c(MO=2,SC=2,UA=1,PD=1,AD=2), "UK", "3L", 50, "female"), 0.712)
  expect_equal(eq5dmap(c(MO=3,SC=3,UA=3,PD=3,AD=3), "UK", "3L", 3, "male"), -0.215)
  expect_equal(eq5dmap(c(MO=3,SC=3,UA=3,PD=3,AD=3), "UK", "3L", 3, "male", digits = 8), -0.21529993)
})

test_that("EQ-5D-5L to EQ-5D-3L age/sex mapping by exact index gives correct answer", {
  expect_equal(eq5dmap(0.922, "UK", "5L", 18, "male"), 0.893)
  expect_equal(eq5dmap(0.922, "UK", "5L", 18, "male", digits = 8), 0.89307115)
})

test_that("EQ-5D-3L to EQ-5D-5L age/sex mapping by exact index gives correct answer", {
  expect_equal(eq5dmap(-0.594, "UK", "3L", 95, "female"), -0.209)
  expect_equal(eq5dmap(-0.594, "UK", "3L", 95, "female", digits = 8), -0.20907130)
})

test_that("EQ-5D-5L to EQ-5D-3L age/sex mapping by summary index gives correct answer", {
  expect_equal(eq5dmap(0.715, "UK", "5L", 50, "male", bwidth = 0.0001), 0.670)
  expect_equal(eq5dmap(0.435, "UK", "5L", 30, "female", bwidth = 0.0001), 0.302)
  expect_equal(eq5dmap(0.95, "UK", "5L", 70, "male", bwidth = 0.0001), 0.935)
  expect_equal(eq5dmap(0.715, "UK", "5L", 50, "male", bwidth = 0.1), 0.637)
  expect_equal(eq5dmap(0.435, "UK", "5L", 30, "female", bwidth = 0.1), 0.297)
  expect_equal(eq5dmap(0.95, "UK", "5L", 70, "male", bwidth = 0.1), 0.844)
})

test_that("EQ-5D-3L to EQ-5D-5L age/sex mapping by summary index gives correct answer", {
  expect_equal(eq5dmap(-0.002, "UK", "3L", 50, "male", bwidth = 0.0001), NaN)
  expect_equal(eq5dmap(0.234, "UK", "3L", 30, "female", bwidth = 0.0001), NaN)
  expect_equal(eq5dmap(0.100, "UK", "3L", 70, "male", bwidth = 0.0001), 0.510)
  expect_equal(eq5dmap(-0.002, "UK", "3L", 50, "male", bwidth = 0.1), 0.312)
  expect_equal(eq5dmap(0.234, "UK", "3L", 30, "female", bwidth = 0.1), 0.579)
  expect_equal(eq5dmap(0.100, "UK", "3L", 70, "male", bwidth = 0.1), 0.404)
})

context("eq5dmap Incorrect params")

test_that("eq5dmap throws error for incorrect parameters", {
  expect_error(eq5dmap(c(MO=1,SC=2,UA=3,PD=2,AD=6), version="5L", country="UK", age=23, sex="male"))
  expect_error(eq5dmap(c(MO=1,SC=2,UA=3,PD=2,AD=6), version="3L", country="UK", age=23, sex="male"))
  expect_error(eq5dmap(c(MO=1,SC=2,UA=3,PD=2,AD=5), version="5L", country="Jupiter", age=23, sex="male"))
  expect_error(eq5dmap(c(MO=1,SC=2,UA=3,PD=2,AD=5), version="5L", country="Germany", age=110, sex="f"))
  expect_error(eq5dmap(c(MO=1,SC=2,UA=3,PD=2,AD=5), version="5L", country="Germany", age=37, sex="None"))
  expect_error(eq5dmap(0.95, "UK", "10L", 70, "male", bwidth = 0.0001))
  expect_error(eq5dmap(1.95, "China", "5L", 70, "male", bwidth = 0.0001))
  expect_error(eq5dmap(0.713, version="5L", country="Germany", age=29, sex="male", bwidth=-0.00001))
  expect_error(eq5dmap(c(MO=1,SC=2,UA=3,PD=4,AD=NA), "Germany", "5L", 50, "female"))
  expect_error(eq5dmap(0.923, "UK", "5L", 50, "male", bwidth=0))
  expect_error(eq5dmap(2, "UK", "5L", 50, "male", bwidth=0))
  expect_error(eq5dmap(2, "Japan", "3L", 50, "female", bwidth=0))
  expect_error(eq5dmap(0.100, "UK", "5L", "Twenty", "female", bwidth=0.1))
  expect_error(eq5dmap("Five", "UK", "5L", "20", "female", bwidth=0.1))
})
