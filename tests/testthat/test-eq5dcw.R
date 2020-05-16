context("EQ-5D-5L Crosswalk")

test_that("EQ-5D-3L Denmark Crosswalk gives correct answer", {
  expect_equal(eq5dcw(c(MO=1,SC=1,UA=1,PD=1,AD=1), "Denmark"), 1.000)
  expect_equal(eq5dcw(c(MO=3,SC=2,UA=1,PD=2,AD=3), "Denmark"), 0.663)
  expect_equal(eq5dcw(c(MO=1,SC=2,UA=1,PD=1,AD=2), "Denmark"), 0.784)
  expect_equal(eq5dcw(c(MO=1,SC=3,UA=1,PD=3,AD=1), "Denmark"), 0.761)
  expect_equal(eq5dcw(c(MO=3,SC=2,UA=4,PD=2,AD=4), "Denmark"), 0.437)
  expect_equal(eq5dcw(c(MO=5,SC=5,UA=5,PD=5,AD=5), "Denmark"), -0.624)
})

test_that("EQ-5D-3L France Crosswalk gives correct answer", {
  expect_equal(eq5dcw(c(MO=1,SC=1,UA=1,PD=1,AD=1), "France"), 1)
  expect_equal(eq5dcw(c(MO=1,SC=1,UA=2,PD=3,AD=3), "France"), 0.673)
  expect_equal(eq5dcw(c(MO=3,SC=3,UA=4,PD=4,AD=4), "France"), 0.009)
  expect_equal(eq5dcw(c(MO=3,SC=4,UA=4,PD=5,AD=5), "France"), -0.205)
  expect_equal(eq5dcw(c(MO=5,SC=5,UA=5,PD=5,AD=5), "France"), -0.53)
})

test_that("EQ-5D-3L Germany Crosswalk gives correct answer", {
  expect_equal(eq5dcw(c(MO=1,SC=1,UA=1,PD=1,AD=1), "Germany"), 1)
  expect_equal(eq5dcw(c(MO=1,SC=2,UA=2,PD=1,AD=1), "Germany"), 0.927)
  expect_equal(eq5dcw(c(MO=4,SC=4,UA=3,PD=1,AD=3), "Germany"), 0.679)
  expect_equal(eq5dcw(c(MO=4,SC=4,UA=4,PD=4,AD=4), "Germany"), 0.274)
  expect_equal(eq5dcw(c(MO=5,SC=5,UA=5,PD=5,AD=5), "Germany"), -0.205)
})

test_that("EQ-5D-3L Japan Crosswalk gives correct answer", {
  expect_equal(eq5dcw(c(MO=1,SC=1,UA=1,PD=1,AD=1), "Japan"), 1)
  expect_equal(eq5dcw(c(MO=2,SC=1,UA=3,PD=3,AD=1), "Japan"), 0.662)
  expect_equal(eq5dcw(c(MO=3,SC=3,UA=3,PD=3,AD=3), "Japan"), 0.532)
  expect_equal(eq5dcw(c(MO=4,SC=1,UA=4,PD=3,AD=3), "Japan"), 0.532)
  expect_equal(eq5dcw(c(MO=5,SC=5,UA=5,PD=5,AD=5), "Japan"), -0.111)
})

test_that("EQ-5D-3L Netherlands Crosswalk gives correct answer", {
  expect_equal(eq5dcw(c(MO=1,SC=1,UA=1,PD=1,AD=1), "Netherlands"), 1)
  expect_equal(eq5dcw(c(MO=2,SC=1,UA=2,PD=1,AD=3), "Netherlands"), 0.750)
  expect_equal(eq5dcw(c(MO=3,SC=3,UA=4,PD=3,AD=3), "Netherlands"), 0.484)
  expect_equal(eq5dcw(c(MO=4,SC=5,UA=4,PD=5,AD=4), "Netherlands"), -0.094)
  expect_equal(eq5dcw(c(MO=5,SC=5,UA=5,PD=5,AD=5), "Netherlands"), -0.329)
})

test_that("EQ-5D-3L Spain Crosswalk gives correct answer", {
  expect_equal(eq5dcw(c(MO=1,SC=1,UA=1,PD=1,AD=1), "Spain"), 1)
  expect_equal(eq5dcw(c(MO=1,SC=1,UA=1,PD=2,AD=2), "Spain"), 0.857)
  expect_equal(eq5dcw(c(MO=3,SC=3,UA=5,PD=5,AD=3), "Spain"), -0.073)
  expect_equal(eq5dcw(c(MO=4,SC=3,UA=5,PD=5,AD=5), "Spain"), -0.178)
  expect_equal(eq5dcw(c(MO=5,SC=5,UA=5,PD=5,AD=5), "Spain"), -0.654)
})

test_that("EQ-5D-5L Thailand Crosswalk gives correct answer", {
  expect_equal(eq5dcw(c(MO=1,SC=1,UA=1,PD=1,AD=1), "Thailand"), 1)
  expect_equal(eq5dcw(c(MO=1,SC=1,UA=1,PD=1,AD=5), "Thailand"), 0.549)
  expect_equal(eq5dcw(c(MO=3,SC=4,UA=2,PD=4,AD=3), "Thailand"), 0.245)
  expect_equal(eq5dcw(c(MO=5,SC=5,UA=2,PD=5,AD=5), "Thailand"), -0.381)
  expect_equal(eq5dcw(c(MO=5,SC=5,UA=5,PD=5,AD=5), "Thailand"), -0.452)
})

test_that("EQ-5D-3L UK Crosswalk gives correct answer", {
  expect_equal(eq5dcw(c(MO=1,SC=1,UA=1,PD=1,AD=1), "UK"), 1)
  expect_equal(eq5dcw(c(MO=1,SC=1,UA=2,PD=2,AD=1), "UK"), 0.795)
  expect_equal(eq5dcw(c(MO=4,SC=4,UA=4,PD=4,AD=1), "UK"), 0.226)
  expect_equal(eq5dcw(c(MO=4,SC=4,UA=5,PD=2,AD=5), "UK"), 0.004)
  expect_equal(eq5dcw(c(MO=5,SC=5,UA=5,PD=5,AD=5), "UK"), -0.594)
})

test_that("EQ-5D-3L USA Crosswalk gives correct answer", {
  expect_equal(eq5dcw(c(MO=1,SC=1,UA=1,PD=1,AD=1), "USA"), 1)
  expect_equal(eq5dcw(c(MO=1,SC=2,UA=2,PD=2,AD=1), "USA"), 0.778)
  expect_equal(eq5dcw(c(MO=5,SC=2,UA=3,PD=4,AD=2), "USA"), 0.216)
  expect_equal(eq5dcw(c(MO=5,SC=5,UA=5,PD=1,AD=3), "USA"), 0.162)
  expect_equal(eq5dcw(c(MO=5,SC=5,UA=5,PD=5,AD=5), "USA"), -0.109)
})

test_that("EQ-5D-3L Zimbabwe Crosswalk gives correct answer", {
  expect_equal(eq5dcw(c(MO=1,SC=1,UA=1,PD=1,AD=1), "Zimbabwe"), 0.9)
  expect_equal(eq5dcw(c(MO=2,SC=2,UA=2,PD=2,AD=2), "Zimbabwe"), 0.653)
  expect_equal(eq5dcw(c(MO=2,SC=3,UA=3,PD=3,AD=4), "Zimbabwe"), 0.544)
  expect_equal(eq5dcw(c(MO=4,SC=5,UA=4,PD=5,AD=4), "Zimbabwe"), 0.119)
  expect_equal(eq5dcw(c(MO=5,SC=5,UA=5,PD=5,AD=5), "Zimbabwe"), -0.145)
})

context("EQ-5D-5L Crosswalk Incorrect params")

test_that("EQ-5D-5L crosswalk throws error for incorrect parameters", {
  expect_error(eq5dcw(c(MD=5,SC=5,UA=5,PD=5,AD=5), "UK"))
  expect_error(eq5dcw(c(MO=1,SC=2,UA=3,PD=4,AD=6), "UK"))
  expect_error(eq5dcw(c(MO=1,SC=2,UA=3,PD=4,AD=5), "Swaziland"))
  expect_error(eq5dcw(c(3, 4, 5, 4, 3), "UK"))
})