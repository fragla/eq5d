context("EQ-5D-3L Reverse Crosswalk using van Hout (2021) mapping")

test_that("van Hout (2021) EQ-5D-5L Crosswalk gives correct answer", {
  expect_equal(eq5drcw(c(MO=3,SC=3,UA=3,PD=3,AD=3), "Germany"), -0.495)
  expect_equal(eq5drcw(c(MO=3,SC=2,UA=1,PD=2,AD=3), "Netherlands"), 0.184)
  expect_equal(eq5drcw(c(MO=1,SC=2,UA=3,PD=2,AD=1), "India"), 0.525)
})

context("EQ-5D-3L Reverse Crosswalk using EuroQol mapping")

test_that("EQ-5D-5L England Crosswalk gives correct answer", {
  expect_equal(eq5drcw(c(MO=1,SC=1,UA=1,PD=1,AD=1), "England", method="EQ"), 0.965)
  expect_equal(eq5drcw(c(MO=2,SC=2,UA=2,PD=2,AD=2), "England", method="EQ"), 0.540)
  expect_equal(eq5drcw(c(MO=3,SC=2,UA=1,PD=2,AD=3), "England", method="EQ"), 0.289)
  expect_equal(eq5drcw(c(MO=3,SC=3,UA=3,PD=3,AD=3), "England", method="EQ"), -0.172)
})

test_that("EQ-5D-5L Germany Crosswalk gives correct answer", {
  expect_equal(eq5drcw(c(MO=1,SC=1,UA=1,PD=1,AD=1), "Germany", method="EQ"), 0.977)
  expect_equal(eq5drcw(c(MO=2,SC=2,UA=2,PD=2,AD=2), "Germany", method="EQ"), 0.620)
  expect_equal(eq5drcw(c(MO=3,SC=2,UA=1,PD=2,AD=3), "Germany", method="EQ"), 0.332)
  expect_equal(eq5drcw(c(MO=3,SC=3,UA=3,PD=3,AD=3), "Germany", method="EQ"), -0.329)
})

test_that("EQ-5D-5L Netherlands Crosswalk gives correct answer", {
  expect_equal(eq5drcw(c(MO=1,SC=1,UA=1,PD=1,AD=1), "Netherlands", method="EQ"), 0.947)
  expect_equal(eq5drcw(c(MO=2,SC=2,UA=2,PD=2,AD=2), "Netherlands", method="EQ"), 0.486)
  expect_equal(eq5drcw(c(MO=3,SC=2,UA=1,PD=2,AD=3), "Netherlands", method="EQ"), 0.215)
  expect_equal(eq5drcw(c(MO=3,SC=3,UA=3,PD=3,AD=3), "Netherlands", method="EQ"), -0.312)
})

test_that("EQ-5D-5L USA Crosswalk gives correct answer", {
  expect_equal(eq5drcw(c(MO=1,SC=1,UA=1,PD=1,AD=1), "USA", method="EQ"), 0.962)
  expect_equal(eq5drcw(c(MO=2,SC=2,UA=2,PD=2,AD=2), "USA", method="EQ"), 0.417)
  expect_equal(eq5drcw(c(MO=3,SC=2,UA=1,PD=2,AD=3), "USA", method="EQ"), 0.176)
  expect_equal(eq5drcw(c(MO=3,SC=3,UA=3,PD=3,AD=3), "USA", method="EQ"), -0.422)
})

context("EQ-5D-3L Reverse Crosswalk Incorrect params")

test_that("EQ-5D-3L crosswalk throws error for incorrect parameters", {
  expect_error(eq5drcw(c(MD=1,SC=2,UA=3,PD=2,AD=1), "Germany"))
  expect_error(eq5drcw(c(MO=1,SC=1,UA=1,PD=1,AD=1), "Swaziland"))
  expect_error(eq5drcw(c(MO=6,SC=1,UA=1,PD=1,AD=1), "Netherlands"))
  expect_error(eq5drcw(c(MO=1,SC=1,UA=1,PD=1,AD=1), "Netherlands", method="EuroQol"))
})
