context("EQ-5D version mapping")

test_that("EQ-5D-5 to EQ-5D-3L age/sex mapping by dimensions gives correct answer", {
  expect_equal(eq5dmap(c(MO=1,SC=2,UA=3,PD=4,AD=5), "UK", "5L", 30, "female")
, 0.067)
  expect_equal(eq5dmap(c(MO=2,SC=4,UA=1,PD=1,AD=2), "UK", "5L", 50, "female")
, 0.761)
  expect_equal(eq5dmap(c(MO=2,SC=3,UA=4,PD=2,AD=1), "UK", "5L", 5, "male")
, 0.609)
})

test_that("EQ-5D-5L to EQ-5D-3L age/sex mapping by exact index gives correct answer", {
  expect_equal(eq5dmap(0.922, "UK", "5L", 18, "male"), 0.893)
})

test_that("EQ-5D-5L to EQ-5D-3L age/sex mapping by summary index gives correct answer", {
  expect_equal(eq5dmap(0.715, "UK", "5L", 50, "male", bwidth = 0.0001), 0.670)
  expect_equal(eq5dmap(0.435, "UK", "5L", 30, "female", bwidth = 0.0001), 0.302)
  expect_equal(eq5dmap(0.95, "UK", "5L", 70, "male", bwidth = 0.0001), 0.935)
  expect_equal(eq5dmap(0.715, "UK", "5L", 50, "male", bwidth = 0.1), 0.637)
  expect_equal(eq5dmap(0.435, "UK", "5L", 30, "female", bwidth = 0.1), 0.297)
  expect_equal(eq5dmap(0.95, "UK", "5L", 70, "male", bwidth = 0.1), 0.844)

})