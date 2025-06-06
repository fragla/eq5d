context("EQ-5D")

test.df <- data.frame(MO=c(1,2,3,4,5),SC=c(1,5,4,3,2),UA=c(1,5,2,3,1),PD=c(1,3,4,3,4),AD=c(1,2,1,2,1))
test.df2 <- data.frame(state=c(11111,25532,34241,43332,52141))
test.df3 <- data.frame(Mobility=c(1,2,3,4,5),Care=c(1,5,4,3,2),Activities=c(1,5,2,3,1),Pain=c(1,3,4,3,4),Anxiety=c(1,2,1,2,1))
test.df4 <- data.frame(MO=c(1,2,2),SC=c(2,4,3),UA=c(3,1,4),PD=c(4,1,2),AD=c(5,2,1),age=c(30,50,5),sex=c("f","f","m"))
test.df5 <- data.frame(Utility=c(0.922,0.590,-0.226,0.435), Age=c(18,36,71,30), Sex=c("m","f","f","f"), bandwidth=c(0,0,0,0.1))
test.df6 <- data.frame(Utility=c(-0.226,0.922), Age=c(18,30), Sex=c("male","female"), bwidth=c(0,""))
test.df7 <- data.frame(Age = c(50,30,70), Sex = c("m","f","m"), Utility = c(0.715,0.435,0.95))
test.char <- c(12321, 23132, 32123)
test.char2 <- c(11211, "12321", NA, "abcd", "")

test_that("Wrapper function gives correct answer", {
  expect_equal(eq5d(c(MO=1,SC=2,UA=3,PD=2,AD=1), type="TTO", version="3L", country="UK"), 0.329)
  expect_equal(eq5d(c(MO=3,SC=2,UA=3,PD=2,AD=3), type="TTO", version="3L", country="Germany"), 0.085)
  expect_equal(eq5d(c(MO=1,SC=1,UA=1,PD=1,AD=1), country="Canada", version="5L", type="VT"), 0.949)
  expect_equal(eq5d(c(MO=1,SC=2,UA=3,PD=4,AD=5), country="Indonesia", version="5L", type="VT"), 0.240)
  expect_equal(eq5d(c(MO=3,SC=3,UA=3,PD=3,AD=3), country="Slovenia", version="Y3L"), -0.691)
  expect_equal(eq5d(test.df, country="Canada", version="5L", type="VT"), c(0.949,0.362,0.39,0.524,0.431))
  expect_equal(eq5d(test.df, country="England", version="5L", type="VT"), c(1.000,0.393,0.434,0.488,0.400))
  expect_equal(eq5d(test.df3, country="England", version="5L", type="VT", dimensions=c("Mobility", "Care", "Activities", "Pain", "Anxiety")), c(1.000,0.393,0.434,0.488,0.400))
  expect_equal(eq5d(test.df2, country="Canada", version="5L", type="VT", five.digit="state"), c(0.949,0.362,0.39,0.524,0.431))
  expect_equal(eq5d(test.df2, country="England", version="5L", type="VT", five.digit="state"), c(1.000,0.393,0.434,0.488,0.400))
  expect_equal(eq5d(as.matrix(test.df), country="Canada", version="5L", type="VT"), c(0.949,0.362,0.39,0.524,0.431))
  expect_equal(eq5d(c(MO=1,SC=2,UA=3,PD=2,AD=1), type="CW", version="5L", country="Denmark"), 0.736)
  expect_equal(eq5d(c(MO=3,SC=3,UA=3,PD=3,AD=3), type="RCW", version="3L", country="Netherlands", method="EQ"), -0.312)
  expect_equal(eq5d(c(MO=3,SC=3,UA=3,PD=3,AD=3), type="RCW", version="3L", country="Germany"), -0.495)
  expect_equal(eq5d(12345, country="Indonesia", version="5L", type="VT"), 0.240)
  expect_equal(eq5d(33333, country="Slovenia", version="Y3L"), -0.691)
  expect_equal(eq5d(test.char, country="UK", version="3L", type="TTO"), c(0.329, -0.090, -0.127))
  expect_equal(eq5d(c(MO=1,SC=2,UA=3,PD=4,AD=5), country="UK", version="5L", type="DSU", age=30, sex="female"), 0.067)
  expect_equal(eq5d(c(MO=2,SC=4,UA=1,PD=1,AD=2), country="UK", version="5L", type="DSU", age=50, sex="female"), 0.761)
  expect_equal(eq5d(c(MO=2,SC=3,UA=4,PD=2,AD=1), country="UK", version="5L", type="DSU", age=5, sex="male"), 0.609)
  expect_equal(eq5d(c(MO=1,SC=2,UA=3,PD=2,AD=1), country="UK", version="3L", type="DSU", age=30, sex="female"), 0.612)
  expect_equal(eq5d(c(MO=2,SC=2,UA=1,PD=1,AD=2), country="UK", version="3L", type="DSU", age=50, sex="female"), 0.712)
  expect_equal(eq5d(c(MO=3,SC=3,UA=3,PD=3,AD=3), country="UK", version="3L", type="DSU", age=3, sex="male"), -0.215)
  expect_equal(eq5d(c(MO=3,SC=3,UA=3,PD=3,AD=3), country="UK", version="3L", type="DSU", age=3, sex="male", digits = 8), -0.21529993)
  expect_equal(eq5d(0.922, country="UK", version="5L", type="DSU", age=18, sex="male"), 0.893)
  expect_equal(eq5d(0.435, country="UK", version="5L", type="DSU", age=30, sex="female", bwidth = 0.0001), 0.302)
  expect_equal(eq5d(0.922, country="UK", version="5L", type="DSU", age=18, sex="male"), 0.893)
  expect_equal(eq5d(-0.594, country="UK", version="3L", type="DSU", age=95, sex="female"), -0.209)
  expect_equal(eq5d(-0.002, country="UK", version="3L", type="DSU", age=50, sex="male", bwidth=0.1), 0.312)
  expect_equal(eq5d(0.715, country="UK", version="5L", type="DSU", age=50, sex="male", bwidth = 0.0001), 0.670)
  expect_equal(eq5d(0.715, country="UK", version="5L", type="DSU", age=50, sex="male", bwidth = 0.0001, digits = 7), 0.6701727)
  expect_equal(eq5d(0.715, country="UK", version="5L", type="DSU", age=50, sex="male", bwidth = 0.0001, ignore.invalid = TRUE, digits = "two"), NA)
  expect_equal(eq5d(0.435, country="UK", version="5L", type="DSU", age=30, sex="female", bwidth = 0.0001), 0.302)
  expect_equal(eq5d(0.95, country="UK", version="5L", type="DSU", age=70, sex="male", bwidth = 0.0001), 0.935)
  expect_equal(eq5d(0.715, country="UK", version="5L", type="DSU", age=50, sex="male", bwidth = 0.1), 0.637)
  expect_equal(eq5d(0.435, country="UK", version="5L", type="DSU", age=30, sex="female", bwidth = 0.1), 0.297)
  expect_equal(eq5d(0.95, country="UK", version="5L", type="DSU", age=70, sex="male", bwidth = 0.1), 0.844)
  expect_equal(eq5d(test.df4,version="5L", type="DSU", country="UK", age="age", sex="sex"), c(0.067,0.761,0.609))
  expect_equal(eq5d(test.df5,version="5L", type="DSU", country="UK", bwidth="bandwidth", utility="Utility"), c(0.893,0.353,-0.409,0.297))
  expect_equal(eq5d(test.df6,version="5L", type="DSU", country="UK", ignore.invalid = TRUE), c(-0.427,NA))
  expect_equal(eq5d(test.df7,version="5L", type="DSU", country="UK", bwidth=0.1), c(0.637,0.297,0.844))
  expect_equal(eq5d(test.df7,version="5L", type="DSU", country="UK", bwidth=0.0001), c(0.670,0.302,0.935))
  expect_equal(eq5d(0.715, country="UK", type="DSU", version="5L", age=50, sex="m", bwidth=0.1), 0.637)
  expect_equal(eq5d(0.715, country="UK", type="DSU", version="5L", age=50, sex="m", bwidth=0.0001), 0.670)
  expect_error(eq5d(test.df, version="3L", type="TTO", country="USA", dimensions=c("Mob", "Self", "Active", "Pain", "Anx")))
})

test_that("eq5d throws error for incorrect parameters", {
  expect_error(eq5d(c(MB=1,SC=2,UA=3,PD=2,AD=1), type="TTO", version="3L", country="UK"))
  expect_error(eq5d(c(MO=1,SC=1,UA=1,PD=1,AD=1),version="10L", type="TTO"))
  expect_error(eq5d(c(MO=1,SC=1,UA=1,PD=1,AD=1), version="5L", type="XXX", country="Ireland"))
  expect_error(eq5d("12345", version="10L", type="VT", country="England"))
  expect_error(eq5d(10000, version="5L", type="VT", country="England"))
  expect_error(eq5d(99999, version="5L", type="VT", country="England"))
  expect_error(eq5d(0.375, country="UK", version="5L", type="DSU", age=50, sex="male", bwidth=-0.00001))
  expect_error(eq5d(test.df7,version="5L", type="DSU", country="UK", bwidth=c(0.0001, 0.1)))
})

test_that("when ignore.invalid flag is TRUE the correct answer is returned", {
  expect_equal(eq5d(c(MO=1,SC=2,UA=3,PD=2,AD=1), type="TTO", version="3L", country="UK", ignore.invalid=TRUE), 0.329)
  expect_equal(eq5d(c(MO=1,SC=2,UA=3,PD=NA,AD=1), type="TTO", version="3L", country="UK", ignore.invalid=TRUE), NA)
  expect_equal(eq5d(12321, type="TTO", version="3L", country="UK", ignore.invalid=TRUE), 0.329)
  expect_equal(eq5d(1232, type="TTO", version="3L", country="UK", ignore.invalid=TRUE), NA)
  expect_equal(eq5d(NA_integer_, type="TTO", version="3L", country="UK", ignore.invalid=TRUE), NA)
  expect_equal(eq5d(test.char2, type="TTO", version="3L", country="UK", ignore.invalid=TRUE), c(0.883, 0.329, NA, NA, NA))
  expect_equal(eq5d(0.923, country="UK", version="5L", type="DSU", age=50, sex="male", bwidth=0, ignore.invalid=TRUE), NA)
  expect_error(eq5d(c(MO=1,SC=2,UA=3,PD=2,AD=6), version="5L", type="DSU", country="UK", age=23, sex="male", ignore.invalid=TRUE), NA)
  expect_error(eq5d(c(MO=1,SC=2,UA=3,PD=2,AD=6), version="3L", type="DSU", country="UK", age=23, sex="male", ignore.invalid=TRUE), NA)
  expect_error(eq5d(c(MO=1,SC=2,UA=3,PD=2,AD=5), version="5L", type="DSU", country="Germany", age=110, sex="female", ignore.invalid=TRUE), NA)
  expect_error(eq5d(c(MO=1,SC=2,UA=3,PD=2,AD=5), version="5L", type="DSU", country="Germany", age=60, sex="None", ignore.invalid=TRUE), NA)
  expect_error(eq5d(c(MO=1,SC=2,UA=3,PD=4,AD=NA), country="Germany", type="DSU", version="5L", age=50, "female", ignore.invalid=TRUE), NA)
})

test_that("when ignore.invalid flag is FALSE the correct answer is returned", {
  expect_error(eq5d(c(MO=1,SC=2,UA=3,PD=NA,AD=1), type="TTO", version="3L", country="UK", ignore.invalid=FALSE))
  expect_error(eq5d(1232, type="TTO", version="3L", country="UK", ignore.invalid=FALSE))
  expect_error(eq5d(NA_integer_, type="TTO", version="3L", country="UK", ignore.invalid=FALSE))
  expect_error(eq5d(test.char2, type="TTO", version="3L", country="UK", ignore.invalid=FALSE))
  expect_error(eq5d(3.5, country="UK", version="5L", type="DSU", age=50, sex="male", bwidth=0.00001))
  expect_error(eq5d(c(MO=1,SC=2,UA=3,PD=2,AD=5), version="5L", type="DSU", country="Germany", age=37, sex="None"))
  expect_error(eq5d(c(MO=1,SC=2,UA=3,PD=2,AD=5), version="5L", type="DSU", country="Japan", age=205, sex="m"))
  expect_error(eq5d(c(MO=1,SC=2,UA=3,PD=2,AD=1), version="3L", type="ABC", country="Germany"))
  expect_error(eq5d(0.923, country="UK", version="5L", type="DSU", age=50, sex="male", bwidth=0))
  expect_error(eq5d(0.715, country="UK", version="5L", type="DSU", age=50, sex="male", bwidth = 0.0001, digits = "two"))
  
})

test_that("eq5d using version='Y' is deprecated", {
  rlang::local_options(lifecycle_verbosity = "error")
  expect_error(eq5d(33333, country="Slovenia", version="Y"))
})

test_that("eq5d using version='Y' still works", {
  rlang::local_options(lifecycle_verbosity = "quiet")
  expect_equal(eq5d(33333, country="Slovenia", version="Y"), -0.691)
})

context("Value sets")

test_that("valuesets function gives correct answer", {
  expect_equal(valuesets(country="Ghana")$DOI, c("10.1016/j.vhri.2024.101045", "10.1016/j.jval.2021.03.009"))
  expect_equal(valuesets(country = "Belgium", version = "Y3L")$DOI, "10.1007/s40273-022-01187-x")
  expect_equal(as.character(valuesets(country = "Bermuda", version="3L", type="TTO", references=NULL)[1,]), c("EQ-5D-3L","TTO","Bermuda"))
})

test_that("valuesets function gives throws error", {
  expect_error(valuesets(country="UK", references = "Medline"))
})

test_that("eq5d using version='Y' is deprecated", {
  rlang::local_options(lifecycle_verbosity = "error")
  expect_error(valuesets(country = "Belgium", version = "Y"))
})

test_that("eq5d using version='Y' still works", {
  rlang::local_options(lifecycle_verbosity = "quiet")
  expect_equal(valuesets(country = "Belgium", version = "Y")$DOI, "10.1007/s40273-022-01187-x")
})
  
