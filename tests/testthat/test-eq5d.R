context("EQ-5D")

test_that("eq5d throws error for incorrect parameters", {
  expect_error(eq5d(c(MB=1,SC=2,UA=3,PD=2,AD=1), type="TTO", version="3L", country="UK"))
  expect_error(eq5d(c(MO=1,SC=1,UA=1,PD=1,AD=1),version="10L", type="TTO"))
  expect_error(eq5d(c(MO=1,SC=1,UA=1,PD=1,AD=1), version="5L", type="XXX", country="Ireland"))
  # expect_error(eq5d("12345", version="5L", type="VT", country="England"))
  expect_error(eq5d(10000, version="5L", type="VT", country="England"))
  expect_error(eq5d(99999, version="5L", type="VT", country="England"))
})

test.df <- data.frame(MO=c(1,2,3,4,5),SC=c(1,5,4,3,2),UA=c(1,5,2,3,1),PD=c(1,3,4,3,4),AD=c(1,2,1,2,1))
test.df2 <- data.frame(state=c(11111,25532,34241,43332,52141))
test.df3 <- data.frame(Mobility=c(1,2,3,4,5),Care=c(1,5,4,3,2),Activities=c(1,5,2,3,1),Pain=c(1,3,4,3,4),Anxiety=c(1,2,1,2,1))

test.char <- c(12321, 23132, 32123)
test.char2 <- c(11211, "12321", NA, "abcd", "")

test_that("Wrapper function gives correct answer", {
  expect_equal(eq5d(c(MO=1,SC=2,UA=3,PD=2,AD=1), type="TTO", version="3L", country="UK"), 0.329)
  expect_equal(eq5d(c(MO=3,SC=2,UA=3,PD=2,AD=3), type="TTO", version="3L", country="Germany"), 0.083)
  expect_equal(eq5d(c(MO=1,SC=1,UA=1,PD=1,AD=1), country="Canada", version="5L", type="VT"), 0.949)
  expect_equal(eq5d(c(MO=1,SC=2,UA=3,PD=4,AD=5), country="Indonesia", version="5L", type="VT"), 0.240)
  expect_equal(eq5d(test.df, country="Canada", version="5L", type="VT"), c(0.949,0.362,0.39,0.524,0.431))
  expect_equal(eq5d(test.df, country="England", version="5L", type="VT"), c(1.000,0.393,0.434,0.488,0.400))
  expect_equal(eq5d(test.df3, country="England", version="5L", type="VT", dimensions=c("Mobility", "Care", "Activities", "Pain", "Anxiety")), c(1.000,0.393,0.434,0.488,0.400))
  expect_equal(eq5d(test.df2, country="Canada", version="5L", type="VT", five.digit="state"), c(0.949,0.362,0.39,0.524,0.431))
  expect_equal(eq5d(test.df2, country="England", version="5L", type="VT", five.digit="state"), c(1.000,0.393,0.434,0.488,0.400))
  expect_equal(eq5d(as.matrix(test.df), country="Canada", version="5L", type="VT"), c(0.949,0.362,0.39,0.524,0.431))
  expect_equal(eq5d(c(MO=1,SC=2,UA=3,PD=2,AD=1), type="CW", version="5L", country="Denmark"), 0.736)
  expect_equal(eq5d(12345, country="Indonesia", version="5L", type="VT"), 0.240)
  expect_equal(eq5d(test.char, country="UK", version="3L", type="TTO"), c(0.329, -0.090, -0.127))
  expect_error(eq5d(test.df, version="3L", type="TTO", country="USA", dimensions=c("Mob", "Self", "Active", "Pain", "Anx")))
})

test_that("when ignore.invalid flag is TRUE the correct answer is returned", {
  expect_equal(eq5d(c(MO=1,SC=2,UA=3,PD=2,AD=1), type="TTO", version="3L", country="UK", ignore.invalid=TRUE), 0.329)
  expect_equal(eq5d(c(MO=1,SC=2,UA=3,PD=NA,AD=1), type="TTO", version="3L", country="UK", ignore.invalid=TRUE), NA)
  expect_equal(eq5d(12321, type="TTO", version="3L", country="UK", ignore.invalid=TRUE), 0.329)
  expect_equal(eq5d(1232, type="TTO", version="3L", country="UK", ignore.invalid=TRUE), NA)
  expect_equal(eq5d(NA_integer_, type="TTO", version="3L", country="UK", ignore.invalid=TRUE), NA)
  expect_equal(eq5d(test.char2, type="TTO", version="3L", country="UK", ignore.invalid=TRUE), c(0.883, 0.329, NA, NA, NA))
})

test_that("when ignore.invalid flag is FALSE the correct answer is returned", {
  expect_error(eq5d(c(MO=1,SC=2,UA=3,PD=NA,AD=1), type="TTO", version="3L", country="UK", ignore.invalid=FALSE))
  expect_error(eq5d(1232, type="TTO", version="3L", country="UK", ignore.invalid=FALSE))
  expect_error(eq5d(NA_integer_, type="TTO", version="3L", country="UK", ignore.invalid=FALSE))
  expect_error(eq5d(test.char2, type="TTO", version="3L", country="UK", ignore.invalid=FALSE))
})

