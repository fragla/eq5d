context("EQ-5D")

test_that("eq5d throws error for incorrect parameters", {
  expect_error(eq5d(c(MB=1,SC=2,UA=3,PD=2,AD=1), type="TTO", version="3L", country="UK"))
  expect_error(eq5d(c(MO=1,SC=1,UA=1,PD=1,AD=1),"10L", "TTO"))
  expect_error(eq5d(c(MO=1,SC=1,UA=1,PD=1,AD=1),"5L", type="XXX", country="Ireland"))
})

test.df <- data.frame(MO=c(1,2,3,4,5),SC=c(1,5,4,3,2),UA=c(1,5,2,3,1),PD=c(1,3,4,3,4),AD=c(1,2,1,2,1))

test_that("Wrapper function gives correct answer", {
  expect_equal(eq5d(c(MO=1,SC=2,UA=3,PD=2,AD=1), type="TTO", version="3L", country="UK"), 0.329)
  expect_equal(eq5d(c(MO=3,SC=2,UA=3,PD=2,AD=3), type="TTO", version="3L", country="Germany"), 0.083)
  expect_equal(eq5d(c(MO=1,SC=1,UA=1,PD=1,AD=1), country="Canada", version="5L", type="VT"), 0.949)
  expect_equal(eq5d(c(MO=1,SC=2,UA=3,PD=4,AD=5), country="Indonesia", version="5L", type="VT"), 0.240)
  expect_equal(eq5d(test.df, country="Canada", version="5L", type="VT"), c(0.949,0.362,0.39,0.524,0.431))
  expect_equal(eq5d(test.df, country="England", version="5L", type="VT"), c(1.000,0.393,0.434,0.488,0.400))
  expect_equal(eq5d(c(MO=1,SC=2,UA=3,PD=2,AD=1), type="CW", version="5L", country="Denmark"), 0.736)
})
