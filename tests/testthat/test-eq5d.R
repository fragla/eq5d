context("EQ-5D")

test_that("eq5d throws error for incorrect parameters", {
  expect_error(eq5d(1,1,1,1,1,"3L", "XXX"))
  expect_error(eq5d(1,1,1,1,1,"10L", "TTO"))
  expect_error(eq5d(1,2,3,4,5,"3L", "VAS"))
  expect_error(eq5d(2,4,6,8,10,"5L", "VAS"))
  expect_error(eq5d(2,4,6,8,10,"5L", "VAS", "Jupiter"))
  expect_error(eq5d(1,2,3,4,5,"5L", "VAS", "UK"))
  expect_error(eq5d(1,2,3,4,5,"5L", "TTO", "UK"))
})

test.df <- data.frame(Mobility=c(1,2,3,4,5),Care=c(1,5,4,3,2),Activity=c(1,5,2,3,1),Pain=c(1,3,4,3,4),Anxiety=c(1,2,1,2,1))

test_that("Wrapper function gives correct answer", {
  expect_equal(eq5d(c(Mobility=1,Care=2,Activity=3,Pain=2,Anxiety=1), type="TTO", version="3L", country="UK"), 0.329)
  expect_equal(eq5d(c(Mobility=3,Care=2,Activity=3,Pain=2,Anxiety=3), type="TTO", version="3L", country="Germany"), 0.083)
  expect_equal(eq5d(c(Mobility=1,Care=1,Activity=1,Pain=1,Anxiety=1), country="Canada", version="5L", type="VT"), 0.949)
  expect_equal(eq5d(c(Mobility=1,Care=2,Activity=3,Pain=4,Anxiety=5), country="Indonesia", version="5L", type="VT"), 0.240)
  expect_equal(eq5d(test.df, country="Canada", version="5L", type="VT"), c(0.949,0.362,0.39,0.524,0.431))
  expect_equal(eq5d(test.df, country="England", version="5L"), c(1.000,0.393,0.434,0.488,0.400))
})
  