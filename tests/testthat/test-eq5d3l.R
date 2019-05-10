context("EQ-5D-3L TTO")

test_that("EQ-5D-3L Denmark TTO gives correct answer", {
  expect_equal(eq5d3l(c(MO=1,SC=1,UA=1,PD=1,AD=1), "TTO", "Denmark"), 1.000)
  expect_equal(eq5d3l(c(MO=3,SC=2,UA=1,PD=2,AD=3), "TTO", "Denmark"), -0.017)
  expect_equal(eq5d3l(c(MO=1,SC=2,UA=1,PD=1,AD=1), "TTO", "Denmark"), 0.823)
  expect_equal(eq5d3l(c(MO=1,SC=2,UA=1,PD=2,AD=1), "TTO", "Denmark"), 0.761)
  expect_equal(eq5d3l(c(MO=3,SC=2,UA=3,PD=2,AD=3), "TTO", "Denmark"), -0.161)
  expect_equal(eq5d3l(c(MO=3,SC=3,UA=3,PD=3,AD=3), "TTO", "Denmark"), -0.624)
})

test_that("EQ-5D-3L France TTO gives correct answer", {
  expect_equal(eq5d3l(c(MO=1,SC=1,UA=1,PD=1,AD=1), "TTO", "France"), 1)
  expect_equal(eq5d3l(c(MO=1,SC=1,UA=2,PD=3,AD=3), "TTO", "France"), 0.201)
  expect_equal(eq5d3l(c(MO=2,SC=1,UA=2,PD=1,AD=2), "TTO", "France"), 0.599)
  expect_equal(eq5d3l(c(MO=3,SC=2,UA=1,PD=3,AD=3), "TTO", "France"), -0.227)
  expect_equal(eq5d3l(c(MO=3,SC=3,UA=3,PD=3,AD=3), "TTO", "France"), -0.53)
})

test_that("EQ-5D-3L Germany TTO gives correct answer", {
  expect_equal(eq5d3l(c(MO=1,SC=1,UA=1,PD=1,AD=1), "TTO", "Germany"), 1)
  expect_equal(eq5d3l(c(MO=1,SC=2,UA=2,PD=1,AD=1), "TTO", "Germany"), 0.912)
  expect_equal(eq5d3l(c(MO=1,SC=2,UA=2,PD=1,AD=3), "TTO", "Germany"), 0.524)
  expect_equal(eq5d3l(c(MO=3,SC=2,UA=3,PD=2,AD=3), "TTO", "Germany"), 0.083)
  expect_equal(eq5d3l(c(MO=3,SC=3,UA=3,PD=3,AD=3), "TTO", "Germany"), -0.207)
})

test_that("EQ-5D-3L Italy TTO gives correct answer", {
  expect_equal(eq5d3l(c(MO=1,SC=1,UA=1,PD=1,AD=1), "TTO", "Italy"), 1)
  expect_equal(eq5d3l(c(MO=2,SC=1,UA=3,PD=2,AD=1), "TTO", "Italy"), 0.714)
  expect_equal(eq5d3l(c(MO=1,SC=2,UA=3,PD=3,AD=2), "TTO", "Italy"), 0.402)
  expect_equal(eq5d3l(c(MO=3,SC=2,UA=2,PD=3,AD=3), "TTO", "Italy"), -0.078)
  expect_equal(eq5d3l(c(MO=3,SC=3,UA=3,PD=3,AD=3), "TTO", "Italy"), -0.38)
})

test_that("EQ-5D-3L Japan TTO gives correct answer", {
  expect_equal(eq5d3l(c(MO=1,SC=1,UA=1,PD=1,AD=1), "TTO", "Japan"), 1)
  expect_equal(eq5d3l(c(MO=2,SC=1,UA=1,PD=1,AD=1), "TTO", "Japan"), 0.773)
  expect_equal(eq5d3l(c(MO=3,SC=1,UA=1,PD=1,AD=3), "TTO", "Japan"), 0.318)
  expect_equal(eq5d3l(c(MO=3,SC=1,UA=2,PD=2,AD=3), "TTO", "Japan"), 0.194)
  expect_equal(eq5d3l(c(MO=3,SC=3,UA=3,PD=3,AD=3), "TTO", "Japan"), -0.111)
})

test_that("EQ-5D-3L Korea TTO gives correct answer", {
  expect_equal(eq5d3l(c(MO=1,SC=1,UA=1,PD=1,AD=1), "TTO", "Korea"), 1)
  expect_equal(eq5d3l(c(MO=1,SC=1,UA=1,PD=2,AD=1), "TTO", "Korea"), 0.913)
  expect_equal(eq5d3l(c(MO=2,SC=2,UA=2,PD=2,AD=2), "TTO", "Korea"), 0.677)
  expect_equal(eq5d3l(c(MO=3,SC=2,UA=3,PD=2,AD=2), "TTO", "Korea"), 0.148)
  expect_equal(eq5d3l(c(MO=3,SC=3,UA=3,PD=3,AD=3), "TTO", "Korea"), -0.171)
})

test_that("EQ-5D-3L Netherlands TTO gives correct answer", {
  expect_equal(eq5d3l(c(MO=1,SC=1,UA=1,PD=1,AD=1), "TTO", "Netherlands"), 1)
  expect_equal(eq5d3l(c(MO=2,SC=1,UA=2,PD=1,AD=1), "TTO", "Netherlands"), 0.861)
  expect_equal(eq5d3l(c(MO=2,SC=1,UA=2,PD=3,AD=1), "TTO", "Netherlands"), 0.298)
  expect_equal(eq5d3l(c(MO=2,SC=1,UA=3,PD=3,AD=3), "TTO", "Netherlands"), -0.052)
  expect_equal(eq5d3l(c(MO=3,SC=3,UA=3,PD=3,AD=3), "TTO", "Netherlands"), -0.329)
})

test_that("EQ-5D-3L Spain TTO gives correct answer", {
  expect_equal(eq5d3l(c(MO=1,SC=1,UA=1,PD=1,AD=1), "TTO", "Spain"), 1)
  expect_equal(eq5d3l(c(MO=1,SC=1,UA=1,PD=2,AD=2), "TTO", "Spain"), 0.825)
  expect_equal(eq5d3l(c(MO=2,SC=3,UA=1,PD=3,AD=3), "TTO", "Spain"), -0.135)
  expect_equal(eq5d3l(c(MO=3,SC=3,UA=1,PD=3,AD=3), "TTO", "Spain"), -0.459)
  expect_equal(eq5d3l(c(MO=3,SC=3,UA=3,PD=3,AD=3), "TTO", "Spain"), -0.654)
})

test_that("EQ-5D-5L Thailand TTO gives correct answer", {
  expect_equal(eq5d3l(c(MO=1,SC=1,UA=1,PD=1,AD=1), "TTO", "Thailand"), 1)
  expect_equal(eq5d3l(c(MO=1,SC=1,UA=1,PD=1,AD=2), "TTO", "Thailand"), 0.766)
  expect_equal(eq5d3l(c(MO=2,SC=1,UA=2,PD=1,AD=2), "TTO", "Thailand"), 0.586)
  expect_equal(eq5d3l(c(MO=2,SC=3,UA=2,PD=3,AD=2), "TTO", "Thailand"), -0.004)
  expect_equal(eq5d3l(c(MO=3,SC=3,UA=3,PD=3,AD=3), "TTO", "Thailand"), -0.452)
})

test_that("EQ-5D-3L UK TTO gives correct answer", {
  expect_equal(eq5d3l(c(MO=1,SC=1,UA=1,PD=1,AD=1), "TTO", "UK"), 1)
  expect_equal(eq5d3l(c(MO=1,SC=1,UA=2,PD=2,AD=1), "TTO", "UK"), 0.760)
  expect_equal(eq5d3l(c(MO=3,SC=3,UA=2,PD=2,AD=1), "TTO", "UK"), -0.037)
  expect_equal(eq5d3l(c(MO=1,SC=2,UA=3,PD=2,AD=1), "TTO", "UK"), 0.329)
  expect_equal(eq5d3l(c(MO=3,SC=3,UA=3,PD=3,AD=3), "TTO", "UK"), -0.594)
})

test_that("EQ-5D-3L USA TTO gives correct answer", {
  expect_equal(eq5d3l(c(MO=1,SC=1,UA=1,PD=1,AD=1), "TTO", "USA"), 1)
  expect_equal(eq5d3l(c(MO=1,SC=1,UA=1,PD=2,AD=1), "TTO", "USA"), 0.827)
  expect_equal(eq5d3l(c(MO=2,SC=2,UA=1,PD=1,AD=2), "TTO", "USA"), 0.759)
  expect_equal(eq5d3l(c(MO=3,SC=3,UA=1,PD=1,AD=3), "TTO", "USA"), 0.102)
  expect_equal(eq5d3l(c(MO=3,SC=3,UA=3,PD=3,AD=3), "TTO", "USA"), -0.109)
})

test_that("EQ-5D-3L Zimbabwe TTO gives correct answer", {
  expect_equal(eq5d3l(c(MO=1,SC=1,UA=1,PD=1,AD=1), "TTO", "Zimbabwe"), 1)
  expect_equal(eq5d3l(c(MO=1,SC=1,UA=2,PD=1,AD=1), "TTO", "Zimbabwe"), 0.857)
  expect_equal(eq5d3l(c(MO=2,SC=3,UA=2,PD=1,AD=1), "TTO", "Zimbabwe"), 0.570)
  expect_equal(eq5d3l(c(MO=3,SC=1,UA=1,PD=3,AD=1), "TTO", "Zimbabwe"), 0.394)
  expect_equal(eq5d3l(c(MO=3,SC=3,UA=3,PD=3,AD=3), "TTO", "Zimbabwe"), -0.145)
})


context("EQ-5D-3L VAS")

test_that("EQ-5D-3L Belgium VAS gives correct answer", {
  expect_equal(eq5d3l(c(MO=1,SC=1,UA=1,PD=1,AD=1), "VAS", "Belgium"), 1)
  expect_equal(eq5d3l(c(MO=2,SC=1,UA=1,PD=1,AD=1), "VAS", "Belgium"), 0.774)
  expect_equal(eq5d3l(c(MO=2,SC=1,UA=3,PD=1,AD=1), "VAS", "Belgium"), 0.456)
  expect_equal(eq5d3l(c(MO=3,SC=2,UA=3,PD=1,AD=1), "VAS", "Belgium"), 0.299)
  expect_equal(eq5d3l(c(MO=3,SC=3,UA=3,PD=3,AD=3), "VAS", "Belgium"), -0.158)
})

test_that("EQ-5D-3L Denmark VAS gives correct answer", {
  expect_equal(eq5d3l(c(MO=1,SC=1,UA=1,PD=1,AD=1), "VAS", "Denmark"), 1)
  expect_equal(eq5d3l(c(MO=1,SC=1,UA=1,PD=3,AD=1), "VAS", "Denmark"), 0.619)
  expect_equal(eq5d3l(c(MO=2,SC=3,UA=3,PD=2,AD=3), "VAS", "Denmark"), 0.037)
  expect_equal(eq5d3l(c(MO=3,SC=3,UA=2,PD=1,AD=3), "VAS", "Denmark"), 0.053)
  expect_equal(eq5d3l(c(MO=3,SC=3,UA=3,PD=3,AD=3), "VAS", "Denmark"), -0.167)
})

test_that("EQ-5D-3L Europe VAS gives correct answer", {
  expect_equal(eq5d3l(c(MO=1,SC=1,UA=1,PD=1,AD=1), "VAS", "Europe"), 1)
  expect_equal(eq5d3l(c(MO=1,SC=1,UA=2,PD=2,AD=1), "VAS", "Europe"), 0.753)
  expect_equal(eq5d3l(c(MO=3,SC=3,UA=1,PD=1,AD=2), "VAS", "Europe"), 0.215)
  expect_equal(eq5d3l(c(MO=3,SC=1,UA=2,PD=3,AD=3), "VAS", "Europe"), 0.141)
  expect_equal(eq5d3l(c(MO=3,SC=3,UA=3,PD=3,AD=3), "VAS", "Europe"), -0.074)
})

test_that("EQ-5D-3L Finland VAS gives correct answer", {
  expect_equal(eq5d3l(c(MO=1,SC=1,UA=1,PD=1,AD=1), "VAS", "Finland"), 1)
  expect_equal(eq5d3l(c(MO=1,SC=2,UA=1,PD=1,AD=1), "VAS", "Finland"), 0.744)
  expect_equal(eq5d3l(c(MO=3,SC=2,UA=1,PD=3,AD=1), "VAS", "Finland"), 0.361)
  expect_equal(eq5d3l(c(MO=3,SC=2,UA=3,PD=3,AD=3), "VAS", "Finland"), 0.034)
  expect_equal(eq5d3l(c(MO=3,SC=3,UA=3,PD=3,AD=3), "VAS", "Finland"), -0.011)
})

test_that("EQ-5D-3L Germany VAS gives correct answer", {
  expect_equal(eq5d3l(c(MO=1,SC=1,UA=1,PD=1,AD=1), "VAS", "Germany"), 1)
  expect_equal(eq5d3l(c(MO=3,SC=2,UA=1,PD=1,AD=1), "VAS", "Germany"), 0.294)
  expect_equal(eq5d3l(c(MO=3,SC=1,UA=1,PD=3,AD=1), "VAS", "Germany"), 0.170)
  expect_equal(eq5d3l(c(MO=3,SC=2,UA=2,PD=3,AD=2), "VAS", "Germany"), 0.099)
  expect_equal(eq5d3l(c(MO=3,SC=3,UA=3,PD=3,AD=3), "VAS", "Germany"), 0.021)
})

test_that("EQ-5D-3L New Zealand VAS gives correct answer", {
  expect_equal(eq5d3l(c(MO=1,SC=1,UA=1,PD=1,AD=1), "VAS", "NewZealand"), 1)
  expect_equal(eq5d3l(c(MO=1,SC=1,UA=2,PD=1,AD=1), "VAS", "NewZealand"), 0.782) 
  expect_equal(eq5d3l(c(MO=1,SC=2,UA=3,PD=2,AD=1), "VAS", "NewZealand"), 0.401) 
  expect_equal(eq5d3l(c(MO=3,SC=2,UA=1,PD=2,AD=3), "VAS", "NewZealand"), 0.094) 
  expect_equal(eq5d3l(c(MO=3,SC=3,UA=3,PD=3,AD=3), "VAS", "NewZealand"), -0.085) 
})

test_that("EQ-5D-3L Slovenia VAS gives correct answer", {
  expect_equal(eq5d3l(c(MO=1,SC=1,UA=1,PD=1,AD=1), "VAS", "Slovenia"), 1)
  expect_equal(eq5d3l(c(MO=1,SC=1,UA=1,PD=1,AD=2), "VAS", "Slovenia"), 0.779)
  expect_equal(eq5d3l(c(MO=2,SC=3,UA=2,PD=2,AD=2), "VAS", "Slovenia"), 0.222)
  expect_equal(eq5d3l(c(MO=3,SC=1,UA=1,PD=3,AD=3), "VAS", "Slovenia"), 0.052)
  expect_equal(eq5d3l(c(MO=3,SC=3,UA=3,PD=3,AD=3), "VAS", "Slovenia"), -0.242)
})

test_that("EQ-5D-3L Spain VAS gives correct answer", {
  expect_equal(eq5d3l(c(MO=1,SC=1,UA=1,PD=1,AD=1), "VAS", "Spain"), 1)
  expect_equal(eq5d3l(c(MO=1,SC=3,UA=1,PD=1,AD=1), "VAS", "Spain"), 0.436)
  expect_equal(eq5d3l(c(MO=1,SC=3,UA=3,PD=1,AD=2), "VAS", "Spain"), 0.274)
  expect_equal(eq5d3l(c(MO=3,SC=3,UA=2,PD=2,AD=2), "VAS", "Spain"), 0.090)
  expect_equal(eq5d3l(c(MO=3,SC=3,UA=3,PD=3,AD=3), "VAS", "Spain"), -0.076)
})

test_that("EQ-5D-3L UK VAS gives correct answer", {
  expect_equal(eq5d3l(c(MO=1,SC=1,UA=1,PD=1,AD=1), "VAS", "UK"), 1)
  expect_equal(eq5d3l(c(MO=1,SC=1,UA=2,PD=1,AD=1), "VAS", "UK"), 0.814)
  expect_equal(eq5d3l(c(MO=2,SC=2,UA=2,PD=2,AD=2), "VAS", "UK"), 0.503)
  expect_equal(eq5d3l(c(MO=3,SC=2,UA=3,PD=2,AD=3), "VAS", "UK"), 0.066)
  expect_equal(eq5d3l(c(MO=3,SC=3,UA=3,PD=3,AD=3), "VAS", "UK"), -0.073)
})

context("EQ-5D-3L Incorrect params")

test_that("EQ-5D-3L throws error for incorrect parameters", {
  expect_error(eq5d3l(c(MB=1,SC=2,UA=3,PD=2,AD=1), type="TTO", country="UK"))
  expect_error(eq5d3l(c(MO=1,SC=2,UA=3,PD=4,AD=5), "VAS", "UK"))
  expect_error(eq5d3l(c(MO=1,SC=1,UA=1,PD=1,AD=1), type="XXX"))
  expect_error(eq5d3l(c(MO=1,SC=2,UA=3,PD=2,AD=1), "VAS", "Liechtenstein"))
})

