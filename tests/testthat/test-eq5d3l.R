context("EQ-5D-3L TTO")

test_that("EQ-5D-3L Argentina TTO gives correct answer", {
  expect_equal(eq5d3l(c(MO=1,SC=1,UA=1,PD=1,AD=1), "TTO", "Argentina"), 1.000)
  expect_equal(eq5d3l(c(MO=1,SC=2,UA=2,PD=2,AD=2), "TTO", "Argentina"), 0.711, tolerance = .0011)
  expect_equal(eq5d3l(c(MO=1,SC=2,UA=2,PD=3,AD=3), "TTO", "Argentina"), 0.258)
  expect_equal(eq5d3l(c(MO=2,SC=2,UA=2,PD=3,AD=2), "TTO", "Argentina"), 0.493, tolerance = .0021)
  expect_equal(eq5d3l(c(MO=3,SC=3,UA=3,PD=1,AD=3), "TTO", "Argentina"), -0.121, tolerance = .0031)
  expect_equal(eq5d3l(c(MO=3,SC=3,UA=3,PD=3,AD=3), "TTO", "Argentina"), -0.376, tolerance = .0041)
})

test_that("EQ-5D-3L Australia TTO gives correct answer", {
  expect_equal(eq5d3l(c(MO=1,SC=1,UA=1,PD=1,AD=1), "TTO", "Australia"), 1.000)
  expect_equal(eq5d3l(c(MO=1,SC=1,UA=1,PD=2,AD=1), "TTO", "Australia"), 0.828, tolerance = .0011)
  expect_equal(eq5d3l(c(MO=1,SC=2,UA=2,PD=3,AD=3), "TTO", "Australia"), 0.101)
  expect_equal(eq5d3l(c(MO=2,SC=2,UA=2,PD=3,AD=2), "TTO", "Australia"), 0.141)
  expect_equal(eq5d3l(c(MO=3,SC=3,UA=3,PD=3,AD=1), "TTO", "Australia"), -0.143)
  expect_equal(eq5d3l(c(MO=3,SC=3,UA=3,PD=3,AD=3), "TTO", "Australia"), -0.217)
})

test_that("EQ-5D-3L Brazil TTO gives correct answer", {
  expect_equal(eq5d3l(c(MO=1,SC=1,UA=1,PD=1,AD=1), "TTO", "Brazil"), 1.000)
  expect_equal(eq5d3l(c(MO=1,SC=1,UA=2,PD=2,AD=2), "TTO", "Brazil"), 0.640)
  expect_equal(eq5d3l(c(MO=1,SC=2,UA=1,PD=3,AD=3), "TTO", "Brazil"), 0.477, tolerance = .0011)
  expect_equal(eq5d3l(c(MO=1,SC=2,UA=2,PD=3,AD=2), "TTO", "Brazil"), 0.425, tolerance = .0011)
  expect_equal(eq5d3l(c(MO=3,SC=3,UA=3,PD=2,AD=3), "TTO", "Brazil"), -0.073)
  expect_equal(eq5d3l(c(MO=3,SC=3,UA=3,PD=3,AD=3), "TTO", "Brazil"), -0.176, tolerance = .0011)
})

test_that("EQ-5D-3L Canada TTO gives correct answer", {
  expect_equal(eq5d3l(c(MO=1,SC=1,UA=1,PD=1,AD=1), "TTO", "Canada"), 1.000)
  expect_equal(eq5d3l(c(MO=1,SC=1,UA=2,PD=1,AD=2), "TTO", "Canada"), 0.754)
  expect_equal(eq5d3l(c(MO=1,SC=2,UA=1,PD=2,AD=3), "TTO", "Canada"), 0.494, tolerance = .0011)
  expect_equal(eq5d3l(c(MO=2,SC=2,UA=2,PD=3,AD=2), "TTO", "Canada"), 0.340, tolerance = .0011)
  expect_equal(eq5d3l(c(MO=3,SC=2,UA=3,PD=2,AD=3), "TTO", "Canada"), 0.066)
  expect_equal(eq5d3l(c(MO=3,SC=3,UA=3,PD=3,AD=3), "TTO", "Canada"), -0.340)
})

test_that("EQ-5D-3L Chile TTO gives correct answer", {
  expect_equal(eq5d3l(c(MO=1,SC=1,UA=1,PD=1,AD=1), "TTO", "Chile"), 1.000)
  expect_equal(eq5d3l(c(MO=1,SC=2,UA=2,PD=1,AD=2), "TTO", "Chile"), 0.564)
  expect_equal(eq5d3l(c(MO=1,SC=2,UA=3,PD=1,AD=3), "TTO", "Chile"), 0.134)
  expect_equal(eq5d3l(c(MO=2,SC=2,UA=3,PD=3,AD=2), "TTO", "Chile"), -0.168)
  expect_equal(eq5d3l(c(MO=3,SC=3,UA=3,PD=1,AD=1), "TTO", "Chile"), -0.129)
  expect_equal(eq5d3l(c(MO=3,SC=3,UA=3,PD=3,AD=3), "TTO", "Chile"), -0.497)
})

test_that("EQ-5D-3L China TTO gives correct answer", {
  expect_equal(eq5d3l(c(MO=1,SC=1,UA=1,PD=1,AD=1), "TTO", "China"), 1.000)
  expect_equal(eq5d3l(c(MO=1,SC=1,UA=2,PD=1,AD=1), "TTO", "China"), 0.887)
  expect_equal(eq5d3l(c(MO=2,SC=3,UA=2,PD=2,AD=1), "TTO", "China"), 0.466)
  expect_equal(eq5d3l(c(MO=2,SC=2,UA=2,PD=3,AD=3), "TTO", "China"), 0.220)
  expect_equal(eq5d3l(c(MO=3,SC=3,UA=3,PD=3,AD=3), "TTO", "China"), -0.149)
})

test_that("EQ-5D-3L Denmark TTO gives correct answer", {
  expect_equal(eq5d3l(c(MO=1,SC=1,UA=1,PD=1,AD=1), "TTO", "Denmark"), 1.000)
  expect_equal(eq5d3l(c(MO=3,SC=2,UA=1,PD=2,AD=3), "TTO", "Denmark"), -0.018, tolerance = .0011)
  expect_equal(eq5d3l(c(MO=1,SC=2,UA=1,PD=1,AD=1), "TTO", "Denmark"), 0.823)
  expect_equal(eq5d3l(c(MO=1,SC=2,UA=1,PD=2,AD=1), "TTO", "Denmark"), 0.761)
  expect_equal(eq5d3l(c(MO=3,SC=2,UA=3,PD=2,AD=3), "TTO", "Denmark"), -0.161)
  expect_equal(eq5d3l(c(MO=3,SC=3,UA=3,PD=3,AD=3), "TTO", "Denmark"), -0.624)
})

test_that("EQ-5D-3L France TTO gives correct answer", {
  expect_equal(eq5d3l(c(MO=1,SC=1,UA=1,PD=1,AD=1), "TTO", "France"), 1)
  expect_equal(eq5d3l(c(MO=1,SC=1,UA=2,PD=3,AD=3), "TTO", "France"), 0.201)
  expect_equal(eq5d3l(c(MO=2,SC=1,UA=2,PD=1,AD=2), "TTO", "France"), 0.600, tolerance = .0011)
  expect_equal(eq5d3l(c(MO=3,SC=2,UA=1,PD=3,AD=3), "TTO", "France"), -0.228, tolerance = .0011)
  expect_equal(eq5d3l(c(MO=3,SC=3,UA=3,PD=3,AD=3), "TTO", "France"), -0.529, tolerance = .0011)
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

test_that("EQ-5D-3L South Korea TTO gives correct answer", {
  expect_equal(eq5d3l(c(MO=1,SC=1,UA=1,PD=1,AD=1), "TTO", "SouthKorea"), 1)
  expect_equal(eq5d3l(c(MO=1,SC=1,UA=1,PD=2,AD=1), "TTO", "SouthKorea"), 0.913)
  expect_equal(eq5d3l(c(MO=2,SC=2,UA=2,PD=3,AD=2), "TTO", "SouthKorea"), 0.513)
  expect_equal(eq5d3l(c(MO=2,SC=1,UA=2,PD=2,AD=1), "TTO", "SouthKorea"), 0.766)
  expect_equal(eq5d3l(c(MO=2,SC=2,UA=2,PD=2,AD=2), "TTO", "SouthKorea"), 0.677)
  expect_equal(eq5d3l(c(MO=3,SC=2,UA=3,PD=2,AD=2), "TTO", "SouthKorea"), 0.148)
  expect_equal(eq5d3l(c(MO=3,SC=3,UA=3,PD=3,AD=3), "TTO", "SouthKorea"), -0.171)
})

test_that("EQ-5D-3L Netherlands TTO gives correct answer", {
  expect_equal(eq5d3l(c(MO=1,SC=1,UA=1,PD=1,AD=1), "TTO", "Netherlands"), 1)
  expect_equal(eq5d3l(c(MO=2,SC=1,UA=2,PD=1,AD=1), "TTO", "Netherlands"), 0.861)
  expect_equal(eq5d3l(c(MO=2,SC=1,UA=2,PD=3,AD=1), "TTO", "Netherlands"), 0.298)
  expect_equal(eq5d3l(c(MO=2,SC=1,UA=3,PD=3,AD=3), "TTO", "Netherlands"), -0.052)
  expect_equal(eq5d3l(c(MO=3,SC=3,UA=3,PD=3,AD=3), "TTO", "Netherlands"), -0.329)
})

test_that("EQ-5D-3L Poland TTO gives correct answer", {
  expect_equal(eq5d3l(c(MO=1,SC=1,UA=1,PD=1,AD=1), "TTO", "Poland"), 1)
  expect_equal(eq5d3l(c(MO=1,SC=1,UA=1,PD=1,AD=2), "TTO", "Poland"), 0.925)
  expect_equal(eq5d3l(c(MO=2,SC=1,UA=2,PD=2,AD=1), "TTO", "Poland"), 0.796)
  expect_equal(eq5d3l(c(MO=2,SC=1,UA=3,PD=3,AD=3), "TTO", "Poland"), -0.009)
  expect_equal(eq5d3l(c(MO=3,SC=3,UA=3,PD=3,AD=3), "TTO", "Poland"), -0.523)
})

test_that("EQ-5D-3L Portugal TTO gives correct answer", {
  expect_equal(eq5d3l(c(MO=1,SC=1,UA=1,PD=1,AD=1), "TTO", "Portugal"), 1)
  expect_equal(eq5d3l(c(MO=1,SC=1,UA=1,PD=2,AD=1), "TTO", "Portugal"), 0.694)
  expect_equal(eq5d3l(c(MO=1,SC=1,UA=2,PD=1,AD=2), "TTO", "Portugal"), 0.665)
  expect_equal(eq5d3l(c(MO=1,SC=1,UA=3,PD=1,AD=2), "TTO", "Portugal"), 0.444)
  expect_equal(eq5d3l(c(MO=2,SC=2,UA=2,PD=2,AD=2), "TTO", "Portugal"), 0.288)
  expect_equal(eq5d3l(c(MO=3,SC=1,UA=3,PD=1,AD=3), "TTO", "Portugal"), -0.274)
  expect_equal(eq5d3l(c(MO=3,SC=1,UA=3,PD=2,AD=3), "TTO", "Portugal"), -0.143)
  expect_equal(eq5d3l(c(MO=3,SC=1,UA=3,PD=3,AD=1), "TTO", "Portugal"), -0.344)
  expect_equal(eq5d3l(c(MO=3,SC=3,UA=3,PD=2,AD=3), "TTO", "Portugal"), -0.335)
  expect_equal(eq5d3l(c(MO=3,SC=3,UA=3,PD=3,AD=3), "TTO", "Portugal"), -0.496)
})

test_that("EQ-5D-3L Singapore TTO gives correct answer", {
  expect_equal(eq5d3l(c(MO=1,SC=1,UA=1,PD=1,AD=1), "TTO", "Singapore"), 1)
  expect_equal(eq5d3l(c(MO=1,SC=1,UA=1,PD=2,AD=1), "TTO", "Singapore"), 0.854)
  expect_equal(eq5d3l(c(MO=3,SC=3,UA=3,PD=3,AD=3), "TTO", "Singapore"), -0.769)
})

test_that("EQ-5D-3L Spain TTO gives correct answer", {
  expect_equal(eq5d3l(c(MO=1,SC=1,UA=1,PD=1,AD=1), "TTO", "Spain"), 1)
  expect_equal(eq5d3l(c(MO=1,SC=1,UA=1,PD=2,AD=2), "TTO", "Spain"), 0.825)
  expect_equal(eq5d3l(c(MO=2,SC=3,UA=1,PD=3,AD=3), "TTO", "Spain"), -0.135)
  expect_equal(eq5d3l(c(MO=3,SC=3,UA=1,PD=3,AD=3), "TTO", "Spain"), -0.459)
  expect_equal(eq5d3l(c(MO=3,SC=3,UA=3,PD=3,AD=3), "TTO", "Spain"), -0.654)
})

test_that("EQ-5D-3L Sri Lanka TTO gives correct answer", {
  expect_equal(eq5d3l(c(MO=1,SC=1,UA=1,PD=1,AD=1), "TTO", "SriLanka"), 1)
  expect_equal(eq5d3l(c(MO=2,SC=2,UA=2,PD=1,AD=2), "TTO", "SriLanka"), 0.395)
  expect_equal(eq5d3l(c(MO=3,SC=3,UA=3,PD=3,AD=3), "TTO", "SriLanka"), -0.711)
})

test_that("EQ-5D-3L Sweden TTO gives correct answer", {
  expect_equal(eq5d3l(c(MO=1,SC=1,UA=1,PD=1,AD=1), "TTO", "Sweden"), 0.969)
  expect_equal(eq5d3l(c(MO=1,SC=1,UA=1,PD=2,AD=1), "TTO", "Sweden"), 0.935)
  expect_equal(eq5d3l(c(MO=1,SC=2,UA=3,PD=2,AD=1), "TTO", "Sweden"), 0.728, tolerance = .0011)
  expect_equal(eq5d3l(c(MO=1,SC=2,UA=3,PD=1,AD=3), "TTO", "Sweden"), 0.555)
  expect_equal(eq5d3l(c(MO=2,SC=2,UA=2,PD=2,AD=3), "TTO", "Sweden"), 0.489)
  expect_equal(eq5d3l(c(MO=3,SC=2,UA=3,PD=2,AD=3), "TTO", "Sweden"), 0.396)
  expect_equal(eq5d3l(c(MO=3,SC=3,UA=2,PD=3,AD=3), "TTO", "Sweden"), 0.375)
  expect_equal(eq5d3l(c(MO=3,SC=3,UA=3,PD=3,AD=3), "TTO", "Sweden"), 0.340)
})

test_that("EQ-5D-3L Taiwan TTO gives correct answer", {
  expect_equal(eq5d3l(c(MO=1,SC=1,UA=1,PD=1,AD=1), "TTO", "Taiwan"), 1)
  expect_equal(eq5d3l(c(MO=2,SC=1,UA=1,PD=1,AD=2), "TTO", "Taiwan"), 0.539, tolerance = .0011)
  expect_equal(eq5d3l(c(MO=2,SC=1,UA=3,PD=1,AD=3), "TTO", "Taiwan"), 0.012)
  expect_equal(eq5d3l(c(MO=2,SC=3,UA=2,PD=3,AD=2), "TTO", "Taiwan"), -0.273, tolerance = .0011)
  expect_equal(eq5d3l(c(MO=3,SC=3,UA=3,PD=3,AD=3), "TTO", "Taiwan"), -0.674)
})

test_that("EQ-5D-3L Thailand TTO gives correct answer", {
  expect_equal(eq5d3l(c(MO=1,SC=1,UA=1,PD=1,AD=1), "TTO", "Thailand"), 1)
  expect_equal(eq5d3l(c(MO=1,SC=1,UA=1,PD=1,AD=2), "TTO", "Thailand"), 0.766)
  expect_equal(eq5d3l(c(MO=2,SC=1,UA=2,PD=1,AD=2), "TTO", "Thailand"), 0.586)
  expect_equal(eq5d3l(c(MO=2,SC=3,UA=2,PD=3,AD=2), "TTO", "Thailand"), -0.005, tolerance = .0011)
  expect_equal(eq5d3l(c(MO=3,SC=3,UA=3,PD=3,AD=3), "TTO", "Thailand"), -0.454, tolerance = .0021)
})

test_that("EQ-5D-3L Trinidad and Tobago TTO gives correct answer", {
  expect_equal(eq5d3l(c(MO=1,SC=1,UA=1,PD=1,AD=1), "TTO", "Trinidad_and_Tobago"), 1)
  expect_equal(eq5d3l(c(MO=1,SC=1,UA=1,PD=1,AD=2), "TTO", "Trinidad_and_Tobago"), 0.896)
  expect_equal(eq5d3l(c(MO=2,SC=1,UA=2,PD=1,AD=2), "TTO", "Trinidad_and_Tobago"), 0.807, tolerance = .0011)
  expect_equal(eq5d3l(c(MO=2,SC=3,UA=2,PD=3,AD=2), "TTO", "Trinidad_and_Tobago"), 0.406)
  expect_equal(eq5d3l(c(MO=3,SC=3,UA=3,PD=3,AD=3), "TTO", "Trinidad_and_Tobago"), -0.163)
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
  expect_equal(eq5d3l(c(MO=1,SC=2,UA=3,PD=2,AD=1), "VAS", "Finland"), 0.502)
  expect_equal(eq5d3l(c(MO=3,SC=2,UA=1,PD=3,AD=1), "VAS", "Finland"), 0.361)
  expect_equal(eq5d3l(c(MO=3,SC=2,UA=3,PD=3,AD=3), "VAS", "Finland"), 0.034)
  expect_equal(eq5d3l(c(MO=3,SC=3,UA=3,PD=2,AD=3), "VAS", "Finland"), 0.031)
  expect_equal(eq5d3l(c(MO=3,SC=3,UA=3,PD=3,AD=3), "VAS", "Finland"), -0.011)
})

test_that("EQ-5D-3L Germany VAS gives correct answer", {
  expect_equal(eq5d3l(c(MO=1,SC=1,UA=1,PD=1,AD=1), "VAS", "Germany"), 1)
  expect_equal(eq5d3l(c(MO=3,SC=2,UA=1,PD=1,AD=1), "VAS", "Germany"), 0.294)
  expect_equal(eq5d3l(c(MO=3,SC=1,UA=1,PD=3,AD=1), "VAS", "Germany"), 0.170)
  expect_equal(eq5d3l(c(MO=3,SC=2,UA=2,PD=3,AD=2), "VAS", "Germany"), 0.099)
  expect_equal(eq5d3l(c(MO=3,SC=3,UA=3,PD=3,AD=3), "VAS", "Germany"), 0.021)
})

test_that("EQ-5D-3L Iran VAS gives correct answer", {
  expect_equal(eq5d3l(c(MO=1,SC=1,UA=1,PD=1,AD=1), "VAS", "Iran"), 1)
  expect_equal(eq5d3l(c(MO=1,SC=1,UA=1,PD=1,AD=2), "VAS", "Iran"), 0.749)
  expect_equal(eq5d3l(c(MO=1,SC=1,UA=1,PD=1,AD=3), "VAS", "Iran"), 0.640)
  expect_equal(eq5d3l(c(MO=2,SC=1,UA=2,PD=3,AD=2), "VAS", "Iran"), 0.295)
  expect_equal(eq5d3l(c(MO=3,SC=2,UA=2,PD=2,AD=3), "VAS", "Iran"), 0.043)
  expect_equal(eq5d3l(c(MO=3,SC=3,UA=3,PD=3,AD=3), "VAS", "Iran"), -0.070)
})

test_that("EQ-5D-3L Malaysia VAS gives correct answer", {
  expect_equal(eq5d3l(c(MO=1,SC=1,UA=1,PD=1,AD=1), "VAS", "Malaysia"), 1)
  expect_equal(eq5d3l(c(MO=1,SC=1,UA=1,PD=2,AD=1), "VAS", "Malaysia"), 0.879)
  expect_equal(eq5d3l(c(MO=1,SC=1,UA=3,PD=1,AD=2), "VAS", "Malaysia"), 0.614)
  expect_equal(eq5d3l(c(MO=1,SC=3,UA=3,PD=1,AD=1), "VAS", "Malaysia"), 0.535)
  expect_equal(eq5d3l(c(MO=3,SC=2,UA=3,PD=1,AD=3), "VAS", "Malaysia"), 0.321)
  expect_equal(eq5d3l(c(MO=3,SC=3,UA=3,PD=3,AD=3), "VAS", "Malaysia"), 0.130, tolerance = .0011)
})

test_that("EQ-5D-3L New Zealand VAS gives correct answer", {
  expect_equal(eq5d3l(c(MO=1,SC=1,UA=1,PD=1,AD=1), "VAS", "NewZealand"), 1)
  expect_equal(eq5d3l(c(MO=1,SC=1,UA=2,PD=1,AD=1), "VAS", "NewZealand"), 0.782)
  expect_equal(eq5d3l(c(MO=1,SC=1,UA=1,PD=1,AD=2), "VAS", "NewZealand"), 0.704)
  expect_equal(eq5d3l(c(MO=2,SC=2,UA=2,PD=2,AD=2), "VAS", "NewZealand"), 0.464)
  expect_equal(eq5d3l(c(MO=1,SC=2,UA=3,PD=2,AD=1), "VAS", "NewZealand"), 0.401)
  expect_equal(eq5d3l(c(MO=1,SC=1,UA=1,PD=3,AD=3), "VAS", "NewZealand"), 0.236)
  expect_equal(eq5d3l(c(MO=3,SC=3,UA=3,PD=2,AD=1), "VAS", "NewZealand"), 0.179)
  expect_equal(eq5d3l(c(MO=3,SC=2,UA=1,PD=2,AD=3), "VAS", "NewZealand"), 0.094)
  expect_equal(eq5d3l(c(MO=2,SC=3,UA=2,PD=3,AD=2), "VAS", "NewZealand"), 0.096)
  expect_equal(eq5d3l(c(MO=3,SC=3,UA=3,PD=3,AD=3), "VAS", "NewZealand"), -0.085)
})

test_that("EQ-5D-3L Slovenia VAS gives correct answer", {
  expect_equal(eq5d3l(c(MO=1,SC=1,UA=1,PD=1,AD=1), "VAS", "Slovenia"), 1)
  expect_equal(eq5d3l(c(MO=1,SC=1,UA=1,PD=1,AD=2), "VAS", "Slovenia"), 0.779)
  expect_equal(eq5d3l(c(MO=1,SC=2,UA=3,PD=2,AD=1), "VAS", "Slovenia"), 0.560)
  expect_equal(eq5d3l(c(MO=2,SC=1,UA=2,PD=1,AD=2), "VAS", "Slovenia"), 0.519)
  expect_equal(eq5d3l(c(MO=2,SC=2,UA=2,PD=2,AD=2), "VAS", "Slovenia"), 0.315)
  expect_equal(eq5d3l(c(MO=2,SC=3,UA=2,PD=2,AD=2), "VAS", "Slovenia"), 0.222)
  expect_equal(eq5d3l(c(MO=2,SC=3,UA=2,PD=3,AD=2), "VAS", "Slovenia"), 0.111)
  expect_equal(eq5d3l(c(MO=3,SC=1,UA=1,PD=3,AD=3), "VAS", "Slovenia"), 0.052)
  expect_equal(eq5d3l(c(MO=3,SC=3,UA=3,PD=2,AD=2), "VAS", "Slovenia"), -0.038)
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

