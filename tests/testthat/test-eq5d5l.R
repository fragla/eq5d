context("EQ-5D-5L")

test_that("EQ-5D-5L Australia gives correct answer", {
  expect_equal(eq5d5l(c(MO=1,SC=1,UA=1,PD=1,AD=1), "Australia"), 1)
  expect_equal(eq5d5l(c(MO=2,SC=3,UA=2,PD=3,AD=5), "Australia"), 0.431)
  expect_equal(eq5d5l(c(MO=2,SC=2,UA=2,PD=2,AD=2), "Australia"), 0.855)
  expect_equal(eq5d5l(c(MO=3,SC=3,UA=3,PD=3,AD=3), "Australia"), 0.673)
  expect_equal(eq5d5l(c(MO=4,SC=4,UA=4,PD=4,AD=4), "Australia"), -0.126)
  expect_equal(eq5d5l(c(MO=5,SC=5,UA=5,PD=5,AD=5), "Australia"),-0.301)
})

test_that("EQ-5D-5L Belgium gives correct answer", {
  expect_equal(eq5d5l(c(MO=1,SC=1,UA=1,PD=1,AD=1), "Belgium"), 1)
  expect_equal(eq5d5l(c(MO=2,SC=2,UA=2,PD=2,AD=2), "Belgium"), 0.755)
  expect_equal(eq5d5l(c(MO=3,SC=3,UA=3,PD=3,AD=3), "Belgium"), 0.577)
  expect_equal(eq5d5l(c(MO=4,SC=4,UA=4,PD=4,AD=4), "Belgium"), -0.215)
  expect_equal(eq5d5l(c(MO=1,SC=5,UA=3,PD=4,AD=2), "Belgium"), 0.309)
  expect_equal(eq5d5l(c(MO=5,SC=4,UA=3,PD=2,AD=1), "Belgium"), 0.491)
  expect_equal(eq5d5l(c(MO=5,SC=5,UA=5,PD=5,AD=5), "Belgium"),-0.532)
})

test_that("EQ-5D-5L Canada gives correct answer", {
  expect_equal(eq5d5l(c(MO=2,SC=3,UA=1,PD=4,AD=5), "Canada"), 0.275)
  expect_equal(eq5d5l(c(MO=1,SC=1,UA=1,PD=1,AD=1), "Canada"), 0.949)
  expect_equal(eq5d5l(c(MO=5,SC=5,UA=5,PD=5,AD=5), "Canada"),-0.148)
})

test_that("EQ-5D-5L China gives correct answer", {
  expect_equal(eq5d5l(c(MO=1,SC=1,UA=1,PD=1,AD=1), "China"), 1)
  expect_equal(eq5d5l(c(MO=1,SC=1,UA=2,PD=1,AD=1), "China"), 0.955)
  expect_equal(eq5d5l(c(MO=2,SC=1,UA=1,PD=1,AD=1), "China"), 0.934)
  expect_equal(eq5d5l(c(MO=5,SC=5,UA=5,PD=5,AD=5), "China"), -0.391)
})

test_that("EQ-5D-5L Denmark gives correct answer", {
  expect_equal(eq5d5l(c(MO=1,SC=1,UA=1,PD=1,AD=1), "Denmark"), 1)
  expect_equal(eq5d5l(c(MO=1,SC=3,UA=2,PD=2,AD=4), "Denmark"), 0.439)
  expect_equal(eq5d5l(c(MO=5,SC=5,UA=5,PD=5,AD=5), "Denmark"), -0.758)
})

test_that("EQ-5D-5L Denmark gives correct answer", {
  expect_equal(eq5d5l(c(MO=1,SC=1,UA=1,PD=1,AD=1), "Egypt"), 1)
  expect_equal(eq5d5l(c(MO=1,SC=1,UA=2,PD=1,AD=1), "Egypt"), 0.948)
  expect_equal(eq5d5l(c(MO=1,SC=1,UA=1,PD=2,AD=1), "Egypt"), 0.946)
  expect_equal(eq5d5l(c(MO=3,SC=3,UA=3,PD=3,AD=3), "Egypt"), 0.321)
  expect_equal(eq5d5l(c(MO=5,SC=5,UA=5,PD=5,AD=5), "Egypt"), -0.964)
})

test_that("EQ-5D-5L England gives correct answer", {
  expect_equal(eq5d5l(c(MO=2,SC=3,UA=2,PD=4,AD=5), "England"), 0.247)
  expect_equal(eq5d5l(c(MO=1,SC=1,UA=1,PD=1,AD=1), "England"), 1)
  expect_equal(eq5d5l(c(MO=1,SC=1,UA=1,PD=1,AD=2), "England"), 0.922)
  expect_equal(eq5d5l(c(MO=1,SC=1,UA=1,PD=2,AD=1), "England"), 0.937)
  expect_equal(eq5d5l(c(MO=1,SC=1,UA=2,PD=1,AD=1), "England"), 0.950)
  expect_equal(eq5d5l(c(MO=1,SC=2,UA=1,PD=1,AD=1), "England"), 0.950)
  expect_equal(eq5d5l(c(MO=2,SC=1,UA=1,PD=1,AD=1), "England"), 0.942)
  expect_equal(eq5d5l(c(MO=5,SC=5,UA=5,PD=5,AD=5), "England"), -0.285)
})

test_that("EQ-5D-5L Ethiopia gives correct answer", {
  expect_equal(eq5d5l(c(MO=1,SC=1,UA=1,PD=1,AD=1), "Ethiopia"), 1)
  expect_equal(eq5d5l(c(MO=1,SC=1,UA=1,PD=1,AD=2), "Ethiopia"), 0.974 )
  expect_equal(eq5d5l(c(MO=1,SC=1,UA=1,PD=2,AD=1), "Ethiopia"), 0.964)
  expect_equal(eq5d5l(c(MO=1,SC=1,UA=1,PD=2,AD=2), "Ethiopia"), 0.938)
  expect_equal(eq5d5l(c(MO=1,SC=1,UA=1,PD=1,AD=3), "Ethiopia"), 0.915)
  expect_equal(eq5d5l(c(MO=5,SC=5,UA=5,PD=5,AD=5), "Ethiopia"), -0.718)
})

test_that("EQ-5D-5L France gives correct answer", {
  expect_equal(eq5d5l(c(MO=1,SC=1,UA=1,PD=1,AD=1), "France"), 1)
  expect_equal(eq5d5l(c(MO=2,SC=2,UA=2,PD=2,AD=2), "France"), 0.850)
  expect_equal(eq5d5l(c(MO=3,SC=3,UA=3,PD=3,AD=3), "France"), 0.768)
  expect_equal(eq5d5l(c(MO=4,SC=4,UA=4,PD=4,AD=4), "France"), 0.028)
  expect_equal(eq5d5l(c(MO=5,SC=4,UA=3,PD=2,AD=1), "France"), 0.441)
  expect_equal(eq5d5l(c(MO=5,SC=5,UA=5,PD=5,AD=5), "France"), -0.525)
})

test_that("EQ-5D-5L Germany gives correct answer", {
  expect_equal(eq5d5l(c(MO=1,SC=1,UA=1,PD=1,AD=1), "Germany"), 1)
  expect_equal(eq5d5l(c(MO=2,SC=1,UA=1,PD=1,AD=1), "Germany"), 0.974)
  expect_equal(eq5d5l(c(MO=1,SC=2,UA=1,PD=1,AD=1), "Germany"), 0.950)
  expect_equal(eq5d5l(c(MO=1,SC=1,UA=2,PD=1,AD=1), "Germany"), 0.964)
  expect_equal(eq5d5l(c(MO=1,SC=1,UA=1,PD=2,AD=1), "Germany"), 0.943)
  expect_equal(eq5d5l(c(MO=1,SC=1,UA=1,PD=1,AD=2), "Germany"), 0.970)
  expect_equal(eq5d5l(c(MO=1,SC=2,UA=3,PD=4,AD=5), "Germany"), 0.141)
  expect_equal(eq5d5l(c(MO=5,SC=5,UA=5,PD=5,AD=5), "Germany"), -0.661)
})

test_that("EQ-5D-5L Ghana gives correct answer", {
  expect_equal(eq5d5l(c(MO=1,SC=1,UA=1,PD=1,AD=1), "Ghana"), 1)
  expect_equal(eq5d5l(c(MO=1,SC=1,UA=2,PD=1,AD=3), "Ghana"), 0.873)
  expect_equal(eq5d5l(c(MO=1,SC=1,UA=1,PD=1,AD=2), "Ghana"), 0.969, tolerance = .0011)
  expect_equal(eq5d5l(c(MO=2,SC=2,UA=2,PD=2,AD=2), "Ghana"), 0.768)
  expect_equal(eq5d5l(c(MO=3,SC=3,UA=3,PD=3,AD=3), "Ghana"), 0.583)
  expect_equal(eq5d5l(c(MO=4,SC=4,UA=4,PD=4,AD=4), "Ghana"), -0.087)
  expect_equal(eq5d5l(c(MO=5,SC=5,UA=5,PD=5,AD=5), "Ghana"), -0.493)
})

test_that("EQ-5D-5L Hong Kong gives correct answer", {
  expect_equal(eq5d5l(c(MO=1,SC=1,UA=1,PD=1,AD=1), "HongKong"), 1)
  expect_equal(eq5d5l(c(MO=2,SC=1,UA=1,PD=1,AD=1), "HongKong"), 0.891)
  expect_equal(eq5d5l(c(MO=1,SC=2,UA=1,PD=1,AD=1), "HongKong"), 0.913)
  expect_equal(eq5d5l(c(MO=1,SC=1,UA=2,PD=1,AD=1), "HongKong"), 0.933)
  expect_equal(eq5d5l(c(MO=1,SC=1,UA=1,PD=2,AD=1), "HongKong"), 0.924)
  expect_equal(eq5d5l(c(MO=1,SC=1,UA=1,PD=1,AD=2), "HongKong"), 0.920)
  expect_equal(eq5d5l(c(MO=5,SC=5,UA=5,PD=5,AD=5), "HongKong"), -0.865)
})

test_that("EQ-5D-5L Hungary gives correct answer", {
  expect_equal(eq5d5l(c(MO=1,SC=1,UA=1,PD=1,AD=1), "Hungary"), 1)
  expect_equal(eq5d5l(c(MO=2,SC=1,UA=1,PD=1,AD=1), "Hungary"), 0.965)
  expect_equal(eq5d5l(c(MO=1,SC=2,UA=1,PD=1,AD=1), "Hungary"), 0.955)
  expect_equal(eq5d5l(c(MO=1,SC=1,UA=2,PD=1,AD=1), "Hungary"), 0.965)
  expect_equal(eq5d5l(c(MO=1,SC=1,UA=1,PD=2,AD=1), "Hungary"), 0.957)
  expect_equal(eq5d5l(c(MO=1,SC=1,UA=1,PD=1,AD=2), "Hungary"), 0.960)
  expect_equal(eq5d5l(c(MO=1,SC=2,UA=3,PD=4,AD=5), "Hungary"), 0.242)
  expect_equal(eq5d5l(c(MO=3,SC=3,UA=3,PD=3,AD=3), "Hungary"), 0.571)
  expect_equal(eq5d5l(c(MO=4,SC=5,UA=5,PD=5,AD=5), "Hungary"), -0.656)
  expect_equal(eq5d5l(c(MO=5,SC=4,UA=5,PD=5,AD=5), "Hungary"), -0.723)
  expect_equal(eq5d5l(c(MO=5,SC=5,UA=4,PD=5,AD=5), "Hungary"), -0.789)
  expect_equal(eq5d5l(c(MO=5,SC=5,UA=5,PD=4,AD=5), "Hungary"), -0.725)
  expect_equal(eq5d5l(c(MO=5,SC=5,UA=5,PD=5,AD=4), "Hungary"), -0.769)
  expect_equal(eq5d5l(c(MO=5,SC=5,UA=5,PD=5,AD=5), "Hungary"), -0.848)
})

test_that("EQ-5D-5L India gives correct answer", {
  expect_equal(eq5d5l(c(MO=1,SC=1,UA=1,PD=1,AD=1), "India"), 1)
  expect_equal(eq5d5l(c(MO=1,SC=2,UA=1,PD=2,AD=1), "India"), 0.897)
  expect_equal(eq5d5l(c(MO=3,SC=1,UA=1,PD=1,AD=1), "India"), 0.901)
  expect_equal(eq5d5l(c(MO=4,SC=1,UA=1,PD=1,AD=1), "India"), 0.746)
  expect_equal(eq5d5l(c(MO=5,SC=1,UA=1,PD=1,AD=1), "India"), 0.613)
  expect_equal(eq5d5l(c(MO=1,SC=2,UA=3,PD=4,AD=5), "India"), 0.223)
  expect_equal(eq5d5l(c(MO=3,SC=4,UA=5,PD=2,AD=1), "India"), 0.224)
  expect_equal(eq5d5l(c(MO=5,SC=5,UA=5,PD=5,AD=5), "India"), -0.923)
  
})

test_that("EQ-5D-5L Indonesia gives correct answer", {
  expect_equal(eq5d5l(c(MO=1,SC=1,UA=1,PD=1,AD=1), "Indonesia"), 1)
  expect_equal(eq5d5l(c(MO=1,SC=1,UA=1,PD=1,AD=2), "Indonesia"), 0.921)
  expect_equal(eq5d5l(c(MO=2,SC=1,UA=1,PD=1,AD=1), "Indonesia"), 0.881)
  expect_equal(eq5d5l(c(MO=1,SC=2,UA=3,PD=4,AD=5), "Indonesia"), 0.240)
  expect_equal(eq5d5l(c(MO=5,SC=5,UA=5,PD=5,AD=5), "Indonesia"), -0.865)
})

test_that("EQ-5D-5L Iran gives correct answer", {
  expect_equal(eq5d5l(c(MO=1,SC=1,UA=1,PD=1,AD=1), "Iran"), 1)
  expect_equal(eq5d5l(c(MO=1,SC=2,UA=3,PD=4,AD=5), "Iran"), 0.001)
  expect_equal(eq5d5l(c(MO=2,SC=2,UA=2,PD=2,AD=2), "Iran"), 0.440)
  expect_equal(eq5d5l(c(MO=3,SC=3,UA=3,PD=3,AD=3), "Iran"), 0.033)
  expect_equal(eq5d5l(c(MO=4,SC=4,UA=4,PD=4,AD=4), "Iran"), -0.469)
  expect_equal(eq5d5l(c(MO=5,SC=5,UA=5,PD=5,AD=5), "Iran"),-1.190)
})

test_that("EQ-5D-5L Ireland gives correct answer", {
  expect_equal(eq5d5l(c(MO=1,SC=1,UA=1,PD=1,AD=1), "Ireland"), 1)
  expect_equal(eq5d5l(c(MO=1,SC=1,UA=2,PD=1,AD=1), "Ireland"), 0.951)
  expect_equal(eq5d5l(c(MO=2,SC=3,UA=2,PD=4,AD=3), "Ireland"), 0.225)
  expect_equal(eq5d5l(c(MO=5,SC=5,UA=5,PD=5,AD=5), "Ireland"), -0.974)
})

test_that("EQ-5D-5L Italy gives correct answer", {
  expect_equal(eq5d5l(c(MO=1,SC=1,UA=1,PD=1,AD=1), "Italy"), 1)
  expect_equal(eq5d5l(c(MO=1,SC=1,UA=2,PD=1,AD=1), "Italy"), 0.950)
  expect_equal(eq5d5l(c(MO=1,SC=2,UA=3,PD=4,AD=5), "Italy"), 0.215)
  expect_equal(eq5d5l(c(MO=5,SC=5,UA=5,PD=5,AD=5), "Italy"), -0.571)
})

test_that("EQ-5D-5L Japan cTTO gives correct answer", {
  expect_equal(eq5d5l(c(MO=1,SC=1,UA=1,PD=1,AD=1), "Japan_cTTO"), 1)
  expect_equal(eq5d5l(c(MO=3,SC=5,UA=3,PD=4,AD=1), "Japan_cTTO"), 0.444)
  expect_equal(eq5d5l(c(MO=1,SC=1,UA=1,PD=2,AD=1), "Japan_cTTO"), 0.895)
  expect_equal(eq5d5l(c(MO=2,SC=1,UA=2,PD=3,AD=2), "Japan_cTTO"), 0.685)
  expect_equal(eq5d5l(c(MO=5,SC=5,UA=5,PD=5,AD=5), "Japan_cTTO"), -0.025, tolerance = .0011)
})

test_that("EQ-5D-5L Japan DCE gives correct answer", {
  expect_equal(eq5d5l(c(MO=1,SC=1,UA=1,PD=1,AD=1), "Japan_DCE"), 1)
  expect_equal(eq5d5l(c(MO=3,SC=5,UA=3,PD=4,AD=1), "Japan_DCE"), 0.450)
  expect_equal(eq5d5l(c(MO=1,SC=2,UA=1,PD=1,AD=1), "Japan_DCE"), 0.900)
  expect_equal(eq5d5l(c(MO=2,SC=1,UA=2,PD=3,AD=2), "Japan_DCE"), 0.670)
  expect_equal(eq5d5l(c(MO=3,SC=3,UA=3,PD=3,AD=3), "Japan_DCE"), 0.485)
  expect_equal(eq5d5l(c(MO=4,SC=4,UA=4,PD=4,AD=4), "Japan_DCE"), 0.19)
  expect_equal(eq5d5l(c(MO=5,SC=5,UA=5,PD=5,AD=5), "Japan_DCE"), -0.025)
})

test_that("EQ-5D-5L South Korea gives correct answer", {
  expect_equal(eq5d5l(c(MO=1,SC=1,UA=1,PD=1,AD=1), "SouthKorea"), 1)
  expect_equal(eq5d5l(c(MO=1,SC=2,UA=3,PD=4,AD=5), "SouthKorea"), 0.440)
  expect_equal(eq5d5l(c(MO=1,SC=1,UA=2,PD=1,AD=1), "SouthKorea"), 0.883)
  expect_equal(eq5d5l(c(MO=5,SC=5,UA=5,PD=5,AD=5), "SouthKorea"), -0.066)
})

test_that("EQ-5D-5L Malaysia gives correct answer", {
  expect_equal(eq5d5l(c(MO=1,SC=1,UA=1,PD=1,AD=1), "Malaysia"), 1)
  expect_equal(eq5d5l(c(MO=1,SC=1,UA=2,PD=1,AD=1), "Malaysia"), 0.952)
  expect_equal(eq5d5l(c(MO=1,SC=2,UA=3,PD=4,AD=5), "Malaysia"), 0.314)
  expect_equal(eq5d5l(c(MO=5,SC=5,UA=5,PD=5,AD=5), "Malaysia"), -0.442)
})

test_that("EQ-5D-5L Mexico gives correct answer", {
  expect_equal(eq5d5l(c(MO=1,SC=1,UA=1,PD=1,AD=1), "Mexico"), 1)
  expect_equal(eq5d5l(c(MO=2,SC=1,UA=1,PD=1,AD=1), "Mexico"), 0.984)
  expect_equal(eq5d5l(c(MO=1,SC=2,UA=3,PD=4,AD=5), "Mexico"), 0.295)
  expect_equal(eq5d5l(c(MO=5,SC=5,UA=5,PD=5,AD=5), "Mexico"), -0.596)
})

test_that("EQ-5D-5L Morocco gives correct answer", {
  expect_equal(eq5d5l(c(MO=1,SC=1,UA=1,PD=1,AD=1), "Morocco"), 1)
  expect_equal(eq5d5l(c(MO=1,SC=1,UA=2,PD=1,AD=1), "Morocco"), 0.979)
  expect_equal(eq5d5l(c(MO=2,SC=1,UA=1,PD=1,AD=1), "Morocco"), 0.979)
  expect_equal(eq5d5l(c(MO=1,SC=2,UA=3,PD=4,AD=5), "Morocco"), -0.128)
  expect_equal(eq5d5l(c(MO=5,SC=5,UA=5,PD=5,AD=5), "Morocco"), -1.492, tolerance = .0011)
})

test_that("EQ-5D-5L Netherlands gives correct answer", {
  expect_equal(eq5d5l(c(MO=1,SC=1,UA=1,PD=1,AD=1), "Netherlands"), 1)
  expect_equal(eq5d5l(c(MO=2,SC=1,UA=1,PD=1,AD=1), "Netherlands"), 0.918)
  expect_equal(eq5d5l(c(MO=5,SC=5,UA=5,PD=1,AD=1), "Netherlands"), 0.390)
  expect_equal(eq5d5l(c(MO=4,SC=4,UA=4,PD=4,AD=4), "Netherlands"), -0.289)
  expect_equal(eq5d5l(c(MO=5,SC=5,UA=5,PD=5,AD=5), "Netherlands"), -0.446)
})

test_that("EQ-5D-5L NewZealand gives correct answer", {
  expect_equal(eq5d5l(c(MO=1,SC=1,UA=1,PD=1,AD=1), "NewZealand"), 1)
  expect_equal(eq5d5l(c(MO=2,SC=1,UA=1,PD=1,AD=1), "NewZealand"), 0.944)
  expect_equal(eq5d5l(c(MO=5,SC=5,UA=5,PD=1,AD=1), "NewZealand"), -0.060)
  expect_equal(eq5d5l(c(MO=2,SC=2,UA=2,PD=2,AD=2), "NewZealand"), 0.701)
  expect_equal(eq5d5l(c(MO=3,SC=3,UA=3,PD=3,AD=3), "NewZealand"), 0.321, tolerance = .0011)
  expect_equal(eq5d5l(c(MO=4,SC=4,UA=4,PD=4,AD=4), "NewZealand"), -0.201, tolerance = .0011)
  expect_equal(eq5d5l(c(MO=5,SC=5,UA=5,PD=5,AD=5), "NewZealand"), -0.830)
})

test_that("EQ-5D-5L Norway gives correct answer", {
  expect_equal(eq5d5l(c(MO=1,SC=1,UA=1,PD=1,AD=1), "Norway"), 1)
  expect_equal(eq5d5l(c(MO=1,SC=5,UA=4,PD=3,AD=2), "Norway"), 0.460)
  expect_equal(eq5d5l(c(MO=2,SC=2,UA=2,PD=2,AD=2), "Norway"), 0.779)
  expect_equal(eq5d5l(c(MO=3,SC=3,UA=3,PD=3,AD=3), "Norway"), 0.539)
  expect_equal(eq5d5l(c(MO=4,SC=4,UA=4,PD=4,AD=4), "Norway"), -0.126)
  expect_equal(eq5d5l(c(MO=5,SC=5,UA=5,PD=5,AD=5), "Norway"), -0.453)
})

test_that("EQ-5D-5L Peru cTTO gives correct answer", {
  expect_equal(eq5d5l(c(MO=1,SC=1,UA=1,PD=1,AD=1), "Peru_cTTO"), 1)
  expect_equal(eq5d5l(c(MO=2,SC=2,UA=2,PD=2,AD=2), "Peru_cTTO"), 0.441)
  expect_equal(eq5d5l(c(MO=3,SC=3,UA=3,PD=3,AD=3), "Peru_cTTO"), 0.146)
  expect_equal(eq5d5l(c(MO=4,SC=4,UA=4,PD=4,AD=4), "Peru_cTTO"), -0.285)
  expect_equal(eq5d5l(c(MO=5,SC=5,UA=5,PD=5,AD=5), "Peru_cTTO"), -1.076)
})

test_that("EQ-5D-5L Peru DCE gives correct answer", {
  expect_equal(eq5d5l(c(MO=1,SC=1,UA=1,PD=1,AD=1), "Peru_DCE"), 1)
  expect_equal(eq5d5l(c(MO=2,SC=2,UA=2,PD=2,AD=2), "Peru_DCE"), 0.810)
  expect_equal(eq5d5l(c(MO=3,SC=3,UA=3,PD=3,AD=3), "Peru_DCE"), 0.640)
  expect_equal(eq5d5l(c(MO=4,SC=4,UA=4,PD=4,AD=4), "Peru_DCE"), 0.299)
  expect_equal(eq5d5l(c(MO=5,SC=5,UA=5,PD=5,AD=5), "Peru_DCE"), -0.213)
})

test_that("EQ-5D-5L Philippines gives correct answer", {
  expect_equal(eq5d5l(c(MO=1,SC=1,UA=1,PD=1,AD=1), "Philippines"), 1)
  expect_equal(eq5d5l(c(MO=1,SC=2,UA=3,PD=4,AD=5), "Philippines"), 0.442)
  expect_equal(eq5d5l(c(MO=2,SC=2,UA=2,PD=2,AD=2), "Philippines"), 0.804)
  expect_equal(eq5d5l(c(MO=3,SC=3,UA=3,PD=3,AD=3), "Philippines"), 0.755)
  expect_equal(eq5d5l(c(MO=4,SC=4,UA=4,PD=4,AD=4), "Philippines"), -0.023)
  expect_equal(eq5d5l(c(MO=5,SC=5,UA=5,PD=5,AD=5), "Philippines"), -0.438)
})

test_that("EQ-5D-5L Poland gives correct answer", {
  expect_equal(eq5d5l(c(MO=1,SC=1,UA=1,PD=1,AD=1), "Poland"), 1)
  expect_equal(eq5d5l(c(MO=1,SC=1,UA=1,PD=1,AD=2), "Poland"), 0.982)
  expect_equal(eq5d5l(c(MO=1,SC=2,UA=3,PD=4,AD=5), "Poland"), 0.436)
  expect_equal(eq5d5l(c(MO=2,SC=2,UA=2,PD=2,AD=2), "Poland"), 0.873)
  expect_equal(eq5d5l(c(MO=3,SC=3,UA=3,PD=3,AD=3), "Poland"), 0.800)
  expect_equal(eq5d5l(c(MO=4,SC=4,UA=4,PD=4,AD=4), "Poland"), 0.296, tolerance = 0.0011)
  expect_equal(eq5d5l(c(MO=5,SC=5,UA=5,PD=5,AD=5), "Poland"), -0.590)
})

test_that("EQ-5D-5L Portugal gives correct answer", {
  expect_equal(eq5d5l(c(MO=1,SC=1,UA=1,PD=1,AD=1), "Portugal"), 1)
  expect_equal(eq5d5l(c(MO=1,SC=1,UA=1,PD=1,AD=2), "Portugal"), 0.964)
  expect_equal(eq5d5l(c(MO=1,SC=1,UA=1,PD=2,AD=1), "Portugal"), 0.959)
  expect_equal(eq5d5l(c(MO=1,SC=4,UA=2,PD=3,AD=5), "Portugal"), 0.415)
  expect_equal(eq5d5l(c(MO=3,SC=3,UA=3,PD=3,AD=3), "Portugal"), 0.589)
  expect_equal(eq5d5l(c(MO=4,SC=4,UA=4,PD=4,AD=4), "Portugal"), 0.061)
  expect_equal(eq5d5l(c(MO=5,SC=5,UA=5,PD=5,AD=5), "Portugal"), -0.603)
})

test_that("EQ-5D-5L Romania gives correct answer", {
  expect_equal(eq5d5l(c(MO=1,SC=1,UA=1,PD=1,AD=1), "Romania"), 1)
  expect_equal(eq5d5l(c(MO=1,SC=1,UA=1,PD=1,AD=2), "Romania"), 0.962)
  expect_equal(eq5d5l(c(MO=1,SC=2,UA=3,PD=4,AD=5), "Romania"), 0.520)
  expect_equal(eq5d5l(c(MO=2,SC=2,UA=2,PD=2,AD=2), "Romania"), 0.783)
  expect_equal(eq5d5l(c(MO=3,SC=3,UA=3,PD=3,AD=3), "Romania"), 0.698)
  expect_equal(eq5d5l(c(MO=4,SC=4,UA=4,PD=4,AD=4), "Romania"), 0.418)
  expect_equal(eq5d5l(c(MO=5,SC=5,UA=5,PD=5,AD=5), "Romania"), -0.323)
})

test_that("EQ-5D-5L Saudi Arabia gives correct answer", {
  expect_equal(eq5d5l(c(MO=1,SC=1,UA=1,PD=1,AD=1), "SaudiArabia"), 1)
  expect_equal(eq5d5l(c(MO=2,SC=2,UA=2,PD=2,AD=2), "SaudiArabia"), 0.813)
  expect_equal(eq5d5l(c(MO=3,SC=3,UA=3,PD=3,AD=3), "SaudiArabia"), 0.614)
  expect_equal(eq5d5l(c(MO=4,SC=4,UA=4,PD=4,AD=4), "SaudiArabia"), -0.127)
  expect_equal(eq5d5l(c(MO=5,SC=5,UA=5,PD=5,AD=5), "SaudiArabia"), -0.683)
  expect_equal(eq5d5l(c(MO=4,SC=2,UA=3,PD=1,AD=5), "SaudiArabia"), 0.338)
  
})

test_that("EQ-5D-5L Slovenia gives correct answer", {
  expect_equal(eq5d5l(c(MO=1,SC=1,UA=1,PD=1,AD=1), "Slovenia"), 1)
  expect_equal(eq5d5l(c(MO=2,SC=2,UA=2,PD=2,AD=2), "Slovenia"), 0.764)
  expect_equal(eq5d5l(c(MO=3,SC=3,UA=3,PD=3,AD=3), "Slovenia"), 0.505)
  expect_equal(eq5d5l(c(MO=4,SC=4,UA=4,PD=4,AD=4), "Slovenia"), -0.360)
  expect_equal(eq5d5l(c(MO=5,SC=5,UA=5,PD=5,AD=5), "Slovenia"), -1.090, tolerance = .0011)
})

test_that("EQ-5D-5L Spain gives correct answer", {
  expect_equal(eq5d5l(c(MO=1,SC=1,UA=1,PD=1,AD=1), "Spain"), 1)
  expect_equal(eq5d5l(c(MO=2,SC=1,UA=1,PD=1,AD=1), "Spain"), 0.916)
  expect_equal(eq5d5l(c(MO=1,SC=2,UA=1,PD=1,AD=1), "Spain"), 0.95)
  expect_equal(eq5d5l(c(MO=1,SC=1,UA=2,PD=1,AD=1), "Spain"), 0.956)
  expect_equal(eq5d5l(c(MO=1,SC=1,UA=1,PD=2,AD=1), "Spain"), 0.922)
  expect_equal(eq5d5l(c(MO=1,SC=1,UA=1,PD=1,AD=2), "Spain"), 0.919)
  expect_equal(eq5d5l(c(MO=5,SC=5,UA=5,PD=5,AD=5), "Spain"), -0.416)
})

test_that("EQ-5D-5L Sweden (2020) gives correct answer", {
  expect_equal(eq5d5l(c(MO=1,SC=1,UA=1,PD=1,AD=1), "Sweden_2020"), 0.975, tolerance = .0011)
  expect_equal(eq5d5l(c(MO=1,SC=2,UA=3,PD=4,AD=5), "Sweden_2020"), 0.497)
  expect_equal(eq5d5l(c(MO=2,SC=2,UA=3,PD=3,AD=5), "Sweden_2020"), 0.528)
  expect_equal(eq5d5l(c(MO=3,SC=4,UA=5,PD=4,AD=3), "Sweden_2020"), 0.503)
  expect_equal(eq5d5l(c(MO=5,SC=5,UA=5,PD=5,AD=5), "Sweden_2020"), 0.243)
})

test_that("EQ-5D-5L Sweden (2022) gives correct answer", {
  expect_equal(eq5d5l(c(MO=1,SC=1,UA=1,PD=1,AD=1), "Sweden_2022"), 1)
  expect_equal(eq5d5l(c(MO=2,SC=2,UA=2,PD=2,AD=2), "Sweden_2022"), 0.896)
  expect_equal(eq5d5l(c(MO=3,SC=3,UA=3,PD=3,AD=3), "Sweden_2022"), 0.701)
  expect_equal(eq5d5l(c(MO=4,SC=4,UA=4,PD=4,AD=4), "Sweden_2022"), 0.073)
  expect_equal(eq5d5l(c(MO=1,SC=2,UA=3,PD=4,AD=5), "Sweden_2022"), 0.220)
  expect_equal(eq5d5l(c(MO=5,SC=5,UA=5,PD=5,AD=5), "Sweden_2022"), -0.314)
})

test_that("EQ-5D-5L Taiwan gives correct answer", {
  expect_equal(eq5d5l(c(MO=1,SC=1,UA=1,PD=1,AD=1), "Taiwan"), 1)
  expect_equal(eq5d5l(c(MO=1,SC=2,UA=3,PD=4,AD=5), "Taiwan"), 0.039, tolerance = .0011)
  expect_equal(eq5d5l(c(MO=4,SC=2,UA=1,PD=1,AD=4), "Taiwan"), 0.219)
  expect_equal(eq5d5l(c(MO=5,SC=5,UA=5,PD=4,AD=5), "Taiwan"), -0.913)
  expect_equal(eq5d5l(c(MO=5,SC=5,UA=4,PD=4,AD=4), "Taiwan"), -0.761)
  expect_equal(eq5d5l(c(MO=5,SC=5,UA=5,PD=4,AD=5), "Taiwan"), -0.913)
  expect_equal(eq5d5l(c(MO=5,SC=5,UA=5,PD=5,AD=5), "Taiwan"), -1.026)
})

test_that("EQ-5D-5L Thailand gives correct answer", {
  expect_equal(eq5d5l(c(MO=1,SC=1,UA=1,PD=1,AD=1), "Thailand"), 1)
  expect_equal(eq5d5l(c(MO=1,SC=1,UA=1,PD=2,AD=1), "Thailand"), 0.944)
  expect_equal(eq5d5l(c(MO=3,SC=1,UA=2,PD=4,AD=5), "Thailand"), 0.353)
  expect_equal(eq5d5l(c(MO=5,SC=5,UA=5,PD=5,AD=5), "Thailand"), -0.421)
})

test_that("EQ-5D-5L Trinidad and Tobago gives correct answer", {
  expect_equal(eq5d5l(c(MO=1,SC=1,UA=1,PD=1,AD=1), "Trinidad_and_Tobago"), 1)
  expect_equal(eq5d5l(c(MO=1,SC=1,UA=1,PD=5,AD=1), "Trinidad_and_Tobago"), 0.52)
  expect_equal(eq5d5l(c(MO=1,SC=1,UA=5,PD=1,AD=1), "Trinidad_and_Tobago"), 0.781)
  expect_equal(eq5d5l(c(MO=2,SC=1,UA=3,PD=5,AD=4), "Trinidad_and_Tobago"), 0.267)
  expect_equal(eq5d5l(c(MO=5,SC=5,UA=5,PD=5,AD=5), "Trinidad_and_Tobago"), -0.563)
})

test_that("EQ-5D-5L United Arab Emirates gives correct answer", {
  expect_equal(eq5d5l(c(MO=1,SC=1,UA=1,PD=1,AD=1), "UAE"), 1)
  expect_equal(eq5d5l(c(MO=1,SC=1,UA=2,PD=1,AD=1), "UAE"), 0.962)
  expect_equal(eq5d5l(c(MO=2,SC=2,UA=2,PD=2,AD=2), "UAE"), 0.721)
  expect_equal(eq5d5l(c(MO=3,SC=3,UA=3,PD=3,AD=3), "UAE"), 0.555)
  expect_equal(eq5d5l(c(MO=3,SC=2,UA=2,PD=3,AD=4), "UAE"), 0.465)
  expect_equal(eq5d5l(c(MO=4,SC=4,UA=4,PD=4,AD=4), "UAE"), -0.145)
  expect_equal(eq5d5l(c(MO=5,SC=5,UA=5,PD=5,AD=5), "UAE"), -0.654)
})

test_that("EQ-5D-5L Uganda gives correct answer", {
  expect_equal(eq5d5l(c(MO=1,SC=1,UA=1,PD=1,AD=1), "Uganda"), 1)
  expect_equal(eq5d5l(c(MO=1,SC=1,UA=1,PD=1,AD=2), "Uganda"), 0.950)
  expect_equal(eq5d5l(c(MO=2,SC=3,UA=5,PD=1,AD=4), "Uganda"), 0.276)
  expect_equal(eq5d5l(c(MO=5,SC=5,UA=5,PD=5,AD=5), "Uganda"), -1.116)
})

test_that("EQ-5D-5L Uruguay gives correct answer", {
  expect_equal(eq5d5l(c(MO=1,SC=1,UA=1,PD=1,AD=1), "Uruguay"), 1)
  expect_equal(eq5d5l(c(MO=1,SC=1,UA=1,PD=1,AD=2), "Uruguay"), 0.978)
  expect_equal(eq5d5l(c(MO=3,SC=1,UA=4,PD=1,AD=2), "Uruguay"), 0.827)
  expect_equal(eq5d5l(c(MO=2,SC=5,UA=4,PD=1,AD=3), "Uruguay"), 0.538)
  expect_equal(eq5d5l(c(MO=4,SC=4,UA=4,PD=4,AD=4), "Uruguay"), 0.353)
  expect_equal(eq5d5l(c(MO=5,SC=5,UA=5,PD=1,AD=1), "Uruguay"), 0.184)
  expect_equal(eq5d5l(c(MO=5,SC=5,UA=5,PD=5,AD=5), "Uruguay"), -0.264)
})

test_that("EQ-5D-5L USA gives correct answer", {
  expect_equal(eq5d5l(c(MO=1,SC=1,UA=1,PD=1,AD=1), "USA"), 1)
  expect_equal(eq5d5l(c(MO=2,SC=1,UA=1,PD=1,AD=1), "USA"), 0.904)
  expect_equal(eq5d5l(c(MO=1,SC=2,UA=1,PD=1,AD=1), "USA"), 0.911)
  expect_equal(eq5d5l(c(MO=1,SC=1,UA=2,PD=1,AD=1), "USA"), 0.932)
  expect_equal(eq5d5l(c(MO=1,SC=1,UA=1,PD=2,AD=1), "USA"), 0.940)
  expect_equal(eq5d5l(c(MO=1,SC=1,UA=1,PD=1,AD=2), "USA"), 0.943)
  expect_equal(eq5d5l(c(MO=2,SC=1,UA=3,PD=5,AD=4), "USA"), 0.090)
  expect_equal(eq5d5l(c(MO=5,SC=5,UA=5,PD=5,AD=5), "USA"), -0.573)
})

test_that("EQ-5D-5L Vietnam gives correct answer", {
  expect_equal(eq5d5l(c(MO=1,SC=1,UA=1,PD=1,AD=1), "Vietnam"), 1)
  expect_equal(eq5d5l(c(MO=2,SC=1,UA=1,PD=1,AD=1), "Vietnam"), 0.931)
  expect_equal(eq5d5l(c(MO=1,SC=2,UA=1,PD=1,AD=1), "Vietnam"), 0.957)
  expect_equal(eq5d5l(c(MO=1,SC=1,UA=2,PD=1,AD=1), "Vietnam"), 0.954)
  expect_equal(eq5d5l(c(MO=1,SC=1,UA=1,PD=2,AD=1), "Vietnam"), 0.916)
  expect_equal(eq5d5l(c(MO=1,SC=1,UA=1,PD=1,AD=2), "Vietnam"), 0.936)
  expect_equal(eq5d5l(c(MO=1,SC=2,UA=3,PD=4,AD=5), "Vietnam"), 0.390)
  expect_equal(eq5d5l(c(MO=5,SC=5,UA=5,PD=5,AD=5), "Vietnam"), -0.511)
})

test_that("EQ-5D-5L WePP gives correct answer", {
  expect_equal(eq5d5l(c(MO=1,SC=1,UA=1,PD=1,AD=1), "WePP"), 1)
  expect_equal(eq5d5l(c(MO=2,SC=2,UA=2,PD=2,AD=2), "WePP"), 0.74)
  expect_equal(eq5d5l(c(MO=3,SC=3,UA=3,PD=3,AD=3), "WePP"), 0.57)
  expect_equal(eq5d5l(c(MO=4,SC=4,UA=4,PD=4,AD=4), "WePP"), -0.03)
  expect_equal(eq5d5l(c(MO=5,SC=5,UA=5,PD=5,AD=5), "WePP"), -0.24)
})

context("EQ-5D-5L Incorrect params")

test_that("EQ-5D-5L throws error for incorrect parameters", {
  expect_error(eq5d5l(c(MD=5,SC=5,UA=5,PD=5,AD=5), "China"))
  expect_error(eq5d5l(c(MO=1,SC=7,UA=1,PD=1,AD=1), "Canada"))
  expect_error(eq5d5l(c(MO=1,SC=2,UA=3,PD=4,AD=5), "Swaziland"))
  expect_error(eq5d5l(c(3, 4, 5, 4, 3), "Sweden"))
})
