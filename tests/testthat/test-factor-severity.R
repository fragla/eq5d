test_that("severity scores use factor labels rather than storage codes", {
  dims <- c("MO", "SC", "UA", "PD", "AD")
  for (ordered in c(FALSE, TRUE)) {
    for (levels in list(c(2,3), c(3,2,1))) {
      response <- factor(c(2,3,NA), levels=levels, ordered=ordered)
      dat <- as.data.frame(setNames(rep(list(response), 5), dims))
      expect_equal(lss(dat, version="3L", ignore.invalid=TRUE), c(10L,15L,NA_integer_))
      expect_equal(lfs(dat, version="3L", ignore.invalid=TRUE), c("050","005",NA_character_))
      expect_equal(lss(dat[1:2,], version="Y3L"), c(10L,15L))
      expect_equal(lfs(dat[1:2,], version="5L"), c("05000","00500"))
    }
  }
  invalid <- as.data.frame(setNames(rep(list(factor("9")), 5), dims))
  expect_error(lss(invalid, version="3L"), "Invalid dimension state")
  expect_error(lfs(invalid, version="3L"), "Invalid dimension state")
  expect_equal(lss(data.frame(State=factor(c("22222","33333"))), version="3L"),
               c(10L,15L))
})
