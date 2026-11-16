test_that("lookup keys use dimension names, not vector order", {
  s5 <- c(MO=1, SC=1, UA=1, PD=5, AD=1)
  s3 <- c(MO=1, SC=1, UA=1, PD=3, AD=1)
  au <- c(MO=2, SC=3, UA=3, PD=1, AD=1)
  corrected <- c(MO=1, SC=2, UA=1, PD=3, AD=3)
  orders <- list(names(s5), sort(names(s5)), rev(names(s5)))
  for (nm in orders) {
    # Fixed entries for 11151/11131 in CW, RCW, RCWVH and DSU tables.
    expect_equal(eq5dcw(s5[nm], "UK"), 0.264)
    expect_equal(eq5drcw(s3[nm], "England", method="EQ"), 0.697)
    expect_equal(eq5drcw(s3[nm], "UK", method="VH"), 0.712)
    expect_equal(eq5dmap(s5[nm], "England_2018", "5L", 50, "female"), 0.833)
    expect_equal(eq5dmap(s3[nm], "UK_2026", "3L", 50, "female"), 0.903)
    # Australia 23311: 1 - .105 - .068 - .267 - .139 - .055.
    expect_equal(eq5d3l(au[nm], country="Australia"), 0.366)
    expect_equal(eq5d3l(corrected[nm], country="Australia"), 0.154)

    expect_equal(eq5d(s5[nm], "5L", "CW", "UK"), 0.264)
    expect_equal(eq5d(s3[nm], "3L", "RCW", "UK"), 0.712)
    expect_equal(eq5d(s3[nm], "3L", "RCW", "England", method="EQ"), 0.697)
    expect_equal(eq5d(s5[nm], "5L", "DSU", "England_2018",
                      age=50, sex="female"), 0.833)
    expect_equal(eq5d(s3[nm], "3L", "DSU", "UK_2026",
                      age=50, sex="female"), 0.903)
    expect_equal(eq5d(au[nm], "3L", "TTO", "Australia"), 0.366)
  }
})
