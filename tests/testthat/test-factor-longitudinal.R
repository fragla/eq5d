test_that("long-form alignment retains factor response labels", {
  dat <- data.frame(id=c(1,2,1,2), visit=c("pre","pre","post","post"))
  for (nm in c("MO", "SC", "UA", "PD", "AD")) {
    dat[[nm]] <- factor(c(3,2,1,2), levels=3:1)
  }
  f <- MO + SC + UA + PD + AD ~ visit | id
  # Subject 1 improves; subject 2 has unchanged problems.
  expect_equal(pchc(f, data=dat, version="3L", summary=FALSE, totals=FALSE),
               c("Improve", "No change"))
  expect_equal(unlist(ps(f, data=dat, version="3L")),
               c(MO=.75, SC=.75, UA=.75, PD=.75, AD=.75))
  expect_equal(hpg(f, data=dat, version="3L", country="UK", type="TTO"),
               data.frame(Pre=c(243L,33L), Post=c(1L,33L),
                          PCHC=c("Improve", "No change")))

  expect_equal(pchc(f, data=dat[-4,], version="3L", summary=FALSE, totals=FALSE),
               c("Improve", NA_character_))
  expect_equal(unlist(ps(f, data=dat[-4,], version="3L")),
               c(MO=1, SC=1, UA=1, PD=1, AD=1))

  unchanged <- dat[c(2,4),]
  unchanged[3:7] <- lapply(unchanged[3:7], function(x) ordered(as.character(x)))
  expect_equal(pchc(f, data=unchanged, version="3L", summary=FALSE, totals=FALSE),
               "No change")
})

test_that("factor profile columns and missing visits retain their labels", {
  dat <- data.frame(id=c(1,2,1), visit=c("pre","pre","post"),
                    profile=factor(c("33333","22222","11111")))
  expect_equal(pchc(profile ~ visit | id, data=dat, version="3L",
                    summary=FALSE, totals=FALSE), c("Improve", NA_character_))
})
