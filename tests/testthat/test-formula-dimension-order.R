test_that("formula dimensions retain their named meaning", {
  dat <- data.frame(id=c(1,1), visit=c("pre","post"),
                    MO=c(3,1), SC=c(1,3), UA=1, PD=1, AD=1)
  canonical <- MO + SC + UA + PD + AD ~ visit | id
  formulas <- list(canonical, SC + MO + UA + PD + AD ~ visit | id,
                   cbind(AD, PD, UA, SC, MO) ~ visit | id)
  reference_hpg <- hpg(canonical, data=dat, version="3L", country="UK", type="TTO")
  for (f in formulas) {
    # One mobility improvement, one self-care worsening, three ties.
    expect_equal(unlist(ps(f, data=dat, version="3L")),
                 c(MO=1, SC=0, UA=.5, PD=.5, AD=.5))
    out <- pchc(f, data=dat, version="3L", by.dimension=TRUE,
                summary=FALSE, totals=FALSE)
    expect_equal(out$MO, "Improve")
    expect_equal(out$SC, "Worsen")
    expect_equal(hpg(f, data=dat, version="3L", country="UK", type="TTO"),
                 reference_hpg)
  }

  names(dat)[3:7] <- c("mob", "self", "use", "pain", "anx")
  mapping <- c(SC="self", MO="mob", AD="anx", UA="use", PD="pain")
  f <- mob + self + use + pain + anx ~ visit | id
  expect_equal(unlist(ps(f, data=dat, version="3L", dimensions=mapping)),
               c(MO=1, SC=0, UA=.5, PD=.5, AD=.5))
  out <- pchc(f, data=dat, version="3L", dimensions=mapping,
              by.dimension=TRUE, summary=FALSE, totals=FALSE)
  expect_equal(out$MO, "Improve")
  expect_equal(out$SC, "Worsen")
  expect_equal(hpg(f, data=dat, version="3L", country="UK", type="TTO",
                   dimensions=mapping), reference_hpg)
})
