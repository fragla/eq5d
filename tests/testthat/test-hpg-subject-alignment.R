test_that("HPG ranks and classifications use the same subject order", {
  dat <- data.frame(id=c(20,10,10,20), visit=c("pre","pre","post","post"),
                    profile=c(33333,11111,33333,11111))
  out <- hpg(profile ~ visit | id, data=dat, country="UK", version="3L",
             type="TTO", pre.level="pre", post.level="post")
  # ID 20 improves in every dimension; ID 10 worsens in every dimension.
  expect_equal(out, data.frame(Pre=c(243L,1L), Post=c(1L,243L),
                               PCHC=c("Improve","Worsen")))
  # Preserve factor labels during rank preparation; factor PCHC is a separate fix.
  canonical <- c("MO", "SC", "UA", "PD", "AD")
  dat[canonical] <- rep(list(factor(c(3,1,3,1), levels=3:1)), 5)
  out <- hpg(MO + SC + UA + PD + AD ~ visit | id, data=dat,
             country="UK", version="3L", type="TTO")
  expect_equal(out$Pre, c(243L,1L))
  expect_equal(out$Post, c(1L,243L))
})

test_that("HPG does not recycle ranks across missing visits", {
  dat <- data.frame(id=c(10,20,10), visit=c("pre","pre","post"),
                    profile=c(33333,22222,11111))
  out <- hpg(profile ~ visit | id, data=dat, country="UK", version="3L",
             type="TTO", pre.level="pre", post.level="post")
  expect_equal(out, data.frame(Pre=c(243L,33L), Post=c(1L,NA_integer_),
                               PCHC=c("Improve",NA_character_)))
  # A subject observed only at follow-up must also retain a missing baseline.
  dat <- rbind(dat, data.frame(id=30, visit="post", profile=33333))
  out <- hpg(profile ~ visit | id, data=dat, country="UK", version="3L",
             type="TTO", pre.level="pre", post.level="post")
  expect_equal(out, data.frame(Pre=c(243L,33L,NA_integer_),
                               Post=c(1L,NA_integer_,243L),
                               PCHC=c("Improve",NA_character_,NA_character_)))
})
