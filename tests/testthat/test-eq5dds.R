context("EQ-5D-DS")

test1.df <- data.frame(MO=c(1,2,3,2,1,2), SC=c(2,3,2,1,2,3), UA=c(3,2,1,2,3,1), PD=c(1,1,2,3,2,1), AD=c(2,2,3,3,1,1), Sex=c("M","F","M","F","M","F"))
test2.df <- data.frame(MO=c(1,2,3,NA,1,2), SC=c(2,3,2,1,2,3), UA=c("F",2,1,2,3,1), PD=c(1,1,2,3,2,1), AD=c(2,2,3,3,1,1), Sex=c("M","F","M","F","M","F"))
test3.df <- data.frame(Mob=c(1,2,3,2,1,2), SCa=c(2,3,2,1,2,3), UAc=c(3,2,1,2,3,1), PaD=c(1,1,2,3,2,1), AaD=c(2,2,3,3,1,1), Sex=c("M","F","M","F","M","F"))
test4.df <- data.frame(State=eq5d::get_health_states_from_dimensions(test1.df), Sex=test1.df$Sex)
test5.df <- data.frame(Profile=eq5d::get_health_states_from_dimensions(test1.df), Sex=test1.df$Sex)
  
res1.df <- data.frame(MO=c(33.3,50.0,16.7),SC=c(16.7,50.0,33.3),UA=c(33.3,33.3,33.3),PD=c(50.0,33.3,16.7),AD=c(33.3,33.3,33.3))
res2.df <- data.frame(MO=c(2,3,1),SC=c(1,3,2),UA=c(2,2,2),PD=c(3,2,1),AD=c(2,2,2))
res3.df <- data.frame(MO=c(25,50,25),SC=c(0,50,50),UA=c(50,25,25),PD=c(50,50,0),AD=c(50,25,25))
res4.df <- data.frame(MO=c(1,2,1),SC=c(0,2,2),UA=c(2,1,1),PD=c(2,2,0),AD=c(2,1,1))
res5.df <- list(M=data.frame(MO=c(66.7,0,33.3),SC=c(0,100,0),UA=c(33.3,0,66.7),PD=c(33.3,66.7,0),AD=c(33.3,33.3,33.3)),
                F=data.frame(MO=c(0,100,0),SC=c(33.3,0,66.7),UA=c(33.3,66.7,0),PD=c(66.7,0,33.3),AD=c(33.3,33.3,33.3)))
res6.df <- list(M=data.frame(MO=c(2,0,1),SC=c(0,3,0),UA=c(1,0,2),PD=c(1,2,0),AD=c(1,1,1)),
                 F=data.frame(MO=c(0,3,0),SC=c(1,0,2),UA=c(1,2,0),PD=c(2,0,1),AD=c(1,1,1)))
test_that("eq5dds returns correct answer", {
  expect_equal(eq5dds(test1.df, version="3L"), res1.df)
  expect_equal(eq5dds(test1.df, version="3L", counts=TRUE), res2.df)
  expect_equal(eq5dds(test2.df, version="3L"), res3.df)
  expect_equal(eq5dds(test2.df, version="3L", counts=TRUE), res4.df)
  expect_equal(eq5dds(test1.df, version="3L", by="Sex")$M, res5.df$M)
  expect_equal(eq5dds(test1.df, version="3L", by="Sex")$F, res5.df$F)
  expect_equal(eq5dds(test4.df, version="3L", by="Sex")$M, res5.df$M)
  expect_equal(eq5dds(test4.df, version="3L", by="Sex")$F, res5.df$F)
  expect_equal(eq5dds(test4.df$State, version="3L"), res1.df)
  expect_equal(eq5dds(test5.df, version="3L", by="Sex", five.digit="Profile")$M, res5.df$M)
  expect_equal(eq5dds(test5.df, version="3L", by="Sex", five.digit="Profile")$F, res5.df$F)
  expect_equal(eq5dds(test1.df, version="3L", counts=TRUE, by="Sex")$M, res6.df$M)
  expect_equal(eq5dds(test1.df, version="3L", counts=TRUE, by="Sex")$F, res6.df$F)
  expect_equal(eq5dds(test3.df, version="3L", dimensions=c("Mob","SCa","UAc","PaD","AaD")), res1.df)
  expect_equal(eq5dds(test1.df, version="Y3L"), res1.df)
  expect_equal(eq5dds(test1.df, version="Y3L", counts=TRUE), res2.df)
})

test_that("eq5dds throws error", {
  expect_error(eq5dds(test1.df, version="3L", by="Gender"))
  expect_error(eq5dds(test1.df, version="7L"))
  expect_error(eq5dds(test1.df[c("MO","SC","UA","PD")], version="3L"))
})

test_that("eq5dds using version='Y' is deprecated", {
  rlang::local_options(lifecycle_verbosity = "error")
  expect_error(eq5dds(test1.df, version="Y"))
})

test_that("eq5dds using version='Y' still works", {
  rlang::local_options(lifecycle_verbosity = "quiet")
  expect_equal(eq5dds(test1.df, version="Y"), res1.df)
})
