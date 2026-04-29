test_that("table_ps formats ungrouped ps output", {
  
  ps_out <- list(MO = 0.61, SC = 0.54, UA = 0.58, PD = 0.49, AD = 0.63)
  
  tab <- table_ps(ps_out)
  
  expect_true(is.data.frame(tab))
  expect_named(tab, c("Dimension", "PS"))
  
  expect_equal(tab$PS, unname(unlist(ps_out)))
  expect_equal(tab$Dimension, names(ps_out))
})


test_that("table_ps formats grouped ps output", {
  
  ps_out <- list(
    Group1 = list(MO = 0.61, SC = 0.54, UA = 0.58),
    Group2 = list(MO = 0.66, SC = 0.59, UA = 0.60)
  )
  
  tab <- table_ps(ps_out)
  
  expect_type(tab, "list")
  expect_named(tab, c("Group1", "Group2"))
  
  expect_true(all(vapply(tab, is.data.frame, logical(1))))
  expect_true(all(vapply(tab, function(x) {
    identical(names(x), c("Dimension", "PS"))
  }, logical(1))))
})
