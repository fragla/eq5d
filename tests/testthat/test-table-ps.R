test_that("table_ps formats ungrouped ps output", {
  
  ps_out <- c(MO = 0.61, SC = 0.54, UA = 0.58, PD = 0.49, AD = 0.63)
  
  tab <- table_ps(ps_out)
  
  expect_true(is.data.frame(tab))
  expect_true(all(c("Dimension", "PS") %in% names(tab)))
  expect_equal(tab$PS, unname(round(ps_out, 2)))
})


test_that("table_ps formats grouped ps output", {
  
  ps_out <- list(
    Group1 = c(MO = 0.61, SC = 0.54, UA = 0.58),
    Group2 = c(MO = 0.66, SC = 0.59, UA = 0.60)
  )
  
  tab <- table_ps(ps_out)
  
  expect_type(tab, "list")
  expect_true(all(vapply(tab, is.data.frame, logical(1))))
})
