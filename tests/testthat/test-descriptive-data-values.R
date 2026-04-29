context("descriptive_data specific values")

test_that("descriptive_data computes correct counts", {
  
  test_data <- data.frame(
    MO = c(1, 1, 2, 3),
    SC = c(1, 2, 2, 3),
    UA = c(1, 1, 1, 1),
    PD = c(3, 3, 2, 2),
    AD = c(2, 2, 2, 2)
  )
  
  dd <- descriptive_data(
    test_data,
    version = "3L",
    metric  = "count"
  )
  
  mo <- dd[dd$Dimension == "MO", ]
  
  expect_equal(mo$Value, c(2, 1, 1))
})


test_that("descriptive_data computes correct percentages", {
  
  test_data <- data.frame(
    MO = c(1, 1, 2, 3),
    SC = c(1, 2, 2, 3),
    UA = c(1, 1, 1, 1),
    PD = c(3, 3, 2, 2),
    AD = c(2, 2, 2, 2)
  )
  
  dd <- descriptive_data(
    test_data,
    version = "3L",
    metric  = "percent"
  )
  
  mo <- dd[dd$Dimension == "MO", ]
  
  expect_equal(mo$Value, c(50, 25, 25))
})


test_that("descriptive_data computes correct grouped values", {
  
  test_data <- data.frame(
    MO = c(1, 1, 2, 3),
    SC = c(1, 2, 2, 3),
    UA = c(1, 1, 1, 1),
    PD = c(3, 3, 2, 2),
    AD = c(2, 2, 2, 2),
    Sex = c("Female", "Female", "Male", "Male")
  )
  
  dd <- descriptive_data(
    test_data,
    version = "3L",
    metric  = "count",
    group   = "Sex"
  )
  
  mo_female <- dd[dd$Dimension == "MO" & dd$Group == "Female", ]
  mo_male   <- dd[dd$Dimension == "MO" & dd$Group == "Male", ]
  
  expect_equal(mo_female$Value, c(2, 0, 0))
  expect_equal(mo_male$Value,   c(0, 1, 1))
})
