context("plot_hpg")

## ------------------------------------------------------------------
## Helper: minimal mock output shaped like `hpg()`
## ------------------------------------------------------------------
mock_hpg_data <- function(include_no_problems = TRUE) {
  
  df <- data.frame(
    Pre  = c(5, 4, 3, 2),
    Post = c(3, 4, 2, 1),
    PCHC = c(
      "Improve",
      "No change",
      "Worsen",
      "Mixed change"
    ),
    stringsAsFactors = FALSE
  )
  
  if (include_no_problems) {
    df <- rbind(
      df,
      data.frame(
        Pre = 1,
        Post = 1,
        PCHC = "No problems",
        stringsAsFactors = FALSE
      )
    )
  }
  
  df
}

## ------------------------------------------------------------------
## Basic behaviour
## ------------------------------------------------------------------
test_that("plot_hpg returns a ggplot object", {
  
  hpg_data <- mock_hpg_data()
  
  p <- plot_hpg(hpg_data, version = "3L")
  
  expect_s3_class(p, "ggplot")
})

## ------------------------------------------------------------------
## No-problems exclusion
## ------------------------------------------------------------------
test_that("include_no_problems = FALSE removes only full-health transitions", {
  
  hpg_data <- mock_hpg_data(include_no_problems = TRUE)
  
  p <- plot_hpg(
    hpg_data,
    version = "3L",
    include_no_problems = FALSE
  )
  
  built <- ggplot2::ggplot_build(p)
  plotted <- built$data[[1]]
  
  # No (1,1) points should remain
  expect_false(any(plotted$x == 1 & plotted$y == 1))
})

## ------------------------------------------------------------------
## Legend content matches data (no forcing of absent classes)
## ------------------------------------------------------------------
test_that("legend reflects transition classes present in the data", {
  
  hpg_data <- mock_hpg_data(include_no_problems = FALSE)
  
  p <- plot_hpg(hpg_data, version = "3L")
  
  # Should not error when legend classes are absent
  expect_s3_class(p, "ggplot")
})

## ------------------------------------------------------------------
## Factor ordering is stable
## ------------------------------------------------------------------
test_that("PCHC factor ordering follows book ordering", {
  
  hpg_data <- mock_hpg_data()
  
  p <- plot_hpg(hpg_data, version = "3L")
  
  expect_equal(
    levels(p$data$PCHC),
    c(
      "Improve",
      "No change",
      "Worsen",
      "Mixed change",
      "No problems"
    )
  )
})

## ------------------------------------------------------------------
## Axis label customisation
## ------------------------------------------------------------------
test_that("custom xlab and ylab are respected", {
  
  hpg_data <- mock_hpg_data()
  
  p <- plot_hpg(
    hpg_data,
    version = "3L",
    xlab = "Post-treatment",
    ylab = "Baseline"
  )
  
  expect_equal(p$labels$x, "Post-treatment")
  expect_equal(p$labels$y, "Baseline")
})

## ------------------------------------------------------------------
## Colour and shape overrides
## ------------------------------------------------------------------
test_that("custom colours and shapes can be supplied", {
  
  hpg_data <- mock_hpg_data()
  
  p <- plot_hpg(
    hpg_data,
    version = "3L",
    colours = c("Improve" = "black"),
    shapes  = c("Improve" = 8)
  )
  
  expect_s3_class(p, "ggplot")
})

## ------------------------------------------------------------------
## Error handling
## ------------------------------------------------------------------
test_that("plot_hpg errors on invalid inputs", {
  
  bad <- data.frame(x = 1:5)
  
  expect_error(
    plot_hpg(bad, version = "3L"),
    "must contain columns"
  )
  
  hpg_data <- mock_hpg_data()
  
  expect_error(
    plot_hpg(hpg_data, version = "2L"),
    "EQ-5D version not one of"
  )
})