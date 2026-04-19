context("EQ-5D cumulative frequency (eq5dcf)")

## ------------------------------------------------------------------
## Test data
## ------------------------------------------------------------------

dat <- read.csv("../testdata/pre.csv")$x

# Legacy expected output (old eq5dcf.csv) used as reference only
res_old <- read.csv(
  "../testdata/eq5dcf.csv",
  colClasses = c("character", rep("numeric", 4))
)

## Data-frame version of the same data
pre_df <- read.csv("../testdata/pre_df.csv")

pre_df_state <- data.frame(
  state = get_health_states_from_dimensions(pre_df, version = "3L"),
  stringsAsFactors = FALSE
)

## ------------------------------------------------------------------
## Five-digit input
## ------------------------------------------------------------------

test_that("eq5dcf five-digit input returns correct cumulative distribution", {
  
  cf <- eq5dcf(dat, version = "3L", ignore.invalid = TRUE)
  
  ## Basic structure
  expect_true(is.data.frame(cf))
  expect_true(all(c(
    "State",
    "Frequency",
    "Proportion",
    "CumulativeProp",
    "CumulativeState",
    "Percentage",
    "CumulativePerc"
  ) %in% names(cf)))
  
  ## Frequencies and percentages match legacy reference
  expect_equal(cf$State, res_old$State)
  expect_equal(cf$Frequency, res_old$Frequency)
  expect_equal(cf$Percentage, res_old$Percentage)
  expect_equal(cf$CumulativePerc, res_old$CumulativePerc)
  
  ## Canonical distribution relationships
  expect_equal(
    cf$Proportion,
    cf$Frequency / sum(cf$Frequency)
  )
  
  expect_equal(
    cf$CumulativeProp,
    cumsum(cf$Proportion)
  )
  
  expect_equal(
    cf$CumulativePerc,
    round(cf$CumulativeProp * 100, 1),
    tolerance = 1e-6
  )
})


test_that("eq5dcf five-digit input throws error when ignore.invalid = FALSE", {
  expect_error(eq5dcf(dat, version = "3L", ignore.invalid = FALSE))
  expect_error(eq5dcf(dat, version = "Y3L", ignore.invalid = FALSE))
})

## ------------------------------------------------------------------
## Data-frame inputs
## ------------------------------------------------------------------

test_that("eq5dcf data.frame inputs are representation-invariant", {
  
  cf_vec   <- eq5dcf(dat, version = "3L")
  cf_df    <- eq5dcf(pre_df, version = "3L")
  cf_state <- eq5dcf(pre_df_state, version = "3L")
  
  expect_equal(cf_df, cf_vec)
  expect_equal(cf_state, cf_vec)
})


test_that("eq5dcf data.frame throws error on invalid input", {
  expect_error(eq5dcf(pre_df, version = "3L", ignore.invalid = FALSE))
})

## ------------------------------------------------------------------
## Deprecated version handling
## ------------------------------------------------------------------

test_that("eq5dcf using version = 'Y' is deprecated", {
  rlang::local_options(lifecycle_verbosity = "error")
  expect_error(eq5dcf(dat, version = "Y"))
})


test_that("eq5dcf using version = 'Y' still works quietly", {
  rlang::local_options(lifecycle_verbosity = "quiet")
  expect_equal(
    eq5dcf(dat, version = "Y"),
    eq5dcf(dat, version = "3L")
  )
})
