#' Calculate the cumulative frequency distribution of EQ-5D health states
#'
#' Computes the frequency, proportion, and cumulative distribution of EQ-5D
#' health states in a dataset. The function accepts EQ-5D data supplied either
#' as dimension-level columns (MO, SC, UA, PD, AD) or as a single column of
#' five-digit EQ-5D health states.
#'
#' The output is a tidy data.frame containing frequencies and cumulative
#' proportions, suitable for computing informativity indices (e.g. HSDI)
#' or for plotting Health State Density Curves (HSDC).
#'
#' @param scores EQ-5D health states supplied as:
#'   \itemize{
#'     \item a vector of 5-digit EQ-5D health states (character or numeric), or
#'     \item a data.frame containing either dimension columns
#'           (\code{MO}, \code{SC}, \code{UA}, \code{PD}, \code{AD})
#'           or a single health-state column named \code{"state"}
#'           (case-insensitive).
#'   }
#' @param version Character string identifying the EQ-5D version:
#'   one of \code{"3L"}, \code{"5L"}, or \code{"Y3L"}.
#' @param ignore.invalid Logical. If \code{TRUE}, invalid health states are
#'   replaced with \code{NA}. If \code{FALSE}, invalid values trigger an error.
#' @param digits Integer specifying the number of decimal places used when
#'   rounding percentages. Defaults to 1.
#' @param ... Additional arguments reserved for future use.
#'
#' @return A data.frame with one row per observed health state and columns:
#'   \itemize{
#'     \item \code{State}: EQ-5D health state (five-digit code),
#'     \item \code{Frequency}: number of observations of the state,
#'     \item \code{Proportion}: relative frequency of the state,
#'     \item \code{CumulativeProp}: cumulative proportion of states,
#'     \item \code{CumulativeState}: cumulative share of distinct states,
#'     \item \code{Percentage}: percentage frequency,
#'     \item \code{CumulativePerc}: cumulative percentage.
#'   }
#'
#' @examples
#' dat <- read.csv(system.file("extdata", "eq5d3l_example.csv", package="eq5d"))
#' eq5dcf(dat, "3L")
#' 
#' @export
eq5dcf <- function(scores, version, ignore.invalid=TRUE, digits=1, ...) {
  
  if (!is.null(version) && version == "Y") {
    lifecycle::deprecate_soft("0.15.4", I('Setting `version = "Y"`'), I('`version = "Y3L"`'))
    version <- "Y3L"
  }
  
  if (is.null(version) || !version %in% c("3L", "5L", "Y3L")) {
    stop("EQ-5D version not one of 3L, 5L or Y3L.", call. = FALSE)
  }
  
  ## ---- normalise input to state vector ----
  if (is.data.frame(scores)) {
    
    dims <- .get_dimension_names()
    nms  <- names(scores)
    idx  <- match("state", tolower(nms))
    
    if (all(dims %in% nms)) {
      states <- get_health_states_from_dimensions(
        scores,
        version = version,
        ignore.invalid = ignore.invalid
      )
    } else if (!is.na(idx)) {
      states <- scores[[nms[idx]]]
    } else {
      stop("Unable to identify EQ-5D states in data.frame.", call. = FALSE)
    }
    
  } else {
    states <- scores
  }
  
  ## ---- validate states ----
  valid <- get_all_health_states(version)
  bad   <- !states %in% valid
  
  if (any(bad)) {
    if (ignore.invalid) {
      states[bad] <- NA
    } else {
      stop("Invalid EQ-5D state(s) found.", call. = FALSE)
    }
  }
  
  ## ---- frequency table ----
  freq <- sort(table(states, useNA = "no"), decreasing = TRUE)
  n    <- sum(freq)
  
  df <- data.frame(
    State = names(freq),
    Frequency = as.integer(freq),
    stringsAsFactors = FALSE
  )
  
  ## ---- proportions and cumulative measures ----
  df$Proportion      <- df$Frequency / n
  df$CumulativeProp  <- cumsum(df$Proportion)
  df$CumulativeState <- seq_len(nrow(df)) / nrow(df)

  if (!is.null(digits)) {
    df$Percentage     <- round(df$Proportion * 100, digits)
    df$CumulativePerc <- round(df$CumulativeProp * 100, digits)
  } else {
    df$Percentage     <- df$Proportion * 100
    df$CumulativePerc <- df$CumulativeProp * 100
  }
  
  return(df)
}
