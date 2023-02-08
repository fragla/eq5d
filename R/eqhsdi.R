#' Calculate the Health State Density Index
#' 
#' Calculate the Health State Density Index (HSDI) for an EQ-5D dataset.
#' 
#' @param scores scores data.frame, numeric or character. For data.frame default column 
#' names should be MO, SC, UA, PD and AD representing Mobility, Self-care, 
#' Usual activities, Pain/discomfort and Anxiety/depression. Vector using five 
#' digit format can also be used.
#' @param version string of value "3L" or "5L" to indicate instrument version.
#' @param ignore.invalid booloean whether to ignore invalid scores. TRUE returns NA, FALSE throws an 
#' error.
#' @param digits numeric specifying the number of decimal places for percentages. Defaults to 1, 
#' use NULL to skip rounding.
#' @param ... character vector, specifying "dimensions" column names. Defaults 
#' are "MO", "SC", "UA", "PD" and "AD".
#' @return numeric containing the HSDI value.
#' @examples
#' dat <- read.csv(system.file("extdata", "eq5d3l_example.csv", package="eq5d"))
#' hsdi(dat, version="3L")
#' 
#' @export
hsdi <- function(scores, version=NULL, ignore.invalid=TRUE, digits=2, ...) {
  
  if(is.null(version) || !version %in% c("3L", "5L", "Y"))
    stop("EQ-5D version not one of 3L, 5L or Y.")
  
  cf <- eq5dcf(scores, version=version, ignore.invalid=ignore.invalid, proportions=TRUE, digits=NULL, ...)
  cf$CumulativeState <- 1:nrow(cf) / nrow(cf)
  
  cf$X <- sapply(1:nrow(cf), function(x){cf$CumulativeProp[x]-ifelse(x==1, 0, cf$CumulativeProp[x-1])})
  cf$Y <- sapply(1:nrow(cf), function(x){cf$CumulativeState[x]+ifelse(x==1, 0, cf$CumulativeState[x-1])})
  
  hsdi <- sum(cf$X*cf$Y)
  
  if(!is.null(digits)) {
    hsdi <- round(hsdi, digits)
  }
  
  return(hsdi)
}
