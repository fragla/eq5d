#' Calculate the Probablility of Superiority
#' 
#' Calculate the Probablility of Superiority (PS) for the dimensions of two EQ-5D 
#' datasets. Score is less than 0.5 if more patients deteriorate than improve, 0.5 
#' if the same number of patients improve and deteriorate or do not change and 
#' greater than 0.5 if more patients improve than deteriorate.
#' 
#' @param pre data.frame, numeric or character. For data.frame default column 
#' names should be MO, SC, UA, PD and AD representing Mobility, Self-care, 
#' Usual activities, Pain/discomfort and Anxiety/depression. Vector using five 
#' digit format can also be used.
#' @param post data.frame, numeric or character. For data.frame default column 
#' names should be MO, SC, UA, PD and AD representing Mobility, Self-care, 
#' Usual activities, Pain/discomfort and Anxiety/depression. Vector using five 
#' digit format can also be used.
#' @param version string of value "3L" or "5L" to indicate instrument version.
#' @param ignore.invalid boolean whether to ignore invalid scores. TRUE returns NA, FALSE 
#' throws an error.
#' @param dimensions character vector, specifying "dimension" column names. Defaults 
#' are "MO", "SC", "UA", "PD" and "AD".
#' @param digits numeric specifying the number of decimal places. Defaults to 2.
#' @return a list of Probability of Superiority scores by dimension.
#' @examples
#' dat <- read.csv(system.file("extdata", "eq5d3l_example.csv", package="eq5d"))
#' 
#' pre <- dat[dat$Group=="Group1",][1:50,]
#' post <- dat[dat$Group=="Group2",][1:50,]
#' 
#' ps(pre, post, version="3L")
#' 
#' @export
ps <- function(pre, post, version=NULL, ignore.invalid=TRUE, dimensions=.getDimensionNames(), digits=2) {
  dat <- pchc(pre, post, version=version, no.problems=FALSE, totals=TRUE, by.dimension=TRUE, ignore.invalid=ignore.invalid, dimensions=dimensions)
  lapply(dat, .ps)
}

.ps <- function(x, digits=2) {
  round((x["Improve","Number"] + (0.5 * x["No change","Number"])) / x["Total","Number"], digits)
}