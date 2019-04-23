#' Calculate EQ-5D-3L indices.
#' 
#' Calculate indices for EQ-5D-3L value sets. Available value sets can be seen 
#'   using the function \code{valuesets}
#' 
#' @param scores numeric with names Mobility, Care, Activity, Pain and Anxiety.
#' @param type 3L values set type. Either TTO or VAS.
#' @param country value set country. 
#' @examples
#' eq5d3l(scores=c(Mobility=1,Care=2,Activity=3,Pain=1,Anxiety=3), type="VAS", country="UK")
#' eq5d3l(scores=c(Mobility=3,Care=2,Activity=3,Pain=2,Anxiety=3), type="TTO", country="Germany")
#' 
#' @export
eq5d3l <- function(scores, type="TTO", country="UK") {
  
  if(!all(names(scores) %in% c("Mobility", "Care", "Activity", "Pain", "Anxiety"))) {
    stop("Unable to identify EQ-5D dimensions in scores.")
  }
  
  if(!all(scores %in% 1:3))
    stop("Scores must be coded as 1, 2 or 3 for EQ-5D-3L")
  
  if(!type %in% c("TTO", "VAS"))
    stop("Valuation type must be one of TTO or VAS.")
  
  survey <- get(type)
  
  if(!country %in% colnames(survey))
    stop(paste("For EQ-5D-3L", type, "value sets country must be one of:", paste(colnames(survey), collapse=", ")))
  
  survey <- survey[country]

  values <- c(.minOne2Or3(scores, survey), .minOne3(scores, survey), .dimensionScores(scores, survey), .ordinalScore(scores, survey))
  index <- NULL

  if(type=="VAS" && country=="Germany") {
    index <- .eq5d3l.mult(values)
  } else {
    index <- .eq5d3l.add(values)
  }

  return(round(index, digits=3))
  #.eq5d3l.std(mobility, care, activity, pain, anxiety, survey)
}

.eq5d3l.add <- function(values) {
  1+sum(values, na.rm = TRUE)
}

.eq5d3l.mult <- function(values) {
  1*prod(values, na.rm = TRUE)
}

.minOne2Or3 <- function(scores, survey) {
  ##at least one mobility, care, activity, pain, anxiety > 1
  if(sum(scores) > 5 && !is.na(survey["AtLeastOne2Or3",])) {
    value <- survey["AtLeastOne2Or3",]
    names(value) <- "AtLeastOne2Or3"
    return(value) ##At least one 2 or 3 (constant)
  }
}

.minOne3 <- function(scores, survey) {
  ##at least one 3.
  if(any(scores == 3) && !is.na(survey["AtLeastOne3",])) {
    value <- survey["AtLeastOne3",]
    names(value) <- "AtLeastOne3"
    return(value)
  }
}

.dimensionScores <- function(scores, survey) {
  values <- survey[paste0(names(scores), scores),]
  names(values) <- names(scores)
  return(values)
}

.ordinalScore <- function(scores, survey) {
  if (all(!is.na(survey[c("D1", "I2square", "I3", "I3square"),]))) {
    oscore <-
      .D1(scores) * survey["D1",] +
      .I2Square(scores) * survey["I2square",] +
      .I3(scores) * survey["I3",] +
      .I3Square(scores) * survey["I3square",]
  }
}

.D1 <- function(scores) {
  x <- 0
  if(!is.na(scores["Mobility"]) && scores["Mobility"] > 1) x <- x + 1
  if(!is.na(scores["Care"]) && scores["Care"] > 1) x <- x + 1
  if(!is.na(scores["Activity"]) && scores["Activity"] > 1) x <- x + 1
  if(!is.na(scores["Pain"]) && scores["Pain"] > 1) x <- x + 1
  if(!is.na(scores["Anxiety"]) && scores["Anxiety"] > 1) x <- x + 1
  x <- x - 1
  ifelse(x > 0, return(x), return(0))}

.I2 <- function(scores) {
  x <- 0
  if(!is.na(scores["Mobility"]) && scores["Mobility"] == 2) x <- x + 1
  if(!is.na(scores["Care"]) && scores["Care"] == 2) x <- x + 1
  if(!is.na(scores["Activity"]) && scores["Activity"] ==2) x <- x + 1
  if(!is.na(scores["Pain"]) && scores["Pain"] == 2) x <- x + 1
  if(!is.na(scores["Anxiety"]) && scores["Anxiety"] == 2) x <- x + 1
  x <- x - 1
  ifelse(x > 0, return(x), return(0))
}

.I2Square <- function(scores) {
  .I2(scores)^2
}

.I3 <- function(scores) {
  x <- 0
  if(!is.na(scores["Mobility"]) && scores["Mobility"] == 3) x <- x + 1
  if(!is.na(scores["Care"]) && scores["Care"] == 3) x <- x + 1
  if(!is.na(scores["Activity"]) && scores["Activity"] == 3) x <- x + 1
  if(!is.na(scores["Pain"]) && scores["Pain"] == 3) x <- x + 1
  if(!is.na(scores["Anxiety"]) && scores["Anxiety"] == 3) x <- x + 1
  x <- x - 1
  ifelse(x > 0, return(x), return(0))
}

.I3Square <- function(scores) {
  return(.I3(scores)^2)
}


