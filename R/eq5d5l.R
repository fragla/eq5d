#' Calculate EQ-5D-5L indices.
#' 
#' Calculate indices for EQ-5D-5L value sets. Available value sets can be seen 
#'   using the function \code{valuesets}
#' 
#' @param scores numeric with names Mobility, Care, Activity, Pain and Anxiety.
#' @param country value set country. 
#' @examples
#' eq5d5l(scores=c(Mobility=1,Care=2,Activity=3,Pain=4,Anxiety=5), country="England")
#' eq5d5l(scores=c(Mobility=3,Care=2,Activity=5,Pain=2,Anxiety=3), country="Netherlands")
#' 
#' @export
eq5d5l <- function(scores, country="England") {
  
  if(!all(names(scores) %in% c("Mobility", "Care", "Activity", "Pain", "Anxiety")))
    stop("Unable to identify EQ-5D dimensions in scores.")
  
  if(!all(scores %in% 1:5))
    stop("Scores must be coded as 1, 2, 3, 4 or 5 for EQ-5D-5L")
  
  survey <- get("VT")
  
  if(!country %in% colnames(survey))
    stop(paste("Country must be one of:", paste(colnames(survey), collapse=", ")))
  
  survey <- survey[country]

  values <- c(survey["StartValue",],
              .dimensionScores(scores, survey),
              .minOneGreaterThan1(scores,survey),
              .level4Or5(scores, survey),
              .num45sq(scores, survey),
              .N4(scores,survey))

    return(round(sum(values, na.rm = TRUE), digits=3))
}

.level4Or5 <- function(scores, survey) {
  if(any(scores %in% c(4,5))) {
    idx <- which(scores %in% c(4,5))
    values <- survey[paste0(names(scores)[idx], "45"),]
    names(values) <- paste0(names(scores)[idx], "45")

    return(values)
  }
}

.num45sq <- function(scores, survey) {
  if(any(scores %in% c(4,5))) {
    idx <- which(scores %in% c(4,5))
    (length(idx) - 1)^2 * survey["Num45sq",]
  }
}

.minOneGreaterThan1 <- function(scores, survey) {
  ##at least one mobility, care, activity, pain, anxiety > 1
  if(sum(scores) > 5 && !is.na(survey["Intercept",])) {
    value <- survey["Intercept",]
    names(value) <- "Intercept"
    return(value) ##At least one 2 or 3 (constant)
  }
}

.N4 <- function(scores, survey) {
  ##at least one mobility, care, activity, pain, anxiety >= 4
  if(any(scores >= 4) && !is.na(survey["N4",])) {
    value <- survey["N4",]
    names(value) <- "N4"
    return(value)
  }
}
