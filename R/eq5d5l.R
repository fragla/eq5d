#' Calculate EQ-5D-5L index scores
#' 
#' Calculate indices for EQ-5D-5L value sets. Available value sets can be viewed 
#'   using the function \code{valuesets}.
#' 
#' @param scores numeric with names MO, SC, UA, PD and AD representing
#'   Mobility, Self-care, Usual activities, Pain/discomfort and Anxiety/depression.
#' @param country value set country. 
#' @return calculated utility index score.
#' @examples
#' eq5d5l(scores=c(MO=1,SC=2,UA=3,PD=4,AD=5), country="England")
#' eq5d5l(scores=c(MO=3,SC=2,UA=5,PD=2,AD=3), country="Netherlands")
#' 
#' @export
eq5d5l <- function(scores, country="England") {
  
  if(!all(.getDimensionNames() %in% names(scores)))
    stop("Unable to identify EQ-5D dimensions in scores.")
  
  if(!all(scores %in% 1:5))
    stop("Scores must be coded as 1, 2, 3, 4 or 5 for EQ-5D-5L.")
  
  survey <- get("VT")
  
  if(is.null(country) || !country %in% colnames(survey))
    stop(paste("Country must be one of:", paste(colnames(survey), collapse=", ")))
  
  survey <- survey[country]

  values <- c(survey["StartValue",],
              .dimensionScores(scores, survey),
              .minOneGreaterThan1(scores,survey),
              .level4Or5(scores, survey),
              .num45sq(scores, survey),
              .N4(scores,survey),
              .N5(scores,survey))

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

.N5 <- function(scores, survey) {
  ##at least one mobility, care, activity, pain, anxiety == 5
  if(any(scores == 5) && !is.na(survey["N5",])) {
    value <- survey["N5",]
    names(value) <- "N5"
    return(value)
  }
}
