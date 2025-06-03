#' Calculate EQ-5D-5L index scores
#'
#' Calculate indices for EQ-5D-5L value sets. Available value sets can be viewed
#'   using the function \code{valuesets}.
#'
#' @param scores numeric with names MO, SC, UA, PD and AD representing
#'   Mobility, Self-care, Usual activities, Pain/discomfort and Anxiety/depression.
#' @param country value set country.
#' @param digits number of decimal places to return.
#' @return calculated utility index score.
#' @examples
#' eq5d5l(scores=c(MO=1,SC=2,UA=3,PD=4,AD=5), country="England")
#' eq5d5l(scores=c(MO=3,SC=2,UA=5,PD=2,AD=3), country="Netherlands")
#'
#' @export
eq5d5l <- function(scores, country="England", digits = 3) {

  if(!all(.get_dimension_names() %in% names(scores)))
    stop("Unable to identify EQ-5D dimensions in scores.")

  if(!all(scores %in% 1:5))
    stop("Scores must be coded as 1, 2, 3, 4 or 5 for EQ-5D-5L.")

  survey <- VT

  if(is.null(country) || !country %in% colnames(survey))
    stop(paste("Country must be one of:", paste(colnames(survey), collapse=", ")))

  survey <- setNames(survey[[country]], rownames(survey))

  values <- c(survey["StartValue"],
              .dimensionScores(scores, survey),
              .minOneGreaterThan1(scores,survey),
              .level4Or5(scores, survey),
              .num45sq(scores, survey),
              .N4(scores,survey),
              .N5(scores,survey))

    return(round(sum(values, na.rm = TRUE), digits=digits))
}

.level4Or5 <- function(scores, survey) {
  if(any(scores %in% c(4,5))) {
    idx <- which(scores %in% c(4,5))
    survey[paste0(names(scores)[idx], "45")]
  }
}

.num45sq <- function(scores, survey) {
  if(any(scores %in% c(4,5))) {
    idx <- which(scores %in% c(4,5))
    (length(idx) - 1)^2 * survey["Num45sq"]
  }
}

.minOneGreaterThan1 <- function(scores, survey) {
  ##at least one mobility, care, activity, pain, anxiety > 1
  if(sum(scores) > 5 && !is.na(survey["Intercept"])) {
    survey["Intercept"] ##At least one 2 or 3 (constant)
  }
}

.N4 <- function(scores, survey) {
  ##at least one mobility, care, activity, pain, anxiety >= 4
  if(any(scores >= 4) && !is.na(survey["N4"])) {
    survey["N4"]
  }
}

.N5 <- function(scores, survey) {
  ##at least one mobility, care, activity, pain, anxiety == 5
  if(any(scores == 5) && !is.na(survey["N5"])) {
    survey["N5"]
  }
}
