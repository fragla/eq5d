#' Calculate EQ-5D-Y index scores
#'
#' Calculate indices for EQ-5D-Y value sets. Available value sets can be viewed
#'   using the function \code{valuesets}.
#'
#' @param scores numeric with names MO, SC, UA, PD and AD representing
#'   Mobility, Self-care, Usual activities, Pain/discomfort and Anxiety/depression.
#' @param country value set country.
#' @return calculated utility index score.
#' @examples
#' eq5dy(scores=c(MO=3,SC=3,UA=3,PD=3,AD=3), country="Slovenia")
#'
#' @export
eq5dy <- function(scores, country=NULL) {

  if(!all(.get_dimension_names() %in% names(scores)))
    stop("Unable to identify EQ-5D dimensions in scores.")

  if(!all(scores %in% 1:3)) {
    message <- "Scores must be coded as 1, 2 or 3 for EQ-5D-Y."
    if(all(scores %in% 1:5)) {
      message <- paste(message, "Are you using EQ-5D-5L?")
    }
    stop(message)
  }

  survey <- Y

  if(is.null(country) || !country %in% colnames(survey))
    stop(paste("Country must be one of:", paste(colnames(survey), collapse=", ")))

  survey <- setNames(survey[[country]], rownames(survey))

  values <- c(survey["FullHealth"],
              .intercept(scores, survey), .power(scores, survey), .all3(scores, survey))

  return(round(sum(values, na.rm = TRUE), digits=3))
}

.intercept <- function(scores, survey) {
  ##at least one mobility, care, activity, pain, anxiety > 1
  if(sum(scores) > 5 && !is.na(survey["Intercept"])) {
    survey["Intercept"] ##At least one 2 or 3 (constant)
  }
}

.power <- function(scores, survey) {
  ##check for power coefficient - currently only Indonesia.
  if(!is.na(survey["Power"])) {
    .total <- -1*sum(.dimensionScores(scores, survey)*-1, na.rm = TRUE)^survey["Power"]
  } else {
    .total <- sum(.dimensionScores(scores, survey), na.rm = TRUE)
  }
  return(.total)
}

.all3 <- function(scores, survey) {
  ##all dimensions == 3
  if(all(scores == 3) && !is.na(survey["A3"])) {
    survey["A3"]
  }
}
