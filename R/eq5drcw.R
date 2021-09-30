#' Calculate EQ-5D-3L reverse crosswalk index scores
#'
#' Calculate indices for EQ-5D-3L indices by mapping them onto EQ-5D-5L value sets.
#' Available value sets can be viewed using the function \code{valuesets}.
#'
#' @param scores numeric with names MO, SC, UA, PD and AD representing
#'   Mobility, Self-care, Usual activities, Pain/discomfort and Anxiety/depression.
#' @param country value set country.
#' @return calculated utility index score.
#' @examples
#' eq5drcw(scores=c(MO=1,SC=2,UA=3,PD=2,AD=1), country="Netherlands")
#' eq5drcw(scores=c(MO=3,SC=3,UA=3,PD=3,AD=3), country="Germany")
#'
#' @export
eq5drcw <- function(scores, country="UK") {
  if(!all(.getDimensionNames() %in% names(scores))) {
    stop("Unable to identify EQ-5D dimensions in scores.")
  }

  if(!all(scores %in% 1:3))
    stop("Scores must be coded as 1, 2, or 3 for EQ-5D-3L Reverse Crosswalk.")

  survey <- RCW

  if(is.null(country) || !country %in% colnames(survey))
    stop(paste("For EQ-5D-3L reverse crosswalk value sets country must be one of:", paste(colnames(survey), collapse=", ")))

  return(survey[paste(scores, collapse=""), country])
}
