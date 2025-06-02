#' Calculate EQ-5D-3L reverse crosswalk index scores
#'
#' Calculate indices for EQ-5D-3L indices by mapping them onto EQ-5D-5L value sets.
#' Available value sets can be viewed using the function \code{valuesets}.
#'
#' @param scores numeric with names MO, SC, UA, PD and AD representing
#'   Mobility, Self-care, Usual activities, Pain/discomfort and Anxiety/depression.
#' @param country value set country.
#' @param method crosswalk values to use. Either "VH" (Van Hout, 2021) or "EQ" 
#' (EuroQol 2019 values). The van Hout method is recommended.
#' @param digits number of decimal places to return.
#' @return calculated utility index score.
#' @examples
#' eq5drcw(scores=c(MO=1,SC=2,UA=3,PD=2,AD=1), country="Netherlands")
#' eq5drcw(scores=c(MO=3,SC=3,UA=3,PD=3,AD=3), country="Germany")
#'
#' @export
eq5drcw <- function(scores, country="UK", method="VH", digits = 3) {
  if(!all(.get_dimension_names() %in% names(scores))) {
    stop("Unable to identify EQ-5D dimensions in scores.")
  }
  
  if(!method %in% c("VH", "EQ")) {
    stop("Unable to identify reverse crosswalk values to use.")
  }

  if(!all(scores %in% 1:3))
    stop("Scores must be coded as 1, 2, or 3 for EQ-5D-3L Reverse Crosswalk.")
  
  if(method == "VH") {
    survey <- RCWVH
  } else {
    survey <- RCW
  }
  
  if(is.null(country) || !country %in% colnames(survey))
    stop(paste("For EQ-5D-3L reverse crosswalk value sets country must be one of:", paste(colnames(survey), collapse=", ")))

  return(round(survey[paste(scores, collapse=""), country], digits))
}

