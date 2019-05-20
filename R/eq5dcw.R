#' Calculate EQ-5D-5L crosswalk index scores
#' 
#' Calculate indices for EQ-5D-5L indices by mapping them onto EQ-5D-3L value sets. 
#' Available value sets can be viewed using the function \code{valuesets}.
#' 
#' @param scores numeric with names MO, SC, UA, PD and AD representing
#'   Mobility, Self-care, Usual activities, Pain/discomfort and Anxiety/depression.
#' @param country value set country. 
#' @examples
#' eq5dcw(scores=c(MO=1,SC=2,UA=5,PD=1,AD=3), country="UK")
#' eq5dcw(scores=c(MO=3,SC=5,UA=5,PD=2,AD=3), country="Germany")
#' 
#' @export
eq5dcw <- function(scores, country="UK") {  
  if(!all(names(scores) %in% c("MO", "SC", "UA", "PD", "AD"))) {
    stop("Unable to identify EQ-5D dimensions in scores.")
  }
  
  if(!all(scores %in% 1:5))
    stop("Scores must be coded as 1, 2, 3, 4 or 5 for EQ-5D-5L Crosswalk.")
  
  survey <- get("CW")
  
  if(!country %in% colnames(survey))
    stop(paste("For EQ-5D-5L crosswalk value sets country must be one of:", paste(colnames(survey), collapse=", ")))
  
  return(survey[paste(scores, collapse=""), country])
}
