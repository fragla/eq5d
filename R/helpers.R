.get_dimension_names <- function() {
  return(c("MO", "SC", "UA", "PD", "AD"))
}

.get_number_levels <- function(version) {
  if(!version %in% c("3L", "5L", "Y"))
    stop("Version must be either 3L, 5L or Y.")
  
  levels <- ifelse(version=="Y", 3, as.numeric(sub("L", "", version)))
  
  return(levels)
}

#' Get all five digit health state scores
#' 
#' Get all five digit health state scores for either EQ-5D-3L, EQ-5D-5L or
#' EQ-5D-Y
#' 
#' @param version the EQ-5D version. Either 3L or 5L.
#' @return A character vector of five digit health states.
#' @aliases getHealthStates
#' @examples
#' get_all_health_states("3L")
#' get_all_health_states("5L")
#' get_all_health_states("Y")
#' 
#' @export getHealthStates get_all_health_states
get_all_health_states <- function(version) {
  if(!version %in% c("3L", "5L", "Y"))
    stop("Version must be either 3L, 5L or Y.")
  
  if(version %in% c("3L", "Y")) {
    return(STATES[["3L"]])
  } else {
    return(STATES[["5L"]])
  }
}
getHealthStates = get_all_health_states
#' Get individual dimension scores from their five digit health states
#' 
#' Get a data.frame of individual dimension scores from their five digit health states.
#' 
#' @param scores a vector of five digit scores
#' @param ignore.invalid whether to ignore invalid scores. TRUE returns NA, FALSE throws an 
#' error.
#' @param version 3L, 5L or Y. Used for validating scores when ignore.invalid 
#' is FALSE.
#' @return A data.frame of individual dimension scores.
#' @aliases splitHealthStates getDimensionsFromHealthStates
#' @examples
#' get_dimensions_from_health_states(c("12345", "54321"), version="5L")
#' 
#'@export get_dimensions_from_health_states splitHealthStates getDimensionsFromHealthStates
get_dimensions_from_health_states <- function(scores, ignore.invalid=TRUE, version="5L") {
  if(ignore.invalid) {
    idx <- which(!scores %in% get_all_health_states(version))
    scores[idx] <- NA
  } else {
    stop("Invalid health states found.")
  }
  scores <- lapply(scores, function(x) { 
    as.numeric(strsplit(as.character(x), "")[[1]])
  })
  
  scores <- as.data.frame(do.call(rbind, scores))
  names(scores) <- .get_dimension_names()
  return(scores)
}
splitHealthStates = getDimensionsFromHealthStates = get_dimensions_from_health_states 

#' Get five digit health states from dimension scores
#' 
#' Merge MO, SC, UA, PD and AD dimension scores to get five digit health states.
#' 
#' @param scores a data.fram containing each dimension in a column
#' @param ignore.invalid whether to ignore invalid scores. TRUE returns NA, FALSE throws an 
#' error.
#' @param version 3L, 5L or Y. Used for validating scores when ignore.invalid 
#' is FALSE.
#' @param dimensions character vector specifying "dimensions" column names. Defaults 
#' are "MO", "SC", "UA", "PD" and "AD".
#' @return A character vector of individual dimension scores.
#' @aliases getHealthStatesFromDimensions
#' @examples
#' scores <- data.frame(MO=c(1,1,1,1,1),SC=c(1,2,1,2,1),
#'                      UA=c(1,2,3,2,1),PD=c(3,2,1,2,3),AD=c(3,3,3,3,3))
#' get_health_states_from_dimensions(scores, version="5L")
#' 
#'@export get_health_states_from_dimensions getHealthStatesFromDimensions
get_health_states_from_dimensions <- function(scores, version="5L", ignore.invalid=TRUE, dimensions=.get_dimension_names()) {
  if(all(dimensions %in% names(scores))) {
    scores <- scores[,dimensions]
    colnames(scores) <- .get_dimension_names()
  } else {
    stop("Unable to identify EQ-5D dimensions in data.frame.")
  }
  
  states <- paste0(scores$MO, scores$SC, scores$UA, scores$PD, scores$AD)
  
  invalid.idx <- which(!states %in% get_all_health_states(version))

  if(length(invalid.idx) > 0) {
    if(ignore.invalid) {
      states[invalid.idx] <- NA
    } else {
      stop("Invalid dimension state(s) found.")
    }
  } 
  
  return(states)
}

getHealthStatesFromDimensions = get_health_states_from_dimensions