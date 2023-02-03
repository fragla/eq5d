.getDimensionNames <- function() {
  return(c("MO", "SC", "UA", "PD", "AD"))
}

.getNumberLevels <- function(version) {
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
#' @examples
#' getHealthStates("3L")
#' getHealthStates("5L")
#' getHealthStates("Y")
#' 
#' @export
getHealthStates <- function(version) {
  if(!version %in% c("3L", "5L", "Y"))
    stop("Version must be either 3L, 5L or Y.")
  
  if(version %in% c("3L", "Y")) {
    return(STATES[["3L"]])
  } else {
    return(STATES[["5L"]])
  }
}

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
#' @aliases splitHealthStates
#' @examples
#' getDimensionsFromHealthStates(c("12345", "54321"), version="5L")
#' 
#'@export getDimensionsFromHealthStates splitHealthStates
getDimensionsFromHealthStates <- function(scores, ignore.invalid=TRUE, version="5L") {
  if(ignore.invalid) {
    idx <- which(!scores %in% getHealthStates(version))
    scores[idx] <- NA
  } else {
    stop("Invalid health states found.")
  }
  scores <- lapply(scores, function(x) { 
    as.numeric(strsplit(as.character(x), "")[[1]])
  })
  
  scores <- as.data.frame(do.call(rbind, scores))
  names(scores) <- .getDimensionNames()
  return(scores)
}
splitHealthStates = getDimensionsFromHealthStates 

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
#' @examples
#' scores <- data.frame(MO=c(1,1,1,1,1),SC=c(1,2,1,2,1),
#'                      UA=c(1,2,3,2,1),PD=c(3,2,1,2,3),AD=c(3,3,3,3,3))
#' getHealthStatesFromDimensions(scores, version="5L")
#' 
#'@export
getHealthStatesFromDimensions <- function(scores, version="5L", ignore.invalid=TRUE, dimensions=.getDimensionNames()) {
  if(all(dimensions %in% names(scores))) {
    scores <- scores[,dimensions]
    colnames(scores) <- .getDimensionNames()
  } else {
    stop("Unable to identify EQ-5D dimensions in data.frame.")
  }
  
  states <- paste0(scores$MO, scores$SC, scores$UA, scores$PD, scores$AD)
  
  invalid.idx <- which(!states %in% getHealthStates(version))

  if(length(invalid.idx) > 0) {
    if(ignore.invalid) {
      states[invalid.idx] <- NA
    } else {
      stop("Invalid dimension state(s) found.")
    }
  } 
  
  return(states)
}

