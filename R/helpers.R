.getDimensionNames <- function() {
  return(c("MO", "SC", "UA", "PD", "AD"))
}

#' Get all five digit health state scores
#' 
#' Get all five digit health state scores for either EQ-5D-3L or EQ-5D-5L.
#' 
#' @param version the EQ-5D version. Either 3L or 5L.
#' @return A character vector of five digit health states.
#' @examples
#' getHealthStates("3L")
#' getHealthStates("5L")
#' 
#' @export
getHealthStates <- function(version) {
  if(!version %in% c("3L", "5L"))
    stop("Version must be either 3L or 5L.")
  
  max.value <- sub("L", "", version)
  dimensions <- expand.grid(1:max.value, 1:max.value, 1:max.value, 1:max.value, 1:max.value)
  indexes <- apply(dimensions, 1, function(x){paste(x, collapse="")})
  indexes <- sort(indexes)
  return(indexes)
}

#' Split five digit health states
#' 
#' Split five digit health states into their individual dimension scores.
#' 
#' @param scores a vector of five digit scores
#' @param ignore.invalid whether to ignore invalid scores. TRUE returns NA, FALSE throws an 
#' error.
#' @param version 3L or 5L. Used for validating scores when ignore.invalid 
#' is FALSE.
#' @return A data.frame of individual dimension scores.
#' @examples
#' splitHealthStates(c("12345", "54321"), version="5L")
#' 
#'@export
splitHealthStates <- function(scores, ignore.invalid=TRUE, version="5L") {
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

