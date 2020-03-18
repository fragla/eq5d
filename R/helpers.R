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