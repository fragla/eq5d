#' Calculate the Level Sum Score for an EQ-5D profile
#' 
#' Calculate the Levels Sum Score for a single or number of EQ-5D profiles
#' 
#' @param scores data.frame with names MO, SC, UA, PD and AD representing
#'   Mobility, Self-care, Usual activities, Pain/discomfort and Anxiety/depression.
#' @param version string of value "3L" or "5L" to indicate instrument version.
#' @param ignore.invalid whether to ignore invalid scores. TRUE returns NA, FALSE throws an 
#' error.
#' @param ... character vector, specifying "dimensions" column names. Defaults 
#' are "MO", "SC", "UA", "PD" and "AD".
#' @return a data.frame or list of data.frames of counts/percentages. Columns 
#' contain dimensions names and rows the EQ-5D score.
#' @export
lss <- function(scores, version, ignore.invalid, ...) {
  UseMethod("lss", scores)
}

#' @export
lss.data.frame <- function(scores, version=NULL, ignore.invalid=FALSE, ...) {
  args <- list(...)
  
  dimensions <- .getDimensionNames()
  five.digit <- "State"
  
  if(!is.null(args$dimensions)) {dimensions <- args$dimensions}
  if(!is.null(args$five.digit)) {five.digit <- args$five.digit}
  
  if(all(dimensions %in% names(scores))) {
    scores <- scores[,dimensions]
    colnames(scores) <- .getDimensionNames()
  } else if(five.digit %in% tolower(names(scores))) {
    scores <- scores[,five.digit, drop=FALSE]
  } else {
    stop("Unable to identify EQ-5D dimensions in data.frame.")
  }
  
  res <- apply(scores, 1, function(x) {
    lss.default(x, version=version, ignore.invalid=ignore.invalid,...)
  })
  return(res)
}

#' @export
lss.matrix <- function(scores, version=NULL, ignore.invalid=FALSE, ...) {
  scores <- as.data.frame(scores)
  lss.data.frame(scores, version=version, ignore.invalid=ignore.invalid, ...)
}

#' @export
lss.default <- function(scores, version=NULL, ignore.invalid=FALSE, ...){
  
  if(!version %in% c("3L", "5L", "Y"))
    stop("EQ-5D version not one of 3L, 5L or Y.")
  
  .length = length(scores)
  
  if(is.character(scores)){
    .names <- names(scores)
    scores <- suppressWarnings(as.numeric(scores))
    names(scores) <- .names
  }
  
  if(.length>1) {
    if(.length==5 && all(.getDimensionNames() %in% names(scores))) {
      res <- .lss(scores, version=version, ignore.invalid=ignore.invalid)
    } else {
      res <- sapply(scores, function(x) {
        lss.default(x, version=version, ignore.invalid=ignore.invalid)
      })
    }
  } else if (.length==1 && scores %in% getHealthStates(version)) {
    scores <- as.numeric(strsplit(as.character(scores[1]), "")[[1]])
    names(scores) <- .getDimensionNames()
    res <- .lss(scores, version=version, ignore.invalid=ignore.invalid)
  } else {
    if(ignore.invalid) {
      res <- NA
    } else {
      stop("Invalid dimension state found.")
    }
  }
  return(res)
}

.lss <- function(scores, version, ignore.invalid) {
  if(!all(.getDimensionNames() %in% names(scores)) || any(!scores %in% 1:.getNumberLevels(version))) {
    if(ignore.invalid) {
      res <- NA
    } else {
      stop("Invalid dimension state found.")
    }
  } else {
    return(sum(scores))
  }
}
