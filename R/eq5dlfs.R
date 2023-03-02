#' Calculate the Level Freqeuncy Score for an EQ-5D profile
#' 
#' Calculate the Levels Frequency Score for a single or number of EQ-5D profiles
#' 
#' @param scores data.frame with names MO, SC, UA, PD and AD representing
#'   Mobility, Self-care, Usual activities, Pain/discomfort and Anxiety/depression.
#' @param version string of value "3L", "5L" or "Y" to indicate instrument version.
#' @param ignore.invalid whether to ignore invalid scores. TRUE returns NA, FALSE throws an 
#' error.
#' @param ... character vector, specifying "dimensions" column names. Defaults 
#' are "MO", "SC", "UA", "PD" and "AD".
#' @return a data.frame or list of data.frames of counts/percentages. Columns 
#' contain dimensions names and rows the EQ-5D score.
#' @examples
#' lfs(c(MO=1,SC=2,UA=3,PD=2,AD=1), version="3L")
#' lfs(55555, version="5L")
#' lfs(c(11111, 12345, 55555), version="5L")
#' 
#' @export
lfs <- function(scores, version, ignore.invalid, ...) {
  UseMethod("lfs", scores)
}

#' @export
lfs.data.frame <- function(scores, version=NULL, ignore.invalid=FALSE, ...) {
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
    lfs.default(x, version=version, ignore.invalid=ignore.invalid,...)
  })
  return(res)
}

#' @export
lfs.matrix <- function(scores, version=NULL, ignore.invalid=FALSE, ...) {
  scores <- as.data.frame(scores)
  lfs.data.frame(scores, version=version, ignore.invalid=ignore.invalid, ...)
}

#' @export
lfs.default <- function(scores, version=NULL, ignore.invalid=FALSE, ...){
  
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
      res <- .lfs(scores, version=version, ignore.invalid=ignore.invalid)
    } else {
      res <- sapply(scores, function(x) {
        lfs.default(x, version=version, ignore.invalid=ignore.invalid)
      })
    }
  } else if (.length==1 && scores %in% getHealthStates(version)) {
    scores <- as.numeric(strsplit(as.character(scores[1]), "")[[1]])
    names(scores) <- .getDimensionNames()
    res <- .lfs(scores, version=version, ignore.invalid=ignore.invalid)
  } else {
    if(ignore.invalid) {
      res <- NA
    } else {
      stop("Invalid dimension state found.")
    }
  }
  return(res)
}

.lfs <- function(scores, version, ignore.invalid) {
  if(!all(.getDimensionNames() %in% names(scores)) || any(!scores %in% 1:sub("L", "", version))) {
    if(ignore.invalid) {
      res <- NA
    } else {
      stop("Invalid dimension state found.")
    }
  } else {
    return(sum(scores))
  }
}


.lfs <- function(scores, version, ignore.invalid) {
  if(!all(.getDimensionNames() %in% names(scores)) || any(!scores %in% 1:.getNumberLevels(version))) {
    if(ignore.invalid) {
      res <- NA
    } else {
      stop("Invalid dimension state found.")
    }
  } else {
    freq <- table(scores)
    score <- rep(0,.getNumberLevels(version))
    names(score) <- 1:.getNumberLevels(version)
    score[match(names(freq), names(score))] <- freq
    score <- paste(score, collapse="")
    return(score)
  }
}
