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
#' @examples
#' lss(c(MO=1,SC=2,UA=3,PD=2,AD=1), version="3L")
#' lss(55555, version="5L")
#' lss(c(11111, 12345, 55555), version="5L")
#' 
#' @export
lss <- function(scores, version, ignore.invalid, ...) {
  UseMethod("lss", scores)
}

#' @export
lss.data.frame <- function(scores, version=NULL, ignore.invalid=FALSE, ...) {
  args <- list(...)
  
  dimensions <- .get_dimension_names()
  five.digit <- "State"
  
  if(!is.null(args$dimensions)) {dimensions <- args$dimensions}
  if(!is.null(args$five.digit)) {five.digit <- args$five.digit}
  
  if(all(dimensions %in% names(scores))) {
    scores <- scores[,dimensions]
    colnames(scores) <- .get_dimension_names()
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
  
  if (!is.null(version) && version == "Y") {
    lifecycle::deprecate_warn("0.16.0", I('Setting `version = "Y"`'), I('`version = "Y3L"`'))
    version <- "Y3L"
  }
  
  if(!version %in% c("3L", "5L", "Y3L"))
    stop("EQ-5D version not one of 3L, 5L or Y3L.")
  
  .length = length(scores)
  
  if(is.character(scores)){
    .names <- names(scores)
    scores <- suppressWarnings(as.numeric(scores))
    names(scores) <- .names
  }
  
  if(.length>1) {
    if(.length==5 && all(.get_dimension_names() %in% names(scores))) {
      res <- .lss(scores, version=version, ignore.invalid=ignore.invalid)
    } else {
      res <- sapply(scores, function(x) {
        lss.default(x, version=version, ignore.invalid=ignore.invalid)
      })
    }
  } else if (.length==1 && scores %in% get_all_health_states(version)) {
    scores <- as.numeric(strsplit(as.character(scores[1]), "")[[1]])
    names(scores) <- .get_dimension_names()
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
  if(!all(.get_dimension_names() %in% names(scores)) || any(!scores %in% 1:.get_number_levels(version))) {
    if(ignore.invalid) {
      res <- NA
    } else {
      stop("Invalid dimension state found.")
    }
  } else {
    return(sum(scores))
  }
}
