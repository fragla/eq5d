#' Calculate the Level Sum Score for an EQ-5D profile
#' 
#' Calculate the Levels Sum Score for one or more EQ-5D profiles
#' 
#' @param scores EQ-5D health states supplied as:
#'   \itemize{
#'     \item a named numeric vector of dimension levels (MO, SC, UA, PD, AD),
#'     \item a 5-digit EQ-5D health state (character or numeric),
#'     \item a vector of 5-digit health states,
#'     \item or a data.frame containing either dimension columns
#'           or a single health state column.
#'   }
#' @param version string of value "3L", "5L" or "Y3L" to indicate instrument version.
#' @param ignore.invalid whether to ignore invalid scores. TRUE returns NA, FALSE throws an 
#' error.
#' @param ... Optional arguments.
#'   \describe{
#'     \item{dimensions}{Character vector giving names of EQ-5D dimension columns.}
#'     \item{five.digit}{Name of the column containing 5-digit EQ-5D health states
#'       when `scores` is a data.frame (default: "State"). Matching is case-insensitive.}
#'   }
#' are "MO", "SC", "UA", "PD" and "AD".
#' @return an integer vector of Level Sum Scores.
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
    scores <- scores[, dimensions, drop = FALSE]
    colnames(scores) <- .get_dimension_names()
  } else {
    nms <- names(scores)
    idx <- match(tolower(five.digit), tolower(nms))
    
    if (!is.na(idx)) {
      scores <- scores[, nms[idx], drop = FALSE]
    } else {
      stop("Unable to identify EQ-5D dimensions in data.frame.", call. = FALSE)
    }
  }
  
  res <- vapply(
    seq_len(nrow(scores)), 
    function(i) {
      row <- scores[i, , drop = FALSE]
      x   <- setNames(as.numeric(row), names(row))
      lss.default(x, version = version, ignore.invalid = ignore.invalid, ...)
    }, FUN.VALUE = NA_integer_
  )
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
    lifecycle::deprecate_soft("0.15.4", I('Setting `version = "Y"`'), I('`version = "Y3L"`'))
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
      res <- vapply(scores, function(x) {
        lss.default(x, version=version, ignore.invalid=ignore.invalid)
      }, FUN.VALUE = NA_integer_)
    }
  } else if (.length==1 && scores %in% get_all_health_states(version)) {
    scores <- as.numeric(strsplit(as.character(scores[1]), "")[[1]])
    names(scores) <- .get_dimension_names()
    res <- .lss(scores, version=version, ignore.invalid=ignore.invalid)
  } else {
    if(ignore.invalid) {
      res <- NA_integer_
    } else {
      stop("Invalid dimension state found.")
    }
  }
  return(res)
}

.lss <- function(scores, version, ignore.invalid) {
  n_levels <- .get_number_levels(version)
  
  if(!all(.get_dimension_names() %in% names(scores)) || any(!scores %in% seq_len(n_levels))) {
    if(ignore.invalid) {
      return(NA_integer_)
    } else {
      stop("Invalid dimension state found.", call. = FALSE)
    }
  }
    
  as.integer(sum(scores))
}
