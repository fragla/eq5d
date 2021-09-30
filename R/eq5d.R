#' Calculate EQ-5D index scores
#'
#' Wrapper for \code{eq5d3l} and \code{eq5d5l}. Calculate EQ-5D index scores for
#' EQ-5D-3L and EQ-5D-5L. Available value sets can be viewed using the function
#' \code{valuesets}.
#'
#' @param scores numeric or data.frame with names/colnames MO, SC, UA, PD and AD
#'   representing Mobility, Self-care, Usual activities, Pain/discomfort and
#'   Anxiety/depression. Alternatively an EQ-5D  score can be provided in
#'   five digit format e.g. 12321.
#' @param version string of value "3L", "5L" or "Y" to indicate instrument
#'   version.
#' @param type string specifying method type used in deriving value set scores.
#'   Options are TTO or VAS for EQ-5D-3L, VT for EQ-5D-5L or CW for EQ-5D-5L
#'   crosswalk conversion valuesets.
#' @param country string of value set country name used.
#' @param ignore.invalid logical to indicate whether to ignore dimension data
#'   with invalid, incomplete or missing data.
#' @param ... character vectors, specifying "dimensions" column names or
#'   "five.digit" column name. Defaults are "MO", "SC", "UA", "PD" and "AD"
#'   for dimensions and "State" for five.digit.
#' @return a numeric vector of utility index scores.
#' @examples
#' eq5d(scores=c(MO=1,SC=2,UA=3,PD=4,AD=5), type="VT",
#'  country="Indonesia", version="5L")
#' eq5d(scores=c(MO=3,SC=2,UA=3,PD=2,AD=3),
#'  type="TTO", version="3L", country="Germany")
#'
#' scores.df <- data.frame(
#'   MO=c(1,2,3,4,5), SC=c(1,5,4,3,2),
#'   UA=c(1,5,2,3,1), PD=c(1,3,4,3,4), AD=c(1,2,NA,2,1)
#'   )
#' eq5d(scores.df, country="Canada", version="5L", type="VT", ignore.invalid=TRUE)
#'
#' eq5d(scores=12321, type="TTO", version="3L", country="UK")
#'
#' scores.df2 <- data.frame(
#'   state=c(11111,12121,23232,33333)
#' )
#'
#' eq5d(scores=scores.df2, type="TTO", version="3L", country="UK", five.digit="state")
#'
#' eq5d(scores=scores.df2$state, type="TTO", version="3L", country="UK")
#'
#' @export
eq5d <- function (scores, version, type, country, ignore.invalid, ...) {
  UseMethod("eq5d", scores)
}

#' @export
eq5d.data.frame <- function(scores, version=NULL, type=NULL, country=NULL, ignore.invalid=FALSE, ...) {
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
    eq5d.default(x, version=version, type=type, country=country, ignore.invalid=ignore.invalid,...)
  })
  return(res)
}

#' @export
eq5d.matrix <- function(scores, version=NULL, type=NULL, country=NULL, ignore.invalid=FALSE, ...) {
  scores <- as.data.frame(scores)
  eq5d.data.frame(scores, version=version, type=type, country=country, ignore.invalid=ignore.invalid, ...)
}

#' @export
eq5d.default <- function(scores, version=NULL, type=NULL, country=NULL, ignore.invalid=FALSE, ...){

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
      res <- .eq5d(scores, version=version, type=type, country=country, ignore.invalid=ignore.invalid)
    } else {
      res <- sapply(scores, function(x) {
        eq5d.default(x, version=version, type=type, country=country, ignore.invalid=ignore.invalid)
      })
    }
  } else if (.length==1 && scores %in% getHealthStates(version)) {
    scores <- as.numeric(strsplit(as.character(scores[1]), "")[[1]])
    names(scores) <- .getDimensionNames()
    res <- .eq5d(scores, version=version, type=type, country=country, ignore.invalid=ignore.invalid)
  } else {
    if(ignore.invalid) {
      res <- NA
    } else {
      stop("Invalid dimension state found. Add 'ignore.invalid=TRUE' parameter to return NA for invalid scores.")
    }
  }
  return(res)
}

.eq5d <- function(scores,version=version,type=type, country=country, ignore.invalid, ...){

  #num.dims <- ifelse(version=="Y", 3, sub("L", "", version))

  if(!all(.getDimensionNames() %in% names(scores)) || any(!scores %in% 1:.getNumberLevels(version))) {
    if(ignore.invalid) {
      return(NA)
    } else {
      stop("Missing/non-numeric dimension found.")
    }
  }

  if(version=="3L") {
    if(!is.null(type) && type %in% c("TTO", "VAS")) {
      eq5d3l(scores, type=type, country=country)
    } else if(!is.null(type) && type=="RCW") {
      eq5drcw(scores, country=country)
    } else {
      stop("EQ-5D-3L valueset type not recognised. Must be one of 'TTO', 'VAS' or 'RCW'.")
    }
  } else if (version=="Y") {
    eq5dy(scores, country=country)
  }
  else {
    if(!is.null(type) && type=="VT") {
      eq5d5l(scores, country=country)
    } else if(!is.null(type) && type=="CW") {
      eq5dcw(scores, country=country)
    } else {
      stop("EQ-5D-5L valueset type not recognised. Must be one of 'VT' or 'CW'.")
    }
  }
}

#
#' Get the available EQ-5D value sets.
#'
#' \code{valuesets} returns a data.frame of the available EQ-5D value sets
#'     in the \code{eq5d} package.
#'
#' @param type string EQ-5D value set type. TTO or VAS for EQ-5D-3L, VT for EQ-5D-5L or
#'   CW for EQ-5D-5L crosswalk conversion dataset.
#' @param version string either 3L or 5L.
#' @param country string one of the countries for which there is a value set.
#'
#' @return A data.frame containing the EQ-5D version, value set type and country
#' @examples
#' valuesets()
#' valuesets(type="TTO")
#' valuesets(version="5L")
#' valuesets(country="UK")
#' @export
valuesets <- function(type=NULL, version=NULL, country=NULL) {
  if(!is.null(version)) version <- paste0("EQ-5D-", version)

  tto <- data.frame(Version="EQ-5D-3L", Type="TTO", Country=colnames(TTO))
  vas <- data.frame(Version="EQ-5D-3L", Type="VAS", Country=colnames(VAS))
  rcw <- data.frame(Version="EQ-5D-3L", Type="RCW", Country=colnames(RCW))
  vt <- data.frame(Version="EQ-5D-5L", Type="VT", Country=colnames(VT))
  cw <- data.frame(Version="EQ-5D-5L", Type="CW", Country=colnames(CW))
  y <- data.frame(Version="EQ-5D-Y", Type="cTTO", Country=colnames(Y))
  vs <- rbind(tto, vas, rcw, vt, cw, y)

  if(!is.null(type)) vs <- vs[vs$Type==type,]
  if(!is.null(version)) vs <- vs[vs$Version==version,]
  if(!is.null(country)) vs <- vs[vs$Country==country,]
  rownames(vs) <- NULL
  return(vs)
}
