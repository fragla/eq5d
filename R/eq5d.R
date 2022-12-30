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
#'   Options are TTO or VAS for EQ-5D-3L, VT for EQ-5D-5L, CW for EQ-5D-5L
#'   crosswalk conversion valuesets, RCW for EQ-5D-3L reverse crosswalk
#'   conversion valuesets and DSU for the NICE Decision Support Unit age-sex
#'   based EQ-5D-3L to EQ-5D-5L and EQ-5D-5L to EQ-5D-3L mappings
#' @param country string of value set country name used.
#' @param ignore.invalid logical to indicate whether to ignore dimension data
#'   with invalid, incomplete or missing data.
#' @param ... character vectors for column names when using a data.frame. Use
#'   "dimensions" (default c("MO", "SC", "UA", "PD" and "AD")), "five.digit"
#'   (default "State") or "utility", "age", "sex" and "bwidth" (defaults
#'   "Utility", "Age", "Sex" and "bwidth") for NICE DSU mapping. When a single
#'   NICE DSU score is being calculated "age", "sex" and "bwidth" are also
#'   used. See \code{\link{eq5dmap}} for valid options.
#' @return a numeric vector of utility index scores.
#' @examples
#' eq5d(scores=c(MO=1,SC=2,UA=3,PD=4,AD=5), type="VT",
#'  country="Indonesia", version="5L")
#' eq5d(scores=c(MO=3,SC=2,UA=3,PD=2,AD=3),
#'  type="TTO", version="3L", country="Germany")
#'
#' eq5d(0.922, country="UK", version="5L", type="DSU",
#'  age=18, sex="male")
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
  utility <- "Utility"
  sex <- "Sex"
  age <- "Age"
  bwidth <- "bwidth"

  if(!is.null(args$dimensions)) {dimensions <- args$dimensions}
  if(!is.null(args$five.digit)) {five.digit <- args$five.digit}
  if(!is.null(args$utility)) {utility <- args$utility}
  if(!is.null(args$sex)) {sex <- args$sex}
  if(!is.null(args$age)) {age <- args$age}
  if(!is.null(args$bwidth)) {bwidth <- args$bwidth}

  eq5d.columns <- NULL
  if(all(dimensions %in% names(scores))) {
    colnames(scores)[match(dimensions, colnames(scores))] <- .getDimensionNames()
    eq5d.columns <- .getDimensionNames()
  } else if(five.digit %in% names(scores)) {
    eq5d.columns <- five.digit
  } else if(utility %in% names(scores)) {
    eq5d.columns <- utility
  } else {
    stop("Unable to identify EQ-5D dimensions in data.frame.")
  }

  if(!bwidth %in% names(scores)) {
    bwidth <- NULL
  }

  res <- apply(scores, 1, function(x) {
    if(type=="DSU") {
      if(is.null(bwidth)) {
        eq5d.default(x[eq5d.columns], version=version, type=type, country=country, ignore.invalid=ignore.invalid, age=x[age], sex=x[sex])
      } else {
        eq5d.default(x[eq5d.columns], version=version, type=type, country=country, ignore.invalid=ignore.invalid, age=x[age], sex=x[sex], bwidth=x[bwidth])
      }
    } else {
      eq5d.default(x[eq5d.columns], version=version, type=type, country=country, ignore.invalid=ignore.invalid, ...)
    }
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
  if(!is.null(type) && type=="DSU") {
    .range <- .getDSURange(country, version)
  }

  if(is.character(scores)){
    .names <- names(scores)
    scores <- suppressWarnings(as.numeric(scores))
    names(scores) <- .names
  }

  if(.length>1) {
    if(.length==5 && all(.getDimensionNames() %in% names(scores))) {
      res <- .eq5d(scores, version=version, type=type, country=country, ignore.invalid=ignore.invalid, ...)
    } else {
      res <- sapply(scores, function(x) {
        eq5d.default(x, version=version, type=type, country=country, ignore.invalid=ignore.invalid, ...)
      })
    }
  } else if (.length==1 && scores %in% getHealthStates(version)) {
    scores <- as.numeric(strsplit(as.character(scores[1]), "")[[1]])
    names(scores) <- .getDimensionNames()
    res <- .eq5d(scores, version=version, type=type, country=country, ignore.invalid=ignore.invalid, ...)
  } else if(.length==1 && !is.na(scores) && exists(".range") && scores >= .range[1] && scores <= .range[2]) {
    res <- .eq5d(scores, version=version, type=type, country=country, ignore.invalid=ignore.invalid, ...) #sex=sex, age=age, bwidth=bwidth)
  } else {
    if(ignore.invalid) {
      res <- NA
    } else {
      stop("Invalid dimension state/utility score found. Add 'ignore.invalid=TRUE' parameter to return NA for invalid scores.")
    }
  }
  return(res)
}

.eq5d <- function(scores,version=version,type=type, country=country, ignore.invalid, ...){
  args <- list(...)
  if(!is.null(args$bwidth)) {
    bwidth <- suppressWarnings(as.numeric(args$bwidth))
    if(is.na(bwidth) || bwidth < 0) {
      if(ignore.invalid) {
        return(NA)
      } else {
        stop("bwidth must be a number >= 0.")
      }
    }
  } else {
    bwidth <- 0
  }

  if(!is.null(args$age) && is.na(.getAgeGroup(args$age))) {
    if(ignore.invalid) {
      return(NA)
    } else {
      stop("Age must be between 18 and 100, or an age category between 1 and 5.")
    }
  }

  if(!is.null(args$sex) && is.na(.getSex(args$sex))) {
    if(ignore.invalid) {
      return(NA)
    } else {
      stop("Sex must be Male, Female, M or F (case insensitive).")
    }
  }

  .length <- length(scores)
  if(.length==5 && any(!scores %in% 1:.getNumberLevels(version))) { #if length==5
    if(ignore.invalid) {
      return(NA)
    } else {
      stop("Missing/non-numeric dimension found.")
    }
  }

  if(.length==1) {
    if(bwidth==0 && !.isValidUtility(scores, country, version)) {
      if(ignore.invalid) {
        return(NA)
      } else {
        stop("Invalid utility score provided. If approximate score please supply bwidth value")
      }
    }
  }

  if(version=="3L") {
    if(!is.null(type) && type %in% c("TTO", "VAS")) {
      eq5d3l(scores, type=type, country=country)
    } else if(!is.null(type) && type=="RCW") {
      eq5drcw(scores, country=country)
    } else if(!is.null(type) && type=="DSU") {
      eq5dmap(scores, country, version, args$age, args$sex, bwidth)
    } else {
      stop("EQ-5D-3L valueset type not recognised. Must be one of 'TTO', 'VAS', 'RCW' or 'DSU'.")
    }
  } else if (version=="Y") {
    eq5dy(scores, country=country)
  }
  else {
    if(!is.null(type) && type=="VT") {
      eq5d5l(scores, country=country)
    } else if(!is.null(type) && type=="CW") {
      eq5dcw(scores, country=country)
    } else if(!is.null(type) && type=="DSU") {
      eq5dmap(scores, country, version, as.numeric(args$age), args$sex, bwidth)
    } else {
      stop("EQ-5D-5L valueset type not recognised. Must be one of 'VT', 'CW' or 'DSU'.")
    }
  }
}

#
#' Get the available EQ-5D value sets.
#'
#' \code{valuesets} returns a data.frame of the available EQ-5D value sets
#'     in the \code{eq5d} package.
#'
#' @param type string EQ-5D value set type. TTO or VAS for EQ-5D-3L, VT for EQ-5D-5L,
#'   cTTO for EQ-5D-Y, CW for EQ-5D-5L crosswalk conversion dataset, or DSU for NICE Decision Support
#'   Unit's EQ-5D-5L to EQ-5D-3L and EQ-5D-3L to EQ-5D-5L mappings.
#' @param version string either 3L, 5L or Y.
#' @param country string one of the countries for which there is a value set.
#' @param references character vector of reference columns. One or more of PubMed, 
#'   DOI, ISBN or ExternalURL. Default is all. Reference columns can be removed by 
#'   setting argument to NULL.
#'
#' @return A data.frame containing the EQ-5D version, the value set type and
#'   country, along with PubMed IDs, DOIs, ISBNs and external URLs where available.
#' @examples
#' valuesets()
#' valuesets(type="TTO")
#' valuesets(version="5L")
#' valuesets(country="UK")
#' valuesets(version="Y", references=c("DOI", "PubMed"))
#' @export
valuesets <- function(type=NULL, version=NULL, country=NULL, references=c("PubMed", "DOI", "ISBN", "ExternalURL")) {
  if(!is.null(version)) version <- paste0("EQ-5D-", version)

  tto <- data.frame(Version="EQ-5D-3L", Type="TTO", Country=colnames(TTO))
  vas <- data.frame(Version="EQ-5D-3L", Type="VAS", Country=colnames(VAS))
  rcw <- data.frame(Version="EQ-5D-3L", Type="RCW", Country=colnames(RCW))
  vt <- data.frame(Version="EQ-5D-5L", Type="VT", Country=colnames(VT))
  cw <- data.frame(Version="EQ-5D-5L", Type="CW", Country=colnames(CW))
  y <- data.frame(Version="EQ-5D-Y", Type="cTTO", Country=colnames(Y))
  dsu3l <- data.frame(Version="EQ-5D-3L", Type="DSU", Country=sub("Copula", "", grep("Copula", sort(colnames(DSU3L)), value=TRUE)))
  dsu5l <- data.frame(Version="EQ-5D-5L", Type="DSU", Country=sub("Copula", "", grep("Copula", sort(colnames(DSU5L)), value=TRUE)))
  vs <- rbind(tto, vas, rcw, vt, cw, y, dsu3l, dsu5l)

  if(!is.null(type)) vs <- vs[vs$Type==type,]
  if(!is.null(version)) vs <- vs[vs$Version==version,]
  if(!is.null(country)) vs <- vs[grep(paste0("^",country), vs$Country),]
  rownames(vs) <- NULL
  
  vs <- merge(vs, REFERENCES, by = c("Version", "Type", "Country"))
  
  if(is.null(references)) {
    vs <- vs[,c("Version", "Type", "Country")]
  } else if (all(references %in% c("PubMed", "DOI", "ISBN", "ExternalURL"))) {
    vs <- vs[,c("Version", "Type", "Country", references)]
  } else {
    stop("One or more reference columns not found. Valid options are one or more of PubMed, DOI, ISBN and ExternalURL.")
  }
  
  vs
}
