#' Calculate EQ-5D index scores
#'
#' Wrapper for \code{eq5d3l}, \code{eq5d5l} and \code{eq5dy3l}. Calculate EQ-5D index scores for
#' EQ-5D-3L, EQ-5D-5L and EQ-5D-Y-3L. Available value sets can be viewed using the function
#' \code{valuesets}.
#'
#' @param scores numeric or data.frame with names/colnames MO, SC, UA, PD and AD
#'   representing Mobility, Self-care, Usual activities, Pain/discomfort and
#'   Anxiety/depression. Alternatively EQ-5D scores can be provided in
#'   five digit format e.g. 12321. If five digit scores are used in a data.frame
#'   the default column name look for by the function is "State".
#' @param version string of value "3L", "5L" or "Y3L" to indicate instrument
#'   version.
#' @param type string specifying method type used in deriving value set scores.
#'   Options are TTO or VAS for EQ-5D-3L, VT for EQ-5D-5L, CW for EQ-5D-5L
#'   crosswalk conversion valuesets, RCW for EQ-5D-3L reverse crosswalk
#'   conversion valuesets and DSU for the NICE Decision Support Unit's EEPRU 
#'   age-sex based EQ-5D-3L to EQ-5D-5L and EQ-5D-5L to EQ-5D-3L mappings.
#' @param country string of value set country name used.
#' @param ignore.invalid logical to indicate whether to ignore dimension data
#'   with invalid, incomplete or missing data.
#' @param ... character vectors for column names when using a data.frame. Use
#'   "dimensions" (default c("MO", "SC", "UA", "PD" and "AD")), "five.digit"
#'   (default "State") or "utility", "age", "sex" and "bwidth" (defaults
#'   "Utility", "Age", "Sex" and "bwidth") for NICE DSU mapping. bwidth can also
#'   be a number which is applied to the whole dataset. When a single
#'   NICE DSU score is being calculated "age", "sex" and "bwidth" are also
#'   used. See \code{\link{eq5dmap}} for valid options. "digits" can also be 
#'   used to return scores with more precision.
#' @return a numeric vector of utility index scores.
#' @examples
#' 
#' #EQ-5D-5L single utility score by dimension
#' eq5d(scores=c(MO=1,SC=2,UA=3,PD=4,AD=5), type="VT",
#'  country="Indonesia", version="5L")
#'  
#' #EQ-5D-3L single utility score by dimension
#' eq5d(scores=c(MO=3,SC=2,UA=3,PD=2,AD=3),
#'  type="TTO", version="3L", country="Germany")
#'
#' #Mapping an EQ-5D-5L utility score to EQ-5D-3L using NICE DSU method
#' eq5d(0.922, country="UK", version="5L", type="DSU",
#'  age=18, sex="male")
#'
#' #Calculation of multiple EQ-5D-5L utility scores from a data.frame of dimensions
#' scores.df <- data.frame(
#'   MO=c(1,2,3,4,5), SC=c(1,5,4,3,2),
#'   UA=c(1,5,2,3,1), PD=c(1,3,4,3,4), AD=c(1,2,NA,2,1)
#' )
#'
#' eq5d(scores.df, country="Canada", version="5L", type="VT", ignore.invalid=TRUE)
#'
#' #Calculation of a utility score using five digit state
#' eq5d(scores=12321, type="TTO", version="3L", country="UK")
#'
#' scores.df2 <- data.frame(
#'   state=c(11111,12121,23232,33333)
#' )
#'
#' #Calculation of utility scores using a data.frame with five digit states
#' eq5d(scores=scores.df2, type="TTO", version="3L", country="UK", five.digit="state")
#'
#' #Calculation of utility scores from a vector of five digit states
#' eq5d(scores=scores.df2$state, type="TTO", version="3L", country="UK")
#' 
#' #Mapping multiple utility scores from EQ-5D-5L to EQ-5D-3L using NICE DSU method
#' scores.df3 <- data.frame(
#'   Utility=c(0.715,0.435,0.95),
#'   Age=c(50,30,70),
#'   Sex=c("m","f","m"),
#'   bwidth=c(0.2,0.2,0.1)
#' )
#' 
#' #using bwidth column values (one per observation)
#' eq5d(scores.df3, type="DSU", version="5L", country="UK")
#' 
#' #using single bwidth value for whole dataset
#' eq5d(scores.df3, type="DSU", version="5L", country="UK", bwidth=0.1) 
#'
#' @export
eq5d <- function (scores, version, type, country, ignore.invalid, ...) {
  UseMethod("eq5d", scores)
}

#' @export
eq5d.data.frame <- function(scores, version=NULL, type=NULL, country=NULL, ignore.invalid=FALSE, ...) {
  args <- list(...)

  dimensions <- .get_dimension_names()
  five.digit <- "State"
  utility <- "Utility"
  sex <- "Sex"
  age <- "Age"
  bwidth <- "bwidth"
  digits <- 3

  if(!is.null(args$dimensions)) {dimensions <- args$dimensions}
  if(!is.null(args$five.digit)) {five.digit <- args$five.digit}
  if(!is.null(args$utility)) {utility <- args$utility}
  if(!is.null(args$sex)) {sex <- args$sex}
  if(!is.null(args$age)) {age <- args$age}
  if(!is.null(args$bwidth)) {bwidth <- args$bwidth}
  if(!is.null(args$digits)) {digits <- args$digits}

  eq5d.columns <- NULL
  if(all(dimensions %in% names(scores))) {
    colnames(scores)[match(dimensions, colnames(scores))] <- .get_dimension_names()
    eq5d.columns <- .get_dimension_names()
  } else if(five.digit %in% names(scores)) {
    eq5d.columns <- five.digit
  } else if(utility %in% names(scores)) {
    eq5d.columns <- utility
  } else {
    stop("Unable to identify EQ-5D dimensions in data.frame.")
  }
  
  if(!length(bwidth) %in% 0:1) {
    stop("bwidth must be a single column name or decimal.")
  }

  if(!bwidth %in% names(scores) && !is.numeric(bwidth)) {
    bwidth <- NULL
  }

  res <- apply(scores, 1, function(x) {
    if(type=="DSU") {
      if(is.null(bwidth)) {
        eq5d.default(x[eq5d.columns], version=version, type=type, country=country, ignore.invalid=ignore.invalid, age=x[age], sex=x[sex], digits=digits)
      } else if(is.numeric(bwidth)) {
        eq5d.default(x[eq5d.columns], version=version, type=type, country=country, ignore.invalid=ignore.invalid, age=x[age], sex=x[sex], bwidth=bwidth, digits=digits)
      } else {
        eq5d.default(x[eq5d.columns], version=version, type=type, country=country, ignore.invalid=ignore.invalid, age=x[age], sex=x[sex], bwidth=x[bwidth], digits=digits)
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
  
  if (!is.null(version) && version == "Y") {
    lifecycle::deprecate_soft("0.15.4", I('Setting `version = "Y"`'), I('`version = "Y3L"`'))
    version <- "Y3L"
  }
  
  if(!version %in% c("3L", "5L", "Y3L"))
    stop("EQ-5D version not one of 3L, 5L or Y3L.")

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
    if(.length==5 && all(.get_dimension_names() %in% names(scores))) {
      res <- .eq5d(scores, version=version, type=type, country=country, ignore.invalid=ignore.invalid, ...)
    } else {
      res <- sapply(scores, function(x) {
        eq5d.default(x, version=version, type=type, country=country, ignore.invalid=ignore.invalid, ...)
      })
    }
  } else if (.length==1 && scores %in% get_all_health_states(version)) {
    scores <- as.numeric(strsplit(as.character(scores[1]), "")[[1]])
    names(scores) <- .get_dimension_names()
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
  
  if(!is.null(args$digits)) {
    digits <- suppressWarnings(as.numeric(args$digits))
    if(is.na(digits)) {
      if(ignore.invalid) {
        return(NA)
      } else {
        stop("\"digits\" must be a numeric.")
      }
    }
  } else {
    digits <- 3
  }

  .length <- length(scores)
  if(.length==5 && any(!scores %in% 1:.get_number_levels(version))) { #if length==5
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
      eq5d3l(scores, type=type, country=country, digits = digits)
    } else if(!is.null(type) && type=="RCW") {
      if(is.null(args$method)) {
        method <- "VH"
      } else {
        method <- args$method
      }
      eq5drcw(scores, country=country, method=method, digits = digits)
    } else if(!is.null(type) && type=="DSU") {
      eq5dmap(scores, country, version, args$age, args$sex, bwidth, digits)
    } else {
      stop("EQ-5D-3L valueset type not recognised. Must be one of 'TTO', 'VAS', 'RCW' or 'DSU'.")
    }
  } else if (version=="Y3L") {
    eq5dy3l(scores, country=country, digits = digits)
  }
  else {
    if(!is.null(type) && type=="VT") {
      eq5d5l(scores, country=country, digits = digits)
    } else if(!is.null(type) && type=="CW") {
      eq5dcw(scores, country=country)
    } else if(!is.null(type) && type=="DSU") {
      eq5dmap(scores, country, version, as.numeric(args$age), args$sex, bwidth, digits)
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
#'   cTTO for EQ-5D-Y-3L, CW for EQ-5D-5L crosswalk conversion dataset, or DSU for NICE Decision Support
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
#' valuesets(version="Y3L", references=c("DOI", "PubMed"))
#' @export
valuesets <- function(type=NULL, version=NULL, country=NULL, references=c("PubMed", "DOI", "ISBN", "ExternalURL")) {
  
  if (!is.null(version) && version == "Y") {
    lifecycle::deprecate_soft("0.15.4", I('Setting `version = "Y"`'), I('`version = "Y3L"`'))
    version <- "Y3L"
  }
  
  if(!is.null(version) && version == "Y3L") version <- "Y-3L"
  if(!is.null(version)) version <- paste0("EQ-5D-", version)

  tto <- data.frame(Version="EQ-5D-3L", Type="TTO", Country=colnames(TTO), Notes=NA)
  vas <- data.frame(Version="EQ-5D-3L", Type="VAS", Country=colnames(VAS), Notes=NA)
  rcw <- data.frame(Version="EQ-5D-3L", Type="RCW", Country=colnames(RCW), Notes="EuroQol (2019)")
  rcwvh <- data.frame(Version="EQ-5D-3L", Type="RCW", Country=colnames(RCWVH), Notes="van Hout (2021)")
  vt <- data.frame(Version="EQ-5D-5L", Type="VT", Country=colnames(VT), Notes=NA)
  cw <- data.frame(Version="EQ-5D-5L", Type="CW", Country=colnames(CW), Notes=NA)
  y <- data.frame(Version="EQ-5D-Y-3L", Type="cTTO", Country=colnames(Y3L), Notes=NA)
  dsu3l <- data.frame(Version="EQ-5D-3L", Type="DSU", Country=sub("Copula", "", grep("Copula", sort(colnames(DSU3L)), value=TRUE)), Notes=NA)
  dsu5l <- data.frame(Version="EQ-5D-5L", Type="DSU", Country=sub("Copula", "", grep("Copula", sort(colnames(DSU5L)), value=TRUE)), Notes=NA)

  vs1 <- rbind(tto, vas, rcw, vt, cw, y, dsu3l, dsu5l)
  vs1 <- merge(vs1, REFERENCES, by = c("Version", "Type", "Country"))
  
  vs2 <- cbind(rcwvh, REFERENCES[REFERENCES$Type=="RCW" & is.na(REFERENCES$Country),!names(REFERENCES) %in% c("Country", "Version", "Type")], row.names = NULL)
  vs <- rbind(vs1, vs2)
  
  if(!is.null(type)) vs <- vs[vs$Type==type,]
  if(!is.null(version)) vs <- vs[vs$Version==version,]
  if(!is.null(country)) vs <- vs[grep(paste0("^",country), vs$Country),]
  rownames(vs) <- NULL
  
  if(is.null(references)) {
    vs <- vs[,c("Version", "Type", "Country", "Notes")]
  } else if (all(references %in% c("PubMed", "DOI", "ISBN", "ExternalURL", "Notes"))) {
    vs <- vs[,c("Version", "Type", "Country", references, "Notes")]
  } else {
    stop("One or more reference columns not found. Valid options are one or more of PubMed, DOI, ISBN and ExternalURL.")
  }
  
  if(all(is.na(vs$Notes))) {
    vs <- vs[,!(names(vs) %in% "Notes")]
  }
  
  vs
}
