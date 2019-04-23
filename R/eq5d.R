#' Calculate EQ-5D indices.
#' 
#' Calculate EQ-5D indices for EQ-5D-3L and EQ-5D-5L. Available value sets can be seen 
#'   using the function \code{valuesets}
#' 
#' @param scores numeric or data.frame with names/colnames Mobility, Care, Activity, Pain and Anxiety.
#' @param version string of value "3L" or "5L" to indicate instrument version. 
#' @param type string specifying method type used in deriving value set scores. Options 
#'   are TTO or VAS for EQ-5D-3L and VT for EQ-5D-5L.
#' @param country string of value set country name used.
#' @examples
#' eq5d(scores=c(Mobility=1,Care=2,Activity=3,Pain=4,Anxiety=5), 
#'  country="Indonesia", version="5L")
#' eq5d(scores=c(Mobility=3,Care=2,Activity=3,Pain=2,Anxiety=3), 
#'  type="TTO", version="3L", country="Germany")
#' 
#' test.df <- data.frame(
#'   Mobility=c(1,2,3,4,5), Care=c(1,5,4,3,2),
#'   Activity=c(1,5,2,3,1), Pain=c(1,3,4,3,4), Anxiety=c(1,2,1,2,1)
#'   )
#' eq5d(test.df, country="Canada", version="5L")
#'
#' @export
eq5d <- function (scores, version, type, country) {
  UseMethod("eq5d", scores)
}

#' @export
eq5d.numeric <- function(scores, version, type, country) {
  
  if(!all(names(scores) %in% c("Mobility", "Care", "Activity", "Pain", "Anxiety"))) {
    stop("Unable to identify EQ-5D dimensions in scores.")
  }

  if(!version %in% c("3L", "5L"))
    stop("EQ-5D version not one of 3L or 5L.")

  # if(version=="3L" && !type %in% c("TTO", "VAS"))
  #   stop("Valuation type must be one of TTO or VAS.")
  # 
  # if(!all(scores %in% 1:3) && version=="3L")
  #   stop("Scores must be coded as 1, 2 or 3 for EQ-5D-3L")
  # 
  # if(!all(scores %in% 1:5) && version=="5L")
  #   stop("Scores must be coded as 1, 2, 3, 4 or 5 for EQ-5D-5L")

  if(version=="3L") {
    eq5d3l(scores, type=type, country=country)
  } else {
    eq5d5l(scores, country=country)
  }
}

#' @export
eq5d.data.frame <- function(scores, version, type, country) {
  indices <- sapply(1:nrow(scores), function(x) {
    eq5d.numeric(scores[x,], version=version, type=type, country=country)
  })
  
  return(indices)
}

#' Get the available EQ-5D value sets
#' 
#' \code{valuesets} returns a data.frame of the available EQ-5D value sets
#'     in the \code{eq5d} package.
#' 
#' @return A data.frame countaining the EQ-5D version, value set type and country
#' @examples
#' valuesets()
#' @export
valuesets <- function() {
  tto <- data.frame(Version="EQ-5D-3L", Type="TTO", Country=colnames(get("TTO")))
  vas <- data.frame(Version="EQ-5D-3L", Type="VAS", Country=colnames(get("VAS")))
  vt <- data.frame(Version="EQ-5D-5L", Type="VT", Country=colnames(get("VT")))
  rbind(tto, vas, vt)
}
