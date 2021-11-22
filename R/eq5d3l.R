#' Calculate EQ-5D-3L index scores
#'
#' Calculate indices for EQ-5D-3L value sets. Available value sets can be viewed
#'   using the function \code{valuesets}.
#'
#' @param scores numeric with names MO, SC, UA, PD and AD representing
#'   Mobility, Self-care, Usual activities, Pain/discomfort and Anxiety/depression.
#' @param type 3L values set type. Either TTO or VAS.
#' @param country value set country.
#' @return calculated utility index score.
#' @examples
#' eq5d3l(scores=c(MO=1,SC=2,UA=3,PD=1,AD=3), type="VAS", country="UK")
#' eq5d3l(scores=c(MO=3,SC=2,UA=3,PD=2,AD=3), type="TTO", country="Germany")
#'
#' @export
eq5d3l <- function(scores, type="TTO", country="UK") {

  if(!all(.getDimensionNames() %in% names(scores))) {
    stop("Unable to identify EQ-5D dimensions in scores.")
  }

  if(!all(scores %in% 1:3)) {
    message <- "Scores must be coded as 1, 2 or 3 for EQ-5D-3L."
    if(all(scores %in% 1:5)) {
      message <- paste(message, "Are you using EQ-5D-5L?")
    }
    stop(message)
  }


  if(!type %in% c("TTO", "VAS"))
    stop("Valuation type must be one of TTO or VAS.")

  survey <- get(type)

  if(is.null(country) || !country %in% colnames(survey))
    stop(paste("For EQ-5D-3L", type, "value sets country must be one of:", paste(colnames(survey), collapse=", ")))

  survey <- setNames(survey[[country]], rownames(survey))

  values <- c(survey["FullHealth"], .minOne2Or3(scores, survey), .minOne3(scores, survey), .dimensionScores(scores, survey), .ordinalScore(scores, survey), .interactions(scores, survey))
  index <- NULL

  if(type=="VAS" && country=="Germany") {
    index <- .eq5d3l.mult(values)
  } else if(country=="Australia" && type=="TTO" && .collapseScore(scores) %in% names(.australiaImplausible())) {
    index <- as.numeric(.australiaImplausible()[[.collapseScore(scores)]])
  } else {
    index <- .eq5d3l.add(values)
  }

  return(round(index, digits=3))
}

.eq5d3l.add <- function(values) {
  sum(values, na.rm = TRUE)
}

.eq5d3l.mult <- function(values) {
  1*prod(values, na.rm = TRUE)
}

.minOne2Or3 <- function(scores, survey) {
  ##at least one mobility, care, activity, pain, anxiety > 1
  if(sum(scores) > 5 && !is.na(survey["AtLeastOne2Or3"])) {
    survey["AtLeastOne2Or3"]
  }
}

.minOne3 <- function(scores, survey) {
  ##at least one 3.
  if(any(scores == 3) && !is.na(survey["AtLeastOne3"])) {
    survey["AtLeastOne3"]
  }
}

.dimensionScores <- function(scores, survey) {
  survey[paste0(names(scores), scores)]


}

.ordinalScore <- function(scores, survey) {
  return(
      c(.D1(scores) * survey["D1"],
      .I2(scores) * survey["I2"],
      .I2Square(scores) * survey["I2square"],
      .I3(scores) * survey["I3"],
      .I3Square(scores) * survey["I3square"],
      .O2(scores) * survey["O2"],
      .O3(scores) * survey["O3"],
      .C2Square(scores) * survey["C2square"],
      .C3Square(scores) * survey["C3square"],
      .X5(scores) * survey["X5"],
      .Z2(scores) * survey["Z2"],
      .Z3(scores) * survey["Z3"])
  )
}

.D1 <- function(scores) {
  x <- 0
  if(!is.na(scores["MO"]) && scores["MO"] > 1) x <- x + 1
  if(!is.na(scores["SC"]) && scores["SC"] > 1) x <- x + 1
  if(!is.na(scores["UA"]) && scores["UA"] > 1) x <- x + 1
  if(!is.na(scores["PD"]) && scores["PD"] > 1) x <- x + 1
  if(!is.na(scores["AD"]) && scores["AD"] > 1) x <- x + 1
  x <- x - 1
  ifelse(x > 0, return(x), return(0))}

.I2 <- function(scores) {
  x <- 0
  if(!is.na(scores["MO"]) && scores["MO"] == 2) x <- x + 1
  if(!is.na(scores["SC"]) && scores["SC"] == 2) x <- x + 1
  if(!is.na(scores["UA"]) && scores["UA"] == 2) x <- x + 1
  if(!is.na(scores["PD"]) && scores["PD"] == 2) x <- x + 1
  if(!is.na(scores["AD"]) && scores["AD"] == 2) x <- x + 1
  x <- x - 1
  ifelse(x > 0, return(x), return(0))
}

.I2Square <- function(scores) {
  .I2(scores)^2
}

.I3 <- function(scores) {
  x <- 0
  if(!is.na(scores["MO"]) && scores["MO"] == 3) x <- x + 1
  if(!is.na(scores["SC"]) && scores["SC"] == 3) x <- x + 1
  if(!is.na(scores["UA"]) && scores["UA"] == 3) x <- x + 1
  if(!is.na(scores["PD"]) && scores["PD"] == 3) x <- x + 1
  if(!is.na(scores["AD"]) && scores["AD"] == 3) x <- x + 1
  x <- x - 1
  ifelse(x > 0, return(x), return(0))
}

.I3Square <- function(scores) {
  return(.I3(scores)^2)
}

.C2 <- function(scores) {
  x <- 0
  if(!is.na(scores["MO"]) && scores["MO"] == 2) x <- x + 1
  if(!is.na(scores["SC"]) && scores["SC"] == 2) x <- x + 1
  if(!is.na(scores["UA"]) && scores["UA"] == 2) x <- x + 1
  if(!is.na(scores["PD"]) && scores["PD"] == 2) x <- x + 1
  if(!is.na(scores["AD"]) && scores["AD"] == 2) x <- x + 1

  return(x)
}

.C2Square <- function(scores) {
  return(.C2(scores)^2)
}

.C3 <- function(scores) {
  x <- 0
  if(!is.na(scores["MO"]) && scores["MO"] == 3) x <- x + 1
  if(!is.na(scores["SC"]) && scores["SC"] == 3) x <- x + 1
  if(!is.na(scores["UA"]) && scores["UA"] == 3) x <- x + 1
  if(!is.na(scores["PD"]) && scores["PD"] == 3) x <- x + 1
  if(!is.na(scores["AD"]) && scores["AD"] == 3) x <- x + 1

  return(x)
}

.C3Square <- function(scores) {
  return(.C3(scores)^2)
}

.O2 <- function(scores){
  if (setequal(scores, c(1,2)) || all(scores==2)) 1 else 0
}

.O3 <- function(scores){
  if (setequal(scores, c(1,3)) || all(scores==3)) 1 else 0
}

.X5 <- function(scores) {
  x5 <- all(scores %in% c(2,3))
  return(ifelse(x5, 1, 0))
}

.Z2 <- function(scores) {
  #at least one dimension at level 2 and one dimension at level 3
  z2 <- any(scores == 2) & any(scores==3)
  return(ifelse(z2, 1, 0))
}

.Z3 <- function(scores) {
  #number of dimensions at level 2 given at least one dimension at level 3
  x <- 0
  if(any(scores==3)) {
    x <- sum(scores==2)
  }
  return(x)
}

.interactions <- function(scores, survey) {
  INTERACTIONS <- c("MO2SC2", "MO2SC3", "MO2UA2", "MO2UA3", "MO2PD2", "MO2PD3",
                    "MO2AD2", "MO2AD3", "MO3SC3", "MO3UA3", "MO3PD2", "MO3PD3",
                    "MO3AD2", "MO3AD3", "SC2UA2", "SC2UA3", "SC2PD2", "SC2PD3",
                    "SC2AD2", "SC2AD3", "SC3UA2", "SC3UA3", "SC3PD2", "SC3PD3",
                    "SC3AD2", "SC3AD3", "UA2PD2", "UA2PD3", "UA2AD2", "UA2AD3",
                    "UA3PD2", "UA3PD3", "UA3AD2", "UA3AD3", "PD2AD2", "PD2AD3",
                    "PD3AD2", "PD3AD3")

  score.dimensions <- paste0(names(scores), scores)
  interactions <- INTERACTIONS[which(!is.na(survey[INTERACTIONS]))]

  if(length(interactions) > 0) {
    interaction.pairs <- sapply(interactions, function(x) {
      pairs <- strsplit(gsub("([[:digit:]])([[:upper:]])", "\\1 \\2", x, " ")," ")
      lapply(pairs, function(y){
        all(y %in% score.dimensions)
      })
    })
    interaction.pairs <- unlist(interaction.pairs)
    interaction.present <- which(interaction.pairs)

    if(length(interaction.present) > 0) {
      return(survey[names(interaction.present)])
    }
  }
}

.australiaImplausible <- function() {
  implausible <- c(0.154, 0.101, 0.154, 0.101, 0.02, 0.02, 0.086, 0.033, 0.086, 0.033, -0.048, -0.048, -0.083, -0.136, -0.206, -0.045, -0.083, -0.098, -0.136, -0.199, -0.217, -0.217)
  names(implausible) <- c("12133", "12233", "13133", "13233", "13332", "13333", "22133", "22233", "23133", "23233", "23332", "23333", "32133", "32233", "32333", "33132", "33133", "33232", "33233", "33323", "33332", "33333")
  return(implausible)
}

.collapseScore <- function(score) {
  return(paste(score, collapse=""))
}
