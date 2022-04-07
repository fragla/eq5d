#' Calculate utility index scores by mapping between EQ-5D-3L and EQ-5D-5L
#'
#' Conditional prediction of the utility values of 5L scores onto 3L value 
#' sets and 3L scores onto 5L value sets from observed or specified values 
#' conditional on age and gender using the NICE Decision Support Unit's 
#' models (see \href{https://nicedsu.sites.sheffield.ac.uk/methods-development/mapping-eq-5d-5l-to-3l}{NICE DSU}'s
#' website for more information).
#'
#' Available value sets can be viewed using the function \code{valuesets}.
#'
#' @param scores numeric with names MO, SC, UA, PD and AD representing
#'   Mobility, Self-care, Usual activities, Pain/discomfort and Anxiety/depression. 
#'   or a utility index score
#' @param country value set country
#' @param version string of value "3L" or "5L" to indicate starting instrument version.
#' @param age age in years (18-100), or age category (1: 18-34, 2: 35-44, 3: 45-54, 4: 55-64, 5: 65-100)
#' @param sex Male or Female
#' @param bwidth bandwith score for approximate scores (< 0.8: 0.2, 0.8-0.951: 0.1, 0.951-1: small, but large enough to include 1)
#' @return calculated utility index score.
#' @examples
#' eq5dmap(c(MO=1,SC=2,UA=3,PD=4,AD=5), "UK", "5L", 30, "female")
#' eq5dmap(0.922, "UK", "5L", 18, "male")
#' eq5dmap(0.715, "UK", "5L", 50, "male", bwidth = 0.0001)
#'
#' @export
eq5dmap <- function(scores, country, version, age, sex, bwidth=0) {

  if(version=="3L") {
    survey <- DSU3L
  } else if(version=="5L") {
    survey <- DSU5L
  } else {
    stop("Version must be either 3L or 5L.")
  }

  if(is.null(country) || !country %in% colnames(survey)) {
    countries <- sub("Copula", "", grep("Copula", sort(colnames(survey)), value=TRUE))
    stop(paste0("For mapping from EQ-5D-", version," country must be one of: ", paste(countries, collapse=", ")))
  }
  
  if(all(.getDimensionNames() %in% names(scores))) {
    if(!all(scores %in% 1:.getNumberLevels(version))) {
      stop(paste0("Scores must be in the range 1 to ", .getNumberLevels(version), " for EQ-5D-", version,"."))
    }
  } else if (is.double(scores)) {
    range <- .getDSURange(country, version)
    if(!(scores >= range[1] && scores <= range[2])) {
      stop(paste0("Index scores must be in the range ", range[1], " to ", range[2], " for ", country, " EQ-5D-", version,"."))
    }
  } else {
    stop("Invalid EQ-5D score.")
  }
  
  age.grp <- .getAgeGroup(age)
  if(is.na(age.grp)) {
    stop("Age must be between 18 and 100, or an age category between 1 and 5.")
  }
  
  sex <- .getSex(sex)
  if(is.na(sex)) {
    stop("Sex must be Male, Female, M or F (case insensitive).")
  }
  
  bwidth <- as.numeric(bwidth)
  if(is.na(bwidth) || bwidth < 0) {
    stop("bwidth must be a number >= 0.")
  }
  
  if(all(.getDimensionNames() %in% names(scores))) {
    state <- paste(scores, collapse = "")
    idx <- which(survey$State==state & survey$Age==age.grp & survey$Sex==sex)
    index <- round(survey[idx, paste0(country,"Copula"), drop=TRUE],3)
    return(index)
  } else if (is.numeric(scores)) {
    if(bwidth==0) {
      idx <- which(survey[[country]]==scores & survey[["Age"]]==age.grp & survey[["Sex"]]==sex)
      if(length(idx)==0) {
        stop("Invalid utility score provided. If approximate score please supply bwidth value")
      } else {
        m <- round(mean(survey[idx, paste0(country,"Copula"), drop=TRUE]),3)
      }
      return(m)
    } else {
      idx <- which(survey$Age==age.grp & survey$Sex==sex)
      epan <- .epan(target=scores, values=survey[idx, country], bwidth=bwidth)
      wm <- round(weighted.mean(x=survey[idx, paste0(country,"Copula")], w=epan),3)
      return(wm)
    }
  }
}

.epan = function(target, values, bwidth){ # Generate Epan weight
  distance = values - target # Calculate distance from input
  epan <- ifelse(abs(distance) >= bwidth, 0, 1 - (distance/bwidth)^2)
  return(epan)
}

.getAgeGroup <- function(age) {
  age.groups <- c("18-34","35-44","45-54","55-64","65+")
  age <- suppressWarnings(as.numeric(age))
  if(age >= 18 & age < 35) {
    return(age.groups[1])
  } else if(age >= 35 & age < 45) {
    return(age.groups[2])
  } else if(age >= 45 & age < 55) {
    return(age.groups[3])
  } else if(age >= 55 & age < 65) {
    return(age.groups[4])
  } else if(age >= 65 & age <= 100) {
    return(age.groups[5])
  } else if(age >= 1 & age <= 5) {
    return(age.groups[age])
  } else {
    return(NA)
  }
}

.getSex <-function(sex) {
  sex <- tolower(sex)
  if(sex %in% c("m","male")) {
    return("male")
  } else if (sex %in% c("f","female")) {
    return("female")
  } else {
    return(NA)
  }
}

.getDSURange <- function(country, version) {
  if(!version %in% c("3L", "5L"))
    stop("Version must be either 3L or 5L.")
  
  if(version=="3L") {
    return(DSU3LRANGE[[country]])
  } else {
    return(DSU5LRANGE[[country]])
  }
  # survey <- get(paste0("DSU",version))
  # range <- range(survey[[country]])
  # return(range)
}

.isValidUtility <- function(scores, country, version) {
  survey <- get(paste0("DSU",version))
  idx <- which(survey[[country]]==scores)
  return(length(idx)>0)
}