#' Calculate utility index scores by mapping between EQ-5D-3L and EQ-5D-5L
#'
#' Conditional prediction of the utility values of 5L scores onto 3L value 
#' sets and 3L scores onto 5L value sets from observed or specified values 
#' conditional on age and gender using the NICE Decision Support Unit's 
#' models.
#'
#' Available value sets can be viewed using the function \code{valuesets}.
#'
#' @param scores numeric with names MO, SC, UA, PD and AD representing
#'   Mobility, Self-care, Usual activities, Pain/discomfort and Anxiety/depression.
#' @param country value set country.
#' @param version string of value "3L" or "5L" to indicate starting instrument version.
#' @param age age in years (18-100), or age category (1: 18-34, 2: 35-44, 3: 45-54, 4: 55-64, 5: 65-100)
#' @param sex Male or Female
#' @param bwidth bandwith score
#' @return calculated utility index score.
#' @examples
#' eq5dmap(c(MO=1,SC=2,UA=3,PD=4,AD=5), "UK", "5L", 30, "female")
#' eq5dmap(0.922, "UK", "5L", 18, "male")
#' eq5dmap(0.715, "UK", "5L", 50, "male", bwidth = 0.0001)
#'
#' @export
eq5dmap <- function(scores, country, version, age, sex, bwidth=0) {
  #if(!all(.getDimensionNames() %in% names(scores))) {
  #  stop("Unable to identify EQ-5D dimensions in scores.")
  #}
  
  # if(!all(scores %in% 1:3)) {
  #   message <- "Scores must be coded as 1, 2 or 3 for EQ-5D-3L."
  #   
  #   stop(message)
  # }
  
  if(version=="3L") {
    survey <- get("DSU3L")
  } else if(version=="5L") {
    survey <- get("DSU5L")
  } else {
    stop("Version must be either 3L or 5L.")
  }

  if(is.null(country) || !country %in% colnames(survey))
    stop(paste0("For mapping from EQ-5D-", version," country must be one of:", paste(colnames(survey), collapse=", ")))
  
  age.grp <- .getAgeGroup(age)
  sex <- .getSex(sex)
  
  if(all(.getDimensionNames() %in% names(scores))) {
    state <- paste(scores, collapse = "")
    idx <- which(survey$State==state & survey$Age==age.grp & survey$Sex==sex)
    index <- round(survey[idx, paste0(country,"Copula"), drop=TRUE],3)
    return(index)
  } else if (is.numeric(scores)) {
    if(bwidth==0) {
      idx <- which(survey[[country]]==scores & survey$Age==age.grp & survey$Sex==sex)
      m <- round(mean(survey[idx, paste0(country,"Copula"), drop=TRUE]),3)
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
    return(age.groups[as.double(age)])
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
    stop("Sex not recognised.")
  }
}
