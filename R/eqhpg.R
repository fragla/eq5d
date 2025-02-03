#' Calculate the Health Profile Grid
#' 
#' Calculate the Health Profile Grid (HPG) for two EQ-5D datasets.
#' 
#' @param pre data.frame, numeric or character. For data.frame default column 
#' names should be MO, SC, UA, PD and AD representing Mobility, Self-care, 
#' Usual activities, Pain/discomfort and Anxiety/depression. Vector using five 
#' digit format can also be used.
#' @param post data.frame, numeric or character. For data.frame default column 
#' names should be MO, SC, UA, PD and AD representing Mobility, Self-care, 
#' Usual activities, Pain/discomfort and Anxiety/depression. Vector using five 
#' digit format can also be used.
#' @param country string of value set country name used.
#' @param version string of value "3L" or "5L" to indicate instrument version.
#' @param type string specifying method type used in deriving value set scores.
#' Options are TTO or VAS for EQ-5D-3L, VT for EQ-5D-5L, CW for EQ-5D-5L
#' crosswalk conversion valuesets, RCW for EQ-5D-3L reverse crosswalk
#' conversion valuesets.
#' @param ignore.invalid boolean whether to ignore invalid scores. TRUE returns NA, FALSE 
#' throws an error.
#' @param dimensions character vector, specifying "dimension" column names. Defaults 
#' are "MO", "SC", "UA", "PD" and "AD".
#' @param no.problems boolean. Summarise 11111 "No change" subjects in a "No problems" 
#' group.
#' @return a data.frame or list of data.frames containing the columns Pre, Post and PCHC. 
#' Pre and Post contain the severity rankings and PCHC the PCHC category.
#' @examples
#' dat <- read.csv(system.file("extdata", "eq5d3l_example.csv", package="eq5d"))
#' 
#' pre <- dat[dat$Group=="Group1",][1:50,]
#' post <- dat[dat$Group=="Group2",][1:50,]
#' res <- hpg(pre, post, country="UK", version="3L", type="TTO")
#' head(res)
#' 
#' @export
hpg <- function(pre, post, country=NULL, version=NULL, type=NULL, ignore.invalid=TRUE, dimensions=.get_dimension_names(), no.problems=TRUE) {
  
  if (!is.null(version) && version == "Y") {
    lifecycle::deprecate_soft("0.15.4", I('Setting `version = "Y"`'), I('`version = "Y3L"`'))
    version <- "Y3L"
  }
  
  if(is.null(version) || !version %in% c("3L", "5L", "Y3L"))
    stop("EQ-5D version not one of 3L, 5L or Y3L.")
  
  if(is.character(pre) || is.numeric(pre)) {
    pre <- get_dimensions_from_health_states(pre, version=version, ignore.invalid=ignore.invalid)
  }
  
  if(is.character(post) || is.numeric(post)) {
    post <- get_dimensions_from_health_states(post, version=version, ignore.invalid=ignore.invalid)
  }
  
  if(all(dimensions %in% names(pre)) && all(dimensions %in% names(post))) {
    pre <- pre[,dimensions]
    colnames(pre) <- .get_dimension_names()
    post <- post[,dimensions]
    colnames(post) <- .get_dimension_names()
  } else {
    stop("Unable to identify EQ-5D dimensions in data.frames.")
  }
  
  if(nrow(pre)!=nrow(post)) {
    stop("Different numbers of health states in pre and post.")
  }

  #calculate PCHC to be used for colouring
  pchc <- pchc(pre, post, version=version, no.problems=no.problems, totals=FALSE, summary=FALSE)
  
  #convert pre/post scores to index utility scores
  pre <- eq5d(scores=pre, version = version, type = type, country = country, ignore.invalid=TRUE)
  post <- eq5d(scores=post, version = version, type = type, country = country, ignore.invalid=TRUE)
  
  utilities <- sort(eq5d(scores=get_all_health_states(version = version), version = version, type = type, country = country), decreasing=TRUE)
  
  pre.match <- sapply(pre, function(x){if(length(wm <- which.min(abs(utilities-x)))) wm else NA})
  post.match <- sapply(post, function(x){if(length(wm <- which.min(abs(utilities-x)))) wm else NA})
  
  dat <- data.frame(Pre=pre.match, Post=post.match, PCHC=pchc)
  
  return(dat)
}
