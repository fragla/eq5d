#' Calculate Shannon's Index
#' 
#' Calculate Shannon's H' (diversity) index, H' max and Shannon's J' (evenness) 
#' index for an EQ-5D data set. This can be calculated both by dimension and 
#' for health states as a whole.
#' 
#' @param scores data.frame, numeric or character. For data.frame default column 
#' names should be MO, SC, UA, PD and AD representing Mobility, Self-care, 
#' Usual activities, Pain/discomfort and Anxiety/depression. Vector using five 
#' digit format can also be used.
#' @param version string of value "3L" or "5L" to indicate instrument version.
#' @param by.dimension boolean whether to calculate scores by EQ-5D dimensions 
#' or for the whole dataset. Defaults to TRUE.
#' @param ignore.invalid boolean whether to ignore invalid scores. TRUE returns NA, FALSE 
#' throws an error.
#' @param dimensions character vector, specifying "dimension" column names. Defaults 
#' are "MO", "SC", "UA", "PD" and "AD".
#' @param base numeric base of logarithm to use. Defaults to base 2.
#' @param digits numeric specifying the number of decimal places. Defaults to 2.
#' @param permutations boolean whether to use maximum number of permutations for 
#' H' max or the number of observed unique profiles. Default is TRUE.
#' @return a single list or list of dimensions containing H' H' max and J' scores.
#' @examples
#' dat <- read.csv(system.file("extdata", "eq5d3l_example.csv", package="eq5d"))
#' 
#' shannon(dat, version="3L", by.dimension=FALSE)
#' shannon(dat, version="3L", by.dimension=TRUE)
#' 
#' @export
shannon <- function(scores, version=NULL, by.dimension=TRUE, ignore.invalid=TRUE, dimensions=.get_dimension_names(), base=2, digits=2, permutations=TRUE) {
  
  if (!is.null(version) && version == "Y") {
    lifecycle::deprecate_soft("0.15.4", I('Setting `version = "Y"`'), I('`version = "Y3L"`'))
    version <- "Y3L"
  }
  
  if(is.null(version) || !version %in% c("3L", "5L", "Y3L"))
    stop("EQ-5D version not one of 3L, 5L or Y3L.")
  
  if(is.character(scores) || is.numeric(scores)) {
    scores <- get_dimensions_from_health_states(scores, version=version, ignore.invalid=ignore.invalid)
  }
  
  if(all(dimensions %in% names(scores))) {
    scores <- scores[,dimensions]
    colnames(scores) <- .get_dimension_names()
  } else {
    stop("Unable to identify EQ-5D dimensions in data.frames.")
  }
  
  scores.idx <- which(apply(scores, 1, function(x) { any(!x%in% 1:.get_number_levels(version))}))
  
  if(length(scores.idx)>0) {
    if(ignore.invalid) {
      scores[scores.idx,] <- NA
    } else {
      stop("Missing/non-numeric dimension found.")
    }
  }
  
  if(!by.dimension) {
    scores <- get_health_states_from_dimensions(scores)
    max.levels <- ifelse(permutations, .get_number_levels(version)^5, length(unique(na.omit(scores))))
    res <- .shannon(scores, max.levels, base, digits)
  } else {
    res <- lapply(.get_dimension_names(), function(x) {
      max.levels <- ifelse(permutations, .get_number_levels(version), length(unique(na.omit(scores[[x]]))))
      .shannon(scores[,x, drop=FALSE], max.levels, base, digits)
    })
    names(res) <- .get_dimension_names()
  }
  
  return(res)

}

.shannon <- function(scores, max.levels, base=2, digits=2) {

  #H'max based on max levels or permutations of health states
  H.max <- log(max.levels, base)
  
  #Totals of each level
  t <- table(scores)
  
  #Total number subjects
  n <- sum(t)
  
  #Proportions of each level
  p <- t  / n
  
  #H' - Shannon index
  H <- -sum(p * log(p, base))
  
  #J' - Shannon’s Evenness index 
  J <- H / H.max
  
  list(H=round(H, digits), H.max=round(H.max, digits), J=round(J, digits))
}
