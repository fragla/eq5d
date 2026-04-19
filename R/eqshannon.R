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
shannon <- function(scores, version=NULL, by.dimension=TRUE, ignore.invalid=TRUE, dimensions=NULL, base=2, digits=2, permutations=TRUE) {
  
  if (!is.null(version) && version == "Y") {
    lifecycle::deprecate_soft("0.15.4", I('Setting `version = "Y"`'), I('`version = "Y3L"`'))
    version <- "Y3L"
  }
  
  if(is.null(version) || !version %in% c("3L", "5L", "Y3L"))
    stop("EQ-5D version not one of 3L, 5L or Y3L.", call. = FALSE)
  
  if(is.null(dimensions)) {
    dimensions <- .get_dimension_names()
  }
  
  if(is.character(scores) || is.numeric(scores)) {
    scores <- get_dimensions_from_health_states(scores, version=version, ignore.invalid=ignore.invalid)
  }
  
  if (!is.data.frame(scores) || !all(dimensions %in% names(scores))) {
    stop("Unable to identify EQ-5D dimensions in data.frame.", call. = FALSE)
  }
  
  scores <- scores[, dimensions, drop = FALSE]
  colnames(scores) <- .get_dimension_names()
  
#  scores.idx <- which(apply(scores, 1, function(x) { any(!x%in% 1:.get_number_levels(version))}))
  
  bad <- vapply(seq_len(nrow(scores)),
    function(x) {
      any(!scores[x, ] %in% seq_len(.get_number_levels(version)))
    },
    logical(1)
  )
  
  if (any(bad)) {
    if (ignore.invalid) {
      scores[bad, ] <- NA
    } else {
      stop("Invalid EQ-5D dimension levels detected.", call. = FALSE)
    }
  }
  
  # if(length(scores.idx)>0) {
  #   if(ignore.invalid) {
  #     scores[scores.idx,] <- NA
  #   } else {
  #     stop("Missing/non-numeric dimension found.")
  #   }
  # }
  
  if(!by.dimension) {
    states <- get_health_states_from_dimensions(scores)
    max.levels <- ifelse(permutations, .get_number_levels(version)^5, length(unique(na.omit(states))))
    ent <- .shannon(counts = .profile_counts(states), max.levels = max.levels, base = base)
    
    return(
      data.frame(
        scope = "profile",
        H = round(ent$H, digits),
        H.max = round(ent$H.max, digits),
        J = round(ent$J, digits),
        row.names = NULL
      )
    )
  } else {
    res <- lapply(.get_dimension_names(), function(d) {
      x <- scores[[d]]
      
      max.levels <- ifelse(permutations, .get_number_levels(version), length(unique(na.omit(x))))
      ent <- .shannon(counts = .profile_counts(x), max.levels = max.levels, base = base)
      
      data.frame(
        dimension = d,
        H = round(ent$H, digits),
        H.max = round(ent$H.max, digits),
        J = round(ent$J, digits)
      )
    })
    
    return(do.call(rbind, res))
  }
}

.shannon <- function(counts, max.levels, base=2) {
  n <- sum(counts)
  if (n == 0) {
    return(list(H = NA_real_, H.max = NA_real_, J = NA_real_))
  }
  
  #Proportions of each level
  p <- counts / n
  p <- p[p > 0] # Remove zero proportions to avoid log(0)
  
  #H'max based on max levels or permutations of health states
  H <- -sum(p * log(p, base))
  
  # Maximum entropy
  H.max <- log(max.levels, base)
  
  #J' - Shannon’s Evenness index 
  J <- H / H.max
  
  list(H = H, H.max = H.max, J = J)
}


.profile_counts <- function(x) {
  table(x, useNA = "no")
}
