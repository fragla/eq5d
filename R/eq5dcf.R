#' Calculate the cumulative frequency profile of an EQ-5D dataset
#' 
#' Calculate the frequency, percentage, cumulative frequency and cumulative 
#' percentage for each profile in an EQ-5D dataset.
#' 
#' @param data data.frame with names MO, SC, UA, PD and AD representing
#'   Mobility, Self-care, Usual activities, Pain/discomfort and Anxiety/depression.
#' @param version string of value "3L" or "5L" to indicate instrument version.
#' @param ignore.invalid whether to ignore invalid scores. TRUE returns NA, FALSE throws an 
#' error.
#' @param ... character vector, specifying "dimensions" column names. Defaults 
#' are "MO", "SC", "UA", "PD" and "AD".
#' @return a data.frame or list of data.frames of counts/percentages. Columns 
#' contain dimensions names and rows the EQ-5D score.
#' @export
eq5dcf <- function(data, version, ignore.invalid, ...) {
  UseMethod("eq5dcf", data)
}

#' @export
eq5dcf.data.frame <- function(data, version, ignore.invalid=TRUE, ...) {
  args <- list(...)
  
  dimensions <- .getDimensionNames()
  if(!is.null(args$dimensions)) {dimensions <- args$dimensions}
  
  states <- getHealthStatesFromDimensions(data, version, ignore.invalid, dimensions)
  eq5dcf.default(states, version, ignore.invalid, ...)
}

#' @export
eq5dcf.matrix <- function(data, version, ignore.invalid=TRUE, ...) {
  data <- as.data.frame(data)
  eq5dcf.data.frame(data, version, ignore.invalid=TRUE, ...)
}

#' @export
eq5dcf.default <- function(data, version, ignore.invalid=TRUE, ...) {
  invalid.idx <- which(!data %in% getHealthStates(version))
  
  if(length(invalid.idx) > 0) {
    if(ignore.invalid) {
      data[invalid.idx] <- NA
    } else {
      stop("Invalid dimension state(s) found.")
    }
  } 

  #State, Frequency, Percentage, Cumulative frequency, Cumulative percentage
  frequencies <- sort(table(data), decreasing=TRUE)
  percentage <- round(prop.table(as.numeric(frequencies))*100,1)
  cum.freq <- cumsum(as.numeric(frequencies))
  cum.perc <- round(cum.freq/sum(frequencies)*100,1)
  
  return(data.frame(State=names(frequencies), Frequency=as.numeric(frequencies),
                    Percentage=percentage, CumulativeFreq=cum.freq, CumulativePerc=cum.perc, 
                    stringsAsFactors=FALSE))
}

