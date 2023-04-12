#' Calculate the cumulative frequency profile of an EQ-5D dataset
#' 
#' Calculate the frequency, percentage, cumulative frequency and cumulative 
#' percentage for each profile in an EQ-5D dataset.
#' 
#' @param data A data.frame with columns MO, SC, UA, PD and AD representing
#'   Mobility, Self-care, Usual activities, Pain/discomfort and Anxiety/depression 
#'   or a "State" column containing five digit scores. Alternatively a vector of 
#'   five digit scores can also be used. 
#' @param version string of value "3L" or "5L" to indicate instrument version.
#' @param ignore.invalid booloean whether to ignore invalid scores. TRUE returns NA, FALSE throws an 
#' error.
#' @param proportions boolean whether to include proportion data columns Proportions and
#' CumulativeProp. Default is FALSE.
#' @param digits  numeric specifying the number of decimal places for percentages. Defaults to 1.
#' @param ... character vector, specifying "dimensions" column names. Defaults 
#' are "MO", "SC", "UA", "PD" and "AD".
#' @return a data.frame or list of data.frames of counts/percentages. Columns 
#' contain dimensions names and rows the EQ-5D score.
#' @examples
#' dat <- read.csv(system.file("extdata", "eq5d3l_example.csv", package="eq5d"))
#' eq5dcf(dat, "3L")
#' 
#' @export
eq5dcf <- function(data, version, ignore.invalid, proportions, digits, ...) {
  UseMethod("eq5dcf", data)
}

#' @export
eq5dcf.data.frame <- function(data, version, ignore.invalid=TRUE, proportions=FALSE, digits=1, ...) {
  args <- list(...)
  
  dimensions <- .getDimensionNames()
  five.digit <- "State"
  
  if(!is.null(args$dimensions)) {dimensions <- args$dimensions}
  if(!is.null(args$five.digit)) {five.digit <- args$five.digit}
  
  if(all(dimensions %in% names(data))) {
    states <- getHealthStatesFromDimensions(data, version, ignore.invalid, dimensions)
  } else if(five.digit %in% names(data)) {
    states <- data[[five.digit]]
  } else {
    stop("Unable to identify EQ-5D dimensions in data.frame.")
  }
  eq5dcf.default(states, version, ignore.invalid, proportions, digits=digits, ...)
}

#' @export
eq5dcf.matrix <- function(data, version, ignore.invalid=TRUE, proportions=FALSE, digits=1, ...) {
  data <- as.data.frame(data)
  eq5dcf.data.frame(data, version, ignore.invalid, proportions, digits, ...)
}

#' @export
eq5dcf.default <- function(data, version, ignore.invalid=TRUE, proportions=FALSE, digits=1, ...) {
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
  percentage <- prop.table(as.numeric(frequencies)) * 100
  cum.freq <- cumsum(as.numeric(frequencies))
  cum.perc <- cum.freq/sum(frequencies) * 100
  
  if(proportions) {
    prop <- prop.table(as.numeric(frequencies))
    cum.prop <- cum.freq/sum(frequencies)
  }
  
  if(!is.null(digits)) {
    percentage <- round(percentage, digits)
    cum.perc <- round(cum.perc, digits)
  }
  
  if(proportions) {
    cf <- data.frame(State=names(frequencies), Frequency=as.numeric(frequencies),
               Percentage=percentage, Proportions=prop, CumulativeFreq=cum.freq, CumulativePerc=cum.perc, 
               CumulativeProp=cum.prop, stringsAsFactors=FALSE)
  } else {
    cf <- data.frame(State=names(frequencies), Frequency=as.numeric(frequencies),
                     Percentage=percentage, CumulativeFreq=cum.freq, CumulativePerc=cum.perc, 
                     stringsAsFactors=FALSE)
  }
  
  return(cf)
}

