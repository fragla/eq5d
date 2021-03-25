#' Analyse the descriptive system of an EQ-5D dataset
#' 
#' Analyses the descriptive components of an EQ-5D dataset producing summary 
#' information either as counts or as percentages.
#' 
#' @param data data.frame with names MO, SC, UA, PD and AD representing
#'   Mobility, Self-care, Usual activities, Pain/discomfort and Anxiety/depression.
#' @param version string of value "3L" or "5L" to indicate instrument version.
#' @param counts logical show absolute counts in the summary table. Default is 
#' FALSE, which shows percentages for each EQ-5D dimension.
#' @param by character specifying the column in the data.frame by which to 
#' group the results.
#' @param ... character vector, specifying "dimensions" column names. Defaults 
#' are "MO", "SC", "UA", "PD" and "AD".
#' @return a data.frame or list of data.frames of counts/percentages. Columns 
#' contain dimensions names and rows the EQ-5D score.
#' @examples
#' dat <- data.frame(
#'          matrix(
#'            sample(1:3,5*12, replace=TRUE),12,5, 
#'            dimnames=list(1:12,c("MO","SC","UA","PD","AD"))
#'          ),
#'          Sex=rep(c("Male", "Female"))
#'        )
#' 
#' eq5dds(dat, version="3L")
#' eq5dds(dat, version="3L", counts=TRUE)
#' 
#' eq5dds(dat, version="3L", by="Sex")
#' 
#' @export
eq5dds <- function(data, version, counts=FALSE, by=NULL, ...) {
  args <- list(...)
  
  dimensions <- .getDimensionNames()

  if(!is.null(args$dimensions)) {dimensions <- args$dimensions}

  if(!version %in% c("3L", "5L", "Y"))
    stop("EQ-5D version not one of 3L, 5L or Y.")
  
  if(all(dimensions %in% names(data))) {
    colnames(data)[match(dimensions, colnames(data))] <- .getDimensionNames()
  } else {
    stop("Unable to identify EQ-5D dimensions in data.frame.")
  }
  
  class.check <- sapply(data[.getDimensionNames()], function(x){class(x)!="numeric"})
  if(any(class.check)) {
    data[.getDimensionNames()] <- suppressWarnings(sapply(data[.getDimensionNames()],function(x){as.numeric(as.character(x))}))
  }
  
  if(!is.null(by)) {
    if(by %in% colnames(data)) {
      res <- by(data, data[,by], function(x){eq5dds(x, version=version, counts=counts)})
      return(res)
    } else {
      stop("Unable to identify by column in data.frame.")
    }
  }
  else {
    ##remove missing/incorrect
    max.value <- .getNumberLevels(version)

    df <- as.data.frame(matrix(0, nrow=max.value, ncol=5))
    colnames(df) <- .getDimensionNames()
    
    idx <- apply(data[,.getDimensionNames()], 1, function(x) {all(x %in% 1:max.value)})
    
    data <- data[idx,]
    
    for(i in colnames(df)) {
      ctable <- table(data[,i])
      df[names(ctable),i] <- ctable
    }
    
    if(!counts) {
      df <- as.data.frame(round(prop.table(as.matrix(df), 2)*100,1))
    }
    return(df)
  }
}
