#' Analyse the descriptive system of an EQ-5D dataset
#' 
#' Analyses the descriptive components of an EQ-5D dataset producing summary 
#' information either as counts or as percentages.
#' 
#' @param data numeric or data.frame with names/colnames MO, SC, UA, PD and AD
#' representing Mobility, Self-care, Usual activities, Pain/discomfort and
#' Anxiety/depression. Alternatively an EQ-5D  score can be provided in
#' five digit format e.g. 12321.
#' @param version string of value "3L" or "5L" to indicate instrument version.
#' @param counts logical show absolute counts in the summary table. Default is 
#' FALSE, which shows percentages for each EQ-5D dimension.
#' @param by character specifying the column in the data.frame by which to 
#' group the results.
#' @param ignore.invalid boolean whether to ignore invalid scores. TRUE returns NA, FALSE 
#' throws an error.
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
eq5dds <- function(data, version, counts=FALSE, by=NULL, ignore.invalid=TRUE, ...) {
  args <- list(...)
  
  dimensions <- .get_dimension_names()
  five.digit <- "State"

  if(!is.null(args$dimensions)) {dimensions <- args$dimensions}
  if(!is.null(args$five.digit)) {five.digit <- args$five.digit}

  if(!version %in% c("3L", "5L", "Y"))
    stop("EQ-5D version not one of 3L, 5L or Y.")
  
  if(all(dimensions %in% names(data))) {
    colnames(data)[match(dimensions, colnames(data))] <- .get_dimension_names()
  } else if(five.digit %in% names(data)) {
    data <- cbind(data, get_dimensions_from_health_states(data[[five.digit]], version=version, ignore.invalid=ignore.invalid))
  } else if(is.character(data) || is.numeric(data)) {
    data <- get_dimensions_from_health_states(data, version=version, ignore.invalid=ignore.invalid)
  } else {
    stop("Unable to identify EQ-5D dimensions in data.frame.")
  }
  
  class.check <- sapply(data[.get_dimension_names()], function(x){class(x)!="numeric"})
  if(any(class.check)) {
    data[.get_dimension_names()] <- suppressWarnings(sapply(data[.get_dimension_names()],function(x){as.numeric(as.character(x))}))
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
    max.value <- .get_number_levels(version)

    df <- as.data.frame(matrix(0, nrow=max.value, ncol=5))
    colnames(df) <- .get_dimension_names()
    
    idx <- apply(data[,.get_dimension_names()], 1, function(x) {all(x %in% 1:max.value)})
    
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
