#' Plot Health State Density Curve (HSDC)
#'
#' Generate Health State Density Curve for an EQ-5D dataset. Health State 
#' Density Index is also calculated representing equality/inequality. A group 
#' function can be provided to plot multiple curves by group.
#'
#' @param data A data.frame with columns MO, SC, UA, PD and AD representing
#'   Mobility, Self-care, Usual activities, Pain/discomfort and Anxiety/depression 
#'   or a "State" column containing five digit scores. Alternatively a vector of 
#'   five digit scores can also be used. Additional columns can be included for 
#'   producing HSDCs by a grouping variable.
#' @param version string of value "3L", "5L" or "Y3L" to indicate instrument 
#'  version.
#' @param group string (optional) referencing a data.frame column to group data
#'  by (default=NULL)
#' @param background boolean include background and gridlines.
#' @return a ggplot object.
#' @examples
#' data <- read.csv(system.file("extdata", "eq5d3l_example.csv", package="eq5d"))
#' hsdc_plot(data = data, version = "3L")
#' hsdc_plot(data = data, version = "3L", group="Group")
#'
#' @export
hsdc_plot <- function(data, version, group=NULL, background=TRUE) {
  
  if (!is.null(version) && version == "Y") {
    lifecycle::deprecate_soft("0.15.4", I('Setting `version = "Y"`'), I('`version = "Y3L"`'))
    version <- "Y3L"
  }
  
  if(is.null(group)) {
    
    hsdi <- hsdi(data, version=version)
    
    res <- eq5dcf(data, version=version, proportions = TRUE)
    res$CumulativeState <- 1:nrow(res)/nrow(res)
    
    p <- ggplot(res, aes(.data[["CumulativeProp"]], .data[["CumulativeState"]])) + 
      geom_line(color="#FF9999") + 
      annotate("segment", x=0, y=0, xend=1,yend=1, colour="black") +  
      annotate("text", x=0.5, y=0.9, label=paste0("HSDI=", hsdi)) +
      theme(panel.border = element_blank(), panel.grid.minor = element_blank()) +
      coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
      xlab("Cumulative proportion of observations") +
      ylab("Cumulative proportion of profiles")
    
  } else {
    
    hsdi <- sapply(unique(data[[group]]), function(x){
      hsdi(data[which(data[[group]]==x),], version=version)
    })
    
    names(hsdi) <- unique(data[[group]])
    label <- paste("HSDI:", paste(names(hsdi), hsdi, sep="=", collapse = ", "))
    
    res <- lapply(unique(data[[group]]), function(x){
      cf <- eq5dcf(data[which(data[[group]]==x),], version=version, proportions = TRUE)
      cf$CumulativeState <- 1:nrow(cf)/nrow(cf)
      cf[,group] <- x
      cf
    })
    
    res <- do.call(rbind, res)
    
    group <- sym(group)
    
    p <- ggplot(res, aes(.data[["CumulativeProp"]], .data[["CumulativeState"]], colour=!!group, group=!!group)) +
      geom_line() +
      annotate("segment", x=0, y=0, xend=1,yend=1, colour="black") +
      annotate("text", x=0.5, y=0.9, label=label, size=3) +
      theme(panel.border = element_blank(), panel.grid.minor = element_blank()) +
      coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
      #scale_fill_manual(values=colours) +
      #scale_color_manual(values=colours) +
      xlab("Cumulative proportion of observations") +
      ylab("Cumulative proportion of profiles")
    
  }
  
  if(!background) {
    p <- p + theme(
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(),
      panel.background = element_blank(), 
      axis.line = element_line(colour = "black"))
  }
    
  return(p)
}   

