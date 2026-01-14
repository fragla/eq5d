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

#########plotted against the LFS


#' Generate Severity Summary Plot for EQ-5D Index Values
#'
#' Creates a plot summarizing EQ-5D index values by severity levels using either
#' Level Frequency Score (LFS) or Level Severity Score (LSS). The plot displays
#' the lowest, median, and highest index values for each severity level, with
#' error bars and color-coded points.
#'
#' @param data A data frame containing EQ-5D responses.
#' @param country A character string specifying the country for value set calculation.
#' @param version A string indicating the EQ-5D version. Must be one of "3L", "5L", or "Y3L".
#' @param type A string string specifying method type used in deriving value set scores.
#' @param severity A character string specifying the severity metric. Must be one of
#'   "LFS" (Level Frequency Score) or "LSS" (Level Severity Score). Defaults to "LFS".
#'
#' @return a ggplot object.
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' data <- read.csv(system.file("extdata", "eq5d3l_example.csv", package="eq5d"))
#' p <- severity_summary_plot(data = data,
#'                            country = "UK",
#'                            version = "5L",
#'                            type = "TTO",
#'                            severity = "LFS")
#' print(p)
#' }
#'
#' @import ggplot2
#' @importFrom stats median
#' @export

severity_summary_plot <- function(data, country, version, type, severity = c("LFS", "LSS")) {
  # --- Validate & normalize severity argument ---
  severity <- match.arg(severity)
  
  # --- Handle legacy version 'Y' => 'Y3L' ---
  if (!is.null(version) && version == "Y") {
    lifecycle::deprecate_soft("0.15.4", I('Setting `version = "Y"`'), I('`version = "Y3L"`'))
    version <- "Y3L"
  }
  if (is.null(version) || !version %in% c("3L", "5L", "Y3L")) {
    stop("EQ-5D version not one of 3L, 5L or Y3L.")
  }
  
  # --- Compute EQ-5D index values ---
  data$index <- eq5d(data, country = country, version = version, type = type)
  
  # --- Compute severity vector (LFS or LSS) ---
  if (severity == "LFS") {
    data$severity <- lfs(data, version = version)
  } else if (severity == "LSS") {
    data$severity <- lss(data, version = version)
  } else {
    stop("Invalid `severity` argument. Must be 'LFS' or 'LSS'.")
  }
  
  # --- Aggregate to per-severity min/median/max (base R only) ---
  agg <- aggregate(index ~ severity, data = data,
                   FUN = function(x) c(
                     lowest  = min(x, na.rm = TRUE),
                     median  = stats::median(x, na.rm = TRUE),
                     highest = max(x, na.rm = TRUE)
                   ))
  
  # Unpack the matrix column returned by aggregate
  agg_expanded <- data.frame(
    severity = agg$severity,
    lowest   = agg$index[, "lowest"],
    median   = agg$index[, "median"],
    highest  = agg$index[, "highest"],
    row.names = NULL,
    check.names = FALSE
  )
  
  # --- Robust ordering: descending median, tie-break by highest then lowest ---
  ord <- order(agg_expanded$median,
               agg_expanded$highest,
               agg_expanded$lowest,
               decreasing = TRUE)
  agg_expanded <- agg_expanded[ord, ]
  
  # Set factor levels to the ordered severity for plotting
  agg_expanded$severity <- factor(agg_expanded$severity, levels = agg_expanded$severity)
  
  # --- Build a LONG frame via base::reshape for auto legend (no tidyverse) ---
  long_df <- reshape(
    agg_expanded,
    direction = "long",
    varying   = list(c("lowest", "median", "highest")),
    v.names   = "value",
    timevar   = "series",
    times     = c("Lowest", "Median", "Highest"),
    idvar     = "severity"
  )
  # Carry levels & set legend order
  long_df$severity <- factor(long_df$severity, levels = levels(agg_expanded$severity))
  long_df$series   <- factor(long_df$series, levels = c("Lowest", "Median", "Highest"))
  
  # --- Colors ---
  col_low  <- "#2E75B6"  # Lowest (blue)
  col_med  <- "#7F7F7F"  # Median (grey)
  col_high <- "#ED7D31"  # Highest (orange)
  col_err  <- "black"    # Error bars
  
  # --- Axis labels ---
  x_lab <- if (severity == "LFS") "Level Frequency Score" else "Level Severity Score"
  y_lab <- if (version %in% c("3L", "Y3L")) {
    sprintf("EQ-5D-3L value (%s)", country)
  } else {
    sprintf("EQ-5D-5L value (%s)", country)
  }
  
  # --- Median-only data (ordered by severity levels) ---
  median_df <- long_df[long_df$series == "Median", , drop = FALSE]
  median_df <- median_df[order(as.numeric(median_df$severity)), , drop = FALSE]
  
  # --- Plot (join only Median; Lowest/Highest points only) ---
  p <- ggplot2::ggplot(long_df, ggplot2::aes(x = severity, y = value, color = series)) +
    ggplot2::geom_linerange(
      data = agg_expanded,
      ggplot2::aes(x = severity, ymin = lowest, ymax = highest),
      inherit.aes = FALSE, color = col_err, size = 0.3
    ) +
    ggplot2::geom_line(
      data = median_df,
      ggplot2::aes(group = 1),
      linewidth = 0.7,
      na.rm = TRUE
    ) +
    ggplot2::geom_point(size = 1.8, na.rm = TRUE) +
    ggplot2::scale_color_manual(values = c("Lowest" = col_low, "Median" = col_med, "Highest" = col_high)) +
    ggplot2::scale_y_continuous(
      limits = c(-0.35, 1.02),
      breaks = seq(-0.3, 1.0, by = 0.1),
      labels = function(x) sprintf("%.1f", x)  # 1 dp
    ) +
    ggplot2::labs(x = x_lab, y = y_lab, color = NULL) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      # Only horizontal grid lines
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(color = "#dddddd"),
      panel.grid.minor.y = ggplot2::element_blank(),
      
      axis.text.x      = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1),
      axis.title.x     = ggplot2::element_text(margin = ggplot2::margin(t = 8)),
      axis.title.y     = ggplot2::element_text(margin = ggplot2::margin(r = 8)),
      legend.position  = "bottom",
      legend.title     = ggplot2::element_blank()
    )
  
  return(p)
}
