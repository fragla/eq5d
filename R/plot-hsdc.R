#' Plot Health State Density Curve (HSDC)
#'
#' Visualises the distribution of EQ-5D health profiles using the
#' Health State Density Curve (HSDC) as described by Zamora et al (2018)
#' and Devlin et al. (2020) The HSDC plots the cumulative proportion
#' of observed health states against the cumulative proportion of
#' observations, analogous to a Lorenz curve.
#'
#' The input data must be a pre-computed cumulative distribution,
#' typically returned by \code{\link{eq5dcf}} or \code{\link{make_hsdc_by_group}}.
#' This function performs no analytical computation; it only visualises
#' the supplied data.
#'
#' Axes are displayed as cumulative percentages (0--100\%). A 45-degree 
#' reference line indicates a perfectly even distribution of health states.
#'
#' @param data A data.frame containing cumulative distribution data with
#'   at least the columns \code{CumulativeProp} and \code{CumulativeState},
#'   as returned by \code{\link{eq5dcf}}.
#' @param group Optional character scalar giving the name of a grouping
#'   column for plotting multiple HSDCs on the same axes.
#' @param hsdi Optional numeric value, or named numeric vector for grouped
#'   data, giving Health State Density Index (HSDI) values to annotate
#'   on the plot.
#' @param diagonal Logical; if \code{TRUE}, draw the 45-degree reference line.
#' @param colours Optional named character vector specifying colours for
#'   grouped curves.
#' @param linewidth Numeric line width for plotted curves.
#' @param alpha Numeric transparency level for plotted curves.
#' @param theme A ggplot2 theme. Defaults to the internal \code{eq5d_theme()}.
#' @param xlab Character string for the x-axis label.
#' @param ylab Character string for the y-axis label.
#'
#' @return A \code{ggplot} object.
#'
#' @details
#' Curves that lie close to the diagonal indicate an even distribution of
#' health profiles. Curves that lie further below the diagonal indicate
#' increasing concentration of observations on a small number of
#' health states.
#'
#' @seealso
#' \code{\link{eq5dcf}}, \code{\link{hsdi}}, \code{\link{make_hsdc_by_group}}
#'
#' @references
#' Zamora B, Parkin D, Feng Y, Bateman A, Herdman M, Devlin N (2018).
#' New methods for analysing the distribution of EQ-5D observations.
#' OHE Research Paper.
#'
#' Devlin N, Parkin D, Janssen B (2020).
#' \emph{Methods for Analysing and Reporting EQ-5D Data}.
#' Springer Open. \doi{10.1007/978-3-030-47622-9}
#'
#' @examples
#' dat <- read.csv(system.file("extdata", "eq5d3l_example.csv", package = "eq5d"))
#' 
#' cf <- eq5dcf(dat, version = "3L")
#' plot_hsdc(cf)
#'
#' ## Grouped Health State Density Curves
#' ## Generate cumulative distributions by group
#' hsdc_grp <- make_hsdc_by_group(dat, group = "Group", version = "3L")
#'
#' ## Compute HSDI by group
#' hsdi_grp <- make_hsdi_by_group(dat, group = "Group", version = "3L")
#'
#' ## Plot grouped HSDCs with HSDI annotation
#' plot_hsdc(
#'   data  = hsdc_grp,
#'   group = "Group",
#'   hsdi  = hsdi_grp
#' )
#'
#' @export
plot_hsdc <- function(
    data,
    group = NULL,
    hsdi = NULL,
    diagonal = TRUE,
    colours = NULL,
    linewidth = 1,
    alpha = 1,
    theme = eq5d_theme(),
    xlab = "Cumulative percentage of observations",
    ylab = "Cumulative percentage of health states"
) {
  
  ## ---- input validation ---------------------------------------------
  if (!is.data.frame(data)) {
    stop("`data` must be a data.frame.", call. = FALSE)
  }
  
  required <- c("CumulativeProp", "CumulativeState")
  if (!all(required %in% names(data))) {
    stop(
      "Input data must contain columns: ",
      paste(required, collapse = ", "),
      call. = FALSE
    )
  }
  
  if (!is.null(group) && !group %in% names(data)) {
    stop("Grouping column not found in input data.", call. = FALSE)
  }
  
  ## ---- base plot ----------------------------------------------------
  if (is.null(group)) {
    
    p <- ggplot2::ggplot(
      data,
      ggplot2::aes(
        x = .data$CumulativeProp,
        y = .data$CumulativeState
      )
    ) +
      ggplot2::geom_line(
        linewidth = linewidth,
        alpha = alpha
      )
    
  } else {
    
    p <- ggplot2::ggplot(
      data,
      ggplot2::aes(
        x = .data$CumulativeProp,
        y = .data$CumulativeState,
        colour = .data[[group]],
        group  = .data[[group]]
      )
    ) +
      ggplot2::geom_line(
        linewidth = linewidth,
        alpha = alpha
      )
    
    if (!is.null(colours)) {
      p <- p + ggplot2::scale_color_manual(values = colours)
    }
  }
  
  ## ---- exact reference diagonal (0–100%) -----------------------------
  if (isTRUE(diagonal)) {
    p <- p +
      ggplot2::geom_segment(
        x = 0,
        y = 0,
        xend = 1,
        yend = 1,
        linetype = "dashed",
        colour = "black"
      )
  }
  
  
  ## ---- HSDI annotation (top-left, wrapped to 2 per line) ---------------
  if (!is.null(hsdi)) {
    
    if (length(hsdi) > 1 && !is.null(names(hsdi))) {
      
      ## Grouped: "HSDI Group = 0.23"
      labels <- paste0(
        "HSDI ", names(hsdi), " = ", round(hsdi, 2)
      )
      
      ## Wrap to maximum 2 labels per line
      label <- paste(
        vapply(
          split(labels, ceiling(seq_along(labels) / 2)),
          function(x) paste(x, collapse = ", "),
          character(1)
        ),
        collapse = "\n"
      )
      
    } else {
      
      ## Ungrouped: "HSDI = 0.23"
      label <- paste0("HSDI = ", round(hsdi, 2))
    }
    
    p <- p +
      ggplot2::annotate(
        geom      = "label",
        x         = 0.02,
        y         = 0.98,
        label     = label,
        hjust     = 0,
        vjust     = 1,
        size      = 3,
        fill      = "white",
        linewidth = 0.25
      )
  }
  
  ## ---- percentage scales + final formatting -------------------------
  p +
    ggplot2::coord_equal(
      xlim = c(0, 1),
      ylim = c(0, 1)
    ) +
    ggplot2::scale_x_continuous(
      breaks = seq(0, 1, by = 0.2),
      labels = function(x) paste0(100 * x, "%")
    ) +
    ggplot2::scale_y_continuous(
      breaks = seq(0, 1, by = 0.2),
      labels = function(x) paste0(100 * x, "%")
    ) +
    ggplot2::labs(
      x = xlab,
      y = ylab
    ) +
    theme
}
