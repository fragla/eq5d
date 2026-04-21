#' Plot EQ-5D Descriptive System
#'
#' Creates bar charts summarising the EQ-5D descriptive system by dimension
#' and level using pre-computed descriptive data. Bars representing EQ-5D
#' response levels are displayed side-by-side within each dimension.
#' When grouped data are supplied, separate panels are created using faceting.
#'
#' This function performs no analytical computation. It visualises the
#' output of \code{\link{descriptive_data}}, which serves as the canonical
#' representation of the EQ-5D descriptive system.
#'
#' @param descriptive_data A data.frame produced by
#'   \code{\link{descriptive_data}}.
#' @param alpha Numeric transparency for bars.
#' @param theme A ggplot2 theme, defaults to the internal 
#'   \code{eq5d_theme()}.
#' @param xlab Character string for the x-axis label.
#' @param ylab Character string for the y-axis label. If \code{NULL}, it is
#'   inferred from the \code{Metric} column ("Percentage" or "Count").
#' @param dimension_labels Optional named character vector mapping EQ-5D
#'   dimension codes (MO, SC, UA, PD, AD) to display labels.
#'
#' @return A ggplot object.
#'
#' @details
#' The descriptive system summarises the distribution of response levels
#' within each EQ-5D dimension. Bars are shown side-by-side to facilitate
#' comparison across response levels. When grouped data are supplied, the
#' plot uses faceting to aid comparison across groups while avoiding
#' overplotting and excessive use of colour.
#'
#' @seealso
#' \code{\link{descriptive_data}}
#'
#' @references
#' Ramos-Goñi JM, Ramallo-Fariña Y (2016).
#' eq5dds: A command to analyze the descriptive system of the EQ-5D
#' quality-of-life instrument.
#' \emph{The Stata Journal}, 16(3), 691–701.
#' \doi{10.1177/1536867X1601600309}
#' 
#' @examples
#' ## Load example EQ-5D-3L data included with the package
#' dat <- read.csv(
#'   system.file("extdata", "eq5d3l_example.csv", package = "eq5d")
#' )
#'
#' ## Create canonical descriptive data (percentages)
#' dd <- descriptive_data(dat, version = "3L")
#'
#' ## Basic descriptive system plot
#' plot_descriptive(dd)
#'
#' ## Grouped descriptive system plot
#' dd_grp <- descriptive_data(dat, version = "3L", group = "Group")
#' plot_descriptive(dd_grp)
#'
#' ## Descriptive system using counts
#' dd_count <- descriptive_data(dat, version = "3L", metric = "count")
#' plot_descriptive(dd_count)
#'
#' ## Descriptive system with full dimension labels
#' plot_descriptive(
#'   dd,
#'   dimension_labels = c(
#'     MO = "Mobility",
#'     SC = "Self care",
#'     UA = "Usual activities",
#'     PD = "Pain & Discomfort",
#'     AD = "Anxiety & Depression"
#'   )
#' )
#'
#' @export
plot_descriptive <- function(
    descriptive_data,
    alpha = 1,
    theme = eq5d_theme(),
    xlab = "Dimension",
    ylab = NULL,
    dimension_labels = NULL
) {
  
  if (!is.data.frame(descriptive_data)) {
    stop(
      "`descriptive_data` must be a data.frame produced by descriptive_data().",
      call. = FALSE
    )
  }
  
  required <- c("Dimension", "Level", "Value", "Metric")
  if (!all(required %in% names(descriptive_data))) {
    stop(
      "Input does not appear to be descriptive data produced by descriptive_data().",
      call. = FALSE
    )
  }
  
  has_group <- "Group" %in% names(descriptive_data)
  
  if (is.null(ylab)) {
    ylab <- if (unique(descriptive_data$Metric) == "count") {
      "Count"
    } else {
      "Percentage"
    }
  }
  
  ## ---- base plot: dodged bars --------------------------------------
  p <- ggplot2::ggplot(
    descriptive_data,
    ggplot2::aes(
      x    = .data$Dimension,
      y    = .data$Value,
      fill = .data$Level
    )
  ) +
    ggplot2::geom_bar(
      stat     = "identity",
      position = ggplot2::position_dodge(),
      alpha    = alpha
    ) +
    ggplot2::labs(fill = "Score")
  
  ## ---- facet by group (if present) ---------------------------------
  if (has_group) {
    p <- p + ggplot2::facet_wrap(~ Group)
  }
  
  ## ---- dimension label mapping -------------------------------------
  if (!is.null(dimension_labels)) {
    p <- p + ggplot2::scale_x_discrete(labels = dimension_labels)
  }
  
  p +
    ggplot2::labs(
      x = xlab,
      y = ylab
    ) +
    theme
}
