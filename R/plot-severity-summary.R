#' Plot EQ-5D Severity Summary
#'
#' Summarises EQ-5D index values by severity category using the Level
#' Frequency Score (LFS) or Level Sum Score (LSS). For each severity
#' category, the lowest, median, and highest EQ-5D index values are shown
#' using a range marker and horizontal ticks.
#'
#' This plot corresponds to severity summary figures in the Devlin book,
#' where EQ-5D index values are examined across severity strata rather than
#' plotting severity measures themselves. LFS and LSS are treated as
#' alternative severity definitions and are not plotted together.
#'
#' @param data A data.frame containing EQ-5D responses.
#' @param country Country value set used for EQ-5D index calculation.
#' @param version EQ-5D version ("3L", "5L", or "Y3L").
#' @param type EQ-5D valuation type.
#' @param severity Severity metric to use; either \code{"LFS"} or \code{"LSS"}.
#' @param tick_width Width of horizontal ticks in severity category units.
#' @param theme A ggplot2 theme applied to the plot. Defaults to the internal
#'   \code{eq5d_theme()}.
#'
#' @return A ggplot object.
#'
#' @seealso
#' \code{\link{lfs}}, \code{\link{lss}}, \code{\link{eq5d}}
#'
#' @references
#' Devlin N, Parkin D, Janssen B (2020).
#' \emph{Methods for Analysing and Reporting EQ-5D Data}.
#' Springer Open. \doi{10.1007/978-3-030-47622-9}
#'
#' @export
plot_severity_summary <- function(
  data,
  country,
  version,
  type,
  severity = c("LFS", "LSS"),
  tick_width = 0.12,
  theme = eq5d_theme()
) {

  severity <- match.arg(severity)

  if (!version %in% c("3L", "5L", "Y3L")) {
    stop("EQ-5D version not one of 3L, 5L or Y3L.", call. = FALSE)
  }

  ## ---- Compute EQ-5D index -----------------------------------------
  index <- eq5d(data, country = country, version = version, type = type)

  ## ---- Compute severity score --------------------------------------
  sev_version <- if (version %in% c("3L", "Y3L")) "3L" else "5L"

  sev <- if (severity == "LFS") {
    lfs(data, version = sev_version)
  } else {
    lss(data, version = sev_version)
  }

  df <- data.frame(
    severity = sev,
    index    = index
  )

  ## ---- Aggregate EQ-5D index by severity ---------------------------
  stats3 <- function(x) {
    x <- x[!is.na(x)]
    if (!length(x)) {
      return(c(lowest = NA_real_, median = NA_real_, highest = NA_real_))
    }
    c(
      lowest  = min(x),
      median  = stats::median(x),
      highest = max(x)
    )
  }

  agg <- aggregate(index ~ severity, data = df, FUN = stats3)

  M <- if (is.matrix(agg$index)) agg$index else do.call(rbind, agg$index)
  M <- as.data.frame(M, stringsAsFactors = FALSE)
  colnames(M) <- c("lowest", "median", "highest")

  plot_df <- cbind(
    severity = agg$severity,
    M,
    row.names = NULL
  )

  ## ---- Order by descending median ----------------------------------
  plot_df <- plot_df[
    order(plot_df$median, plot_df$highest, plot_df$lowest, decreasing = TRUE),
  ]
  plot_df$severity <- factor(plot_df$severity, levels = plot_df$severity)

  ## ---- Tick extents ------------------------------------------------
  x_num <- as.numeric(plot_df$severity)
  xmin  <- x_num - tick_width / 2
  xmax  <- x_num + tick_width / 2

  ## ---- Axis labels -------------------------------------------------
  instrument_label <- switch(
    version,
    "3L"  = "EQ-5D-3L",
    "Y3L" = "EQ-5D-Y3L",
    "5L"  = "EQ-5D-5L"
  )

  x_lab <- if (severity == "LFS") {
    "Level Frequency Score"
  } else {
    "Level Sum Score"
  }

  y_lab <- sprintf("%s index (%s)", instrument_label, country)

  ## ---- Plot --------------------------------------------------------
  ggplot2::ggplot(plot_df, ggplot2::aes(x = severity)) +
    ggplot2::geom_linerange(
      ggplot2::aes(ymin = .data$lowest, ymax = .data$highest),
      linewidth = 0.3,
      colour = "666666"
    ) +
    ggplot2::geom_line(
      ggplot2::aes(y = .data$median, group = 1),
      linewidth = 0.7,
      colour = "#7F7F7F"
    ) +
    ggplot2::geom_segment(
      ggplot2::aes(x = xmin, xend = xmax, y = .data$lowest, yend = .data$lowest),
      inherit.aes = FALSE,
      linewidth = 0.9,
      colour = "#2E75B6"
    ) +
    ggplot2::geom_segment(
      ggplot2::aes(x = xmin, xend = xmax, y = .data$highest, yend = .data$highest),
      inherit.aes = FALSE,
      linewidth = 0.9,
      colour = "#ED7D31"
    ) +
    ggplot2::geom_segment(
      ggplot2::aes(x = xmin, xend = xmax, y = .data$median, yend = .data$median),
      inherit.aes = FALSE,
      linewidth = 1.1,
      colour = "#7F7F7F"
    ) +
    ggplot2::scale_y_continuous(
      limits = c(-0.35, 1.02),
      breaks = seq(-0.3, 1.0, by = 0.1),
      labels = function(x) sprintf("%.1f", x)
    ) +
    ggplot2::labs(
      x = x_lab,
      y = y_lab
    ) +
    theme
}
