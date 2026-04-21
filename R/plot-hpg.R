#' Plot Health Profile Grid (HPG)
#'
#' Visualises paired changes in EQ-5D health states using a Health Profile
#' Grid (HPG). Each point represents an individual pre–post transition in
#' EQ-5D index space, with a diagonal reference line indicating no change.
#'
#' This function is a pure visualisation layer and expects the output of
#' \code{\link{hpg}}. No analytical computation is performed.
#' 
#' The input to \code{plot_hpg()} is the result of \code{hpg()}, which may be
#' generated using paired pre- and post-measurement data (for example as
#' vectors or data frames), or via an alternative formula interface supported
#' by \code{hpg()}.
#'
#' Transition classes follow the terminology and ordering used in the
#' reference book: Improve, Mixed change, No change, Worsen, and No problems.
#' Only transition classes present in the plotted data are shown in the legend.
#'
#' @param hpg_data A data.frame returned by \code{\link{hpg}}, containing at
#'   least the columns \code{Pre}, \code{Post}, and \code{PCHC}.
#' @param version EQ-5D version ("3L", "5L", or "Y3L").
#' @param include_no_problems Logical; if \code{FALSE}, transitions
#'   corresponding to full health to full health (11111 to 11111) are excluded
#'   from the plot. This affects only the visualisation and does not modify
#'   the underlying \code{hpg()} results.
#' @param xlab Character string giving the x-axis label. Defaults to
#'   \code{"Post"}.
#' @param ylab Character string giving the y-axis label. Defaults to
#'   \code{"Pre"}.
#' @param colours Optional named character vector overriding the default
#'   colours for transition classes present in the data.
#' @param shapes Optional named numeric vector overriding the default
#'   shapes for transition classes present in the data.
#' @param theme A ggplot2 theme applied to the plot. Defaults to the internal
#'   \code{eq5d_theme()}.
#'
#' @return A ggplot object.
#'
#' @details
#' The Health Profile Grid is intended for paired data and illustrates how
#' individuals move between health states over time. Points on the diagonal
#' indicate no change, while points above the diagonal represent improvement
#' and points below the diagonal represent deterioration.
#'
#' @seealso
#' \code{\link{hpg}}
#'
#' @references
#' Devlin N, Parkin D, Janssen B (2020).
#' \emph{Methods for Analysing and Reporting EQ-5D Data}.
#' Springer Open. \doi{10.1007/978-3-030-47622-9}
#'
#' @examples
#' \dontrun{
#' res <- hpg(pre, post, country = "UK", version = "3L", type = "TTO")
#'
#' ## Default labels
#' plot_hpg(res, version = "3L")
#'
#' ## Custom axis labels
#' plot_hpg(
#'   res,
#'   version = "3L",
#'   xlab = "Post-treatment",
#'   ylab = "Pre-treatment"
#' )
#' }
#'
#' @export
plot_hpg <- function(
    hpg_data,
    version,
    include_no_problems = TRUE,
    xlab = "Post",
    ylab = "Pre",
    colours = NULL,
    shapes = NULL,
    theme = eq5d_theme()
) {
  
  ## ---- Basic checks ------------------------------------------------
  if (!is.data.frame(hpg_data)) {
    stop("`hpg_data` must be a data.frame returned by hpg().", call. = FALSE)
  }
  
  required <- c("Pre", "Post", "PCHC")
  if (!all(required %in% names(hpg_data))) {
    stop("`hpg_data` must contain columns: Pre, Post, and PCHC.", call. = FALSE)
  }
  
  if (!version %in% c("3L", "5L", "Y3L")) {
    stop("EQ-5D version not one of 3L, 5L or Y3L.", call. = FALSE)
  }
  
  ## ---- Optionally remove full-health to full-health ----------------
  if (!include_no_problems) {
    states <- get_all_health_states(version)
    full_health_index <- which(states == "11111")
    
    hpg_data <- hpg_data[
      !(hpg_data$Pre == full_health_index &
          hpg_data$Post == full_health_index),
    ]
  }
  
  ## ---- Enforce level ordering --------------------------------------
  transition_levels <- c(
    "Improve",
    "No change",
    "Worsen",
    "Mixed change",
    "No problems"
  )
  
  hpg_data$PCHC <- factor(
    hpg_data$PCHC,
    levels = transition_levels
  )
  
  ## ---- Axis limits ------------------------------------------------
  ts <- length(get_all_health_states(version))
  
  ## ---- Default book colours and shapes -----------------------------
  default_colours <- c(
    "Improve"       = "#2ca02c",
    "No change"     = "#eebe47",
    "Worsen"        = "#d62728",
    "Mixed change"  = "#1f77b4",
    "No problems"   = "#7f7f7f"
  )
  
  default_shapes <- c(
    "Improve"       = 3,   # +
    "Mixed change"  = 17,  # triangle
    "No change"     = 16,  # circle
    "Worsen"        = 4,   # ×
    "No problems"   = 1    # hollow circle
  )
  
  if (!is.null(colours)) {
    default_colours[names(colours)] <- colours
  }
  
  if (!is.null(shapes)) {
    default_shapes[names(shapes)] <- shapes
  }
  
  ## ---- Plot -------------------------------------------------------
  ggplot2::ggplot(
    hpg_data,
    ggplot2::aes(
      x = .data$Post, 
      y = .data$Pre, 
      colour = .data$PCHC, 
      shape = .data$PCHC
    )
  ) +
    ggplot2::geom_point(size = 2, alpha = 0.85) +
    ggplot2::coord_cartesian(xlim = c(1, ts), ylim = c(1, ts)) +
    ggplot2::scale_x_continuous(breaks = c(1, ts)) +
    ggplot2::scale_y_continuous(breaks = c(1, ts)) +
    ggplot2::scale_colour_manual(values = default_colours) +
    ggplot2::scale_shape_manual(values = default_shapes) +
    ggplot2::annotate(
      "segment",
      x = 1, y = 1,
      xend = ts, yend = ts,
      colour = "black"
    ) +
    ggplot2::labs(
      x = xlab,
      y = ylab,
      colour = NULL,
      shape  = NULL
    ) +
    theme
}
