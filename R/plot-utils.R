#' eq5d plotting theme
#'
#' Default ggplot2 theme matching the academic style used in
#' Methods for Analysing and Reporting EQ-5D Data (Devlin et al., 2020).
#' 
#' @param base_size Base font size, in points.
#' @param base_family Base font family.
#'
#' @return A ggplot2 theme.
eq5d_theme <- function(base_size = 11, base_family = "") {
  
  ggplot2::theme_classic(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      panel.border     = ggplot2::element_blank(),
      panel.grid       = ggplot2::element_blank(),
      axis.line        = ggplot2::element_line(colour = "black", linewidth = 0.5),
      axis.ticks       = ggplot2::element_line(colour = "black"),
      axis.text        = ggplot2::element_text(colour = "black"),
      axis.title       = ggplot2::element_text(face = "plain"),
      legend.key       = ggplot2::element_blank(),
      legend.background= ggplot2::element_blank(),
      legend.title     = ggplot2::element_blank(),
      plot.background  = ggplot2::element_blank(),
      strip.background = ggplot2::element_blank(),
      strip.text       = ggplot2::element_text(face = "bold")
    )
}
