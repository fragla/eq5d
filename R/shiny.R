#' Launch shiny EQ-5D interface
#' 
#' \code{shiny_eq5d} launches a shiny interface for browser based EQ-5D calculations.
#' 
#' @param display.mode The display mode to be passed to \link[shiny]{runApp}
#' @return NULL
#' @examples
#' \dontrun{
#' shiny_eq5d()
#' shiny_eq5d(display.mode="normal")
#' }
#' @export
shiny_eq5d <- function(display.mode = "normal") {
  pkgs <- c("shiny", "DT", "FSA", "ggplot2", "ggiraph", "ggiraphExtra", "mime", "PMCMRplus", "readxl", "shinycssloaders", "shinyWidgets")
  missing <- sapply(pkgs, function(x){!requireNamespace(x, quietly=TRUE)})
  if (any(missing)) {
    stop(paste("The following package(s) are required for shiny_eq5d to work:", 
               paste(pkgs[missing], collapse=", ")),
         call. = FALSE)
  }
  app_dir <- system.file("shiny", package = "eq5d")
  shiny::runApp(app_dir, display.mode = display.mode)
}