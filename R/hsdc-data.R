#' Generate Health State Density Curve (HSDC) data by group
#'
#' @description
#' Computes health-state cumulative frequency distributions separately for
#' each level of a grouping variable and returns a combined data frame
#' suitable for Health State Density Curve (HSDC) plotting.
#'
#' This function is a lightweight orchestration helper: it performs explicit
#' data splitting and applies \code{\link{eq5dcf}} to each subgroup, but does
#' not alter the definition or interpretation of the underlying distributional
#' summaries.
#'
#' @param data A data.frame containing EQ-5D descriptive-system data.
#' @param group Character scalar specifying the name of the grouping variable
#'   in \code{data}.
#' @param version EQ-5D instrument version. One of \code{"3L"}, \code{"5L"},
#'   or \code{"Y3L"}.
#'
#' @return
#' A data.frame containing cumulative frequency distributions for each group.
#' The returned data are suitable for direct use with
#' \code{\link{plot_hsdc}}.
#'
#' @seealso
#' \code{\link{eq5dcf}}, \code{\link{hsdi}}, \code{\link{plot_hsdc}},
#' \code{\link{make_hsdi_by_group}}
#'
#' @examples
#' dat <- read.csv(
#'   system.file("extdata", "eq5d3l_example.csv", package = "eq5d")
#' )
#'
#' ## Grouped HSDC data by treatment group
#' hsdc_by_group <- make_hsdc_by_group(
#'   dat,
#'   group = "Group",
#'   version = "3L"
#' )
#'
#' plot_hsdc(hsdc_by_group)
#'
#' @export
make_hsdc_by_group <- function(data, group, version) {
  
  if (!is.data.frame(data)) {
    stop("`data` must be a data.frame.", call. = FALSE)
  }
  
  if (!group %in% names(data)) {
    stop("Grouping variable not found in data.", call. = FALSE)
  }
  
  split_data <- split(data, data[[group]])
  
  hsdc_list <- lapply(
    names(split_data),
    function(g) {
      cf <- eq5dcf(
        split_data[[g]],
        version = version,
        digits = NULL
      )
      cf[[group]] <- g
      cf
    }
  )
  
  do.call(rbind, hsdc_list)
}

#' Compute Health State Density Index (HSDI) by group
#'
#' @description
#' Computes the Health State Density Index (HSDI) separately for each level
#' of a grouping variable. This function is a lightweight orchestration
#' helper that performs explicit data splitting prior to computation and
#' does not alter the definition or interpretation of HSDI.
#'
#' @param data A data.frame containing EQ-5D descriptive-system data.
#' @param group Character scalar giving the name of the grouping variable
#'   in \code{data}.
#' @param version EQ-5D instrument version. One of \code{"3L"}, \code{"5L"},
#'   or \code{"Y3L"}.
#'
#' @return
#' A named numeric vector of HSDI values, with one entry per group.
#'
#' @seealso
#' \code{\link{eq5dcf}}, \code{\link{hsdi}}, \code{\link{make_hsdc_by_group}}
#'
#' @examples
#' dat <- read.csv(
#'   system.file("extdata", "eq5d3l_example.csv", package = "eq5d")
#' )
#'
#' ## HSDI by group
#' hsdi_by_group <- make_hsdi_by_group(
#'   dat,
#'   group = "Group",
#'   version = "3L"
#' )
#'
#' @export
make_hsdi_by_group <- function(data, group, version) {
  
  if (!is.data.frame(data)) {
    stop("`data` must be a data.frame.", call. = FALSE)
  }
  
  if (!group %in% names(data)) {
    stop("Grouping variable not found in data.", call. = FALSE)
  }
  
  split_data <- split(data, data[[group]])
  
  sapply(split_data, function(d) {
    hsdi(d, version = version)
  })
}
