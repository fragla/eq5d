#' Generate grouped HSDC data
#'
#' Applies \code{eq5dcf()} separately to each level of a grouping variable
#' and returns a combined data.frame suitable for Health State Density
#' Curve (HSDC) plotting.
#'
#' @param data A data.frame containing EQ-5D data.
#' @param group Character scalar giving the grouping variable name.
#' @param version EQ-5D version ("3L", "5L", or "Y3L").
#'
#' @return A data.frame containing cumulative distributions for each group.
#' @export
make_hsdc_data <- function(data, group, version) {
  
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


#' Compute HSDI by group
#'
#' Computes the Health State Density Index (HSDI) separately for each level
#' of a grouping variable.
#'
#' @param data A data.frame containing EQ-5D data.
#' @param group Character scalar giving the grouping variable name.
#' @param version EQ-5D version ("3L", "5L", or "Y3L").
#'
#' @return A named numeric vector of HSDI values.
#' @export
make_hsdi_by_group <- function(data, group, version) {
  
  if (!is.data.frame(data)) {
    stop("`data` must be a data.frame.", call. = FALSE)
  }
  
  if (!group %in% names(data)) {
    stop("Grouping variable not found in data.", call. = FALSE)
  }
  
  tapply(
    data[[group]],
    data[[group]],
    function(g) {
      hsdi(
        data[data[[group]] == g, ],
        version = version
      )
    }
  )
}
