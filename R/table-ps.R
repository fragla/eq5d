#' Probability of Superiority Tables
#'
#' @description
#' Formats output from \code{\link{ps}} into a tabular representation suitable
#' for reporting.
#'
#' The function does not compute Probability of Superiority values; it
#' restructures the numeric output of \code{ps()} into a data.frame with
#' explicit dimension labels.
#'
#' @param ps_out Output from \code{\link{ps}}, either a named numeric vector
#'   of PS values or a named list of such vectors.
#'
#' @param digits Integer specifying the number of decimal places used when
#'   rounding PS values.
#'
#' @return
#' A data.frame with columns \code{Dimension} and \code{PS}, or a named list
#' of such data.frames when grouped input is supplied.
#'
#' @export
table_ps <- function(ps_out, digits = 2) {
  
  format_one <- function(x) {
    
    if (!is.numeric(x) || is.null(names(x))) {
      stop("`ps_out` must be a named numeric vector or list of such vectors.")
    }
    
    data.frame(
      Dimension = names(x),
      PS = round(as.numeric(x), digits),
      row.names = NULL,
      stringsAsFactors = FALSE
    )
  }
  
  ## Grouped case
  if (is.list(ps_out) && !is.numeric(ps_out)) {
    return(lapply(ps_out, format_one))
  }
  
  ## Ungrouped case
  format_one(ps_out)
}
