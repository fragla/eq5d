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
#' @param ps_out Output from \code{\link{ps}}: a named list of numeric
#'   Probability of Superiority values by EQ‑5D dimension, or a named list
#'   of such lists for grouped output.
#'
#' @param digits Integer specifying the number of decimal places used when
#'   rounding PS values. Defaults to 2.
#'
#' @return
#' If \code{ps_out} is ungrouped, a data.frame with columns \code{Dimension} 
#' and \code{PS}.
#'
#' If \code{ps_out} is grouped, a named list of such data.frames.
#'
#' @examples
#' dat <- read.csv(
#'   system.file("extdata", "eq5d3l_example.csv", package = "eq5d")
#' )
#'
#' pre  <- dat[dat$Group == "Group1", ][1:50, ]
#' post <- dat[dat$Group == "Group2", ][1:50, ]
#'
#' ps_res <- ps(pre, post, version = "3L")
#' table_ps(ps_res)
#'
#' @export
table_ps <- function(ps_out, digits = 2) {
  
  if (!is.list(ps_out)) {
    stop(
      "`ps_out` must be a named list of numeric PS values (or a named list of such lists), as returned by ps().",
      call. = FALSE
    )
  }
  
  format_one <- function(x) {
    
    if (!is.list(x) ||
        is.null(names(x)) ||
        any(names(x) == "") ||
        !all(vapply(x, is.numeric, logical(1))) ||
        !all(vapply(x, length, integer(1)) == 1L)) {
      stop(
        "`ps_out` must be a named list of numeric scalars by dimension.",
        call. = FALSE
      )
    }
    
    data.frame(
      Dimension = names(x),
      PS = round(unlist(x), digits),
      row.names = NULL,
      stringsAsFactors = FALSE
    )
  }
  
  ## Ungrouped case
  if (all(vapply(ps_out, is.numeric, logical(1)))) {
    return(format_one(ps_out))
  }
  
  
  ## Grouped case
  
  if (all(vapply(ps_out, is.list, logical(1)))) {
    
    if (is.null(names(ps_out)) || any(names(ps_out) == "")) {
      stop(
        "Grouped `ps_out` must be a named list.",
        call. = FALSE
      )
    }
    
    out <- lapply(ps_out, format_one)
    names(out) <- names(ps_out)
    return(out)
  }
  
  
  stop(
    "`ps_out` must be a named list of numeric PS values or a named list of such lists.",
    call. = FALSE
  )
}
