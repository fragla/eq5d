#' Descriptive System Tables for EQ-5D
#'
#' @description
#' Formats EQ-5D descriptive system summaries into publication-ready tables.
#' This function is the reporting-layer companion to
#' \code{\link{descriptive_data}}, and mirrors the tabular presentation
#' recommended in the EQ-5D reference literature and implemented by the
#' Stata \code{eq5dds} command.
#'
#' Tables contain either percentages or counts, depending on the metric
#' supplied to \code{\link{descriptive_data}}. No analytical computation
#' is performed by this function.
#'
#' @details
#' This function operates on the output of \code{\link{descriptive_data}},
#' which returns tidy descriptive-system summaries with one row per
#' Dimension–Level–Metric combination. Values are reshaped into wide
#' tables for reporting.
#'
#' Counts and percentages are generated via separate calls to
#' \code{\link{descriptive_data}}, consistent with the presentation in
#' the EQ-5D reference literature.
#'
#' When descriptive data are grouped, \code{table_descriptive()} returns
#' a named list of tables, one per group, consistent with the behaviour
#' of \code{eq5dds} when used with subgroup reporting.
#'
#' @param dd A data.frame returned by \code{\link{descriptive_data}}, containing
#'   the columns \code{Dimension}, \code{Level}, \code{Value}, and
#'   \code{Metric}. Grouped descriptive data are supported via a
#'   \code{Group} column.
#'
#' @param type Character string specifying which table to produce.
#'   One of \code{"percent"} or \code{"count"}.
#'
#' @param digits Integer specifying the number of decimal places used when
#'   rounding percentages. Ignored for count tables.
#'
#' @param include_total Logical; if \code{TRUE}, include a \code{"Total"}
#'   column for count tables. Ignored for percentage tables.
#'
#' @return
#' If descriptive data are ungrouped, a data.frame. If grouped, a named
#' list of data.frames, one per group.
#'
#' @references
#' Devlin N, Parkin D, Janssen B (2020).
#' \emph{Methods for Analysing and Reporting EQ-5D Data}.
#' Springer Open. \doi{10.1007/978-3-030-47622-9}
#'
#' Ramos-Goñi JM, Ramallo-Fariña Y (2016).
#' eq5dds: A command to analyze the descriptive system of the EQ-5D
#' quality-of-life instrument.
#' \emph{The Stata Journal}, 16(3), 691–701.
#'
#' @examples
#' dat <- read.csv(
#'   system.file("extdata", "eq5d3l_example.csv", package = "eq5d")
#' )
#'
#' ## Percentage table
#' dd_pct <- descriptive_data(dat, version = "3L")
#' table_descriptive(dd_pct, type = "percent")
#'
#' ## Count table
#' dd_cnt <- descriptive_data(dat, version = "3L", metric = "count")
#' table_descriptive(dd_cnt, type = "count")
#'
#' ## Grouped percentage tables
#' dd_grp <- descriptive_data(dat, version = "3L", group = "Group")
#' table_descriptive(dd_grp, type = "percent")
#'
#' @export
table_descriptive <- function(
    dd,
    type = c("percent", "count"),
    digits = 1,
    include_total = TRUE
) {
  
  type <- match.arg(type)
  metric <- if (type == "percent") "percentage" else "count"
  
  if (!metric %in% dd$Metric) {
    stop(
      "Requested type = '", type, "' but descriptive_data contains metric(s): ",
      paste(unique(dd$Metric), collapse = ", "),
      "."
    )
  }
  
  build_table <- function(x) {
    
    x <- x[x$Metric == metric, , drop = FALSE]
    
    dims   <- unique(x$Dimension)
    levels <- unique(x$Level)
    
    mat <- matrix(
      NA_real_,
      nrow = length(levels),
      ncol = length(dims),
      dimnames = list(levels, dims)
    )
    
    for (i in seq_len(nrow(x))) {
      mat[
        as.character(x$Level[i]),
        as.character(x$Dimension[i])
      ] <- x$Value[i]
    }
    
    df <- as.data.frame(mat, stringsAsFactors = FALSE)
    df <- cbind(Level = rownames(df), df, stringsAsFactors = FALSE)
    rownames(df) <- NULL
    
    ## Include totals only for count tables
    if (include_total && type == "count") {
      num_cols <- setdiff(names(df), "Level")
      df$Total <- rowSums(df[, num_cols, drop = FALSE], na.rm = TRUE)
    }
    
    ## Round percentages only
    if (type == "percent") {
      num_cols <- setdiff(names(df), "Level")
      df[, num_cols] <- round(df[, num_cols], digits)
    }
    
    df
  }
  
  ## Grouped output
  if ("Group" %in% names(dd)) {
    
    groups <- levels(dd$Group)
    out <- setNames(vector("list", length(groups)), groups)
    
    for (g in groups) {
      out[[g]] <- build_table(dd[dd$Group == g, , drop = FALSE])
    }
    
    return(out)
  }
  
  ## Ungrouped output
  build_table(dd)
}
