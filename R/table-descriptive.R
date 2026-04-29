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
#' @param digits Integer specifying the number of decimal places used when
#'   rounding percentages. Ignored for count tables.
#'
#' @param include_total Logical; if \code{TRUE}, include a \code{"Total"}
#'   row.
#'
#' @param group_order Optional character vector specifying the order in which
#'   groups should be displayed when \code{dd} contains a \code{Group} column.
#'   Must match the levels of \code{dd$Group}. If \code{NULL} (default),
#'   groups are ordered according to the factor levels of \code{Group}
#'   as produced by \code{\link{descriptive_data}}.
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
#' dat1 <- subset(dat, Group == "Group1")
#'
#' ## Percentage table
#' dd_pct <- descriptive_data(dat1, version = "3L", metric = "percent")
#' table_descriptive(dd_pct)
#'
#' ## Count table
#' dd_cnt <- descriptive_data(dat1, version = "3L", metric = "count")
#' table_descriptive(dd_cnt)
#'
#' ## Grouped percentage tables
#' dd_grp <- descriptive_data(dat, version = "3L", metric = "percent", group = "Group")
#' table_descriptive(dd_grp)
#'
#' @export
table_descriptive <- function(
    dd,
    digits = 1,
    include_total = TRUE,
    group_order = NULL
) {
  
  # Determine metric from descriptive data
  metric <- unique(dd$Metric)
  
  if (length(metric) != 1L) {
    stop(
      "descriptive_data must contain exactly one metric, but found: ",
      paste(metric, collapse = ", "),
      call. = FALSE
    )
  }
  
  build_table <- function(x) {
    
    #x <- x[x$Metric == metric, , drop = FALSE]
    
    dims   <- unique(x$Dimension)
    levels <- levels(x$Level)
    
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
    
    ## Include total row (sum of level rows per dimension)
    if (include_total) {
      num_cols <- setdiff(names(df), "Level")
      totals <- colSums(df[, num_cols, drop = FALSE], na.rm = TRUE)
      
      total_row <- data.frame(
        Level = "Total",
        t(totals),
        row.names = NULL,
        check.names = FALSE
      )
      
      df <- rbind(df, total_row)
    }
    
    ## Round percentages only
    if (metric == "percent") {
      num_cols <- setdiff(names(df), "Level")
      df[, num_cols] <- round(df[, num_cols], digits)
    }
    
    df
  }
  
  ## Grouped output
  if ("Group" %in% names(dd)) {
    
    #groups <- levels(dd$Group)
    groups <- if (is.null(group_order)) {
      levels(dd$Group)
    } else {
      match.arg(group_order, choices = levels(dd$Group), several.ok = TRUE)
    }
    
    out <- setNames(vector("list", length(groups)), groups)
    
    for (g in groups) {
      out[[g]] <- build_table(dd[dd$Group == g, , drop = FALSE])
    }
    
    return(out)
  }
  
  ## Ungrouped output
  build_table(dd)
}
