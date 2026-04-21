#' Generate descriptive system data for EQ-5D
#'
#' Creates a tidy data.frame representing the EQ-5D descriptive system,
#' suitable for plotting and tabular presentation. The output contains
#' explicit columns for dimension, level, metric (count or percentage),
#' and optional grouping variables.
#'
#' This function is designed as a canonical internal representation and
#' may be used by plotting and table-generation functions. It does not
#' return Stata-style summary tables; for those see \code{\link{eq5dds}}.
#'
#' @param data A data.frame containing EQ-5D responses.
#' @param version EQ-5D version ("3L", "5L", or "Y3L").
#' @param metric Character string, one of \code{"percentage"} (default)
#'   or \code{"count"}.
#' @param group Optional character scalar giving the name of a grouping variable.
#' @param dimensions Character vector of EQ-5D dimension names.
#' @param ignore.invalid Logical; whether to ignore invalid responses.
#'
#' @return A tidy data.frame with columns:
#'   \itemize{
#'     \item \code{Dimension} EQ-5D dimension (MO, SC, UA, PD, AD)
#'     \item \code{Level} Response level (1, 2, ..., L)
#'     \item \code{Value} Count or percentage
#'     \item \code{Metric} Either "count" or "percentage"
#'     \item \code{Group} Group label (if grouping applied)
#'   }
#'
#' @seealso
#' \code{\link{eq5dds}}, \code{\link{plot_descriptive}}
#'
#' @examples
#' ## Load example EQ-5D-3L data included with the package
#' dat <- read.csv(
#'   system.file("extdata", "eq5d3l_example.csv", package = "eq5d")
#' )
#'
#' ## Descriptive system for the full sample
#' dd <- descriptive_data(
#'   data = dat,
#'   version = "3L"
#' )
#'
#' dd
#'
#' ## Descriptive system stratified by group
#' dd_group <- descriptive_data(
#'   data = dat,
#'   version = "3L",
#'   group = "Group"
#' )
#'
#' dd_group
#'
#' @references
#' Ramos-Goñi JM, Ramallo-Fariña Y (2016).
#' eq5dds: A command to analyze the descriptive system of the EQ-5D
#' quality-of-life instrument.
#' \emph{The Stata Journal}, 16(3), 691–701.
#' \doi{10.1177/1536867X1601600309}
#'
#' @export
descriptive_data <- function(
    data,
    version,
    metric = c("percentage", "count"),
    group = NULL,
    dimensions = c("MO", "SC", "UA", "PD", "AD"),
    ignore.invalid = TRUE
) {
  
  metric <- match.arg(metric)
  
  if (!is.data.frame(data)) {
    stop("`data` must be a data.frame.", call. = FALSE)
  }
  
  if (!is.null(group) && !group %in% names(data)) {
    stop("Grouping column not found in data.", call. = FALSE)
  }
  
  ## Use eq5dds for computation (counts or percentages)
  counts_flag <- metric == "count"
  
  if (is.null(group)) {
    dd <- eq5dds(
      data,
      version = version,
      counts = counts_flag,
      dimensions = dimensions,
      ignore.invalid = ignore.invalid,
      digits = NULL
    )
    
    long <- do.call(
      rbind,
      lapply(
        names(dd),
        function(d) {
          vals <- as.numeric(dd[[d]])
          data.frame(
            Dimension = d,
            Level     = seq_along(vals),
            Value     = vals,
            Metric    = metric,
            stringsAsFactors = FALSE
          )
        }
      )
    )
    
  } else {
    
    dd <- eq5dds(
      data,
      version = version,
      counts = counts_flag,
      by = group,
      dimensions = dimensions,
      ignore.invalid = ignore.invalid,
      digits = NULL
    )
    
    long <- do.call(
      rbind,
      lapply(
        names(dd),
        function(g) {
          do.call(
            rbind,
            lapply(
              names(dd[[g]]),
              function(d) {
                vals <- as.numeric(dd[[g]][[d]])
                data.frame(
                  Group     = g,
                  Dimension = d,
                  Level     = seq_along(vals),
                  Value     = vals,
                  Metric    = metric,
                  stringsAsFactors = FALSE
                )
              }
            )
          )
        }
      )
    )
  }
  
  long$Dimension <- factor(long$Dimension, levels = dimensions)
  long$Level     <- factor(long$Level)
  
  if (!is.null(group)) {
    long$Group <- factor(long$Group)
  }
  
  long
}
