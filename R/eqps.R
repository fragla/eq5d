#' Probability of Superiority (PS) for EQ‑5D
#'
#' @description
#' Computes the Probability of Superiority (PS) for each EQ‑5D dimension
#' based on the Paretian Classification of Health Change (PCHC).
#'
#' PS quantifies whether post‑intervention EQ‑5D states tend to be superior
#' to pre‑intervention states:
#'
#' \deqn{
#'   PS = \frac{\text{Improve} + 0.5 \times \text{No change}}{\text{Total}}
#' }
#'
#' Interpretation:
#' \itemize{
#'   \item \strong{PS < 0.5}: deterioration dominates
#'   \item \strong{PS = 0.5}: improvement and deterioration balanced
#'   \item \strong{PS > 0.5}: improvement dominates
#' }
#'
#' Two interfaces are supported:
#'
#' \strong{1. Wide‑form input (default)}  
#' \code{ps(pre, post)} where \code{pre} and \code{post} are EQ‑5D states in
#' 5‑digit form or data frames containing EQ‑5D dimension columns.
#'
#' \strong{2. Long‑form + formula interface}  
#' \code{ps(formula, data)} mirroring the interface of \code{pchc()}.
#'
#' @param pre For the wide‑form interface, pre‑intervention EQ‑5D states
#'   (5‑digit character/numeric vector or data frame of EQ‑5D dimensions).
#'   For the formula interface, a formula describing EQ‑5D variables on the
#'   left‑hand side and a time variable (and optionally an ID variable)
#'   on the right‑hand side, e.g. \code{profile ~ visit | id}.
#'
#' @param post For the wide‑form interface, post‑intervention EQ‑5D states.
#'   Ignored for the formula interface.
#'   
#' @param formula A formula describing EQ‑5D variables on the left‑hand side
#'   and a time variable (and optionally an ID variable) on the right‑hand side,
#'   separated by \code{|}. Used only with the formula interface.
#'
#' @param data A long‑form data frame used with the formula interface.
#'   Ignored for the wide‑form interface.
#'
#' @param version EQ‑5D instrument version. One of \code{"3L"} or \code{"5L"}.
#'
#' @param ignore.invalid Logical; if TRUE, invalid scores are converted to NA;
#'   if FALSE, invalid scores trigger an error.
#'
#' @param dimensions Character vector specifying EQ‑5D dimension column names.
#'   Defaults to \code{c("MO", "SC", "UA", "PD", "AD")}.
#'
#' @param digits Numeric specifying the number of decimal places. Defaults to 2.
#'
#' @param pre.level Optional value identifying the pre‑intervention visit
#'   in the formula interface.
#'
#' @param post.level Optional value identifying the post‑intervention visit
#'   in the formula interface.
#'
#' @return
#' A named numeric vector (or list) of Probability of Superiority scores
#' by EQ‑5D dimension.
#'
#' @examples
#' dat <- read.csv(system.file("extdata", "eq5d3l_example.csv", package="eq5d"))
#'
#' ## Wide form
#' pre <- dat[dat$Group=="Group1",][1:50,]
#' post <- dat[dat$Group=="Group2",][1:50,]
#' ps(pre, post, version="3L")
#'
#' ## Long form
#' \dontrun{
#' ps(profile ~ visit | id, data=eq_long)
#' ps(MO + SC + UA + PD + AD ~ visit | id, data=eq_long)
#' }
#'
#' @export
#' @rdname ps
ps <- function(
  pre,
  post = NULL,
  version = NULL,
  ignore.invalid = TRUE,
  dimensions = NULL,
  digits = 2,
  pre.level = NULL,
  post.level = NULL,
  data = NULL
) {
  UseMethod("ps", pre)
}

# ============================================================
# Wide‑form method
# ============================================================

#' @rdname ps
#'
#' @description
#' Computes the Probability of Superiority from wide‑form EQ‑5D data where
#' pre‑ and post‑intervention states are supplied as separate objects.
#'
#' @export
ps.default <- function(
  pre,
  post,
  version = NULL,
  ignore.invalid = TRUE,
  dimensions = NULL,
  digits = 2,
  pre.level = NULL, 
  post.level = NULL,
  data = NULL
) {
  dat <- pchc(
    pre, post,
    version         = version,
    no.problems     = FALSE,
    totals          = TRUE,
    by.dimension    = TRUE,
    ignore.invalid  = ignore.invalid,
    dimensions      = dimensions
  )
  
  lapply(dat, .ps, digits = digits)
}

# ============================================================
# Long‑form method
# ============================================================

#' @rdname ps
#'
#' @description
#' Computes the Probability of Superiority from long‑form EQ‑5D data using a
#' formula interface.
#'
#' @export
ps.formula <- function(
  formula,
  post = NULL,
  version = NULL,
  ignore.invalid = TRUE,
  dimensions = NULL,
  digits = 2,
  pre.level = NULL,
  post.level = NULL,
  data = NULL
) {
  dat <- pchc(
    formula,
    data = data,
    version = version,
    no.problems = FALSE,
    totals = TRUE,
    by.dimension = TRUE,
    ignore.invalid = ignore.invalid,
    dimensions = dimensions,
    pre.level = pre.level,
    post.level = post.level
  )
  
  lapply(dat, .ps, digits = digits)
}

.ps <- function(x, digits=2) {
  round((x["Improve","Number"] + (0.5 * x["No change","Number"])) / x["Total","Number"], digits)
}