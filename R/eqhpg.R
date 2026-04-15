#' Health Profile Grid (HPG) for EQ-5D
#'
#' @description
#' Computes the Health Profile Grid (HPG) for two sets of EQ-5D health states.
#' The HPG displays pre- and post-intervention utility ranks and the associated
#' PCHC category for each subject.
#'
#' Two input interfaces are supported:
#'
#' \strong{1. Wide-form input (default method)}  
#' \code{hpg(pre, post)}  
#' where \code{pre} and \code{post} are EQ-5D health states represented either as:
#' \itemize{
#'   \item 5-digit EQ-5D profiles (character or numeric), or
#'   \item data.frames with dimension columns \code{MO}, \code{SC}, \code{UA},
#'         \code{PD}, \code{AD}
#' }
#'
#' \strong{2. Long-form input (formula method)}  
#' \code{hpg(formula, data)}  
#' where the formula has left-hand side forms:
#' \itemize{
#'   \item a single 5-digit EQ-5D profile column
#'   \item dimension columns, e.g. \code{MO + SC + UA + PD + AD}
#'   \item \code{cbind(MO,SC,UA,PD,AD)}
#' }
#' and right-hand side of the form \code{time | id}, where:
#' \itemize{
#'  \item \code{time} is the visit variable
#'  \item \code{id} is the subject identifier
#' }
#'
#' For formula methods, the time variable must contain exactly two observable
#' levels unless the user specifies \code{pre.level} and \code{post.level}.
#' When not supplied, HPG follows the same ordering logic as \code{pchc.formula()}:
#' \itemize{
#'   \item factor time variables use the factor level order
#'   \item character/numeric time variables use the order of appearance
#' }
#'
#' @param pre For the wide-form interface, pre-intervention EQ-5D values
#'   (5-digit profiles or dimension data.frame).
#'   For the formula interface, a formula describing EQ-5D variables on the
#'   left-hand side and a time variable (and optionally an ID variable)
#'   on the right-hand side, e.g. \code{profile ~ visit | id}.
#'
#' @param post For the wide-form interface, post-intervention EQ-5D values.
#'   Ignored for the formula interface.
#'
#' @param formula A formula describing EQ‑5D variables on the left‑hand side
#'   and a time variable (and optionally an ID variable) on the right‑hand side,
#'   separated by \code{|}. Used only with the formula interface.
#'
#' @param data Long-form dataset used with the formula interface.
#'   Ignored for the wide-form interface.
#'
#' @param country Country name passed to \code{eq5d()}.
#'
#' @param version EQ-5D version: \code{"3L"}, \code{"5L"}, or \code{"Y3L"}.
#'
#' @param type EQ-5D valuation method supplied to \code{eq5d()}.
#'
#' @param ignore.invalid Logical; if TRUE, invalid EQ-5D values are replaced with NA.
#'   If FALSE, invalid values trigger an error.
#'
#' @param dimensions Optional named character vector mapping canonical dimension
#'   names (\code{MO}, \code{SC}, \code{UA}, \code{PD}, \code{AD}) to alternative
#'   column names. Only used in formula methods with multiple dimension columns.
#'
#' @param pre.level Optional value of the time variable representing the
#'   pre-intervention visit.
#'
#' @param post.level Optional value of the time variable representing the
#'   post-intervention visit.
#'
#' @param no.problems Logical; passed to \code{pchc()} to classify
#'   \code{11111 -> 11111} transitions as \code{"No problems"}.
#'
#' @return
#' A data.frame with columns:
#' \itemize{
#'   \item \strong{Pre} - severity rank of the pre-intervention utility score
#'   \item \strong{Post} - severity rank of the post-intervention utility score
#'   \item \strong{PCHC} - individual-level PCHC category
#' }
#'
#' @examples
#' \dontrun{
#'
#' ## Wide-form usage
#' hpg(pre, post, country = "UK", version = "3L", type = "TTO")
#'
#' ## Long-form with a 5-digit profile column
#' hpg(profile ~ visit | id, data = df,
#'     country = "UK", version = "3L", type = "TTO")
#'
#' ## Long-form with dimension columns
#' hpg(MO + SC + UA + PD + AD ~ visit | id, data = df,
#'     country = "UK", version = "3L", type = "TTO")
#'
#' ## Explicit time ordering
#' hpg(profile ~ time | id, data = df, version = "3L",
#'     pre.level = "baseline", post.level = "followup")
#'
#' }
#'
#' @export
#' @rdname hpg
hpg <- function(pre,
  post = NULL,
  country = NULL,
  version = NULL,
  type = NULL,
  ignore.invalid = TRUE,
  dimensions = NULL,
  pre.level = NULL,
  post.level = NULL,
  no.problems = TRUE,
  data = NULL
) {
  UseMethod("hpg", pre)
}

# ============================================================
# Wide-form method
# ============================================================

#' @rdname hpg
#' @export
hpg.default <- function(
  pre,
  post,
  country = NULL,
  version = NULL,
  type = NULL,
  ignore.invalid = TRUE,
  dimensions = NULL,
  pre.level = NULL,
  post.level = NULL,
  no.problems = TRUE,
  data = NULL
) {
  
  if (!is.null(version) && version == "Y") {
    lifecycle::deprecate_soft("0.15.4", I('version="Y"'), I('version="Y3L"'))
    version <- "Y3L"
  }
  
  if (is.null(version) || !version %in% c("3L","5L","Y3L"))
    stop("EQ-5D version not one of 3L, 5L, or Y3L.")
  
  if (is.null(dimensions)) {
    dimensions <- .get_dimension_names()
  }
  
  # Convert 5-digit to dimensions
  if (is.character(pre) || is.numeric(pre))
    pre <- get_dimensions_from_health_states(pre, version=version, ignore.invalid=ignore.invalid)
  if (is.character(post) || is.numeric(post))
    post <- get_dimensions_from_health_states(post, version=version, ignore.invalid=ignore.invalid)
  
  # Align dimension columns
  if (all(dimensions %in% names(pre)) && all(dimensions %in% names(post))) {
    pre  <- pre[, dimensions]
    post <- post[, dimensions]
    colnames(pre)  <- .get_dimension_names()
    colnames(post) <- .get_dimension_names()
  } else {
    stop("Unable to identify EQ-5D dimensions in data.frames.")
  }
  
  if (nrow(pre) != nrow(post))
    stop("Different numbers of health states in pre and post.")
  
  # Individual-level PCHC (always totals=FALSE, summary=FALSE)
  pchc_vec <- pchc(pre, post,
                   version      = version,
                   no.problems  = no.problems,
                   totals       = FALSE,
                   summary      = FALSE)
  
  # Convert dimension rows to utilities
  pre_u  <- eq5d(scores=pre,  version=version, type=type, country=country, ignore.invalid=TRUE)
  post_u <- eq5d(scores=post, version=version, type=type, country=country, ignore.invalid=TRUE)
  
  # Rank utilities against all possible health states
  utilities <- sort(eq5d(scores=get_all_health_states(version),
                         version = version,
                         type    = type,
                         country = country),
                    decreasing = TRUE)
  
  matchrank <- function(vals) {
    sapply(vals, function(x) {
      w <- which.min(abs(utilities - x))
      if (length(w)) w else NA
    })
  }
  
  dat <- data.frame(Pre = matchrank(pre_u), Post = matchrank(post_u), PCHC = pchc_vec)
  rownames(dat) <- NULL
  dat
}

# ============================================================
# Formula method
# ============================================================

#' @rdname hpg
#' @export
hpg.formula <- function(
    formula,
    post = NULL,
    country = NULL,
    version = NULL,
    type = NULL,
    ignore.invalid = TRUE,
    dimensions = NULL,
    pre.level = NULL,
    post.level = NULL,
    no.problems = TRUE,
    data = NULL
) {
  
  # ---- Let pchc.formula() do the parsing + wide-form construction ----
  
  # 1. Obtain individual-level PCHC classifications
  pchc_vec <- pchc(
    formula,
    data            = data,
    version         = version,
    no.problems     = no.problems,
    pre.level       = pre.level,
    post.level      = post.level,
    totals          = FALSE,
    summary         = FALSE,
    ignore.invalid  = ignore.invalid,
    dimensions      = dimensions
  )
  
  # 2. Rebuild pre/post as wide-form dimension matrices
  #    using the *same* parsing done inside pchc.formula()
  parsed    <- .parse_pchc_formula(formula)
  canonical <- .get_dimension_names()
  
  # Identify time and id variables
  time_var <- parsed$time
  id_var   <- parsed$id
  
  if (is.null(id_var) || !(id_var %in% names(data)))
    stop("ID variable not found; supply via formula or `id=`.")
  
  if (!(time_var %in% names(data)))
    stop("Time variable not found in data.")
  
  time_vals <- data[[time_var]]
  unique_vals <- unique(as.character(time_vals))
  
  if (is.null(pre.level) || is.null(post.level)) {
    # Time ordering identical to pchc.formula()
    if (length(unique_vals) != 2)
      stop("Time variable must have exactly two levels for HPG formula, specify `pre.level` and `post.level`.")
    
    if (is.factor(time_vals)) {
      lvl <- levels(time_vals)
      lvl <- lvl[lvl %in% unique_vals]
      
      if (length(lvl) != 2)
        stop("Time factor must contain exactly two relevant levels.")
      
      pre.level  <- lvl[1]
      post.level <- lvl[2]
    } else {
      pre.level  <- unique_vals[1]
      post.level <- unique_vals[2]
    }
  }
  
  ids <- unique(data[[id_var]])
  
  # Extract rows
  
  pre_df <- data[
    data[[time_var]] == pre.level &
      data[[id_var]]   %in% ids,
    ,
    drop = FALSE
  ]

  post_df <- data[
    data[[time_var]] == post.level &
      data[[id_var]]   %in% ids,
    ,
    drop = FALSE
    ]
  
  pre_df  <- pre_df[order(pre_df[[id_var]]), , drop = FALSE]
  post_df <- post_df[order(post_df[[id_var]]), , drop = FALSE]
  
  
  # Resolve dimension columns using same rules as pchc.formula()
  if (parsed$is_profile) {
    pre_mat  <- get_dimensions_from_health_states(pre_df[[parsed$dims]],  version, ignore.invalid)
    post_mat <- get_dimensions_from_health_states(post_df[[parsed$dims]], version, ignore.invalid)
  } else {
    # Dimension-column or cbind formula
    dims <- parsed$dims
    if (!is.null(dimensions)) {
      if (!all(canonical %in% names(dimensions)))
        stop("`dimensions` must map all EQ-5D dimensions (MO, SC, UA, PD, AD)")
      
      dims <- dimensions[canonical]  # mapping
    }
    
    
    if (!all(dims %in% names(data)))
      stop("Dimension columns not found in data.")
    
    pre_mat  <- as.data.frame(pre_df[, dims, drop=FALSE])
    post_mat <- as.data.frame(post_df[, dims, drop=FALSE])
  }
  
  colnames(pre_mat)  <- canonical
  colnames(post_mat) <- canonical
  
  # Convert to utilities
  pre_u  <- eq5d(scores=pre_mat,  version=version, type=type, country=country, ignore.invalid=TRUE)
  post_u <- eq5d(scores=post_mat, version=version, type=type, country=country, ignore.invalid=TRUE)
  
  utilities <- sort(eq5d(scores=get_all_health_states(version),
                         version=version, type=type, country=country),
                    decreasing = TRUE)
  
  matchrank <- function(vals) {
    sapply(vals, function(x) {
      w <- which.min(abs(utilities - x))
      if (length(w)) w else NA
    })
  }
  
  
  dat <- data.frame(Pre  = matchrank(pre_u), Post = matchrank(post_u), PCHC = pchc_vec)
  rownames(dat) <- NULL
  dat
}

