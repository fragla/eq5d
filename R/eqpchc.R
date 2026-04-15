#' Paretian Classification of Health Change (PCHC)
#'
#' @description
#' Computes the Paretian Classification of Health Change (PCHC) for EQ‑5D data.
#' PCHC classifies change between two EQ‑5D health states into:
#' \itemize{
#'   \item \strong{Improve}
#'   \item \strong{Worsen}
#'   \item \strong{No change}
#'   \item \strong{Mixed change}
#'   \item \strong{No problems} (optional)
#' }
#'
#' The method applied depends on the class of the first argument:
#' \itemize{
#'   \item \code{pchc.default()} for wide‑form data with explicit pre/post states
#'   \item \code{pchc.formula()} for long‑form data using a formula interface
#' }
#'
#' @details
#' Missing or invalid EQ‑5D values are handled consistently across all methods
#' via the \code{ignore.invalid} argument.
#'
#' @param pre For the wide‑form interface, pre‑intervention EQ‑5D states,
#'   supplied either as a vector of 5‑digit EQ‑5D profiles or a data.frame
#'   containing EQ‑5D dimension columns.
#'   For the formula interface, a formula describing EQ‑5D variables on the
#'   left‑hand side and a time variable (and optionally an ID variable)
#'   on the right‑hand side, e.g. \code{profile ~ visit | id}.
#'
#' @param post For the wide‑form interface, post‑intervention EQ‑5D states
#'   in the same format as \code{pre}. Ignored for the formula interface.
#'
#' @param formula A formula specifying EQ‑5D variables on the left‑hand side
#'   and a time variable (and optionally an ID variable) on the right‑hand side.
#'   This argument is used only in the formula interface.
#'
#' @param data A long‑form data.frame used with the formula interface.
#'
#' @param version EQ‑5D instrument version. One of \code{"3L"}, \code{"5L"},
#'   or \code{"Y3L"}.
#'
#' @param id Optional character string naming the subject identifier column
#'   when it is not supplied in the formula.
#'
#' @param pre.level Optional value of the time variable identifying the
#'   pre‑intervention visit. Required when the time variable has more than
#'   two levels or when automatic inference would be incorrect.
#'
#' @param post.level Optional value of the time variable identifying the
#'   post‑intervention visit.
#'
#' @param duplicates How to handle duplicate observations per subject and
#'   time point in long‑form data. One of \code{"error"}, \code{"first"},
#'   or \code{"last"}.
#'
#' @param no.problems Logical; if TRUE, transitions from \code{11111 to 11111}
#'   are classified as \code{"No problems"} rather than \code{"No change"}.
#'
#' @param totals Logical; if TRUE, include total rows in summary output.
#'
#' @param by.dimension Logical; if TRUE, compute PCHC separately for each
#'   EQ‑5D dimension.
#'
#' @param ignore.invalid Logical; if TRUE, invalid or missing EQ‑5D values
#'   yield \code{NA}. If FALSE, invalid values trigger an error.
#'
#' @param dimensions Character vector naming EQ‑5D dimension columns.
#'   Defaults to \code{c("MO","SC","UA","PD","AD")}.
#'
#' @param summary Logical; if TRUE, return summary tables. If FALSE, return
#'   individual‑level classifications.
#'
#' @return
#' A data.frame or list summarising PCHC categories, or a vector of
#' individual‑level classifications when \code{summary = FALSE}.
#'
#' @export
#' @rdname pchc
pchc <- function(pre,
  post = NULL,
  version = NULL,
  id = NULL,
  pre.level = NULL,
  post.level = NULL,
  duplicates = c("error", "first", "last"),
  no.problems = TRUE,
  totals = TRUE,
  by.dimension = FALSE,
  ignore.invalid = TRUE,
  dimensions = NULL,
  summary = TRUE,
  data = NULL
) {
  UseMethod("pchc", pre)
}

# ============================================================
# Wide-form Method
# ============================================================

#' @rdname pchc
#'
#' @description
#' Computes the Paretian Classification of Health Change from wide-form EQ-5D
#' data where pre- and post-intervention states are supplied as separate objects.
#'
#' @param pre A data.frame of EQ-5D dimensions or a vector of 5-digit EQ-5D profiles.
#' @param post Same format as \code{pre}, representing post-intervention states.
#' @param version EQ-5D instrument version: \code{"3L"}, \code{"5L"}, or \code{"Y3L"}.
#' @param no.problems Logical; classify \code{11111 to 11111} as \code{"No problems"}.
#' @param totals Logical; include total rows in the summary table.
#' @param by.dimension Logical; compute PCHC separately for each EQ-5D dimension.
#' @param ignore.invalid Logical; if TRUE, invalid or missing values yield NA rows;
#'   if FALSE, an error is thrown.
#' @param dimensions Character vector naming EQ-5D dimension columns.
#' @param summary Logical; return summary table or individual classifications.
#'
#' @export
pchc.default <- function(
  pre, post, version = NULL, id = NULL, pre.level = NULL, post.level = NULL,
  duplicates = c("error", "first", "last"), no.problems = TRUE, totals = TRUE,
  by.dimension = FALSE, ignore.invalid = TRUE, dimensions = NULL, summary = TRUE,
  data = NULL
) {
  if (!is.null(version) && version == "Y") {
    lifecycle::deprecate_soft("0.15.4", I('version="Y"'), I('version="Y3L"'))
    version <- "Y3L"
  }
  if (is.null(version) || !version %in% c("3L","5L","Y3L"))
    stop("EQ-5D version must be '3L', '5L', or 'Y3L'.")
  
  if(is.null(dimensions)) {
    dimensions <- .get_dimension_names()
  }
  
  if (is.character(pre) || is.numeric(pre))
    pre <- get_dimensions_from_health_states(pre, version = version, ignore.invalid = ignore.invalid)
  
  if (is.character(post) || is.numeric(post))
    post <- get_dimensions_from_health_states(post, version = version, ignore.invalid = ignore.invalid)
  
  if (all(dimensions %in% names(pre)) && all(dimensions %in% names(post))) {
    pre  <- pre[, dimensions, drop = FALSE]
    post <- post[, dimensions, drop = FALSE]
    colnames(pre)  <- .get_dimension_names()
    colnames(post) <- .get_dimension_names()
  } else stop("Unable to identify EQ-5D dimension columns.")
  
  if (nrow(pre) != nrow(post))
    stop("Pre and post datasets must have identical row counts.")
  
  if (totals && !summary)
    warning("'totals=TRUE' ignored when summary=FALSE.")
  
  pre.bad  <- which(apply(pre, 1, function(x) any(!x %in% 1:.get_number_levels(version))))
  post.bad <- which(apply(post,1, function(x) any(!x %in% 1:.get_number_levels(version))))
  
  if (length(pre.bad) || length(post.bad)) {
    if (ignore.invalid) {
      bad <- unique(c(pre.bad, post.bad))
      pre[bad, ]  <- NA
      post[bad, ] <- NA
    } else stop("Invalid EQ-5D dimension levels detected.")
  }
  
  if (!by.dimension) {
    .pchc(pre, post, no.problems, totals, summary)
  } else {
    res <- lapply(.get_dimension_names(), function(dim) {
      out <- .pchc(pre[, dim, drop=FALSE], post[, dim, drop=FALSE], no.problems, totals, summary)
      if (summary) out <- out[rownames(out) != "Mixed change", ]
      out
    })
    names(res) <- .get_dimension_names()
    res
  }
}

# ============================================================
# Long-form Method
# ============================================================

#' @rdname pchc
#'
#' @description
#' Computes the Paretian Classification of Health Change from long-form EQ-5D
#' data using a formula interface.
#'
#' @export
pchc.formula <- function(
  formula, post = NULL, version = NULL, id = NULL,
  pre.level = NULL, post.level = NULL,
  duplicates = c("error", "first", "last"),
  no.problems = TRUE, totals = TRUE, by.dimension = FALSE,
  ignore.invalid = TRUE, dimensions = NULL, summary = TRUE, data = NULL
) {
  duplicates <- match.arg(duplicates)
  
  if (is.null(version) || !version %in% c("3L", "5L", "Y3L"))
    stop("EQ-5D version must be '3L', '5L', or 'Y3L'.")
  
  if (!is.data.frame(data))
    stop("`data` must be a data.frame.")
  
  parsed <- .parse_pchc_formula(formula)
  canonical <- .get_dimension_names()

  if (!parsed$is_profile) {
    dims <- parsed$dims
    if (!is.null(dimensions)) {
      if (!all(canonical %in% names(dimensions)))
        stop("`dimensions` must name all EQ-5D dimensions (MO,SC,UA,PD,AD)")
      mapped <- dimensions[canonical]
      if (!all(dims %in% mapped))
        stop("Formula LHS variables do not match supplied `dimensions` mapping.")
      dims <- mapped
    }
    if (!all(dims %in% names(data))) stop("Dimension columns not found in data.")
    dims <- dims[match(canonical, names(dimensions) %||% canonical)]
  }
  
  time_var <- parsed$time
  id_var   <- parsed$id %||% id
  if (is.null(id_var) || !(id_var %in% names(data)))
    stop("ID variable not found; supply via formula or `id=`.")
  
  if (!(time_var %in% names(data)))
    stop("Time variable not found in data.")
  
  time_vals   <- data[[time_var]]
  unique_vals <- unique(as.character(time_vals))
  
  # If either level missing infer
  if (is.null(pre.level) || is.null(post.level)) {
    
    if (length(unique_vals) != 2)
      stop("Time variable has more than two levels; specify `pre.level` and `post.level`.")
    
    if (is.factor(time_vals)) {
      lvl <- levels(time_vals)
      lvl <- lvl[lvl %in% unique_vals]  # drop unused levels
      
      if (length(lvl) != 2)
        stop("Time factor must contain exactly two relevant levels.")
      
      pre.level  <- lvl[1]
      post.level <- lvl[2]
      
    } else {
      # character/numeric: order of appearance
      pre.level  <- unique_vals[1]
      post.level <- unique_vals[2]
    }
  }
  
  pre_df  <- data[data[[time_var]] == pre.level, c(id_var, parsed$dims), drop=FALSE]
  post_df <- data[data[[time_var]] == post.level, c(id_var, parsed$dims), drop=FALSE]
  
  if (duplicates != "error") {
    pre_df  <- .dedupe_id_block(pre_df, id_var, duplicates)
    post_df <- .dedupe_id_block(post_df, id_var, duplicates)
  } else {
    if (any(duplicated(pre_df[[id_var]])))  stop("Duplicate pre rows.")
    if (any(duplicated(post_df[[id_var]]))) stop("Duplicate post rows.")
  }
  
  all_ids <- union(pre_df[[id_var]], post_df[[id_var]])
  pre_df  <- .align_ids(pre_df,  all_ids, id_var)
  post_df <- .align_ids(post_df, all_ids, id_var)
  
  if (parsed$is_profile) {
    pre_mat  <- get_dimensions_from_health_states(pre_df[[parsed$dims]], version = version, ignore.invalid = ignore.invalid)
    post_mat <- get_dimensions_from_health_states(post_df[[parsed$dims]], version = version, ignore.invalid = ignore.invalid)
    colnames(pre_mat)  <- canonical
    colnames(post_mat) <- canonical
  } else {
    pre_mat  <- as.data.frame(pre_df[, dims, drop=FALSE])
    post_mat <- as.data.frame(post_df[, dims, drop=FALSE])
    colnames(pre_mat)  <- canonical
    colnames(post_mat) <- canonical
  }
  
  pre_bad  <- which(apply(pre_mat,1,function(x) any(!x %in% 1:.get_number_levels(version))))
  post_bad <- which(apply(post_mat,1,function(x) any(!x %in% 1:.get_number_levels(version))))
  if (length(pre_bad) || length(post_bad)) {
    if (ignore.invalid) {
      bad <- unique(c(pre_bad,post_bad))
      pre_mat[bad,]  <- NA
      post_mat[bad,] <- NA
    } else stop("Invalid or missing EQ-5D values detected.")
  }
  
  if (!by.dimension) {
    .pchc(pre_mat, post_mat, no.problems, totals, summary)
  } else {
    res <- lapply(canonical, function(dim) {
      out <- .pchc(pre_mat[,dim,drop=FALSE], post_mat[,dim,drop=FALSE],
                   no.problems, totals, summary)
      if (summary) out <- out[rownames(out) != "Mixed change",]
      out
    })
    names(res) <- canonical
    res
  }
}

# ============================================================
# Formula Parser (profile / + / cbind)
# ============================================================

.parse_pchc_formula <- function(formula) {
  ch <- as.character(formula)
  if (length(ch) != 3)
    stop("Formula must be of form `profile ~ time | id` or `MO + SC + UA + PD + AD ~ time | id`.")
  
  lhs <- ch[2]
  rhs <- ch[3]
  
  if (grepl("^cbind\\(", lhs)) {
    inside <- gsub("^cbind\\(|\\)$", "", lhs)
    dims <- trimws(strsplit(inside,",")[[1]])
    is_profile <- FALSE
  } else if (grepl("\\+", lhs)) {
    dims <- trimws(strsplit(lhs,"\\+")[[1]])
    is_profile <- FALSE
  } else {
    dims <- trimws(lhs)
    is_profile <- TRUE
  }
  
  parts <- strsplit(rhs,"\\|")[[1]]
  time_var <- trimws(parts[1])
  id_var <- if (length(parts)>1) trimws(parts[2]) else NULL
  
  list(dims=dims, time=time_var, id=id_var, is_profile=is_profile)
}

# ============================================================
# Helpers
# ============================================================

.dedupe_id_block <- function(df, id_var, keep=c("first","last")) {
  keep <- match.arg(keep)
  ord <- order(df[[id_var]], seq_len(nrow(df)))
  df <- df[ord, , drop=FALSE]
  if (keep=="first") df <- df[!duplicated(df[[id_var]]),]
  else df <- df[!duplicated(df[[id_var]],fromLast=TRUE),]
  df
}

.align_ids <- function(df, ids, id_var) {
  idx <- match(ids, df[[id_var]])
  out <- df[1, , drop=FALSE][rep(1,length(ids)),]
  rownames(out) <- NULL
  out[[id_var]] <- ids
  for (nm in setdiff(names(df), id_var)) {
    col <- rep(NA,length(ids))
    m <- !is.na(idx)
    col[m] <- df[idx[m], nm]
    out[[nm]] <- col
  }
  out
}

# ============================================================
# PCHC Core Engine
# ============================================================

.pchc <- function(pre, post, no.problems, totals, summary) {
  res <- sapply(1:nrow(pre), function(i) {
    if (any(is.na(pre[i,])) || any(is.na(post[i,]))) return(NA)
    .classif(pre[i,], post[i,], no.problems)
  })
  if (summary) {
    if (no.problems) .total.no.problems(res, totals)
    else .total(res, totals)
  } else res
}

.total.no.problems <- function(res, totals=TRUE) {
  res <- table(res)
  df <- data.frame(Number=rep(0,5), Percent=rep(0,5),
                   row.names=c("No change","Improve","Worsen","Mixed change","No problems"))
  df$Number[match(names(res), rownames(df))] <- res
  
  if (totals) {
    df["Total with problems",] <- 0
    df <- df[c("No change","Improve","Worsen","Mixed change","Total with problems","No problems"),]
    prb <- match(c("No change","Improve","Worsen","Mixed change"), rownames(df))
    tpp <- match(c("Total with problems","No problems"), rownames(df))
    df["Total with problems","Number"] <- sum(df[prb,"Number"])
    df[prb,"Percent"] <- round(prop.table(df[prb,"Number"])*100,1)
    df[tpp,"Percent"] <- round(prop.table(df[tpp,"Number"])*100,1)
  } else df$Percent <- round(prop.table(df$Number)*100,1)
  df
}

.total <- function(res, totals=TRUE) {
  res <- table(res)
  df <- data.frame(Number=rep(0,4), row.names=c("No change","Improve","Worsen","Mixed change"))
  df$Number[match(names(res), rownames(df))] <- res
  df$Percent <- round(prop.table(df$Number)*100,1)
  if (totals) df["Total",] <- apply(df,2,sum)
  df
}

.classif <- function(pre, post, no.problems=TRUE) {
  improve <- any(pre > post)
  worsen  <- any(pre < post)
  if (improve && worsen) return("Mixed change")
  if (!improve && !worsen) {
    if (all(pre==1) && no.problems) return("No problems")
    return("No change")
  }
  if (improve) return("Improve")
  if (worsen)  return("Worsen")
}
