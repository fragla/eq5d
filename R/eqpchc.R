#' Calculate the Paretian Classification of Health Change
#' 
#' Calculate the Paretian Classification of Health Change (PCHC) for two EQ-5D datasets.
#' 
#' @param pre data.frame, numeric or character. For data.frame default column 
#' names should be MO, SC, UA, PD and AD representing Mobility, Self-care, 
#' Usual activities, Pain/discomfort and Anxiety/depression. Vector using five 
#' digit format can also be used.
#' @param post data.frame, numeric or character. For data.frame default column 
#' names should be MO, SC, UA, PD and AD representing Mobility, Self-care, 
#' Usual activities, Pain/discomfort and Anxiety/depression. Vector using five 
#' digit format can also be used.
#' @param version string of value "3L" or "5L" to indicate instrument version.
#' @param no.problems boolean. Summarise 11111 "No change" subjects in a "No problems" 
#' group.
#' @param totals boolean. Include a summary total.
#' @param by.dimension boolean. Summarise results by each EQ-5D dimension rather 
#' than by the whole dataset.
#' @param ignore.invalid boolean whether to ignore invalid scores. TRUE returns NA, FALSE 
#' throws an error.
#' @param dimensions character vector, specifying "dimension" column names. Defaults 
#' are "MO", "SC", "UA", "PD" and "AD".
#' @param summary boolean. Summarise results or return all classifications.
#' @return a data.frame or list of data.frames of changes according to PCHC.
#' contain dimensions names and rows the EQ-5D score or, if summary=FALSE, a vector or  
#' list of vectors of changes.
#' @examples
#' dat <- read.csv(system.file("extdata", "eq5d3l_example.csv", package="eq5d"))
#' 
#' pre <- dat[dat$Group=="Group1",][1:50,]
#' post <- dat[dat$Group=="Group2",][1:50,]
#' 
#' pchc(pre, post, version="3L", no.problems=FALSE, totals=FALSE)
#' 
#' @export
pchc <- function(pre, post, version=NULL, no.problems=TRUE, totals=TRUE, by.dimension=FALSE, ignore.invalid=TRUE, dimensions=.get_dimension_names(), summary=TRUE) {
  
  if (!is.null(version) && version == "Y") {
    lifecycle::deprecate_soft("0.15.4", I('Setting `version = "Y"`'), I('`version = "Y3L"`'))
    version <- "Y3L"
  }
  
  if(is.null(version) || !version %in% c("3L", "5L", "Y3L"))
    stop("EQ-5D version not one of 3L, 5L or Y3L.")
  
  if(is.character(pre) || is.numeric(pre)) {
    pre <- get_dimensions_from_health_states(pre, version=version, ignore.invalid=ignore.invalid)
  }

  if(is.character(post) || is.numeric(post)) {
    post <- get_dimensions_from_health_states(post, version=version, ignore.invalid=ignore.invalid)
  }

  if(all(dimensions %in% names(pre)) && all(dimensions %in% names(post))) {
    pre <- pre[,dimensions]
    colnames(pre) <- .get_dimension_names()
    post <- post[,dimensions]
    colnames(post) <- .get_dimension_names()
  } else {
    stop("Unable to identify EQ-5D dimensions in data.frames.")
  }
  
  if(nrow(pre)!=nrow(post)) {
    stop("Different numbers of health states in pre and post.")
  }
  
  if(totals && !summary) {
    warning("'totals = TRUE' and 'summary = FALSE' can't be used together. 'totals' will be ignored.")
  }
  
  pre.idx <- which(apply(pre, 1, function(x) { any(!x%in% 1:.get_number_levels(version))}))
  post.idx <- which(apply(post, 1, function(x) { any(!x%in% 1:.get_number_levels(version))}))
  
  if(length(pre.idx)>0 || length(post.idx)>0) {
    if(ignore.invalid) {
      pre[unique(c(pre.idx,post.idx)),] <- NA
      post[unique(c(pre.idx,post.idx)),] <- NA
    } else {
      stop("Missing/non-numeric dimension found.")
    }
  }
  
  if(!by.dimension) {
    res <- .pchc(pre, post, no.problems, totals, summary)
  } else {
    res <- lapply(.get_dimension_names(), function(x) {
      dim.pchc <- .pchc(pre[,x, drop=FALSE], post[,x, drop=FALSE], no.problems, totals, summary)
      if(summary){
        dim.pchc <- dim.pchc[!rownames(dim.pchc) %in% "Mixed change",]
      }
      dim.pchc
    })
    names(res) <- .get_dimension_names()
  }
  return(res)
}

.pchc <- function(pre, post, no.problems, totals, summary) {
  res <- sapply(1:nrow(pre), function(x) {
    if(any(is.na(pre[x,]))||any(is.na(post[x,])))
      return(NA)
    .classif(pre[x,], post[x,], no.problems=no.problems)
  })
  
  if(summary) {
    if(no.problems) {
      .total.no.problems(res, totals)
    } else {
      .total(res, totals)
    }
  } else {
    res
  }
}

.total.no.problems <- function(res, totals=TRUE) {
  res <- table(res)
  res.df <- data.frame(row.names=c("No change", "Improve", "Worsen", "Mixed change", "No problems"),
                    Number=rep(0,5), Percent=rep(0,5))
  idx <- match(names(res), rownames(res.df))
  res.df$Number[idx] <- res
  
  if(totals) {
    res.df["Total with problems",] <- 0
    res.df <- res.df[c("No change", "Improve", "Worsen", "Mixed change", "Total with problems", "No problems"),]
    
    problem.idx <- match(c("No change", "Improve", "Worsen", "Mixed change"), rownames(res.df))
    totals.idx <- match(c("Total with problems", "No problems"), rownames(res.df))
    
    res.df["Total with problems","Number"] <- sum(res.df[problem.idx,"Number"])
    res.df[problem.idx,"Percent"] <- round(prop.table(res.df[problem.idx,"Number"])*100,1)
    res.df[totals.idx,"Percent"] <- round(prop.table(res.df[totals.idx,"Number"])*100,1)
  } else {
    res.df$Percent <- round(prop.table(res.df$Number)*100,1)
  }
  return(res.df)
}

.total <- function(res, totals=TRUE){
  res <- table(res)
  res.df <- data.frame(row.names=c("No change", "Improve", "Worsen", "Mixed change"), 
                       Number=rep(0,4))
  idx <- match(names(res), rownames(res.df))
  res.df$Number[idx] <- res
  res.df$Percent <- prop.table(res.df$Number)
  if(totals) {
    res.df["Total",] <- apply(res.df, 2, sum)
  }
  res.df$Percent <- round(res.df$Percent*100,1)
  return(res.df)
}

.classif <- function(pre, post, no.problems=TRUE) {
  improve <- any(pre > post)
  worsen <- any(pre < post)

  if(improve && worsen) {
    return("Mixed change")
  } else if(!improve && !worsen) {
    if(all(pre==1) && no.problems) {
      return("No problems")
    } else {
      return("No change")
    }
  } else if(improve) {
    return("Improve")
  } else if(worsen) {
    return("Worsen")
  }
}