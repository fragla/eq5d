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
#' @return a data.frame or list of data.frames of changes according to PCHC.
#' contain dimensions names and rows the EQ-5D score.
#' @export
pchc <- function(pre, post, version="5L", no.problems=TRUE, totals=TRUE, by.dimension=FALSE, ignore.invalid=TRUE, dimensions=.getDimensionNames()) {
  
  if(is.character(pre) || is.numeric(pre)) {
    pre <- getDimensionsFromHealthStates(pre, version=version, ignore.invalid=ignore.invalid)
  }

  if(is.character(post) || is.numeric(post)) {
    post <- getDimensionsFromHealthStates(post, version=version, ignore.invalid=ignore.invalid)
  }

  if(all(dimensions %in% names(pre)) && all(dimensions %in% names(post))) {
    pre <- pre[,dimensions]
    colnames(pre) <- .getDimensionNames()
    post <- post[,dimensions]
    colnames(post) <- .getDimensionNames()
  } else {
    stop("Unable to identify EQ-5D dimensions in data.frames.")
  }
  
  if(nrow(pre)!=nrow(post)) {
    stop("Different numbers of health states in pre and post.")
  }
  
  pre.idx <- which(apply(pre, 1, function(x) { any(!x%in% 1:sub("L", "", version))}))
  post.idx <- which(apply(post, 1, function(x) { any(!x%in% 1:sub("L", "", version))}))
  
  if(length(pre.idx)>0 || length(post.idx)>0) {
    if(ignore.invalid) {
      pre[c(pre.idx,post.idx),] <- NA
      post[c(pre.idx,post.idx),] <- NA
    } else {
      stop("Missing/non-numeric dimension found.")
    }
  }
  
  if(!by.dimension) {
    res <- .pchc(pre, post, no.problems, totals)
  } else {
    res <- lapply(.getDimensionNames(), function(x) {
      dim.pchc <- .pchc(pre[,x, drop=FALSE], post[,x, drop=FALSE], no.problems, totals)
      dim.pchc <- dim.pchc[!rownames(dim.pchc) %in% "Mixed change",]
    })
    names(res) <- .getDimensionNames()
  }
  return(res)
}

.pchc <- function(pre, post, no.problems, totals) {
  res <- sapply(1:nrow(pre), function(x) {
    if(any(is.na(pre[x,]))||any(is.na(post[x,])))
      return(NA)
    .classif(pre[x,], post[x,], no.problems=no.problems)
  })
  
  if(no.problems) {
    .total.no.problems(res, totals)
  } else {
    .total(res, totals)
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