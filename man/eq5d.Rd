% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/eq5d.R
\name{eq5d}
\alias{eq5d}
\title{Calculate EQ-5D index scores}
\usage{
eq5d(scores, version, type, country, ignore.invalid, ...)
}
\arguments{
\item{scores}{numeric or data.frame with names/colnames MO, SC, UA, PD and AD
representing Mobility, Self-care, Usual activities, Pain/discomfort and
Anxiety/depression. Alternatively EQ-5D scores can be provided in
five digit format e.g. 12321. If five digit scores are used in a data.frame
the default column name look for by the function is "State".}

\item{version}{string of value "3L", "5L" or "Y3L" to indicate instrument
version.}

\item{type}{string specifying method type used in deriving value set scores.
Options are TTO or VAS for EQ-5D-3L, VT for EQ-5D-5L, CW for EQ-5D-5L
crosswalk conversion valuesets, RCW for EQ-5D-3L reverse crosswalk
conversion valuesets and DSU for the NICE Decision Support Unit's EEPRU 
age-sex based EQ-5D-3L to EQ-5D-5L and EQ-5D-5L to EQ-5D-3L mappings. Not 
required for EQ-5D-Y-3L.}

\item{country}{string of value set country name used.}

\item{ignore.invalid}{logical to indicate whether to ignore dimension data
with invalid, incomplete or missing data.}

\item{...}{character vectors for column names when using a data.frame. Use
"dimensions" (default c("MO", "SC", "UA", "PD" and "AD")), "five.digit"
(default "State") or "utility", "age", "sex" and "bwidth" (defaults
"Utility", "Age", "Sex" and "bwidth") for NICE DSU mapping. bwidth can also
be a number which is applied to the whole dataset. When a single
NICE DSU score is being calculated "age", "sex" and "bwidth" are also
used. See \code{\link{eq5dmap}} for valid options. "digits" can also be 
used to return scores with more precision.}
}
\value{
a numeric vector of utility index scores.
}
\description{
Wrapper for \code{eq5d3l}, \code{eq5d5l} and \code{eq5dy3l}. Calculate EQ-5D index scores for
EQ-5D-3L, EQ-5D-5L and EQ-5D-Y-3L. Available value sets can be viewed using the function
\code{valuesets}.
}
\examples{

#EQ-5D-5L single utility score by dimension
eq5d(scores=c(MO=1,SC=2,UA=3,PD=4,AD=5), type="VT",
 country="Indonesia", version="5L")
 
#EQ-5D-3L single utility score by dimension
eq5d(scores=c(MO=3,SC=2,UA=3,PD=2,AD=3),
 type="TTO", version="3L", country="Germany")

#Mapping an EQ-5D-5L utility score to EQ-5D-3L using NICE DSU method
eq5d(0.922, country="UK", version="5L", type="DSU",
 age=18, sex="male")

#Calculation of multiple EQ-5D-5L utility scores from a data.frame of dimensions
scores.df <- data.frame(
  MO=c(1,2,3,4,5), SC=c(1,5,4,3,2),
  UA=c(1,5,2,3,1), PD=c(1,3,4,3,4), AD=c(1,2,NA,2,1)
)

eq5d(scores.df, country="Canada", version="5L", type="VT", ignore.invalid=TRUE)

#Calculation of a utility score using five digit state
eq5d(scores=12321, type="TTO", version="3L", country="UK")

scores.df2 <- data.frame(
  state=c(11111,12121,23232,33333)
)

#Calculation of utility scores using a data.frame with five digit states
eq5d(scores=scores.df2, type="TTO", version="3L", country="UK", five.digit="state")

#Calculation of utility scores from a vector of five digit states
eq5d(scores=scores.df2$state, type="TTO", version="3L", country="UK")

#Mapping multiple utility scores from EQ-5D-5L to EQ-5D-3L using NICE DSU method
scores.df3 <- data.frame(
  Utility=c(0.715,0.435,0.95),
  Age=c(50,30,70),
  Sex=c("m","f","m"),
  bwidth=c(0.2,0.2,0.1)
)

#using bwidth column values (one per observation)
eq5d(scores.df3, type="DSU", version="5L", country="UK")

#using single bwidth value for whole dataset
eq5d(scores.df3, type="DSU", version="5L", country="UK", bwidth=0.1) 

}
