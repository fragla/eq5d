# This file generates external and internal data for valuesets.
root <- file.path(here::here(), "data-raw")

# valueset tables
CW <- read.csv(file.path(root, "CW.csv"), row.names = 1L)
# DSU3L <- read.csv(file.path(root, "DSU3L.csv"), row.names = 1L)
# DSU5L <- read.csv(file.path(root, "DSU5L.csv"), row.names = 1L)
DSU3L <- read.csv(file.path(root, "DSU3L_2026.csv"), row.names = 1L)
DSU5L <- read.csv(file.path(root, "DSU5L_2026.csv"), row.names = 1L)
RCW <- read.csv(file.path(root, "RCW.csv"), row.names = 1L)
TTO <- read.csv(file.path(root, "TTO.csv"), row.names = 1L)
VAS <- read.csv(file.path(root, "VAS.csv"), row.names = 1L)
VT <- read.csv(file.path(root, "VT.csv"), row.names = 1L)
Y3L <- read.csv(file.path(root, "Y.csv"), row.names = 1L)

# DSU ranges
#DSU3LRANGE <- as.list(read.csv("data-raw/DSU3LRANGE.csv", row.names = 1L))
#DSU5LRANGE <- as.list(read.csv("data-raw/DSU5LRANGE.csv", row.names = 1L))

dsu_ranges <- function(x) {
  cols <- c("MO", "SC", "UA", "PD", "AD", "State", "Age", "Sex", grep("Copula", colnames(x), value = TRUE))
  ranges <- lapply(x[, which(!names(x) %in% cols)], range)
  ranges
}

DSU3LRANGE <- dsu_ranges(DSU3L)
DSU5LRANGE <- dsu_ranges(DSU5L)

# valueset references
REFERENCES <- read.csv(file.path(root, "references.csv"))
REFERENCES[REFERENCES == ""] <- NA

# make states (note we reverse and combine to match previously used data)
s3<- expand.grid(replicate(5L, 1:3, simplify = FALSE))
s3 <- do.call(paste0, s3[,5:1])
s5<- expand.grid(replicate(5L, 1:5, simplify = FALSE))
s5 <- do.call(paste0, s5[,5:1])
STATES <- list(`3L` = s3, `5L` = s5)

# internal helper function to calculate VT scores for all states in a survey
.score_5l_vt_states <- function(states, survey) {
  
  keep <- function(x) {
    if (is.null(x) || length(x) == 0L) {
      NULL
    } else {
      unname(as.numeric(x))
    }
  }
  
  unname(vapply(states, function(st) {
    
    scores <- as.integer(strsplit(st, "")[[1]])
    names(scores) <- c("MO", "SC", "UA", "PD", "AD")
    
    values <- c(
      unname(survey["StartValue"]),
      keep(eq5d:::.dimensionScores(scores, survey)),
      keep(eq5d:::.minOneGreaterThan1(scores, survey)),
      keep(eq5d:::.level4Or5(scores, survey)),
      keep(eq5d:::.num45sq(scores, survey)),
      keep(eq5d:::.N4(scores, survey)),
      keep(eq5d:::.N5(scores, survey)),
      keep(eq5d:::.MOAD(scores, survey)),
      keep(eq5d:::.PDAD(scores, survey))
    )
    
    sum(values, na.rm = TRUE)
    
  }, numeric(1)))
}


# van Hout (2021) EQ-5D-3L to EQ-5D-5L
# VH_2021_PROBS <- read.csv(file.path(root, "VH2021_probs.csv"), row.names = 1L)
# COUNTRIES_5L <- colnames(VT)
# RCWVH <- lapply(COUNTRIES_5L, function(x) {
#   scores <- eq5d(eq5d:::STATES$`5L`, country = x, version = "5L", type = "VT", digits = Inf)
#   mapping <- as.matrix(VH_2021_PROBS) %*% as.matrix(scores)
# })
# RCWVH <- do.call(cbind, RCWVH)
# colnames(RCWVH) <- COUNTRIES_5L

VH_2021_PROBS <- read.csv(
  file.path(root, "VH2021_probs.csv"),
  row.names = 1L
)

COUNTRIES_5L <- colnames(VT)

RCWVH <- lapply(COUNTRIES_5L, function(country) {
  
  # Match eq5d5l(): name coefficient vector using VT rownames
  survey <- setNames(VT[[country]], rownames(VT))
  
  scores_5L <- .score_5l_vt_states(STATES$`5L`, survey)
  
  drop(as.matrix(VH_2021_PROBS) %*% matrix(scores_5L, ncol = 1))
})

RCWVH <- do.call(cbind, RCWVH)
colnames(RCWVH) <- COUNTRIES_5L

# Build sysdata.rda
usethis::use_data(
    CW, DSU3L, DSU5L, RCW, RCWVH, TTO, VAS, VT, Y3L,
    DSU3LRANGE, DSU5LRANGE,
    REFERENCES,
    STATES,
    internal = TRUE,
    version = 3,
    overwrite = TRUE
)

tools::resaveRdaFiles("R/")
