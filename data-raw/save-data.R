# This file generates external and internal data for valuesets.
root <- file.path(here::here(), "data-raw")

# valueset tables
CW <- read.csv(file.path(root, "CW.csv"), row.names = 1L)
DSU3L <- read.csv(file.path(root, "DSU3L.csv"), row.names = 1L)
DSU5L <- read.csv(file.path(root, "DSU5L.csv"), row.names = 1L)
RCW <- read.csv(file.path(root, "RCW.csv"), row.names = 1L)
TTO <- read.csv(file.path(root, "TTO.csv"), row.names = 1L)
VAS <- read.csv(file.path(root, "VAS.csv"), row.names = 1L)
VT <- read.csv(file.path(root, "VT.csv"), row.names = 1L)
Y <- read.csv(file.path(root, "Y.csv"), row.names = 1L)

# DSU ranges
DSU3LRANGE <- as.list(read.csv("data-raw/DSU3LRANGE.csv", row.names = 1L))
DSU5LRANGE <- as.list(read.csv("data-raw/DSU5LRANGE.csv", row.names = 1L))

# valueset references
REFERENCES <- read.csv(file.path(root, "references.csv"))
REFERENCES[REFERENCES == ""] <- NA

# make states (note we reverse and combine to match previously used data)
s3<- expand.grid(replicate(5L, 1:3, simplify = FALSE))
s3 <- do.call(paste0, s3[,5:1])
s5<- expand.grid(replicate(5L, 1:5, simplify = FALSE))
s5 <- do.call(paste0, s5[,5:1])
STATES <- list(`3L` = s3, `5L` = s5)

usethis::use_data(
    CW, DSU3L, DSU5L, RCW, TTO, VAS, VT, Y,
    DSU3LRANGE, DSU5LRANGE,
    REFERENCES,
    STATES,
    internal = TRUE,
    version = 3,
    overwrite = TRUE
)







