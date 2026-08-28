
<!-- README.md is generated from README.Rmd. Please edit that file -->

# eq5d

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/eq5d)](https://CRAN.R-project.org/package=eq5d)
[![Downloads](https://cranlogs.r-pkg.org/badges/grand-total/eq5d)](https://CRAN.R-project.org/package=eq5d)
[![R-CMD-check](https://github.com/fragla/eq5d/actions/workflows/check-standard.yaml/badge.svg)](https://github.com/fragla/eq5d/actions/workflows/check-standard.yaml)
[![Codecov test
coverage](https://codecov.io/gh/fragla/eq5d/branch/master/graph/badge.svg)](https://app.codecov.io/gh/fragla/eq5d?branch=master)
[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://github.com/fragla/eq5d/blob/master/LICENSE.md)
<!-- badges: end -->

`eq5d` provides tools for the valuation, reporting and analysis of EQ-5D
health outcomes.

The package supports utility index calculation, descriptive system
reporting, severity and distributional summaries, longitudinal change
analysis and visualisation of EQ-5D profiles. A companion Shiny
application is also included for interactive analysis of EQ-5D datasets.

EQ-5D is a health-related quality-of-life instrument developed by the
EuroQol Group. Health is described using five dimensions: Mobility (MO),
Self-care (SC), Usual Activities (UA), Pain/Discomfort (PD) and
Anxiety/Depression (AD). `eq5d` supports valuation and analysis of
EQ-5D-3L, EQ-5D-Y-3L and EQ-5D-5L data.

## Installation

Install the latest release from
[CRAN](https://CRAN.R-project.org/package=eq5d):

``` r
install.packages("eq5d")
```

Install the development version from
[GitHub](https://github.com/fragla/eq5d):

``` r
# install.packages("remotes")
remotes::install_github("fragla/eq5d")
```

## Getting started

The examples below illustrate some of the main functionality provided by
`eq5d`.

### Utility index calculation

``` r
library(eq5d)
#> Loading required package: lifecycle
#> Loading required package: rlang

scores <- c(MO = 1, SC = 2, UA = 3, PD = 2, AD = 1)

eq5d(scores, country = "UK", version = "3L", type = "TTO")
#> [1] 0.329

# Five-digit health state
eq5d(12321, country = "UK", version = "3L", type = "TTO")
#> [1] 0.329
```

### Finding value sets

``` r
valuesets(
  country = "France",
  references = c("PubMed", "DOI")
)
#>    Version Type Country   PubMed                        DOI           Notes
#> 1 EQ-5D-3L  DSU  France       NA                       <NA>            <NA>
#> 2 EQ-5D-3L  TTO  France 21935715  10.1007/s10198-011-0351-x            <NA>
#> 3 EQ-5D-5L   CW  France 22867780 10.1016/j.jval.2012.02.008            <NA>
#> 4 EQ-5D-5L  DSU  France       NA                       <NA>            <NA>
#> 5 EQ-5D-5L   VT  France 31912325 10.1007/s40273-019-00876-4            <NA>
#> 6 EQ-5D-3L  RCW  France 34452708 10.1016/j.jval.2021.03.009 van Hout (2021)
```

### Descriptive-system reporting

``` r
dat <- read.csv(
  system.file("extdata", "eq5d3l_example.csv", package = "eq5d")
)

dd <- descriptive_data(
  dat,
  version = "3L",
  metric = "percent"
)

table_descriptive(dd)
#>   Level    MO  SC    UA  PD    AD
#> 1     1  52.5  60  39.5  17  65.5
#> 2     2  47.5  40  55.5  72  31.0
#> 3     3   0.0   0   5.0  11   3.5
#> 4 Total 100.0 100 100.0 100 100.0
```

### Severity summaries

``` r
lss(scores, version = "3L")
#> [1] 9

lfs(scores, version = "3L")
#> [1] "221"
```

### Change analysis

``` r
pre  <- subset(dat, Group == "Group1")
post <- subset(dat, Group == "Group2")

pchc(pre, post, version = "3L", summary = TRUE)
#>                     Number Percent
#> No change               14      14
#> Improve                 59      59
#> Worsen                  14      14
#> Mixed change            13      13
#> Total with problems    100     100
#> No problems              0       0
```

## Value sets

`eq5d` includes a wide range of EQ-5D-3L, EQ-5D-5L, EQ-5D-Y-3L,
crosswalk, reverse-crosswalk and DSU mapping value sets.

See `?valuesets` for details.

## Documentation

Further documentation is available in the package vignettes:

**Core reporting vignettes**

- Reporting EQ-5D Data with `eq5d`
- Reporting the EQ-5D Descriptive System
- Reporting EQ-5D Severity and Distributional Summaries
- Reporting EQ-5D Change Analysis

**Specialised vignettes**

- Mapping between EQ-5D-5L and EQ-5D-3L using the NICE DSU models

## Shiny application

Launch the companion Shiny application with:

``` r
shiny_eq5d()
```

Online version:

<https://fragla.shinyapps.io/shiny-eq5d>

## Contributing

Bug reports, feature requests and pull requests are welcome.

Please use the GitHub issue tracker:

<https://github.com/fragla/eq5d/issues>

## Citation

To obtain citation information for `eq5d`:

``` r
citation("eq5d")
```

## License

MIT License.
