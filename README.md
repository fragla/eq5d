
<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

[![R build
status](https://github.com/fragla/eq5d/workflows/R-CMD-check/badge.svg)](https://github.com/fragla/eq5d/actions)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/fragla/eq5d?branch=master&svg=true)](https://ci.appveyor.com/project/fragla/eq5d)
[![Codecov test
coverage](https://codecov.io/gh/fragla/eq5d/branch/master/graph/badge.svg)](https://codecov.io/gh/fragla/eq5d?branch=master)
<!-- badges: end -->

# EQ-5D

EQ-5D is a popular health related quality of life instrument used in the
clinical and economic evaluation of health care. Developed by the
[EuroQol](https://euroqol.org/) group, the instrument consists of two
components: health state description and evaluation.

For the description component a subject self-rates their health in terms
of five dimensions; mobility, self-care, usual activities,
pain/discomfort, and anxiety/depression using either a three-level
([EQ-5D-3L](https://euroqol.org/eq-5d-instruments/eq-5d-3l-about/)) or a
five-level
([EQ-5D-5L](https://euroqol.org/eq-5d-instruments/eq-5d-5l-about/))
scale.

The evaluation component requires a patient to record their overall
health status using a visual analogue scale (EQ-VAS).

Following assessment the scores from the descriptive component can be
reported as a five digit number ranging from 11111 (full health) to
33333/55555 (worst health). A number of methods exist for analysing
these five digit profiles. However, frequently they are converted to a
single utility index using country specific value sets, which can be
used in the clinical and economic evaluation of health care as well as
in population health surveys.

The eq5d package provides methods for the cross-sectional and
longitudinal analysis of EQ-5D profiles and also the calculation of
utility index scores from a subject’s dimension scores. Additionally, a
[Shiny](https://shiny.rstudio.com) app is included to enable the
calculation, visualisation and automated statistical analysis of
multiple EQ-5D index values via a web browser using EQ-5D dimension
scores stored in CSV or Excel files.

Value sets for EQ-5D-3L are available for many countries and have been
produced using the time trade-off (TTO) valuation technique or the
visual analogue scale (VAS) valuation technique. Some countries have TTO
and VAS value sets for EQ-5D-3L.

For EQ-5D-5L, a standardised valuation study protocol (EQ-VT) was
developed by the EuroQol group based on the composite time trade-off
(cTTO) valuation technique supplemented by a discrete choice experiment
(DCE). The EuroQol group recommends users to use a standard value set
where available.

The EQ-5D-5L “crosswalk” value sets published by [van Hout *et
al*](https://pubmed.ncbi.nlm.nih.gov/22867780/) are also included. The
crosswalk value sets enable index values to be calculated for EQ-5D-5L
data where no value set is available by mapping between the EQ-5D-5L and
EQ-5D-3L descriptive systems.

Additional information on EQ-5D can be found on the
[EuroQol](https://euroqol.org/) website as well as in [Szende *et al*
(2007)](https://www.doi.org/10.1007/1-4020-5511-0) and [Szende *et al*
(2014)](https://www.doi.org/10.1007/978-94-007-7596-1). Advice on
[choosing a value
set](https://euroqol.org/eq-5d-instruments/eq-5d-3l-about/valuation/choosing-a-value-set/)
can also be found on the EuroQol website.

## Installation

You can install the released version of eq5d from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("eq5d")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("fragla/eq5d")
```

## Quick Start

``` r
library(eq5d)

#single calculation

#named vector MO, SC, UA, PD and AD represent mobility, self-care, usual activites, pain/discomfort and anxiety/depression, respectfully.
scores <- c(MO=1,SC=2,UA=3,PD=2,AD=1)

#EQ-5D-3L using the UK TTO value set
eq5d(scores=scores, country="UK", version="3L", type="TTO")
#> [1] 0.329

#Using five digit format
eq5d(scores=12321, country="UK", version="3L", type="TTO")
#> [1] 0.329

#multiple calculations using the Canadian VT value set

#data.frame with individual dimensions
scores.df <- data.frame(
  MO=c(1,2,3,4,5), SC=c(1,5,4,3,2), UA=c(1,5,2,3,1), PD=c(1,3,4,3,4), AD=c(1,2,1,2,1)
)

eq5d(scores.df, country="Canada", version="5L", type="VT")
#> [1] 0.949 0.362 0.390 0.524 0.431

#data.frame using five digit format
scores.df2 <- data.frame(state=c(11111,25532,34241,43332,52141))

eq5d(scores.df2, country="Canada", version="5L", type="VT", five.digit="state")
#> [1] 0.949 0.362 0.390 0.524 0.431

#or using a vector

eq5d(scores.df2$state, country="Canada", version="5L", type="VT")
#> [1] 0.949 0.362 0.390 0.524 0.431
```

## Value sets

The available value sets can be viewed using the ***valuesets***
function. The results can be filtered by EQ-5D version, value set type
or by country.

``` r
# Return all value sets (top 6 returned for brevity).
head(valuesets())
#>    Version Type   Country
#> 1 EQ-5D-3L  TTO Argentina
#> 2 EQ-5D-3L  TTO Australia
#> 3 EQ-5D-3L  TTO    Brazil
#> 4 EQ-5D-3L  TTO    Canada
#> 5 EQ-5D-3L  TTO     Chile
#> 6 EQ-5D-3L  TTO     China

# Return VAS based value sets (top 6 returned for brevity).
head(valuesets(type="VAS"))
#>    Version Type Country
#> 1 EQ-5D-3L  VAS Belgium
#> 2 EQ-5D-3L  VAS Denmark
#> 3 EQ-5D-3L  VAS  Europe
#> 4 EQ-5D-3L  VAS Finland
#> 5 EQ-5D-3L  VAS Germany
#> 6 EQ-5D-3L  VAS    Iran

# Return EQ-5D-5L value sets (top 6 returned for brevity).
head(valuesets(version="5L"))
#>    Version Type  Country
#> 1 EQ-5D-5L   VT   Canada
#> 2 EQ-5D-5L   VT    China
#> 3 EQ-5D-5L   VT  Denmark
#> 4 EQ-5D-5L   VT  England
#> 5 EQ-5D-5L   VT Ethiopia
#> 6 EQ-5D-5L   VT   France

# Return all UK value sets.
valuesets(country="UK")
#>    Version Type Country
#> 1 EQ-5D-3L  TTO      UK
#> 2 EQ-5D-3L  VAS      UK
#> 3 EQ-5D-5L   CW      UK
```

## Analysis of EQ-5D Profiles

A number of methods have been published that enable the analysis of
EQ-5D profiles, most recently in the open access book Methods for
Analysing and Reporting EQ-5D Data by [Devlin, Janssen and
Parkin](https://www.springer.com/gp/book/9783030476212). The eq5d
package includes R implentations of some of the methods from this book
and from other sources that may be of use in analysing EQ-5D data.

### Cumulative frequency analysis

The ***eq5dcf*** function calculates the frequency, percentage,
cumulative frequency and cumulative percentage for each five digit
profile in an EQ-5D dataset. Either a vector of five digit profiles or a
data.frame of indiviual dimensions can be passed to this function in
order to summarise data in this way.

``` r
library(readxl)

#load example data
data <- read_excel(system.file("extdata", "eq5d3l_example.xlsx", package="eq5d"))

#run eq5dcf function on a data.frame
res <- eq5dcf(data, "3L")

# Return data.frame of cumulative frequency stats (top 6 returned for brevity).
head(res)
#>   State Frequency Percentage CumulativeFreq CumulativePerc
#> 1 11121        36       18.0             36           18.0
#> 2 11111        24       12.0             60           30.0
#> 3 22222        21       10.5             81           40.5
#> 4 22221        18        9.0             99           49.5
#> 5 11221        12        6.0            111           55.5
#> 6 21221        11        5.5            122           61.0
```

### Summarising the Severity of EQ-5D Profiles

The eq5d package includes methods for summarising the severity of EQ-5D
health state. The Level Sum Score (LSS) treats each dimension’s level as
a number rather than a category. Each number is added up to produce a
score between 5 and 15 for EQ-5D-3L and 5 and 25 for EQ-5D-5L.

``` r
lss(c(MO=1,SC=2,UA=3,PD=2,AD=1), version="3L")
#> [1] 9

lss(55555, version="5L")
#> [1] 25

lss(c(11111,12345, 55555), version="5L")
#> [1]  5 15 25
```

The Level Frequency Score (LFS) is an alternative method of summarising
profile data developed by [Oppe and de
Charro](https://pubmed.ncbi.nlm.nih.gov/11189116). Here the frequency of
the levels for each health state are characterised. As described in
Devlin, Janssen and Parkin’s book, the full health profile 11111 for
EQ-5D-5L has 5, 1 s, no level 2, 3, 4 and 5s, so the LFS is 50000; the
health profile 55555 is 00005; profiles such as 31524 and 53412 would be
11111.

``` r
lfs(c(MO=1,SC=2,UA=3,PD=2,AD=1), version="3L")
#> [1] "221"

lfs(55555, version="5L")
#> [1] "00005"

lfs(c(11111,12345, 55555), version="5L")
#> [1] "50000" "11111" "00005"
```

### Paretian Classification of Health Change

The Paretian Classification of Health Change (PCHC) was developed by
[Devlin et al](https://pubmed.ncbi.nlm.nih.gov/20623685) in 2010 and is
used to compare changes in individuals over time. PCHC classifies the
change in an individual’s health state as better (improvement in at
least one dimension), worse (a deterioration in at least one dimension),
mixed (improvements and deteriorations in dimensions) or there being no
change in the health state. Those classified in the No change group with
the 11111 health state can be separated into their own “No problems”
group. PCHC can be calculated using the ***pchc*** function.

``` r
library(readxl)

#load example data
data <- read_excel(system.file("extdata", "eq5d3l_example.xlsx", package="eq5d"))

#use first 50 entries of each group as pre/post
pre <- data[data$Group=="Group1",][1:50,]
post <- data[data$Group=="Group2",][1:50,]

#run pchc function on data.frames

#Show no change, improve, worse, mixed without totals
res1 <- pchc(pre, post, version="3L", no.problems=FALSE, totals=FALSE)
res1
#>              Number Percent
#> No change         5      10
#> Improve          32      64
#> Worsen           10      20
#> Mixed change      3       6

#Show totals, but not those with no problems
res2 <- pchc(pre, post, version="3L", no.problems=FALSE, totals=TRUE)
res2
#>              Number Percent
#> No change         5      10
#> Improve          32      64
#> Worsen           10      20
#> Mixed change      3       6
#> Total            50     100

#Show totals and no problems for each dimension
res3 <- pchc(pre, post, version="3L", no.problems=TRUE, totals=TRUE, by.dimension=TRUE)
res3
#> $MO
#>                     Number Percent
#> No change               16    57.1
#> Improve                 11    39.3
#> Worsen                   1     3.6
#> Total with problems     28    56.0
#> No problems             22    44.0
#> 
#> $SC
#>                     Number Percent
#> No change               10    37.0
#> Improve                 14    51.9
#> Worsen                   3    11.1
#> Total with problems     27    54.0
#> No problems             23    46.0
#> 
#> $UA
#>                     Number Percent
#> No change               10      25
#> Improve                 26      65
#> Worsen                   4      10
#> Total with problems     40      80
#> No problems             10      20
#> 
#> $PD
#>                     Number Percent
#> No change               27    57.4
#> Improve                 19    40.4
#> Worsen                   1     2.1
#> Total with problems     47    94.0
#> No problems              3     6.0
#> 
#> $AD
#>                     Number Percent
#> No change                7    30.4
#> Improve                  9    39.1
#> Worsen                   7    30.4
#> Total with problems     23    46.0
#> No problems             27    54.0
```

### EQ-5D-DS

The ***eq5dds*** function is an R approximation of the Stata command
written by [Ramos-Goñi &
Ramallo-Fariña](https://www.stata-journal.com/article.html?article=st0450).
The function analyses and summarises the descriptive components of an
EQ-5D dataset. The “by” argument enables a grouping variable to be
specified when analysing the data subgroup.

``` r

dat <- data.frame(
         matrix(
           sample(1:3,5*12, replace=TRUE),12,5, 
           dimnames=list(1:12,c("MO","SC","UA","PD","AD"))
         ),
         Sex=rep(c("Male", "Female"))
       )

eq5dds(dat, version="3L")
#>     MO   SC   UA   PD   AD
#> 1 41.7 16.7 33.3 41.7 25.0
#> 2 41.7 58.3 25.0 16.7 41.7
#> 3 16.7 25.0 41.7 41.7 33.3

eq5dds(dat, version="3L", counts=TRUE)
#>   MO SC UA PD AD
#> 1  5  2  4  5  3
#> 2  5  7  3  2  5
#> 3  2  3  5  5  4

eq5dds(dat, version="3L", by="Sex")
#> data[, by]: Female
#>     MO   SC   UA   PD   AD
#> 1 16.7 33.3 33.3 33.3 50.0
#> 2 66.7 33.3  0.0 16.7 33.3
#> 3 16.7 33.3 66.7 50.0 16.7
#> ------------------------------------------------------------ 
#> data[, by]: Male
#>     MO   SC   UA   PD AD
#> 1 66.7  0.0 33.3 50.0  0
#> 2 16.7 83.3 50.0 16.7 50
#> 3 16.7 16.7 16.7 33.3 50
```

## Helper functions

Helper functions are included, which may be useful in the processing of
EQ-5D data. ***getHealthStates*** returns a vector of all possible five
digit health states for a specified EQ-5D version.
***getDimensionsFromHealthStates*** splits a vector of five digit health
states into a data.frame of their individual components and
***getHealthStatesFromDimensions*** combines indiviual dimensions in a
data.frame into five digit health states.

``` r

# Get all EQ-5D-3L five digit health states (top 6 returned for brevity).
head(getHealthStates("3L"))
#> [1] "11111" "11112" "11113" "11121" "11122" "11123"

# Split five digit health states into their individual components.
getDimensionsFromHealthStates(c("12345", "54321"), version="5L")
#>   MO SC UA PD AD
#> 1  1  2  3  4  5
#> 2  5  4  3  2  1
```

## Example data

Example data is included with the package and can be accessed using the
***system.file*** function.

``` r
# View example files.
dir(path=system.file("extdata", package="eq5d"))
#> [1] "eq5d3l_example.xlsx"            "eq5d3l_five_digit_example.xlsx"
#> [3] "eq5d5l_example.xlsx"

# Read example EQ-5D-3L data.
library(readxl)
data <- read_excel(system.file("extdata", "eq5d3l_example.xlsx", package="eq5d"))

# Calculate index scores
scores <- eq5d(data, country="UK", version="3L", type="TTO")

# Top 6 scores
head(scores)
#> [1]  0.760  0.796 -0.003  0.796  0.656  1.000
```

## Shiny web interface

The calculation (and visualisation) of multiple EQ-5D indices can also
be performed by upload of a CSV or Excel file using the packaged
[Shiny](https://shiny.rstudio.com) app. This requires the
[shiny](https://cran.r-project.org/package=shiny),
[DT](https://cran.r-project.org/package=DT),
[FSA](https://cran.r-project.org/package=FSA),
[ggplot2](https://cran.r-project.org/package=ggplot2),
[ggiraph](https://cran.r-project.org/package=ggiraph),
[ggiraphExtra](https://cran.r-project.org/package=ggiraphExtra),
[mime](https://cran.r-project.org/package=mime),
[PMCMRplus](https://cran.r-project.org/package=PMCMRplus),
[readxl](https://cran.r-project.org/package=readxl),
[shinycssloaders](https://cran.r-project.org/package=shinycssloaders)
and [shinyWidgets](https://cran.r-project.org/package=shinyWidgets)
packages. Ideally the CSV/Excel headers should be the same as the names
of the vector passed to the ***eq5d*** function i.e. MO, SC, UA, PD and
AD or the column name “State” if using the five digit format. However, a
modal dialog will prompt the user to select the appropriate columns if
the defaults can not be found. Both files below will produce the same
results.

![Shiny EQ-5D app excel data
formats](man/figures/shiny_app_excel_scores.png)

The app is launched using the ***shiny\_eq5d*** function.

``` r
shiny_eq5d()
```

Alternatively, it can be accessed without installing R/Shiny/eq5d by
visiting [shinyapps.io](https://fragla.shinyapps.io/shiny-eq5d).

![Shiny EQ-5D app main
screenshot](man/figures/shiny_app_screenshot_main.png) ![Shiny EQ-5D app
main screenshot](man/figures/shiny_app_screenshot_barplot.png) ![Shiny
EQ-5D app density plot
screenshot](man/figures/shiny_app_screenshot_density.png) ![Shiny EQ-5D
app ecdf plot screenshot](man/figures/shiny_app_screenshot_ecdf.png)
![Shiny EQ-5D app radar plot
screenshot](man/figures/shiny_app_screenshot_radar.png) ![Shiny EQ-5D
app posthoc stats plot
screenshot](man/figures/shiny_app_screenshot_posthoc.png)

## License

This project is licensed under the MIT License - see the
[LICENSE.md](https://github.com/fragla/eq5d/blob/master/LICENSE.md) file
for details.
