
<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

[![Travis build
status](https://travis-ci.org/fragla/eq5d.svg?branch=master)](https://travis-ci.org/fragla/eq5d)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/fragla/eq5d?branch=master&svg=true)](https://ci.appveyor.com/project/fragla/eq5d)
[![Codecov test
coverage](https://codecov.io/gh/fragla/eq5d/branch/master/graph/badge.svg)](https://codecov.io/gh/fragla/eq5d?branch=master)
<!-- badges: end -->

# EQ-5D

EQ-5D is a popular health related quality of life instrument used in the
clinical and economic evaluation of health care. Developed by the
[EuroQol](https://www.euroqol.org) group, the instrument consists of two
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
33333/55555 (worst health). However, frequently the scores on these five
dimensions are converted to a single utility index using country
specific value sets, which can be used in the clinical and economic
evaluation of health care as well as in population health surveys.

The eq5d package provides methods to calculate index scores from a
subject’s dimension scores, for both EQ-5D-3L and EQ-5D-5L value sets.
Additionally, a [Shiny](https://shiny.rstudio.com) app is included to
enable the calculation, visualisation and automated statistical analysis
of multiple EQ-5D index values via a web browser using EQ-5D dimension
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
al*](https://www.ncbi.nlm.nih.gov/pubmed/22867780) are also included.
The crosswalk value sets enable index values to be calculated for
EQ-5D-5L data where no value set is available by mapping between the
EQ-5D-5L and EQ-5D-3L descriptive systems.

Additional information on EQ-5D can be found on the
[EuroQol](https://www.euroqol.org) website as well as in [Szende *et al*
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
#> 3 EQ-5D-5L   VT  England
#> 4 EQ-5D-5L   VT Ethiopia
#> 5 EQ-5D-5L   VT   France
#> 6 EQ-5D-5L   VT  Germany

# Return all UK value sets.
valuesets(country="UK")
#>    Version Type Country
#> 1 EQ-5D-3L  TTO      UK
#> 2 EQ-5D-3L  VAS      UK
#> 3 EQ-5D-5L   CW      UK
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
[ggplot2](https://cran.r-project.org/package=ggplot2),
[ggiraph](https://cran.r-project.org/package=ggiraph),
[ggiraphExtra](https://cran.r-project.org/package=ggiraphExtra),
[mime](https://cran.r-project.org/package=mime) and
[readxl](https://cran.r-project.org/package=readxl) packages. The
CSV/Excel headers should be the same as the names of the vector passed
to the ***eq5d*** function i.e. MO, SC, UA, PD and AD or the column name
“State” if using the five digit format. Both files below will produce
the same results.

![Shiny EQ-5D app excel data
formats](man/figures/shiny_app_excel_scores.png)

The app is launched using the ***shiny\_eq5d*** function.

``` r
shiny_eq5d()
```

Alternatively, it can be accessed without installing R/Shiny/eq5d by
visiting [shinyapps.io](https://fragla.shinyapps.io/shiny-eq5d).

![Shiny EQ-5D app main
screenshot](man/figures/shiny_app_screenshot_main.png)

![Shiny EQ-5D app density plot
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
