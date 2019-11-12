
<!-- NEWS.md is generated from NEWS.Rmd. Please edit that file -->
eq5d 0.4.0 (2019-11-12)
=======================

-   Added Polish and Portuguese EQ-5D-5L value sets.

-   Added Argentinian, Australian, Chinese, Sri Lankan and Swedish EQ-5D-3L TTO value sets.

-   Functionality added to handle dimension scores in five digit format e.g. 12321

-   Speed improvements when calculating multiple index scores.

eq5d 0.3.0 (2019-09-12)
=======================

-   Added Iran and Malaysia EQ-5D-3L VAS value sets.

-   Added Singapore EQ-5D-3L TTO value set.

-   Added USA EQ-5D-5L value set.

-   Updates to shiny application
    -   New navbar layout
    -   Added radar plot.
    -   Ability to customise plots by adding/removing specific data groups.
    -   Tool tips added means/medians of density/ecdf plots.
    -   shinycssloader icons used when waiting for a plot to load in shiny.
    -   Download button added to plots page.
    -   Help/FAQ page added.

eq5d 0.2.0 (2019-06-27)
=======================

-   Added Brazilian, Canadian, Chilean, Polish, Portuguese, Taiwanese and Trinidad and Tobago EQ-5D-3L TTO value sets.

-   Added Taiwanese EQ-5D-5L VT value set.

-   Example data added to shiny app.

-   Plots tab and ggplot2 density and ecdf plots added to shiny app.

-   Added functionality to shiny app to calculate a single index score.

-   Notification added to shiny app to warn when EQ-5D-5L is selected, but all uploaded values are 1, 2 or 3.

eq5d 0.1.1 (2019-05-27)
=======================

-   Fixed issue where displayed description values were mislabelled in the Shiny app table when the columns in the uploaded file weren't in the order MO, SC, UA, PD, AD.

-   Fixed issue with Shiny app download button not working correctly.

-   Added checkbox to Shiny app to give the option of displaying the index scores with all info provided in the uploaded file or just with the dimension scores.

eq5d 0.1.0 (2019-05-10)
=======================

-   Functions for calculating EQ-5D index values.

-   EQ-5D-3L TTO value sets for Denmark, France, Germany, Italy, Japan, Korea, Netherlands, Spain, Thailand, UK, USA and Zimbabwe.

-   EQ-5D-3L VAS value sets for Belgium, Denmark, Europe, Finland, Germany, NewZealand, Slovenia, Spain and UK.

-   EQ-5D-5L VT value sets for Canada, China, England, Germany, HongKong, Indonesia, Ireland, Japan, Korea, Malaysia, Netherlands, Spain, Thailand and Uruguay.

-   EQ-5D-5L Crosswalk value sets for Denmark, France, Germany, Japan, Netherlands, Spain, Thailand, UK, USA and Zimbabwe.
