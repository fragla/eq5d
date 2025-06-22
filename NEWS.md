
<!-- NEWS.md is generated from NEWS.Rmd. Please edit that file -->

# eq5d 0.16.0 (2025-06-19)

- Moroccan EQ-5D-5L value set added.

- “digits” argument added to eq5d3l, eq5d5l and eq5dy3l functions to
  enable scores to be returned with more precision.

- van Hout (2021) reverse crosswalk method added to eq5drcw function.
  This is now the default and recommended option for mapping EQ-5D-3L to
  EQ-5D-5L. The previous EuroQol (2019) mappings can be accessed from
  within R by specifying the `method = "EQ"` argument.

- Shiny app updated to include van Hout (2021) reverse crosswalk value
  sets. EuroQol (2019) value sets now removed.

# eq5d 0.15.7 (2025-02-28)

- United Arab Emirates EQ-5D-5L value set added.

- Bug fixed in calculating index score for 11111 using Australian and
  Sri Lankan EQ-5D-3L value sets with the latest R development version
  on Ubuntu.

# eq5d 0.15.6 (2025-01-17)

- Norwegian EQ-5D-5L value set added.

# eq5d 0.15.5 (2024-11-19)

- Ghana EQ-5D-5L value set added.

- Jordan EQ-5D-3L and EQ-5D-5L to EQ-5D-3L crosswalk value sets added.

# eq5d 0.15.4 (2024-09-04)

- Trinidad and Tobago EQ-5D-5L value set added.

- Bermuda EQ-5D-3L and EQ-5D-5L to EQ-5D-3L crosswalk value sets added.

- Brazilian EQ-5D-Y value set added.

- EQ-5D-Y changed to EQ-5D-Y-3L. “Y” value for version parameter is now
  deprecated. “Y3L” should be used instead.

- “digits” argument added to eq5d and eq5dmap functions to enable NICE
  DSU mapping scores to be returned with more precision.

# eq5d 0.15.3 (2024-04-29)

- Saudi Arabia EQ-5D-5L value set added.

# eq5d 0.15.2 (2024-01-31)

- Japan EQ-5D-5L hybrid DCE value set added in addition to TTO.

- Bug calculating Shannon H.max for 3L profiles when permutations = TRUE
  fixed.

- Helper functions now use camel case instead of snake case.
  getHealthStates renamed to get_all_health_states.

- URLs updated for new EuroQol website.

# eq5d 0.15.1 (2023-11-18)

- Pakistan EQ-5D-3L value set added.

# eq5d 0.15.0 (2023-07-13)

- Iranian and Slovenian EQ-5D-5L value sets added.

- Health Profile Grid plot added to the web app.

- When analysing data using the NICE DSU models, the value passed to
  bwidth argument in the eq5d function can now be a column name (column
  contains a value for each observation) or a single numerical value to
  be applied to all observations.

- New vignette describing how to map between EQ-5D-5L and EQ-5D-3L using
  the NICE DSU EEPRU models.

# eq5d 0.14.0 (2023-02-03)

- Functions for Probability of Superiority, Health Profile Grid, Health
  State Density Index and Shannon’s Index added.

- “summary” argument added to PCHC function to enable either all data or
  a summary table to be returned.

- Chinese and Indonesian EQ-5D-Y value sets added.

- Australian EQ-5D-5L value set added.

- Swedish “social” EQ-5D-5L value set added. There are now two EQ-5D-5L
  value sets, the 2020 “experience-based” and the new 2022
  “social-based” value set.

- Links to value set references, HSDC plot and HSDI/Shannon scores added
  to the web app.

# eq5d 0.13.0 (2022-11-11)

- Belgian, Dutch and Hungarian EQ-5D-Y value sets added

- valuesets function now returns PubMed/DOI/ISBN/external URL.

- Restructuring of how value set data is stored internally.

- Thanks to @TimTaylor for implementing both of these things.

- NICE DSU functionality added to the Shiny app.

# eq5d 0.12.0 (2022-07-26)

- Philippine, Romanian and Indian EQ-5D-5L value sets added.

- Bug in Shiny app fixed. App was unable to see value sets for index
  score calculation.

- Incorrect MO3 value for German EQ-5D-3L TTO fixed.

- Changed Japanese EQ-5D-5L value set to TTO instead of DCE.

# eq5d 0.11.0 (2022-04-05)

- NICE DSU age and sex based mapping from EQ-5D-5L to EQ-5D-3L and
  EQ-5D-3L to EQ-5D-5L for China, Germany, Japan, Netherlands, South
  Korea, Spain and UK added.

- Egyptian, New Zealand, Ugandan and Western Preference Pattern (WePP)
  EQ-5D-5L value sets added.

- Spanish EQ-5D-Y value set added.

- Code speed up in eq5d5l, eq5d3l and eq5d3y. Thanks to @TimTaylor for
  implementing this.

# eq5d 0.10.1 (2021-11-12)

- Italian EQ-5D-5L value sets added.

- Value set data now internalised to enable functions to work without
  loading the whole package. Thanks to @TimTaylor for implementing this.

# eq5d 0.10.0 (2021-07-27)

- Belgian and Mexican EQ-5D-5L value sets added.

- EQ-5D-3L reverse crosswalk value sets added for England, Germany,
  Netherlands and USA.

- Ecuadorian EQ-5D-3L value set added.

- Egyptian EQ-5D-5L removed due to article being retracted - see
  [PubMed](https://pubmed.ncbi.nlm.nih.gov/34129227/).

- Clearer messaging about ‘ignore.invalid’ parameter error message.

# eq5d 0.9.0 (2021-04-12)

- Slovenian and Japanese EQ-5D-Y value sets now available.

- Russian EQ-5D-3L value set added.

- Egyptian EQ-5D-5L and Russian EQ-5D-5L crosswalk value sets added.

# eq5d 0.8.1 (2021-02-09)

- Tunisian EQ-5D-3L value set added.

- Danish EQ-5D-5L value set added.

# eq5d 0.8.0 (2020-11-11)

- New analysis methods from Devlin, Janssen and Parkin’s new books
  added. Cumulative frequency, Level Sum Score (LSS), Level Frequency
  Score (LFS) and Paretian Classification of Health Change (PCHC).

- Hungarian EQ-5D-3L and EQ-5D-5L value sets added.

- Peruvian cTTO and DCE EQ-5D-5L value sets added.

- Option to calculate LSS/LFS added to Shiny app.

- ignore.incomplete in eq5d.R function and associated documents changed
  to ignore.invalid for consistency across package.

# eq5d 0.7.2 (2020-07-20)

- Added Vietnamese EQ-5D-5L value set.

- eq5dds dimension columns can now be specified using dimensions
  argument.

- Modal dialog box added to Shiny app for column selection.

- Bug fixed in Shiny app using EQ-5D-5L with missing data.

# eq5d 0.7.1 (2020-06-06)

- Added Swedish EQ-5D-5L value set.

- Bug fixed checking for dimension names in vectors.

- Updates for testing with R-4.0.0.

# eq5d 0.7.0 (2020-04-05)

- eq5dds function added to summarise EQ-5D descriptive system.

- eq5d function rewritten to allow a vector of five digit numbers to
  used. Thanks to @bitowaqr for the initial code.

- Added getHealthStates and splitHealthStates helper functions.

- Updated error messages.

- Shiny application updated.

  - Barplot added to display eq5dds info.
  - Summary eq5dds table added for barplot/radar plot.

# eq5d 0.6.0 (2020-02-03)

- Added French EQ-5D-5L value set.

- Minor edits to docs.

- Fixed unit test failing due to new implementation of round function.

# eq5d 0.5.0 (2019-12-15)

- Wilcoxon rank sum test and Kruskal-Wallis/Dunn tests added to shiny
  application.

- Wilcoxon signed rank test and Friedman/Nemenyi tests also added for
  paired data.

- Added Ethiopian EQ-5D-5L value set.

- Added ignore.incomplete option to eq5d function to skip incomplete
  scores.

- Added dimensions and five.digit options to eq5d function to change
  default column names when using data.frames.

- Addressed the noLD unit test issues in the previous version.

# eq5d 0.4.0 (2019-11-12)

- Added Polish and Portuguese EQ-5D-5L value sets.

- Added Argentinian, Australian, Chinese, Sri Lankan and Swedish
  EQ-5D-3L TTO value sets.

- Functionality added to handle dimension scores in five digit format
  e.g. 12321

- Speed improvements when calculating multiple index scores.

# eq5d 0.3.0 (2019-09-12)

- Added Iran and Malaysia EQ-5D-3L VAS value sets.

- Added Singapore EQ-5D-3L TTO value set.

- Added USA EQ-5D-5L value set.

- Updates to shiny application

  - New navbar layout
  - Added radar plot.
  - Ability to customise plots by adding/removing specific data groups.
  - Tool tips added means/medians of density/ecdf plots.
  - shinycssloader icons used when waiting for a plot to load in shiny.
  - Download button added to plots page.
  - Help/FAQ page added.

# eq5d 0.2.0 (2019-06-27)

- Added Brazilian, Canadian, Chilean, Polish, Portuguese, Taiwanese and
  Trinidad and Tobago EQ-5D-3L TTO value sets.

- Added Taiwanese EQ-5D-5L VT value set.

- Example data added to shiny app.

- Plots tab and ggplot2 density and ecdf plots added to shiny app.

- Added functionality to shiny app to calculate a single index score.

- Notification added to shiny app to warn when EQ-5D-5L is selected, but
  all uploaded values are 1, 2 or 3.

# eq5d 0.1.1 (2019-05-27)

- Fixed issue where displayed description values were mislabelled in the
  Shiny app table when the columns in the uploaded file weren’t in the
  order MO, SC, UA, PD, AD.

- Fixed issue with Shiny app download button not working correctly.

- Added checkbox to Shiny app to give the option of displaying the index
  scores with all info provided in the uploaded file or just with the
  dimension scores.

# eq5d 0.1.0 (2019-05-10)

- Functions for calculating EQ-5D index values.

- EQ-5D-3L TTO value sets for Denmark, France, Germany, Italy, Japan,
  Korea, Netherlands, Spain, Thailand, UK, USA and Zimbabwe.

- EQ-5D-3L VAS value sets for Belgium, Denmark, Europe, Finland,
  Germany, NewZealand, Slovenia, Spain and UK.

- EQ-5D-5L VT value sets for Canada, China, England, Germany, HongKong,
  Indonesia, Ireland, Japan, Korea, Malaysia, Netherlands, Spain,
  Thailand and Uruguay.

- EQ-5D-5L Crosswalk value sets for Denmark, France, Germany, Japan,
  Netherlands, Spain, Thailand, UK, USA and Zimbabwe.
