library(ggiraph)
library(shinycssloaders)

shinyUI(
  navbarPage(title="EQ-5D",
             position="fixed-top",
             header=singleton(tags$head(
               tags$style(type="text/css", "text {font-family: sans-serif}"),
               tags$style(type="text/css", "body {padding-top: 70px;}")
             )),
    
    tabPanel("Data", tags$style(HTML("
              .col-sm-4, .col-sm-8 {margin-top:5px;}")),
      sidebarPanel(
        uiOutput("choose_version"),
        uiOutput("choose_country"),
        uiOutput("choose_type"),
        uiOutput("reference_links"),
        uiOutput("choose_calc_type"),
        conditionalPanel(
          condition = "input.multi == 'multiple'",
          uiOutput("choose_dataset"),
          uiOutput("include_severity_scores"),
          uiOutput("include_raw_data"),
          hr(),
          h5("Example data"),
          p("EQ-5D-3L example data:", a(img(src="images/icons8-microsoft-excel-48.png", height = 24, width = 24), href="example-data/eq5d3l_example.xlsx", target="_blank"), style="margin-bottom:0"),
          p("EQ-5D-5L example data:", a(img(src="images/icons8-microsoft-excel-48.png", height = 24, width = 24), href="example-data/eq5d5l_example.xlsx", target="_blank"), style="margin-bottom:0")
        )
      ),
      mainPanel(
        conditionalPanel(
          condition = "input.multi == 'multiple'",
          withSpinner(DT::dataTableOutput("eq5d_table")),
          uiOutput("export_table")
        ),
        conditionalPanel(
          condition = "input.multi == 'single'",
          uiOutput("choose_dimensions"),
          conditionalPanel(
            condition = "input.type == 'DSU'",
            uiOutput("choose_utility"),
            uiOutput("choose_age"),
            uiOutput("choose_sex"),
            uiOutput("choose_bwidth")
          ),
          textOutput("eq5d_text")
        )
      )
    ),
    tabPanel("Analysis",
      sidebarPanel(
        conditionalPanel(
          condition = "input.plot_type != 'radar' && input.plot_type != 'summary' && input.plot_type != 'hsdc' && input.plot_type != 'hpg'",
          uiOutput("choose_plot_data")
        ),
        uiOutput("choose_plot_type"),
        uiOutput("choose_group_by"),
        conditionalPanel(
          condition = "input.group != 'None'",
          uiOutput("choose_group_members")
        ),
        conditionalPanel(
          condition = "input.plot_type != 'radar' && input.plot_type != 'summary' && input.plot_type != 'hsdc' && input.plot_type != 'hpg'",
          uiOutput("show_average"),
          uiOutput("choose_average_method")
        ),
        conditionalPanel(
          condition = "input.plot_type != 'radar' && input.plot_type != 'summary' && input.plot_type != 'hsdc'",
          uiOutput("show_paired")
        ),
        conditionalPanel(
          condition = "input.plot_type == 'radar' || input.plot_type == 'summary' || input.plot_type == 'hsdc'",
          uiOutput("choose_summary_type")
        ),
        conditionalPanel(
          condition = "input.plot_type == 'radar' || input.plot_type == 'summary' || input.plot_type == 'hsdc'",
          uiOutput("choose_shannon_type")
        )
      ),
      mainPanel(
        fluidRow(
          column(8, 
            withSpinner(girafeOutput("plot")),
          ),
          column(4,  style = "margin-top: 300px;",
            uiOutput("export_plot")
          )
        ),
        fluidRow(
          column(12,
          wellPanel(uiOutput("statistics"))
        )
        )
      )
    ),
    tabPanel("Settings",
             div(style="padding-left:20%;padding-right:20%;",
                 h3("Error handling"),
                 uiOutput("ignore_invalid")
             )
    ),
    tabPanel("Help/FAQs",
      div(style="padding-left:20%;padding-right:20%;",
        h4("How do I format my data for uploading?"),
        p("Data to be uploaded should be in either a Microsoft Excel (xlsx/xls) file or 
          a CSV file. The file should contain either a header row with the labels 
          MO, SC, UA, PD and AD (representing the dimensions Mobility, Self-care, 
          Usual activities, Pain/discomfort and Anxiety/depression) or use the column 
          label 'State' with the scores encoded in five digit format. Additional columns 
          can be included in the file as can been seen in the image below. Both formats 
          in the image will produce the same results. Non-numeric columns can be used for 
          grouping data in the plots tab. An example file can be downloaded " , 
          a("here", href="data/eq5d3l_example.xlsx", target="_blank"), "."),
        div(img(src="images/shiny_app_excel_scores.png", width="75%"), style="text-align:center"),
        p("Dimensions scores can also be converted from EQ-5D-3L to EQ-5D-5L and 
          EQ-5D-5L to EQ-5D-3L using the ", a(href='https://www.sheffield.ac.uk/nice-dsu/methods-development/mapping-eq-5d-5l-3l', 'NICE Decision Support Unit'),
          " age-sex based mappings. To use these value sets files require \"Age\" and 
          \"Sex\" columns in addition to dimension scores. Age should be specified 
          in years (18-100) or as a category (1: 18-34, 2: 35-44, 3: 45-54, 4: 55-64, 
          5: 65-100). Sex should be specified as Male or Female. In addition index 
          scores can also be converted. Scores should be specified in a \"Utility\" 
          column. If the scores provided are an approximate score then a \"bwidth\" 
          column must also be included. The NICE DSU recommendations are < 0.8: 0.2, 
          0.8-0.951: 0.1, 0.951-1: small, but large enough to include 1. Zero can be 
          used for an exact match. Example files can be seen in the image below and 
          more info can be found at the NICE DSU link above."),
        div(img(src="images/shiny_app_dsu_dimension.png", width="35%"), 
            img(src="images/shiny_app_dsu_utility.png", width="30%"),
            img(src="images/shiny_app_dsu_utility_bwidth.png", width="30%"),style="text-align:center"),
        h4("I've uploaded my data. What do I do now?"),
        p("Once a file of EQ-5D dimension scores has been successfully uploaded the 
          correct EQ-5D version and value set need to be selected from side bar for 
          the calculation of the correct index score. A table of the uploaded data 
          and calculated index scores will be displayed in the main panel. This can 
          be downloaded for further analysis, or the data can be visualised by 
          clicking on the plots page. Currently there are density and ECDF plots of the 
          calculated EQ-5D index scores as well as a radar plot of the individual 
          dimensions. If the uploaded data included additional categorical  
          information, it is possible to incorporate this information into the plots. 
          To do this select the category from the 'Group by' drop down menu on the 
          plots page. This information will also be used in the automated statistical 
          analysis. "),
        h4("My EQ-5D data is missing/incomplete. What should I do?"),
        p("The application will automatically skip invalid/incomplete/missing EQ-5D data. If 
          you wish to turn this feature off (and receive an error) you can do so by 
          deselecting the 'Ignore data with invalid/incomplete/missing dimension scores' 
          checkbox in the 'Settings' tab."),
        h4("What statistical analyses are performed?"),
        p("The statistical tests are performed when a density or ECDF plot are present 
          (for summary barplot and radar plot a summary table is generated). The test 
          performed depends on the data and the category selected in the 'Group by' 
          drop down. The statistical tests that can be performed can be seen in the 
          table below. The application will automatically try to determine whether the 
          data are paired. Paired testing may be switched off using the 'Data are 
          paired' checkbox."),
          tableOutput("stats_tests"),
        h4("What does 'Unable to identify EQ-5D dimensions in the file header' mean?"),
        p("This means it's not been possible for the software to find all of the 
          EQ-5D dimensions in the header of the uploaded file. This could be for a 
          few different reasons. Does the file have a header? Is the header the first 
          row of the file? Are there any spelling mistakes?"),
        h4("What does 'Scores must be coded as 1, 2 or 3 for EQ-5D-3L' mean?"),
        p("Dimension scores other than 1, 2 or 3 have been found in the file. These could be scores 
          lower than 1 or higher than 3, a decimal in the 1 to 3 range or a blank or 
          non-integer value."),
        h4("Can I download and install the application locally?"),
        p(HTML(paste0('Absolutely. Instructions for how to install and run the web 
          app locally in R can be found on the eq5d ', 
          a(href='https://github.com/fragla/eq5d', 'GitHub'), ' page'))),
        h4("How can I get help, request a feature or report bugs?"),
        p(HTML(paste0('Our preferred method of requesting new features or reporting 
          bugs with the R package or web application is by logging an issue on the 
          eq5d GitHub ', a(href='https://github.com/fragla/eq5d/issues', 'issue page'), '. 
          Alternatively, you can contact the maintainer via email. Contact details 
          can be found ', 
          a(href='https://github.com/fragla/eq5d/blob/master/DESCRIPTION', ' here'), '.')))
      )
    )
  )
)
