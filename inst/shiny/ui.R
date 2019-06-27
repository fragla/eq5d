shinyUI(
  fluidPage(
    titlePanel("EQ-5D"),

    tabsetPanel(
      tabPanel("Data", tags$style(HTML("
                .col-sm-4, .col-sm-8 {margin-top:5px;}")),
        sidebarPanel(
          uiOutput("choose_version"),
          uiOutput("choose_country"),
          uiOutput("choose_type"),
          uiOutput("choose_calc_type"),
          conditionalPanel(
            condition = "input.multi == 'multiple'",
            uiOutput("choose_dataset"),
            uiOutput("include_raw_data"),
            hr(),
            h5("Example data"),
            p("EQ-5D-3L example data:", a(img(src="images/icons8-microsoft-excel-48.png", height = 24, width = 24), href="data/eq5d3l_example.xlsx", target="_blank"), style="margin-bottom:0"),
            p("EQ-5D-5L example data:", a(img(src="images/icons8-microsoft-excel-48.png", height = 24, width = 24), href="data/eq5d5l_example.xlsx", target="_blank"), style="margin-top:0")
          )
        ),
        mainPanel(
          conditionalPanel(
            condition = "input.multi == 'multiple'",
            DT::dataTableOutput("eq5d_table"),
            uiOutput("export")
          ),
          conditionalPanel(
            condition = "input.multi == 'single'",
            uiOutput("choose_dimensions"),
            textOutput("eq5d_text")
          )
        )
      ),
      tabPanel("Plots",
        sidebarPanel(
          uiOutput("choose_plot_data"),
          uiOutput("choose_plot_type"),
          uiOutput("choose_group_by"),
          uiOutput("show_average"),
          uiOutput("choose_average_method")
        ),
        mainPanel(
          plotOutput("plot")
        )     
      )
    )
  )
)
