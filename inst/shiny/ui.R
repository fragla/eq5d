fluidPage(
  titlePanel("EQ-5D"),

  tabsetPanel(
    tabPanel("Data", tags$style(HTML("
              .col-sm-4, .col-sm-8 {margin-top:5px;}")),
      sidebarPanel(
        uiOutput("choose_version"),
        uiOutput("choose_country"),
        uiOutput("choose_type"),
        uiOutput("choose_dataset"),
        uiOutput("include_raw_data"),
        hr(),
        uiOutput("example_data")
      ),
      mainPanel(
        DT::dataTableOutput("eq5d_table"),
        uiOutput("export")
      )
    ),
    tabPanel("Plots",
      sidebarPanel(
        uiOutput("choose_plot_data"),
        uiOutput("choose_group_by"),
        uiOutput("show_mean")
        #plot dimensions/index
        #split by group
      ),
      mainPanel(
        h2("Density plot"),
        plotOutput("density_plot")
      )     
    )
  )
)
