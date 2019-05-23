shinyUI(pageWithSidebar(
  
  headerPanel("EQ-5D"),
  
  sidebarPanel(
    uiOutput("choose_version"),
    uiOutput("choose_country"),
    uiOutput("choose_type"),
    uiOutput("choose_dataset"),
    uiOutput("include_raw_data")
    
  ),
  
  
  mainPanel(
    DT::dataTableOutput("eq5d_table"),
    uiOutput("export")
  )
))