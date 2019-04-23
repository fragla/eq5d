shinyUI(pageWithSidebar(
  
  headerPanel("EQ-5D"),
  
  sidebarPanel(
    uiOutput("choose_version"),
    uiOutput("choose_country"),
    uiOutput("choose_type"),
    uiOutput("choose_dataset")
  ),
  
  
  mainPanel(
    DT::dataTableOutput("eq5d_table"),
    uiOutput("export")
  )
))