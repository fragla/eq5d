library(eq5d)
library(DT)
library(mime)
library(readxl)

shinyServer(function(input, output) {
  
  # Drop-down selection box for which data set
  output$choose_dataset <- renderUI({
    fileInput("data", "Choose data file",
              accept = c(
                mimemap["csv"],
                mimemap["xls"],
                mimemap["xlsx"])
    )
  })
  
  output$choose_version <- renderUI({
    radioButtons("version", "EQ-5D version:",
                 c("EQ-5D-3L"="3L", "EQ-5D-5L"="5L"),
                 selected="3L",
                 inline=T)
  })
  
  output$choose_country <- renderUI({
    if(is.null(input$version))
      return()
    
    countries <- sort(unique(as.character(valuesets(version=input$version)$Country)))

    selectInput("country", "Country:", 
                choices=countries, selected=FALSE, selectize = FALSE)
  })
  
  output$choose_type <- renderUI({
    if(is.null(input$country))
      return()
    
    type <- NULL
      if(input$version=="3L") {
        
          if(input$country %in% colnames(TTO))
            type <-c(type, "TTO")
          
          if(input$country %in% colnames(VAS))
            type <-c(type, "VAS")
          
      } else {
        
          if(input$country %in% colnames(VT))
            type <-c(type, "VT")
          
          if(input$country %in% colnames(CW))
            type <-c(type, "CW")
          
      }
    
    selectInput("type", "Type:", 
                choices=type, selected=FALSE, selectize = FALSE)
    
  })
  
  output$include_raw_data <- renderUI({
    checkboxInput("raw", "Include all submitted data in table", TRUE)
  })
  
  output$eq5d_table <- DT::renderDataTable({
    if(is.null(input$data) || is.null(input$version) || is.null(input$type) || is.null(input$country))
      return()
    
    vs <- valuesets(version=input$version, type=input$type, country=input$country)
    
    if(nrow(vs) != 1)
      return()
    
    res <- getTableData()

    return(res)
  })
  
  output$export<- renderUI({
    if(!is.null(input$data)) {
      downloadButton("download", 'Download Output File')
    }
  })
  
  output$download <- downloadHandler(
    filename = function() {
      paste(input$version, "_", input$country, "_", input$type, "_", format(Sys.time(), "%Y%m%d%M%S"), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(getTableData(), file, row.names = FALSE)
    }
  )
  
  rawdata <- reactive({
    if(input$data$type %in% c(mimemap["xls"], mimemap["xlsx"])) {
      dat <- read_excel(input$data$datapath)
      dat <- as.data.frame(dat)
    }
    else {
      dat <- read.csv(file=input$data$datapath, header=TRUE)
    }    
  })
  
  dataset <- reactive({
    dat <- rawdata()
    
    idx <- getColumnIndex(dat)
    if(is.null(idx)) {
      stop("Unable to identify EQ-5D dimensions in the file header.")
    }
    return(dat[,idx])
  })
  
  getColumnIndex <- function(dat) {
    
    short <- c("MO", "SC", "UA", "PD", "AD")
    long <- c("Mobility", "Care", "Activity", "Pain", "Anxiety")
    
    short.idx <- match(tolower(short), tolower(colnames(dat)))
    long.idx <- match(tolower(long), tolower(colnames(dat)))
    
    if(all(!is.na(short.idx))) {
      return(short.idx)
    } else if (all(!is.na(long.idx))) {
      return(long.idx)
    } else {
      return(NULL)
    }
  }
  
  getTableData <- function() {
    eq5d <- eq5d(dataset(), version=input$version, type=input$type, country=input$country)
    if(input$raw) {
      res <- cbind(rawdata(), eq5d)
      colnames(res)[ncol(res)] <- paste0("EQ-5D-", input$version)
    } else {
      res <- cbind(dataset(), eq5d)
      colnames(res) <- c("Mobility", "Self-care", "Usual activities", "Pain/discomfort", "Anxiety/depression", paste0("EQ-5D-", input$version))
    }
    return(res)
  }
})
