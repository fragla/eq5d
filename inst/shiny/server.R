library(eq5d)
library(DT)
library(mime)
library(xlsx)

shinyServer(function(input, output) {
  
  # Drop-down selection box for which data set
  output$choose_dataset <- renderUI({
    fileInput("data", "Choose data file",
              accept = c(
                "text/csv",
                "text/comma-separated-values,text/plain",
                ".csv",
                "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet")
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
    
    if(input$version=="5L") {
      countries <- colnames(VT)
    }
    else{
      countries <- sort(unique(c(colnames(TTO), colnames(VAS))))
    }
    selectInput("country", "Country:", 
                choices=countries, selected=FALSE, selectize = FALSE)
  })
  
  output$choose_type <- renderUI({
    if(is.null(input$version))
      return()
    
    if(input$version!="3L")
      return()
    
    type <- NULL
    if(!is.null(input$country)) {
      if(input$country %in% colnames(TTO))
        type <-c(type, "TTO")
      
      if(input$country %in% colnames(VAS))
        type <-c(type, "VAS")
    }
    
    selectInput("type", "Type:", 
                choices=type, selected=FALSE, selectize = FALSE)
    
  })
  
  output$eq5d_table <- DT::renderDataTable({
    if(is.null(input$data))
      return()
    
    datasetInput()
  })
  
  output$export<- renderUI({
    if(!is.null(input$data)) {
      downloadButton("download", 'Download Output File')
    }
  })
  
  output$download <- downloadHandler(
    filename = function() {
      type <- ifelse(input$version=="3L", paste0(input$country, "_", input$type), input$country)
      paste(input$version, "_", type, "_", format(Sys.time(), "%Y%m%d%M%S"), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(datasetInput(), file, row.names = FALSE)
    }
  )
  
  datasetInput <- reactive({
    if(input$data$type==mimemap["xlsx"]) {
      dat <- read.xlsx2(file=input$data$datapath, sheetIndex=1, header=TRUE)
    }
    else {
      dat <- read.csv(file=input$data$datapath, header=TRUE)
    }
    
    idx <- getColumnIndex(dat)
    if(is.null(idx)) {
      stop("Unable to identify EQ-5D dimensions in the file header.")
    }
    
    # if(input$version=="EQ-5D-5L" && !any(dat > 3)) {
    #   showNotification("No scores greater than 3 found. Is this really EQ-5D-5L?", type="message")
    # }
    
    eq5d <- sapply(1:nrow(dat), function(x) {
      if(input$version=="5L") {
        eq5d::eq5d5l(scores=c(Mobility=dat[x,idx[1]],Care=dat[x,idx[2]],Activity=dat[x,idx[3]],Pain=dat[x,idx[4]],Anxiety=dat[x,idx[5]]), country=input$country)
      } else {
        eq5d::eq5d3l(scores=c(Mobility=dat[x,idx[1]],Care=dat[x,idx[2]],Activity=dat[x,idx[3]],Pain=dat[x,idx[4]],Anxiety=dat[x,idx[5]]), type=input$type, country=input$country)
      }
    })
    res <- cbind(dat[,idx],eq5d)
    colnames(res) <- c("Mobility", "Self-care", "Usual activities", "Pain", "Anxiety/depression", paste0("EQ-5D-", input$version))
    
    res
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
})
