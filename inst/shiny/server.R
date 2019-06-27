library(eq5d)
library(DT)
library(mime)
library(readxl)
library(ggplot2)

options(shiny.sanitize.errors = FALSE)

shinyServer(function(input, output) {
  
  # Drop-down selection box for which data set
  output$choose_dimensions <- renderUI({
    if(is.null(input$version))
      return()
    
    if(input$version=="3L") {
      levels <- 1:3
    } else {
      levels <- 1:5
    }

    splitLayout(
    
      selectInput("mo", "Mobility:", 
                  choices=levels, selected=FALSE, selectize = FALSE, width="100px"),
      selectInput("sc", "Self care:", 
                  choices=levels, selected=FALSE, selectize = FALSE, width="100px"),
      selectInput("ua", "Usual activities:",
                  choices=levels, selected=FALSE, selectize = FALSE, width="100px"),
      selectInput("pd", "Pain/discomfort:",
                  choices=levels, selected=FALSE, selectize = FALSE, width="100px"),
      selectInput("ad", "Anxiety/depression:",
                  choices=levels, selected=FALSE, selectize = FALSE, width="100px")
    )
    
  })
  output$choose_dataset <- renderUI({
    fileInput("data", "Choose data file",
              accept = c(
                mimemap["csv"],
                mimemap["xls"],
                mimemap["xlsx"])
    )
  })
  
  output$choose_calc_type <- renderUI({
    radioButtons("multi", "Multiple calculations:",
                 c("Single"="single", "Multiple"="multiple"),
                 selected="multiple",
                 inline=T)
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

  output$show_average <- renderUI({
    checkboxInput("average", "Show mean/median on plot", TRUE)
  })
  
  output$choose_average_method <- renderUI({
    radioButtons("average_method", "Show:",
                 c("Mean"="mean", "Median"="median"),
                 selected="mean",
                 inline=T)
  })
  
  output$eq5d_table <- DT::renderDataTable({
    if(is.null(input$data) || is.null(input$version) || is.null(input$type) || is.null(input$country))
      return()
    
    vs <- valuesets(version=input$version, type=input$type, country=input$country)
    
    if(nrow(vs) != 1)
      return()
    
    res <- getTableData()
    
    if(input$version=="5L" && !any(dataset() >3)) {
      message <- "EQ-5D-5L selected, but all dimension scores are 1, 2, or 3. Is this correct?"
      showNotification(message, type="error", duration=15)
    }

    return(res)
  })
  
  output$eq5d_text <- renderText({
    if(any(is.null(input$mo), is.null(input$sc), is.null(input$ua), is.null(input$pd), is.null(input$ad)))
      return()
    
    if(any(is.null(input$version), is.null(input$type), is.null(input$country)))
       return()
    
    if(!paste0("EQ-5D-", input$version) %in% valuesets(type=input$type)$Version)
      return()
    
    if(!input$country %in% valuesets(type=input$type)$Country)
      return()
    
    dimensions <- c(MO=input$mo, SC=input$sc, UA=input$ua, PD=input$pd, AD=input$ad)
    class(dimensions) <- "numeric"
    score <- eq5d(dimensions, version=input$version, type=input$type, country=input$country)
    paste0("The index for EQ-5D-", input$version, " ", input$country, " ", input$type, " value set is: ", score)
  })
  
  output$export<- renderUI({
    if(!is.null(input$data)) {
      downloadButton("download", 'Download Output File')
    }
  })
  
  output$download <- downloadHandler(
    filename = function() {
      paste(input$version, "_", input$country, "_", input$type, "_", format(Sys.time(), "%Y%m%d%H%M%S"), ".csv", sep = "")
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
    } else {
      res <- cbind(dataset(), eq5d)
    }
    colnames(res)[ncol(res)] <- "Index"
    return(res)
  }

  output$plot <- renderPlot({
    if(is.null(input$data) || is.null(input$plot_type) || is.null(input$group))
      return()

    if(input$plot_type=="density") {
      return(density_plot())
    } else if(input$plot_type=="ecdf") {
      return(ecdf_plot())
    } else {
      stop("Unable to identify plot type")
    }
  })

  density_plot <- reactive({
    if(is.null(input$data) || is.null(input$plot_type) || is.null(input$group))
      return()

    data <- getTableData()

    ave.meth <- get_average_method()
    
    if(input$group=="None" || !input$raw) {
      p <- ggplot(data, aes_string(x=input$plot_data)) + 
           geom_density(color="darkblue", fill="lightblue", alpha=0.4)

      if(input$average) {
        p <- p + geom_vline(aes_string(xintercept=ave.meth(data[[input$plot_data]])),
            color="darkblue", linetype="dashed")
      }
           
    } else {

      mu <- aggregate(data[[input$plot_data]], list(group=data[[input$group]]), ave.meth)

      p <- ggplot(data, aes_string(x=input$plot_data, fill=input$group)) + 
           geom_density(alpha=0.4)         

      if(input$average) {
        p <- p + geom_vline(data=mu, aes_string(xintercept="x", color="group"),
             linetype="dashed", show.legend=FALSE)
      }   
    }

    if(input$plot_data != "Index") {
      p <- p + scale_x_continuous(breaks= 1:sub("L", "", input$version), labels = 1:sub("L", "", input$version))
    }

    return(p)
    
  })

  ecdf_plot <- reactive({
    if(is.null(input$data) || is.null(input$plot_type) || is.null(input$group))
      return()

    data <- getTableData()
    
    ave.meth <- get_average_method()

    if(input$group=="None" || !input$raw) {

      p <- ggplot(data, aes_string(input$plot_data)) + stat_ecdf(geom = "step", colour="darkblue")

      if(input$average) {
        p <- p + geom_vline(aes_string(xintercept=ave.meth(data[[input$plot_data]])),
            color="darkblue", linetype="dashed")
      }
           
    } else {

      p <- ggplot(data, aes_string(input$plot_data, colour = input$group)) + stat_ecdf(geom = "step")
      mu <- aggregate(data[[input$plot_data]], list(group=data[[input$group]]), ave.meth)        

      if(input$average) {
        p <- p + geom_vline(data=mu, aes_string(xintercept="x", color="group"),
             linetype="dashed", show.legend=FALSE)
      }   
    }

    p <- p + labs(y='Cumulative probability')

    return(p)
    
  })

  output$choose_plot_data <- renderUI({
    selectInput("plot_data", "Plot data:",
        c("Index", "MO", "SC", "UA", "PD", "AD")
    )
  })

  output$choose_plot_type <- renderUI({
    selectInput("plot_type", "Plot type:",
        c("Density"="density", "ECDF"="ecdf")
    )
  })

  output$choose_group_by <- renderUI({
    if(is.null(input$data)) {
      return()
    }
    data <- getTableData()
    columns <- colnames(data)
    columns <- columns[!columns %in% c("MO", "SC", "UA", "PD", "AD", "Index")]

    groups <- "None"
    if(length(columns) > 0) {
      include <- apply(data[columns], 2, function(x) { length(unique(x))!=length(x)})
      groups <- c(groups, names(which(include)))
    } 
    selectInput("group", "Group by:",
        groups
    )
  })
  
  get_average_method <- reactive({
    if(input$average_method=="mean") {
      return(mean)
    } else {
      return(median)
    }
  })

})
