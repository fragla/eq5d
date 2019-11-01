library(eq5d)
library(DT)
library(mime)
library(readxl)
library(ggplot2)
library(ggiraph)
library(ggiraphExtra)
library(shinyWidgets)

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
    
    selectInput("country", "Country:", 
                choices=getReadableCountryNames(), selected=FALSE, selectize = FALSE)
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
    
    print(paste(input$version, type=input$type, country=input$country))
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
  
  output$export_table <- renderUI({
    if(!is.null(input$data)) {
      downloadButton("download_table", 'Download Output File')
    }
  })
  
  output$download_table <- downloadHandler(
    filename = function() {
      paste(input$version, "_", input$country, "_", input$type, "_", format(Sys.time(), "%Y%m%d%H%M%S"), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(getTableData(), file, row.names = FALSE)
    }
  )
  
  output$export_plot <- renderUI({
    if(!is.null(input$data)) {
      downloadButton("download_plot", 'Download plot')
    }
  })
  
  output$download_plot <- downloadHandler(
    filename = function() {
      paste(input$version, "_", input$country, "_", input$type, "_", input$plot_type, "_", format(Sys.time(), "%Y%m%d%H%M%S"), ".pdf", sep = "")
    },
    content = function(file) {
      ggsave(file, getPlot(),"pdf")
    }
  )
  
  rawdata <- reactive({
    if(input$data$type %in% c(mimemap["xls"], mimemap["xlsx"])) {
      dat <- read_excel(input$data$datapath, na=c("NA",""))
      dat <- as.data.frame(dat)
    }
    else {
      dat <- read.csv(file=input$data$datapath, header=TRUE, stringsAsFactors=FALSE, na.strings=c("NA",""))
    }   
  })
  
  dataset <- reactive({
    dat <- rawdata()
    
    idx <- getColumnIndex(dat)
    if(is.null(idx)) {
      stop("Unable to identify EQ-5D dimensions in the file header.")
    }
    
    if(length(idx)==1) {
      dat <- as.data.frame(do.call(rbind, strsplit(as.character(dat[[idx]]), "")))
      colnames(dat) <- c("MO", "SC", "UA", "PD", "AD")
      dat <- as.data.frame(apply(dat, 2, function(x) as.numeric(as.character(x))))
      
    } else {
      dat <- dat[idx]
    }
    
    if(!all(sapply(dat, function(x) is.numeric(x)))) {
      stop("Non-numeric values found in uploaded EQ-5D dimensions.")
    }
    return(dat)
  })
  
  getColumnIndex <- function(dat) {
    
    short <- getDimensionNames()
    five.digit <- "State"
    
    short.idx <- match(tolower(short), tolower(colnames(dat)))
    five.digit.idx <- match(tolower(five.digit), tolower(colnames(dat)))
    
    if(all(!is.na(short.idx))) {
      return(short.idx)
    } else if (!is.na(five.digit.idx)) {
      return(five.digit.idx)
    } else {
      return(NULL)
    }
  }
  
  getTableData <- reactive({
    eq5d <- eq5d(dataset(), version=input$version, type=input$type, country=input$country)
    if(input$raw) {
      if(all(getDimensionNames() %in% colnames(rawdata()))) {
        res <- cbind(rawdata(), eq5d)
      } else {
        res <- cbind(rawdata(), dataset(), eq5d)
      }
    } else {
      res <- cbind(dataset(), eq5d)
    }
    colnames(res)[ncol(res)] <- "Index"
    return(res)
  })
  
  getTableDataByGroup <- reactive({
    data <- getTableData()
    
    if(input$group != "None") {
      data <- data[which(data[,input$group] %in% input$group_member),]
    }
    
    return(data)
  })

  output$plot <- renderggiraph({
    if(is.null(input$data) || is.null(input$plot_type) || is.null(input$group))
      return()
    
    if(input$group!="None" && is.null(input$group_member))
      return()

    code <- getPlot()

    output <- ggiraph(code = print(code), selection_type = "single")

    return(output)
  })
  
  getPlot <- reactive({
    if(is.null(input$group)) {
      return()
    }

    if(input$plot_type=="density") {
      print("Density")
      code <- density_plot()
    } else if(input$plot_type=="ecdf") {
      print("ECDF")
      code <- ecdf_plot()
    } else if(input$plot_type=="radar") {
      print("Radar")
      code <- radar_plot()
    }  else {
      stop("Unable to identify plot type")
    }
    return(code)
  })

  density_plot <- reactive({
    if(is.null(input$data) || is.null(input$plot_type) || is.null(input$group))
      return()

    data <- getTableDataByGroup()
    
    if(nrow(data) > 0) {
      ave.meth <- get_average_method()
      
      if(input$group=="None" || !input$raw) {
        p <- ggplot(data, aes_string(x=input$plot_data)) + 
             geom_density(color="darkblue", fill="lightblue", alpha=0.4)
  
        if(input$average) {
          p <- p + geom_vline_interactive(aes_string(xintercept=ave.meth(data[[input$plot_data]])),
              color="darkblue", linetype="dashed", tooltip = paste0(input$average_method, ": ", get_average_value()), data_id = "density_mean")
        }
             
      } else {
        colours <- getGroupColours()
        mu <- get_average_value()
        p <- ggplot(data, aes_string(x=input$plot_data, fill=input$group)) + 
             geom_density(alpha=0.4) + scale_fill_manual(values=colours) + scale_color_manual(values=colours)  
  
        if(input$average) {
          p <- p + geom_vline_interactive(data=mu, aes_string(xintercept="x", color="group"),
               linetype="dashed", show.legend=FALSE, tooltip = paste0(input$average_method, ": ", mu$x), data_id = paste0("density_", input$average_method, "_", mu$group))
        }   
      }
  
      if(input$plot_data != "Index") {
        p <- p + scale_x_continuous(breaks= 1:sub("L", "", input$version), labels = 1:sub("L", "", input$version))
      }
  
      return(p)
    }
  })

  ecdf_plot <- reactive({
    if(is.null(input$data) || is.null(input$plot_type) || is.null(input$group))
      return()

    data <- getTableDataByGroup()
    
    ave.meth <- get_average_method()

    if(input$group=="None" || !input$raw) {

      p <- ggplot(data, aes_string(input$plot_data)) + stat_ecdf(geom = "step", colour="darkblue")

      if(input$average) {
        p <- p + geom_vline_interactive(aes_string(xintercept=ave.meth(data[[input$plot_data]])),
            color="darkblue", linetype="dashed", tooltip = paste0(input$average_method, ": ", get_average_value()), data_id = "ecdf_mean")
      }
           
    } else {
      colours <- getGroupColours()
      p <- ggplot(data, aes_string(input$plot_data, colour = input$group)) + 
        stat_ecdf(geom = "step") + scale_color_manual(values=colours)
      mu <- get_average_value()        

      if(input$average) {
        p <- p + geom_vline_interactive(data=mu, aes_string(xintercept="x", color="group"),
             linetype="dashed", show.legend=FALSE, tooltip = paste0(input$average_method, ": ", mu$x), data_id = paste0("ecdf_", input$average_method, "_", mu$group))
      }   
    }

    p <- p + labs(y='Cumulative probability')

    return(p)
    
  })
  
  radar_plot <- reactive({
    if(is.null(input$data) || is.null(input$plot_type) || is.null(input$group))
      return()
    
    data <- getTableDataByGroup()
    
    if(input$group=="None" || !input$raw) {
    
      data <- data[,names(data) %in% getDimensionNames()]
      p <- ggRadar(data=data, rescale=FALSE, colour = "#F8766D", alpha = 0.4)
    } else {
      colours <- getGroupColours()
      data <- data[,names(data) %in% c("MO", "SC", "UA", "PD", "AD", input$group)]
      p <- ggRadar(data=data,aes_string(color=input$group), rescale=FALSE) + 
        scale_fill_manual(values=colours) + scale_color_manual(values=colours) +
        theme(legend.position="right")
    }
    return(p)
  })

  output$choose_plot_data <- renderUI({
    selectInput("plot_data", "Plot data:",
        c("Index", "MO", "SC", "UA", "PD", "AD")
    )
  })

  output$choose_plot_type <- renderUI({
    selectInput("plot_type", "Plot type:",
        c("Density"="density", "ECDF"="ecdf", "Radar"="radar")
    )
  })

  output$choose_group_by <- renderUI({
    if(is.null(input$data)) {
      return()
    }
    data <- getTableData()
    data <- data[!colnames(data) %in% getDimensionNames()]
    data <- data[sapply(data, function(x) is.character(x) || is.logical(x) || is.factor(x))]
    
    groups <- "None"
    if(ncol(data) > 0) {
      include <- apply(data, 2, function(x) {length(unique(x))!=length(x)})
      groups <- c(groups, names(which(include)))
    } 
    selectInput("group", "Group by:",
        groups
    )
  })
  
  output$choose_group_members <- renderUI({

    if(is.null(input$group) || input$group=="None") {
      return()
    }
    data <- getTableData()

    pickerInput(
      inputId = "group_member",
      label = "Select/deselect group members",
      choices = unique(data[[input$group]]),
      selected =unique(data[[input$group]]),
      options = list(
        `actions-box` = FALSE,
        `none-selected-text` = "Please select at least one."),
      multiple = TRUE
    )
  })
  
  get_average_method <- reactive({
    if(input$average_method=="mean") {
      return(mean)
    } else {
      return(median)
    }
  })

  get_average_value <- reactive({

    data <- getTableDataByGroup()
    
    if(nrow(data)==0)
      return()
    
    ave.meth <- get_average_method()
    
    if(input$group == "None") {
      mu <- aggregate(as.formula(paste(input$plot_data, "~ 1")), data, function(x){round(ave.meth(x),3)})
    } else {
      mu <- aggregate(data[[input$plot_data]], list(group=data[[input$group]]), function(x){round(ave.meth(x),3)})
    }
    return(mu)
  })
  
  ggplotColours <- function(n = 6, h = c(0, 360) + 15){
    if ((diff(h) %% 360) < 1) h[2] <- h[2] - 360/n
    hcl(h = (seq(h[1], h[2], length = n)), c = 100, l = 65)
  }
  
  getGroupColours <- reactive({
    data <- getTableData()
    groups <- unique(data[[input$group]])
    colours <- ggplotColours(length(groups))
    names(colours) <- groups
    return(colours)
  })

  gg_color_hue <- function(n) {
    hues = seq(15, 375, length = n + 1)
    hcl(h = hues, l = 65, c = 100)[1:n]
  }
  
  getDimensionNames <- reactive({
    return(c("MO", "SC", "UA", "PD", "AD"))
  })
  
  getReadableCountryNames <- reactive({
    countries <- sort(unique(as.character(valuesets(version=input$version)$Country)))
    countries.list <- as.list(countries)
    countries <- gsub("_", " ", countries)
    countries <- unlist(lapply(strsplit(gsub("([[:lower:]])([[:upper:]])", "\\1 \\2", countries), " "), function(x){paste(x, collapse=" ")}))
    names(countries.list) <- countries
    return(countries.list)
  })
})
