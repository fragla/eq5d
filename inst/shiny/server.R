library(eq5d)
library(DT)
library(mime)
library(readxl)
library(ggplot2)
library(ggiraph)
library(ggiraphExtra)
library(reshape2)
library(shinyWidgets)
library(FSA)
library(PMCMRplus)
library(tools)

options(shiny.sanitize.errors = FALSE)

addResourcePath('example-data', system.file("extdata", package="eq5d"))

shinyServer(function(input, output, session) {
  
  # Drop-down selection box for which data set
  output$choose_dimensions <- renderUI({
    if(is.null(input$version))
      return()
    
    if(input$version %in% c("3L", "Y")) {
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
                 c("EQ-5D-3L"="3L", "EQ-5D-5L"="5L", "EQ-5D-Y"="Y"),
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
        
        if(input$country %in% colnames(RCW))
          type <-c(type, "RCW")
        
    } else if(input$version=="Y") {
      type <-c(type, "cTTO")
    }
    else {
      
        if(input$country %in% colnames(VT))
          type <-c(type, "VT")
        
        if(input$country %in% colnames(CW))
          type <-c(type, "CW")
        
    }
    
    selectInput("type", "Type:", 
                choices=type, selected=FALSE, selectize = FALSE)
    
  })
  
  output$include_severity_scores <- renderUI({
    calculations <- c("Level Sum Score" = "lss",
                      "Level Frequency Score" = "lfs")
    
    checkboxGroupInput("severity_scores", "Include severity scores:", calculations)
    
  })
  
  output$include_raw_data <- renderUI({
    checkboxInput("raw", "Include all submitted data in table", TRUE)
  })
  
  output$include_raw_data <- renderUI({
    checkboxInput("raw", "Include all submitted data in table", TRUE)
  })

  getPaired <- reactive({
    data <- getTableDataByGroup()
    group.totals <- table(data[input$group])
    
    data <- data[!colnames(data) %in% c(getDimensionNames(), "Index", input$group)]
    if(ncol(data) > 0) {
      id.columns <- lapply(data, function(x){
        id.totals <- table(x)
        length(unique(id.totals))==1 & length(unique(group.totals))==1 & sum(id.totals)==sum(group.totals)
      })
      return(names(id.columns)[which(unlist(id.columns))])
    }
  })
  
  output$show_paired <- renderUI({
    if(!is.null(input$group) && input$group != "None") { 
      if(length(getPaired())==0)
        return()
      
      data <- getTableData()
      data <- data[!colnames(data) %in% c(getDimensionNames(), "Index", input$group)]
      id.groups <- getPaired()
      if(!is.null(id.groups)) {
        tagList(
          checkboxInput("paired", "Data are paired", TRUE),
          selectInput("id", "ID column:", 
                      choices=id.groups, selected=FALSE, selectize = FALSE)
        )
      }
    }
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
  
  output$choose_summary_type <- renderUI({
    radioButtons("summary_type", "Summary type:",
                 c("Counts"="counts", "Percentage"="percentage"),
                 selected="percentage",
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

    if(input$version=="5L" && !any(dataset() >3, na.rm=TRUE)) {
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
  
  readdata <- reactive({
    if(is.null(input$data))
      return()
    
    resetColumns(vals)

    if(input$data$type %in% c(mimemap["xls"], mimemap["xlsx"])) {
      dat <- read_excel(input$data$datapath, na=c("NA",""))
      dat <- as.data.frame(dat)
    }
    else {
      dat <- read.csv(file=input$data$datapath, header=TRUE, stringsAsFactors=FALSE, na.strings=c("NA",""))
    }
  })
  
  resetColumns <- function(vals) {
    vals$MO <- vals$SC <- vals$UA <- vals$PD <- vals$AD  <- vals$State <- NULL
  }
  
  rawdata <- reactive({
    dat <- readdata()
    
    if(is.null(getColumnIndex(dat))) {
      showModal(columnModal(dat))
      return()
    } 
    
    return(dat)
  })
  
  columnModal <- function(dat, failed = FALSE) {
    select.options <- c("None", colnames(dat))
    modalDialog(
      div(h2(tags$b("Select dimension columns:"))),
      splitLayout(
        
        selectInput("mo_col", "MO:", 
                    choices=select.options, selected=FALSE, selectize = FALSE),
        selectInput("sc_col", "SC:", 
                    choices=select.options, selected=FALSE, selectize = FALSE),
        selectInput("ua_col", "UA:",
                    choices=select.options, selected=FALSE, selectize = FALSE, width="100px"),
        selectInput("pd_col", "PD:",
                    choices=select.options, selected=FALSE, selectize = FALSE, width="100px"),
        selectInput("ad_col", "AD:",
                    choices=select.options, selected=FALSE, selectize = FALSE, width="100px")
      ),
      div(p(tags$b("or:"))),
      selectInput("state_col", "Five digit:",
                  choices=select.options, selected=FALSE, selectize = FALSE, width="100px"),

      if (failed)
        div(tags$b("Invalid column names.", style = "color: red;")),
      
      footer = tagList(
        modalButton("Cancel"),
        actionButton("ok", "OK")
      )
    )
  }
  
  modalDimensions <- reactive({c(input$mo_col, input$sc_col, input$ua_col, input$pd_col, input$ad_col)})
  
  observeEvent(modalDimensions(), {
    dat <- readdata()
    options <- colnames(dat)
    updateSelectInput(session, "mo_col", 
                      choices = c("None", options[!options %in% c(input$sc_col,input$ua_col,input$pd_col,input$ad_col)]),
                      selected = input$mo_col)
    updateSelectInput(session, "sc_col", 
                      choices = c("None", options[!options %in% c(input$mo_col,input$ua_col,input$pd_col,input$ad_col)]),
                      selected = input$sc_col)
    updateSelectInput(session, "ua_col", 
                      choices = c("None", options[!options %in% c(input$mo_col,input$sc_col,input$pd_col,input$ad_col)]),
                      selected = input$ua_col)
    updateSelectInput(session, "pd_col", 
                      choices = c("None", options[!options %in% c(input$mo_col,input$sc_col,input$ua_col,input$ad_col)]),
                      selected = input$pd_col)
    updateSelectInput(session, "ad_col", 
                      choices = c("None", options[!options %in% c(input$mo_col,input$sc_col,input$ua_col,input$pd_col)]),
                      selected = input$ad_col)
  })
  
  vals <- reactiveValues(MO=NULL, SC=NULL, UA=NULL, PD=NULL, AD=NULL, State=NULL)
  
  getMaxLevels <- reactive({
    if(input$version %in% c("3L", "Y")) {
      return(3)
    } else if(input$version=="5L") {
      return(5)
    } else {
      stop("Invalid EQ-5D version.")
    }
  })
  
  dimensionsValid <- reactive({
    dat <- readdata()
    cols <- modalDimensions()
    is.valid <- all(sapply(cols, function(x){any(dat[,x] %in% 1:getMaxLevels())}))
    return(is.valid)
  })
  
  stateValid <- reactive({
    dat <- readdata()
    is.valid <- any(dat[,input$state_col] %in% getHealthStates(input$version))
    return(is.valid)
  })
  
  observeEvent(input$ok, {
    if(all(modalDimensions()!="None") && dimensionsValid()) {
      vals$MO <- input$mo_col
      vals$SC <- input$sc_col
      vals$UA <- input$ua_col
      vals$PD <- input$pd_col
      vals$AD <- input$ad_col
      removeModal()
    } else if(input$state_col!="None" && stateValid()) {
      vals$State <- input$state_col
      removeModal()
    } else {
      showModal(columnModal(readdata(), failed = TRUE))
    }
  })
  
  dataset <- reactive({
    dat <- rawdata()
    if(is.null(dat))
      return()
    
    idx <- getColumnIndex(dat)
    if(is.null(idx)) {
      print(head(dat))
      return()
      stop("Unable to identify EQ-5D dimensions in the file header.")
    }
    
    if(length(idx)==1) {
      length.check <- sapply(dat[[idx]], nchar)
      if(any(is.na(length.check)|length.check!=5)) {
        if(ignoreInvalid()) {
          dat[which(length.check!=5),idx] <- NA
        } else {
          stop("States identified without five digits.")
        }
      }
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
    five.digit <- getStateName()
    
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
    eq5d <- eq5d(dataset(), version=input$version, type=input$type, country=input$country, ignore.invalid=ignoreInvalid(), dimensions=getDimensionNames())
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
    
    if("lss" %in% input$severity_scores) {
      res$LSS <- lss(dataset(), version=input$version, ignore.invalid=ignoreInvalid(), dimensions=getDimensionNames())
    }
    
    if("lfs" %in% input$severity_scores) {
      res$LFS <- lfs(dataset(), version=input$version, ignore.invalid=ignoreInvalid(), dimensions=getDimensionNames())
    }
    
    return(res)
  })
  
  getTableDataByGroup <- reactive({
    data <- getTableData()
    data <- data[!is.na(data[[input$plot_data]]),]
    
    if(!is.null(input$group) && input$group != "None") {
      data <- data[which(data[,input$group] %in% input$group_member),]
    }
    
    return(data)
  })
  
  ignoreInvalid <- reactive({
    ignore.invalid <- ifelse(is.null(input$ignore_invalid), TRUE, input$ignore_invalid)
    return(ignore.invalid)
  })

  output$plot <- renderggiraph({
    if(is.null(input$data) || is.null(input$plot_type) || is.null(input$group))
      return()
    
    if(input$plot_type=="summary" && is.null(input$summary_type))
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
    } else if(input$plot_type=="summary") {
      code <- bar_plot()
    } else {
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
  
      # if(input$plot_data == "LFS") {
      #   p <- p + scale_x_continuous(labels = function(x) formatC(x, width = sub("L", "", input$version), format = "d", flag = "0"))
      # }
      
      if(input$plot_data %in% c("MO", "SC", "UA", "PD", "AD")) {
        p <- p + scale_x_continuous(breaks= 1:getMaxLevels(), labels = 1:getMaxLevels())
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
  
  bar_plot <- reactive({
    if(is.null(input$data) || is.null(input$plot_type) || is.null(input$group) || is.null(input$summary_type))
      return()
    
    data <- getTableDataByGroup()
    
    counts <- ifelse(input$summary_type == "counts", TRUE, FALSE)
    summary_type <- toTitleCase(input$summary_type)
    
    if(input$group=="None" || !input$raw) {
      data <- eq5dds(data, version=input$version, counts=counts, dimensions=getDimensionNames())
      
    } else {
      data <- eq5dds(data, version=input$version, counts=counts, by=input$group, dimensions=getDimensionNames())
    }
    
    if(input$group=="None") {
      data <- melt(as.matrix(data))
      colnames(data) <- c("Score", "Dimension", summary_type)
      data$Score <- as.factor(data$Score)
      
      p <- ggplot(data, aes_string(fill="Score", y=summary_type, x="Dimension", tooltip=summary_type)) + 
        geom_bar_interactive(position="dodge", stat="identity")
    } else {
      data <- lapply(data, function(x){melt(as.matrix(x))})
      data <- do.call(rbind, unname(Map(cbind, Group = names(data), data)))
      colnames(data) <- c(input$group, "Score", "Dimension", summary_type)
      data$Score <- as.factor(data$Score)
      p <- ggplot(data, aes_string(fill="Score", y=summary_type, x="Dimension", tooltip=summary_type)) + 
        geom_bar_interactive(position="dodge", stat="identity") + facet_wrap(as.formula(paste("~", input$group)))
    }
    
    return(p)
  })  
  
  output$choose_plot_data <- renderUI({
    options <- "Index"
    
    if("lss" %in% input$severity_scores) {
      options <- c(options, "LSS")
    }
    
    # if("lfs" %in% input$severity_scores) {
    #   options <- c(options, "LFS")
    # }
    
    selectInput("plot_data", "Plot data:",
        c(options, "MO", "SC", "UA", "PD", "AD")
    )
  })

  output$choose_plot_type <- renderUI({
    selectInput("plot_type", "Plot type:",
        c("Summary"="summary", "Density"="density", "ECDF"="ecdf", "Radar"="radar")
    )
  })

  output$choose_group_by <- renderUI({
    if(is.null(input$data)) {
      return()
    }
    data <- getTableData()
    data <- data[!tolower(colnames(data)) %in% tolower(c(getDimensionNames(), getStateName(), "LFS"))]
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
  
  getSummary <- reactive({
    if(is.null(input$data) || is.null(input$plot_type) || is.null(input$summary_type)) {
      return()
    }
    counts <- ifelse(input$summary_type == "counts", TRUE, FALSE)

    data <- getTableDataByGroup()
    if(input$group=="None" || !input$raw) {
     data <- eq5dds(data, version=input$version, counts=counts, dimensions=getDimensionNames())
    } else {
      data <- eq5dds(data, version=input$version, counts=counts, by=input$group, dimensions=getDimensionNames())
    }
    return(data)
  })
  
  getStatistics <- reactive({
    if(is.null(input$group) || input$group=="None") {
      return()
    }
    
    data <- getTableDataByGroup()
    stats <- NULL
    if(length(input$group_member)==2) {
      stats <- getWilcoxStats()
    } else if (length(input$group_member) > 2) {
      if(!is.null(getPaired()) & length(getPaired()) > 0 & !is.null(input$paired) && input$paired) {
        print("Friedman")
        stats <- getFriedmanStats()
      } else {
        print("Kruskal")
        stats <- getKruskalStats()
      }
    }
    return(stats)
  })
  
  output$statistics <- renderUI({
    if(is.null(input$data) || is.null(input$plot_type))
      return()

    if(input$plot_type %in% c("radar", "summary")) {
      summ <- getSummary()
      if(input$group=="None" || !input$raw) {
        taglist <- tagList(
          h5(paste("Descriptive system by", input$summary_type)),
          renderDT(summ,options = list(searching = FALSE, paging = FALSE, info = FALSE))
        )
      } else {
        if(length(summ)==0)
          return()
        
        taglist <- tagList(
          h5(paste("Descriptive system by", input$summary_type)),
          radioButtons("eq5dds", "Group table:", names(summ), selected=ifelse(is.null(input$eq5dds),names(summ)[1],input$eq5dds), inline=T),
          renderDT(summ[[input$eq5dds]],options = list(searching = FALSE, paging = FALSE, info = FALSE))
        )
      }
    } else {
      stats <- getStatistics()
      if(is.null(stats))
        return("Select a group to perform statistical tests.")
      
      taglist <- tagList(
        h4("Statistical analysis"),
        p(strong(stats$method)),
        p(strong(paste("Data:")), paste(stats$data.name)),
        p(strong(paste0(names(stats$statistic))), paste0(" = ", round(stats$statistic,1)),
        strong(paste0("p value = ")), paste0(round(stats$p.value,5)))
      )
      if(length(input$group_member) > 2 & stats$p.value < 0.05) {
        taglist[[length(taglist)+1]] <- actionButton("posthoc","View post hoc tests")
      }
    }
    return(taglist)
  })
  
  output$posthocTable <- renderDataTable({
    stats <- getStatistics()
    table <- data.frame(lapply(stats$posthoc$res, function(y) if(is.numeric(y)) round(y, 5) else y))
    datatable(table)
  })
  
  observeEvent(input$posthoc,{
    stats <- getStatistics()
    showModal(
      modalDialog(
        h2("Post hoc tests"),
        p(stats$posthoc$method),
        DT::dataTableOutput('posthocTable'),
        uiOutput("export_posthoc"),
        size = "m"
      )
    )
  })
  
  getWilcoxStats <- reactive({
    data <- getTableDataByGroup()
    paired <- FALSE
    if(!is.null(getPaired()) & length(getPaired()) > 0 & !is.null(input$paired) && input$paired) {
      data <- data[order(data[input$id], data[input$group]),]
      paired <- TRUE
    }
    res <- wilcox.test(as.formula(paste(input$plot_data," ~ ", input$group)), data, paired=paired)
    return(res)
  })
  
  getKruskalStats <- reactive({
    data <- getTableDataByGroup()
    res <- kruskal.test(as.formula(paste(input$plot_data," ~ ", input$group)), data)
    
    if(res$p.value < 0.05) {
      res$posthoc <- dunnTest(as.formula(paste(input$plot_data," ~ ", input$group)), data)
      res$posthoc$method <- paste("Dunn's test with", res$posthoc$method, "correction")
    }
    return(res)
  })
  
  getFriedmanStats <- reactive({
    data <- getTableDataByGroup()
    res <- friedman.test(as.formula(paste(input$plot_data, " ~ ", input$group, " | ", input$id)),
                  data = data)
    if(res$p.value < 0.05) {
      nt <- frdAllPairsNemenyiTest(as.formula(paste(input$plot_data, " ~ ", input$group, " | ", input$id)), data)
      nt.stats <- na.omit(as.data.frame(as.table(nt$statistic)))
      nt.p.value <- na.omit(as.data.frame(as.table(nt$p.value)))
      nt$res <- data.frame(Comparison=paste(nt.stats$Var2, "-", nt.stats$Var1), "Mean rank diff"=nt.stats$Freq, P.adj=nt.p.value$Freq)
      nt$method <- paste("Nemenyi test with", nt$p.adjust.method, "correction")
      res$posthoc <- nt
    }
    return(res)
  })
  
  output$export_posthoc <- renderUI({
      downloadButton("download_posthoc", 'Download Post Hoc Data')
  })
  
  output$download_posthoc <- downloadHandler(
    filename = function() {
      paste(input$version, "_", input$country, "_", input$type, "_post_hoc_", format(Sys.time(), "%Y%m%d%H%M%S"), ".csv", sep = "")
    },
    content = function(file) {
      stats <- getStatistics()
      write.csv(stats$posthoc$res, file, row.names = FALSE)
    }
  )
  
  output$ignore_invalid <- renderUI({
    checkboxInput("ignore_invalid", "Ignore data with invalid/incomplete/missing dimension scores", TRUE)
  })
  
  output$stats_tests <- renderTable({

      data.frame(Groups=c(2, 2, ">2", ">2"),
                 Paired=c("No", "Yes", "No", "Yes"),
                 Test=c("Wilcoxon rank sum test", "Wilcoxon signed rank test",
                        "Kruskal-Wallis rank sum test with Dunn's test for post hoc testing.", "Friedman's rank sum test with the Nemenyi test for post hoc testing."))

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
    dimensions <- c(vals$MO, vals$SC, vals$UA, vals$PD, vals$AD)
    if(all(!is.null(dimensions))) {
      return(c(vals$MO, vals$SC, vals$UA, vals$PD, vals$AD))
    } else {
      return(c("MO", "SC", "UA", "PD", "AD"))
    }
  })
  
  getStateName <- reactive({
    if(!is.null(vals$State)) {
      return(vals$State)
    } else {
      return("State")
    }
  })
  
  getReadableCountryNames <- reactive({
    countries <- sort(unique(as.character(valuesets(version=input$version)$Country)))
    countries.list <- as.list(countries)
    countries <- gsub("_", " ", countries)
    countries <- unlist(lapply(strsplit(gsub("([[:lower:]][[:lower:]])([[:upper:]])", "\\1 \\2", countries), " "), function(x){paste(x, collapse=" ")}))
    names(countries.list) <- countries
    return(countries.list)
  })
})
