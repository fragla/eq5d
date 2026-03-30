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
library(rlang)

options(shiny.sanitize.errors = FALSE)

addResourcePath('example-data', system.file("extdata", package="eq5d"))

shinyServer(function(input, output, session) {

  output$choose_dimensions <- renderUI({
    if (is.null(input$version)) { return() }
    levels <- if (input$version %in% c("3L", "Y3L")) { 1:3 } else { 1:5 }
    splitLayout(
      selectInput("mo", "Mobility:", choices = levels, selected = FALSE, selectize = FALSE, width = "100px"),
      selectInput("sc", "Self care:", choices = levels, selected = FALSE, selectize = FALSE, width = "100px"),
      selectInput("ua", "Usual activities:", choices = levels, selected = FALSE, selectize = FALSE, width = "100px"),
      selectInput("pd", "Pain/discomfort:", choices = levels, selected = FALSE, selectize = FALSE, width = "100px"),
      selectInput("ad", "Anxiety/depression:", choices = levels, selected = FALSE, selectize = FALSE, width = "100px")
    )
  })

  output$choose_dataset <- renderUI({
    fileInput("data", "Choose data file", accept = c(mimemap["csv"], mimemap["xls"], mimemap["xlsx"]))
  })

  output$choose_calc_type <- renderUI({
    radioButtons("multi", "Multiple calculations:", c("Single" = "single", "Multiple" = "multiple"), selected = "multiple", inline = TRUE)
  })

  output$choose_version <- renderUI({
    radioButtons("version", "EQ-5D version:", c("EQ-5D-3L" = "3L", "EQ-5D-5L" = "5L", "EQ-5D-Y-3L" = "Y3L"), selected = "3L", inline = TRUE)
  })

  output$choose_country <- renderUI({
    req(input$version)
    selectInput("country", "Country:", choices = getReadableCountryNames(), selected = FALSE, selectize = FALSE)
  })

  output$choose_type <- renderUI({
    req(input$country, input$version)
    vs <- valuesets(version = input$version)
    vs <- subset(vs, Country == input$country)
    possible_types  <- c("TTO", "VAS", "RCW", "DSU", "IVP", "VT", "CW")
    types_available <- intersect(possible_types, unique(vs$Type))
    validate(need(length(types_available) > 0, "No value set types available for this selection."))
    
    # Determine preferred default based on version
    preferred <- if (input$version == "3L") {
      "TTO"
    } else if (input$version == "5L") {
      "VT"
    } else {
      NULL
    }
    
    # Only use it if it's actually available
    selected_val <- if(!is.null(preferred) && preferred %in% types_available) {
      preferred
    } else {
      types_available[1]
    }
    
    selectInput("type", "Type:", choices = types_available, selected = selected_val, selectize = FALSE)
  })

  output$include_severity_scores <- renderUI({
    calculations <- c("Level Sum Score" = "lss", "Level Frequency Score" = "lfs")
    checkboxGroupInput("severity_scores", "Include severity scores:", calculations)
  })

  output$choose_utility <- renderUI({ textInput("utility", label = "Utility:", width = "100px") })
  output$choose_age     <- renderUI({ selectInput("age", "Age:", choices = c("18-34" = 1, "35-44" = 2, "45-54" = 3, "55-64" = 4, "65-100" = 5), selected = FALSE, selectize = FALSE, width = "100px") })
  output$choose_sex     <- renderUI({ selectInput("sex", "Sex:", choices = c("Male" = "male", "Female" = "female"), selected = FALSE, selectize = FALSE, width = "100px") })
  output$choose_bwidth  <- renderUI({ textInput("bwidth", label = "Bandwidth:", value = 0, width = "100px") })
  output$include_raw_data <- renderUI({ checkboxInput("raw", "Include all submitted data in table", TRUE) })

  readdata <- reactive({
    if (is.null(input$data)) { return() }
    resetColumns(vals)
    if (input$data$type %in% c(mimemap["xls"], mimemap["xlsx"])) {
      dat <- read_excel(input$data$datapath, na = c("NA", ""))
      dat <- as.data.frame(dat)
    } else {
      dat <- read.csv(file = input$data$datapath, header = TRUE, stringsAsFactors = FALSE, na.strings = c("NA", ""))
    }
    dat
  })

  resetColumns <- function(vals) {
    vals$MO <- vals$SC <- vals$UA <- vals$PD <- vals$AD <- vals$State <- vals$Utility <- vals$Age <- vals$Sex <- NULL
  }

  rawdata <- reactive({
    dat <- readdata()
    if (is.null(dat)) { return() }
    if (is.null(getColumnIndex(dat))) {
      showModal(columnModal(dat))
      return()
    }
    dat
  })

  columnModal <- function(dat, failed = FALSE) {
    select.options <- c("None", colnames(dat))
    modalDialog(
      div(h2(tags$b("Select dimension columns:"))),
      splitLayout(
        selectInput("mo_col", "MO:", choices = select.options, selected = FALSE, selectize = FALSE),
        selectInput("sc_col", "SC:", choices = select.options, selected = FALSE, selectize = FALSE),
        selectInput("ua_col", "UA:", choices = select.options, selected = FALSE, selectize = FALSE, width = "100px"),
        selectInput("pd_col", "PD:", choices = select.options, selected = FALSE, selectize = FALSE, width = "100px"),
        selectInput("ad_col", "AD:", choices = select.options, selected = FALSE, selectize = FALSE, width = "100px")
      ),
      div(p(tags$b("or:"))),
      selectInput("state_col", "Five digit:", choices = select.options, selected = FALSE, selectize = FALSE, width = "100px"),
      div(p(tags$b("or:"))),
      selectInput("utility_col", "Utility:", choices = select.options, selected = FALSE, selectize = FALSE, width = "100px"),
      if (failed) { div(tags$b("Invalid column names. Columns may contain non-valid data.", style = "color: red;")) },
      footer = tagList(modalButton("Cancel"), actionButton("ok", "OK"))
    )
  }

  modalDimensions <- reactive({
    c(input$mo_col, input$sc_col, input$ua_col, input$pd_col, input$ad_col)
  })

  observeEvent(modalDimensions(), {
    dat <- readdata()
    if (is.null(dat)) { return() }
    options <- colnames(dat)
    updateSelectInput(session, "mo_col", choices = c("None", options[!options %in% c(input$sc_col, input$ua_col, input$pd_col, input$ad_col)]), selected = input$mo_col)
    updateSelectInput(session, "sc_col", choices = c("None", options[!options %in% c(input$mo_col, input$ua_col, input$pd_col, input$ad_col)]), selected = input$sc_col)
    updateSelectInput(session, "ua_col", choices = c("None", options[!options %in% c(input$mo_col, input$sc_col, input$pd_col, input$ad_col)]), selected = input$ua_col)
    updateSelectInput(session, "pd_col", choices = c("None", options[!options %in% c(input$mo_col, input$sc_col, input$ua_col, input$ad_col)]), selected = input$pd_col)
    updateSelectInput(session, "ad_col", choices = c("None", options[!options %in% c(input$mo_col, input$sc_col, input$ua_col, input$pd_col)]), selected = input$ad_col)
  })

  vals <- reactiveValues(MO = NULL, SC = NULL, UA = NULL, PD = NULL, AD = NULL, State = NULL, Utility = NULL, Age = NULL, Sex = NULL, bwidth = NULL)

  getMaxLevels <- reactive({
    if (input$version %in% c("3L", "Y3L")) { 3 } else if (input$version == "5L") { 5 } else { stop("Invalid EQ-5D version.") }
  })

  dimensionsValid <- reactive({
    dat <- readdata()
    if (is.null(dat)) { return(FALSE) }
    cols <- modalDimensions()
    all(sapply(cols, function(x) {
      any(dat[, x] %in% 1:getMaxLevels())
    }))
  })

  stateValid <- reactive({
    dat <- readdata()
    if (is.null(dat)) { return(FALSE) }
    any(dat[, input$state_col] %in% get_all_health_states(input$version))
  })

  utilityValid <- reactive({
    dat <- readdata()
    if (is.null(dat)) { return(FALSE) }
    any(!check.integer(dat[, input$utility_col]))
  })

  observeEvent(input$ok, {
    if (all(modalDimensions() != "None") && isTRUE(dimensionsValid())) {
      vals$MO <- input$mo_col; vals$SC <- input$sc_col; vals$UA <- input$ua_col; vals$PD <- input$pd_col; vals$AD <- input$ad_col
      removeModal()
    } else if (input$state_col != "None" && isTRUE(stateValid())) {
      vals$State <- input$state_col
      removeModal()
    } else if (input$utility_col != "None" && isTRUE(utilityValid())) {
      vals$Utility <- input$utility_col
      removeModal()
    } else {
      showModal(columnModal(readdata(), failed = TRUE))
    }
  })

  dataset <- reactive({
    dat <- rawdata()
    if (is.null(dat)) { return() }
    idx <- getColumnIndex(dat)
    if (is.null(idx)) { return() }

    dsu <- NULL
    if (input$type == "DSU" && !is.null(getDSUIndex(dat))) { dsu <- dat[, getDSUIndex(dat), drop = FALSE] }
    if (input$type == "DSU" && (!getAgeName() %in% colnames(dsu) || !getSexName() %in% colnames(dsu))) { stop("NICE DSU value set selected, but no Age or Sex columns found.") }

    if (length(idx) == 1 && all(dat[[idx]] == round(dat[[idx]]))) {
      length.check <- sapply(dat[[idx]], nchar)
      if (any(is.na(length.check) | length.check != 5)) {
        if (ignoreInvalid()) { dat[which(length.check != 5), idx] <- NA } else { stop("States identified without five digits.") }
      }
      dat <- as.data.frame(do.call(rbind, strsplit(as.character(dat[[idx]]), "")))
      colnames(dat) <- c("MO", "SC", "UA", "PD", "AD")
      dat <- as.data.frame(lapply(dat, function(x) as.numeric(as.character(x))))
    } else if (length(idx) == 1 && any(!check.integer(dat[[idx]]))) {
      dat <- dat[idx]; colnames(dat) <- "Utility"
    } else {
      dat <- dat[idx]
    }

    if (!all(sapply(dat, is.numeric))) { stop("Non-numeric values found in uploaded EQ-5D dimensions.") }
    if (input$type == "DSU" && !is.null(dsu)) { dat <- cbind(dat, dsu) }
    dat
  })

  getColumnIndex <- function(dat) {
    short      <- get_dimension_names()
    five.digit <- getStateName()
    utility    <- getUtilityName()
    short.idx      <- match(tolower(short), tolower(colnames(dat)))
    five.digit.idx <- match(tolower(five.digit), tolower(colnames(dat)))
    utility.idx    <- match(tolower(utility), tolower(colnames(dat)))
    if (all(!is.na(short.idx))) { return(short.idx) } else if (!is.na(five.digit.idx)) { return(five.digit.idx) } else if (!is.na(utility.idx)) { return(c(utility.idx)) } else { return(NULL) }
  }

  getDSUIndex <- function(dat) {
    age    <- getAgeName()
    sex    <- getSexName()
    bwidth <- getBwidthName()
    age.idx <- match(tolower(age), tolower(colnames(dat)))
    sex.idx <- match(tolower(sex), tolower(colnames(dat)))
    bw.idx  <- match(tolower(bwidth), tolower(colnames(dat)))
    cols <- NULL
    if (!is.na(age.idx)) { cols <- c(cols, age.idx) }
    if (!is.na(sex.idx)) { cols <- c(cols, sex.idx) }
    if (!is.na(bw.idx))  { cols <- c(cols, bw.idx) }
    cols
  }

  output$eq5d_table <- DT::renderDataTable({
    req(input$multi == "multiple")
    req(input$data)
    req(input$version, input$type, input$country)

    vs <- valuesets(version = input$version, type = input$type, country = input$country)
    if ("Notes" %in% colnames(vs)) { vs <- vs[which(vs$Notes != "EuroQol (2019)"), , drop = FALSE] }
    req(nrow(vs) == 1)

    res <- getTableData()
    validate(need(!is.null(res), ""))

    if (input$version == "5L" && !any(dataset() > 3, na.rm = TRUE)) {
      showNotification("EQ-5D-5L selected, but all dimension scores are 1, 2, or 3. Is this correct?", type = "error", duration = 15)
    }
    res
  })

  output$export_table <- renderUI({ if (!is.null(input$data)) { downloadButton("download_table", 'Download Output File') } })

  output$download_table <- downloadHandler(
    filename = function() { paste(input$version, "_", input$country, "_", input$type, "_", format(Sys.time(), "%Y%m%d%H%M%S"), ".csv", sep = "") },
    content  = function(file) { write.csv(getTableData(), file, row.names = FALSE) }
  )

  getTableData <- reactive({
    req(input$version, input$type, input$country)
    dat <- dataset()
    if (is.null(dat)) { return() }

    if (input$type == "DSU") {
      if (!is.null(getBwidthName()) && getBwidthName() %in% colnames(dat)) {
        eq <- eq5d(dat, version = input$version, type = input$type, country = input$country, ignore.invalid = ignoreInvalid(), dimensions = get_dimension_names(), age = getAgeName(), sex = getSexName(), bwidth = getBwidthName())
      } else {
        eq <- eq5d(dat, version = input$version, type = input$type, country = input$country, ignore.invalid = ignoreInvalid(), dimensions = get_dimension_names(), age = getAgeName(), sex = getSexName())
      }
    } else {
      eq <- eq5d(dat, version = input$version, type = input$type, country = input$country, ignore.invalid = ignoreInvalid(), dimensions = get_dimension_names())
    }

    if (isTRUE(input$raw)) {
      rd <- rawdata()
      if (all(get_dimension_names() %in% colnames(rd))) {
        res <- cbind(rd, eq)
      } else if (all(rd[[getColumnIndex(rd)]] != round(rd[[getColumnIndex(rd)]]))) {
        res <- cbind(rd, eq)
      } else {
        res <- cbind(rd, dat, eq)
      }
    } else {
      res <- cbind(dat, eq)
    }

    colnames(res)[ncol(res)] <- "Index"

    if (all(get_dimension_names() %in% colnames(dat))) {
      if ("lss" %in% input$severity_scores) { res$LSS <- lss(dat, version = input$version, ignore.invalid = ignoreInvalid(), dimensions = get_dimension_names()) }
      if ("lfs" %in% input$severity_scores) { res$LFS <- lfs(dat, version = input$version, ignore.invalid = ignoreInvalid(), dimensions = get_dimension_names()) }
    }
    res
  })

  getTableDataByGroup <- reactive({
    data <- getTableData()
    if (is.null(data)) { return(data) }
    if (!is.null(input$plot_data)) { data <- data[!is.na(data[[input$plot_data]]), ] }
    if (!is.null(input$group) && input$group != "None") {
      data <- data[which(data[, input$group] %in% input$group_member), ]
      data[[input$group]] <- factor(data[[input$group]], levels = unique(data[[input$group]]))
    }
    data
  })

  ignoreInvalid <- reactive({ ifelse(is.null(input$ignore_invalid), TRUE, input$ignore_invalid) })

  output$plot <- renderGirafe({
    req(input$data, input$plot_type, input$group)
    if (input$plot_type == "summary") { req(input$summary_type) }
    if (input$group != "None") { req(input$group_member) }
    code <- getPlot()
    req(!is.null(code))
    out  <- girafe(ggobj = code)
    girafe_options(x = out, selection_type = "single", opts_toolbar(saveaspng = FALSE, hidden = c("lasso_select", "lasso_deselect")))
  })

  getPlot <- reactive({
    if (is.null(input$group)) { return() }
    if (input$plot_type == "density") { density_plot() }
    else if (input$plot_type == "ecdf") { ecdf_plot() }
    else if (input$plot_type == "hsdc") { hsdc_plot() }
    else if (input$plot_type == "hpg") { hpg_plot() }
    else if (input$plot_type == "radar") { radar_plot() }
    else if (input$plot_type == "summary") { bar_plot() }
    else { stop("Unable to identify plot type") }
  })

  density_plot <- reactive({
    req(input$data, input$plot_type, input$group)
    data <- getTableDataByGroup()
    if (nrow(data) == 0) { return() }
    ave.meth  <- get_average_method()
    plot_data <- sym(input$plot_data)
    if (input$group == "None" || !isTRUE(input$raw)) {
      p <- ggplot(data, aes(x = !!plot_data)) + geom_density(color = "darkblue", fill = "lightblue", alpha = 0.4)
      if (isTRUE(input$average)) { p <- p + geom_vline_interactive(aes(xintercept = !!ave.meth(data[[input$plot_data]])), color = "darkblue", linetype = "dashed", tooltip = paste0(input$average_method, ": ", get_average_value()), data_id = "density_mean") }
    } else {
      group   <- sym(input$group)
      colours <- getGroupColours()
      mu      <- get_average_value()
      p <- ggplot(data, aes(x = !!plot_data, fill = !!group)) + geom_density(alpha = 0.4) + scale_fill_manual(values = colours) + scale_color_manual(values = colours)
      if (isTRUE(input$average)) { p <- p + geom_vline_interactive(data = mu, aes(xintercept = x, color = group), linetype = "dashed", show.legend = FALSE, tooltip = paste0(input$average_method, ": ", mu$x), data_id = paste0("density_", input$average_method, "_", mu$group)) }
    }
    if (input$plot_data %in% c("MO", "SC", "UA", "PD", "AD")) { p <- p + scale_x_continuous(breaks = 1:getMaxLevels(), labels = 1:getMaxLevels()) }
    p
  })

  ecdf_plot <- reactive({
    req(input$data, input$plot_type, input$group)
    data <- getTableDataByGroup()
    if (nrow(data) == 0) { return() }
    ave.meth  <- get_average_method()
    plot_data <- sym(input$plot_data)
    if (input$group == "None" || !isTRUE(input$raw)) {
      p <- ggplot(data, aes(!!plot_data)) + stat_ecdf(geom = "step", colour = "darkblue")
      if (isTRUE(input$average)) { p <- p + geom_vline_interactive(aes(xintercept = !!ave.meth(data[[input$plot_data]])), color = "darkblue", linetype = "dashed", tooltip = paste0(input$average_method, ": ", get_average_value()), data_id = "ecdf_mean") }
    } else {
      group   <- sym(input$group)
      colours <- getGroupColours()
      p <- ggplot(data, aes(!!plot_data, colour = !!group)) + stat_ecdf(geom = "step") + scale_color_manual(values = colours)
      mu <- get_average_value()
      if (isTRUE(input$average)) { p <- p + geom_vline_interactive(data = mu, aes(xintercept = x, color = group), linetype = "dashed", show.legend = FALSE, tooltip = paste0(input$average_method, ": ", mu$x), data_id = paste0("ecdf_", input$average_method, "_", mu$group)) }
    }
    p + labs(y = 'Cumulative probability')
  })

  hsdc_plot <- reactive({
    req(input$data, input$plot_type, input$group)
    data <- getTableDataByGroup()
    if (nrow(data) == 0) { return() }
    if (input$group == "None" || !isTRUE(input$raw)) {
      hsdi_val <- hsdi(data, version = input$version)
      res <- eq5dcf(data, version = input$version, proportions = TRUE)
      res$CumulativeState <- 1:nrow(res) / nrow(res)
      ggplot(res, aes(CumulativeProp, CumulativeState)) +
        geom_line(color = "#FF9999") +
        annotate("segment", x = 0, y = 0, xend = 1, yend = 1, colour = "black") +
        annotate("text", x = 0.5, y = 0.9, label = paste0("HSDI=", hsdi_val)) +
        theme(panel.border = element_blank(), panel.grid.minor = element_blank()) +
        coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
        xlab("Cumulative proportion of observations") +
        ylab("Cumulative proportion of profiles")
    } else {
      colours <- getGroupColours()
      group   <- sym(input$group)
      hsdi_list <- sapply(input$group_member, function(x) { hsdi(data[data[[input$group]] == x, ], version = input$version) })
      names(hsdi_list) <- input$group_member
      label <- paste("HSDI:", paste(names(hsdi_list), hsdi_list, sep = "=", collapse = ", "))
      res <- lapply(unique(data[[input$group]]), function(x) {
        cf <- eq5dcf(data[data[[input$group]] == x, ], version = input$version, proportions = TRUE)
        cf$CumulativeState <- 1:nrow(cf) / nrow(cf)
        cf[, input$group] <- x
        cf
      })
      res <- do.call(rbind, res)
      ggplot(res, aes(CumulativeProp, CumulativeState, colour = !!group, group = !!group)) +
        geom_line() +
        annotate("segment", x = 0, y = 0, xend = 1, yend = 1, colour = "black") +
        annotate("text", x = 0.5, y = 0.9, label = label, size = 3) +
        theme(panel.border = element_blank(), panel.grid.minor = element_blank()) +
        coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
        scale_fill_manual(values = colours) +
        scale_color_manual(values = colours) +
        xlab("Cumulative proportion of observations") +
        ylab("Cumulative proportion of profiles")
    }
  })

  hpg_plot <- reactive({
    req(input$data, input$plot_type, input$group)
    if (is.null(getPaired)) { stop("Paired data required for HPG plot.") }
    if (input$group == "None") { stop("Group required for HPG plot.") }
    if (length(input$group_member) != 2) { stop("Two levels required for HPG plot") }
    if (!is.null(input$paired) && !input$paired) { stop("Data are paired checkbox must be selected") }
    data <- getTableDataByGroup()
    data <- data[order(data[[getPaired()]], data[[input$group]]), ]
    pre  <- data[data[[input$group]] == input$group_member[1], ]
    post <- data[data[[input$group]] == input$group_member[2], ]
    if (!all.equal(pre[[getPaired()]], post[[getPaired()]])) { stop("Unable to match subjects") }
    res <- hpg(pre, post, country = input$country, version = input$version, type = input$type)
    ts  <- length(get_all_health_states(input$version))
    ggplot(res, aes(Post, Pre, color = PCHC)) +
      geom_point(aes(shape = PCHC)) +
      coord_cartesian(xlim = c(1, ts), ylim = c(1, ts)) +
      scale_x_continuous(breaks = c(1, ts)) +
      scale_y_continuous(breaks = c(1, ts)) +
      annotate("segment", x = 1, y = 1, xend = ts, yend = ts, colour = "black") +
      theme(panel.border = element_blank(), panel.grid.minor = element_blank()) +
      xlab(input$group_member[2]) +
      ylab(input$group_member[1])
  })

  radar_plot <- reactive({
    req(input$data, input$plot_type, input$group)
    if (!all(get_dimension_names() %in% colnames(dataset()))) { stop("Unable to generate plot without dimension data.") }
    data <- getTableDataByGroup()
    if (input$group == "None" || !isTRUE(input$raw)) {
      data <- data[, names(data) %in% get_dimension_names()]
      ggRadar(data = data, rescale = FALSE, colour = "#F8766D", alpha = 0.4)
    } else {
      group   <- sym(input$group)
      colours <- getGroupColours()
      data <- data[, names(data) %in% c("MO", "SC", "UA", "PD", "AD", input$group)]
      ggRadar(data = data, aes(color = !!group), rescale = FALSE) + scale_fill_manual(values = colours) + scale_color_manual(values = colours) + theme(legend.position = "right")
    }
  })

  bar_plot <- reactive({
    req(input$data, input$plot_type, input$group, input$summary_type)
    if (!all(get_dimension_names() %in% colnames(dataset()))) { stop("Unable to generate plot without dimension data.") }
    data <- getTableDataByGroup()
    counts <- ifelse(input$summary_type == "counts", TRUE, FALSE)
    summary_type <- sym(toTitleCase(input$summary_type))
    if (input$group == "None" || !isTRUE(input$raw)) {
      data <- eq5dds(data, version = input$version, counts = counts, dimensions = get_dimension_names())
      data <- melt(as.matrix(data))
      colnames(data) <- c("Score", "Dimension", summary_type)
      data$Score <- as.factor(data$Score)
      ggplot(data, aes(fill = Score, y = !!summary_type, x = Dimension, tooltip = !!summary_type)) + geom_bar_interactive(position = "dodge", stat = "identity")
    } else {
      data <- eq5dds(data, version = input$version, counts = counts, by = input$group, dimensions = get_dimension_names())
      data <- lapply(data, function(x) { melt(as.matrix(x)) })
      data <- do.call(rbind, unname(Map(cbind, Group = names(data), data)))
      colnames(data) <- c(input$group, "Score", "Dimension", summary_type)
      data$Score <- as.factor(data$Score)
      data[[input$group]] <- factor(data[[input$group]], levels = unique(data[[input$group]]))
      ggplot(data, aes(fill = Score, y = !!summary_type, x = Dimension, tooltip = !!summary_type)) + geom_bar_interactive(position = "dodge", stat = "identity") + facet_wrap(as.formula(paste("~", input$group)))
    }
  })

  output$export_plot <- renderUI({ if (!is.null(input$data)) { downloadButton("download_plot", 'Download plot') } })

  output$download_plot <- downloadHandler(
    filename = function() { paste(input$version, "_", input$country, "_", input$type, "_", input$plot_type, "_", format(Sys.time(), "%Y%m%d%H%M%S"), ".pdf", sep = "") },
    content  = function(file) { ggsave(file, getPlot(), "pdf") }
  )

  output$choose_plot_data <- renderUI({
    options <- "Index"
    if ("lss" %in% input$severity_scores) { options <- c(options, "LSS") }
    selectInput("plot_data", "Plot data:", c(options, "MO", "SC", "UA", "PD", "AD"))
  })

  output$choose_plot_type <- renderUI({
    selectInput("plot_type", "Plot type:", c("Summary" = "summary", "Density" = "density", "ECDF" = "ecdf", "Health State Density Curve" = "hsdc", "Health Profile Grid" = "hpg", "Radar" = "radar"))
  })

  output$choose_group_by <- renderUI({
    if (is.null(input$data)) { return() }
    data <- getTableData()
    if (is.null(data)) { return() }
    data <- data[!tolower(colnames(data)) %in% tolower(c(get_dimension_names(), getStateName(), "LFS"))]
    data <- data[sapply(data, function(x) is.character(x) || is.logical(x) || is.factor(x))]
    groups <- "None"
    if (ncol(data) > 0) {
      include <- apply(data, 2, function(x) { length(unique(x)) != length(x) })
      groups <- c(groups, names(which(include)))
    }
    selectInput("group", "Group by:", groups)
  })

  output$choose_group_members <- renderUI({
    if (is.null(input$group) || input$group == "None") { return() }
    data <- getTableData()
    pickerInput(inputId = "group_member", label = "Select/deselect group members", choices = unique(data[[input$group]]), selected = unique(data[[input$group]]), options = list(`actions-box` = FALSE, `none-selected-text` = "Please select at least one."), multiple = TRUE)
  })

  getSummary <- reactive({
    req(input$data, input$plot_type, input$summary_type)
    if (!all(get_dimension_names() %in% colnames(dataset()))) { stop("Unable to generate summary without dimension data.") }
    counts <- ifelse(input$summary_type == "counts", TRUE, FALSE)
    data <- getTableDataByGroup()
    if (input$group == "None" || !isTRUE(input$raw)) { eq5dds(data, version = input$version, counts = counts, dimensions = get_dimension_names()) } else { eq5dds(data, version = input$version, counts = counts, by = input$group, dimensions = get_dimension_names()) }
  })

  getShannon <- reactive({
    req(input$data, input$plot_type, input$summary_type)
    if (!all(get_dimension_names() %in% colnames(dataset()))) { stop("Unable to generate summary without dimension data.") }
    data <- getTableDataByGroup()
    if (input$group != "None" && isTRUE(input$raw)) { return() }
    permutations <- input$shannon_type == "permutations"
    sh <- shannon(data, version = input$version, by.dimension = TRUE, ignore.invalid = ignoreInvalid(), dimensions = get_dimension_names(), permutations = permutations)
    sh <- as.data.frame(t(do.call(rbind, sh)))
    rownames(sh) <- c("H'", "H' max", "J'")
    sh$Overall <- shannon(data, version = input$version, by.dimension = FALSE, ignore.invalid = ignoreInvalid(), dimensions = get_dimension_names(), permutations = permutations)
    sh
  })

  getPaired <- reactive({
    data <- getTableDataByGroup()
    group.totals <- table(data[input$group])
    data <- data[!colnames(data) %in% c(get_dimension_names(), "Index", input$group)]
    if (ncol(data) > 0) {
      id.columns <- lapply(data, function(x) {
        id.totals <- table(x)
        length(unique(id.totals)) == 1 & length(unique(group.totals)) == 1 & sum(id.totals) == sum(group.totals)
      })
      return(names(id.columns)[which(unlist(id.columns))])
    }
  })

  output$show_paired <- renderUI({
    if (!is.null(input$group) && input$group != "None") {
      if (length(getPaired()) == 0) { return() }
      data <- getTableData()
      data <- data[!colnames(data) %in% c(get_dimension_names(), "Index", input$group)]
      id.groups <- getPaired()
      if (!is.null(id.groups)) {
        tagList(checkboxInput("paired", "Data are paired", TRUE), selectInput("id", "ID column:", choices = id.groups, selected = FALSE, selectize = FALSE))
      }
    }
  })

  output$show_average         <- renderUI({ checkboxInput("average", "Show mean/median on plot", TRUE) })
  output$choose_average_method <- renderUI({ radioButtons("average_method", "Show:", c("Mean" = "mean", "Median" = "median"), selected = "mean", inline = TRUE) })
  output$choose_summary_type   <- renderUI({ radioButtons("summary_type", "Summary type:", c("Counts" = "counts", "Percentage" = "percentage"), selected = "percentage", inline = TRUE) })
  output$choose_shannon_type   <- renderUI({ radioButtons("shannon_type", "Shannon type:", c("Permutations" = "permutations", "Observations" = "observations"), selected = "permutations", inline = TRUE) })

  getStatistics <- reactive({
    if (is.null(input$group) || input$group == "None") { return() }
    data <- getTableDataByGroup()
    stats <- NULL
    if (length(input$group_member) == 2) {
      stats <- getWilcoxStats()
    } else if (length(input$group_member) > 2) {
      if (!is.null(getPaired()) & length(getPaired()) > 0 & !is.null(input$paired) && input$paired) { stats <- getFriedmanStats() } else { stats <- getKruskalStats() }
    }
    stats
  })

  output$statistics <- renderUI({
    if (is.null(input$data) || is.null(input$plot_type)) { return() }
    if (input$plot_type %in% c("radar", "summary", "hsdc")) {
      summ <- getSummary(); sh <- getShannon()
      if (input$group == "None" || !isTRUE(input$raw)) {
        tagList(tabsetPanel(tabPanel("Descriptive", h5(paste("Descriptive system by", input$summary_type)), renderDT(summ, options = list(searching = FALSE, paging = FALSE, info = FALSE))), tabPanel("Shannon", h5("Shannon's index values"), renderDT(sh, options = list(searching = FALSE, paging = FALSE, info = FALSE)))))
      } else {
        if (length(summ) == 0) { return() }
        tagList(h5(paste("Descriptive system by", input$summary_type)), radioButtons("eq5dds", "Group table:", names(summ), selected = ifelse(is.null(input$eq5dds), names(summ)[1], input$eq5dds), inline = TRUE), renderDT(summ[[input$eq5dds]], options = list(searching = FALSE, paging = FALSE, info = FALSE)))
      }
    } else if (input$plot_type == "hpg") {
      data <- getTableDataByGroup()
      if (length(input$group_member) != 2) { stop("Two levels required for HPG plot") }
      pre  <- data[data[[input$group]] == input$group_member[1], ]
      post <- data[data[[input$group]] == input$group_member[2], ]
      if (!all.equal(pre[[input$id]], post[[input$id]])) { stop("Paired IDs do not match") }
      if (!input$paired) { stop("Data are paired checkbox must be selected") }
      pchc_tbl <- pchc(pre, post, version = input$version, no.problems = TRUE, totals = TRUE, summary = TRUE)
      tagList(h5("PCHC"), renderDT(pchc_tbl, options = list(searching = FALSE, paging = FALSE, info = FALSE)))
    } else {
      stats <- getStatistics(); if (is.null(stats)) { return("Select a group to perform statistical tests.") }
      taglist <- tagList(h4("Statistical analysis"), p(strong(stats$method)), p(strong(paste("Data:")), paste(stats$data.name)), p(strong(paste0(names(stats$statistic))), paste0(" = ", round(stats$statistic, 1)), strong(paste0("p value = ")), paste0(round(stats$p.value, 5))))
      if (length(input$group_member) > 2 & stats$p.value < 0.05) { taglist[[length(taglist) + 1]] <- actionButton("posthoc", "View post hoc tests") }
      taglist
    }
  })

  output$posthocTable <- renderDataTable({ stats <- getStatistics(); if (is.null(stats) || is.null(stats$posthoc)) { return(NULL) } ; table <- data.frame(lapply(stats$posthoc$res, function(y) if (is.numeric(y)) { round(y, 5) } else { y })); datatable(table) })

  observeEvent(input$posthoc, { stats <- getStatistics(); if (is.null(stats) || is.null(stats$posthoc)) { return() } ; showModal(modalDialog(h2("Post hoc tests"), p(stats$posthoc$method), DT::dataTableOutput('posthocTable'), uiOutput("export_posthoc"), size = "m")) })

  getWilcoxStats <- reactive({ data <- getTableDataByGroup(); if (is.null(data)) { return() } ; paired <- FALSE; if (!is.null(getPaired()) & length(getPaired()) > 0 & !is.null(input$paired) && input$paired) { data <- data[order(data[[input$id]], data[[input$group]]), ]; paired <- TRUE } ; wilcox.test(as.formula(paste(input$plot_data, " ~ ", input$group)), data, paired = paired) })
  getKruskalStats <- reactive({ data <- getTableDataByGroup(); if (is.null(data)) { return() } ; res <- kruskal.test(as.formula(paste(input$plot_data, " ~ ", input$group)), data); if (res$p.value < 0.05) { res$posthoc <- dunnTest(as.formula(paste(input$plot_data, " ~ ", input$group)), data); res$posthoc$method <- paste("Dunn's test with", res$posthoc$method, "correction") } ; res })
  getFriedmanStats <- reactive({ data <- getTableDataByGroup(); if (is.null(data)) { return() } ; res <- friedman.test(as.formula(paste(input$plot_data, " ~ ", input$group, " | ", input$id)), data = data); if (res$p.value < 0.05) { nt <- frdAllPairsNemenyiTest(as.formula(paste(input$plot_data, " ~ ", input$group, " | ", input$id)), data); nt.stats <- na.omit(as.data.frame(as.table(nt$statistic))); nt.p.value <- na.omit(as.data.frame(as.table(nt$p.value))); nt$res <- data.frame(Comparison = paste(nt.stats$Var2, "-", nt.stats$Var1), "Mean rank diff" = nt.stats$Freq, P.adj = nt.p.value$Freq); nt$method <- paste("Nemenyi test with", nt$p.adjust.method, "correction"); res$posthoc <- nt } ; res })

  output$export_posthoc <- renderUI({ downloadButton("download_posthoc", 'Download Post Hoc Data') })
  output$download_posthoc <- downloadHandler(
    filename = function() { paste(input$version, "_", input$country, "_", input$type, "_post_hoc_", format(Sys.time(), "%Y%m%d%H%M%S"), ".csv", sep = "") },
    content  = function(file) { stats <- getStatistics(); if (is.null(stats) || is.null(stats$posthoc)) { return() } ; write.csv(stats$posthoc$res, file, row.names = FALSE) }
  )

  output$ignore_invalid <- renderUI({ checkboxInput("ignore_invalid", "Ignore data with invalid/incomplete/missing dimension scores", TRUE) })

  output$stats_tests <- renderTable({ data.frame(Groups = c(2, 2, ">2", ">2"), Paired = c("No", "Yes", "No", "Yes"), Test = c("Wilcoxon rank sum test", "Wilcoxon signed rank test", "Kruskal-Wallis rank sum test with Dunn's test for post hoc testing.", "Friedman's rank sum test with the Nemenyi test for post hoc testing.")) })

  get_average_method <- reactive({ if (input$average_method == "mean") { mean } else { median } })

  get_average_value  <- reactive({ data <- getTableDataByGroup(); if (is.null(data) || nrow(data) == 0) { return() } ; ave.meth <- get_average_method(); if (input$group == "None") { aggregate(as.formula(paste(input$plot_data, "~ 1")), data, function(x) { round(ave.meth(x), 3) }) } else { aggregate(data[[input$plot_data]], list(group = data[[input$group]]), function(x) { round(ave.meth(x), 3) }) } })

  ggplotColours <- function(n = 6, h = c(0, 360) + 15) { if ((diff(h) %% 360) < 1) { h[2] <- h[2] - 360 / n } ; hcl(h = (seq(h[1], h[2], length = n)), c = 100, l = 65) }
  getGroupColours <- reactive({ data <- getTableData(); if (is.null(data)) { return() } ; groups <- unique(data[[input$group]]); colours <- ggplotColours(length(groups)); names(colours) <- groups; colours })

  check.integer <- function(N) { !grepl("[^[:digit:]]", format(N, digits = 20, scientific = FALSE)) }

  get_dimension_names <- reactive({ dimensions <- c(vals$MO, vals$SC, vals$UA, vals$PD, vals$AD); if (all(!is.null(dimensions))) { c(vals$MO, vals$SC, vals$UA, vals$PD, vals$AD) } else { c("MO", "SC", "UA", "PD", "AD") } })
  getStateName   <- reactive({ if (!is.null(vals$State)) { vals$State } else { "State" } })
  getUtilityName <- reactive({ if (!is.null(vals$Utility)) { vals$Utility } else { "Utility" } })
  getAgeName     <- reactive({ if (!is.null(vals$Age)) { vals$Age } else { "Age" } })
  getSexName     <- reactive({ if (!is.null(vals$Sex)) { vals$Sex } else { "Sex" } })
  getBwidthName  <- reactive({ if (!is.null(vals$bwidth)) { vals$bwidth } else { "bwidth" } })

  getReadableCountryNames <- reactive({ countries <- sort(unique(as.character(valuesets(version = input$version)$Country))); countries.list <- as.list(countries); countries <- gsub("_", " ", countries); countries <- unlist(lapply(strsplit(gsub("([[:lower:]][[:lower:]])([[:upper:]])", "\\1 \\2", countries), " "), function(x) { paste(x, collapse = " ") })); names(countries.list) <- countries; countries.list })

  getValueSet <- reactive({ req(input$version, input$type, input$country); vs <- valuesets(version = input$version, type = input$type, country = input$country); if (nrow(vs) != 1) { return(NULL) } ; vs })

  output$reference_links <- renderUI({ div(span("Value set references:", style = "color:grey"), getDOI(), getPubMed(), getISBN(), getExternalURL(), style = "padding-bottom: 16px") })
  getDOI        <- reactive({ vs <- getValueSet(); if (is.null(vs)) { return() } ; if (!is.na(vs$DOI)) { a('DOI', href = paste0('https://doi.org/', vs$DOI), target = "_blank") } })
  getPubMed     <- reactive({ vs <- getValueSet(); if (is.null(vs)) { return() } ; if (!is.na(vs$PubMed)) { a('PubMed', href = paste0('https://pubmed.ncbi.nlm.nih.gov/', vs$PubMed, '/'), target = "_blank") } })
  getISBN       <- reactive({ vs <- getValueSet(); if (is.null(vs)) { return() } ; if (!is.na(vs$ISBN)) { a('ISBN', href = paste0('https://isbndb.com/book/', vs$ISBN), target = "_blank") } })
  getExternalURL<- reactive({ vs <- getValueSet(); if (is.null(vs)) { return() } ; if (!is.na(vs$ExternalURL)) { a('External URL', href = vs$ExternalURL, target = "_blank") } })

  output$eq5d_text <- renderText({
    if (any(is.null(input$mo), is.null(input$sc), is.null(input$ua), is.null(input$pd), is.null(input$ad))) { return() }
    if (any(is.null(input$version), is.null(input$type), is.null(input$country)))  { return() }
    if (!paste0("EQ-5D-", input$version) %in% valuesets(type = input$type)$Version) { return() }
    if (!input$country %in% valuesets(type = input$type)$Country) { return() }

    dimensions <- c(MO = input$mo, SC = input$sc, UA = input$ua, PD = input$pd, AD = input$ad)
    class(dimensions) <- "numeric"

    if (input$type == "DSU") {
      if (input$utility != "") {
        utility <- suppressWarnings(as.numeric(input$utility))
        if (is.na(utility)) { stop("Utility score is not numeric.") }
        score <- eq5d(utility, version = input$version, type = input$type, country = input$country, age = input$age, sex = input$sex, bwidth = input$bwidth)
      } else {
        score <- eq5d(dimensions, version = input$version, type = input$type, country = input$country, age = input$age, sex = input$sex, bwidth = input$bwidth)
      }
    } else {
      score <- eq5d(dimensions, version = input$version, type = input$type, country = input$country)
    }

    paste0("The index for EQ-5D-", input$version, " ", input$country, " ", input$type, " value set is: ", score)
  })

})
