# modules/getting_started.R
library(shiny)
library(shinydashboard)
library(haven) # needed for sjlabelled to work on shinyapps.io
library(sjlabelled) # for labelled SPSS data
library(jmvReadWrite) # for jamovi import
library(DT) # for data tables
library(dplyr) # for data wrangling
library(stringr) # for string manipulation

getting_started_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    tagList(
      fluidRow(
        column(
          width = 12,
          h2("Welcome to SJMV"),
          h3("Get started"),
          p("To get started, please upload your data file. The app currently supports SPSS (.sav), jamovi (.omv) and CSV files with first rows as header."),
          fileInput(
            inputId = ns("file"),
            label = "Choose a file",
            accept = c(
              ".sav",
              ".omv",
              ".csv"
            )
          ),
          uiOutput(outputId = ns("data_management_controls_ui")),
          uiOutput(
            outputId = ns("data_types")
          ),
          uiOutput(
            outputId = ns("warning_box")
          ),
          uiOutput(
            outputId = ns("preview_tab")
          )
        )
      )
    )
  )
}

getting_started_server <- function(input, output, session) {
  ns <- session$ns
  datafile <- reactiveValues(
    df_raw = NULL,
    df_filtered = NULL,
    type = NULL,
    filter_active = FALSE,
    excluded_row_ids = numeric(0)
  )

  # Get the file input and assign it to the datafile reactive value
  observe({
    req(input$file)
    df <- data.frame()
    if (endsWith(input$file$datapath, ".sav")) {
      df <- sjlabelled::read_spss(input$file$datapath, convert.factors = FALSE)
      df <- as.data.frame(lapply(df, function(x) {
        labs <- attr(x, "labels", exact = TRUE)
        lab <- attr(x, "label", exact = TRUE)
        if (is.atomic(x) && !is.null(labs) && length(labs) >= length(unique(stats::na.omit(x)))) {
          x <- as_factor(x)
          if (!is.null(lab)) attr(x, "label") <- lab
        }
        x
      }), stringsAsFactors = FALSE)
      datafile$type <- "SPSS"
    } else if (endsWith(input$file$datapath, ".omv")) {
      df <- jmvReadWrite::read_omv(input$file$datapath)
      datafile$type <- "jamovi"
    } else if (endsWith(input$file$datapath, ".csv")) {
      df <- read.csv(input$file$datapath, header = TRUE, stringsAsFactors = TRUE)
      datafile$type <- "CSV"
    }

    datafile$df_raw <- df
    datafile$df_filtered <- df
    datafile$filter_active <- FALSE
    datafile$excluded_row_ids <- numeric(0)
  })

  # --- DATA MANAGEMENT CONTROLS (FILTER & RELABEL) ---

  # 1. UI buttons and status render
  output$data_management_controls_ui <- renderUI({
    req(datafile$df_raw)
    tagList(
      actionButton(ns("show_filter_modal"), "Filter Data", icon = icon("filter")),
      actionButton(ns("show_relabel_modal"), "Relabel Factor Levels", icon = icon("tags")),
      actionButton(ns("reset_filters"), "Reset Filters & Exclusions", icon = icon("eraser")),
      br(), br(),
      uiOutput(ns("filter_status_text"))
    )
  })

  # 2. Status text
  output$filter_status_text <- renderUI({
    n_excluded <- length(datafile$excluded_row_ids)
    n_raw <- nrow(datafile$df_raw)
    n_filtered <- nrow(datafile$df_filtered)

    msg_parts <- c()
    if (n_excluded > 0) {
      msg_parts <- c(msg_parts, sprintf("%d cases excluded by row ID.", n_excluded))
    }
    if (n_filtered < (n_raw - n_excluded)) {
      msg_parts <- c(msg_parts, "Additional filtering by variable values is active.")
    }

    if (datafile$filter_active) {
      final_msg <- paste(
        "Filtering is active!",
        paste(msg_parts, collapse = " "),
        sprintf("Showing %d of %d original cases (%.1f%%).", n_filtered, n_raw, (n_filtered / n_raw) * 100)
      )
      tags$p(tags$b(style = "color: #d33;", final_msg))
    } else {
      tags$p(tags$i("No filters or exclusions are currently active."))
    }
  })

  # --- Filtering Logic ---
  observeEvent(input$show_filter_modal, {
    req(datafile$df_raw)
    showModal(modalDialog(
      title = "Filter Data",
      size = "l",
      textAreaInput(
        inputId = ns("exclude_ids_text"),
        label = "Exclude rows by ID (comma-separated):",
        placeholder = "e.g. 5, 12, 43",
        value = paste(datafile$excluded_row_ids, collapse = ", "),
        rows = 2
      ),
      hr(),
      lapply(names(datafile$df_raw), function(var_name) {
        var_label <- sjlabelled::get_label(datafile$df_raw[[var_name]], def.value = var_name)
        enable_filter_input_id <- ns(paste0("enable_filter_", var_name))

        filter_control_ui <- if (is.factor(datafile$df_raw[[var_name]])) {
          choices <- levels(datafile$df_raw[[var_name]])
          pickerInput(inputId = ns(paste0("filter_", var_name)), label = NULL, choices = choices, multiple = TRUE, options = pickerOptions(actionsBox = TRUE, liveSearch = TRUE))
        } else if (is.numeric(datafile$df_raw[[var_name]])) {
          min_val <- min(datafile$df_raw[[var_name]], na.rm = TRUE)
          max_val <- max(datafile$df_raw[[var_name]], na.rm = TRUE)
          sliderInput(inputId = ns(paste0("filter_", var_name)), label = NULL, min = min_val, max = max_val, value = c(min_val, max_val), step = 1)
        }

        tagList(
          fluidRow(
            column(6, tags$b(paste(var_name, "-", var_label))),
            column(6, materialSwitch(inputId = enable_filter_input_id, label = "Filter by this variable", status = "primary", right = TRUE))
          ),
          conditionalPanel(condition = paste0("input['", enable_filter_input_id, "']"), filter_control_ui),
          hr(style = "margin-top: 5px; margin-bottom: 15px;")
        )
      }),
      footer = tagList(modalButton("Cancel"), actionButton(ns("apply_filters"), "Apply Filters", class = "btn-primary"))
    ))
  })

  observeEvent(input$apply_filters, {
    id_string <- input$exclude_ids_text
    excluded_ids <- as.numeric(str_trim(unlist(strsplit(id_string, ","))))
    excluded_ids <- unique(na.omit(excluded_ids[excluded_ids > 0]))
    datafile$excluded_row_ids <- excluded_ids

    df_temp <- datafile$df_raw
    if (length(excluded_ids) > 0) {
      df_temp <- df_temp %>% filter(!row_number() %in% excluded_ids)
    }

    for (var_name in names(datafile$df_raw)) {
      enable_filter_input_id <- paste0("enable_filter_", var_name)
      if (isTRUE(input[[enable_filter_input_id]])) {
        filter_input_id <- paste0("filter_", var_name)
        if (!is.null(input[[filter_input_id]])) {
          if (is.factor(df_temp[[var_name]])) {
            df_temp <- df_temp %>% filter(.data[[var_name]] %in% input[[filter_input_id]])
          } else if (is.numeric(df_temp[[var_name]])) {
            slider_range <- input[[filter_input_id]]
            df_temp <- df_temp %>% filter(is.na(.data[[var_name]]) | (.data[[var_name]] >= slider_range[1] & .data[[var_name]] <= slider_range[2]))
          }
        }
      }
    }
    datafile$df_filtered <- df_temp

    if (length(datafile$excluded_row_ids) > 0 || nrow(datafile$df_filtered) < nrow(datafile$df_raw)) {
      datafile$filter_active <- TRUE
    } else {
      datafile$filter_active <- FALSE
    }
    removeModal()
  })

  observeEvent(input$reset_filters, {
    datafile$df_filtered <- datafile$df_raw
    datafile$filter_active <- FALSE
    datafile$excluded_row_ids <- numeric(0)
    showNotification("All filters and row exclusions have been reset.", type = "message")
  })

  # --- NEW: Relabeling Logic ---

  # 4. Show Relabeling Modal
  observeEvent(input$show_relabel_modal, {
    req(datafile$df_raw)
    factor_vars <- names(datafile$df_raw)[sapply(datafile$df_raw, is.factor)]

    showModal(modalDialog(
      title = "Relabel Factor Levels",
      selectInput(ns("relabel_var_select"), "Select factor variable to relabel:", choices = factor_vars),
      uiOutput(ns("relabel_ui_dynamic")),
      footer = tagList(modalButton("Cancel"), actionButton(ns("apply_relabeling"), "Apply New Labels", class = "btn-primary"))
    ))
  })

  # 5. Dynamically generate UI for relabeling
  output$relabel_ui_dynamic <- renderUI({
    req(input$relabel_var_select)
    selected_var <- input$relabel_var_select
    current_levels <- levels(datafile$df_raw[[selected_var]])

    lapply(seq_along(current_levels), function(i) {
      textInput(ns(paste0("relabel_level_", i)), label = paste0("Old: ", current_levels[i]), value = current_levels[i])
    })
  })

  # 6. Apply the relabeling changes
  observeEvent(input$apply_relabeling, {
    req(input$relabel_var_select)
    var_to_relabel <- input$relabel_var_select
    old_levels <- levels(datafile$df_raw[[var_to_relabel]])

    # Collect new labels from the dynamic text inputs
    new_levels <- vapply(seq_along(old_levels), function(i) {
      input[[paste0("relabel_level_", i)]]
    }, FUN.VALUE = character(1))

    # Check for empty or duplicate new labels
    if (any(new_levels == "") || any(duplicated(new_levels))) {
      showNotification("New labels cannot be empty or duplicated.", type = "error")
      return()
    }

    # Apply to raw data
    levels(datafile$df_raw[[var_to_relabel]]) <- new_levels
    # Apply to filtered data
    levels(datafile$df_filtered[[var_to_relabel]]) <- new_levels

    showNotification(paste("Levels for variable", var_to_relabel, "have been updated."), type = "message")
    removeModal()
  })

  # --- END OF DATA MANAGEMENT ---

  # Render the data types box
  output$data_types <- renderUI({
    req(datafile$df_raw)
    list(
      h3("Data types"),
      DT::renderDataTable(
        DT::datatable(
          data.frame(
            variable = names(datafile$df_raw),
            label = sjlabelled::get_label(datafile$df_raw, def.value = NA),
            datatype = sapply(datafile$df_raw, function(x) paste(class(x), collapse = ", ")),
            levels = sapply(datafile$df_raw, function(x) {
              if (is.factor(x)) paste(levels(x), collapse = ", ") else NA
            }),
            stringsAsFactors = FALSE
          ),
          colnames = c("Variable", "Description", "Data type", "Levels"),
          rownames = FALSE,
          options = list(scrollX = TRUE, pageLength = 10, dom = "t")
        )
      )
    )
  })

  # Render the warning box
  output$warning_box <- renderUI({
    req(datafile$df_raw)
    if (any(sapply(datafile$df_raw, is.factor))) {
      list(
        br(),
        box(
          title = "Warning: factor variables detected",
          status = "warning",
          solidHeader = TRUE,
          collapsible = TRUE,
          collapsed = TRUE,
          width = 12,
          renderText(paste("Factor variables are not suitable for certain statistical tests (e.g. mean, SD), and will be excluded from such analysis.")),
          br(),
          renderText(paste("This data file contains the following factor variables:", paste0(names(datafile$df_raw)[sapply(datafile$df_raw, is.factor)], collapse = ", ")))
        )
      )
    }
  })

  # Datafile preview tab uses the FILTERED data
  output$preview_tab <- renderUI({
    req(datafile$df_filtered)
    df_preview <- datafile$df_filtered
    row.names(df_preview) <- row.names(datafile$df_raw)[row.names(datafile$df_raw) %in% row.names(df_preview)]
    df_preview <- df_preview %>% tibble::rownames_to_column(var = "original_row_id")

    list(
      h3("Datafile preview (filtered data with original row numbers)"),
      DT::renderDataTable({
        DT::datatable(df_preview, options = list(scrollX = TRUE, pageLength = 10, dom = "ltipr"))
      })
    )
  })

  return(datafile)
}
