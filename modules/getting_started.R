# Module for ShinyStat welcome page and data import
#   It passes on the uploaded data to the descriptive module
library(shiny)
library(shinydashboard)
library(sjlabelled) # for labelled SPSS data
library(jmvReadWrite) # for jamovi import
library(DT) # for data tables

getting_started_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    tagList(
      fluidRow(
        column(
          width = 12,
          h1("Welcome to ShinyStat"),
          h2("A Shiny app for statistical analysis"),
          p("This app is designed to help you perform statistical analysis on your data. It is a work in progress, so please let me know if you have any suggestions for improvements."),
          h3("Get started"),
          p("To get started, please upload your data file. The app currently supports SPSS (.sav), jamovi (.omv) and CSV files with first rows as header. If you have a different file type, please let me know and I will try to add support for it."),
          fileInput(
            inputId = ns("file"),
            label = "Choose a file",
            accept = c(
              ".sav",
              ".omv",
              ".csv"
            )
          ),
          uiOutput(
            outputId = ns("warning_box")
          ),
          # verbatimTextOutput(
          #   outputId = ns("debug")
          # ),
          uiOutput(
            outputId = ns("preview_tab")
          )
        )
      )
    )
  )
}

getting_started_server <- function(input, output, session) {
  datafile <- reactiveValues(df = NULL)

  # Get the file input and assign it to the datafile reactive value
  observe({
    df <- data.frame()
    # Check if input$file is loaded, then run import
    if (!is.null(input$file)) {
      if (endsWith(input$file$datapath, ".sav")) {
        df <- sjlabelled::read_spss(input$file$datapath, drop.labels = FALSE)
      } else if (endsWith(input$file$datapath, ".omv")) {
        df <- jmvReadWrite::read_omv(input$file$datapath)
      } else if (endsWith(input$file$datapath, ".csv")) {
        df <- read.csv(input$file$datapath,
          header = TRUE, stringsAsFactors = TRUE
        )
      }
    }

    # Assign the datafile reactive value
    datafile$df <- df
  })

  # Render the warning box
  output$warning_box <- renderUI({
    if (!is.null(input$file) &&
      length(which(sapply(datafile$df, is.factor))) > 0) {
      box(
        title = "Warning: factor variables detected",
        status = "warning",
        solidHeader = TRUE,
        collapsible = FALSE,
        collapsed = FALSE,
        width = 12,
        renderText(
          paste(
            "This datafile contains factor variables.",
            "Factor variables are not suitable for certain statistical",
            "tests (e.g. mean, SD), and will be excluded from analysis.",
            "While you might consider converting factor variables to",
            "numeric variables, caution is warranted when interpreting",
            "results of tests run on converted variables."
          )
        ),
        renderText(
          paste(
            "This data file contains the following factor variables:",
            paste0(names(datafile$df)[sapply(
              datafile$df,
              is.factor
            )], collapse = ", ")
          )
        )
      )
    } else {
      NULL
    }
  })

  # Datafile variable dimensions and header
  output$debug <- renderPrint({
    if (!is.null(input$file)) {
      list(
        # get levels for all factors
        levels = sapply(datafile$df, function(x) {
          if (is.factor(x)) {
            levels(x)
          } else {
            NA
          }
        })
      )
    } else {
      "No datafile loaded"
    }
  })

  # Datafile preview tab
  output$preview_tab <- renderUI({
    if (!is.null(input$file)) {
      DT::renderDataTable({
        DT::datatable(
          datafile$df,
          caption = "Datafile preview",
          options = list(
            scrollX = TRUE,
            pageLength = 5
          )
        )
      })
    } else {
      NULL
    }
  })

  # Return the datafile reactive value
  return(datafile)
}
