# Module for ShinyStat welcome page and data import
#   It passes on the uploaded data to the descriptive module
library(shiny)
library(shinydashboard)
library(haven) # needed for sjlabelled to work on shinyapps.io
library(sjlabelled) # for labelled SPSS data
library(jmvReadWrite) # for jamovi import
library(DT) # for data tables
library(dplyr) # for data wrangling

getting_started_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    tagList(
      fluidRow(
        column(
          width = 12,
          h2("Welcome to ShinyStat"),
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
  datafile <- reactiveValues(df = NULL, type = NULL)

  # Get the file input and assign it to the datafile reactive value
  observe({
    df <- data.frame()
    # Check if input$file is loaded, then run import
    if (!is.null(input$file)) {
      if (endsWith(input$file$datapath, ".sav")) {
        df <- sjlabelled::read_spss(input$file$datapath, convert.factors = FALSE)
        # Atomic to factors manual conversion method
        #   Source: R/read.R in sjlabelled package
        df <- as.data.frame(lapply(df, function(x) {
          labs <- attr(x, "labels", exact = TRUE)
          lab <- attr(x, "label", exact = TRUE)
          if (is.atomic(x) && !is.null(labs) && length(labs) >= length(unique(stats::na.omit(x)))) {
            x <- as.factor(x)
            attr(x, "labels") <- labs
            if (!is.null(lab)) attr(x, "label") <- lab
          }
          x
        }), stringsAsFactors = FALSE)

        datafile$type <- "SPSS"
      } else if (endsWith(input$file$datapath, ".omv")) {
        df <- jmvReadWrite::read_omv(input$file$datapath)
        datafile$type <- "jamovi"
      } else if (endsWith(input$file$datapath, ".csv")) {
        df <- read.csv(input$file$datapath,
          header = TRUE, stringsAsFactors = TRUE
        )
        datafile$type <- "CSV"
      }
    }

    # Assign the datafile reactive value
    datafile$df <- df
  })

  # Render the data types box
  output$data_types <- renderUI({
    if (!is.null(input$file)) {
      list(
        h3("Data types"),
        DT::renderDataTable(
          DT::datatable(
            data.frame(
              variable = names(datafile$df),
              label = if (datafile$type == "SPSS") {
                sjlabelled::get_label(datafile$df)
              } else {
                # Get jmv-desc attribute if available
                sapply(datafile$df, function(x) {
                  if (is.null(attr(x, "jmv-desc"))) {
                    NA
                  } else if (datafile$type == "jamovi") {
                    attr(x, "jmv-desc")
                  }
                })
              },
              datatype = sapply(datafile$df, class),
              levels = sapply(datafile$df, function(x) {
                if (is.factor(x)) {
                  paste0(
                    if (datafile$type == "SPSS") {
                      sjlabelled::get_labels(x, attr.only = TRUE, values = "p")
                    } else if (datafile$type == "jamovi") {
                      attr(x, "levels") %>% unlist()
                    },
                    collapse = ", "
                  )
                } else {
                  NA
                }
              }),
              stringsAsFactors = FALSE
            ),
            colnames = c("Variable", "Description", "Data type", "Levels"),
            rownames = FALSE,
            options = list(
              scrollX = TRUE,
              pageLength = -1,
              dom = "t"
            )
          )
        )
      )
    } else {
      NULL
    }
  })

  # Render the warning box
  output$warning_box <- renderUI({
    if (!is.null(input$file) &&
      # There is at least one factor variable
      any(sapply(datafile$df, is.factor))) {
      list(
        br(),
        box(
          title = "Warning: factor variables detected",
          status = "warning",
          solidHeader = TRUE,
          collapsible = TRUE,
          collapsed = TRUE,
          width = 12,
          renderText(
            paste(
              "This data file contains the following factor variables:",
              paste0(names(datafile$df)[sapply(
                datafile$df,
                is.factor
              )], collapse = ", ")
            )
          ),
          br(),
          renderText(
            paste(
              "Factor variables are not suitable for certain statistical",
              "tests (e.g. mean, SD), and will be excluded from such analysis.",
              "While you might consider converting factor variables to",
              "numeric variables and re-load, caution is warranted when",
              "interpreting results of tests run on converted variables."
            )
          )
        )
      )
    } else {
      NULL
    }
  })

  # Datafile preview tab
  output$preview_tab <- renderUI({
    if (!is.null(input$file)) {
      list(
        h3("Datafile preview"),
        DT::renderDataTable({
          DT::datatable(
            if (datafile$type == "SPSS") {
              datafile$df %>%
                mutate(across(everything(), ~ {
                  lvls <- sjlabelled::get_labels(.,
                    value = "p", attr.only = FALSE
                  )
                  levels(.) <- lvls
                  .
                }))
            } else {
              datafile$df
            },
            options = list(
              scrollX = TRUE,
              pageLength = 10,
              dom = "ltipr"
            )
          )
        })
      )
    } else {
      NULL
    }
  })

  # Return the datafile reactive value
  return(datafile)
}
