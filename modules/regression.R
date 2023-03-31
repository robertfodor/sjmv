# This is the descriptive statistics tab for ShinyStat.
# It takes as input the datafile uploaded in the Getting Started tab.
library(shiny)
library(dplyr) # for data manipulation
library(ggplot2) # for plotting
library(shinyWidgets) # for custom widgets
library(car) # vif
library(lm.beta) # standardized beta coefficients
library(psych) # needed for corr.test

# UI
regression_ui <- function(id) {
    ns <- NS(id)
    tagList(
        fluidPage(
            fluidRow(
                column(
                    width = 12,
                    h2("Regression Analysis"),
                    h3("Model Builder")
                )
            ),
            fluidRow(
                column(
                    width = 6,
                    pickerInput(
                        inputId = ns("outcome"),
                        label = "Select outcome variable:",
                        multiple = FALSE,
                        choices = NULL
                    ),
                    uiOutput(ns("predictor_blocks"))
                ),
                column(
                    width = 6,
                    dropdownButton(
                        sliderTextInput(
                            inputId = ns("blocks"),
                            label = "Number of blocks:",
                            choices = seq(1, 10, 1),
                            grid = TRUE,
                            selected = 1
                        ),
                        circle = FALSE, status = "primary",
                        icon = icon("gear"), width = "200px",
                        tooltip = tooltipOptions(title = "Adjust number of blocks")
                    )
                )
            ),
            fluidRow(
                column(
                    width = 12,
                    verbatimTextOutput(ns("debug")),
                    h3("Model Fit Measures"),
                    tableOutput(ns("model_fit_measures")),
                    h3("Model Change Measures"),
                    tableOutput(ns("model_change_measures")),
                    h3("Model Coefficients"),
                    verbatimTextOutput(ns("model_coefficients")),
                    h3("Model Plots"),
                    plotOutput(ns("model_plots"))
                )
            )
        )
    )
}

regression_server <- function(
    input, output, session,
    file_input) {
    # Create UI for predictor blocks based on number of blocks
    output$predictor_blocks <- renderUI({
        # Create a list of UI elements
        ui_list <- list()
        # session ns
        ns <- session$ns
        # Loop through each block
        for (i in 1:input$blocks) {
            # Create a UI element for each block
            ui_list[[i]] <- pickerInput(
                # use session ns for inputId naming
                inputId = ns(paste0("block_", i)),
                label = paste0("Predictor block ", i),
                choices = setdiff(names(file_input$df), input$outcome),
                multiple = TRUE
            )
        }

        # Return a list of UI elements
        return(ui_list)
    })

    # Update the choices of the pickerInput based on file_input$df
    observeEvent(file_input$df, {
        # Update the choices of the outcome pickerInput
        updatePickerInput(
            session = session,
            inputId = "outcome",
            choices = names(file_input$df)
        )
        # Update the choices of the predictor blocks
        for (i in 1:input$blocks) {
            updatePickerInput(
                session = session,
                inputId = paste0("block_", i),
                choices = setdiff(names(file_input$df), input$outcome)
            )
        }
    })

    # Debug
    output$debug <- renderPrint({
        list()
    })
}
