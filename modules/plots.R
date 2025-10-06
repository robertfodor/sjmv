# modules/plots.R
library(shiny)
library(shinyWidgets)
library(ggplot2)
library(dplyr)

# Source helper files needed for plotting
source("R/plots.R")
source("R/themes.R")

# --- UI DEFINITION ---
plots_ui <- function(id) {
    ns <- NS(id)
    tagList(
        fluidPage(
            h1("Plots"),
            fluidRow(
                box(
                    title = "Plot Controls", status = "primary", solidHeader = TRUE, width = 12,
                    fluidRow(
                        # --- Column 1: Variable Selection ---
                        column(
                            width = 4,
                            pickerInput(inputId = ns("plot_var_y"), label = "Variable (Y-axis or Primary):", choices = NULL, multiple = FALSE),
                            conditionalPanel(
                                "input.plot_type == 'Scatter Plot'",
                                ns = ns,
                                pickerInput(inputId = ns("plot_var_x"), label = "Variable (X-axis):", choices = NULL, multiple = FALSE)
                            ),
                            pickerInput(inputId = ns("split_by_var"), label = "Split by (Factor):", choices = NULL, multiple = FALSE, options = pickerOptions(noneSelectedText = "None"))
                        ),
                        # --- Column 2: Plot Type & Canvas ---
                        column(
                            width = 4,
                            selectInput(
                                inputId = ns("plot_type"), label = "Select Plot Type:",
                                choices = c("Box Plot", "Histogram", "Density", "Histogram + Density", "Q-Q Plot", "Scatter Plot", "Bar Plot")
                            ),
                            hr(),
                            h4("Canvas Settings"),
                            numericInput(inputId = ns("plot_height"), label = "Plot Height (px):", value = 500, min = 200, max = 2000),
                            sliderInput(inputId = ns("plot_width"), label = "Plot Width (%):", min = 20, max = 100, value = 100, step = 5)
                        ),
                        # --- Column 3: Plot-Specific Options ---
                        column(
                            width = 4,
                            h4("Plot Options"),
                            conditionalPanel(
                                condition = "input.plot_type == 'Box Plot'", ns = ns,
                                materialSwitch(inputId = ns("plot_box_mean"), label = "Show mean", value = FALSE, status = "info"),
                                materialSwitch(inputId = ns("plot_box_points"), label = "Show data points", value = FALSE, status = "primary"),
                                materialSwitch(inputId = ns("plot_box_outliers"), label = "Label Outliers with Row ID", value = TRUE, status = "success")
                            ),
                            conditionalPanel(
                                condition = "input.plot_type == 'Histogram' || input.plot_type == 'Histogram + Density'", ns = ns,
                                radioButtons(inputId = ns("hist_bins_type"), label = "Bin Calculation:", choices = c("Sturges", "Manual")),
                                conditionalPanel(
                                    condition = "input.hist_bins_type == 'Manual'", ns = ns,
                                    numericInput(ns("hist_bins_manual"), "Number of Bins:", 30, min = 1)
                                )
                            )
                        )
                    ),
                    hr(),
                    fluidRow(
                        column(
                            width = 12, align = "center",
                            actionButton(ns("generate_plot"), "Generate Plot", icon = icon("play"), class = "btn-lg btn-primary")
                        )
                    )
                )
            ),
            fluidRow(
                column(
                    width = 12,
                    uiOutput(ns("plot_container_ui"))
                )
            )
        )
    )
}

# --- SERVER LOGIC ---
plots_server <- function(input, output, session,
                         file_input, non_factor_variables) {
    observeEvent(file_input$df, {
        df <- file_input$df
        all_vars <- names(df)
        factor_vars <- names(df)[sapply(df, is.factor)]

        all_choices_list <- as.list(setNames(all_vars, all_vars))
        factor_choices_list <- as.list(setNames(factor_vars, factor_vars))

        updatePickerInput(session = session, inputId = "plot_var_y", choices = all_choices_list)
        updatePickerInput(session = session, inputId = "plot_var_x", choices = all_choices_list)
        updatePickerInput(session = session, inputId = "split_by_var", choices = factor_choices_list, selected = character(0))
    })

    plot_data_full <- reactive({
        req(file_input$df)
        file_input$df %>%
            tibble::rownames_to_column("original_row_id")
    })

    output$plot_container_ui <- renderUI({
        ns <- session$ns
        plotOutput(ns("descriptive_plot"),
            height = paste0(input$plot_height, "px"),
            width = paste0(input$plot_width, "%")
        )
    })

    plot_object <- eventReactive(input$generate_plot, {
        shiny::validate(
            shiny::need(input$plot_var_y, "Please select a primary variable.")
        )

        generate_custom_plot(
            data = plot_data_full(),
            plot_type = input$plot_type,
            y_var = input$plot_var_y,
            x_var = input$plot_var_x,
            split_var = input$split_by_var,
            # Pass new and existing options
            show_mean = input$plot_box_mean,
            show_points = input$plot_box_points,
            label_outliers = input$plot_box_outliers,
            bins_type = input$hist_bins_type,
            bins_manual = input$hist_bins_manual
        )
    })

    output$descriptive_plot <- renderPlot({
        plot_object()
    })
}
