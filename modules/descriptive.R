# This is the descriptive statistics tab for ShinyStat.
# It takes as input the datafile uploaded in the Getting Started tab.
library(shiny)
library(dplyr) # for data manipulation
library(ggplot2) # for plotting
library(e1071) # skewness and kurtosis
library(shinyWidgets) # for custom widgets

descriptive_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      fluidRow(
        column(
          width = 12,
          h2("Descriptive Statistics"),
          pickerInput(
            inputId = ns("var"),
            label = "Select variables:",
            multiple = TRUE,
            choices = NULL
          ),
          dropdownButton(
            h4("Select statistics:"),
            awesomeCheckboxGroup(
              inputId = ns("central_tendency"),
              label = "Central tendency",
              choices = c(
                "N", "Missing", "Mean", "Mean SE",
                "Median", "Mode"
              ),
              selected = c("N", "Mean", "Median")
            ),
            awesomeCheckboxGroup(
              inputId = ns("dispersion"),
              label = "Dispersion",
              choices = c(
                "SD", "Var", "Range", "IQR", "Min", "Max"
              ),
              selected = c("SD", "IQR", "Min", "Max")
            ),
            awesomeCheckboxGroup(
              inputId = ns("skewness_kurtosis"),
              label = "Skewness and kurtosis",
              choices = c("Skewness", "Kurtosis"),
              selected = c("Skewness", "Kurtosis")
            ),
            awesomeCheckboxGroup(
              inputId = ns("normality"),
              label = "Normality",
              choices = c("Shapiro-Wilk", "Shapiro-Wilk p-value", "Normal"),
              selected = NULL
            ),
            h4("Adjust display settings:"),
            sliderTextInput(
              inputId = ns("digits"),
              label = "Number of decimals:",
              choices = seq(2, 6, 1),
              grid = TRUE,
              selected = 2
            ),
            materialSwitch(
              inputId = ns("switch_columns_rows"),
              label = "Show variables on top",
              right = TRUE,
              status = "info",
              value = FALSE
            ),
            circle = FALSE, status = "primary",
            icon = icon("gear"), width = "200px",
            tooltip = tooltipOptions(title = "Select inputs and adjust settings")
          )
        ),
      ),
      fluidRow(
        tableOutput(ns("descriptives"))
      )
    )
  )
}

descriptive_server <- function(
    input, output, session,
    file_input, non_factor_variables) {
  # Update the choices of the pickerInput based on non_factor_variables
  observeEvent(
    non_factor_variables,
    handlerExpr = {
      updatePickerInput(
        session = session,
        inputId = "var",
        choices = non_factor_variables
      )
    }
  )

  # Descriptive statistics
  output$descriptives <- renderTable(
    { # If at least one variable is selected
      if (length(input$var) > 0) {
        # df is the subset of the datafile based on the selected variables
        df <- file_input$df %>% select(input$var)

        # Calculate the descriptives
        descriptives <- data.frame(
          n = apply(df, 2, function(x) {
            length(x) - sum(is.na(x))
          }),
          missing = apply(df, 2, function(x) {
            sum(is.na(x))
          }),
          mean = apply(df, 2, mean),
          se = apply(df, 2, function(x) {
            sqrt(var(x) / length(x))
          }),
          median = apply(df, 2, median),
          mode = apply(df, 2, function(x) {
            x[which.max(tabulate(match(x, x)))]
          }),
          sd = apply(df, 2, sd),
          var = apply(df, 2, var),
          range = apply(df, 2, function(x) {
            max(x) - min(x)
          }),
          iqr = apply(df, 2, function(x) {
            quantile(x, probs = 0.75) - quantile(x, probs = 0.25)
          }),
          min = apply(df, 2, min),
          max = apply(df, 2, max),
          shapiro_wilk = apply(df, 2, function(x) {
            shapiro.test(x)$statistic
          }),
          # If Shapiro-Wilk p-value is <.001, display "<.001" as text
          #   otherwise force 3 digits
          shapiro_wilk_p = apply(df, 2, function(x) {
            if (shapiro.test(x)$p.value < .001) {
              "<.001"
            } else {
              round(shapiro.test(x)$p.value, 3)
            }
          }),
          normal = apply(df, 2, function(x) {
            if (shapiro.test(x)$p.value < .05) {
              "FALSE"
            } else {
              "TRUE"
            }
          }),
          skewness = apply(df, 2, skewness, type = 2),
          kurtosis = apply(df, 2, kurtosis, type = 2)
        )

        # Assign column names based on list
        colnames(descriptives) <- c(
          "N", "Missing", "Mean", "Mean SE",
          "Median", "Mode", "SD", "Var",
          "Range", "IQR", "Min", "Max",
          "Shapiro-Wilk", "Shapiro-Wilk p-value", "Normal",
          "Skewness", "Kurtosis"
        )

        # Filter the table based on the checkboxes using dplyr
        subset_descriptives <- descriptives %>%
          select(
            c(
              input$central_tendency,
              input$dispersion,
              input$skewness_kurtosis,
              input$normality
            )
          )

        # If materialSwitch is on, transpose the table
        if (input$switch_columns_rows) {
          return(t(subset_descriptives))
        } else {
          return(subset_descriptives)
        }
      }
    },
    digits = input$digits,
    rownames = TRUE
  )
}
