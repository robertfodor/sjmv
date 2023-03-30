# This is the descriptive statistics tab for ShinyStat.
# It takes as input the datafile uploaded in the Getting Started tab.
library(shiny)
library(DT) # for data tables
library(dplyr) # for data manipulation
library(ggplot2) # for plotting
library(e1071) # skewness and kurtosis
library(shinyWidgets) # for custom widgets

descriptive_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      fluidRow(
        materialSwitch(
          inputId = ns("switch_columns_rows"),
          label = "Show variables on top",
          right = TRUE,
          status = "info",
          value = TRUE
        ),
        h4("Select statistics to display:"),
        column(
          width = 4,
          awesomeCheckboxGroup(
            inputId = ns("central_tendency"),
            label = "Central tendency",
            choices = c(
              "N", "Missing", "Mean", "Mean SE",
              "Median", "Mode", "Skewness", "Kurtosis"
            ),
            selected = c("N", "Mean", "Median", "Skewness", "Kurtosis")
          )
        ),
        column(
          width = 4,
          awesomeCheckboxGroup(
            inputId = ns("dispersion"),
            label = "Dispersion",
            choices = c(
              "SD", "Var", "Range", "IQR", "Min", "Max"
            ),
            selected = c("SD", "IQR", "Min", "Max")
          )
        ),
        column(
          width = 4,
          awesomeCheckboxGroup(
            inputId = ns("normality"),
            label = "Checkboxes",
            choices = c("Shapiro-Wilk", "Shapiro-Wilk p-value", "Result"),
            selected = c("Shapiro-Wilk p-value", "Result")
          )
        )
      ),
      fluidRow(
        verbatimTextOutput(ns("debug")),
        column(
          width = 12,
          tableOutput(ns("descriptives"))
        )
      )
    )
  )
}

descriptive_server <- function(input, output, session, descr_df, which_analysis, digits) {
  df <- descr_df

  # Descriptive statistics
  output$descriptives <- renderTable(
    { # N, missing, mean, median, mode, skewness, kurtosis, Shapiro-Wilk
      descriptives <- data.frame(
        n = apply(df(), 2, function(x) {
          length(x) - sum(is.na(x))
        }),
        missing = apply(df(), 2, function(x) {
          sum(is.na(x))
        }),
        mean = apply(df(), 2, mean),
        se = apply(df(), 2, function(x) {
          sqrt(var(x) / length(x))
        }),
        sd = apply(df(), 2, sd),
        var = apply(df(), 2, var),
        median = apply(df(), 2, median),
        mode = apply(df(), 2, function(x) {
          x[which.max(tabulate(match(x, x)))]
        }),
        range = apply(df(), 2, function(x) {
          max(x) - min(x)
        }),
        iqr = apply(df(), 2, function(x) {
          quantile(x, probs = 0.75) - quantile(x, probs = 0.25)
        }),
        min = apply(df(), 2, min),
        max = apply(df(), 2, max),
        skewness = apply(df(), 2, skewness, type = 2),
        kurtosis = apply(df(), 2, kurtosis, type = 2),
        shapiro_wilk = apply(df(), 2, function(x) {
          shapiro.test(x)$statistic
        }),
        shapiro_wilk_p = apply(df(), 2, function(x) {
          shapiro.test(x)$p.value
        }),
        normal = apply(df(), 2, function(x) {
          shapiro.test(x)$p.value > .05
        })
      )

      # If materialSwitch is on, transpose the table
      if (input$switch_columns_rows) {
        return(t(descriptives))
      } else {
        return(descriptives)
      }
    },
    digits = digits,
    rownames = TRUE
  )

  # Debugging
  output$debug <- renderPrint({
    list()
  })
}
