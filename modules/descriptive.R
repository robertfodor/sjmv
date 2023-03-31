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
        h4("Select statistics to display:"),
        column(
          width = 4,
          awesomeCheckboxGroup(
            inputId = ns("central_tendency"),
            label = "Central tendency",
            choices = c(
              "N", "Missing", "Mean", "Mean SE",
              "Median", "Mode"
            ),
            selected = c("N", "Mean", "Median")
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
          )
        ),
        materialSwitch(
          inputId = ns("switch_columns_rows"),
          label = "Show variables on top",
          right = TRUE,
          status = "info",
          value = FALSE
        )
      ),
      fluidRow(
        # verbatimTextOutput(ns("debug")), # left here for future debugging
        column(
          width = 12,
          tableOutput(ns("descriptives"))
        )
      )
    )
  )
}

descriptive_server <- function(
    input, output, session,
    descr_df, which_analysis, digits) {
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
        median = apply(df(), 2, median),
        mode = apply(df(), 2, function(x) {
          x[which.max(tabulate(match(x, x)))]
        }),
        sd = apply(df(), 2, sd),
        var = apply(df(), 2, var),
        range = apply(df(), 2, function(x) {
          max(x) - min(x)
        }),
        iqr = apply(df(), 2, function(x) {
          quantile(x, probs = 0.75) - quantile(x, probs = 0.25)
        }),
        min = apply(df(), 2, min),
        max = apply(df(), 2, max),
        shapiro_wilk = apply(df(), 2, function(x) {
          shapiro.test(x)$statistic
        }),
        # If Shapiro-Wilk p-value is <.001, display "<.001" as text
        #   otherwise force 3 digits
        shapiro_wilk_p = apply(df(), 2, function(x) {
          if (shapiro.test(x)$p.value < .001) {
            "<.001"
          } else {
            round(shapiro.test(x)$p.value, 3)
          }
        }),
        normal = apply(df(), 2, function(x) {
          if (shapiro.test(x)$p.value < .05) {
            "FALSE"
          } else {
            "TRUE"
          }
        }),
        skewness = apply(df(), 2, skewness, type = 2),
        kurtosis = apply(df(), 2, kurtosis, type = 2)
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
    },
    digits = digits,
    rownames = TRUE
  )

  # Debugging
  output$debug <- renderPrint({
    list()
  })
}
