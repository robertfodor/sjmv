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
        column(
          width = 12,
          h4("Settings:"),
          materialSwitch(
            inputId = ns("switch_columns_rows"),
            label = "Show variables on top",
            right = TRUE,
            status = "info"
          ),
          # verbatimTextOutput(ns("debug")),
          tableOutput(ns("descriptives")),
        )
      )
    )
  )
}

descriptive_server <- function(input, output, session, descr_df, which_analysis, digits) {
  df <- descr_df

  # Debug
  output$debug <- renderPrint({
    list()
  })

  # Analysis based on which_analysis
  switch(which_analysis,
    "descriptive_a" = {
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
            skewness = apply(df(), 2, skewness, type = 2),
            kurtosis = apply(df(), 2, kurtosis, type = 2)
          )
          colnames(descriptives) <- c(
            "N", "Missing", "Mean", "Mean SE",
            "Median", "Mode",
            "Skewness", "Kurtosis"
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
    },
    "descriptive_b" = {
      # Dispersion
      output$descriptives <- renderTable(
        { # SD, variance, range, IQR, min, max, SE
          descriptives <- data.frame(
            sd = apply(df(), 2, sd),
            var = apply(df(), 2, var),
            range = apply(df(), 2, function(x) {
              max(x) - min(x)
            }),
            iqr = apply(df(), 2, function(x) {
              quantile(x, probs = 0.75) - quantile(x, probs = 0.25)
            }),
            min = apply(df(), 2, min),
            max = apply(df(), 2, max)
          )
          colnames(descriptives) <- c(
            "SD", "Var", "Range", "IQR", "Min", "Max"
          )
          return(descriptives)
        },
        digits = digits,
        rownames = TRUE
      )
    },
    "descriptive_c" = {
      # Normality
      output$descriptives <- renderTable(
        {
          descriptives <- data.frame(
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
          colnames(descriptives) <- c(
            "Shapiro-Wilk", "Shapiro-Wilk p-value", "Normal distribution"
          )
          return(descriptives)
        },
        digits = digits,
        rownames = TRUE
      )
    }
  )
}
