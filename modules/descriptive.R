# This is the descriptive statistics tab for ShinyStat.
# It takes as input the datafile uploaded in the Getting Started tab.
library(shiny)
library(dplyr) # for data manipulation
library(ggplot2) # for plotting
library(e1071) # skewness and kurtosis
library(nortest) # Lilliefors test
library(shinyWidgets) # for custom widgets
library(knitr) # for kable
library(kableExtra) # extra formatting for kable

descriptive_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      fluidRow(column(
        width = 12,
        h2("Descriptive Statistics"),
        pickerInput(
          inputId = ns("var"),
          label = "Select variables:",
          multiple = TRUE,
          choices = NULL
        )
      )),
      fluidRow(
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
          ),
          awesomeCheckboxGroup(
            inputId = ns("skewness_kurtosis"),
            label = "Skewness and kurtosis",
            choices = c("Skewness", "Kurtosis"),
            selected = c("Skewness", "Kurtosis")
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
            selected = c("SD", "IQR")
          )
        ),
        column(
          width = 4,
          awesomeCheckboxGroup(
            inputId = ns("normality"),
            label = "Normality",
            choices = c(
              "Shapiro-Wilk",
              "Kolmogorov-Smirnov"
            ),
            selected = c("Shapiro-Wilk")
          )
        )
      ),
      fluidRow(
        column(
          width = 12,
          tableOutput(ns("descriptives")),
          h4("Normality tests"),
          tableOutput(ns("normality_table"))
        )
      )
    )
  )
}

descriptive_server <- function(
    input, output, session,
    file_input, non_factor_variables, digits) {
  # Update the choices of the pickerInput based on non_factor_variables
  observeEvent(
    non_factor_variables,
    handlerExpr = {
      updatePickerInput(
        session = session,
        inputId = "var",
        choices = non_factor_variables,
        # When digits are changed it shouldn't reset.
        selected = isolate(input$var)
      )
    }
  )

  # Workaround so digits do get updated
  ##   Should fix with a more elegant solution later
  digits <- reactiveVal(
    digits
  )

  # Data source
  df <- reactive({
    if (length(input$var) > 0) {
      df <- file_input$df %>% select(input$var)
      return(df)
    }
  })

  # Descriptives
  statistics <- reactive({
    if (length(input$var) > 0) {
      # df is the subset of the datafile based on the selected variables
      df <- df()
      digits <- digits()
      # Calculate the descriptives
      central_tendency <- data.frame(
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
        })
      ) %>%
        dplyr::mutate_if(
          is.numeric,
          ~ sprintf(paste0("%.", digits, "f"), .)
        )

      colnames(central_tendency) <- c(
        "N", "Missing", "Mean", "Mean SE",
        "Median", "Mode"
      )

      skewness_kurtosis <- data.frame(
        skewness = apply(df, 2, e1071::skewness, type = 2),
        kurtosis = apply(df, 2, e1071::kurtosis, type = 2)
      ) %>%
        dplyr::mutate_if(
          is.numeric,
          ~ sprintf(paste0("%.", digits, "f"), .)
        )

      colnames(skewness_kurtosis) <- c(
        "Skewness", "Kurtosis"
      )

      dispersion <- data.frame(
        sd = apply(df, 2, sd),
        var = apply(df, 2, var),
        range = apply(df, 2, function(x) {
          max(x) - min(x)
        }),
        iqr = apply(df, 2, function(x) {
          quantile(x, probs = 0.75) - quantile(x, probs = 0.25)
        }),
        min = apply(df, 2, min),
        max = apply(df, 2, max)
      ) %>%
        dplyr::mutate_if(
          is.numeric,
          ~ sprintf(paste0("%.", digits, "f"), .)
        )

      colnames(dispersion) <- c(
        "SD", "Var", "Range", "IQR", "Min", "Max"
      )

      normality <- data.frame(
        shapiro_wilk = apply(df, 2, function(x) {
          shapiro.test(x)$statistic
        }),
        # If Shapiro-Wilk p-value is <.001, display "<.001" as text
        #   otherwise force 3 digits
        shapiro_wilk_p = apply(df, 2, function(x) {
          if (shapiro.test(x)$p.value < .001) {
            "<.001"
          } else {
            sprintf(paste0("%.", digits, "f"), shapiro.test(x)$p.value)
          }
        }),
        ks = apply(df, 2, function(x) {
          ks.test(x, "pnorm", exact = TRUE)$statistic
        }),
        ks_p = apply(df, 2, function(x) {
          if (ks.test(x, "pnorm", exact = TRUE)$p.value < .001) {
            "<.001"
          } else {
            sprintf(paste0("%.", digits, "f"), ks.test(x, "pnorm", exact = TRUE)$p.value)
          }
        }),
        ks_lillie_p = apply(df, 2, function(x) {
          if (lillie.test(x)$p.value < .001) {
            "<.001"
          } else {
            sprintf(paste0("%.", digits, "f"), lillie.test(x)$p.value)
          }
        })
      ) %>%
        dplyr::mutate_if(
          is.numeric,
          ~ sprintf(paste0("%.", digits, "f"), .)
        )

      # Combine the tables
      statistics <- tibble(
        descr = cbind(central_tendency, dispersion, skewness_kurtosis),
        normality = normality
      )

      return(statistics)
    }
  })


  # Table outputs for descriptive statistics
  output$descriptives <- function() { # If at least one variable is selected
    if (length(input$var) > 0) {
      header_above_descr <- c(" " = 1)
      if (length(input$central_tendency) > 0) {
        header_above_descr <- c(
          header_above_descr,
          "Central tendency" = length(input$central_tendency)
        )
      }
      if (length(input$dispersion) > 0) {
        header_above_descr <- c(
          header_above_descr,
          "Dispersion" = length(input$dispersion)
        )
      }
      if (length(input$skewness_kurtosis) > 0) {
        header_above_descr <- c(
          header_above_descr,
          "Skewness and kurtosis" = length(input$skewness_kurtosis)
        )
      }

      statistics()$descr %>%
        select(all_of(
          c(input$central_tendency, input$dispersion, input$skewness_kurtosis)
        )) %>%
        # Format as table
        knitr::kable(
          "html",
          align = "c",
          row.names = TRUE,
          caption = "Descriptive statistics"
        ) %>%
        kableExtra::kable_classic(
          full_width = FALSE,
          html_font = "inherit",
          position = "left"
        ) %>%
        kableExtra::add_header_above(header_above_descr)
    }
  }

  # Table outputs for normality
  output$normality_table <- function() { # If at least one variable is selected
    if (length(input$var) > 0) {
      selections <- c()
      header_above_sw <- c()
      header_above_ks <- c()
      colname_sw <- c()
      colname_ks <- c()
      if ("Shapiro-Wilk" %in% input$normality) {
        selections <- c(selections, "shapiro_wilk", "shapiro_wilk_p")
        header_above_sw <- c("Shapiro-Wilk test" = 2)
        colname_sw <- c("W", "p-value")
      }
      if ("Kolmogorov-Smirnov" %in% input$normality) {
        selections <- c(selections, "ks", "ks_p", "ks_lillie_p")
        header_above_ks <- c("Kolmogorov-Smirnov test" = 3)
        colname_ks <- c("D", "p-value", "Lilliefors p-value")
      }

      # Filter the table based on the checkboxes using dplyr
      subset_normality <- statistics()$normality %>%
        select(all_of(selections))

      subset_normality <- subset_normality %>%
        # Format as table
        knitr::kable(
          "html",
          align = "c",
          row.names = TRUE,
          col.names = c(colname_sw, colname_ks),
          caption = "Tests for normality",
          escape = TRUE
        ) %>%
        kableExtra::kable_classic(
          full_width = FALSE,
          html_font = "inherit",
          position = "left"
        ) %>%
        kableExtra::add_header_above(
          c(
            " " = 1,
            header_above_sw,
            header_above_ks
          )
        )

      subset_normality
    }
  }
}
