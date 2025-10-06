# modules/descriptive.R
library(shiny)
library(tidyverse)
library(shinyWidgets)
library(knitr)
library(kableExtra)
library(e1071)
library(nortest)
library(DescTools)

source("R/descriptive_helpers.R")
# UI Definition remains the same...
descriptive_ui <- function(id) {
    ns <- NS(id)
    tagList(
        fluidPage(
            fluidRow(
                column(
                    width = 12,
                    h2("Descriptive Statistics"),
                    pickerInput(
                        inputId = ns("vars"), label = "Select variables for analysis:", multiple = TRUE, choices = NULL,
                        options = pickerOptions(actionsBox = TRUE, liveSearch = TRUE)
                    )
                )
            ),
            h3("Statistics to Show"),
            fluidRow(
                column(
                    width = 3,
                    awesomeCheckboxGroup(inputId = ns("sample"), label = "Sample", choices = c("N", "Missing"), selected = c("N")),
                    awesomeCheckboxGroup(
                        inputId = ns("central_tendency"), label = "Central Tendency",
                        choices = c("Mean", "Median", "Mode"), selected = c("Mean")
                    )
                ),
                column(
                    width = 3,
                    awesomeCheckboxGroup(
                        inputId = ns("dispersion"), label = "Dispersion",
                        choices = c("SD", "Variance", "Range", "IQR", "Min", "Max", "Quartiles"), selected = c("SD", "IQR")
                    ),
                    awesomeCheckboxGroup(
                        inputId = ns("shape"), label = "Shape",
                        choices = c("Skewness", "Kurtosis"), selected = c("Skewness", "Kurtosis")
                    )
                ),
                column(
                    width = 3,
                    awesomeCheckboxGroup(
                        inputId = ns("normality"), label = "Normality Tests",
                        choices = c("Shapiro-Wilk"), selected = c("Shapiro-Wilk")
                    ),
                    awesomeCheckboxGroup(inputId = ns("covariance"), label = "Matrix", choices = c("Covariance Matrix"))
                ),
                column(
                    width = 3,
                    materialSwitch(inputId = ns("variables_across"), label = "Transpose Table (Variables Across)", value = TRUE, status = "info")
                )
            ),
            hr(),
            h3("Confidence Intervals"),
            fluidRow(
                column(
                    width = 3,
                    numericInput(inputId = ns("ci_level"), label = "Confidence Level (%)", value = 95, min = 1, max = 99.9, step = 0.1),
                    awesomeCheckboxGroup(inputId = ns("ci_stats"), label = "Calculate CI for:", choices = c("Mean CI", "SD CI", "Variance CI"))
                ),
                column(
                    width = 3,
                    conditionalPanel(
                        condition = "input.ci_stats.indexOf('Mean CI') > -1", ns = ns,
                        radioButtons(
                            inputId = ns("ci_mean_method"), label = "Mean CI Method:",
                            choices = c("t-distribution" = "t", "Normal distribution" = "normal", "Bootstrap" = "boot")
                        )
                    )
                ),
                column(
                    width = 3,
                    conditionalPanel(
                        condition = "input.ci_stats.indexOf('SD CI') > -1 || input.ci_stats.indexOf('Variance CI') > -1", ns = ns,
                        radioButtons(
                            inputId = ns("ci_var_method"), label = "SD/Variance CI Method:",
                            choices = c("Chi-square" = "chisq", "Bootstrap" = "boot")
                        )
                    )
                ),
                column(
                    width = 3,
                    conditionalPanel(
                        condition = "(input.ci_stats.indexOf('Mean CI') > -1 && input.ci_mean_method == 'boot') || ((input.ci_stats.indexOf('SD CI') > -1 || input.ci_stats.indexOf('Variance CI') > -1) && input.ci_var_method == 'boot')", ns = ns,
                        numericInput(inputId = ns("boot_reps"), label = "Bootstrap Replications (R)", value = 1000, min = 100, max = 10000)
                    )
                )
            ),
            hr(),
            h3("Results"),
            uiOutput(ns("results_ui_tables"))
        )
    )
}

# --- SERVER LOGIC ---
descriptive_server <- function(input, output, session,
                               file_input, non_factor_variables,
                               settings) {
    # FIX: observeEvent now triggers when the value of non_factor_variables() changes
    observeEvent(non_factor_variables(), {
        # FIX: Call the reactive to get its value
        choices_list <- as.list(setNames(non_factor_variables(), non_factor_variables()))
        updatePickerInput(session = session, inputId = "vars", choices = choices_list, selected = isolate(input$vars))
    })

    df_analysis <- reactive({
        shiny::validate(shiny::need(length(input$vars) > 0, "Please select at least one variable for analysis."))
        # FIX: Call the reactive to get its value
        req(all(input$vars %in% non_factor_variables()))
        file_input$df %>% select(all_of(input$vars))
    })

    # The rest of the server function remains the same
    results_data <- reactive({
        df <- df_analysis()
        vars <- input$vars
        ci_level <- input$ci_level / 100
        current_settings <- settings()

        statistics_tibble <- tribble(
            ~id, ~group, ~label, ~func, ~is_p_value,
            "N", "Sample", "N", ~ sum(!is.na(.x)), FALSE,
            "Missing", "Sample", "Missing", ~ sum(is.na(.x)), FALSE,
            "Mean", "Central Tendency", "Mean", ~ mean(.x, na.rm = TRUE), FALSE,
            "Median", "Central Tendency", "Median", ~ median(.x, na.rm = TRUE), FALSE,
            "Mode", "Central Tendency", "Mode", ~ calculate_mode(.x), FALSE,
            "SD", "Dispersion", "SD", ~ sd(.x, na.rm = TRUE), FALSE,
            "Variance", "Dispersion", "Variance", ~ var(.x, na.rm = TRUE), FALSE,
            "Min", "Dispersion", "Min", ~ min(.x, na.rm = TRUE), FALSE,
            "Max", "Dispersion", "Max", ~ max(.x, na.rm = TRUE), FALSE,
            "Range", "Dispersion", "Range", ~ diff(range(.x, na.rm = TRUE)), FALSE,
            "IQR", "Dispersion", "IQR", ~ IQR(.x, na.rm = TRUE), FALSE,
            "Q1", "Dispersion", "Q1 (25th percentile)", ~ stats::quantile(.x, probs = 0.25, na.rm = TRUE), FALSE,
            "Q2", "Dispersion", "Q2 (50th percentile)", ~ stats::quantile(.x, probs = 0.50, na.rm = TRUE), FALSE,
            "Q3", "Dispersion", "Q3 (75th percentile)", ~ stats::quantile(.x, probs = 0.75, na.rm = TRUE), FALSE,
            "Skewness", "Shape", "Skewness", ~ e1071::skewness(.x, na.rm = TRUE, type = 2), FALSE,
            "Kurtosis", "Shape", "Kurtosis", ~ e1071::kurtosis(.x, na.rm = TRUE, type = 2), FALSE,
            "Shapiro-W", "Normality Tests", "Shapiro-Wilk W", ~ shapiro.test(.x)$statistic, FALSE,
            "Shapiro-p", "Normality Tests", "Shapiro-Wilk p", ~ shapiro.test(.x)$p.value, TRUE
        )

        stats_to_run <- c(
            input$sample,
            input$central_tendency,
            input$dispersion,
            input$shape,
            if ("Quartiles" %in% input$dispersion) c("Q1", "Q2", "Q3") else NULL,
            if ("Shapiro-Wilk" %in% input$normality) c("Shapiro-W", "Shapiro-p") else NULL
        )

        filtered_stats <- statistics_tibble %>% filter(id %in% stats_to_run)

        results <- vars %>%
            set_names() %>%
            map_df(~ {
                x <- df[[.x]]
                stats_for_var <- filtered_stats
                if ((length(na.omit(x)) < 3 || length(na.omit(x)) > 5000) && "Shapiro-W" %in% filtered_stats$id) {
                    stats_for_var <- filtered_stats %>% filter(!id %in% c("Shapiro-W", "Shapiro-p"))
                }
                if (nrow(stats_for_var) == 0) {
                    return(NULL)
                }
                stats_for_var %>%
                    mutate(Value = map_dbl(func, ~ purrr::as_mapper(.x)(x))) %>%
                    select(group, label, Value, is_p_value)
            }, .id = "Variable")

        ci_results <- list()
        if (length(input$ci_stats) > 0) {
            for (v in vars) {
                x <- df[[v]]
                if ("Mean CI" %in% input$ci_stats) {
                    ci <- switch(input$ci_mean_method,
                        "t" = ci_mean_t(x, ci_level),
                        "normal" = ci_mean_normal(x, ci_level),
                        "boot" = ci_bootstrap(x, mean, ci_level, input$boot_reps)
                    )
                    ci_results <- c(ci_results, list(
                        tibble(Variable = v, group = "Central Tendency", label = paste0(input$ci_level, "% CI Lower (Mean)"), Value = ci[1], is_p_value = F),
                        tibble(Variable = v, group = "Central Tendency", label = paste0(input$ci_level, "% CI Upper (Mean)"), Value = ci[2], is_p_value = F)
                    ))
                }
                if ("SD CI" %in% input$ci_stats) {
                    ci <- switch(input$ci_var_method,
                        "chisq" = ci_sd_chisq(x, ci_level),
                        "boot" = ci_bootstrap(x, sd, ci_level, input$boot_reps)
                    )
                    ci_results <- c(ci_results, list(
                        tibble(Variable = v, group = "Dispersion", label = paste0(input$ci_level, "% CI Lower (SD)"), Value = ci[1], is_p_value = F),
                        tibble(Variable = v, group = "Dispersion", label = paste0(input$ci_level, "% CI Upper (SD)"), Value = ci[2], is_p_value = F)
                    ))
                }
                if ("Variance CI" %in% input$ci_stats) {
                    ci <- switch(input$ci_var_method,
                        "chisq" = ci_var_chisq(x, ci_level),
                        "boot" = ci_bootstrap(x, var, ci_level, input$boot_reps)
                    )
                    ci_results <- c(ci_results, list(
                        tibble(Variable = v, group = "Dispersion", label = paste0(input$ci_level, "% CI Lower (Var)"), Value = ci[1], is_p_value = F),
                        tibble(Variable = v, group = "Dispersion", label = paste0(input$ci_level, "% CI Upper (Var)"), Value = ci[2], is_p_value = F)
                    ))
                }
            }
        }

        final_results <- bind_rows(results, bind_rows(ci_results))

        stat_order <- c(
            "N", "Missing",
            "Mean", paste0(input$ci_level, "% CI Lower (Mean)"), paste0(input$ci_level, "% CI Upper (Mean)"),
            "Median", "Mode",
            "SD", paste0(input$ci_level, "% CI Lower (SD)"), paste0(input$ci_level, "% CI Upper (SD)"),
            "Variance", paste0(input$ci_level, "% CI Lower (Var)"), paste0(input$ci_level, "% CI Upper (Var)"),
            "Min", "Max", "Range", "IQR",
            "Q1 (25th percentile)", "Q2 (50th percentile)", "Q3 (75th percentile)",
            "Skewness", "Kurtosis",
            "Shapiro-Wilk W", "Shapiro-Wilk p"
        )

        final_results %>%
            mutate(label = factor(label, levels = stat_order)) %>%
            arrange(Variable, label) %>%
            filter(!is.na(label)) %>%
            mutate(
                Value_Formatted = case_when(
                    is.na(Value) ~ "-",
                    is_p_value & Value < 0.001 ~ "< .001",
                    is_p_value ~ as.character(round(Value, current_settings$p_digits)),
                    !is.numeric(Value) ~ as.character(Value),
                    TRUE ~ format(round(Value, current_settings$digits), nsmall = current_settings$digits)
                )
            )
    })

    output$results_ui_tables <- renderUI({
        ns <- session$ns
        shiny::validate(shiny::need(length(input$vars) > 0, "Please select at least one variable for analysis."))
        tagList(
            h4("Descriptive Statistics"),
            uiOutput(ns("descriptives_table")),
            if ("Covariance Matrix" %in% input$covariance) {
                tagList(h4("Covariance Matrix"), tableOutput(ns("covariance_matrix_table")))
            }
        )
    })

    output$descriptives_table <- renderUI({
        req(results_data(), nrow(results_data()) > 0)
        table_data <- results_data()

        k_table <- if (input$variables_across) {
            table_data_wide <- table_data %>%
                select(Variable, group, label, Value_Formatted) %>%
                pivot_wider(names_from = Variable, values_from = Value_Formatted)

            group_index <- table(factor(table_data_wide$group, levels = unique(table_data_wide$group)))

            table_data_wide %>%
                select(-group) %>%
                knitr::kable("html", escape = FALSE, align = c("l", rep("c", length(input$vars))), col.names = c("Statistic", input$vars)) %>%
                kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE) %>%
                group_rows(index = group_index)
        } else {
            table_data_wide <- table_data %>%
                select(Variable, label, Value_Formatted) %>%
                pivot_wider(names_from = label, values_from = Value_Formatted)

            stat_labels <- colnames(table_data_wide)[-1]
            group_lookup <- table_data %>%
                distinct(label, group) %>%
                arrange(label)
            ordered_groups <- left_join(tibble(label = as.factor(stat_labels)), group_lookup, by = "label")$group
            header_rle <- rle(as.character(ordered_groups))
            header_vec <- setNames(header_rle$lengths, header_rle$values)
            final_header <- c(" " = 1, header_vec)

            table_data_wide %>%
                knitr::kable("html", escape = FALSE) %>%
                kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE) %>%
                add_header_above(final_header)
        }

        HTML(k_table)
    })

    output$covariance_matrix_table <- renderTable(
        {
            req("Covariance Matrix" %in% input$covariance, length(input$vars) >= 2)
            cov(df_analysis(), use = "pairwise.complete.obs")
        },
        rownames = TRUE,
        digits = reactive(settings()$digits)
    )
}
