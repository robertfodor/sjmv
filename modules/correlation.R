# modules/correlation.R
library(shiny)
library(dplyr)
library(shinyWidgets)
library(knitr)
library(kableExtra)
library(mvShapiroTest)
library(psych)

# --- REFACTORED HELPER FUNCTIONS ---

# Function 1: Performs the statistical calculations and returns RAW results
run_correlation_stats <- function(data, vars, force_method = "auto", normality_p_threshold = 0.05) {
    if (!is.data.frame(data)) stop("'data' must be a data.frame.")
    if (!all(vars %in% names(data))) {
        missing_vars <- vars[!vars %in% names(data)]
        stop(paste("The following variables were not found:", paste(missing_vars, collapse = ", ")))
    }

    data_subset <- data %>%
        dplyr::select(all_of(vars)) %>%
        na.omit()

    if (nrow(data_subset) < 3) {
        stop("After removing cases with missing data, fewer than 3 complete rows remain.")
    }

    # Overall Multivariate Normality Test
    mv_norm_result_df <- tryCatch(
        {
            if (nrow(data_subset) > 5000) {
                data.frame(Statistic = "Shapiro-Wilk W", Value = NA, `p-value` = NA, Note = "Sample size > 5000, test skipped.", check.names = FALSE)
            } else {
                test <- mvShapiroTest::mvShapiro.Test(as.matrix(data_subset))
                data.frame(Statistic = "Shapiro-Wilk W", Value = test$statistic, `p-value` = test$p.value, Note = NA, check.names = FALSE)
            }
        },
        error = function(e) {
            data.frame(Statistic = "Shapiro-Wilk W", Value = NA, `p-value` = NA, Note = stringr::str_squish(e$message), check.names = FALSE)
        }
    )

    # Pairwise Multivariate Normality Test
    pairwise_vars <- combn(vars, 2, simplify = FALSE)
    pairwise_normality_list <- lapply(pairwise_vars, function(v) {
        test_data <- data_subset[, v, drop = FALSE]
        test_result <- suppressWarnings(mvShapiroTest::mvShapiro.Test(as.matrix(test_data)))
        data.frame(Variable_Pair = paste(v, collapse = " & "), p_value = test_result$p.value)
    })
    pairwise_normality_df <- do.call(rbind, pairwise_normality_list)

    # Select Correlation Method
    recommended_method <- ifelse(any(pairwise_normality_df$p_value < normality_p_threshold, na.rm = TRUE), "spearman", "pearson")
    cor_method <- if (force_method == "auto") recommended_method else force_method

    warning_msg <- NULL
    if (force_method != "auto" && cor_method != recommended_method) {
        warning_msg <- paste0("Warning: Based on pairwise normality tests, the recommended method is '", recommended_method, "', but you have forced the use of '", cor_method, "'.")
    }

    # Compute Correlation
    cor_results <- psych::corr.test(data_subset, method = cor_method, adjust = "none")

    # Return a list of RAW results
    return(list(
        vars = vars,
        mv_norm_raw = mv_norm_result_df,
        pairwise_norm_raw = pairwise_normality_df,
        warning_message = warning_msg,
        used_method = cor_method,
        force_method = force_method,
        r_matrix = cor_results$r,
        p_matrix = cor_results$p
    ))
}

# Function 2: Takes RAW results and formats them into kable tables
format_correlation_tables <- function(raw_results, stars, digits, p_digits) {
    # Format Normality Tables
    mv_norm_formatted <- raw_results$mv_norm_raw %>%
        mutate(
            # Use 'digits' for the statistic value
            across(any_of("Value"), ~ if (is.numeric(.x)) round(.x, digits) else .x),
            # Use 'p_digits' and special formatting for the p-value
            `p-value` = if_else(
                is.numeric(`p-value`) & `p-value` < 0.001,
                "< .001",
                as.character(round(`p-value`, p_digits))
            )
        )
    mv_norm_table <- knitr::kable(mv_norm_formatted, caption = "Overall Multivariate Normality (All Variables)") %>%
        kable_classic(full_width = FALSE)

    pairwise_norm_table <- knitr::kable(raw_results$pairwise_norm_raw,
        caption = "Pairwise Multivariate Normality Results",
        col.names = c("Variable Pair", "p-value"), digits = p_digits
    ) %>%
        kable_classic(full_width = FALSE)

    # Format Correlation Table
    vars <- raw_results$vars
    r_matrix <- raw_results$r_matrix
    p_matrix <- raw_results$p_matrix
    n_vars <- length(vars)

    formatted_table <- matrix("", nrow = n_vars, ncol = n_vars, dimnames = list(vars, vars))
    for (i in 1:n_vars) {
        for (j in 1:n_vars) {
            if (i < j) {
                r_val <- r_matrix[i, j]
                p_val <- p_matrix[i, j]
                r_str <- format(round(r_val, digits), nsmall = digits)

                if (stars) {
                    sig_stars <- case_when(p_val < 0.001 ~ "***", p_val < 0.01 ~ "**", p_val < 0.05 ~ "*", TRUE ~ "")
                    formatted_table[j, i] <- paste0(r_str, sig_stars)
                } else {
                    p_str <- if (p_val < 0.001) {
                        "< .001"
                    } else {
                        p_val_rounded_str <- format(round(p_val, p_digits), nsmall = p_digits)
                        paste0("= ", sub("0.", ".", p_val_rounded_str, fixed = TRUE))
                    }
                    formatted_table[j, i] <- paste0(r_str, ", p ", p_str)
                }
            } else if (i == j) {
                formatted_table[i, j] <- "—"
            }
        }
    }

    method_note <- if (raw_results$force_method != "auto") " (forced method)" else ""
    caption <- paste0("Correlation Matrix (Method: ", raw_results$used_method, ")", method_note)
    table_footer <- if (stars) "Note: * p < .05, ** p < .01, *** p < .001" else ""

    correlation_table <- kable(formatted_table, caption = caption) %>%
        kable_classic(full_width = FALSE) %>%
        footnote(general = table_footer, general_title = "")

    return(list(
        mv_norm_table = mv_norm_table,
        pairwise_norm_table = pairwise_norm_table,
        correlation_table = correlation_table
    ))
}


# --- SHINY UI ---
correlation_ui <- function(id) {
    ns <- NS(id)
    tagList(
        fluidPage(
            fluidRow(column(width = 12, h1("Correlation Analysis"))),
            fluidRow(
                box(
                    title = "Analysis Controls", status = "primary", solidHeader = TRUE, width = 12,
                    fluidRow(
                        column(
                            width = 6,
                            pickerInput(
                                inputId = ns("vars"), label = "Select variables (2 or more):", multiple = TRUE,
                                choices = NULL, options = pickerOptions(actionsBox = TRUE, liveSearch = TRUE)
                            )
                        ),
                        column(
                            width = 6,
                            awesomeRadio(
                                inputId = ns("method"), label = "Select method:",
                                choices = c("Auto-detect based on normality" = "auto", "Pearson" = "pearson", "Spearman" = "spearman"),
                                selected = "auto", status = "info"
                            ),
                            materialSwitch(inputId = ns("stars"), label = "Use significance stars", value = TRUE, status = "success")
                        )
                    ),
                    br(),
                    fluidRow(column(
                        width = 12, align = "center",
                        actionButton(ns("run_analysis"), "Run Correlation Analysis", icon = icon("play"), class = "btn-lg btn-success")
                    ))
                )
            ),
            fluidRow(uiOutput(ns("results_ui")))
        )
    )
}

# --- SHINY SERVER ---
correlation_server <- function(input, output, session, file_input, settings) {
    ns <- session$ns

    observeEvent(file_input$df, {
        numeric_vars <- names(file_input$df)[sapply(file_input$df, is.numeric)]
        updatePickerInput(session = session, inputId = "vars", choices = numeric_vars)
    })

    # Reactive 1: Calculation - Runs ONLY when the button is clicked
    raw_results <- eventReactive(input$run_analysis, {
        shiny::validate(
            shiny::need(length(input$vars) >= 2, "Please select at least two variables to correlate.")
        )
        showNotification("Running statistical tests...", type = "message", duration = 2)

        tryCatch(
            {
                run_correlation_stats(
                    data = file_input$df,
                    vars = input$vars,
                    force_method = input$method
                )
            },
            error = function(e) {
                showNotification(paste("An error occurred:", e$message), type = "error", duration = 10)
                return(NULL)
            }
        )
    })

    # Reactive 2: Formatting - Runs when raw_results() OR settings() change
    formatted_tables <- reactive({
        req(raw_results())

        current_settings <- settings()

        format_correlation_tables(
            raw_results = raw_results(),
            stars = input$stars,
            digits = current_settings$digits,
            p_digits = current_settings$p_digits
        )
    })

    # Render the UI container once results are available
    output$results_ui <- renderUI({
        req(raw_results())

        tagList(
            box(
                title = "Assumption Checks: Normality", status = "info", solidHeader = TRUE, width = 12, collapsible = TRUE,
                p("Multivariate normality is assessed for all variables together and for each pair. If any pairwise test fails (p < .05), a non-parametric method like Spearman's is recommended."),
                fluidRow(
                    column(width = 6, uiOutput(ns("mv_norm_table"))),
                    column(width = 6, uiOutput(ns("pairwise_norm_table")))
                )
            ),
            box(
                title = "Correlation Matrix", status = "success", solidHeader = TRUE, width = 12,
                uiOutput(ns("warning_ui")),
                uiOutput(ns("correlation_table"))
            )
        )
    })

    # Render the specific tables using the FORMATTED results
    output$mv_norm_table <- renderUI({
        HTML(formatted_tables()$mv_norm_table)
    })
    output$pairwise_norm_table <- renderUI({
        HTML(formatted_tables()$pairwise_norm_table)
    })
    output$correlation_table <- renderUI({
        HTML(formatted_tables()$correlation_table)
    })

    # Render the warning message (this comes from the raw results, as it's part of the calculation logic)
    output$warning_ui <- renderUI({
        req(raw_results())
        msg <- raw_results()$warning_message
        if (!is.null(msg)) {
            tags$div(class = "alert alert-warning", role = "alert", msg)
        }
    })
}
