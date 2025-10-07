# modules/correlation.R
library(shiny)
library(dplyr)
library(shinyWidgets)
library(knitr)
library(kableExtra)
library(mvShapiroTest)
library(psych)
library(ppcor) # For partial and semi-partial correlations
library(purrr) # For map_df

# --- REFACTORED HELPER FUNCTIONS ---

# Function 1: Performs the statistical calculations and returns RAW results
run_correlation_stats <- function(data, vars, control_vars = NULL, cor_type = "Standard", force_method = "auto", normality_p_threshold = 0.05) {
    if (!is.data.frame(data)) stop("'data' must be a data.frame.")
    if (!all(vars %in% names(data))) {
        missing_vars <- vars[!vars %in% names(data)]
        stop(paste("The following variables were not found:", paste(missing_vars, collapse = ", ")))
    }

    analysis_vars <- vars
    if (cor_type != "Standard") {
        if (is.null(control_vars) || length(control_vars) == 0) {
            stop("At least one control variable must be selected for partial or part correlations.")
        }
        if (any(vars %in% control_vars)) {
            stop("Main variables and control variables must not overlap.")
        }
        analysis_vars <- c(vars, control_vars)
    }

    data_subset <- data %>%
        dplyr::select(all_of(analysis_vars)) %>%
        na.omit()

    if (nrow(data_subset) < 4) { # Increased requirement for partial correlations
        stop("After removing cases with missing data, fewer than 4 complete rows remain, which is insufficient for this analysis.")
    }

    # --- Normality Tests ---
    mv_norm_result_df <- tryCatch(
        {
            test_data_matrix <- as.matrix(data_subset)
            if (nrow(test_data_matrix) > 5000) {
                data.frame(Statistic = "Shapiro-Wilk W", Value = NA, `p-value` = NA, Note = "Sample size > 5000, test skipped.", check.names = FALSE)
            } else {
                test <- mvShapiroTest::mvShapiro.Test(test_data_matrix)
                data.frame(Statistic = "Shapiro-Wilk W", Value = test$statistic, `p-value` = test$p.value, Note = NA, check.names = FALSE)
            }
        },
        error = function(e) {
            data.frame(Statistic = "Shapiro-Wilk W", Value = NA, `p-value` = NA, Note = stringr::str_squish(e$message), check.names = FALSE)
        }
    )

    pairwise_vars_comb <- combn(vars, 2, simplify = FALSE)
    pairwise_normality_df <- map_df(pairwise_vars_comb, function(v_pair) {
        test_data <- data_subset[, v_pair, drop = FALSE]
        test_result <- suppressWarnings(mvShapiroTest::mvShapiro.Test(as.matrix(test_data)))
        tibble(Variable_Pair = paste(v_pair, collapse = " & "), p_value = test_result$p.value)
    })

    # --- Correlation Method Selection ---
    recommended_method <- ifelse(any(pairwise_normality_df$p_value < normality_p_threshold, na.rm = TRUE), "spearman", "pearson")
    cor_method <- if (force_method == "auto") recommended_method else force_method

    warning_msg <- NULL
    if (force_method != "auto" && cor_method != recommended_method) {
        warning_msg <- paste0("Warning: Based on pairwise normality tests, the recommended method is '", recommended_method, "', but you have forced the use of '", cor_method, "'.")
    }

    # --- Correlation Computation ---
    if (cor_type == "Standard") {
        cor_results <- psych::corr.test(data_subset[, vars], method = cor_method, adjust = "none")
        r_matrix <- cor_results$r
        p_matrix <- cor_results$p
    } else { # Partial or Part
        r_matrix <- matrix(NA, nrow = length(vars), ncol = length(vars), dimnames = list(vars, vars))
        p_matrix <- matrix(NA, nrow = length(vars), ncol = length(vars), dimnames = list(vars, vars))
        diag(r_matrix) <- 1

        for (pair in pairwise_vars_comb) {
            v1 <- pair[1]
            v2 <- pair[2]

            x_vec <- data_subset[[v1]]
            y_vec <- data_subset[[v2]]
            z_df <- data_subset[, control_vars, drop = FALSE]

            test_func <- if (cor_type == "Partial") ppcor::pcor.test else ppcor::spcor.test

            result <- tryCatch(
                test_func(x = x_vec, y = y_vec, z = z_df, method = cor_method),
                error = function(e) NULL
            )

            if (!is.null(result)) {
                r_matrix[v1, v2] <- r_matrix[v2, v1] <- result$estimate
                p_matrix[v1, v2] <- p_matrix[v2, v1] <- result$p.value
            }
        }
    }

    return(list(
        vars = vars, control_vars = control_vars, cor_type = cor_type,
        mv_norm_raw = mv_norm_result_df, pairwise_norm_raw = pairwise_normality_df,
        warning_message = warning_msg, used_method = cor_method, force_method = force_method,
        r_matrix = r_matrix, p_matrix = p_matrix
    ))
}


# Function 2: Takes RAW results and formats them into kable tables
format_correlation_tables <- function(raw_results, stars, digits, p_digits) {
    # Format Normality Tables
    mv_norm_formatted <- raw_results$mv_norm_raw %>%
        mutate(
            across(any_of("Value"), ~ if (is.numeric(.x)) round(.x, digits) else .x),
            `p-value` = if_else(
                is.numeric(`p-value`) & `p-value` < 0.001,
                "< .001",
                as.character(round(`p-value`, p_digits))
            )
        )
    mv_norm_table <- knitr::kable(mv_norm_formatted, caption = "Overall Multivariate Normality (Incl. Control Variables)") %>%
        kable_classic(full_width = FALSE)

    pairwise_norm_table <- knitr::kable(raw_results$pairwise_norm_raw,
        caption = "Pairwise Multivariate Normality (Main Variables)",
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
    cor_type_label <- raw_results$cor_type
    if (cor_type_label == "Part (Semi-partial)") cor_type_label <- "Semi-partial"

    caption_main <- paste0(cor_type_label, " Correlation Matrix (Method: ", raw_results$used_method, ")", method_note)

    if (raw_results$cor_type != "Standard") {
        caption_main <- paste0(caption_main, "<br><small>Controlling for: ", paste(raw_results$control_vars, collapse = ", "), "</small>")
    }

    table_footer <- if (stars) "Note: * p < .05, ** p < .01, *** p < .001" else ""

    correlation_table <- kable(formatted_table, caption = caption_main, escape = FALSE) %>%
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
                    title = "Standard Correlation", status = "primary", solidHeader = TRUE, width = 12,
                    fluidRow(
                        column(
                            width = 6,
                            pickerInput(
                                inputId = ns("vars_std"), label = "Select variables (2 or more):", multiple = TRUE,
                                choices = NULL, options = pickerOptions(actionsBox = TRUE, liveSearch = TRUE)
                            )
                        ),
                        column(
                            width = 3,
                            awesomeRadio(
                                inputId = ns("method_std"), label = "Select method:",
                                choices = c("Auto-detect" = "auto", "Pearson" = "pearson", "Spearman" = "spearman"),
                                selected = "auto", status = "info"
                            )
                        ),
                        column(
                            width = 3,
                            div(
                                style = "margin-top: 25px;",
                                materialSwitch(inputId = ns("stars_std"), label = "Use significance stars", value = TRUE, status = "success")
                            )
                        )
                    ),
                    fluidRow(column(width = 12, align = "center", actionButton(ns("run_std"), "Run Standard Correlation", icon = icon("play")))),
                    br(),
                    uiOutput(ns("std_results_ui"))
                )
            ),
            fluidRow(
                box(
                    title = "Partial & Part Correlation", status = "info", solidHeader = TRUE, width = 12,
                    fluidRow(
                        column(
                            width = 4,
                            pickerInput(
                                inputId = ns("vars_pp"), label = "Variables to correlate:", multiple = TRUE,
                                choices = NULL, options = pickerOptions(actionsBox = TRUE, liveSearch = TRUE)
                            )
                        ),
                        column(
                            width = 4,
                            pickerInput(
                                inputId = ns("control_vars_pp"), label = "Variables to control for:", multiple = TRUE,
                                choices = NULL, options = pickerOptions(actionsBox = TRUE, liveSearch = TRUE)
                            )
                        ),
                        column(
                            width = 4,
                            awesomeRadio(inputId = ns("cor_type_pp"), label = "Correlation Type:", choices = c("Partial", "Part (Semi-partial)"), selected = "Partial", status = "info"),
                        )
                    ),
                    hr(),
                    fluidRow(
                        column(
                            width = 4,
                            awesomeRadio(
                                inputId = ns("method_pp"), label = "Select method:", choices = c("Pearson" = "pearson", "Spearman" = "spearman"),
                                selected = "pearson", status = "info"
                            )
                        ),
                        column(
                            width = 4,
                            div(
                                style = "margin-top: 25px;",
                                materialSwitch(inputId = ns("stars_pp"), label = "Use significance stars", value = TRUE, status = "success")
                            )
                        )
                    ),
                    br(),
                    fluidRow(column(width = 12, align = "center", actionButton(ns("run_pp"), "Run Partial/Part Correlation", icon = icon("play")))),
                    br(),
                    uiOutput(ns("pp_results_ui"))
                )
            )
        )
    )
}

# --- SHINY SERVER ---
correlation_server <- function(input, output, session, file_input, settings) {
    ns <- session$ns
    std_results_rv <- reactiveVal(NULL)
    pp_results_rv <- reactiveVal(NULL)
    numeric_vars <- reactiveVal(NULL)

    observeEvent(file_input$df, {
        vars <- names(file_input$df)[sapply(file_input$df, is.numeric)]
        numeric_vars(vars)
        updatePickerInput(session, "vars_std", choices = vars)
        updatePickerInput(session, "vars_pp", choices = vars)
        updatePickerInput(session, "control_vars_pp", choices = vars)
    })

    # --- Dynamic Picker Updates to prevent overlap for partial/part ---
    observe({
        req(input$vars_pp)
        remaining <- setdiff(numeric_vars(), input$vars_pp)
        updatePickerInput(session, "control_vars_pp", choices = remaining, selected = isolate(intersect(input$control_vars_pp, remaining)))
    })
    observe({
        req(input$control_vars_pp)
        remaining <- setdiff(numeric_vars(), input$control_vars_pp)
        updatePickerInput(session, "vars_pp", choices = remaining, selected = isolate(intersect(input$vars_pp, remaining)))
    })

    # --- Run Buttons Logic ---
    observeEvent(input$run_std, {
        shiny::validate(shiny::need(length(input$vars_std) >= 2, "Please select at least two variables for standard correlation."))
        showNotification("Running standard correlation...", type = "message", duration = 2)

        res <- tryCatch(
            run_correlation_stats(
                data = file_input$df, vars = input$vars_std, cor_type = "Standard",
                force_method = input$method_std
            ),
            error = function(e) {
                showNotification(paste("An error occurred:", e$message), type = "error", duration = 10)
                return(NULL)
            }
        )
        std_results_rv(res)
    })

    observeEvent(input$run_pp, {
        shiny::validate(
            shiny::need(length(input$vars_pp) >= 2, "Please select at least two variables to correlate."),
            shiny::need(length(input$control_vars_pp) >= 1, "Please select at least one control variable.")
        )
        showNotification("Running partial/part correlation...", type = "message", duration = 2)

        res <- tryCatch(
            run_correlation_stats(
                data = file_input$df, vars = input$vars_pp, control_vars = input$control_vars_pp,
                cor_type = input$cor_type_pp, force_method = input$method_pp
            ),
            error = function(e) {
                showNotification(paste("An error occurred:", e$message), type = "error", duration = 10)
                return(NULL)
            }
        )
        pp_results_rv(res)
    })

    # --- Formatting Reactives ---
    formatted_std_tables <- reactive({
        req(std_results_rv())
        format_correlation_tables(
            raw_results = std_results_rv(), stars = input$stars_std,
            digits = settings()$digits, p_digits = settings()$p_digits
        )
    })

    formatted_pp_tables <- reactive({
        req(pp_results_rv())
        format_correlation_tables(
            raw_results = pp_results_rv(), stars = input$stars_pp,
            digits = settings()$digits, p_digits = settings()$p_digits
        )
    })

    # --- UI Rendering for STANDARD correlations ---
    output$std_results_ui <- renderUI({
        req(std_results_rv())
        tagList(
            box(
                title = "Standard Correlation Results", status = "success", solidHeader = FALSE, width = 12,
                collapsible = TRUE,
                uiOutput(ns("std_warning_ui")),
                uiOutput(ns("std_correlation_table")),
                br(),
                h4("Assumption Checks"),
                fluidRow(
                    column(width = 6, uiOutput(ns("std_mv_norm_table"))),
                    column(width = 6, uiOutput(ns("std_pairwise_norm_table")))
                )
            )
        )
    })
    output$std_warning_ui <- renderUI({
        req(std_results_rv())
        msg <- std_results_rv()$warning_message
        if (!is.null(msg)) {
            tags$div(class = "alert alert-warning", role = "alert", msg)
        }
    })
    output$std_correlation_table <- renderUI({
        HTML(formatted_std_tables()$correlation_table)
    })
    output$std_mv_norm_table <- renderUI({
        HTML(formatted_std_tables()$mv_norm_table)
    })
    output$std_pairwise_norm_table <- renderUI({
        HTML(formatted_std_tables()$pairwise_norm_table)
    })


    # --- UI Rendering for PARTIAL/PART correlations ---
    output$pp_results_ui <- renderUI({
        req(pp_results_rv())
        tagList(
            box(
                title = "Partial/Part Correlation Results", status = "success", solidHeader = FALSE, width = 12,
                collapsible = TRUE,
                uiOutput(ns("pp_warning_ui")),
                uiOutput(ns("pp_correlation_table")),
                br(),
                h4("Assumption Checks"),
                fluidRow(
                    column(width = 6, uiOutput(ns("pp_mv_norm_table"))),
                    column(width = 6, uiOutput(ns("pp_pairwise_norm_table")))
                )
            )
        )
    })
    output$pp_warning_ui <- renderUI({
        req(pp_results_rv())
        msg <- pp_results_rv()$warning_message
        if (!is.null(msg)) {
            tags$div(class = "alert alert-warning", role = "alert", msg)
        }
    })
    output$pp_correlation_table <- renderUI({
        HTML(formatted_pp_tables()$correlation_table)
    })
    output$pp_mv_norm_table <- renderUI({
        HTML(formatted_pp_tables()$mv_norm_table)
    })
    output$pp_pairwise_norm_table <- renderUI({
        HTML(formatted_pp_tables()$pairwise_norm_table)
    })
}
