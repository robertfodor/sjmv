# modules/regression.R
library(shiny)
library(dplyr) # for data manipulation
library(tibble) # for rownames_to_column
library(gridExtra) # for grid.arrange
library(shinyWidgets) # for custom widgets
library(olsrr) # vif, tolerance
library(lm.beta) # for standardized coefficients
library(knitr) # for kable
library(kableExtra) # extra formatting for kable
library(ggplot2) # for plotting diagnostics
library(car) # for durbinWatsonTest

# UI
regression_ui <- function(id) {
    ns <- NS(id)
    tagList(
        withMathJax(),
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
                    sliderTextInput(
                        inputId = ns("blocks"),
                        label = "Number of predictor blocks:",
                        choices = seq(1, 10, 1),
                        grid = TRUE,
                        selected = 1
                    ),
                    materialSwitch(
                        inputId = ns("contrasts"),
                        label = "Numeric factors are categorical variables",
                        value = TRUE
                    ),
                    box(
                        title = "Factor variables",
                        status = "warning",
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        collapsed = TRUE,
                        width = "100%",
                        tagList(
                            HTML("<p>If you have factor variables that are coded
                            as numbers in the source data, the linear model
                            will use the contrast method. However, if you turn
                            the <i>Numeric factors are categorical variables</i>
                            switch off, the model will treat all factors as
                            continuous variables. This will give different
                            results for several statistics: R-squared, F test,
                            t test, and model coefficients. The results will
                            need to be interpreted very differently,
                            so please be very careful!</p>")
                        )
                    )
                )
            ),
            fluidRow(
                column(
                    width = 12,
                    h3("Model Fit Measures"),
                    tableOutput(ns("model_fit_measures")),
                    h3("Model Change Measures"),
                    tableOutput(ns("model_change_measures")),
                    h3("Model Coefficients"),
                    uiOutput(ns("model_coefficients_ui")),
                    h3("Model Diagnostics"),
                    fluidRow(
                        box(
                            title = "Advanced Diagnostic Controls",
                            status = "primary",
                            solidHeader = TRUE,
                            width = 12,
                            collapsible = TRUE,
                            collapsed = FALSE,
                            fluidRow(
                                column(
                                    width = 4,
                                    checkboxGroupInput(
                                        inputId = ns("diagnostic_checks"),
                                        label = "Select diagnostics to run:",
                                        choices = c(
                                            "Cook's Distance" = "cook",
                                            "DFBETA" = "dfbeta",
                                            "Leverage" = "leverage",
                                            "Mahalanobis Distance" = "mahalanobis"
                                        ),
                                        selected = c("cook", "leverage")
                                    )
                                ),
                                column(
                                    width = 4,
                                    materialSwitch(
                                        inputId = ns("show_plots"),
                                        label = "Generate diagnostic plots",
                                        value = TRUE,
                                        status = "success"
                                    ),
                                    actionButton(
                                        inputId = ns("run_diagnostics"),
                                        label = "Run Advanced Diagnostics",
                                        icon = icon("play-circle"),
                                        class = "btn-primary"
                                    )
                                )
                            )
                        )
                    ),
                    fluidRow(
                        column(
                            width = 12,
                            uiOutput(ns("diagnostic_output_ui"))
                        )
                    )
                )
            )
        )
    )
}

regression_server <- function(
    input, output, session,
    file_input, settings) {
    output$predictor_blocks <- renderUI({
        ns <- session$ns
        lapply(1:input$blocks, function(i) {
            pickerInput(
                inputId = ns(paste0("block_", i)),
                label = paste0("Predictor block ", i),
                choices = setdiff(names(file_input$df), input$outcome),
                selected = isolate(input[[ns(paste0("block_", i))]]),
                multiple = TRUE
            )
        })
    })

    predictors <- reactive(lapply(1:input$blocks, function(i) input[[paste0("block_", i)]]))

    df <- reactive({
        req(input$outcome, unlist(predictors()))
        vars_to_select <- c(input$outcome, unlist(predictors()))
        df_selected <- file_input$df[, vars_to_select, drop = FALSE]

        if (!isTRUE(input$contrasts)) {
            df_selected <- df_selected %>% mutate_if(is.factor, as.numeric)
        }
        df_selected
    })

    observeEvent(file_input$df, {
        updatePickerInput(session = session, inputId = "outcome", choices = names(file_input$df), selected = isolate(input$outcome))
    })

    empty_predictors <- reactive(!any(sapply(predictors(), function(p) length(p) > 0)))

    models <- reactive({
        shiny::validate(
            shiny::need(input$outcome, "Please select an outcome variable."),
            shiny::need(!empty_predictors(), "Please select at least one predictor variable.")
        )
        lapply(1:input$blocks, function(i) {
            lm(
                formula = as.formula(paste0(input$outcome, " ~ ", paste0(unlist(predictors()[1:i]), collapse = " + "))),
                data = df()
            )
        })
    })

    output$model_fit_measures <- renderTable(
        {
            req(models())
            p_digits_format <- paste0("%.", settings()$p_digits, "f")

            do.call(rbind, lapply(seq_along(models()), function(i) {
                model_summary <- summary(models()[[i]])
                fstat <- model_summary$fstatistic
                p_value_f <- pf(fstat[1], fstat[2], fstat[3], lower.tail = FALSE)

                dw_test <- car::durbinWatsonTest(models()[[i]])

                data.frame(
                    Model = i, R = sqrt(model_summary$r.squared), R_sq = model_summary$r.squared,
                    Adj_R_sq = model_summary$adj.r.squared, AIC = AIC(models()[[i]]), BIC = BIC(models()[[i]]),
                    RMSE = sqrt(mean(residuals(models()[[i]])^2)), F = fstat[1], df1 = fstat[2], df2 = fstat[3],
                    p_value_f_str = if (p_value_f < 0.001) "< 0.001" else sprintf(p_digits_format, p_value_f),
                    DW_stat = dw_test$dw,
                    p_value_dw_str = if (dw_test$p < 0.001) "< 0.001" else sprintf(p_digits_format, dw_test$p),
                    stringsAsFactors = FALSE, check.names = FALSE
                )
            })) %>%
                `colnames<-`(c("Model", "R", "R²", "Adj. R²", "AIC", "BIC", "RMSE", "F", "df1", "df2", "p-value", "D-W", "p (D-W)"))
        },
        digits = reactive(settings()$digits)
    )

    output$model_change_measures <- renderTable(
        {
            req(length(models()) > 0)

            first_model_summary <- summary(models()[[1]])
            first_model_fstat <- first_model_summary$fstatistic
            first_model_p <- pf(first_model_fstat[1], first_model_fstat[2], first_model_fstat[3], lower.tail = FALSE)

            model_change_measures <- data.frame(
                Model = "1", R_sq = first_model_summary$r.squared, R_sq_Change = first_model_summary$r.squared,
                F_Change = first_model_fstat[1], df1 = first_model_fstat[2], df2 = first_model_fstat[3], p_value = first_model_p,
                stringsAsFactors = FALSE, check.names = FALSE
            )

            if (length(models()) > 1) {
                for (i in 2:length(models())) {
                    anova_test <- anova(models()[[i - 1]], models()[[i]])
                    model_summary_curr <- summary(models()[[i]])
                    model_summary_prev <- summary(models()[[i - 1]])

                    change_row <- data.frame(
                        Model = paste(i - 1, "to", i), R_sq = model_summary_curr$r.squared,
                        R_sq_Change = model_summary_curr$r.squared - model_summary_prev$r.squared,
                        F_Change = anova_test$`F`[2], df1 = anova_test$Df[2], df2 = anova_test$Res.Df[2], p_value = anova_test$`Pr(>F)`[2],
                        stringsAsFactors = FALSE, check.names = FALSE
                    )
                    model_change_measures <- rbind(model_change_measures, change_row)
                }
            }

            colnames(model_change_measures) <- c("Model", "R²", "ΔR²", "ΔF", "df1", "df2", "Δp-value")
            model_change_measures
        },
        digits = reactive(settings()$digits)
    )

    output$model_coefficients_ui <- renderUI({
        ns <- session$ns
        tableOutput(ns("model_coefficients_table"))
    })

    output$model_coefficients_table <- function() {
        req(models())
        current_settings <- settings()

        coef_list <- lapply(seq_along(models()), function(i) {
            model <- models()[[i]]
            model_summary <- summary(model)

            raw_coefs <- as.data.frame(coef(model_summary))
            colnames(raw_coefs) <- c("coefficient", "se", "tstat", "p.value")

            base_df <- raw_coefs %>%
                tibble::rownames_to_column(var = "variable") %>%
                mutate(
                    model = i,
                    p.value_str = ifelse(p.value < 0.001, "<0.001", sprintf(paste0("%.", current_settings$p_digits, "f"), p.value))
                )

            model_with_beta <- lm.beta::lm.beta(model)
            beta_coefs <- as.data.frame(summary(model_with_beta)$coefficients)

            if ("Standardized" %in% colnames(beta_coefs)) {
                beta_df <- beta_coefs %>%
                    tibble::rownames_to_column(var = "variable") %>%
                    dplyr::select(variable, std.beta = Standardized)

                full_df <- base_df %>%
                    left_join(beta_df, by = "variable") %>%
                    dplyr::select(model, variable, coefficient, se, tstat, p.value_str, std.beta)
            } else {
                full_df <- base_df %>%
                    mutate(std.beta = NA_real_) %>%
                    dplyr::select(model, variable, coefficient, se, tstat, p.value_str, std.beta)
            }

            if (length(coef(model)) > 2) {
                coll_diag <- olsrr::ols_coll_diag(model)$vif_t %>%
                    dplyr::select(Variables, Tolerance, VIF)
                full_df <- left_join(full_df, coll_diag, by = c("variable" = "Variables"))
            } else {
                # FIX: Use NA_real_ to ensure column type is numeric, not logical
                full_df$Tolerance <- NA_real_
                full_df$VIF <- NA_real_
            }
            full_df
        })

        coefficients <- do.call(rbind, coef_list)
        grouping <- factor(coefficients$model, levels = unique(coefficients$model))

        coefficients %>%
            dplyr::select(-model) %>%
            mutate(across(where(is.numeric), ~ sprintf(paste0("%.", current_settings$digits, "f"), .))) %>%
            mutate(across(everything(), ~ replace_na(., "—"))) %>%
            knitr::kable("html", align = c("l", rep("c", 7)), row.names = FALSE, col.names = c(" ", "B", "SE", "t", "p", "β", "Tolerance", "VIF"), escape = FALSE) %>%
            kableExtra::kable_classic(full_width = FALSE, html_font = "inherit", position = "left") %>%
            kableExtra::add_header_above(c(" " = 1, "Unstandardised" = 2, " " = 2, "Standardized" = 1, "Collinearity" = 2)) %>%
            kableExtra::group_rows(index = table(grouping), group_label = paste0("Model ", levels(grouping))) %>%
            kableExtra::footnote(general = paste0("Dependent variable: ", isolate(input$outcome)))
    }

    # --- Advanced Diagnostics ---
    diagnostic_results <- reactiveVal(NULL)
    observeEvent(input$run_diagnostics, {
        req(length(models()) > 0)
        final_model <- models()[[length(models())]]
        showNotification("Running diagnostics...", type = "message", duration = 2)
        results <- diagnose_regression(
            model = final_model, data = df(),
            check_cook = "cook" %in% input$diagnostic_checks, check_dfbeta = "dfbeta" %in% input$diagnostic_checks,
            check_leverage = "leverage" %in% input$diagnostic_checks, check_mahalanobis = "mahalanobis" %in% input$diagnostic_checks,
            plot_diagnostics = input$show_plots, verbose = FALSE
        )
        diagnostic_results(results)
    })

    output$diagnostic_output_ui <- renderUI({
        req(diagnostic_results())
        ns <- session$ns
        all_tests_run <- names(diagnostic_results()$cutoffs)

        tag_list <- lapply(all_tests_run, function(name) {
            results_list <- diagnostic_results()$results
            title <- case_when(
                name == "cooks_distance" ~ "Cook's Distance Outliers", name == "dfbeta" ~ "DFBETA Influential Points",
                name == "leverage" ~ "High Leverage Points", name == "mahalanobis" ~ "Mahalanobis Distance Outliers",
                TRUE ~ name
            )

            if (name %in% names(results_list) && nrow(results_list[[name]]$data) > 0) {
                tagList(h4(title), tableOutput(ns(paste0("diag_table_", name))), uiOutput(ns(paste0("diag_note_", name))))
            } else {
                tagList(h4(title), p("No outliers were detected based on the cutoff."), uiOutput(ns(paste0("diag_note_", name))))
            }
        })

        plots_list <- diagnostic_results()$plots
        plot_tags <- NULL
        if (isTRUE(input$show_plots) && length(plots_list) > 0) {
            plot_tags <- tagList(
                h3("Diagnostic Plots"),
                lapply(names(plots_list), function(name) {
                    plotOutput(ns(paste0("diag_plot_", name)))
                })
            )
        }

        tagList(
            box(title = "Advanced Diagnostics Results", status = "info", solidHeader = TRUE, width = 12, tag_list),
            if (!is.null(plot_tags)) {
                box(title = "Diagnostic Plots", status = "info", solidHeader = TRUE, width = 12, collapsible = TRUE, plot_tags)
            }
        )
    })

    observe({
        req(diagnostic_results())
        results_list <- diagnostic_results()$results
        plots_list <- diagnostic_results()$plots
        formulas_list <- diagnostic_results()$formulas
        cutoffs_list <- diagnostic_results()$cutoffs
        current_digits <- settings()$digits

        for (name in names(cutoffs_list)) {
            local({
                my_name <- name
                if (my_name %in% names(results_list) && nrow(results_list[[my_name]]$data) > 0) {
                    my_data <- results_list[[my_name]]$data
                    output[[paste0("diag_table_", my_name)]] <- renderTable(
                        {
                            my_data
                        },
                        digits = current_digits
                    )
                }

                my_formula <- formulas_list[[my_name]]
                my_cutoff <- cutoffs_list[[my_name]]
                output[[paste0("diag_note_", my_name)]] <- renderUI({
                    withMathJax(helpText(HTML(paste(my_formula, "<br><em>Calculated Cutoff:</em>", round(my_cutoff, 4)))))
                })
            })
        }

        if (isTRUE(input$show_plots) && length(plots_list) > 0) {
            for (name in names(plots_list)) {
                local({
                    my_name <- name
                    my_plot <- plots_list[[my_name]]
                    output[[paste0("diag_plot_", my_name)]] <- renderPlot({
                        my_plot
                    })
                })
            }
        }
    })
}
