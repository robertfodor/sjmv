# This is the regression tab.
# It takes as input the datafile uploaded in the Getting Started tab.
library(shiny)
library(dplyr) # for data manipulation
library(gridExtra) # for grid.arrange
library(shinyWidgets) # for custom widgets
library(olsrr) # vif, tolerance
library(reghelper) # regression analysis (switch from lm.beta)
library(knitr) # for kable
library(kableExtra) # extra formatting for kable
library(ggplot2) # for plotting diagnostics

# UI
regression_ui <- function(id) {
    ns <- NS(id)
    tagList(
        withMathJax(), # <--- ADD THIS LINE
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
                    tableOutput(ns("model_coefficients")),

                    # --- MODIFIED DIAGNOSTICS SECTION ---
                    h3("Model Diagnostics"),
                    # Keep the original collinearity output
                    tableOutput(ns("collinearity")),

                    # --- START: NEW UI SECTION INSERTED HERE ---
                    fluidRow(
                        box(
                            title = "Advanced Diagnostic Controls",
                            status = "primary",
                            solidHeader = TRUE,
                            width = 12,
                            collapsible = TRUE,
                            collapsed = FALSE, # Start open for visibility
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
                            # A single UI placeholder for all diagnostic output
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
    file_input, digits) {
    # Create UI for predictor blocks based on number of blocks
    output$predictor_blocks <- renderUI({
        # Create a list of UI elements
        ui_list <- list()
        # session ns
        ns <- session$ns
        # Loop through each block
        for (i in 1:input$blocks) {
            # Create a UI element for each block
            ui_list[[i]] <- pickerInput(
                # use session ns for inputId naming
                inputId = ns(paste0("block_", i)),
                label = paste0("Predictor block ", i),
                # Choices exclude outcome
                choices = setdiff(
                    names(file_input$df),
                    c(
                        input$outcome
                    )
                ),
                # Pre-fill if there is already a predictor selected previously
                #  This also prevents digit changes to reset the predictors
                selected = isolate(input[[paste0("block_", i)]]),
                multiple = TRUE
            )
        }

        # Return a list of UI elements
        return(ui_list)
    })

    # Predictors
    predictors <- reactive({
        predictors <- list()
        for (i in 1:input$blocks) {
            predictors[[i]] <- input[[paste0("block_", i)]]
        }
        return(predictors)
    })

    # Data frame
    df <- reactive({
        df <- data.frame(
            file_input$df %>%
                select(input$outcome, unlist(predictors()))
        )
        # Convert factor predictors to numeric if input$contrasts is FALSE
        if (input$contrasts == FALSE) {
            df <- df %>%
                mutate_if(is.factor, as.numeric)
        }

        return(df)
    })

    # Update the choices of the pickerInput based on file_input$df
    observeEvent(file_input$df, {
        # Update the choices of the outcome pickerInput
        updatePickerInput(
            session = session,
            inputId = "outcome",
            choices = names(file_input$df),
            # When digits are changed it shouldn't reset to 1st col.
            selected = isolate(input$outcome)
        )
    })

    # empty_predictors is TRUE if any of the input$block_i is length 0
    empty_predictors <- reactive({
        for (i in 1:input$blocks) {
            if (length(input[[paste0("block_", i)]]) == 0) {
                empty_predictors <- TRUE
            } else {
                empty_predictors <- FALSE
            }
        }
        return(empty_predictors)
    })

    # Model
    models <- reactive({
        # VALIDATION: Check for outcome and predictor variables
        shiny::validate(
            shiny::need(input$outcome, "Kérem, válasszon ki egy kimeneti (függő) változót."),
            shiny::need(empty_predictors() == FALSE, "Kérem, válasszon ki legalább egy prediktor változót.")
        )

        # Create a list of models
        models <- list()
        # Build a model for each block
        for (i in 1:input$blocks) {
            models[[i]] <-
                lm(
                    formula = as.formula(paste0(
                        input$outcome, " ~ ",
                        paste0(
                            # Predictors are cumulative
                            unlist(predictors()[1:i]),
                            collapse = " + "
                        )
                    )),
                    data = df()
                )
        }

        # Return a list of models
        return(models)
    })

    # Model fit measures for each block in a table. Each row is a model.
    output$model_fit_measures <- renderTable(
        {
            # Create a list of model fit measures
            model_fit_measures <- data.frame()

            # If models length is not null, build a model for each block
            if (length(models()) > 0) {
                for (i in 1:length(models())) {
                    # p-value
                    p_value <- pf(
                        summary(models()[[i]])$fstatistic[1],
                        summary(models()[[i]])$fstatistic[2],
                        summary(models()[[i]])$fstatistic[3],
                        lower.tail = FALSE
                    )

                    # Build model for each block
                    model_fit_measures <- rbind(
                        model_fit_measures,
                        data.frame(
                            model = paste0(i),
                            r = sqrt(summary(models()[[i]])$r.squared),
                            r.squared = summary(models()[[i]])$r.squared,
                            adj.r.squared = summary(
                                models()[[i]]
                            )$adj.r.squared,
                            AIC = AIC(models()[[i]]),
                            BIC = BIC(models()[[i]]),
                            RMSE = sqrt(mean(models()[[i]]$residuals^2)),
                            fstat = summary(models()[[i]])$fstatistic[1],
                            df1 = as.character(round(
                                summary(models()[[i]])$fstatistic[2], 0
                            )),
                            df2 = as.character(round(
                                summary(models()[[i]])$fstatistic[3], 0
                            )),
                            p_value = if (p_value < 0.001) {
                                "< 0.001"
                            } else {
                                sprintf(paste0("%.", digits, "f"), p_value)
                            },
                            stringsAsFactors = FALSE
                        )
                    )
                }

                # Return models
                colnames(model_fit_measures) <- c(
                    "Model", "R", "R²", "Adj. R²", "AIC", "BIC", "RMSE",
                    "F", "df1", "df2", "p-value"
                )
                return(model_fit_measures)
            }
        },
        digits = digits
    )
    # Model change table
    output$model_change_measures <- renderTable(
        { # Compare models with ANOVA
            anova_tables <- list()
            if (length(models()) > 1) {
                for (i in 2:length(models())) {
                    anova_tables[[i]] <- anova(
                        models()[[i - 1]],
                        models()[[i]]
                    )
                }
            }

            # Create a list of model change measures
            model_change_measures <- data.frame()
            if (length(models()) > 0) {
                for (i in 1:length(models())) {
                    # Build model for each block
                    model_change_measures <- rbind(
                        model_change_measures,
                        data.frame(
                            model = paste0(i - 1, " to ", i),
                            r.squared = summary(models()[[i]])$r.squared,
                            rsq.change = ifelse(
                                length(models()) > 1 & i > 1,
                                summary(models()[[i]])$r.squared -
                                    summary(models()[[i - 1]])$r.squared,
                                #  R squared for the first model
                                summary(models()[[i]])$r.squared
                            ),
                            fstat.change = ifelse(
                                length(models()) > 1 & i > 1,
                                anova_tables[[i]][2, 5],
                                summary(models()[[1]])$fstatistic[1]
                            ),
                            df1 = ifelse(
                                length(models()) > 1 & i > 1,
                                anova_tables[[i]][2, 3],
                                as.character(round(
                                    summary(models()[[1]])$fstatistic[2], 0
                                ))
                            ),
                            df2 = ifelse(
                                length(models()) > 1 & i > 1,
                                anova_tables[[i]][2, 1],
                                as.character(round(
                                    summary(models()[[1]])$fstatistic[3], 0
                                ))
                            ),
                            p.value = ifelse(
                                length(models()) > 1 & i > 1,
                                anova_tables[[i]][2, 6],
                                pf(
                                    summary(models()[[1]])$fstatistic[1],
                                    summary(models()[[1]])$fstatistic[2],
                                    summary(models()[[1]])$fstatistic[3],
                                    lower.tail = FALSE
                                )
                            ),
                            stringsAsFactors = FALSE
                        )
                    )
                }

                # Return models
                colnames(model_change_measures) <- c(
                    "Model", "R²", "ΔR²",
                    "ΔF", "df1", "df2", "Δp-value"
                )
                return(model_change_measures)
            }
        },
        digits = digits
    )

    # Model coefficients table
    output$model_coefficients <- function() {
        # Create a list of model coefficients
        coefficients <- data.frame()

        # If models length is not null, build a model for each block
        if (length(models()) > 0) {
            for (i in 1:length(models())) {
                # Build model for each block
                coefficients <- rbind(
                    coefficients,
                    data.frame(
                        #  First column is the model number
                        model = paste0(i),
                        #  Second column is the variable name
                        variable = names(
                            models()[[i]]$coefficients
                        ),
                        #  Third column is the coefficient
                        coefficient = summary(models()[[i]])$coefficients[
                            , 1
                        ],
                        #  Fourth column is the standard error
                        se = summary(models()[[i]])$coefficients[, 2],
                        #  Fifth column is the t statistic
                        tstat = summary(models()[[i]])$coefficients[, 3],
                        #  Sixth column is the p value
                        #   If the p value is less than 0.001, return "<0.001"
                        p.value = ifelse(
                            round(summary(
                                models()[[i]]
                            )$coefficients[, 4], 3) < 0.001,
                            "<0.001",
                            as.character(round(summary(
                                models()[[i]]
                            )$coefficients[, 4], digits = digits))
                        ),
                        #   Seventh column is the Standardized β
                        std.beta = reghelper::beta(
                            models()[[i]]
                        )$coefficients[, 1]
                    )
                )
            }

            # Create a vector of model names and count of variables
            grouping <- factor(
                coefficients$model,
                levels = unique(coefficients$model),
                labels = paste0("Model ", unique(coefficients$model))
            )

            coefficients %>%
                # Discard column "model"
                dplyr::select(-model) %>%
                # Round numeric columns with sprintf to specified digits
                dplyr::mutate_if(
                    is.numeric,
                    ~ sprintf(paste0("%.", digits, "f"), .)
                ) %>%
                # Format t
                knitr::kable(
                    "html",
                    align = c("l", rep("c", 5)),
                    row.names = FALSE,
                    col.names = c(
                        " ", "B coeff.", "SE",
                        "t", "p-value", "β coeff."
                    )
                ) %>%
                kableExtra::kable_classic(
                    full_width = FALSE,
                    html_font = "inherit",
                    position = "left"
                ) %>%
                kableExtra::add_header_above(c(
                    " " = 1,
                    "Unstandardised" = 2,
                    " " = 2,
                    "Standardized" = 1
                )) %>%
                # Group rows by model number
                kableExtra::group_rows(
                    index = table(grouping),
                    group_label = paste0("Model ", grouping)
                ) %>%
                kableExtra::footnote(
                    general = paste0(
                        "Dependent (outcome) variable: ",
                        isolate(input$outcome)
                    )
                )
        }
    }

    # Collinearity diagnostics table
    output$collinearity <- function() {
        tolvif <- data.frame()

        if (length(models()) > 0) {
            for (i in 1:length(models())) {
                if (length(models()[[i]]$coefficients) > 2) {
                    tolvif <- rbind(tolvif, data.frame(
                        model = paste0("", i),
                        Variable =
                            olsrr::ols_coll_diag(models()[[i]])$vif_t[, 1],
                        Tolerance =
                            olsrr::ols_coll_diag(models()[[i]])$vif_t[, 2],
                        VIF =
                            olsrr::ols_coll_diag(models()[[i]])$vif_t[, 3]
                    ))
                } else {
                    tolvif <- rbind(tolvif, data.frame(
                        model = paste0("", i),
                        Variable = names(models()[[i]]$coefficients),
                        Tolerance = 1,
                        VIF = 1
                    ) %>% # remove intercept
                        dplyr::filter(Variable != "(Intercept)"))
                }
            }

            # Check Tolerance and VIF and add footnote.
            #  Source: DOI: 10.12691/ajams-8-2-1
            for (i in 1:nrow(tolvif)) {
                if (tolvif$VIF[i] > 5) {
                    tolvif$Variable[i] <- paste0(
                        tolvif$Variable[i], footnote_marker_symbol(2, "html")
                    )
                }
            }

            # If any VIF is greater than 5, add footnote
            if (any(tolvif$VIF > 5)) {
                symbol <- "Severe multicollinearity detected:
                    VIF > 5 and Tolerance < 0.2"
                symbol_manual <- c(footnote_marker_symbol(2, "html"))
            } else {
                symbol <- ""
                symbol_manual <- c(" ")
            }


            # Create a vector of model names and count of variables
            grouping <- factor(
                tolvif$model,
                levels = unique(tolvif$model),
                labels = paste0("Model ", unique(tolvif$model))
            )

            # Formatting
            tolvif %>%
                # Discard column "model"
                dplyr::select(-model) %>%
                # Round numeric columns with sprintf to specified digits
                dplyr::mutate_if(
                    is.numeric,
                    ~ sprintf(paste0("%.", digits, "f"), .)
                ) %>%
                knitr::kable(
                    "html",
                    caption = "Multicollinearity diagnostics",
                    align = "lcc",
                    escape = FALSE
                ) %>%
                kableExtra::kable_classic(
                    full_width = FALSE,
                    html_font = "inherit",
                    position = "left"
                ) %>%
                # Group rows by model number
                kableExtra::group_rows(
                    index = table(grouping),
                    group_label = paste0("Model ", grouping)
                ) %>%
                kableExtra::footnote(
                    symbol_title = "Multicollinearity assumptions checked. ",
                    symbol = symbol,
                    symbol_manual = symbol_manual
                )
        }
    }

    # --- START: NEW ADVANCED DIAGNOSTICS SERVER LOGIC ---

    # 1. Create a reactiveVal to store diagnostic results
    diagnostic_results <- reactiveVal(NULL)

    # 2. Run diagnostics when the button is clicked
    observeEvent(input$run_diagnostics, {
        # Ensure a model exists
        req(length(models()) > 0, cancelOutput = TRUE)

        # Use the *final* model for diagnostics
        final_model <- models()[[length(models())]]
        model_data <- df() # Use the reactive data frame for the model

        # Show a notification
        showNotification("Running advanced regression diagnostics...", type = "message", duration = 2)

        # Call your function with parameters from the UI
        results <- diagnose_regression(
            model = final_model,
            data = model_data,
            check_cook = "cook" %in% input$diagnostic_checks,
            check_dfbeta = "dfbeta" %in% input$diagnostic_checks,
            check_leverage = "leverage" %in% input$diagnostic_checks,
            check_mahalanobis = "mahalanobis" %in% input$diagnostic_checks,
            plot_diagnostics = input$show_plots,
            verbose = FALSE # <--- ADD THIS LINE TO SUPPRESS LOGGING
        )

        # Store the results in the reactiveVal
        diagnostic_results(results)
    })

    # --- PASTE THIS NEW CODE IN ITS PLACE ---

    # 3. Generate the UI Placeholders for all diagnostic output
    output$diagnostic_output_ui <- renderUI({
        req(diagnostic_results())
        ns <- session$ns

        # We need to check which results have data to display
        # This includes tests that ran but found no outliers
        all_tests_run <- names(diagnostic_results()$cutoffs)

        # Create UI for each test that was run
        tag_list <- lapply(all_tests_run, function(name) {
            results_list <- diagnostic_results()$results
            title <- case_when(
                name == "cooks_distance" ~ "Cook's Distance Outliers",
                name == "dfbeta" ~ "DFBETA Influential Points",
                name == "leverage" ~ "High Leverage Points",
                name == "mahalanobis" ~ "Mahalanobis Distance Outliers",
                TRUE ~ name
            )

            # Check if this test found any outliers
            if (name %in% names(results_list) && nrow(results_list[[name]]$data) > 0) {
                # If yes, create placeholder for table and note
                tagList(
                    h4(title),
                    tableOutput(ns(paste0("diag_table_", name))),
                    uiOutput(ns(paste0("diag_note_", name)))
                )
            } else {
                # If no outliers found, just show the note
                tagList(
                    h4(title),
                    p("No outliers were detected using the following cutoff:"),
                    uiOutput(ns(paste0("diag_note_", name)))
                )
            }
        })

        # Create plot placeholders
        plots_list <- diagnostic_results()$plots
        plot_tags <- NULL
        if (input$show_plots && length(plots_list) > 0) {
            plot_tags <- tagList(
                h3("Diagnostic Plots"),
                lapply(names(plots_list), function(name) {
                    plotOutput(ns(paste0("diag_plot_", name)))
                })
            )
        }

        # Combine everything into boxes
        tagList(
            box(title = "Advanced Diagnostics Results", status = "info", solidHeader = TRUE, width = 12, tag_list),
            if (!is.null(plot_tags)) {
                box(title = "Diagnostic Plots", status = "info", solidHeader = TRUE, width = 12, collapsible = TRUE, plot_tags)
            }
        )
    })

    # 4. Create the Renderers in a separate observer
    observe({
        req(diagnostic_results())
        results_list <- diagnostic_results()$results
        plots_list <- diagnostic_results()$plots
        formulas_list <- diagnostic_results()$formulas
        cutoffs_list <- diagnostic_results()$cutoffs

        # Create renderers for all tests that were run
        for (name in names(cutoffs_list)) {
            local({
                my_name <- name

                # Create table renderer if there is data
                if (my_name %in% names(results_list) && nrow(results_list[[my_name]]$data) > 0) {
                    my_data <- results_list[[my_name]]$data
                    output[[paste0("diag_table_", my_name)]] <- renderTable(
                        {
                            my_data
                        },
                        digits = digits
                    )
                }

                # Create note renderer
                my_formula <- formulas_list[[my_name]]
                my_cutoff <- cutoffs_list[[my_name]]
                output[[paste0("diag_note_", my_name)]] <- renderUI({
                    # Wrap with withMathJax() to ensure dynamic LaTeX is rendered
                    withMathJax(
                        helpText(HTML(paste(
                            my_formula,
                            "<br><em>Calculated Cutoff:</em>",
                            round(my_cutoff, 4)
                        )))
                    )
                })
            })
        }

        # Create plot renderers
        if (input$show_plots && length(plots_list) > 0) {
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
