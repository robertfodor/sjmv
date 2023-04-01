# This is the descriptive statistics tab for ShinyStat.
# It takes as input the datafile uploaded in the Getting Started tab.
library(shiny)
library(dplyr) # for data manipulation
library(ggplot2) # for plotting
library(shinyWidgets) # for custom widgets
library(car) # vif
library(lm.beta) # standardized beta coefficients

# UI
regression_ui <- function(id) {
    ns <- NS(id)
    tagList(
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
                    dropdownButton(
                        sliderTextInput(
                            inputId = ns("blocks"),
                            label = "Number of blocks:",
                            choices = seq(1, 10, 1),
                            grid = TRUE,
                            selected = 1
                        ),
                        circle = FALSE, status = "primary",
                        icon = icon("gear"), width = "200px",
                        tooltip = tooltipOptions(title = "Adjust number of blocks")
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
                    verbatimTextOutput(ns("model_coefficients")),
                    h3("Model Plots"),
                    plotOutput(ns("model_plots")),
                    verbatimTextOutput(ns("debug"))
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
                choices = setdiff(names(file_input$df), input$outcome),
                multiple = TRUE
            )
        }

        # Return a list of UI elements
        return(ui_list)
    })

    # Update the choices of the pickerInput based on file_input$df
    observeEvent(file_input$df, {
        # Update the choices of the outcome pickerInput
        updatePickerInput(
            session = session,
            inputId = "outcome",
            choices = names(file_input$df)
        )
        # Update the choices of the predictor blocks
        for (i in 1:input$blocks) {
            updatePickerInput(
                session = session,
                inputId = paste0("block_", i),
                choices = setdiff(names(file_input$df), input$outcome)
            )
        }
    })

    # empty_predictors is TRUE if any of the input$block_i is length 0
    empty_predictors <- reactive({
        empty_predictors <- TRUE
        for (i in 1:input$blocks) {
            if (length(input[[paste0("block_", i)]]) == 0) {
                empty_predictors <- TRUE
            } else {
                empty_predictors <- FALSE
            }
        }
        return(empty_predictors)
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
        return(data.frame(
            file_input$df %>%
                select(input$outcome, unlist(predictors()))
        ))
    })

    # Model
    #  Only if no block is empty
    models <- reactive({
        if (empty_predictors() == FALSE) {
            # Create a list of models
            models <- list()
            # If predictors is not empty, build a model for each block
            for (i in 1:input$blocks) {
                models[[i]] <- lm(
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
            # Standardize models
            models[[i]] <- lm.beta(models[[i]])
            # Return a list of models
            return(models)
        }
    })

    # Model ANOVA
    anova_stat <- reactive({
        # Create a list of anova tables
        anova_tables <- list()
        # Compare each model to the previous model
        if (length(models()) > 1) {
            for (i in 2:length(models())) {
                anova_tables[[i]] <- anova(
                    models()[[i - 1]],
                    models()[[i]]
                )
            }
        }
        # Return a list of anova tables
        return(anova_tables)
    })

    # Model fit measures for each block in a table. Each row is a model.
    output$model_fit_measures <- renderTable(
        {
            # Create a list of model fit measures
            model_fit_measures <- data.frame()

            # If models length is not null, build a model for each block
            if (length(models()) > 0) {
                for (i in 1:length(models())) {
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
                            df1 = summary(models()[[i]])$fstatistic[2],
                            df2 = summary(models()[[i]])$fstatistic[3],
                            p.value = pf(
                                summary(models()[[i]])$fstatistic[1],
                                summary(models()[[i]])$fstatistic[2],
                                summary(models()[[i]])$fstatistic[3],
                                lower.tail = FALSE
                            ),
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
        {
            # Create a list of model change measures
            model_change_measures <- data.frame()

            # If models length is not null, build a model for each block
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
                                #  R squared for the first model
                                summary(models()[[i]])$r.squared
                            ),
                            fstat.change = ifelse(
                                length(models()) > 1 & i > 1,
                                anova_stat()[[i]][2, 5],
                                # F statistic for the first model
                                summary(models()[[1]])$fstatistic[1]
                            ),
                            df1 = ifelse(
                                length(models()) > 1 & i > 1,
                                anova_stat()[[i]][2, 3],
                                # F statistic for the first model
                                summary(models()[[1]])$fstatistic[2]
                            ),
                            df2 = ifelse(
                                length(models()) > 1 & i > 1,
                                anova_stat()[[i]][2, 1],
                                # F statistic for the first model
                                summary(models()[[1]])$fstatistic[3]
                            ),
                            p.value = ifelse(
                                length(models()) > 1 & i > 1,
                                anova_stat()[[i]][2, 6],
                                # F statistic for the first model
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
    # Debug
    output$debug <- renderPrint({
        list()
    })
}
