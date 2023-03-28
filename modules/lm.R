# Shiny app for linear regression model building and evaluation

# Load libraries
library(shiny) # shiny
library(dplyr) # data wrangling
library(ggplot2) # plotting
library(car) # vif
library(showtext) # ggplot2 fonts
library(modelr) # modelr
library(broom) # tidy model output
library(lm.beta) # standardized beta coefficients
library(haven) # import SPSS files
library(jmvReadWrite) # import jamovi files
library(e1071) # skewness and kurtosis
library(psych) # needed for corr.test

# Load google fonts for ggplot2
font_add_google("Roboto", "chartfont")
showtext_auto()

# Create UI where user can select data, and prepare groupings for model building
# Assume linear model, no transformations, and with interactions
# Assume listwise deletion for missing data
# Allow SPSS, CSV, and jamovi files
# Default standardization is "standardize" but allow "mean center"
ui <- fluidPage(
    # Custom CSS for tables
    tags$style(HTML("
    .table, table {
      border-top: 1px solid black;
      border-bottom: 1px solid black;
    }

    .table>tbody>tr>td, .table>thead>tr>th {
      border: 0px;
      font-weight: normal;
    }

    .table>tbody>tr>td {
      text-align: right;
    }

    thead {
      border-bottom: 1px solid black;
    }

    pre {
      font-family: \"Helvetica Neue\",Helvetica,Arial,sans-serif;
      font-size: 14px;
      padding: 0px;
      border: 0px;
      background-color: transparent;
    }
  ")),
    titlePanel("Linear Regression Model"),
    sidebarLayout(
        sidebarPanel(
            fileInput(
                "file",
                "Choose a file",
                accept = c(
                    ".csv",
                    ".sav",
                    ".omv"
                )
            ),
            h3("Model Builder"),
            selectInput(
                "outcome",
                "Outcome (dependent) variable",
                choices = NULL
            ),
            sliderInput(
                "blocks",
                "Number of blocks",
                min = 1,
                max = 10,
                value = 1
            ),
            uiOutput("predictor_blocks"),
            # Model building options
            h3("Model Building Options"),
            selectInput(
                "standardize",
                "Standardisation",
                choices = c("True", "False")
            ),
            # Button to build model
            uiOutput("build_model_button")
        ),
        mainPanel(
            h3("Descriptive Statistics"),
            conditionalPanel(
                condition = "input.build_model == 1",
                h4("Central tendency and distribution"),
                tableOutput("descriptives_a"),
                h4("Dispersion"),
                tableOutput("descriptives_b"),
                h4("Normality"),
                tableOutput("descriptives_c")
            ),
            h3("Correlations"),
            conditionalPanel(
                condition = "input.build_model == 1",
                tableOutput("correlation_matrix"),
            ),
            h3("Model Evaluation"),
            conditionalPanel(
                condition = "input.build_model == 1",
                h4("Model Fit Measures"),
                tableOutput("model_fit_measures"),
                h4("Model Change Measures"),
                tableOutput("model_change_measures"),
                h4("Model Coefficients"),
                verbatimTextOutput("model_coefficients"),
                h4("Model Plots"),
                plotOutput("model_plots")
            )
        )
    )
)


# Create server logic
server <- function(input, output, session) {
    # Load logic based on selected file type
    data <- reactive({
        req(input$file)
        # Decide import script based on file extension
        if (endsWith(input$file$name, ".csv")) {
            df <- read.csv(
                input$file$datapath,
                header = TRUE,
                stringsAsFactors = FALSE
            )
        } else if (endsWith(input$file$name, ".sav")) {
            df <- haven::read_spss(
                input$file$datapath,
                user_na = TRUE
            )
        } else if (endsWith(input$file$name, ".omv")) {
            df <- read_omv(input$file$datapath)
        }
        # Results in a data.frame
        return(df)
    })

    # Update choices for outcome
    observe({
        updateSelectInput(
            session,
            "outcome",
            choices = names(data()),
            selected = names(data())[1]
        )
    })

    # Create UI for predictor blocks based on number of blocks
    output$predictor_blocks <- renderUI({
        # Create a list of UI elements
        ui_list <- list()
        # Loop through each block
        for (i in 1:input$blocks) {
            # Create a UI element for each block
            ui_list[[i]] <- selectInput(
                paste0("block_", i),
                paste0("Block ", i),
                choices = setdiff(names(data()), input$outcome),
                multiple = TRUE
            )
        }
        # Return a list of UI elements
        return(ui_list)
    })

    # Create a function to return predictors
    predictors <- reactive({
        # Loop through each block and append predictors to a list
        predictors <- list()
        for (i in 1:input$blocks) {
            predictors[[i]] <- input[[paste0("block_", i)]]
        }
        # Return a list of predictors
        return(predictors)
    })

    # Are any input blocks empty?
    empty_blocks <- reactive({
        empty_blocks <- list()
        for (i in 1:input$blocks) {
            empty_blocks[[i]] <- is.null(input[[paste0("block_", i)]])
        }
        return(any(empty_blocks == TRUE))
    })

    # If empty_blocks is FALSE, render the build model button
    output$build_model_button <- renderUI({
        if (!empty_blocks()) {
            return(
                actionButton(
                    "build_model",
                    "Build model"
                )
            )
        }
    })

    #  Validate button every time a block or the number of blocks is changed
    observeEvent(
        input$blocks,
        {
            validate(
                need(
                    !empty_blocks(),
                    "All blocks must have at least one predictor"
                )
            )
        }
    )

    # Data frame for statistics
    df <- reactive({
        req(input$build_model)
        return(data.frame(
            data()[, input$outcome],
            data()[, unlist(predictors()[1:input$blocks])]
        ))
    })

    # Create a list of models based on the number of blocks
    models <- reactive({
        req(input$build_model)
        # Create a list of models
        models <- list()
        # If predictors is not empty, build a model for each block
        if (!empty_blocks()) {
            for (i in 1:input$blocks) {
                # Build model for each block
                models[[i]] <- lm(
                    formula = paste0(
                        input$outcome,
                        " ~ ",
                        paste(
                            # Add predictors from previous blocks and current block
                            unlist(predictors()[1:i]),
                            collapse = " + "
                        )
                    ),
                    data = data()
                )

                # If standardize is true, standardize the model with lm.beta
                if (input$standardize == "True") {
                    models[[i]] <- lm.beta(models[[i]])
                }
            }
        }
        # Return a list of models
        return(models)
    })

    # Create a list of correlation tests based on the number of blocks
    correlation_tests <- reactive({
        corr.test(
            df(),
            use = "pairwise",
            method = "pearson",
            adjust = "none",
            alpha = .05
        )
    })

    # ANOVA for models
    anova_stat <- reactive({
        # Create a list of anova tables
        anova_tables <- list()
        # If there are more than one block, compare each model to the previous model
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

    # Descriptives
    output$descriptives_a <- renderTable(
        { # N, missing, mean, median, mode, skewness, kurtosis, Shapiro-Wilk
            descriptives <- data.frame(
                n = apply(df(), 2, length),
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
                "N", "Mean", "Mean SE", "Median", "Mode",
                "Skewness", "Kurtosis"
            )
            return(descriptives)
        },
        digits = 3,
        rownames = TRUE
    )

    output$descriptives_b <- renderTable(
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
        digits = 3,
        rownames = TRUE
    )

    output$descriptives_c <- renderTable(
        { # Shapiro-Wilk
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
        digits = 3,
        rownames = TRUE
    )

    # Correlation matrix
    output$correlation_matrix <- renderTable(
        {
            correlation_tests()$r
        },
        digits = 3,
        rownames = TRUE
    )

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
                            adj.r.squared = summary(models()[[i]])$adj.r.squared,
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
                    "Model",
                    "R",
                    "R²",
                    "Adj. R²",
                    "AIC",
                    "BIC",
                    "RMSE",
                    "F",
                    "df1",
                    "df2",
                    "p-value"
                )
                return(model_fit_measures)
            }
        },
        digits = 3
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
                    "Model",
                    "R²",
                    "ΔR²",
                    "ΔF",
                    "df1",
                    "df2",
                    "Δp-value"
                )
                return(model_change_measures)
            }
        },
        digits = 3
    )
}

# Run the application
shinyApp(ui = ui, server = server)
