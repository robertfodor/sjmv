# Variance analysis
library(shiny)
library(dplyr) # for data manipulation
library(shinyWidgets) # for custom widgets
library(knitr) # for kable
library(kableExtra) # extra formatting for kable
library(report)
library(s20x) # for Levene's test for non-interaction ANOVA
library(car) # for leveneTest due to centering option
library(broom) # to clean up aov output
library(ggplot2)

# UI
variance_ui <- function(id) {
    ns <- NS(id)
    tagList(
        fluidPage(
            fluidRow(
                column(
                    width = 12,
                    h2("Variance Analysis")
                ),
                column(
                    width = 6,
                    pickerInput(
                        inputId = ns("outcome"),
                        label = "Select quantitative outcome variable:",
                        multiple = FALSE,
                        choices = NULL
                    ),
                    awesomeRadio(
                        inputId = ns("type"),
                        label = "Select type of variance analysis:",
                        choices = c(
                            "One-way ANOVA",
                            "Factorial ANOVA without interactions",
                            "Factorial (Two-way) ANOVA with interactions"
                        ),
                        selected = "One-way ANOVA",
                        inline = FALSE,
                        status = "danger"
                    ),
                    uiOutput(ns("predictor_selector"))
                ),
                column(
                    width = 6,
                    box(
                        title = "More on ANOVA types",
                        status = "info",
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        collapsed = TRUE,
                        width = "100%",
                        tagList(
                            HTML("<p>If you have one predictor (independent)
                            variable, make sure to select <b>One-way ANOVA</b>.
                            </p>
                            <p>If you have at least two predictor variables,
                            you'll have to select one of the <b>Factorial</b>
                            ANOVAs.</p>
                            <p> If your predictors are completely independent
                            of each other, select <b>Factorial ANOVA without
                            interactions</b>. Otherwise, select <b>Factorial
                            (Two-way) ANOVA with interactions</b>.</p>
                            ")
                        )
                    )
                )
            ),
            fluidRow(
                column(
                    width = 12,
                    h3("Assumption checks"),
                    tableOutput(ns("homogeneity")),
                    br(),
                    textOutput(ns("homogeneity_text")),
                    tableOutput(ns("normality")),
                    br(),
                    textOutput(ns("normality_text")),
                    br(),
                    plotOutput(ns("qq_plot")),
                    tableOutput(ns("equality_of_covariances")),
                    tableOutput(ns("sphericity")),
                    h3("Results"),
                    tableOutput(ns("anova_results")),
                    br(),
                    tagAppendAttributes(
                        textOutput(ns("results_text")),
                        style = "white-space: pre-wrap;"
                    ),
                    h3("Post-hoc analysis"),
                    tableOutput(ns("post_hoc_comparison"))
                )
            )
        )
    )
}

variance_server <- function(
    input, output, session,
    file_input, digits) {
    # Update the choices of the pickerInput based on file_input$df
    observeEvent(file_input$df, {
        # Update the choices of the outcome pickerInput
        updatePickerInput(
            session = session,
            inputId = "outcome",
            # Outcome choices can only be numeric columns
            choices = names(file_input$df)[sapply(file_input$df, is.numeric)],
            # When digits are changed it shouldn't reset to 1st col.
            selected = isolate(input$outcome)
        )
    })

    # Update the predictors based on input$type
    output$predictor_selector <- renderUI({
        # session ns
        ns <- session$ns

        # If type is One-way ANOVA, only one predictor is allowed
        if (input$type == "One-way ANOVA") {
            label <- "Select categorical grouping variable"
            multiple <- FALSE
        } else {
            label <- "Select categorical predictor (factor) variables"
            multiple <- TRUE
        }

        # UI list
        ui_list <- list(pickerInput(
            # use session ns for naming
            inputId = ns("predictors"),
            label = label,
            # Choices must be factor columns
            choices = names(file_input$df)[sapply(file_input$df, is.factor)],
            # Pre-fill if there is already a predictor selected previously
            #  This also prevents digit changes to reset the predictors
            selected = isolate(input$predictors),
            multiple = multiple
        ))

        # Return a list of UI elements
        return(ui_list)
    })

    # Data frame
    df <- reactive({
        df <- data.frame(
            file_input$df %>%
                select(input$outcome, unlist(input$predictors))
        )
        return(df)
    })

    # Check if any inputs are missing
    missing_inputs <- reactive({
        if (is.null(input$outcome) | is.null(input$predictors)) {
            return(TRUE)
        } else {
            return(FALSE)
        }
    })

    # Create formula
    formula <- reactive({
        if (missing_inputs()) {
            return(NULL)
        } else {
            collapse <- ifelse(
                input$type == "Factorial ANOVA without interactions",
                "+",
                "*"
            )
            return(as.formula(paste(
                input$outcome,
                "~",
                paste(input$predictors, collapse = collapse)
            )))
        }
    })

    # Create aov model
    model <- reactive({
        if (missing_inputs()) {
            return(NULL)
        } else {
            return(aov(
                formula = formula(),
                data = df()
            ))
        }
    })

    # Levene
    levene <- reactive({
        if (missing_inputs()) {
            return(NULL)
        } else {
            if (input$type == "Factorial ANOVA without interactions") {
                test <- s20x::levene.test(
                    formula(),
                    data = df(),
                    digit = digits
                )

                return(data.frame(
                    Test = "Levene's test (median centered)",
                    Statistic = sprintf(
                        paste0("%.", digits, "f"), test$f.value[1]
                    ),
                    df1 = test$df[1],
                    df2 = test$df[2],
                    p.value = sprintf(
                        paste0("%.", digits, "f"), test$p.value[1]
                    )
                ))
            } else {
                test <- car::leveneTest(
                    formula(),
                    center = "mean",
                    data = df()
                )
                return(data.frame(
                    Test = "Levene's test (mean centered)",
                    Statistic = sprintf(
                        paste0("%.", digits, "f"), test$`F value`[1]
                    ),
                    df1 = test$Df[1],
                    df2 = test$Df[2],
                    p.value = sprintf(
                        paste0("%.", digits, "f"), test$`Pr(>F)`[1]
                    )
                ))
            }
        }
    })

    # Perform homogeneity of variance test first
    output$homogeneity <- function() {
        if (missing_inputs()) {
            return(NULL)
        } else {
            levene() %>%
                # dplyr::mutate_if(
                #     is.numeric,
                #     ~ sprintf(paste0("%.", digits, "f"), .)
                # ) %>%
                knitr::kable(
                    "html",
                    caption = "Homogeneity of variance",
                    align = c("l", rep("c", 4)),
                    escape = FALSE,
                    col.names = c(" ", "F statistic", "df1", "df2", "p-value")
                ) %>%
                kableExtra::kable_classic(
                    full_width = FALSE,
                    html_font = "inherit",
                    position = "left"
                )
        }
    }

    # Homogeneity text result
    output$homogeneity_text <- function() {
        if (missing_inputs()) {
            return(NULL)
        } else {
            if (levene()$p.value[1] < 0.05) {
                return(paste(
                    "The homogeneity of variance assumption is violated.",
                    "The variances are not equal, so Welch's ANOVA must to be",
                    "used instead of Fisher's ANOVA. In the post-hoc analysis,",
                    "the Games-Howell test must be used instead of Tukey's test."
                ))
            } else {
                return(paste(
                    "The homogeneity of variance assumption is not violated."
                ))
            }
        }
    }

    # Shapiro test
    shapiro <- reactive({
        if (missing_inputs()) {
            return(NULL)
        } else {
            return(shapiro.test(x = residuals(model())))
        }
    })

    # Perform normality test
    output$normality <- function() {
        if (missing_inputs()) {
            return(NULL)
        } else {
            normality <- data.frame(
                Test = shapiro()$method,
                Statistic = shapiro()$statistic,
                p.value = if (shapiro()$p.value < 0.001) {
                    "< 0.001"
                } else {
                    sprintf(paste0("%.", digits, "f"), shapiro()$p.value)
                }
            )

            normality %>%
                dplyr::mutate_if(
                    is.numeric,
                    ~ sprintf(paste0("%.", digits, "f"), .)
                ) %>%
                knitr::kable(
                    "html",
                    caption = "Normality",
                    align = c("l", rep("c", ncol(normality) - 1)),
                    escape = FALSE,
                    col.names = c(" ", "W statistic", "p-value"),
                    row.names = FALSE
                ) %>%
                kableExtra::kable_classic(
                    full_width = FALSE,
                    html_font = "inherit",
                    position = "left"
                )
        }
    }

    # Normality text result
    output$normality_text <- function() {
        if (missing_inputs()) {
            return(NULL)
        } else {
            if (shapiro()$p.value < 0.05) {
                return(paste(
                    "The normality assumption is violated.",
                    "The residuals are not normally distributed,",
                    "so the non-parametric Kruskal-Wallis test",
                    "should be used instead of one-way ANOVA."
                ))
            } else {
                return(paste(
                    "The normality assumption is not violated."
                ))
            }
        }
    }

    # QQ of residuals
    output$qq_plot <- renderPlot({
        if (missing_inputs()) {
            return(NULL)
        } else {
            qqnorm(residuals(model()))
            qqline(residuals(model()))
        }
    })


    # ANOVA table
    output$anova_results <- function() {
        if (missing_inputs()) {
            return(NULL)
        } else {
            as.data.frame(broom::tidy(model())) %>%
                # dplyr::mutate(
                #     p.value = if (p.value < 0.001) {
                #         "< 0.001"
                #     } else {
                #         sprintf(paste0("%.", digits, "f"), p.value)
                #     }
                # ) %>%
                dplyr::mutate_if(
                    is.numeric,
                    ~ sprintf(paste0("%.", digits, "f"), .)
                ) %>%
                knitr::kable(
                    "html",
                    caption = "ANOVA",
                    align = c("l", rep("c", 5)),
                    escape = FALSE,
                    col.names = c(
                        " ", "Df", "Sum of Squares",
                        "Mean Square", "F", "p-value"
                    ),
                    row.names = FALSE
                ) %>%
                kableExtra::kable_classic(
                    full_width = FALSE,
                    html_font = "inherit",
                    position = "left"
                )
        }
    }

    # Text output
    output$results_text <- renderText({
        if (missing_inputs()) {
            return(NULL)
        } else {
            report::report_text(model()) %>%
                # replace "Eta2" with html eta symbol and squared symbol
                gsub("Eta2", "\u03B7\u00B2", .) %>%
                # remove the row starting with "Effect sizes were labelled"
                gsub("Effect sizes were labelled.*", "", .)
        }
    })
}
