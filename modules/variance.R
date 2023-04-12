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
library(effectsize)
options(es.use_symbols = TRUE)

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
                            "Factorial ANOVA with interactions"
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
                            ANOVA with interactions</b>.</p>
                            ")
                        )
                    )
                )
            ),
            fluidRow(
                column(
                    width = 12,
                    h3("Assumption checks"),
                    h4("Homogeneity of variance assumption"),
                    tableOutput(ns("homogeneity")),
                    br(),
                    textOutput(ns("homogeneity_text")),
                    h4("Normality assumption"),
                    tableOutput(ns("normality")),
                    br(),
                    textOutput(ns("normality_text")),
                    br(),
                    h4("QQ plot of residuals"),
                    plotOutput(ns("qq_plot")),
                    tableOutput(ns("equality_of_covariances")),
                    tableOutput(ns("sphericity")),
                    h3("Results"),
                    h4("ANOVA table"),
                    tableOutput(ns("anova_results")),
                    tableOutput(ns("anova_effect_sizes")),
                    br(),
                    tagAppendAttributes(
                        textOutput(ns("results_text")),
                        style = "white-space: pre-wrap;"
                    ),
                    h4("Non-parametric tests"),
                    h5("Kruskal-Wallis rank sum test"),
                    tableOutput(ns("kruskal_table")),
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
            if (input$type == "Factorial ANOVA with interactions") {
                # Create a list of contrasts
                factorlist <- list()
                for (i in input$predictors) {
                    factorlist[[i]] <- "contr.sum"
                }

                return(
                    aov(
                        formula = formula(),
                        data = df(),
                        contrasts = factorlist
                    )
                )
            } else {
                return(aov(
                    formula = formula(),
                    data = df()
                ))
            }
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
                    # caption = "Homogeneity of variance",
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
                    "The variances are not equal."
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
                    # caption = "Normality",
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
                    "should be used instead of a parametric one-way ANOVA."
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

    # Overall model test
    overall_model_test <- reactive({
        if (missing_inputs()) {
            return(NULL)
        } else {
            return(lm(formula(), data = df()))
        }
    })

    # ANOVA table
    output$anova_results <- function() {
        if (missing_inputs()) {
            return(NULL)
        } else {
            model <- data.frame(
                Term = "Overall model test",
                SS = sum(
                    (overall_model_test()$fitted.values - mean(
                        overall_model_test()$model[[input$outcome]]
                    ))^2
                ),
                Df = summary(overall_model_test())$fstatistic[2],
                MS = sum(
                    (overall_model_test()$fitted.values - mean(
                        overall_model_test()$model[[input$outcome]]
                    ))^2
                ) /
                    summary(overall_model_test())$fstatistic[2],
                F = summary(overall_model_test())$fstatistic[1],
                p.value = pf(
                    summary(overall_model_test())$fstatistic[1],
                    summary(overall_model_test())$fstatistic[2],
                    summary(overall_model_test())$fstatistic[3],
                    lower.tail = FALSE
                )
            )

            if (input$type == "Factorial ANOVA with interactions") {
                # Use car::Anova type 3
                model_intercept <- as.data.frame(broom::tidy(
                    car::Anova(model(), type = 3)
                )) %>%
                    dplyr::filter(term == "(Intercept)")

                intercept <- data.frame(
                    Term = "Intercept",
                    SS = model_intercept[2],
                    Df = model_intercept[3],
                    MS = model_intercept[2] / model_intercept[3],
                    F = model_intercept[4],
                    p.value = model_intercept[5]
                )

                colnames(intercept) <- colnames(model)

                model <- rbind(model, intercept)
            }

            model_nonintercept <- as.data.frame(broom::tidy(model()))

            nonintercept <- data.frame(
                Term = model_nonintercept[1],
                SS = model_nonintercept[3],
                Df = model_nonintercept[2],
                MS = model_nonintercept[4],
                F = model_nonintercept[5],
                p.value = model_nonintercept[6]
            )

            colnames(nonintercept) <- colnames(model)

            model <- rbind(model, nonintercept)

            model %>%
                dplyr::mutate_if(
                    is.numeric,
                    ~ sprintf(paste0("%.", digits, "f"), .)
                ) %>%
                knitr::kable(
                    "html",
                    # caption = "ANOVA (parametric)",
                    align = c("l", rep("c", 5)),
                    escape = FALSE,
                    col.names = c(
                        " ", "Sum of Squares", "Df",
                        "Mean Square",
                        "F", "p-value"
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

    # ANOVA effect sizes
    output$anova_effect_sizes <- function() {
        # For one-way ANOVA, eta squared, for without interaction partial eta squared, and for with interaction omega squared are reported
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

    # Kruskal-Wallis test for non-parametric data
    kruskal <- reactive({
        if (missing_inputs()) {
            return(NULL)
        } else {
            kruskal <- list()
            # Run the test for each factor
            for (i in 1:length(input$predictors)) {
                kruskal[[i]] <- kruskal.test(
                    as.formula(paste0(
                        input$outcome, " ~ ", input$predictors[[i]]
                    )),
                    data = df()
                )
            }
            return(kruskal)
        }
    })

    # Effect size for Kruskal-Wallis test
    epsilon2_kw <- reactive({
        if (missing_inputs()) {
            return(NULL)
        } else {
            epsilon2_kw <- list()
            # Run the test for each factor
            for (i in 1:length(input$predictors)) {
                epsilon2_kw[[i]] <- effectsize::rank_epsilon_squared(
                    as.formula(paste0(
                        input$outcome, " ~ ", input$predictors[[i]]
                    )),
                    data = df()
                )
            }
            return(epsilon2_kw)
        }
    })

    # Kruksal-Wallis table
    output$kruskal_table <- function() {
        kruskal_table <- data.frame()
        # If kruskal() is not null, then ...
        if (length(kruskal()) > 0) {
            for (i in 1:length(kruskal())) {
                kruskal_table <- rbind(
                    kruskal_table,
                    data.frame(
                        Variable = kruskal()[[i]]$data.name,
                        Statistic = kruskal()[[i]]$statistic,
                        df = kruskal()[[i]]$parameter,
                        p.value = kruskal()[[i]]$p.value,
                        epsilon2 = epsilon2_kw()[[i]]$rank_epsilon_squared,
                        epsilon2_ci = paste0(
                            sprintf(
                                paste0("%.", digits, "f"),
                                epsilon2_kw()[[i]]$CI_low
                            ),
                            " - ",
                            sprintf(
                                paste0("%.", digits, "f"),
                                epsilon2_kw()[[i]]$CI_high
                            )
                        )
                    )
                )
            }
            kruskal_table %>%
                dplyr::mutate_if(
                    is.numeric,
                    ~ sprintf(paste0("%.", digits, "f"), .)
                ) %>%
                knitr::kable(
                    "html",
                    # caption = "Kruskal-Wallis rank sum test (non-parametric)",
                    align = c("l", rep("c", 5)),
                    escape = FALSE,
                    col.names = c(
                        " ", "χ² Statistic", "df", "p-value",
                        "(ε²)", "95% CI"
                    ),
                    row.names = FALSE
                ) %>%
                kableExtra::kable_classic(
                    full_width = FALSE,
                    html_font = "inherit",
                    position = "left"
                ) %>%
                kableExtra::add_header_above(
                    c(
                        " " = 1,
                        "Kruskal-Wallis rank sum test" = 3,
                        "Effect size" = 2
                    )
                )
        }
    }
}
