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
library(effectsize)

# Source the helper functions
source("R/variance_helpers.R")

# UI
variance_ui <- function(id) {
    ns <- NS(id)
    tagList(
        fluidPage(
            fluidRow(
                column(
                    width = 12,
                    h1("Variance Analysis"),
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
                        title = "More on ANOVA types", status = "info", solidHeader = TRUE,
                        collapsible = TRUE, collapsed = TRUE, width = "100%",
                        tagList(
                            HTML("<p>If you have one predictor (independent) variable, make sure to select <b>One-way ANOVA</b>.</p>
                                 <p>If you have at least two predictor variables, you'll have to select one of the <b>Factorial</b> ANOVAs.</p>
                                 <p>If your predictors are completely independent of each other, select <b>Factorial ANOVA without interactions</b>. Otherwise, select <b>Factorial ANOVA with interactions</b>.</p>")
                        )
                    )
                )
            ),
            fluidRow(
                column(
                    width = 12,
                    # MODIFICATION: Use uiOutput for dynamic plot height
                    uiOutput(ns("plot_ui")),
                    h2("Assumption checks"),
                    h3("Homogeneity of variance assumption"),
                    tableOutput(ns("homogeneity")),
                    br(),
                    htmlOutput(ns("homogeneity_text")),
                    h3("Normality assumption"),
                    tableOutput(ns("normality")),
                    br(),
                    htmlOutput(ns("normality_text")),
                    tableOutput(ns("equality_of_covariances")),
                    tableOutput(ns("sphericity")),
                    h2("Results"),
                    h3("Fisher's ANOVA (assuming equal variances)"),
                    h5("ANOVA table for Fisher's ANOVA"),
                    tableOutput(ns("anova_results")),
                    br(),
                    h5("Effect sizes (Fisher's ANOVA)"),
                    tableOutput(ns("anova_effect_sizes")),
                    br(),
                    box(
                        title = "Interpretation of Fisher's ANOVA results", status = "success", solidHeader = TRUE,
                        collapsible = TRUE, collapsed = FALSE, width = "100%",
                        tagList(tagAppendAttributes(textOutput(ns("results_text")), style = "white-space: pre-wrap;"))
                    ),
                    h3("Welch's ANOVA (assuming unequal variances)"),
                    h5("ANOVA table for Welch's ANOVA"),
                    tableOutput(ns("anova_results_welch")),
                    h3("Non-parametric tests"),
                    h5("Kruskal-Wallis rank sum test"),
                    tableOutput(ns("kruskal_table")),
                    h2("Post-hoc analysis"),
                    pickerInput(
                        inputId = ns("posthoc_terms"),
                        label = "Select predictor for post-hoc test:",
                        multiple = FALSE,
                        choices = NULL
                    ),
                    tableOutput(ns("tukey_hsd_table"))
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
        updatePickerInput(
            session = session, inputId = "outcome",
            choices = names(file_input$df)[sapply(file_input$df, is.numeric)],
            selected = isolate(input$outcome)
        )
    })

    output$predictor_selector <- renderUI({
        ns <- session$ns
        label <- if (input$type == "One-way ANOVA") "Select categorical grouping variable" else "Select categorical predictor (factor) variables"
        multiple <- if (input$type == "One-way ANOVA") FALSE else TRUE
        pickerInput(
            inputId = ns("predictors"), label = label,
            choices = names(file_input$df)[sapply(file_input$df, is.factor)],
            selected = isolate(input$predictors),
            multiple = multiple
        )
    })

    df <- reactive({
        req(input$outcome, input$predictors)
        file_input$df %>% select(all_of(c(input$outcome, input$predictors)))
    })

    formula <- reactive({
        req(input$outcome, input$predictors)
        collapse <- if (input$type == "Factorial ANOVA without interactions") " + " else " * "
        as.formula(paste(input$outcome, "~", paste(input$predictors, collapse = collapse)))
    })

    model <- reactive({
        shiny::validate(
            shiny::need(input$outcome, "Please select an outcome variable."),
            shiny::need(input$predictors, "Please select at least one predictor variable.")
        )
        if (input$type == "Factorial ANOVA with interactions") {
            factorlist <- setNames(as.list(rep("contr.sum", length(input$predictors))), input$predictors)
            aov(formula = formula(), data = df(), contrasts = factorlist)
        } else {
            aov(formula = formula(), data = df())
        }
    })

    # MODIFICATION: Dynamic plot UI generation
    output$plot_ui <- renderUI({
        ns <- session$ns
        req(input$predictors)
        num_predictors <- length(input$predictors)
        # Calculate height: 400px for the first predictor, 350px for each additional one
        plot_height <- 400 + (num_predictors - 1) * 350
        plotOutput(ns("boxplot"), height = paste0(plot_height, "px"))
    })

    output$boxplot <- renderPlot(
        {
            req(model())
            num_predictors <- length(input$predictors)
            # Set layout to have 2 columns (Boxplot, QQ plot) and rows for each predictor
            layout(matrix(1:(num_predictors * 2), ncol = 2, byrow = TRUE))

            par(cex.axis = 0.8, cex.lab = 0.8, cex.main = 0.9)

            for (pred in input$predictors) {
                single_pred_formula <- as.formula(paste(input$outcome, "~", pred))
                boxplot(
                    formula = single_pred_formula, data = df(), xlab = pred, ylab = input$outcome,
                    main = paste("Boxplot:", input$outcome, "by", pred)
                )

                temp_lm <- lm(formula = single_pred_formula, data = df())
                qqnorm(residuals(temp_lm), main = paste("QQ Plot of Residuals (", pred, ")"))
                qqline(residuals(temp_lm))
            }
        },
        res = 100
    )

    levene <- reactive({
        req(model())
        # Call helper function
        run_levene_test(formula(), df(), input$type, digits)
    })

    output$homogeneity <- function() {
        req(levene())
        levene() %>%
            knitr::kable("html",
                align = c("l", rep("c", 4)), escape = FALSE,
                col.names = c(" ", "F statistic", "df1", "df2", "p-value")
            ) %>%
            kableExtra::kable_classic(full_width = FALSE, html_font = "inherit", position = "left")
    }

    output$homogeneity_text <- renderUI({
        req(levene())
        p_val <- as.numeric(levene()$p.value[1])
        if (p_val < 0.05) {
            HTML("<p>The homogeneity of variance assumption is <b>violated</b> (p < .05). We assume <i>unequal variances</i>.</p><p><b>Welch's ANOVA</b> is recommended.</p>")
        } else {
            HTML("<p>The homogeneity of variance assumption is <b>not violated</b> (p >= .05). We can assume <i>equal variances</i>.</p><p><b>Fisher's ANOVA</b> is acceptable.</p>")
        }
    })

    shapiro <- reactive({
        req(model())
        shapiro.test(x = residuals(model()))
    })

    output$normality <- function() {
        req(shapiro())
        res <- shapiro()
        normality <- data.frame(
            Test = res$method,
            Statistic = res$statistic,
            p.value = if (res$p.value < 0.001) "< 0.001" else sprintf(paste0("%.", digits, "f"), res$p.value)
        )
        normality %>%
            dplyr::mutate_if(is.numeric, ~ sprintf(paste0("%.", digits, "f"), .)) %>%
            knitr::kable("html",
                align = c("l", "c", "c"), escape = FALSE,
                col.names = c(" ", "W statistic", "p-value"), row.names = FALSE
            ) %>%
            kableExtra::kable_classic(full_width = FALSE, html_font = "inherit", position = "left")
    }

    output$normality_text <- renderUI({
        req(shapiro())
        if (shapiro()$p.value < 0.05) {
            HTML("<p>The normality assumption is <b>violated</b> (p < .05). The residuals are not normally distributed.</p><p><b>Non-parametric tests</b> (e.g., Kruskal-Wallis) are recommended.</p>")
        } else {
            HTML("<p>The normality assumption is <b>not violated</b> (p >= .05).</p>")
        }
    })

    overall_model_test <- reactive({
        req(model())
        lm(formula(), data = df())
    })

    output$anova_results <- function() {
        req(model())
        # The logic for building the specific ANOVA table remains here as it's highly tied to the UI inputs
        # ... (ANOVA table logic remains unchanged)
        # This part is complex and specific to the UI state, so leaving it here is reasonable.
        model_data <- data.frame(
            Term = "Overall model test",
            SS = sum((overall_model_test()$fitted.values - mean(overall_model_test()$model[[input$outcome]]))^2),
            Df = summary(overall_model_test())$fstatistic[2],
            MS = sum((overall_model_test()$fitted.values - mean(overall_model_test()$model[[input$outcome]]))^2) / summary(overall_model_test())$fstatistic[2],
            F = summary(overall_model_test())$fstatistic[1],
            p.value = pf(summary(overall_model_test())$fstatistic[1], summary(overall_model_test())$fstatistic[2], summary(overall_model_test())$fstatistic[3], lower.tail = FALSE)
        )
        if (input$type == "Factorial ANOVA with interactions") {
            model_intercept <- as.data.frame(broom::tidy(car::Anova(model(), type = 3)))
            intercept <- data.frame(Term = model_intercept[[1]], SS = model_intercept[[2]], Df = model_intercept[[3]], MS = model_intercept[[2]] / model_intercept[[3]], F = model_intercept[[4]], p.value = model_intercept[[5]])
            colnames(intercept) <- colnames(model_data)
            model_data <- rbind(model_data, intercept)
        } else {
            model_nonintercept <- as.data.frame(broom::tidy(model()))
            nonintercept <- data.frame(Term = model_nonintercept[[1]], SS = model_nonintercept[[3]], Df = model_nonintercept[[2]], MS = model_nonintercept[[4]], F = model_nonintercept[[5]], p.value = model_nonintercept[[6]])
            colnames(nonintercept) <- colnames(model_data)
            model_data <- rbind(model_data, nonintercept)
        }
        model_data %>%
            dplyr::mutate_if(is.numeric, ~ sprintf(paste0("%.", digits, "f"), .)) %>%
            knitr::kable("html",
                align = c("l", rep("c", 5)), escape = FALSE,
                col.names = c(" ", "Sum of Squares", "Df", "Mean Square", "F", "p-value"), row.names = FALSE
            ) %>%
            kableExtra::kable_classic(full_width = FALSE, html_font = "inherit", position = "left")
    }

    output$anova_results_welch <- function() {
        # ... (Welch's ANOVA logic remains unchanged)
        req(model())
        welch_model <- oneway.test(formula(), data = df(), var.equal = FALSE)
        effect_eta <- effectsize::eta_squared(welch_model, partial = FALSE, verbose = FALSE)
        effect_omega <- effectsize::omega_squared(welch_model, partial = FALSE, verbose = FALSE)
        welchanova <- data.frame(
            Term = "Overall model", F = welch_model$statistic, df1 = welch_model$parameter[1], df2 = welch_model$parameter[2], p.value = welch_model$p.value,
            eta_squared = effect_eta[[1]],
            eta_squared_CI = paste(sprintf(paste0("%.", digits, "f"), effect_eta[[3]]), sprintf(paste0("%.", digits, "f"), effect_eta[[4]]), sep = "–"),
            omega_squared = effect_omega[[1]],
            omega_squared_CI = paste(sprintf(paste0("%.", digits, "f"), effect_omega[[3]]), sprintf(paste0("%.", digits, "f"), effect_omega[[4]]), sep = "–")
        )
        welchanova %>%
            dplyr::mutate_if(is.numeric, ~ sprintf(paste0("%.", digits, "f"), .)) %>%
            knitr::kable("html",
                align = c("l", rep("c", 8)),
                col.names = c(" ", "F", "df1", "df2", "p-value", "η²", "95% CI", "ω²", "95% CI"), row.names = FALSE
            ) %>%
            kableExtra::kable_classic(full_width = FALSE, html_font = "inherit", position = "left") %>%
            kableExtra::add_header_above(c(" " = 1, "Welch's ANOVA" = 4, "Eta squared" = 2, "Omega squared" = 2)) %>%
            kableExtra::add_header_above(c(" " = 5, "Effect size" = 4))
    }

    output$anova_effect_sizes <- function() {
        req(model())
        # Call helper function
        effect_sizes <- calculate_anova_effect_sizes(model(), digits)

        # Display logic remains here as it's about presentation
        if (length(input$predictors) < 2) {
            display_df <- effect_sizes %>% select(Term, Eta2, Eta2_CI, Eta2_interp, Omega2, Omega2_CI, Omega2_interp)
            col_names <- c(" ", "η²", "95% CI", " ", "ω²", "95% CI", " ")
            header <- c(" " = 1, "Eta squared" = 3, "Omega squared" = 3)
        } else {
            display_df <- effect_sizes
            col_names <- c(" ", "η²", "95% CI", " ", "η²p", "95% CI", " ", "ω²", "95% CI", " ", "ω²p", "95% CI", " ")
            header <- c(" " = 1, "Eta squared" = 3, "Partial eta sq." = 3, "Omega squared" = 3, "Partial omega sq." = 3)
        }

        display_df %>%
            knitr::kable("html", align = c("l", rep("c", ncol(display_df) - 1)), escape = FALSE, col.names = col_names, row.names = FALSE) %>%
            kableExtra::kable_classic(full_width = FALSE, html_font = "inherit", position = "left") %>%
            kableExtra::add_header_above(header) %>%
            kableExtra::footnote(
                general_title = "Note on interpretation:",
                general = "Effect size interpretation follows Field (2013).", escape = FALSE
            )
    }

    output$results_text <- renderText({
        req(model())
        report::report_text(model()) %>% gsub("Eta2", "\u03B7\u00B2", .)
    })

    output$kruskal_table <- function() {
        req(model())
        # Call helper function
        kruskal_df <- run_kruskal_wallis(input$outcome, input$predictors, df(), digits)

        req(nrow(kruskal_df) > 0)

        kruskal_df %>%
            dplyr::mutate_if(is.numeric, ~ sprintf(paste0("%.", digits, "f"), .)) %>%
            knitr::kable("html",
                align = c("l", rep("c", 5)), escape = FALSE,
                col.names = c(" ", "χ² Statistic", "df", "p-value", "ε²", "95% CI"), row.names = FALSE
            ) %>%
            kableExtra::kable_classic(full_width = FALSE, html_font = "inherit", position = "left") %>%
            kableExtra::add_header_above(c(" " = 1, "Kruskal-Wallis rank sum test" = 3, "Effect size" = 2))
    }

    observeEvent(input$predictors, {
        req(model())
        selected_terms <- unlist(input$predictors)
        if (length(input$predictors) > 1 && input$type == "Factorial ANOVA with interactions") {
            selected_terms <- c(selected_terms, paste(selected_terms, collapse = ":"))
        }
        updatePickerInput(session, "posthoc_terms", choices = selected_terms, selected = selected_terms[1])
    })

    output$tukey_hsd_table <- function() {
        # ... (Tukey HSD logic remains unchanged as it's complex and UI-dependent)
        req(model(), input$posthoc_terms)
        tukey_df <- as.data.frame(TukeyHSD(model(), which = input$posthoc_terms)[[1]]) %>%
            tibble::rownames_to_column(var = "terms") %>%
            tidyr::separate(terms, c("group1", "group2"), sep = "-") %>%
            tidyr::unnest(cols = c(group1, group2))

        if (grepl(":", tukey_df$group1[1])) {
            tukey_df <- tukey_df %>%
                tidyr::separate(group1, c("term1a", "term2a"), sep = ":") %>%
                tidyr::separate(group2, c("term1b", "term2b"), sep = ":") %>%
                tidyr::unnest(cols = c(term1a, term2a, term1b, term2b))
        }

        if ("term1a" %in% names(tukey_df)) {
            tukey_df <- tukey_df %>% dplyr::arrange(term1a, term2a)
            compa <- 4
            col_names_base <- unlist(strsplit(input$posthoc_terms, ":"))
            col_names <- c(col_names_base[1], col_names_base[2], col_names_base[1], col_names_base[2])
        } else {
            tukey_df <- tukey_df %>% dplyr::arrange(group1, group2)
            compa <- 2
            col_names_base <- unlist(strsplit(input$posthoc_terms, ":"))
            col_names <- c(col_names_base[1], col_names_base[1])
        }

        final_col_names <- c(col_names, "Mean Difference", "Lower Bound", "Upper Bound", "p-value")

        tukey_df %>%
            dplyr::mutate_if(is.numeric, ~ sprintf(paste0("%.", digits, "f"), .)) %>%
            knitr::kable("html",
                caption = "Tukey's HSD test (equal variances assumed)",
                align = c("l", rep("c", ncol(tukey_df) - 1)), escape = FALSE, row.names = FALSE, col.names = final_col_names
            ) %>%
            kableExtra::kable_classic(full_width = FALSE, html_font = "inherit", position = "left") %>%
            kableExtra::add_header_above(c("Comparison" = compa, " " = 4))
    }
}
