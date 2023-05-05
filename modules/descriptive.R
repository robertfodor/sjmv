# This is the descriptive statistics tab for ShinyStat.
# It takes as input the datafile uploaded in the Getting Started tab.
library(shiny)
library(tidyverse) # for data manipulation
library(stringr) # for string manipulation
library(e1071) # skewness and kurtosis type 2
library(nortest) # Lilliefors test
library(shinyWidgets) # for custom widgets
library(knitr) # for kable
library(kableExtra) # extra formatting for kable

# Define UI
descriptive_ui <- function(id) {
    ns <- NS(id)
    tagList(
        fluidPage(
            fluidRow(column(
                width = 12,
                h2("Descriptive Statistics"),
                pickerInput(
                    inputId = ns("var"),
                    label = "Select variables:",
                    multiple = TRUE,
                    choices = NULL
                )
            )),
            fluidRow(
                column(
                    width = 4,
                    awesomeCheckboxGroup(
                        inputId = ns("sample"),
                        label = "Sample",
                        choices = c("N", "Missing"),
                        selected = c("N")
                    ),
                    awesomeCheckboxGroup(
                        inputId = ns("central_tendency"),
                        label = "Central tendency",
                        choices = c(
                            "Mean", "Std Error of Mean", "Mean CI",
                            "Median", "Mode"
                        ),
                        selected = c("Mean", "Std Error of Mean")
                    )
                ),
                column(
                    width = 4,
                    awesomeCheckboxGroup(
                        inputId = ns("dispersion"),
                        label = "Dispersion",
                        choices = c(
                            "SD", "Variance", "Range", "IQR", "Min", "Max"
                        ),
                        selected = c("SD", "IQR")
                    ),
                    awesomeCheckboxGroup(
                        inputId = ns("asymmetry"),
                        label = "Skewness and kurtosis",
                        choices = c("Skewness", "Kurtosis"),
                        selected = c("Skewness", "Kurtosis")
                    )
                ), column(
                    width = 4,
                    awesomeCheckboxGroup(
                        inputId = ns("normality"),
                        label = "Normality",
                        choices = c(
                            "Shapiro-Wilk",
                            "Kolmogorov-Smirnov"
                        ),
                        selected = c("Shapiro-Wilk")
                    ),
                    materialSwitch(
                        inputId = ns("variables_across"),
                        label = "Show variables across",
                        value = TRUE,
                        status = "info"
                    )
                )
            ),
            fluidRow(
                column(
                    width = 12,
                    h3("Results"),
                    h4("Descriptive statistics"),
                    tableOutput(ns("descriptives"))
                )
            )
        )
    )
}

# Statistical functions
statistics <- tibble(
    filter = c(
        "N", "Missing",
        "Mean", "Std Error of Mean", rep("Mean CI", 2), "Median", "Mode",
        "SD", "Variance", "Range", "IQR", "Min", "Max",
        rep("Skewness", 2), rep("Kurtosis", 2),
        rep("Shapiro-Wilk", 2),
        rep("Kolmogorov-Smirnov", 2)
    ),
    parent_label = c(
        rep("Sample", 2), rep("Central tendency", 6),
        rep("Dispersion", 6),
        rep("Skewness", 2), rep("Kurtosis", 2),
        rep("Shapiro-Wilk test", 2), rep("Kolmogorov-Smirnov test", 2)
    ),
    label = c(
        "N", "Missing",
        "M", "SEM", "CI lower bound", "CI upper bound", "Median", "Mode",
        "SD", "Variance", "Range", "IQR", "Min", "Max",
        "Skewness", "SE", "Kurtosis", "SE",
        "W", "p",
        "D", "p (Lilliefors)"
    ),
    specialformat = c(
        rep("int", 2),
        rep("float", 6 + 6 + 4),
        "float", "p",
        "float", "p"
    ),
    fun = list(
        # Sample
        function(x) length(x) - sum(is.na(x)),
        function(x) sum(is.na(x)),
        # Central tendency
        function(x) mean(x, trim = 0, na.rm = TRUE),
        function(x) sd(x, na.rm = TRUE) / sqrt(length(x) - sum(is.na(x))),
        function(x, ci_level = 0.95) {
            t.test(x, conf.level = ci_level)$conf.int[1]
        },
        function(x, ci_level = 0.95) {
            t.test(x, conf.level = ci_level)$conf.int[2]
        },
        function(x) median(x, na.rm = TRUE),
        function(x) as.numeric(names(sort(-table(x)))[1]),
        # Dispersion
        function(x) sd(x, na.rm = TRUE),
        function(x) var(x, na.rm = TRUE),
        function(x) max(x, na.rm = TRUE) - min(x, na.rm = TRUE),
        function(x) IQR(x, na.rm = TRUE),
        function(x) min(x, na.rm = TRUE),
        function(x) max(x, na.rm = TRUE),
        # Asymmetry
        function(x) e1071::skewness(x, type = 2),
        function(x) sd(x, na.rm = TRUE) / sqrt(length(x) - sum(is.na(x))),
        function(x) e1071::kurtosis(x, type = 2),
        function(x) sd(x, na.rm = TRUE) / sqrt(length(x) - sum(is.na(x))),
        # Normality
        function(x) shapiro.test(x)$statistic[[1]],
        function(x) shapiro.test(x)$p.value,
        function(x) nortest::lillie.test(x)$statistic,
        function(x) nortest::lillie.test(x)$p.value
    )
)

# Function to filter through based on selections
.get_functions <- function(x) {
    funs <- lapply(intersect(x, statistics$filter), function(f) {
        idx <- which(statistics$filter == f)
        if (length(idx) > 1) {
            return(statistics$fun[idx])
        } else {
            return(statistics$fun[[idx]])
        }
    })
    return(funs)
}

# Function to filter through based on selections and return labels
.get_labels <- function(x) {
    labels <- character()
    for (f in intersect(x, statistics$filter)) {
        idx <- which(statistics$filter == f)
        if (length(idx) > 1) {
            labels <- c(labels, statistics$label[idx])
        } else {
            labels <- c(labels, statistics$label[[idx]])
        }
    }
    return(labels)
}

# Function to get back the parent labels from the statistics
.get_parent_labels <- function(x) {
    labels <- character()
    for (f in intersect(x, statistics$filter)) {
        idx <- which(statistics$filter == f)
        if (length(idx) > 1) {
            labels <- c(labels, statistics$parent_label[idx])
        } else {
            labels <- c(labels, statistics$parent_label[[idx]])
        }
    }
    return(labels)
}

# Function to get back the special formats from the statistics
.get_format <- function(x) {
    specialformat <- character()
    for (f in intersect(x, statistics$filter)) {
        idx <- which(statistics$filter == f)
        if (length(idx) > 1) {
            specialformat <- c(specialformat, statistics$specialformat[idx])
        } else {
            specialformat <- c(specialformat, statistics$specialformat[[idx]])
        }
    }
    return(specialformat)
}

# Function to apply formatting
.format_statistics <- function(x, filter, digits, pdigits) {
    formatting <- .get_format(filter)
    x_eval <- x
    for (i in 1:ncol(x)) {
        if (formatting[i] == "int") {
            x[, i] <- formatC(x[, i], format = "d")
        } else if (formatting[i] == "float") {
            x[, i] <- formatC(x[, i], format = "f", digits = digits)
        } else if (formatting[i] == "p") {
            x_eval[, i] <- formatC(x[, i], format = "f", digits = pdigits + 1)
            x[, i] <- formatC(x[, i], format = "f", digits = pdigits)
            for (j in 1:nrow(x)) {
                to <- 3 + pdigits - 1
                zeros <- paste0(rep("0", pdigits), collapse = "")
                repl <- paste0("< .", paste0(rep("0", pdigits - 1), collapse = ""), "1", collapse = "")
                if (substr(x_eval[j, i], 3, to) == zeros) {
                    x[j, i] <- repl
                }
            }
        }
    }
    # Now convert all to character
    for (i in 1:ncol(x)) {
        x[, i] <- as.character(x[, i])
    }

    return(x)
}

# Function to apply the functions to the data
.apply_functions <- function(df, funs, ci_level = NULL) {
    # Apply functions to each column and store results in new data frame
    stats <- data.frame(lapply(df, function(x) {
        unlist(lapply(funs, function(f) {
            if (length(f) > 1) {
                # Check if the function needs the ci_level parameter before passing it
                if ("ci_level" %in% names(formals(f[[1]]))) {
                    return(unlist(lapply(f, function(ff) ff(x, ci_level = ci_level))))
                } else {
                    return(unlist(lapply(f, function(ff) ff(x))))
                }
            } else {
                # Check if the function needs the ci_level parameter before passing it
                if ("ci_level" %in% names(formals(f))) {
                    return(f(x, ci_level = ci_level))
                } else {
                    return(f(x))
                }
            }
        }))
    }))

    return(t(stats))
}

# Define the server for the application
descriptive_server <- function(input, output, session,
                               file_input, non_factor_variables,
                               digits, p_value_digits, ci_level) {
    # UI renders
    #    Update the choices of the pickerInput based on non_factor_variables
    observeEvent(
        non_factor_variables,
        handlerExpr = {
            updatePickerInput(
                session = session,
                inputId = "var",
                choices = non_factor_variables,
                selected = isolate(input$var)
            )
        }
    )

    # Data source
    df <- reactive({
        # Check the input$var items exists in non_factor_variables
        #   and there is at least 1 item selected
        if (length(input$var) > 0 && all(input$var %in% non_factor_variables)) {
            # Return the selected variables from the data frame
            file_input$df %>%
                dplyr::select(input$var)
        } else {
            return(NULL)
        }
    })

    settings <- reactiveValues(
        digits = digits,
        p_value_digits = p_value_digits,
        ci_level = ci_level
    )

    # Generate descriptive table
    output$descriptives <- function() {
        inputfilter <- c(
            input$sample,
            input$central_tendency,
            input$dispersion,
            input$asymmetry,
            input$normality
        )

        if (!is.null(df()) && length(inputfilter) > 0) {
            # Get the functions to apply
            stats_df <- .apply_functions(
                df = df(),
                funs = .get_functions(inputfilter),
                ci_level = settings$ci_level
            )

            # Get the labels
            labels <- .get_labels(inputfilter)
            parent_labels <- .get_parent_labels(inputfilter)
            columnnames <- c()

            # Apply formatting
            stats_df <- stats_df %>%
                as.data.frame()
            stats_df <- .format_statistics(stats_df,
                filter = inputfilter, digits = settings$digits,
                pdigits = settings$p_value_digits
            )

            # Create a vector of stats parent names and count them
            grouping <- factor(
                parent_labels,
                levels = unique(parent_labels),
                labels = unique(parent_labels)
            )

            if (input$variables_across) {
                stats_df <- t(stats_df)
                stats_df <- stats_df %>%
                    cbind(labels, .)
                columnnames <- c("Statistics", unlist(input$var))
            } else {
                stats_df <- stats_df %>%
                    cbind(unlist(input$var), .)
                columnnames <- c("Variables", labels)
            }

            # Format table and return
            stats_df <- stats_df %>%
                kable(
                    "html",
                    align = c("l", rep("c", length(stats_df) - 1)),
                    # caption = "Descriptive statistics",
                    # Must store variables in column due to duplicate row names
                    row.names = FALSE,
                    col.names = columnnames,
                    escape = TRUE
                ) %>%
                kableExtra::kable_classic(
                    full_width = FALSE,
                    html_font = "inherit",
                    position = "left"
                )

            if (input$variables_across) {
                stats_df <- stats_df %>%
                    # Group rows by stats parent
                    kableExtra::group_rows(
                        index = table(grouping),
                        group_label = grouping
                    )
            } else {
                header <- c(c(" " = 1), table(grouping))
                stats_df <- stats_df %>%
                    # Group columns by stats parent
                    kableExtra::add_header_above(
                        header = header
                    )
            }

            # Return
            stats_df
        }
    }
}
