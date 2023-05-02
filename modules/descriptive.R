# This is the descriptive statistics tab for ShinyStat.
# It takes as input the datafile uploaded in the Getting Started tab.
library(shiny)
library(dplyr) # for data manipulation
library(boot) # for bootstrapping
library(ggplot2) # for plotting
library(e1071) # skewness and kurtosis type 2
library(nortest) # Lilliefors test
library(shinyWidgets) # for custom widgets
library(knitr) # for kable
library(kableExtra) # extra formatting for kable

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
                    width = 6,
                    awesomeCheckboxGroup(
                        inputId = ns("central_tendency"),
                        label = "Central tendency",
                        choices = c(
                            "N", "Missing", "Mean", "Mean SE", "Mean CI",
                            "Median", "Mode"
                        ),
                        selected = c("N", "Mean", "Median")
                    ),
                    awesomeCheckboxGroup(
                        inputId = ns("skewness_kurtosis"),
                        label = "Skewness and kurtosis",
                        choices = c("Skewness", "Kurtosis"),
                        selected = c("Skewness", "Kurtosis")
                    )
                ),
                column(
                    width = 6,
                    awesomeCheckboxGroup(
                        inputId = ns("dispersion"),
                        label = "Dispersion",
                        choices = c(
                            "SD", "Var", "Range", "IQR", "Min", "Max"
                        ),
                        selected = c("SD", "IQR")
                    ),
                    awesomeCheckboxGroup(
                        inputId = ns("normality"),
                        label = "Normality",
                        choices = c(
                            "Shapiro-Wilk",
                            "Kolmogorov-Smirnov"
                        ),
                        selected = c("Shapiro-Wilk")
                    )
                )
            ),
            fluidRow(
                column(
                    width = 12,
                    h4("Results"),
                    tableOutput(ns("descriptives")),
                    tableOutput(ns("normality_table"))
                )
            )
        )
    )
}

# Statistics
#    Central tendency
.n <- function(x) {
    return(length(x) - sum(is.na(x)))
}
.missing <- function(x) {
    return(sum(is.na(x)))
}
.mean <- function(x) {
    return(mean(x, na.rm = TRUE))
}
.mean_se <- function(x) {
    return(sd(x, na.rm = TRUE) / sqrt(.n(x)))
}
.mean_ci_core <- function(x, ci_level = 0.95) {
    return(t.test(x, conf.level = ci_level)$conf.int)
}
.mean_ci_bootstrapped <- function(x, ci_level = 0.95, sample_size = 1000, type = "perc") {
    boot <- boot::boot(x, mean, R = sample_size)
    if (type == "perc") {
        return(boot::boot.ci(boot, conf = ci_level, type = "perc")$perc[4:5])
    } else if (type == "bca") {
        return(boot::boot.ci(boot, conf = ci_level, type = "bca")$bca[4:5])
    } else {
        stop("Invalid type.")
    }
}
.mean_se_bootstrapped <- function(x, sample_size = 1000) {
    boot <- boot::boot(x, mean, R = sample_size)
    if (type == "perc") {
        return(boot::boot.ci(boot, conf = ci_level, type = "perc")$perc[3])
    } else if (type == "bca") {
        return(boot::boot.ci(boot, conf = ci_level, type = "bca")$bca[3])
    } else {
        stop("Invalid type.")
    }
}
.median <- function(x) {
    return(median(x, na.rm = TRUE))
}
.mode <- function(x) {
    return(as.numeric(names(sort(-table(x)))[1]))
}
#    Skewness and kurtosis
.skewness <- function(x) {
    return(e1071::skewness(x, type = 2))
}
.skewness_se <- function(x) {
    return(sd(x, na.rm = TRUE) / sqrt(.n(x)))
}
.skewness_ci_core <- function(x, ci_level = 0.95) {
    return(t.test(x, conf.level = ci_level)$conf.int)
}
.skewness_ci_bootstrapped <- function(x, ci_level = 0.95, sample_size = 1000, type = "perc") {
    boot <- boot::boot(x, e1071::skewness, R = sample_size)
    if (type == "perc") {
        return(boot::boot.ci(boot, conf = ci_level, type = "perc")$perc[4:5])
    } else if (type == "bca") {
        return(boot::boot.ci(boot, conf = ci_level, type = "bca")$bca[4:5])
    } else {
        stop("Invalid type.")
    }
}
.skewness_se_bootstrapped <- function(x, sample_size = 1000) {
    boot <- boot::boot(x, e1071::skewness, R = sample_size)
    if (type == "perc") {
        return(boot::boot.ci(boot, conf = ci_level, type = "perc")$perc[3])
    } else if (type == "bca") {
        return(boot::boot.ci(boot, conf = ci_level, type = "bca")$bca[3])
    } else {
        stop("Invalid type.")
    }
}
.kurtosis <- function(x) {
    return(e1071::kurtosis(x, type = 2))
}
.kurtosis_se <- function(x) {
    return(sd(x, na.rm = TRUE) / sqrt(.n(x)))
}
.kurtosis_ci_core <- function(x, ci_level = 0.95) {
    return(t.test(x, conf.level = ci_level)$conf.int)
}
.kurtosis_ci_bootstrapped <- function(x, ci_level = 0.95, sample_size = 1000, type = "perc") {
    boot <- boot::boot(x, e1071::kurtosis, R = sample_size)
    if (type == "perc") {
        return(boot::boot.ci(boot, conf = ci_level, type = "perc")$perc[4:5])
    } else if (type == "bca") {
        return(boot::boot.ci(boot, conf = ci_level, type = "bca")$bca[4:5])
    } else {
        stop("Invalid type.")
    }
}
.kurtosis_se_bootstrapped <- function(x, sample_size = 1000) {
    boot <- boot::boot(x, e1071::kurtosis, R = sample_size)
    if (type == "perc") {
        return(boot::boot.ci(boot, conf = ci_level, type = "perc")$perc[3])
    } else if (type == "bca") {
        return(boot::boot.ci(boot, conf = ci_level, type = "bca")$bca[3])
    } else {
        stop("Invalid type.")
    }
}
#    Dispersion
.sd <- function(x) {
    return(sd(x, na.rm = TRUE))
}
.sd_ci_core <- function(x, ci_level = 0.95) {
    return(t.test(x, conf.level = ci_level)$conf.int)
}
.sd_ci_bootstrapped <- function(x, ci_level = 0.95, sample_size = 1000, type = "perc") {
    boot <- boot::boot(x, sd, R = sample_size)
    if (type == "perc") {
        return(boot::boot.ci(boot, conf = ci_level, type = "perc")$perc[4:5])
    } else if (type == "bca") {
        return(boot::boot.ci(boot, conf = ci_level, type = "bca")$bca[4:5])
    } else {
        stop("Invalid type.")
    }
}
.var <- function(x) {
    return(var(x, na.rm = TRUE))
}
.var_ci_core <- function(x, ci_level = 0.95) {
    return(t.test(x, conf.level = ci_level)$conf.int)
}
.var_ci_bootstrapped <- function(x, ci_level = 0.95, sample_size = 1000, type = "perc") {
    boot <- boot::boot(x, var, R = sample_size)
    if (type == "perc") {
        return(boot::boot.ci(boot, conf = ci_level, type = "perc")$perc[4:5])
    } else if (type == "bca") {
        return(boot::boot.ci(boot, conf = ci_level, type = "bca")$bca[4:5])
    } else {
        stop("Invalid type.")
    }
}
.range <- function(x) {
    return(max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}
.iqr <- function(x) {
    return(
        quantile(x, probs = 0.75, na.rm = TRUE) -
            quantile(x, probs = 0.25, na.rm = TRUE)
    )
}
.min <- function(x) {
    return(min(x, na.rm = TRUE))
}
.max <- function(x) {
    return(max(x, na.rm = TRUE))
}
#    Normality
.shapiro <- function(x) {
    return(shapiro.test(x)$statistic)
}
.shapiro_p <- function(x) {
    return(shapiro.test(x)$p.value)
}
.ks_d <- function(x) {
    return(nortest::lillie.test(x)$statistic)
}
.ks_p_lilliefors <- function(x) {
    return(nortest::lillie.test(x)$p.value)
}


#  Helper functions
count_unique_names <- function(name_list) {
    # Initialize an empty list to store the counts
    counts <- list()

    # Loop through each element in name_list
    for (name in name_list) {
        # If the element is not in the counts list, add it with count 1
        if (!(name %in% names(counts))) {
            counts[[name]] <- 1
        }
        # If the element is already in the counts list, increment its count
        else {
            counts[[name]] <- counts[[name]] + 1
        }
    }

    # Return the counts list
    return(counts)
}

remove_fun <- function(df) {
    names(df) <- gsub(".*\\.fun\\.", "", names(df))
    return(df)
}

get_parent_names <- function(df) {
    # Initialize an empty list to store the parent names
    parent_names <- list()

    # Loop through each column name in the data frame
    for (name in names(df)) {
        # If the column name contains ".fun.", extract the parent name before it
        if (grepl("\\.fun\\.", name)) {
            parent_name <- gsub("\\.fun\\..*", "", name)
        } else {
            parent_name <- " "
        }

        # Add the parent name to the list
        parent_names[[name]] <- parent_name
    }

    # Return the list of parent names
    return(parent_names)
}


# Define a named list of statistics functions for each checkbox input
statistics <- list(
    central_tendency = list(
        N = list(fun = .n),
        Missing = list(fun = .missing),
        Mean = list(fun = .mean),
        "Mean SE" = list(fun = .mean_se),
        "Mean CI" = list(fun = .mean_ci_core),
        "Mean CI Bootstrapped" = list(fun = .mean_ci_bootstrapped),
        "Mean SE Bootstrapped" = list(fun = .mean_se_bootstrapped),
        Median = list(fun = .median),
        Mode = list(fun = .mode)
    ),
    skewness_kurtosis = list(
        Skewness = list(fun = .skewness),
        "Skewness SE" = list(fun = .skewness_se),
        "Skewness CI" = list(fun = .skewness_ci_core),
        "Skewness CI Bootstrapped" = list(fun = .skewness_ci_bootstrapped),
        "Skewness SE Bootstrapped" = list(fun = .skewness_se_bootstrapped),
        Kurtosis = list(fun = .kurtosis),
        "Kurtosis SE" = list(fun = .kurtosis_se),
        "Kurtosis CI" = list(fun = .kurtosis_ci_core),
        "Kurtosis CI Bootstrapped" = list(fun = .kurtosis_ci_bootstrapped),
        "Kurtosis SE Bootstrapped" = list(fun = .kurtosis_se_bootstrapped)
    ),
    dispersion = list(
        SD = list(fun = .sd),
        "SD CI" = list(fun = .sd_ci_core),
        "SD CI Bootstrapped" = list(fun = .sd_ci_bootstrapped),
        Var = list(fun = .var),
        "Var CI" = list(fun = .var_ci_core),
        "Var CI Bootstrapped" = list(fun = .var_ci_bootstrapped),
        Range = list(fun = .range),
        IQR = list(fun = .iqr),
        Min = list(fun = .min),
        Max = list(fun = .max)
    ),
    normality = list(
        "Shapiro-Wilk" = list(fun = list(
            "W" = .shapiro,
            "p-value" = .shapiro_p
        )),
        "Kolmogorov-Smirnov" = list(fun = list(
            "D" = .ks_d,
            "Lilliefors corr. p" = .ks_p_lilliefors
        ))
    )
)

descriptive_server <- function(input, output, session,
                               file_input, non_factor_variables,
                               digits, ci_level,
                               bootstrap_method, bootstrap_sample_size) {
    # UI renders
    #    Update the choices of the pickerInput based on non_factor_variables
    observeEvent(
        non_factor_variables,
        handlerExpr = {
            updatePickerInput(
                session = session,
                inputId = "var",
                choices = non_factor_variables,
                # When digits are changed it shouldn't reset.
                selected = isolate(input$var)
            )
        }
    )

    # Data source
    df <- reactive({
        if (length(input$var) > 0) {
            file_input$df %>%
                dplyr::select(input$var)
        } else {
            return(NULL)
        }
    })

    #  Combine all checked statistics into a list
    checked <- reactive({
        # Initialize an empty list to store the checked statistics
        checked <- list()

        # Loop through each checkbox input
        for (stat in names(statistics)) {
            # If the checkbox is checked, add the corresponding statistics to the list
            if (length(input[[stat]]) > 0) {
                checked[[stat]] <- statistics[[stat]][input[[stat]]]
            }
        }

        # Return the list of checked statistics
        return(checked)
    })

    # Generate normality table
    output$normality_table <- function() {
        req(df())
        if (length(input$var) == 0 || length(input$normality) == 0) {
            return(NULL)
        }

        stat_functions <- unlist(statistics$normality[input$normality])
        stats <- lapply(df(), function(x) {
            lapply(stat_functions, function(f) f(x))
        })

        # Combine stats into a table
        stats_df <- as.data.frame(stats)
        names(stats_df) <- names(stat_functions)
        parent_names <- get_parent_names(stats_df)
        stats_df <- remove_fun(stats_df)
        rownames(stats_df) <- input$var

        # Format table
        kable(stats_df,
            "html",
            align = c("l", rep("c", length(stats_df) - 1)),
            caption = "Tests for normality",
            digits = digits,
            escape = TRUE
        ) %>%
            kableExtra::kable_classic(
                full_width = FALSE,
                html_font = "inherit",
                position = "left"
            ) %>%
            kableExtra::add_header_above(
                c(" " = 1, count_unique_names(parent_names))
            )
    }

    # Generate descriptive table
    output$descriptives <- function() {
        req(df())
        if (length(input$var) == 0 || length(input$normality) == 0) {
            return(NULL)
        }

        stat_functions <- unlist(checked())
        stats <- lapply(df(), function(x) {
            lapply(stat_functions, function(f) f(x))
        })

        # Combine stats into a table
        stats_df <- as.data.frame(stats)
        names(stats_df) <- names(stat_functions)
        parent_names <- get_parent_names(stats_df)
        stats_df <- remove_fun(stats_df)
        rownames(stats_df) <- input$var

        # Format table
        kable(stats_df,
            "html",
            align = c("l", rep("c", length(stats_df) - 1)),
            caption = "Descriptive statistics",
            digits = digits,
            escape = TRUE
        ) %>%
            kableExtra::kable_classic(
                full_width = FALSE,
                html_font = "inherit",
                position = "left"
            ) %>%
            kableExtra::add_header_above(
                c(" " = 1, count_unique_names(parent_names))
            )
    }
}
