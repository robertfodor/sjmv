# This is the descriptive statistics tab for ShinyStat.
# It takes as input the datafile uploaded in the Getting Started tab.
library(shiny)
library(dplyr) # for data manipulation
library(stringr) # for string manipulation
library(boot) # for bootstrapping
library(e1071) # skewness and kurtosis type 2
library(nortest) # Lilliefors test
library(shinyWidgets) # for custom widgets
library(knitr) # for kable
library(kableExtra) # extra formatting for kable

# Source plots
source("modules/plots.R")

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
                    uiOutput(ns("plots"))
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
.mean_ci_bootstrapped <- function(
    x,
    ci_level = 0.95, sample_size = 1000, type = "perc") {
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
.skewness_ci_bootstrapped <- function(
    x,
    ci_level = 0.95, sample_size = 1000, type = "perc") {
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
.kurtosis_ci_bootstrapped <- function(
    x,
    ci_level = 0.95, sample_size = 1000, type = "perc") {
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
.sd_ci_bootstrapped <- function(
    x,
    ci_level = 0.95, sample_size = 1000, type = "perc") {
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
.var_ci_bootstrapped <- function(
    x,
    ci_level = 0.95, sample_size = 1000, type = "perc") {
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
    return(IQR(x, na.rm = TRUE))
}
.min <- function(x) {
    return(min(x, na.rm = TRUE))
}
.max <- function(x) {
    return(max(x, na.rm = TRUE))
}
#    Normality
.shapiro <- function(x) {
    return(shapiro.test(x)$statistic[[1]])
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
##   Creates enumerated names to put into header
.count_unique_names <- function(name_list) {
    # Initialize an empty list to store the counts
    counts <- list()

    # Loop through each element in name_list
    for (name in name_list) {
        # If the element is not in the counts list, add it with count 1
        if (!(name %in% names(counts))) {
            counts[[name]] <- 1
        } else {
            counts[[name]] <- counts[[name]] + 1
        }
    }

    # Return the counts list
    return(counts)
}

.get_main_parent_names <- function(df) {
    # Initialize an empty list to store the parent names
    main_parent_names <- list()

    # Loop through each column name extract the bits before the first dot
    for (name in names(df)) {
        main_parent_name <- gsub("\\..*", "", name)
        main_parent_names[[name]] <- main_parent_name
    }

    #  Sentence case the main parent names and replace underscores with spaces
    main_parent_names <- stringr::str_replace_all(
        stringr::str_to_sentence(main_parent_names),
        "_", " "
    )

    return(main_parent_names)
}

.remove_main_parent <- function(df) {
    # Delete beginning of the string until the first dot in each column name
    names(df) <- gsub("^[^.]*\\.", "", names(df))
    return(df)
}

.get_parent_names <- function(df) {
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

.remove_fun <- function(df) {
    # If has dual .fun. in name, remove everything before it including .fun.
    names(df) <- gsub(".*\\.fun\\.", "", names(df))
    # Then if it ends with .fun, remove it
    names(df) <- gsub("\\.fun$", "", names(df))

    return(df)
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
            "Lilliefors corrected p" = .ks_p_lilliefors
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

    #  Combine all checked statistics into a list
    checked <- reactive({
        # Initialize an empty list to store the checked statistics
        checked <- list()

        # Loop through each checkbox input
        for (stat in names(statistics)) {
            # If the checkbox is checked, add the statistic to the list
            if (length(input[[stat]]) > 0) {
                checked[[stat]] <- statistics[[stat]][input[[stat]]]
            }
        }

        # Return the list of checked statistics
        return(checked)
    })

    # Generate descriptive table
    output$descriptives <- function() {
        if (!is.null(df()) && length(checked()) > 0) {
            stat_functions <- unlist(checked())
            # Apply each statistic to the data frame
            stats <- apply(df(), 2, function(x) {
                lapply(stat_functions, function(f) f(x))
            })
            # Combine stats into a table
            stats_df <- data.frame(t(sapply(stats, unlist)))
            # Get the main parent names to a list and cut it off
            main_parent_names <- .get_main_parent_names(stats_df)
            stats_df <- .remove_main_parent(stats_df)
            ## Get intermediate parent if multiple output columns
            parent_names <- .get_parent_names(stats_df)
            stats_df <- .remove_fun(stats_df)

            # Format table
            stats_df %>%
                dplyr::mutate_if(
                    is.numeric,
                    ~ sprintf(paste0("%.", digits, "f"), .)
                ) %>%
                kable(
                    "html",
                    align = c("l", rep("c", length(stats_df) - 1)),
                    caption = "Descriptive statistics",
                    row.names = TRUE,
                    escape = TRUE
                ) %>%
                kableExtra::kable_classic(
                    full_width = FALSE,
                    html_font = "inherit",
                    position = "left"
                ) %>%
                kableExtra::add_header_above(
                    c(" " = 1, .count_unique_names(parent_names))
                ) %>%
                kableExtra::add_header_above(
                    c(" " = 1, .count_unique_names(main_parent_names))
                )
        }
    }
}
