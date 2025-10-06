# R/plots.R
library(ggplot2)
library(ggrepel)
library(dplyr)

source("R/themes.R")

#' Generate a custom plot based on user specifications
#'
#' This master function takes all plot-related inputs and generates the appropriate ggplot object.
#'
#' @param data The dataframe, expected to contain an `original_row_id` column.
#' @param plot_type The type of plot to generate (e.g., "Box Plot", "Histogram").
#' @param y_var The variable for the y-axis (or primary variable).
#' @param x_var The variable for the x-axis (for scatter plots).
#' @param split_var A factor variable to split/group the data by.
#' @param ... Additional arguments for aesthetics and options (e.g., label_outliers, show_points, show_mean).
#' @return A ggplot object.
generate_custom_plot <- function(data, plot_type, y_var, x_var = NULL, split_var = NULL, ...) {
    opts <- list(...)

    p_base <- ggplot(data)

    # Base aesthetic for split vs. non-split plots
    aes_map <- if (!is.null(split_var) && split_var != "") {
        aes(x = .data[[split_var]], y = .data[[y_var]], fill = .data[[split_var]], color = .data[[split_var]])
    } else {
        aes(x = factor(1), y = .data[[y_var]])
    }

    p <- switch(plot_type,
        "Box Plot" = {
            p <- ggplot(data, aes_map) +
                geom_boxplot(outlier.shape = NA, alpha = 0.5, width = 0.5) +
                labs(title = paste("Distribution of", y_var), y = y_var, x = split_var %||% "")

            if (isTRUE(opts$show_points)) {
                # Use position_jitterdodge for split plots to keep points within their boxplot
                p <- p + geom_jitter(alpha = 0.5, position = position_jitterdodge(jitter.width = 0.5, dodge.width = 0.75), show.legend = FALSE)
            }

            if (isTRUE(opts$label_outliers)) {
                outlier_data <- data %>%
                    # Group by the split variable if it exists, otherwise it's a single group
                    group_by(across(all_of(split_var))) %>%
                    mutate(is_outlier = .data[[y_var]] %in% boxplot.stats(.data[[y_var]])$out) %>%
                    filter(is_outlier) %>%
                    ungroup()

                if (nrow(outlier_data) > 0) {
                    p <- p +
                        geom_point(data = outlier_data, color = "black", show.legend = FALSE) +
                        ggrepel::geom_text_repel(
                            data = outlier_data, aes(label = original_row_id),
                            color = "black", na.rm = TRUE, segment.alpha = 0, show.legend = FALSE
                        )
                }
            }

            if (isTRUE(opts$show_mean)) {
                p <- p + stat_summary(
                    fun = mean, geom = "point", shape = 23, size = 3,
                    fill = "white", color = "black", na.rm = TRUE, show.legend = FALSE
                )
            }

            if (is.null(split_var) || split_var == "") {
                p <- p + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
            }
            p
        },
        "Histogram" = {
            bins <- if (opts$bins_type == "Sturges") nclass.Sturges(na.omit(data[[y_var]])) else opts$bins_manual
            ggplot(data, aes(x = .data[[y_var]], fill = if (!is.null(split_var) && split_var != "") .data[[split_var]] else NULL)) +
                geom_histogram(bins = bins, position = "identity", alpha = 0.6, color = "black") +
                labs(title = paste("Histogram of", y_var))
        },
        "Density" = {
            ggplot(data, aes(x = .data[[y_var]], fill = if (!is.null(split_var) && split_var != "") .data[[split_var]] else NULL)) +
                geom_density(alpha = 0.6) +
                labs(title = paste("Density Plot of", y_var))
        },
        "Histogram + Density" = {
            bins <- if (opts$bins_type == "Sturges") nclass.Sturges(na.omit(data[[y_var]])) else opts$bins_manual
            p_base <- ggplot(data, aes(x = .data[[y_var]]))
            if (!is.null(split_var) && split_var != "") {
                p_base <- ggplot(data, aes(x = .data[[y_var]], fill = .data[[split_var]], color = .data[[split_var]]))
            }
            p_base +
                geom_histogram(aes(y = after_stat(density)), bins = bins, position = "identity", alpha = 0.5, color = "black") +
                geom_density(aes(color = if (!is.null(split_var) && split_var != "") .data[[split_var]] else NULL), linewidth = 1) +
                labs(title = paste("Histogram and Density of", y_var))
        },
        "Q-Q Plot" = {
            p <- ggplot(data, aes(sample = .data[[y_var]])) +
                stat_qq() +
                stat_qq_line() +
                labs(title = paste("Q-Q Plot of", y_var))
            if (!is.null(split_var) && split_var != "") p <- p + facet_wrap(vars(.data[[split_var]]))
            p
        },
        "Scatter Plot" = {
            shiny::validate(shiny::need(x_var, "Please select a variable for the X-axis for a scatter plot."))
            p_base + geom_point(aes(x = .data[[x_var]], y = .data[[y_var]]), alpha = 0.7) +
                geom_smooth(aes(x = .data[[x_var]], y = .data[[y_var]]), method = "lm", se = FALSE) +
                labs(title = paste("Scatter Plot of", y_var, "vs.", x_var), x = x_var, y = y_var)
        },
        "Bar Plot" = {
            p_base + geom_bar(aes(x = .data[[y_var]]), stat = "count", color = "black") +
                labs(title = paste("Bar Plot of", y_var), x = y_var) +
                theme(axis.text.x = element_text(angle = 45, hjust = 1))
        }
    )

    return(p + theme_rfapa())
}
