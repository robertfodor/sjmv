# R/plots.R
library(ggplot2)
library(ggrepel)
library(dplyr)

# Source the theme file
source("R/themes.R")

generate_boxplot <- function(data, var, label_outliers = FALSE) {
  if (label_outliers && !"original_row_id" %in% names(data)) {
    warning("`original_row_id` column not found for labeling. Add it via tibble::rownames_to_column().")
    label_outliers <- FALSE
  }

  p <- ggplot(data, aes(x = factor(1), y = .data[[var]])) +
    geom_jitter(width = 0.15, alpha = 0.4, shape = 16) +
    geom_boxplot(width = 0.2, fill = "gray80", alpha = 0.5, outlier.shape = NA) +
    labs(x = "", y = var, title = paste("Box Plot of", var)) +
    theme_rfapa() + # Apply custom APA theme
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

  if (label_outliers) {
    outliers_values <- boxplot.stats(data[[var]])$out
    if (length(outliers_values) > 0) {
      outlier_data <- data %>% filter(.data[[var]] %in% outliers_values)

      p <- p + ggrepel::geom_text_repel(
        data = outlier_data,
        aes(label = original_row_id),
        na.rm = TRUE,
        box.padding = 0.5
      )
    }
  }
  return(p)
}
