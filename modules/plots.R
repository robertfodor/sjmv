library(ggplot2)

# Function to create a box plot with mean line
gen_boxplot <- function(data, x_var, y_var) {
    plot <- ggplot(data, aes(x = {{ x_var }}, y = {{ y_var }})) +
        geom_boxplot(fill = "#99d8c9", color = "black") +
        stat_summary(fun.y = mean, geom = "point", shape = 23, size = 3, fill = "white", color = "black") +
        labs(x = deparse(substitute(x_var)), y = deparse(substitute(y_var))) +
        theme_minimal()

    return(plot)
}
