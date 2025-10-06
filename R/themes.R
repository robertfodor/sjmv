# R/themes.R
library(ggplot2)
library(jtools)

theme_rfapa <- function() {
    jtools::theme_apa() +
        theme(
            panel.background = element_blank(),
            plot.background  = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()
        )
}
