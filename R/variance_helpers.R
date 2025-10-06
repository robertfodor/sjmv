# R/variance_helpers.R
# Helper functions for the variance analysis module

#' Run Levene's Test for Homogeneity of Variance
#'
#' @param formula The model formula.
#' @param data The dataframe.
#' @param type The type of ANOVA ("Factorial ANOVA without interactions" uses a different test).
#' @param digits The number of decimal places for formatting.
#' @return A formatted data.frame with the Levene's test result.
run_levene_test <- function(formula, data, type, digits) {
    if (type == "Factorial ANOVA without interactions") {
        test <- s20x::levene.test(
            formula,
            data = data,
            digit = digits,
            show.table = FALSE
        )

        return(data.frame(
            Test = "Levene's test (median centered)",
            Statistic = sprintf(paste0("%.", digits, "f"), test$f.value[1]),
            df1 = test$df[1],
            df2 = test$df[2],
            p.value = sprintf(paste0("%.", digits, "f"), test$p.value[1])
        ))
    } else {
        test <- car::leveneTest(
            formula,
            center = "mean",
            data = data
        )
        return(data.frame(
            Test = "Levene's test (mean centered)",
            Statistic = sprintf(paste0("%.", digits, "f"), test$`F value`[1]),
            df1 = test$Df[1],
            df2 = test$Df[2],
            p.value = sprintf(paste0("%.", digits, "f"), test$`Pr(>F)`[1])
        ))
    }
}


#' Calculate ANOVA Effect Sizes
#'
#' @param model An aov model object.
#' @param digits The number of decimal places for formatting.
#' @return A data.frame containing formatted Eta Squared, Omega Squared, and their partial versions.
calculate_anova_effect_sizes <- function(model, digits) {
    # Show eta squared, partial eta squared, and omega squared
    eta_sq <- effectsize::eta_squared(model, partial = FALSE, verbose = FALSE)
    partial_eta_sq <- effectsize::eta_squared(model, partial = TRUE, verbose = FALSE)
    omega_sq <- effectsize::omega_squared(model, partial = FALSE, verbose = FALSE)
    partial_omega_sq <- effectsize::omega_squared(model, partial = TRUE, verbose = FALSE)

    # Combine the effect sizes into a data frame
    effect_sizes <- data.frame(
        Term = eta_sq[[1]],
        Eta2 = eta_sq[[2]],
        Eta2_CI = paste(
            sprintf(paste0("%.", digits, "f"), eta_sq[[4]]),
            sprintf(paste0("%.", digits, "f"), eta_sq[[5]]),
            sep = "–"
        ),
        Eta2_interp = effectsize::interpret(eta_sq, rules = "field2013")[[6]],
        Eta2p = partial_eta_sq[[2]],
        Eta2p_CI = paste(
            sprintf(paste0("%.", digits, "f"), partial_eta_sq[[4]]),
            sprintf(paste0("%.", digits, "f"), partial_eta_sq[[5]]),
            sep = "–"
        ),
        Eta2p_interp = effectsize::interpret(partial_eta_sq, rules = "field2013")[[6]],
        Omega2 = omega_sq[[2]],
        Omega2_CI = paste(
            sprintf(paste0("%.", digits, "f"), omega_sq[[4]]),
            sprintf(paste0("%.", digits, "f"), omega_sq[[5]]),
            sep = "–"
        ),
        Omega2_interp = effectsize::interpret(omega_sq, rules = "field2013")[[6]],
        Omega2p = partial_omega_sq[[2]],
        Omega2p_CI = paste(
            sprintf(paste0("%.", digits, "f"), partial_omega_sq[[4]]),
            sprintf(paste0("%.", digits, "f"), partial_omega_sq[[5]]),
            sep = "–"
        ),
        Omega2p_interp = effectsize::interpret(partial_omega_sq, rules = "field2013")[[6]]
    )

    # Set column names for clarity
    colnames(effect_sizes) <- c(
        "Term", "Eta2", "Eta2_CI", "Eta2_interp",
        "Eta2p", "Eta2p_CI", "Eta2p_interp",
        "Omega2", "Omega2_CI", "Omega2_interp",
        "Omega2p", "Omega2p_CI", "Omega2p_interp"
    )

    return(effect_sizes)
}


#' Run Kruskal-Wallis Test for all Predictors
#'
#' @param outcome The name of the outcome variable.
#' @param predictors A character vector of predictor variable names.
#' @param data The dataframe.
#' @param digits The number of decimal places for formatting.
#' @return A data.frame containing the formatted Kruskal-Wallis results for each predictor.
run_kruskal_wallis <- function(outcome, predictors, data, digits) {
    kruskal_table <- data.frame()

    if (length(predictors) > 0) {
        for (i in 1:length(predictors)) {
            formula <- as.formula(paste0(outcome, " ~ ", predictors[[i]]))

            kruskal_test <- kruskal.test(formula, data = data)
            epsilon_sq <- effectsize::rank_epsilon_squared(formula, data = data, verbose = FALSE)

            kruskal_table <- rbind(
                kruskal_table,
                data.frame(
                    Variable = kruskal_test$data.name,
                    Statistic = kruskal_test$statistic,
                    df = kruskal_test$parameter,
                    p.value = kruskal_test$p.value,
                    epsilon2 = epsilon_sq$rank_epsilon_squared,
                    epsilon2_ci = paste0(
                        sprintf(paste0("%.", digits, "f"), epsilon_sq$CI_low), "–",
                        sprintf(paste0("%.", digits, "f"), epsilon_sq$CI_high)
                    )
                )
            )
        }
    }
    return(kruskal_table)
}
