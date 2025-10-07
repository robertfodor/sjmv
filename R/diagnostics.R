# R/diagnostics.R
library(ggplot2)
library(dplyr)

# Helper function to perform various regression diagnostics
diagnose_regression <- function(model, data,
                                # Outlier/Influence checks
                                check_cook = TRUE, check_dfbeta = TRUE,
                                check_leverage = TRUE, check_mahalanobis = TRUE,
                                # Residual checks
                                check_resid_plots = TRUE, check_resid_sw_test = TRUE,
                                # General plot switch
                                plot_diagnostics = TRUE, verbose = TRUE) {
  # --- Initial Setup ---
  results <- list()
  plots <- list()
  formulas <- list()
  cutoffs <- list()

  n <- nrow(data)
  k <- length(coef(model)) - 1 # Number of predictors

  # --- Residuals & Predicted Values ---
  diag_df <- data.frame(.rownames = 1:n)

  tryCatch(
    {
      diag_df$.raw_resid <- residuals(model)
      diag_df$.std_resid <- rstandard(model)
      diag_df$.pred_vals <- predict(model)
      diag_df$.leverage <- hatvalues(model)
      diag_df$.cooks_d <- cooks.distance(model)
    },
    error = function(e) {
      warning("Could not calculate some standard diagnostics (e.g., residuals, leverage).")
    }
  )


  # --- Residual Diagnostics ---
  if (check_resid_sw_test && ".std_resid" %in% names(diag_df)) {
    if (n > 2 && n < 5000) {
      sw_test <- shapiro.test(diag_df$.std_resid)
      results$shapiro_std_resid <- data.frame(
        Statistic = "Shapiro-Wilk W",
        Value = sw_test$statistic,
        `p-value` = sw_test$p.value,
        check.names = FALSE
      )
    }
  }

  if (check_resid_plots && plot_diagnostics && all(c(".pred_vals", ".raw_resid", ".std_resid") %in% names(diag_df))) {
    plots$resid_vs_pred <- ggplot(diag_df, aes(x = .pred_vals, y = .raw_resid)) +
      geom_point(shape = 21, size = 2.5, fill = "grey80", color = "black", alpha = 0.7) +
      geom_hline(yintercept = 0, color = "darkred", linewidth = 0.8) +
      scale_y_continuous(
        name = "Residuals",
        sec.axis = sec_axis(~ . / sd(., na.rm = TRUE), name = "Standardized Residuals")
      ) +
      labs(title = "Residuals vs. Predicted", x = "Predicted Values") +
      theme_rfapa()

    plots$hist_resid <- ggplot(diag_df, aes(x = .std_resid)) +
      geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "lightblue", color = "black", alpha = 0.7) +
      geom_density(color = "red", linewidth = 1) +
      labs(title = "Histogram of Standardized Residuals", x = "Standardized Residuals", y = "Density") +
      theme_rfapa()

    plots$qq_resid <- ggplot(diag_df, aes(sample = .std_resid)) +
      stat_qq(shape = 21, size = 2.5, fill = "grey80", color = "black", alpha = 0.7) +
      stat_qq_line(color = "red", linewidth = 1) +
      labs(title = "Q-Q Plot of Standardized Residuals", x = "Theoretical Quantiles", y = "Sample Quantiles") +
      theme_rfapa()
  }

  # --- Outlier/Influence Diagnostics (Tables & Plots) ---
  if (check_cook && ".cooks_d" %in% names(diag_df)) {
    cutoff <- 4 / n
    outliers <- which(diag_df$.cooks_d > cutoff)
    if (length(outliers) > 0) {
      results$cooks_distance <- list(data = data.frame(RowID = outliers, `Cook's D` = diag_df$.cooks_d[outliers]), name = "cooks_distance")
    }
    formulas$cooks_distance <- "Cutoff for Cook's Distance: \\( D_i > \\frac{4}{n} \\)"
    cutoffs$cooks_distance <- cutoff

    if (plot_diagnostics) {
      plots$cooks_distance <- ggplot(diag_df, aes(x = .rownames, y = .cooks_d)) +
        geom_point(shape = 21, fill = "grey80", color = "black") +
        geom_segment(aes(xend = .rownames, yend = 0)) +
        geom_hline(yintercept = cutoff, color = "red", linetype = "dashed") +
        labs(title = "Cook's Distance", x = "Observation Index", y = "Cook's D") +
        theme_rfapa()
    }
  }

  if (check_leverage && ".leverage" %in% names(diag_df)) {
    cutoff <- 2 * (k + 1) / n
    high_leverage <- which(diag_df$.leverage > cutoff)
    if (length(high_leverage) > 0) {
      results$leverage <- list(data = data.frame(RowID = high_leverage, Leverage = diag_df$.leverage[high_leverage]), name = "leverage")
    }
    formulas$leverage <- "Cutoff for Leverage: \\( h_{ii} > \\frac{2(k+1)}{n} \\)"
    cutoffs$leverage <- cutoff

    if (plot_diagnostics) {
      plots$leverage <- ggplot(diag_df, aes(x = .rownames, y = .leverage)) +
        geom_point(shape = 21, fill = "grey80", color = "black") +
        geom_hline(yintercept = cutoff, color = "red", linetype = "dashed") +
        labs(title = "Leverage", x = "Observation Index", y = "Leverage Value (Hat values)") +
        theme_rfapa()
    }
  }

  if (check_dfbeta) {
    dfbetas_vals <- dfbetas(model)
    cutoff <- 2 / sqrt(n)
    influential <- which(apply(abs(dfbetas_vals), 1, max) > cutoff)
    if (length(influential) > 0) {
      results$dfbeta <- list(data = as.data.frame(dfbetas_vals[influential, ]), name = "dfbeta")
    }
    formulas$dfbeta <- "Cutoff for DFBETAs: \\( |DFBETA_{ij}| > \\frac{2}{\\sqrt{n}} \\)"
    cutoffs$dfbeta <- cutoff

    if (plot_diagnostics) {
      diag_df$.max_abs_dfbeta <- apply(abs(dfbetas_vals), 1, max)
      plots$dfbeta <- ggplot(diag_df, aes(x = .rownames, y = .max_abs_dfbeta)) +
        geom_point(shape = 21, fill = "grey80", color = "black") +
        geom_segment(aes(xend = .rownames, yend = 0)) +
        geom_hline(yintercept = cutoff, color = "red", linetype = "dashed") +
        labs(title = "Maximum Absolute DFBETA per Case", x = "Observation Index", y = "Max(|DFBETA|)") +
        theme_rfapa()
    }
  }

  if (check_mahalanobis) {
    final_model_predictors <- all.vars(formula(model))[-1]
    numeric_predictors <- data %>%
      dplyr::select(any_of(final_model_predictors)) %>%
      dplyr::select(where(is.numeric))

    if (ncol(numeric_predictors) > 0) {
      numeric_predictors_clean <- numeric_predictors %>%
        mutate(.rownames = 1:n) %>%
        na.omit()

      if (nrow(numeric_predictors_clean) > ncol(numeric_predictors_clean)) {
        predictor_data_only <- numeric_predictors_clean %>% dplyr::select(-.rownames)

        # FIX: Pre-check for zero variance columns
        variances <- apply(predictor_data_only, 2, var)

        if (any(variances < 1e-10)) {
          warning("Mahalanobis distance not calculated: at least one numeric predictor has zero variance.")
        } else {
          cov_matrix <- cov(predictor_data_only)

          if (rcond(cov_matrix) > 1e-10) {
            maha_dist_vals <- stats::mahalanobis(
              predictor_data_only,
              colMeans(predictor_data_only),
              cov_matrix
            )

            maha_df <- data.frame(.rownames = numeric_predictors_clean$.rownames, .maha_dist = maha_dist_vals)
            diag_df <- left_join(diag_df, maha_df, by = ".rownames")

            cutoff <- stats::qchisq(0.99, df = ncol(numeric_predictors))
            outliers <- which(diag_df$.maha_dist > cutoff)

            if (length(outliers) > 0) {
              results$mahalanobis <- list(data = data.frame(RowID = outliers, `Mahalanobis D2` = diag_df$.maha_dist[outliers]), name = "mahalanobis")
            }
            formulas$mahalanobis <- "Cutoff for Mahalanobis: \\( D^2 > \\chi^2_{df=k, p=0.01} \\)"
            cutoffs$mahalanobis <- cutoff

            if (plot_diagnostics) {
              plots$mahalanobis <- ggplot(diag_df, aes(x = .rownames, y = .maha_dist)) +
                geom_point(shape = 21, fill = "grey80", color = "black") +
                geom_hline(yintercept = cutoff, color = "red", linetype = "dashed") +
                labs(title = "Mahalanobis Distance", x = "Observation Index", y = "Mahalanobis D-squared") +
                theme_rfapa()
            }
          } else {
            warning("Mahalanobis distance not calculated: multicollinearity (singular matrix).")
          }
        }
      }
    }
  }

  return(list(results = results, plots = plots, formulas = formulas, cutoffs = cutoffs))
}
