#' @title Átfogó regressziós diagnosztika
#' @description Futtatja a leggyakoribb regressziós diagnosztikai teszteket a
#'   befolyásoló pontok, a kiugró értékek (outlierek) és a nagy leverage pontok
#'   azonosítására. Opcionálisan vizualizációkat is készít.
#' @param model Egy `lm` objektum.
#' @param data Az eredeti adatkeret (data.frame), amellyel a modell készült.
#' @param verbose Logikai. Ha `TRUE`, üzeneteket és táblázatokat ír a konzolra/logba.
#' @return Láthatatlanul (`invisible`) visszaad egy listát, amely tartalmazza a
#'   `results`-ot, `cutoffs`-ot, `formulas`-t és `plots`-ot.
diagnose_regression <- function(model, data,
                                check_cook = TRUE,
                                check_dfbeta = TRUE,
                                check_leverage = TRUE,
                                check_mahalanobis = TRUE,
                                plot_diagnostics = TRUE,
                                mahalanobis_alpha = 0.001,
                                verbose = TRUE) {
  
  # --- Függőségek és bemenetek ellenőrzése ---
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("A 'ggplot2' csomag szükséges az ábrázoláshoz.")
  if (!inherits(model, "lm")) stop("A 'model' argumentumnak 'lm' objektumnak kell lennie.")
  if (!is.data.frame(data)) stop("A 'data' argumentumnak data.frame-nek kell lennie.")
  
  # --- Alapvető metrikák egyszeri kiszámítása ---
  n <- nrow(data)
  p <- length(stats::coef(model)) # Koefficiensek száma (intercepttel)
  k_orig <- p - 1 # Eredeti prediktorok száma
  influence_metrics <- as.data.frame(stats::influence.measures(model)$infmat)
  
  # --- Eredménytárolók inicializálása ---
  results <- list()
  cutoffs <- list()
  plots <- list()
  formulas <- list() # NEW: To store formulas
  
  # --- 1. Cook-távolság ---
  if (check_cook) {
    cutoff <- 4 / (n - k_orig - 1)
    indices <- which(influence_metrics$cook.d > cutoff)
    if (verbose) message(paste0("\n--- Cook-távolság (Határérték: ", round(cutoff, 4), ") ---"))
    if (length(indices) > 0) {
      data_subset <- data[indices, , drop = FALSE]
      diag_data <- data.frame(
        Case = indices,
        cook.d = round(influence_metrics$cook.d[indices], 4)
      )
      res_data <- cbind(diag_data, data_subset)
      results$cooks_distance <- list(indices = indices, data = res_data)
      if (verbose) print(res_data)
    }
    cutoffs$cooks_distance <- cutoff
    formulas$cooks_distance <- "$$\\text{Cutoff} = \\frac{4}{n - k - 1}$$" # ADDED FORMULA
    
    if (plot_diagnostics) {
      plot_data <- data.frame(Index = 1:n, Cooks.D = influence_metrics$cook.d)
      p_cook <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$Index, y = .data$Cooks.D)) +
        ggplot2::geom_point(alpha = 0.6) +
        ggplot2::geom_hline(yintercept = cutoff, linetype = "dashed", color = "red") +
        ggplot2::labs(title = "Cook's Distance", x = "Case Index", y = "Cook's D") +
        ggplot2::theme_minimal()
      plots$cooks_distance <- p_cook
    }
  }
  
  # --- 2. DFBETA ---
  if (check_dfbeta) {
    cutoff <- 2 / sqrt(n)
    dfbeta_cols <- grep("dfb.", names(influence_metrics))
    dfbetas <- influence_metrics[, dfbeta_cols, drop = FALSE]
    indices <- which(apply(abs(dfbetas) > cutoff, 1, any))
    if (verbose) message(paste0("\n--- DFBETA (Határérték: ±", round(cutoff, 4), ") ---"))
    if (length(indices) > 0) {
      data_subset <- data[indices, , drop = FALSE]
      dfbetas_subset <- round(dfbetas[indices, , drop = FALSE], 4)
      diag_data <- data.frame(Case = indices)
      res_data <- cbind(diag_data, data_subset, dfbetas_subset)
      results$dfbeta <- list(indices = indices, data = res_data)
      if (verbose) print(res_data)
    }
    cutoffs$dfbeta <- cutoff
    formulas$dfbeta <- "$$\\text{Cutoff} = \\frac{2}{\\sqrt{n}}$$" # ADDED FORMULA
    
    if (plot_diagnostics) {
      plot_data <- data.frame(
        Index = 1:n,
        MaxAbsDFBETA = apply(abs(dfbetas), 1, max)
      )
      p_dfbeta <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$Index, y = .data$MaxAbsDFBETA)) +
        ggplot2::geom_point(alpha = 0.6) +
        ggplot2::geom_hline(yintercept = cutoff, linetype = "dashed", color = "red") +
        ggplot2::labs(title = "Maximum Absolute DFBETA per Case", x = "Case Index", y = "Max(|DFBETA|)") +
        ggplot2::theme_minimal()
      plots$dfbeta <- p_dfbeta
    }
  }
  
  # --- 3. Leverage (Hat Values) ---
  if (check_leverage) {
    cutoff <- 3 * (p / n)
    leverage_values <- stats::hatvalues(model)
    indices <- which(leverage_values > cutoff)
    if (verbose) message(paste0("\n--- Leverage (Határérték: ", round(cutoff, 4), ") ---"))
    if (length(indices) > 0) {
      data_subset <- data[indices, , drop = FALSE]
      diag_data <- data.frame(
        Case = indices,
        leverage = round(leverage_values[indices], 4)
      )
      res_data <- cbind(diag_data, data_subset)
      results$leverage <- list(indices = indices, data = res_data)
      if (verbose) print(res_data)
    }
    cutoffs$leverage <- cutoff
    formulas$leverage <- "$$\\text{Cutoff} = \\frac{3 \\times p}{n}$$" # ADDED FORMULA
    
    if (plot_diagnostics) {
      plot_data <- data.frame(Index = 1:n, Leverage = leverage_values)
      p_leverage <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$Index, y = .data$Leverage)) +
        ggplot2::geom_point(alpha = 0.6) +
        ggplot2::geom_hline(yintercept = cutoff, linetype = "dashed", color = "red") +
        ggplot2::labs(title = "Leverage Values", x = "Case Index", y = "Leverage") +
        ggplot2::theme_minimal()
      plots$leverage <- p_leverage
    }
  }
  
  # --- 4. Mahalanobis-távolság ---
  if (check_mahalanobis) {
    if (verbose) message("\n--- Mahalanobis-távolság ---")
    
    model_matrix <- stats::model.matrix(model)
    predictor_matrix <- model_matrix[, colnames(model_matrix) != "(Intercept)", drop = FALSE]
    
    mahal_dist <- NULL
    k_mahal <- NULL # Initialize k_mahal
    if (ncol(predictor_matrix) > 0) {
      k_mahal <- ncol(predictor_matrix) # Define k_mahal here
      mahal_dist <- tryCatch({
        stats::mahalanobis(
          predictor_matrix,
          center = colMeans(predictor_matrix),
          cov = stats::cov(predictor_matrix)
        )
      }, error = function(e) {
        if (verbose) message("Mahalanobis distance could not be calculated (possible multicollinearity).")
        return(NULL)
      })
    } else {
      if (verbose) message("No predictors in model; Mahalanobis distance skipped.")
    }
    
    if (!is.null(mahal_dist)) {
      cutoff <- stats::qchisq(1 - mahalanobis_alpha, df = k_mahal)
      indices <- which(mahal_dist > cutoff)
      
      if (verbose) message(paste0("Cutoff: ", round(cutoff, 4), " (", (1-mahalanobis_alpha)*100, "% Chi-sq, df=", k_mahal, ")"))
      if (length(indices) > 0) {
        data_subset <- data[indices, , drop = FALSE]
        diag_data <- data.frame(
          Case = indices,
          mahalanobis.d = round(mahal_dist[indices], 4)
        )
        res_data <- cbind(diag_data, data_subset)
        results$mahalanobis <- list(indices = indices, data = res_data)
        if (verbose) print(res_data)
      }
      cutoffs$mahalanobis <- cutoff
      # ADDED DYNAMIC FORMULA
      formulas$mahalanobis <- sprintf("$$\\text{Cutoff} = \\chi^2_{(\\alpha=%.3f, df=%d)}$$", mahalanobis_alpha, k_mahal)
      
      if (plot_diagnostics) {
        plot_data <- data.frame(Index = 1:n, Mahalanobis = mahal_dist)
        p_mahal <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$Index, y = .data$Mahalanobis)) +
          ggplot2::geom_point(alpha = 0.6) +
          ggplot2::geom_hline(yintercept = cutoff, linetype = "dashed", color = "red") +
          ggplot2::labs(title = "Mahalanobis Distance", x = "Case Index", y = "Distance") +
          ggplot2::theme_minimal()
        plots$mahalanobis <- p_mahal
      }
    }
  }
  
  # --- Ábrák megjelenítése ---
  if (plot_diagnostics && length(plots) > 0 && verbose) {
    message("\n--- Diagnostic Plots ---")
    print(plots)
  }
  
  # --- Végső objektum visszaadása ---
  invisible(list(results = results, cutoffs = cutoffs, plots = plots, formulas = formulas))
}