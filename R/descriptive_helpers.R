# R/descriptive_helpers.R

# --- NEW: Mode calculation based on JASP source ---
# Source: https://github.com/jasp-stats/jaspDescriptives/blob/master/R/descriptives.R
# Note that if there are multiple modes, only the first one is returned
calculate_mode <- function(x) {
    if (length(x) == 0) {
        return(NA)
    }
    x <- x[!is.na(x)]
    if (length(x) == 0) {
        return(NA)
    }
    ux <- unique(x)
    return(ux[which.max(tabulate(match(x, ux)))])
}


# --- CONFIDENCE INTERVAL HELPER FUNCTIONS ---

# Generic function to get a bootstrap CI for any statistic
ci_bootstrap <- function(x, statistic_fun, ci_level, boot_reps) {
    x_complete <- na.omit(x)
    if (length(x_complete) < 2) {
        return(c(NA, NA))
    }

    stat_func_wrapper <- function(data, indices) {
        statistic_fun(data[indices])
    }

    boot_res <- boot::boot(data = x_complete, statistic = stat_func_wrapper, R = boot_reps)

    boot_ci <- tryCatch(
        {
            boot::boot.ci(boot_res, conf = ci_level, type = "perc")
        },
        error = function(e) {
            NULL
        }
    )

    if (is.null(boot_ci)) {
        return(c(NA, NA))
    }

    return(c(boot_ci$percent[4], boot_ci$percent[5]))
}

# CI for Mean (Normal Distribution)
ci_mean_normal <- function(x, ci_level) {
    n <- sum(!is.na(x))
    if (n < 2) {
        return(c(NA, NA))
    }
    mean_val <- mean(x, na.rm = TRUE)
    sem <- sd(x, na.rm = TRUE) / sqrt(n)
    z_crit <- qnorm(1 - (1 - ci_level) / 2)
    return(c(mean_val - z_crit * sem, mean_val + z_crit * sem))
}

# CI for Mean (t-Distribution)
ci_mean_t <- function(x, ci_level) {
    if (sum(!is.na(x)) < 2) {
        return(c(NA, NA))
    }
    t_test_res <- t.test(x, conf.level = ci_level)
    return(t_test_res$conf.int)
}

# CI for SD (Chi-Square method based on JASP)
ci_sd_chisq <- function(x, ci_level) {
    x_complete <- na.omit(x)
    n <- length(x_complete)
    if (n < 2) {
        return(c(NA, NA))
    }

    s <- sd(x_complete)
    alpha <- 1 - ci_level

    # Quantiles of the chi-square distribution
    chiSqLower <- stats::qchisq(p = alpha / 2, df = n - 1, lower.tail = TRUE)
    chiSqUpper <- stats::qchisq(p = 1 - alpha / 2, df = n - 1, lower.tail = TRUE)

    # Formula from Wikipedia/JASP
    lowerBound <- sqrt((n - 1) * s^2 / chiSqUpper)
    upperBound <- sqrt((n - 1) * s^2 / chiSqLower)

    return(c(lowerBound, upperBound))
}

# CI for Variance (Chi-Square method based on JASP)
ci_var_chisq <- function(x, ci_level) {
    x_complete <- na.omit(x)
    n <- length(x_complete)
    if (n < 2) {
        return(c(NA, NA))
    }

    v <- var(x_complete)
    alpha <- 1 - ci_level

    # Quantiles of the chi-square distribution
    chiSqLower <- stats::qchisq(p = alpha / 2, df = n - 1, lower.tail = TRUE)
    chiSqUpper <- stats::qchisq(p = 1 - alpha / 2, df = n - 1, lower.tail = TRUE)

    # Formula is the square of the SD CI formula
    lowerBound <- (n - 1) * v / chiSqUpper
    upperBound <- (n - 1) * v / chiSqLower

    return(c(lowerBound, upperBound))
}
