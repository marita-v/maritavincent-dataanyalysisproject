# Fits the model of salary vs. location, and makes the plots needed.

library(ggplot2)

# Fits a model predicting log(annual_salary) from location.
# Use log(salary) because raw salary is skewed, and log makes it more normal/symmetric.

fit_salary_model <- function(data) {
  lm(log_annual_salary ~ location, data = data)
}


# Boxplot of salary by location.
plot_salary_by_location <- function(data) {
  ggplot(data, aes(x = location, y = annual_salary)) +
    geom_boxplot() +
    labs(x = "Office location", y = "Annualized salary (USD)")
}


# Residuals vs. fitted plot, used to check the model's assumptions.
plot_residuals_vs_fitted <- function(model) {
  diag_df <- data.frame(fitted = fitted(model), resid = resid(model))

  ggplot(diag_df, aes(x = fitted, y = resid)) +
    geom_point() +
    geom_hline(yintercept = 0) +
    labs(x = "Fitted values", y = "Residuals")
}


# Normal Q-Q plot to check if residuals are approximately normal.
plot_qq <- function(model) {
  diag_df <- data.frame(resid = resid(model))

  ggplot(diag_df, aes(sample = resid)) +
    stat_qq() +
    stat_qq_line()
}
