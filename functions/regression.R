linear_regression <- function(x, y, alpha = 0.05) {
  model <- lm(y ~ ., data = x)
  summary_model <- summary(model)
  coefficients <- coef(summary_model)
  significant_coefs <- coefficients[coefficients[, 4] < 0.05, ]
  return(list(
    model = model,
    summary_model = summary_model,
    coefficients = coefficients,
    significant_coefs = significant_coefs
  ))
}

predict_new_value <- function(model, x) {
  return(predict(model, x))
}
