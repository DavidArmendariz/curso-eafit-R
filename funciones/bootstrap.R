bootstrap <- function(
    data,
    summary_function = "mean",
    alpha = 0.05,
    num_iterations = 100) {
  if (is.character(summary_function)) {
    summary_function <- match.fun(summary_function)
  }

  vec_estimators <- rep(0, num_iterations)

  for (i in 1:num_iterations) {
    sub_sample <- sample(data, length(data), replace = TRUE)
    vec_estimators[i] <- summary_function(sub_sample)
  }

  perc_vec_estimators <- quantile(
    vec_estimators,
    probs = c(alpha / 2, 1 - alpha / 2)
  )

  return(perc_vec_estimators)
}
