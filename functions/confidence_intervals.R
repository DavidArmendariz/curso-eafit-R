confidence_interval_mean_z <- function(data, standard_deviation, alpha = 0.05) {
  x_bar <- mean(data)
  n <- length(data)
  z <- qnorm(alpha / 2, lower.tail = FALSE)
  error <- standard_deviation * z / sqrt(n)
  return(c(x_bar - error, x_bar + error))
}

confidence_interval_mean_t <- function(data, alpha = 0.05) {
  mu_bar <- mean(data)
  s <- sd(data)
  n <- length(data)
  t <- qt(alpha / 2, n - 1, lower.tail = FALSE)
  error <- s * t / sqrt(n)
  return(c(mu_bar - error, mu_bar + error))
}

confidence_interval_proportion <- function(data, category, alpha = 0.05) {
  n <- length(data)
  p_bar <- sum(data == category) / n
  z <- qnorm(alpha / 2, lower.tail = FALSE)
  error <- z * sqrt(p_bar * (1 - p_bar) / n)
  return(c(p_bar - error, p_bar + error))
}

confidence_interval_proportion_with_calculated_p <- function(p, n, alpha = 0.05) {
  z <- qnorm(alpha / 2, lower.tail = FALSE)
  error <- z * sqrt(p * (1 - p) / n)
  return(c(p - error, p + error))
}

confidence_interval_variance <- function(data, alpha = 0.05) {
  n <- length(data)
  s2 <- var(data)
  chi2_inferior <- qchisq(1 - alpha / 2, n - 1)
  chi2_superior <- qchisq(alpha / 2, n - 1)
  numerator <- s2 * (n - 1)
  return(c(numerator / chi2_inferior, numerator / chi2_superior))
}
