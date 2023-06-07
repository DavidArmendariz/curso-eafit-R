confidence_interval_mean_z <- function(datos, alpha = 0.05) {
  x_bar <- mean(datos)
  s <- sd(datos)
  n <- length(datos)
  z <- qnorm(alpha / 2, lower.tail = FALSE)
  error <- s * z / sqrt(n)
  return(c(x_bar - error, x_bar + error))
}

confidence_interval_mean_t <- function(datos, alpha = 0.05) {
  mu_bar <- mean(datos)
  s <- sd(datos)
  n <- length(datos)
  t <- qt(alpha / 2, n - 1, lower.tail = FALSE)
  error <- s * t / sqrt(n)
  return(c(mu_bar - error, mu_bar + error))
}

confidence_interval_proportion <- function(datos, category, alpha = 0.05) {
  n <- length(datos)
  p_bar <- sum(datos == category) / n
  z <- qnorm(alpha / 2, lower.tail = FALSE)
  error <- z * sqrt(p_bar * (1 - p_bar) / n)
  return(c(p_bar - error, p_bar + error))
}

confidence_interval_variance <- function(datos, alpha = 0.05) {
  n <- length(datos)
  s2 <- var(datos)
  chi2_inferior <- qchisq(1 - alpha / 2, n - 1)
  chi2_superior <- qchisq(alpha / 2, n - 1)
  numerator <- s2 * (n - 1)
  return(c(numerator / chi2_inferior, numerator / chi2_superior))
}
