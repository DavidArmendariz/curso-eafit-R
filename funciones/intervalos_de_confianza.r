intervalo_de_confianza_z <- function(datos, alpha) {
  x_bar <- mean(datos)
  s <- sd(datos)
  n <- length(datos)
  z <- qnorm(alpha / 2, lower.tail = FALSE)
  error <- s * z / sqrt(n)
  return(c(x_bar - error, x_bar + error))
}

intervalo_de_confianza_t <- function(datos, alpha) {
  mu_bar <- mean(datos)
  s <- sd(datos)
  n <- length(datos)
  t <- qt(alpha / 2, n - 1, lower.tail = FALSE)
  error <- s * t / sqrt(n)
  return(c(mu_bar - error, mu_bar + error))
}

intervalo_de_confianza_p <- function(datos, category, alpha) {
  n <- length(datos)
  p_bar <- sum(datos == category) / n
  z <- qnorm(alpha / 2, lower.tail = FALSE)
  error <- z * sqrt(p_bar * (1 - p_bar) / n)
  return(c(p_bar - error, p_bar + error))
}
