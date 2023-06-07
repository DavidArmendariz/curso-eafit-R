proportion_hypothesis <- function(datos, category, p0, type, alpha = 0.05) {
  n <- length(datos)
  p_bar <- sum(datos == category) / n
  z <- (p_bar - p0) / sqrt(p0 * (1 - p0) / n)

  if (type == "two.sided") {
    p_value <- 2 * pnorm(-abs(z))
  } else if (type == "less") {
    p_value <- pnorm(z)
  } else if (type == "greater") {
    p_value <- pnorm(-z)
  }

  if (p_value < alpha) {
    print("We reject the null hypothesis")
  } else {
    print("We do not reject the null hypothesis")
  }

  return(c(z, p_value))
}
