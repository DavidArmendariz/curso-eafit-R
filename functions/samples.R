optimal_sample_for_mean <- function(error, alpha, mean, standard_deviation) {
  z <- qnorm(alpha / 2, lower.tail = FALSE)
  return(ceiling((standard_deviation * z / error)^2))
}

optimal_sample_for_proportion <- function(error, alpha, p) {
  return(ceiling((qnorm(alpha / 2, lower.tail = FALSE) / error)^2 * p * (1 - p)))
}
