library(e1071)
library(robustbase)

statistics_summary <- function(data) {
  resumen <- data.frame(
    media = mean(data),
    mediana = median(data),
    varianza = var(data),
    desviacion_estandar = sd(data),
    iqr = IQR(data),
    skewness = skewness(data),
    kurtosis = kurtosis(data)
  )
  return(resumen)
}

get_adjusted_boxplot <- function(data) {
  adjbox(data, horizontal = TRUE)
}
