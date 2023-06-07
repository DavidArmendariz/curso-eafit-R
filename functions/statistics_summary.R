library(e1071)
library(robustbase)

statistics_summary <- function(datos) {
  resumen <- data.frame(
    media = mean(datos),
    mediana = median(datos),
    varianza = var(datos),
    desviacion_estandar = sd(datos),
    iqr = IQR(datos),
    skewness = skewness(datos),
    kurtosis = kurtosis(datos)
  )
  return(resumen)
}

get_adjusted_boxplot <- function(datos) {
  adjbox(datos, horizontal = TRUE)
}
