library("e1071")
library("robustbase")

resumen_estadistica = function(datos) {
  resumen = data.frame(
    media = mean(datos),
    mediana = median(datos),
    varianza = var(datos),
    desviacion_estandar = sd(datos),
    iqr = IQR(datos),
    skewness = skewness(datos)
    kurtosis = kurtosis(datos)
  )
  return(resumen)
}

boxplot_ajustado = function(datos) {
  adjbox(datos, horizontal = T)
}