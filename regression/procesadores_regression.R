setwd(".")
library(lmtest)
library(readxl)
source("functions/regression.R")
source("functions/confidence_intervals.R")

data <- read_excel("data/procesadores.xlsx")
rendimiento_relativo <- data$`Rendimiento_Relativo`
rendimiento_relativo_estimado <- data$`Rendimiento_relativo_estimado`
x <- data
x$`Rendimiento_Relativo` <- NULL
x$`Rendimiento_relativo_estimado` <- NULL

regresion1 <- regresion_lineal(x, rendimiento_relativo)
regresion2 <- regresion_lineal(x, rendimiento_relativo_estimado)

rendimiento_relativo_3 <- predecir_nuevo_valor(regresion1$model, x[3, ])
error_rendimiento_relativo_3 <- regresion1$model$residuals[3]

confidence_interval_mean_t()(rendimiento_relativo)
# H0: u >= a 100
# H1: u < 100
t.test(
  rendimiento_relativo,
  conf.level = 0.95,
  alternative = "less",
  mu = 100,
)

media_columnas <- as.numeric(apply(data, 2, mean))
n <- nrow(data)
distancias_norma1 <- rep(0, n)
distancias_norma2 <- rep(0, n)
distancias_norma_infinito <- rep(0, n)
covarianza_datos <- cov(data)

for (i in 1:n) {
  fila_i <- as.numeric(data[i, ])
  diferencia <- as.matrix(fila_i - media_columnas)
  distancias_norma1[i] <- norm(diferencia, type = "1")
  distancias_norma2[i] <- norm(diferencia, type = "2")
  distancias_norma_infinito[i] <- norm(diferencia, type = "I")
}

distancias_mahalanobis <- mahalanobis(data, media_columnas, covarianza_datos)
