setwd(".")
library(lmtest)
library(readxl)
source("funciones/regresion.R")

datos <- read_excel("datos/procesadores.xlsx")
rendimiento_relativo <- datos$`Rendimiento_Relativo`
rendimiento_relativo_estimado <- datos$`Rendimiento_relativo_estimado`
x <- datos
x$`Rendimiento_Relativo` <- NULL
x$`Rendimiento_relativo_estimado` <- NULL

result1 <- regresion_lineal(x, rendimiento_relativo)
result2 <- regresion_lineal(x, rendimiento_relativo_estimado)
