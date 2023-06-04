setwd(".")
library(lmtest)
library(readxl)

datos <- read_excel("datos/procesadores.xlsx")
rendimiento_relativo <- datos$`Rendimiento_Relativo`
rendimiento_relativo_estimado <- datos$`Rendimiento_relativo_estimado`
x <- datos
x$`Rendimiento_Relativo` <- NULL
x$`Rendimiento_relativo_estimado` <- NULL
model1 <- lm(rendimiento_relativo ~ ., data = x)
model2 <- lm(rendimiento_relativo_estimado ~ ., data = x)

summary_model1 <- summary(model1)
summary_model2 <- summary(model2)
