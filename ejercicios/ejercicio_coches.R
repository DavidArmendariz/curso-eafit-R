setwd(".")
source("funciones/intervalos_de_confianza.R")
source("funciones/hipotesis.R")
library(readxl)

datos <- read_excel("datos/coches.xlsx", skip = 1)
# str(datos) para ver el tipo de columnas
datos$`Tiempo en segundos en alcanzar 100 kph` <- as.numeric(datos$`Tiempo en segundos en alcanzar 100 kph`)
datos$`Consumo de gasolina en litros por cada 100Km` <- as.numeric(datos$`Consumo de gasolina en litros por cada 100Km`)
datos_tiempo <- datos$`Tiempo en segundos en alcanzar 100 kph`
intervalo_de_confianza_t(datos_tiempo, 0.05)
intervalo_de_confianza_var(datos_tiempo, 0.05)

x <- datos
x$`Tiempo en segundos en alcanzar 100 kph` <- NULL

model <- lm(datos_tiempo ~ ., data = x)
summary_model <- summary(model)

coefficients <- coef(summary_model)
significant_coefs <- coefficients[coefficients[, 4] < 0.05, ]

ensamble <- datos$`Ensamble 1 local=1,extranjero=0`
intervalo_de_confianza_proporcion(ensamble, 1, 0.05)

# Prueba de hipótesis con prop.test
# H0: p <= 0.5
# Ha: p > 0.5

locales <- sum(datos$`Ensamble 1 local=1,extranjero=0` == 1)
total <- nrow(datos)
prop.test(
  locales,
  total,
  p = 0.5,
  alternative = "greater",
  conf.level = 0.95,
)
prueba_hipotesis_proporcion_datos(datos$`Ensamble 1 local=1,extranjero=0`, 1, 0.5, "greater", 0.05)

primer_carro <- as.numeric(datos[1, ])
distancias <- rep(0, nrow(datos) - 1)
for (i in 2:nrow(datos)) {
  diferencia <- as.numeric(datos[i, ]) - primer_carro
  diferencia <- as.matrix(diferencia)
  distancias[i - 1] <- norm(diferencia, type = "I")
}
distancia_min <- which.min(distancias)
