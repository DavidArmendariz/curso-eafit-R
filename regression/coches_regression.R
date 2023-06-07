setwd(".")
source("functions/confidence_intervals.R")
source("functions/hypotesis.R")
library(readxl)

data <- read_excel("data/coches.xlsx", skip = 1)
# str(datos) para ver el tipo de columnas
data$`Tiempo en segundos en alcanzar 100 kph` <- as.numeric(data$`Tiempo en segundos en alcanzar 100 kph`)
data$`Consumo de gasolina en litros por cada 100Km` <- as.numeric(data$`Consumo de gasolina en litros por cada 100Km`)
datos_tiempo <- data$`Tiempo en segundos en alcanzar 100 kph`
confidence_interval_mean_t(datos_tiempo, 0.05)
confidence_interval_variance(datos_tiempo, 0.05)

x <- data
x$`Tiempo en segundos en alcanzar 100 kph` <- NULL

model <- lm(datos_tiempo ~ ., data = x)
summary_model <- summary(model)

coefficients <- coef(summary_model)
significant_coefs <- coefficients[coefficients[, 4] < 0.05, ]

ensamble <- data$`Ensamble 1 local=1,extranjero=0`
confidence_interval_proportion(ensamble, 1, 0.05)

# Prueba de hipótesis con prop.test
# H0: p <= 0.5
# Ha: p > 0.5

locales <- sum(data$`Ensamble 1 local=1,extranjero=0` == 1)
total <- nrow(data)
prop.test(
  locales,
  total,
  p = 0.5,
  alternative = "greater",
  conf.level = 0.95,
)
proportion_hypothesis(data$`Ensamble 1 local=1,extranjero=0`, 1, 0.5, "greater", 0.05)

primer_carro <- as.numeric(data[1, ])
distances <- rep(0, nrow(data) - 1)
for (i in 2:nrow(data)) {
  difference <- as.numeric(data[i, ]) - primer_carro
  difference <- as.matrix(difference)
  distances[i - 1] <- norm(difference, type = "I")
}
distancia_min <- which.min(distances)
