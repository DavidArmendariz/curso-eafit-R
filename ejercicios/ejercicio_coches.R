setwd(".")
source("funciones/intervalos_de_confianza.R")
library(readxl)

datos <- read_excel("datos/coches.xlsx", skip = 1)
datos_tiempo <- as.numeric(datos$`Tiempo en segundos en alcanzar 100 kph`)
intervalo_de_confianza_t(datos_tiempo, 0.05)
intervalo_de_confianza_var(datos_tiempo, 0.05)
