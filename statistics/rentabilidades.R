setwd(".")

library(e1071)

data <- read.table("data/rentabilidades.txt")
ncol <- ncol(data)

par(mfrow = c(2, 1))
kurtosis_list <- rep(0, ncol)
asimetria_list <- rep(0, ncol)

for (i in 1:ncol) {
  column_data <- data[, i]
  boxplot(column_data, horizontal = TRUE)
  title(paste("Boxplot rentabilidad para la columna ", i))
  hist(column_data, main = paste("Histograma rentabilidad para la columna ", i))
  kurtosis_list[i] <- kurtosis(column_data)
  asimetria_list[i] <- skewness(column_data)
}

activo_mayor_kurtosis <- which.max(kurtosis_list)
kurtosis_activo_mayor_kurtosis <- kurtosis_list[activo_mayor_kurtosis]
mediana_activo_mayor_kurtosis <- median(data[, activo_mayor_kurtosis])
activo_mayor_asimetria <- which.max(asimetria_list)
asimetria_activo_mayor_asimetria <- asimetria_list[activo_mayor_asimetria]
