setwd(".")

library(e1071)
source("functions/confidence_intervals.R")
source("functions/samples.R")

data <- read.table("data/rentabilidades.txt")
nrow <- nrow(data)
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
data_activo_mayor_kurtosis <- data[, activo_mayor_kurtosis]
mediana_activo_mayor_kurtosis <- median(data_activo_mayor_kurtosis)
activo_mayor_asimetria <- which.max(asimetria_list)
data_activo_mayor_asimetria <- data[, activo_mayor_asimetria]
asimetria_activo_mayor_asimetria <- asimetria_list[activo_mayor_asimetria]
varianza_activo_mayor_asimetria <- var(data_activo_mayor_asimetria)
media_activo_mayor_asimetria <- mean(data_activo_mayor_asimetria)
mediana_activo_mayor_asimetria <- median(data_activo_mayor_asimetria)

media_por_cada_sector <- colMeans(data)
proporciones <- rep(0, ncol)
for (i in 1:ncol) {
  positive <- sum(data[, i] > 0)
  proporciones[i] <- positive / nrow
}

confidence_interval_mean_95 <- list()
confidence_interval_mean_90 <- list()
confidence_interval_proportion_95 <- list()
confidence_interval_proportion_90 <- list()
for (i in 1:ncol) {
  column_data <- data[, i]
  confidence_interval_mean_95[[i]] <- confidence_interval_mean_t(column_data, alpha = 0.05)
  confidence_interval_mean_90[[i]] <- confidence_interval_mean_t(column_data, alpha = 0.1)
  confidence_interval_proportion_95[[i]] <-
    confidence_interval_proportion_with_calculated_p(proporciones[i], nrow, alpha = 0.05)
  confidence_interval_proportion_90[[i]] <-
    confidence_interval_proportion_with_calculated_p(proporciones[i], nrow, alpha = 0.1)
}

n_optimo_media_95 <- list()
n_optimo_media_90 <- list()
n_optimo_proporcion_95 <- list()
n_optimo_proporcion_90 <- list()
for (i in 1:ncol) {
  column_data <- data[, i]
  standard_deviation_column <- sd(column_data)

  error_intervalo_media_95 <- (confidence_interval_mean_95[[i]][2] -
    confidence_interval_mean_95[[i]][1]) / 4
  n_optimo_media_95[[i]] <- optimal_sample_for_mean(error_intervalo_media_95, 0.05, standard_deviation_column)

  error_intervalo_media_90 <- (confidence_interval_mean_90[[i]][2] -
    confidence_interval_mean_90[[i]][1]) / 4
  n_optimo_media_90[[i]] <- optimal_sample_for_mean(error_intervalo_media_90, 0.05, standard_deviation_column)
}
