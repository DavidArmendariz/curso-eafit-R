setwd(".")

library(readxl)

medifis <- read_excel("data/datos.xls", sheet = "Medifis", skip = 2)

# sacar mahalanobis con kendall y spearman
# sacar cutoff chi cuadrado, 99, 97.5, 95, 90
# sacar con norma 1 y 2

data <- medifis[, -(1:2)]
dimension <- ncol(data)
means <- colMeans(data)
covariance_matrix <- cov(data)
mahalanobis_distances <- mahalanobis(data, means, covariance_matrix)

cutoff_99 <- qchisq(0.99, dimension)
cutoff_97_5 <- qchisq(0.975, dimension)
cutoff_95 <- qchisq(0.95, dimension)
cutoff_90 <- qchisq(0.90, dimension)

plot(mahalanobis_distances, type = "h", lwd = 2, ylim = c(0, 20))
abline(h = cutoff_99, lwd = 2, lty = 2, col = "red")
abline(h = cutoff_97_5, lwd = 2, lty = 2, col = "cyan")
abline(h = cutoff_95, lwd = 2, lty = 2, col = "green2")
abline(h = cutoff_90, lwd = 2, lty = 2, col = "black")

outliers_cutoff_99_indexes <- which(mahalanobis_distances >= cutoff_99)
outliers_cutoff_97_5_indexes <- which(mahalanobis_distances >= cutoff_97_5)
outliers_cutoff_95_indexes <- which(mahalanobis_distances >= cutoff_95)
outliers_cutoff_90_indexes <- which(mahalanobis_distances >= cutoff_90)

outliers_cutoff_99 <- data[outliers_cutoff_99_indexes, ]
outliers_cutoff_97_5 <- data[outliers_cutoff_97_5_indexes, ]
outliers_cutoff_95 <- data[outliers_cutoff_95_indexes, ]
outliers_cutoff_90 <- data[outliers_cutoff_90_indexes, ]
