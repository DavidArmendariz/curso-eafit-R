library(corrplot)
library(car)
library(lmtest)

x1 <- rnorm(1000, 5, 4)
x2 <- rnorm(1000, 10, 0.5)
e <- rnorm(1000, 0, 1)
y <- 1 + 2 * x1 + 3 * x2 + e
datos <- data.frame(y, x1, x2)
pairs(datos, pch = 19)
rho <- cor(datos)
corrplot(rho)
sig <- cov(datos)
sigma_xy <- sig[2:3, 1]
sigma_xx <- sig[2:3, 2:3]
betas <- solve(sigma_xx) %*% sigma_xy
vector_medias <- apply(datos, 2, mean)
b0 <- mean(y) - vector_medias[2:3] %*% betas
coeficientes <- c(b0, betas)
x_new <- c(2, 4)
y_x_new <- b0 + x_new %*% betas
y_hat <- as.numeric(b0) + as.matrix(datos[, 2:3]) %*% betas
e_i <- y - y_hat
# Normalidad
qqnorm(e_i, pch = 19)
qqline(e_i, lwd = 2, col = "red")
shapiro.test(e_i)
# Media cero
plot(e_i, lwd = 2, type = "l")
points(e_i, pch = 19)
abline(h = 0, col = "red", lwd = 2)
t.test(e_i)
# Independencia
durbinWatsonTest(as.vector(e_i))
# Arroja el estadístico
# DW es cercano a 2 --- independencia
# DW cercano a 4 --- autocorrelación positiva
# DW cercano a 0 --- autocorrelación negativa
# Homocedasticidad
plot(y_hat, e_i, pch = 19)
abline(h = c(3, -3), col = "red")
# Cuando no hay patrones cíclicos de los puntos y cuando
# no hay patrones de crecimiento o decrimiento de los e_i
# sobre toda la franja de y_hat, entonces decimos que hay
# homocedasticidad

# Como hacer el modelo RLM en R
mod1 <- lm(y ~ x1 + x2, data = datos)
mod1$coefficients

# BP-test (Breusch-Pagan)
bptest(mod1)

# Tarea: realizar un modelo de regresión para la base de datos
# demography
# Y: cantidad de delitos
# estimar los coeficientes
# validar la significancia de los betas estimados
# validar los supuestos teóricos
# predecir los valores de "y" para el restante 20% de la muestra
# efectuar un proceso de validación cruzada para chequear la estabilidad
# de los coeficientes
# estudiar lo siguiente
summary(mod1)
# F test --- H0: b1 = b2 = 0
# H1: al menos un beta es distinto de cero
