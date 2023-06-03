library(lpSolve)

# Juan debe trabajar al menos 15 horas a la semana para complementar
# sus ingresos. Tiene la oportunidad de trabajar en dos tiendas de
# menudeo. En la tienda A puede trabajar entre 6 y10 horas a la
# semana, y en la tienda B le permiten trabajar entre 5 y 12 horas.
# Ambas tiendas pagan el mismo salario por hora. Para decidir cuántas
# horas trabajar en cada tienda, Juan desea tomar su decisión según
# el estrés del trabajo. Basado en entrevistas con
# otros empleados, Juan estima que, en una escala del 1 al 10,
# los factores de estrés son 6 y 8 en las tiendas A y B,
# respectivamente. El estr´es aumenta proporcionalmente a las horas
# que trabaja en cada tienda.

# Problema de optimización:
# Z = 6*HA + 8*HB
# Restricciones:
# HA + HB >= 15
# HA <= 10
# HA >= 6
# HB <= 12
# HB >= 5

opt <- "min"
coef_obj <- c(6, 8)
restriction1 <- c(1, 1)
restriction2 <- c(1, 0)
restriction3 <- c(1, 0)
restriction4 <- c(0, 1)
restriction5 <- c(0, 1)
coef_restrictions <- rbind(
  restriction1,
  restriction2,
  restriction3,
  restriction4,
  restriction5
)
coef_right_side <- c(15, 10, 6, 12, 5)
restriction_types <- c(">=", "<=", ">=", "<=", ">=")
solution <- lp(
  direction = opt,
  objective.in = coef_obj,
  const.mat = coef_restrictions,
  const.dir = restriction_types,
  const.rhs = coef_right_side
)

print("Variables")
print(solution$solution)
paste("Z =", solution$objval)
