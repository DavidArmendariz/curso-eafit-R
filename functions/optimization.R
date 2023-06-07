library(lpSolve)

linear_optimization <- function(
    direction,
    coef_obj,
    matriz_restricciones,
    coef_right_side,
    restriction_types) {
  solution <- lp(
    direction = direction,
    objective.in = coef_obj,
    const.mat = matriz_restricciones,
    const.dir = restriction_types,
    const.rhs = coef_right_side
  )

  print("Variables")
  print(solution$solution)
  paste("Z =", solution$objval)
  return(solution)
}
