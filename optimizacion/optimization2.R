source("functions/optimization.R")

opt <- "min"
coef_obj <- c(1.5, 1)
restriction1 <- c(35, 15)
restriction2 <- c(150, 100)
restrictions_matrix <- rbind(
  restriction1,
  restriction2
)
coef_right_side <- c(30, 110)
restriction_types <- c("<=", ">=")

linear_optimization(
  "min",
  coef_obj,
  restrictions_matrix,
  coef_right_side,
  restriction_types
)
