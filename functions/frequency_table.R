get_frequency_table <- function(datos, classes) {
  # Tabla de Frecuencias
  maximum <- max(datos)
  minimum <- min(datos)
  range <- maximum - minimum # Rango o Recorrido
  number_of_rows <- length(datos)

  if (classes == -2) {
    k <- ceiling(sqrt(number_of_rows)) # Número de clases - Criterio de la Raíz Cuadrada
  } else if (classes == -1) {
    k <- ceiling(2 * (number_of_rows)^(1 / 3)) # Número de clases - Criterio de Rice
  } else if (classes == 0) {
    k <- ceiling(log(number_of_rows, base = 2)) + 1 # Número de clases - Criterio de Sturges
  } else {
    k <- classes
  }

  class_length <- range / k
  limites_clases <- seq(minimum, maximum, by = class_length)
  classes <- cut(datos, limites_clases, right = FALSE, include.lowest = TRUE)
  class_marks <- limites_clases[seq_along(limites_clases) - 1] + (class_length / 2)
  freq_abs <- as.vector(table(classes))
  freq_rel <- as.vector(cbind(freq_abs) / number_of_rows)
  frec_abs_acum <- cumsum(freq_abs)
  frec_rel_acum <- cumsum(freq_rel)
  freq_table <- data.frame(levels(classes), class_marks, freq_abs, freq_rel, frec_abs_acum, frec_rel_acum)
  names(freq_table) <- c("Clases", "Marcas de Clase", "Frec. Abs.", "Frec. Rel.", "Frec. Abs. Acum", "Frec. Rel. Acum")
  return(freq_table)
}
