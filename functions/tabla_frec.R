get_frequency_table <- function(datos, classes) {
  # Tabla de Frecuencias
  maximum <- max(datos)
  minimum <- min(datos)
  range <- maximum - minimum # Rango o Recorrido
  cant <- length(datos)

  if (classes == -2) {
    k <- ceiling(sqrt(cant)) # Número de clases - Criterio de la Raíz Cuadrada
  } else if (classes == -1) {
    k <- ceiling(2 * (cant)^(1 / 3)) # Número de clases - Criterio de Rice
  } else if (classes == 0) {
    k <- ceiling(log(cant, base = 2)) + 1 # Número de clases - Criterio de Sturges
  } else {
    k <- classes
  }

  longitud_clase <- range / k
  limites_clases <- seq(minimum, maximum, by = longitud_clase)
  clases <- cut(datos, limites_clases, right = FALSE, include.lowest = TRUE)
  marcas_clase <- limites_clases[seq_along(limites_clases) - 1] + (longitud_clase / 2)
  freq_abs <- as.vector(table(clases))
  freq_rel <- as.vector(cbind(freq_abs) / cant)
  frec_abs_acum <- cumsum(freq_abs)
  frec_rel_acum <- cumsum(freq_rel)
  freq_table <- data.frame(levels(clases), marcas_clase, freq_abs, freq_rel, frec_abs_acum, frec_rel_acum)
  names(freq_table) <- c("Clases", "Marcas de Clase", "Frec. Abs.", "Frec. Rel.", "Frec. Abs. Acum", "Frec. Rel. Acum")
  return(freq_table)
}
