tabla_frec <- function(datos, clases) {
  # Tabla de Frecuencias
  maximo <- max(datos)
  minimo <- min(datos)
  rango <- maximo - minimo # Rango o Recorrido
  cant <- length(datos)

  if (clases == -2) {
    k <- ceiling(sqrt(cant)) # Número de clases - Criterio de la Raíz Cuadrada
  } else if (clases == -1) {
    k <- ceiling(2 * (cant)^(1 / 3)) # Número de clases - Criterio de Rice
  } else if (clases == 0) {
    k <- ceiling(log(cant, base = 2)) + 1 # Número de clases - Criterio de Sturges
  } else {
    k <- clases
  }

  longitud_clase <- rango / k
  limites_clases <- seq(minimo, maximo, by = longitud_clase)
  clases <- cut(datos, limites_clases, right = FALSE, include.lowest = TRUE)
  marcas_clase <- limites_clases[seq_along(limites_clases) - 1] + (longitud_clase / 2)
  frec_abs <- as.vector(table(clases))
  frec_rel <- as.vector(cbind(frec_abs) / cant)
  frec_abs_acum <- cumsum(frec_abs)
  frec_rel_acum <- cumsum(frec_rel)
  tabla_frec <- data.frame(levels(clases), marcas_clase, frec_abs, frec_rel, frec_abs_acum, frec_rel_acum)
  names(tabla_frec) <- c("Clases", "Marcas de Clase", "Frec. Abs.", "Frec. Rel.", "Frec. Abs. Acum", "Frec. Rel. Acum")
  return(tabla_frec)
}
