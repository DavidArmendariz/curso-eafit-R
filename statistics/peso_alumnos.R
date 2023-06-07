peso_alumnos <- c(
  rep(44, 4),
  rep(50, 7),
  rep(56, 4),
  rep(62, 3),
  rep(68, 4),
  rep(74, 3)
)

mediana_peso_alumnos <- median(peso_alumnos)
media_peso_alumnos <- mean(peso_alumnos)
desvio_peso_alumnos <- sd(peso_alumnos)
percentil_90 <- quantile(peso_alumnos, probs = 0.9)
percentil_10 <- quantile(peso_alumnos, probs = 0.1)
menores_a_dos_desviaciones <- media_peso_alumnos - 2 * desvio_peso_alumnos
