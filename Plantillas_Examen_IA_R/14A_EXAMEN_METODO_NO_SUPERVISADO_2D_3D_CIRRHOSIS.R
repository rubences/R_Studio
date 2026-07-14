############################################################
# 14A_EXAMEN_METODO_NO_SUPERVISADO_2D_3D_CIRRHOSIS.R
############################################################
#
# ADAPTACIÓN PARA EL EXAMEN: CIRRHOSIS
# ------------------------------------
# Objetivo:
#   1) cargar el conjunto de datos cirrhosis.csv;
#   2) realizar una inspección básica (NA, tipos, descriptivos);
#   3) preparar únicamente predictores numéricos para el método
#      no supervisado;
#   4) aplicar PCA y reducir a 2 y 3 dimensiones;
#   5) visualizar el resultado en 2D y 3D;
#   6) cuantificar la estructura con K-means + silueta;
#   7) generar una interpretación automática lista para el examen.
#
# NOTA IMPORTANTE
# ---------------
# - La columna Status se excluye del ajuste no supervisado.
# - Se usa solo como referencia descriptiva al final.
# - Las variables categóricas se inspeccionan, pero no se fuerzan
#   a entrar en PCA para mantener una solución segura y estable.
############################################################

rm(list = ls())

# Asegura que el script se ejecuta desde la carpeta del proyecto.
if (!file.exists("00_UTILIDADES.R") &&
    file.exists(file.path("Plantillas_Examen_IA_R", "00_UTILIDADES.R"))) {
  setwd("Plantillas_Examen_IA_R")
}

if (!file.exists("00_UTILIDADES.R")) {
  stop(
    paste0(
      "\nERROR: no se encuentra 00_UTILIDADES.R. ",
      "Abre Plantillas_Examen_IA_R.Rproj y ejecuta este script ",
      "desde la carpeta del proyecto.\nDirectorio actual: ", getwd()
    ),
    call. = FALSE
  )
}

source("00_UTILIDADES.R")
comprobar_directorio_proyecto()

# La plantilla usa este resumen en varios puntos y no existe en
# 00_UTILIDADES.R, así que lo definimos localmente para que el script
# sea autocontenido.
crear_resumen_na <- function(dataframe) {
  data.frame(
    Variable = names(dataframe),
    Numero_NA = vapply(
      dataframe,
      function(x) sum(is.na(x)),
      numeric(1)
    ),
    Porcentaje_NA = 100 * vapply(
      dataframe,
      function(x) mean(is.na(x)),
      numeric(1)
    ),
    stringsAsFactors = FALSE
  )
}

############################################################
# CONFIGURACIÓN: SOLO EDITAR AQUÍ SI LO NECESITAS
############################################################
CONFIG <- list(
  archivo = file.path("data", "cirrhosis.csv"),
  separador = ";",
  objetivo = "Status",
  columnas_excluir = c("Status"),
  metodo_no_supervisado = "PCA",
  numero_clusters = 3,
  semilla = 1995,
  colorear_tambien_por_objetivo = TRUE
)
############################################################
# FIN DE CONFIGURACIÓN
############################################################

############################################################
# 0. COMPROBACIONES PREVIAS
############################################################

mensaje_paso(0, "Comprobar configuración y paquetes")

CONFIG$metodo_no_supervisado <- toupper(CONFIG$metodo_no_supervisado)
if (!CONFIG$metodo_no_supervisado %in% c("PCA")) {
  error_claro("Este examen está preparado con PCA como método no supervisado principal.")
}

if (!is.numeric(CONFIG$numero_clusters) || CONFIG$numero_clusters < 2) {
  error_claro("numero_clusters debe ser un número entero igual o mayor que 2.")
}

comprobar_paquetes(c("ggplot2", "cluster", "scatterplot3d"))
mensaje_ok("Configuración válida y paquetes disponibles.")

set.seed(CONFIG$semilla)

############################################################
# 1. CARGA E INSPECCIÓN DE LOS DATOS
############################################################

mensaje_paso(1, "Cargar e inspeccionar el conjunto de datos")

datos_originales <- leer_csv_seguro(
  ruta = CONFIG$archivo,
  sep = CONFIG$separador
)

inspeccionar_datos(datos_originales, "datos_originales")
validar_columna(datos_originales, CONFIG$objetivo, "datos_originales")

cat("\nDistribución de la variable de referencia (solo descriptiva):\n")
print(table(datos_originales[[CONFIG$objetivo]], useNA = "ifany"))

# Resumen de valores ausentes por columna.
mensaje_paso(1.1, "Resumen de valores ausentes y descriptivos básicos")

resumen_na <- crear_resumen_na(datos_originales)
resumen_na <- resumen_na[order(-resumen_na$Porcentaje_NA, resumen_na$Variable), ]
guardar_tabla(resumen_na, "14A_resumen_na.csv")

cat("\nColumnas con más NA:\n")
print(utils::head(resumen_na, 10))

# Descriptivos de columnas numéricas.
columnas_numericas <- names(datos_originales)[vapply(datos_originales, is.numeric, logical(1))]
columnas_categoricas <- setdiff(names(datos_originales), columnas_numericas)

if (length(columnas_numericas) > 0) {
  resumen_numerico <- data.frame(
    Variable = columnas_numericas,
    Media = vapply(datos_originales[columnas_numericas], function(x) mean(x, na.rm = TRUE), numeric(1)),
    Mediana = vapply(datos_originales[columnas_numericas], function(x) median(x, na.rm = TRUE), numeric(1)),
    Desvio = vapply(datos_originales[columnas_numericas], function(x) stats::sd(x, na.rm = TRUE), numeric(1)),
    Minimo = vapply(datos_originales[columnas_numericas], function(x) min(x, na.rm = TRUE), numeric(1)),
    Maximo = vapply(datos_originales[columnas_numericas], function(x) max(x, na.rm = TRUE), numeric(1)),
    stringsAsFactors = FALSE
  )
  guardar_tabla(resumen_numerico, "14A_resumen_numerico.csv")
  cat("\nResumen numérico:\n")
  print(resumen_numerico)
}

if (length(columnas_categoricas) > 0) {
  cat("\nFrecuencias de variables categóricas:\n")
  for (col in columnas_categoricas) {
    cat("\n---", col, "---\n")
    print(table(datos_originales[[col]], useNA = "ifany"))
  }
}

############################################################
# 2. PREPROCESAMIENTO PARA PCA
############################################################

mensaje_paso(2, "Preparar la matriz numérica para el método no supervisado")

# Se excluye Status para que no participe en el ajuste.
X_no_supervisado <- preparar_matriz_numerica(
  df = datos_originales,
  columnas_excluir = CONFIG$columnas_excluir,
  max_variables = NULL,
  imputar = TRUE,
  escalar = TRUE
)

cat("\nMatriz utilizada en el análisis no supervisado:\n")
cat("- Muestras:", nrow(X_no_supervisado), "\n")
cat("- Variables:", ncol(X_no_supervisado), "\n")

if (nrow(X_no_supervisado) < 4 || ncol(X_no_supervisado) < 3) {
  error_claro("Se necesitan al menos 4 observaciones y 3 variables numéricas para representar 3 dimensiones.")
}

############################################################
# 3. PCA EN 2D Y 3D
############################################################

mensaje_paso(3, "Aplicar PCA y extraer 2D / 3D")

modelo_pca <- prcomp(
  X_no_supervisado,
  center = FALSE,
  scale. = FALSE
)

if (ncol(modelo_pca$x) < 3) {
  error_claro("PCA no ha generado tres componentes utilizables.")
}

coordenadas_2d <- modelo_pca$x[, 1:2, drop = FALSE]
coordenadas_3d <- modelo_pca$x[, 1:3, drop = FALSE]
colnames(coordenadas_2d) <- c("Dim1", "Dim2")
colnames(coordenadas_3d) <- c("Dim1", "Dim2", "Dim3")

varianza_explicada <- modelo_pca$sdev^2 / sum(modelo_pca$sdev^2)
tabla_varianza <- data.frame(
  componente = paste0("PC", seq_along(varianza_explicada)),
  varianza_explicada = varianza_explicada,
  porcentaje = 100 * varianza_explicada,
  porcentaje_acumulado = 100 * cumsum(varianza_explicada)
)
guardar_tabla(tabla_varianza, "14A_varianza_explicada_pca.csv")

cat("\nVarianza explicada por PC1 + PC2:", round(100 * sum(varianza_explicada[1:2]), 2), "%\n")
cat("Varianza explicada por PC1 + PC2 + PC3:", round(100 * sum(varianza_explicada[1:3]), 2), "%\n")

etiquetas_ejes <- c(
  sprintf("PC1 (%.2f%%)", 100 * varianza_explicada[1]),
  sprintf("PC2 (%.2f%%)", 100 * varianza_explicada[2]),
  sprintf("PC3 (%.2f%%)", 100 * varianza_explicada[3])
)

############################################################
# 4. K-MEANS Y SILUETA SOBRE LAS COORDENADAS PCA
############################################################

mensaje_paso(4, "Aplicar K-means y calcular silueta")

k <- as.integer(CONFIG$numero_clusters)
puntos_distintos_2d <- nrow(unique(as.data.frame(coordenadas_2d)))
puntos_distintos_3d <- nrow(unique(as.data.frame(coordenadas_3d)))
maximo_k_posible <- min(
  nrow(X_no_supervisado) - 1,
  puntos_distintos_2d,
  puntos_distintos_3d
)

if (maximo_k_posible < 2) {
  error_claro("No hay suficientes puntos distintos para ejecutar K-means.")
}

k <- max(2, min(k, maximo_k_posible))
if (k != as.integer(CONFIG$numero_clusters)) {
  mensaje_aviso(
    paste0("El número de clústeres se ajustó automáticamente a ", k, ".")
  )
}

set.seed(CONFIG$semilla)
kmeans_2d <- kmeans(coordenadas_2d, centers = k, nstart = 50)
set.seed(CONFIG$semilla)
kmeans_3d <- kmeans(coordenadas_3d, centers = k, nstart = 50)

silueta_2d <- cluster::silhouette(kmeans_2d$cluster, stats::dist(coordenadas_2d))
silueta_3d <- cluster::silhouette(kmeans_3d$cluster, stats::dist(coordenadas_3d))

silueta_media_2d <- mean(silueta_2d[, "sil_width"])
silueta_media_3d <- mean(silueta_3d[, "sil_width"])

cat("\nSilueta media en 2D:", round(silueta_media_2d, 4), "\n")
cat("Silueta media en 3D:", round(silueta_media_3d, 4), "\n")

############################################################
# 5. TABLA FINAL DE COORDENADAS
############################################################

mensaje_paso(5, "Guardar coordenadas y clústeres")

resultado_no_supervisado <- data.frame(
  muestra = seq_len(nrow(datos_originales)),
  Dim1_2D = coordenadas_2d[, 1],
  Dim2_2D = coordenadas_2d[, 2],
  Cluster_2D = factor(kmeans_2d$cluster),
  Dim1_3D = coordenadas_3d[, 1],
  Dim2_3D = coordenadas_3d[, 2],
  Dim3_3D = coordenadas_3d[, 3],
  Cluster_3D = factor(kmeans_3d$cluster),
  Status = factor(datos_originales[[CONFIG$objetivo]])
)

guardar_tabla(resultado_no_supervisado, "14A_no_supervisado_coordenadas.csv")

cat("\nComparación posterior entre clústeres y Status (solo descriptiva):\n")
print(table(Cluster = resultado_no_supervisado$Cluster_2D, Status = resultado_no_supervisado$Status))

############################################################
# 6. VISUALIZACIÓN 2D
############################################################

mensaje_paso(6, "Crear la visualización 2D")

grafico_2d_clusters <- ggplot2::ggplot(
  resultado_no_supervisado,
  ggplot2::aes(x = Dim1_2D, y = Dim2_2D, color = Cluster_2D)
) +
  ggplot2::geom_point(size = 2.8, alpha = 0.85) +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::labs(
    title = "PCA en 2 dimensiones",
    subtitle = paste0("Color: K-means | k = ", k, " | silueta media = ", round(silueta_media_2d, 3)),
    x = etiquetas_ejes[1],
    y = etiquetas_ejes[2],
    color = "Clúster"
  )

print(grafico_2d_clusters)
guardar_ggplot(grafico_2d_clusters, "14A_pca_2d_clusters.png", ancho = 8, alto = 6)

if (isTRUE(CONFIG$colorear_tambien_por_objetivo)) {
  grafico_2d_status <- ggplot2::ggplot(
    resultado_no_supervisado,
    ggplot2::aes(x = Dim1_2D, y = Dim2_2D, color = Status)
  ) +
    ggplot2::geom_point(size = 2.8, alpha = 0.85) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::labs(
      title = "PCA en 2D: comparación descriptiva con Status",
      subtitle = "Status no participa en el ajuste no supervisado",
      x = etiquetas_ejes[1],
      y = etiquetas_ejes[2],
      color = "Status"
    )

  print(grafico_2d_status)
  guardar_ggplot(grafico_2d_status, "14A_pca_2d_status.png", ancho = 8, alto = 6)
}

############################################################
# 7. VISUALIZACIÓN 3D
############################################################

mensaje_paso(7, "Crear la visualización 3D")

paleta_clusters <- grDevices::rainbow(k)
colores_clusters <- paleta_clusters[as.integer(resultado_no_supervisado$Cluster_3D)]

png(
  filename = file.path("resultados", "14A_pca_3d_clusters.png"),
  width = 1200,
  height = 900,
  res = 150
)

scatterplot3d::scatterplot3d(
  x = resultado_no_supervisado$Dim1_3D,
  y = resultado_no_supervisado$Dim2_3D,
  z = resultado_no_supervisado$Dim3_3D,
  color = colores_clusters,
  pch = 19,
  main = "PCA en 3 dimensiones",
  xlab = etiquetas_ejes[1],
  ylab = etiquetas_ejes[2],
  zlab = etiquetas_ejes[3],
  grid = TRUE,
  box = FALSE
)

legend(
  "topright",
  legend = levels(resultado_no_supervisado$Cluster_3D),
  col = paleta_clusters,
  pch = 19,
  title = paste0("K-means\nSilueta = ", round(silueta_media_3d, 3))
)

dev.off()
mensaje_ok("Gráfico 3D guardado correctamente.")

if (isTRUE(CONFIG$colorear_tambien_por_objetivo)) {
  niveles_status <- levels(resultado_no_supervisado$Status)
  paleta_status <- grDevices::rainbow(length(niveles_status))
  colores_status <- paleta_status[as.integer(resultado_no_supervisado$Status)]

  png(
    filename = file.path("resultados", "14A_pca_3d_status.png"),
    width = 1200,
    height = 900,
    res = 150
  )

  scatterplot3d::scatterplot3d(
    x = resultado_no_supervisado$Dim1_3D,
    y = resultado_no_supervisado$Dim2_3D,
    z = resultado_no_supervisado$Dim3_3D,
    color = colores_status,
    pch = 19,
    main = "PCA en 3D: comparación descriptiva con Status",
    xlab = etiquetas_ejes[1],
    ylab = etiquetas_ejes[2],
    zlab = etiquetas_ejes[3],
    grid = TRUE,
    box = FALSE
  )

  legend(
    "topright",
    legend = niveles_status,
    col = paleta_status,
    pch = 19,
    title = "Status"
  )

  dev.off()
  mensaje_ok("Gráfico 3D por Status guardado correctamente.")
}

############################################################
# 8. INTERPRETACIÓN AUTOMÁTICA
############################################################

mensaje_paso(8, "Generar una interpretación para el examen")

mejor_dimension <- if (silueta_media_3d > silueta_media_2d) "3D" else "2D"
mejor_silueta <- max(silueta_media_2d, silueta_media_3d)

interpretacion_silueta <- if (mejor_silueta >= 0.70) {
  "La separación entre grupos es fuerte y la estructura resulta bastante clara."
} else if (mejor_silueta >= 0.50) {
  "La separación es razonable, aunque todavía existe cierto solapamiento."
} else if (mejor_silueta >= 0.25) {
  "La estructura de grupos es débil y debe interpretarse con cautela."
} else {
  "No se observa una estructura de agrupación clara con esta configuración."
}

lineas_informe <- c(
  "============================================================",
  "BORRADOR DE RESPUESTA: REDUCCIÓN DE DIMENSIONES NO SUPERVISADA",
  "============================================================",
  "",
  "Se cargó el conjunto cirrhosis.csv y se realizó una inspección básica de tipos, valores ausentes y estadísticas descriptivas.",
  "Las variables numéricas se limpiaron, se imputaron con la mediana cuando fue necesario y se estandarizaron antes de aplicar PCA.",
  paste0("La variable de referencia '", CONFIG$objetivo, "' no participó en el ajuste no supervisado; solo se utilizó para una comparación descriptiva posterior."),
  paste0("PCA se redujo a 2D y 3D. La varianza explicada por PC1+PC2 fue de ", round(100 * sum(varianza_explicada[1:2]), 2), "% y por PC1+PC2+PC3 de ", round(100 * sum(varianza_explicada[1:3]), 2), "%."),
  paste0("Sobre las coordenadas reducidas se aplicó K-means con k = ", k, " para visualizar agrupamientos potenciales."),
  paste0("La silueta media fue ", round(silueta_media_2d, 3), " en 2D y ", round(silueta_media_3d, 3), " en 3D. La representación más favorable fue la de ", mejor_dimension, "."),
  interpretacion_silueta,
  "",
  "Conclusión: PCA es una opción segura para este conjunto porque permite resumir la información numérica en pocas componentes y facilita la comparación visual en 2D/3D.",
  ""
)

cat(paste(lineas_informe, collapse = "\n"), "\n")
writeLines(lineas_informe, con = file.path("resultados", "14A_INTERPRETACION_FINAL.txt"))

############################################################
# 9. RESUMEN FINAL
############################################################

cat("\n============================================================\n")
cat("ANÁLISIS NO SUPERVISADO COMPLETADO\n")
cat("============================================================\n")
cat("Revisa la carpeta resultados/. Encontrarás:\n")
cat("1) Resumen de NA y descriptivos.\n")
cat("2) Tabla de varianza explicada de PCA.\n")
cat("3) Coordenadas reducidas y clústeres.\n")
cat("4) Gráficos 2D y 3D.\n")
cat("5) Interpretación final lista para el examen.\n")
cat("============================================================\n")
