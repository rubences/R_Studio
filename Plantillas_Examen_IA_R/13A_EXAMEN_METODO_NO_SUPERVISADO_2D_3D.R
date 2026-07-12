############################################################
# 13A_EXAMEN_METODO_NO_SUPERVISADO_2D_3D.R
############################################################
#
# OBJETIVO
# --------
# Resolver de forma segura un ejercicio que solicite:
#
#   1) aplicar UN método no supervisado;
#   2) representarlo en DOS dimensiones;
#   3) representarlo en TRES dimensiones;
#   4) identificar posibles grupos;
#   5) interpretar la calidad de la agrupación.
#
# MÉTODOS DISPONIBLES
# -------------------
# - PCA: opción recomendada por ser estable, rápida e interpretable.
# - UMAP: opción no lineal, solo cuando el enunciado lo pida.
#
# IMPORTANTE
# ----------
# - La variable objetivo NO participa en PCA, UMAP ni K-means.
# - Solo se usa al final, de forma opcional, para colorear y comparar.
# - Modifica únicamente el bloque CONFIGURACIÓN.
# - Ejecuta el archivo de arriba abajo.
############################################################

rm(list = ls())

# Esta plantilla utiliza las funciones comunes del paquete.
# Debe guardarse dentro de la carpeta Plantillas_Examen_IA_R,
# junto al archivo 00_UTILIDADES.R.
if (!file.exists("00_UTILIDADES.R")) {
  stop(
    paste0(
      "\nERROR: no se encuentra 00_UTILIDADES.R. ",
      "Abre Plantillas_Examen_IA_R.Rproj y ejecuta esta plantilla ",
      "desde la carpeta del proyecto.\nDirectorio actual: ", getwd()
    ),
    call. = FALSE
  )
}

source("00_UTILIDADES.R")
comprobar_directorio_proyecto()

############################################################
# CONFIGURACIÓN: SOLO EDITAR AQUÍ
############################################################
CONFIG <- list(

  # CSV situado dentro de la carpeta data/.
  archivo = file.path("data", "dataset_expresiongenes_cancer.csv"),

  # Etiqueta real. Se excluye totalmente del método no supervisado.
  # Solo se utiliza después para una comparación descriptiva.
  objetivo = "primaryormetastasis",

  # Identificadores u otras columnas que no deben analizarse.
  columnas_excluir = c("id", "ID", "sample", "patient"),

  # Límite de variables numéricas para evitar problemas de memoria.
  # Escribe NULL para utilizar todas las variables válidas.
  max_predictores = 50,

  # "PCA" es la opción más segura. "UMAP" es no lineal.
  metodo_no_supervisado = "PCA",

  # Número de grupos que se buscarán con K-means.
  numero_clusters = 2,

  # Semilla para que el resultado sea reproducible.
  semilla = 1995,

  # TRUE genera gráficos adicionales coloreados por la clase real.
  colorear_tambien_por_clase_real = TRUE
)
############################################################
# FIN DE CONFIGURACIÓN
############################################################

############################################################
# 0. COMPROBACIONES PREVIAS
############################################################

mensaje_paso(0, "Comprobar configuración y paquetes")

CONFIG$metodo_no_supervisado <- toupper(CONFIG$metodo_no_supervisado)
if (!CONFIG$metodo_no_supervisado %in% c("PCA", "UMAP")) {
  error_claro("metodo_no_supervisado solo puede ser 'PCA' o 'UMAP'.")
}

if (!is.numeric(CONFIG$numero_clusters) || CONFIG$numero_clusters < 2) {
  error_claro("numero_clusters debe ser un número entero igual o mayor que 2.")
}

paquetes_necesarios <- c("ggplot2", "cluster", "scatterplot3d")
if (CONFIG$metodo_no_supervisado == "UMAP") {
  paquetes_necesarios <- c(paquetes_necesarios, "uwot")
}
comprobar_paquetes(unique(paquetes_necesarios))
mensaje_ok("Configuración válida y paquetes disponibles.")

############################################################
# 1. CARGA E INSPECCIÓN DE LOS DATOS
############################################################

mensaje_paso(1, "Cargar e inspeccionar el conjunto de datos")

datos_originales <- leer_csv_seguro(CONFIG$archivo)
inspeccionar_datos(datos_originales, "datos_originales")
validar_columna(datos_originales, CONFIG$objetivo, "datos_originales")

cat("\nDistribución de la etiqueta real, usada solo al final:\n")
print(table(datos_originales[[CONFIG$objetivo]], useNA = "ifany"))

############################################################
# 2. MÉTODO NO SUPERVISADO EN 2D Y 3D
############################################################

mensaje_paso(2, "Preparar los datos del análisis no supervisado")

# Muy importante: se excluye la variable objetivo.
# Así garantizamos que PCA/UMAP y K-means no conocen las etiquetas.
columnas_no_supervisadas_excluir <- unique(
  c(CONFIG$columnas_excluir, CONFIG$objetivo)
)

# La función:
#   1) conserva únicamente variables numéricas;
#   2) convierte Inf en NA;
#   3) imputa NA con la mediana;
#   4) elimina variables constantes;
#   5) estandariza todas las variables.
X_no_supervisado <- preparar_matriz_numerica(
  df = datos_originales,
  columnas_excluir = columnas_no_supervisadas_excluir,
  max_variables = CONFIG$max_predictores,
  imputar = TRUE,
  escalar = TRUE
)

cat("\nMatriz usada en el análisis no supervisado:\n")
cat("- Muestras:", nrow(X_no_supervisado), "\n")
cat("- Variables:", ncol(X_no_supervisado), "\n")

if (nrow(X_no_supervisado) < 4 || ncol(X_no_supervisado) < 3) {
  error_claro(
    paste0(
      "Para representar en 3D hacen falta al menos 4 muestras y 3 variables ",
      "numéricas no constantes."
    )
  )
}

# ------------------------------------------------------------------
# 2.1. Reducción a 2 y 3 dimensiones
# ------------------------------------------------------------------

mensaje_paso(2.1, paste0("Aplicar ", CONFIG$metodo_no_supervisado, " en 2D y 3D"))

if (CONFIG$metodo_no_supervisado == "PCA") {

  # PCA encuentra combinaciones lineales ortogonales que concentran
  # la máxima varianza posible. Como X ya está estandarizada,
  # no volvemos a centrar ni escalar dentro de prcomp().
  modelo_no_supervisado <- prcomp(
    X_no_supervisado,
    center = FALSE,
    scale. = FALSE
  )

  if (ncol(modelo_no_supervisado$x) < 3) {
    error_claro("PCA no ha generado tres componentes utilizables.")
  }

  coordenadas_2d <- modelo_no_supervisado$x[, 1:2, drop = FALSE]
  coordenadas_3d <- modelo_no_supervisado$x[, 1:3, drop = FALSE]

  colnames(coordenadas_2d) <- c("Dim1", "Dim2")
  colnames(coordenadas_3d) <- c("Dim1", "Dim2", "Dim3")

  # Varianza explicada por cada componente.
  varianza_explicada <- modelo_no_supervisado$sdev^2 /
    sum(modelo_no_supervisado$sdev^2)

  tabla_varianza <- data.frame(
    componente = paste0("PC", seq_along(varianza_explicada)),
    varianza_explicada = varianza_explicada,
    porcentaje = 100 * varianza_explicada,
    porcentaje_acumulado = 100 * cumsum(varianza_explicada)
  )

  print(head(tabla_varianza, 10))
  guardar_tabla(tabla_varianza, "13A_pca_varianza_explicada.csv")

  cat("\nVarianza explicada por PC1 + PC2:",
      round(100 * sum(varianza_explicada[1:2]), 2), "%\n")
  cat("Varianza explicada por PC1 + PC2 + PC3:",
      round(100 * sum(varianza_explicada[1:3]), 2), "%\n")

  etiquetas_ejes <- c(
    sprintf("PC1 (%.2f%%)", 100 * varianza_explicada[1]),
    sprintf("PC2 (%.2f%%)", 100 * varianza_explicada[2]),
    sprintf("PC3 (%.2f%%)", 100 * varianza_explicada[3])
  )

} else {

  # UMAP conserva principalmente relaciones locales y puede detectar
  # estructuras no lineales. Se ajusta dos veces porque el enunciado
  # solicita expresamente una salida en 2D y otra en 3D.
  numero_vecinos <- max(
    2,
    min(round(0.15 * nrow(X_no_supervisado)), nrow(X_no_supervisado) - 1)
  )

  set.seed(CONFIG$semilla)
  coordenadas_2d <- uwot::umap(
    X_no_supervisado,
    n_neighbors = numero_vecinos,
    n_components = 2,
    min_dist = 0.10,
    n_threads = 1,
    verbose = FALSE
  )

  set.seed(CONFIG$semilla)
  coordenadas_3d <- uwot::umap(
    X_no_supervisado,
    n_neighbors = numero_vecinos,
    n_components = 3,
    min_dist = 0.10,
    n_threads = 1,
    verbose = FALSE
  )

  colnames(coordenadas_2d) <- c("Dim1", "Dim2")
  colnames(coordenadas_3d) <- c("Dim1", "Dim2", "Dim3")

  modelo_no_supervisado <- list(
    metodo = "UMAP",
    numero_vecinos = numero_vecinos,
    min_dist = 0.10
  )

  etiquetas_ejes <- c("UMAP1", "UMAP2", "UMAP3")

  cat("UMAP se ajustó con", numero_vecinos, "vecinos.\n")
}

# ------------------------------------------------------------------
# 2.2. K-means posterior para colorear y cuantificar estructura
# ------------------------------------------------------------------

mensaje_paso(2.2, "Aplicar K-means sobre las representaciones 2D y 3D")

k <- as.integer(CONFIG$numero_clusters)

# K-means necesita al menos tantos puntos distintos como clústeres.
# Calculamos un límite común para que funcionen tanto la salida 2D
# como la salida 3D.
puntos_distintos_2d <- nrow(unique(as.data.frame(coordenadas_2d)))
puntos_distintos_3d <- nrow(unique(as.data.frame(coordenadas_3d)))
maximo_k_posible <- min(
  nrow(X_no_supervisado) - 1,
  puntos_distintos_2d,
  puntos_distintos_3d
)

if (maximo_k_posible < 2) {
  error_claro(
    "No existen al menos dos puntos distintos para ejecutar K-means."
  )
}

k <- max(2, min(k, maximo_k_posible))

if (k != as.integer(CONFIG$numero_clusters)) {
  mensaje_aviso(
    paste0(
      "El número de clústeres se ha ajustado automáticamente a ", k,
      " para evitar un error por falta de puntos distintos."
    )
  )
}

set.seed(CONFIG$semilla)
kmeans_2d <- kmeans(
  coordenadas_2d,
  centers = k,
  nstart = 50
)

set.seed(CONFIG$semilla)
kmeans_3d <- kmeans(
  coordenadas_3d,
  centers = k,
  nstart = 50
)

# La silueta está entre -1 y 1:
#   próxima a 1  -> grupos bien separados;
#   próxima a 0  -> grupos solapados;
#   negativa     -> algunas muestras pueden estar mal asignadas.
silueta_2d <- cluster::silhouette(
  kmeans_2d$cluster,
  stats::dist(coordenadas_2d)
)

silueta_3d <- cluster::silhouette(
  kmeans_3d$cluster,
  stats::dist(coordenadas_3d)
)

silueta_media_2d <- mean(silueta_2d[, "sil_width"])
silueta_media_3d <- mean(silueta_3d[, "sil_width"])

cat("\nSilueta media en 2D:", round(silueta_media_2d, 4), "\n")
cat("Silueta media en 3D:", round(silueta_media_3d, 4), "\n")

# ------------------------------------------------------------------
# 2.3. Tabla completa de coordenadas y clústeres
# ------------------------------------------------------------------

clase_real_no_supervisado <- as.factor(
  datos_originales[[CONFIG$objetivo]]
)

resultado_no_supervisado <- data.frame(
  muestra = seq_len(nrow(datos_originales)),
  Dim1_2D = coordenadas_2d[, 1],
  Dim2_2D = coordenadas_2d[, 2],
  Cluster_2D = factor(kmeans_2d$cluster),
  Dim1_3D = coordenadas_3d[, 1],
  Dim2_3D = coordenadas_3d[, 2],
  Dim3_3D = coordenadas_3d[, 3],
  Cluster_3D = factor(kmeans_3d$cluster),
  Clase_real = clase_real_no_supervisado
)

guardar_tabla(
  resultado_no_supervisado,
  "13A_no_supervisado_coordenadas_2d_3d.csv"
)

# Comparación posterior. No interviene en el entrenamiento no supervisado.
cat("\nComparación POSTERIOR entre clústeres y clases reales (2D):\n")
print(table(
  Cluster = resultado_no_supervisado$Cluster_2D,
  ClaseReal = resultado_no_supervisado$Clase_real
))

cat("\nComparación POSTERIOR entre clústeres y clases reales (3D):\n")
print(table(
  Cluster = resultado_no_supervisado$Cluster_3D,
  ClaseReal = resultado_no_supervisado$Clase_real
))

# ------------------------------------------------------------------
# 2.4. Gráfico 2D coloreado por clúster
# ------------------------------------------------------------------

mensaje_paso(2.4, "Crear y guardar la visualización 2D")

grafico_2d_clusters <- ggplot2::ggplot(
  resultado_no_supervisado,
  ggplot2::aes(x = Dim1_2D, y = Dim2_2D, color = Cluster_2D)
) +
  ggplot2::geom_point(size = 2.8, alpha = 0.85) +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::labs(
    title = paste0(CONFIG$metodo_no_supervisado, " en 2 dimensiones"),
    subtitle = paste0(
      "Color: K-means | k = ", k,
      " | silueta media = ", round(silueta_media_2d, 3)
    ),
    x = etiquetas_ejes[1],
    y = etiquetas_ejes[2],
    color = "Clúster"
  )

print(grafico_2d_clusters)

guardar_ggplot(
  grafico_2d_clusters,
  "13A_no_supervisado_2d_clusters.png",
  ancho = 8,
  alto = 6
)

# Figura adicional coloreada por la clase real, únicamente para interpretar.
if (isTRUE(CONFIG$colorear_tambien_por_clase_real)) {
  grafico_2d_clase <- ggplot2::ggplot(
    resultado_no_supervisado,
    ggplot2::aes(x = Dim1_2D, y = Dim2_2D, color = Clase_real)
  ) +
    ggplot2::geom_point(size = 2.8, alpha = 0.85) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::labs(
      title = paste0(CONFIG$metodo_no_supervisado, " en 2D: clases reales"),
      subtitle = "La etiqueta se usa solo después del método no supervisado",
      x = etiquetas_ejes[1],
      y = etiquetas_ejes[2],
      color = "Clase real"
    )

  print(grafico_2d_clase)
  guardar_ggplot(
    grafico_2d_clase,
    "13A_no_supervisado_2d_clase_real.png",
    ancho = 8,
    alto = 6
  )
}

# ------------------------------------------------------------------
# 2.5. Gráfico 3D estático coloreado por clúster
# ------------------------------------------------------------------

mensaje_paso(2.5, "Crear y guardar la visualización 3D")

# Se utiliza scatterplot3d porque produce una figura 3D sin navegador
# y puede guardarse como PNG, algo útil durante un examen.
paleta_clusters <- grDevices::rainbow(k)
colores_clusters <- paleta_clusters[
  as.integer(resultado_no_supervisado$Cluster_3D)
]

png(
  filename = file.path("resultados", "13A_no_supervisado_3d_clusters.png"),
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
  main = paste0(CONFIG$metodo_no_supervisado, " en 3 dimensiones"),
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
  title = paste0(
    "K-means\nSilueta = ", round(silueta_media_3d, 3)
  )
)

dev.off()
mensaje_ok("Gráfico 3D guardado en resultados/13A_no_supervisado_3d_clusters.png")

# Figura 3D adicional coloreada por clase real.
if (isTRUE(CONFIG$colorear_tambien_por_clase_real)) {
  niveles_clase <- levels(resultado_no_supervisado$Clase_real)
  paleta_clases <- grDevices::rainbow(length(niveles_clase))
  colores_clases <- paleta_clases[
    as.integer(resultado_no_supervisado$Clase_real)
  ]

  png(
    filename = file.path("resultados", "13A_no_supervisado_3d_clase_real.png"),
    width = 1200,
    height = 900,
    res = 150
  )

  scatterplot3d::scatterplot3d(
    x = resultado_no_supervisado$Dim1_3D,
    y = resultado_no_supervisado$Dim2_3D,
    z = resultado_no_supervisado$Dim3_3D,
    color = colores_clases,
    pch = 19,
    main = paste0(
      CONFIG$metodo_no_supervisado,
      " en 3D: clases reales"
    ),
    xlab = etiquetas_ejes[1],
    ylab = etiquetas_ejes[2],
    zlab = etiquetas_ejes[3],
    grid = TRUE,
    box = FALSE
  )

  legend(
    "topright",
    legend = niveles_clase,
    col = paleta_clases,
    pch = 19,
    title = "Clase real"
  )

  dev.off()
  mensaje_ok("Gráfico 3D por clase guardado correctamente.")
}



############################################################
# 3. INTERPRETACIÓN AUTOMÁTICA
############################################################

mensaje_paso(3, "Generar una interpretación para el examen")

mejor_dimension <- if (silueta_media_3d > silueta_media_2d) "3D" else "2D"
mejor_silueta <- max(silueta_media_2d, silueta_media_3d)

interpretacion_silueta <- if (mejor_silueta >= 0.70) {
  "La estructura de clústeres es fuerte y los grupos aparecen bien separados."
} else if (mejor_silueta >= 0.50) {
  "La estructura de clústeres es razonable, aunque existe cierto solapamiento."
} else if (mejor_silueta >= 0.25) {
  "La estructura de clústeres es débil y debe interpretarse con cautela."
} else {
  "No se observa una estructura de clústeres clara con esta configuración."
}

lineas_informe <- c(
  "============================================================",
  "BORRADOR DE RESPUESTA: MÉTODO NO SUPERVISADO EN 2D Y 3D",
  "============================================================",
  "",
  paste0(
    "Se aplicó ", CONFIG$metodo_no_supervisado,
    " sobre predictores numéricos previamente imputados, depurados y estandarizados."
  ),
  paste0(
    "La variable objetivo '", CONFIG$objetivo,
    "' fue excluida completamente del ajuste para mantener el carácter no supervisado."
  ),
  paste0(
    "Posteriormente se aplicó K-means con k = ", k,
    " sobre las coordenadas reducidas."
  ),
  paste0(
    "La silueta media fue ", round(silueta_media_2d, 3),
    " en 2D y ", round(silueta_media_3d, 3), " en 3D."
  ),
  paste0(
    "La representación con mayor separación interna fue la de ", mejor_dimension, "."
  ),
  interpretacion_silueta,
  "",
  "La comparación con las clases reales se realizó únicamente después del ajuste y tiene finalidad descriptiva; no convierte el procedimiento en supervisado."
)

cat(paste(lineas_informe, collapse = "\n"), "\n")
writeLines(
  lineas_informe,
  con = file.path("resultados", "13A_INTERPRETACION_NO_SUPERVISADO.txt")
)

############################################################
# 4. RESUMEN FINAL
############################################################

cat("\n============================================================\n")
cat("ANÁLISIS NO SUPERVISADO COMPLETADO\n")
cat("============================================================\n")
cat("Revisa la carpeta resultados/. Encontrarás:\n")
cat("1) Coordenadas y clústeres en 2D y 3D.\n")
cat("2) Gráfico 2D por clúster.\n")
cat("3) Gráfico 3D por clúster.\n")
cat("4) Gráficos por clase real, cuando estén activados.\n")
cat("5) Tabla de varianza explicada, cuando se use PCA.\n")
cat("6) Interpretación automática.\n")
cat("============================================================\n")
