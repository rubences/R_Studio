############################################################
# 13A_EJEMPLO_CUMPLIMENTADO_NO_SUPERVISADO_2D_3D_IRIS.R
############################################################
#
# EJEMPLO COMPLETO Y EJECUTABLE
# -----------------------------
# Este archivo resuelve un ejercicio típico de examen:
#
#   1. Cargar y revisar los datos.
#   2. Separar la etiqueta real de los predictores.
#   3. Aplicar un método NO supervisado: PCA.
#   4. Representar los datos en 2 dimensiones.
#   5. Representar los datos en 3 dimensiones.
#   6. Aplicar K-means para descubrir grupos.
#   7. Evaluar los grupos con el índice de silueta.
#   8. Comparar, solo al final, los clústeres con la clase real.
#   9. Guardar gráficos, tablas, modelo e interpretación.
#
# DATASET UTILIZADO
# -----------------
# Se utiliza iris, incluido de serie en R. Por tanto, no hace
# falta descargar ni localizar ningún CSV para probar el ejemplo.
#
# MUY IMPORTANTE
# --------------
# La variable Species NO participa en PCA ni en K-means.
# Solo se utiliza al final para comprobar si los grupos hallados
# guardan relación con las especies conocidas.
############################################################


############################################################
# 0. LIMPIEZA DEL ENTORNO Y CONFIGURACIÓN
############################################################

# Borra los objetos existentes para evitar que variables antiguas
# alteren accidentalmente los resultados del ejemplo.
rm(list = ls())

# Configuración ya cumplimentada.
SEMILLA <- 1995
NUMERO_CLUSTERS <- 3
CARPETA_RESULTADOS <- "resultados_ejemplo_13A"

# Creamos la carpeta donde se guardarán todos los resultados.
dir.create(
  CARPETA_RESULTADOS,
  showWarnings = FALSE,
  recursive = TRUE
)

# Fijamos la semilla para que K-means produzca el mismo resultado
# cada vez que se ejecuta el archivo.
set.seed(SEMILLA)


############################################################
# 1. COMPROBACIÓN E INSTALACIÓN DE PAQUETES
############################################################

# Esta función comprueba si los paquetes están disponibles.
# Si falta alguno, intenta instalarlo automáticamente desde CRAN.
asegurar_paquetes <- function(paquetes) {

  faltan <- paquetes[
    !vapply(paquetes, requireNamespace, logical(1), quietly = TRUE)
  ]

  if (length(faltan) > 0) {
    message(
      "Faltan los paquetes: ",
      paste(faltan, collapse = ", "),
      ". Se intentarán instalar."
    )

    try(
      install.packages(
        faltan,
        dependencies = TRUE,
        repos = "https://cloud.r-project.org"
      ),
      silent = TRUE
    )
  }

  # Volvemos a comprobar después del intento de instalación.
  siguen_faltando <- paquetes[
    !vapply(paquetes, requireNamespace, logical(1), quietly = TRUE)
  ]

  if (length(siguen_faltando) > 0) {
    stop(
      paste0(
        "\nERROR: no se pudieron cargar estos paquetes: ",
        paste(siguen_faltando, collapse = ", "),
        ".\nInstálalos manualmente antes de ejecutar el ejemplo:\n",
        "install.packages(c(",
        paste(sprintf('"%s"', siguen_faltando), collapse = ", "),
        "))"
      ),
      call. = FALSE
    )
  }
}

asegurar_paquetes(
  c("ggplot2", "cluster", "scatterplot3d")
)

library(ggplot2)
library(cluster)
library(scatterplot3d)


############################################################
# 2. CARGA E INSPECCIÓN DEL DATASET
############################################################

# iris está incluido en R.
datos <- iris

# Añadimos un identificador únicamente para reconocer las muestras.
# Este identificador tampoco se empleará en el análisis.
datos$id_muestra <- sprintf("M%03d", seq_len(nrow(datos)))

cat("\n============================================================\n")
cat("PASO 1: INSPECCIÓN DEL CONJUNTO DE DATOS\n")
cat("============================================================\n")

cat("\nDimensiones del dataset:\n")
print(dim(datos))

cat("\nNombres de las columnas:\n")
print(names(datos))

cat("\nEstructura de las variables:\n")
str(datos)

cat("\nPrimeras seis filas:\n")
print(head(datos))

cat("\nDistribución de la clase real Species:\n")
print(table(datos$Species))

cat("\nNúmero total de valores ausentes:\n")
print(sum(is.na(datos)))


############################################################
# 3. SEPARACIÓN DE PREDICTORES Y ETIQUETA
############################################################

cat("\n============================================================\n")
cat("PASO 2: PREPARACIÓN DE LOS PREDICTORES\n")
cat("============================================================\n")

# Seleccionamos únicamente las cuatro variables numéricas.
# Species se excluye porque el análisis debe ser no supervisado.
predictores <- datos[
  ,
  c(
    "Sepal.Length",
    "Sepal.Width",
    "Petal.Length",
    "Petal.Width"
  )
]

# Comprobación defensiva: PCA necesita variables numéricas.
if (!all(vapply(predictores, is.numeric, logical(1)))) {
  stop(
    "ERROR: todos los predictores deben ser numéricos.",
    call. = FALSE
  )
}

# Comprobamos que no haya valores perdidos.
if (anyNA(predictores)) {
  stop(
    "ERROR: hay valores NA en los predictores.",
    call. = FALSE
  )
}

# Estandarizamos:
#   media = 0
#   desviación típica = 1
#
# Esto es importante porque las variables pueden estar en escalas
# diferentes y no queremos que una domine el análisis solo por
# tener valores numéricos mayores.
X_escalada <- scale(predictores)

cat("\nMedias después del escalado (deben estar próximas a 0):\n")
print(round(colMeans(X_escalada), 6))

cat("\nDesviaciones típicas después del escalado (deben ser 1):\n")
print(round(apply(X_escalada, 2, sd), 6))


############################################################
# 4. PCA: MÉTODO NO SUPERVISADO
############################################################

cat("\n============================================================\n")
cat("PASO 3: APLICACIÓN DE PCA\n")
cat("============================================================\n")

# Como los datos ya están estandarizados, no volvemos a escalar.
modelo_pca <- prcomp(
  X_escalada,
  center = FALSE,
  scale. = FALSE
)

cat("\nResumen del modelo PCA:\n")
print(summary(modelo_pca))

# Calculamos la proporción de varianza explicada por cada componente.
varianza_explicada <- modelo_pca$sdev^2 /
  sum(modelo_pca$sdev^2)

tabla_varianza <- data.frame(
  Componente = paste0("PC", seq_along(varianza_explicada)),
  Varianza_explicada = varianza_explicada,
  Porcentaje = 100 * varianza_explicada,
  Porcentaje_acumulado = 100 * cumsum(varianza_explicada)
)

cat("\nTabla de varianza explicada:\n")
print(tabla_varianza)

write.csv(
  tabla_varianza,
  file = file.path(
    CARPETA_RESULTADOS,
    "01_varianza_explicada_pca.csv"
  ),
  row.names = FALSE
)

# Extraemos las coordenadas de cada muestra en el nuevo espacio.
coordenadas <- as.data.frame(modelo_pca$x)

# Añadimos información que solo servirá para representar y comparar.
coordenadas$id_muestra <- datos$id_muestra
coordenadas$Species <- datos$Species

write.csv(
  coordenadas,
  file = file.path(
    CARPETA_RESULTADOS,
    "02_coordenadas_pca.csv"
  ),
  row.names = FALSE
)

# Guardamos el modelo para poder reutilizarlo.
saveRDS(
  modelo_pca,
  file = file.path(
    CARPETA_RESULTADOS,
    "modelo_pca.rds"
  )
)


############################################################
# 5. K-MEANS PARA IDENTIFICAR TRES GRUPOS
############################################################

cat("\n============================================================\n")
cat("PASO 4: AGRUPACIÓN MEDIANTE K-MEANS\n")
cat("============================================================\n")

# K-means se aplica sobre las variables estandarizadas completas.
# No usa Species.
#
# nstart = 50 prueba 50 inicializaciones distintas y conserva
# la mejor solución, reduciendo el riesgo de un resultado pobre.
set.seed(SEMILLA)

modelo_kmeans <- kmeans(
  X_escalada,
  centers = NUMERO_CLUSTERS,
  nstart = 50,
  iter.max = 100
)

cat("\nResultado del modelo K-means:\n")
print(modelo_kmeans)

# Convertimos los números de clúster en un factor.
coordenadas$Cluster <- factor(
  modelo_kmeans$cluster,
  levels = seq_len(NUMERO_CLUSTERS),
  labels = paste0("Cluster_", seq_len(NUMERO_CLUSTERS))
)

cat("\nNúmero de muestras asignadas a cada clúster:\n")
print(table(coordenadas$Cluster))

# Guardamos el modelo K-means.
saveRDS(
  modelo_kmeans,
  file = file.path(
    CARPETA_RESULTADOS,
    "modelo_kmeans.rds"
  )
)


############################################################
# 6. EVALUACIÓN MEDIANTE ÍNDICE DE SILUETA
############################################################

cat("\n============================================================\n")
cat("PASO 5: EVALUACIÓN DE LOS CLÚSTERES\n")
cat("============================================================\n")

# La silueta compara:
#   a) la cohesión de cada muestra con su propio clúster;
#   b) su separación respecto al clúster alternativo más próximo.
#
# Interpretación orientativa:
#   valor próximo a 1   -> grupos bien separados;
#   valor próximo a 0   -> grupos solapados;
#   valor negativo      -> posible asignación incorrecta.
distancias <- dist(X_escalada)

silueta <- cluster::silhouette(
  modelo_kmeans$cluster,
  distancias
)

silueta_media <- mean(silueta[, "sil_width"])

cat(
  "\nÍndice de silueta medio:",
  round(silueta_media, 4),
  "\n"
)

tabla_silueta <- as.data.frame(silueta)
tabla_silueta$id_muestra <- datos$id_muestra

write.csv(
  tabla_silueta,
  file = file.path(
    CARPETA_RESULTADOS,
    "03_silueta_por_muestra.csv"
  ),
  row.names = FALSE
)

png(
  filename = file.path(
    CARPETA_RESULTADOS,
    "04_grafico_silueta.png"
  ),
  width = 1200,
  height = 850,
  res = 130
)

plot(
  silueta,
  main = paste0(
    "Índice de silueta de K-means (media = ",
    round(silueta_media, 3),
    ")"
  )
)

dev.off()


############################################################
# 7. REPRESENTACIÓN DEL MÉTODO EN DOS DIMENSIONES
############################################################

cat("\n============================================================\n")
cat("PASO 6: REPRESENTACIÓN PCA EN 2D\n")
cat("============================================================\n")

etiqueta_pc1 <- sprintf(
  "PC1 (%.2f%%)",
  100 * varianza_explicada[1]
)

etiqueta_pc2 <- sprintf(
  "PC2 (%.2f%%)",
  100 * varianza_explicada[2]
)

# Gráfico principal: coloreado por el clúster descubierto.
grafico_2d_clusters <- ggplot(
  coordenadas,
  aes(
    x = PC1,
    y = PC2,
    colour = Cluster,
    shape = Cluster
  )
) +
  geom_point(size = 3, alpha = 0.85) +
  labs(
    title = "PCA en 2D: grupos identificados mediante K-means",
    subtitle = "La variable Species no se utilizó para construir los grupos",
    x = etiqueta_pc1,
    y = etiqueta_pc2,
    colour = "Clúster",
    shape = "Clúster"
  ) +
  theme_minimal(base_size = 12)

print(grafico_2d_clusters)

ggsave(
  filename = file.path(
    CARPETA_RESULTADOS,
    "05_pca_2d_coloreado_por_cluster.png"
  ),
  plot = grafico_2d_clusters,
  width = 9,
  height = 6,
  dpi = 160
)

# Gráfico de comprobación: coloreado por la especie real.
# Este gráfico NO transforma el análisis en supervisado porque
# Species solo se añade después de obtener PCA y K-means.
grafico_2d_especies <- ggplot(
  coordenadas,
  aes(
    x = PC1,
    y = PC2,
    colour = Species,
    shape = Species
  )
) +
  geom_point(size = 3, alpha = 0.85) +
  labs(
    title = "PCA en 2D: comparación con la especie real",
    subtitle = "La etiqueta solo se usa para interpretar el resultado",
    x = etiqueta_pc1,
    y = etiqueta_pc2,
    colour = "Especie",
    shape = "Especie"
  ) +
  theme_minimal(base_size = 12)

print(grafico_2d_especies)

ggsave(
  filename = file.path(
    CARPETA_RESULTADOS,
    "06_pca_2d_coloreado_por_especie_real.png"
  ),
  plot = grafico_2d_especies,
  width = 9,
  height = 6,
  dpi = 160
)


############################################################
# 8. REPRESENTACIÓN DEL MÉTODO EN TRES DIMENSIONES
############################################################

cat("\n============================================================\n")
cat("PASO 7: REPRESENTACIÓN PCA EN 3D\n")
cat("============================================================\n")

etiqueta_pc3 <- sprintf(
  "PC3 (%.2f%%)",
  100 * varianza_explicada[3]
)

# Asignamos un color a cada clúster.
colores_clusters <- grDevices::rainbow(NUMERO_CLUSTERS)
color_por_muestra <- colores_clusters[modelo_kmeans$cluster]

png(
  filename = file.path(
    CARPETA_RESULTADOS,
    "07_pca_3d_coloreado_por_cluster.png"
  ),
  width = 1300,
  height = 950,
  res = 140
)

scatterplot3d::scatterplot3d(
  x = coordenadas$PC1,
  y = coordenadas$PC2,
  z = coordenadas$PC3,
  color = color_por_muestra,
  pch = 19,
  angle = 55,
  xlab = etiqueta_pc1,
  ylab = etiqueta_pc2,
  zlab = etiqueta_pc3,
  main = "PCA en 3D: clústeres obtenidos mediante K-means",
  grid = TRUE,
  box = TRUE
)

legend(
  "topright",
  legend = paste0("Cluster_", seq_len(NUMERO_CLUSTERS)),
  col = colores_clusters,
  pch = 19,
  cex = 0.9
)

dev.off()

# Segundo gráfico 3D: comparación con las especies reales.
niveles_especie <- levels(datos$Species)
colores_especies <- grDevices::rainbow(length(niveles_especie))
color_especie_muestra <- colores_especies[
  as.integer(datos$Species)
]

png(
  filename = file.path(
    CARPETA_RESULTADOS,
    "08_pca_3d_coloreado_por_especie_real.png"
  ),
  width = 1300,
  height = 950,
  res = 140
)

scatterplot3d::scatterplot3d(
  x = coordenadas$PC1,
  y = coordenadas$PC2,
  z = coordenadas$PC3,
  color = color_especie_muestra,
  pch = 19,
  angle = 55,
  xlab = etiqueta_pc1,
  ylab = etiqueta_pc2,
  zlab = etiqueta_pc3,
  main = "PCA en 3D: comparación con la especie real",
  grid = TRUE,
  box = TRUE
)

legend(
  "topright",
  legend = niveles_especie,
  col = colores_especies,
  pch = 19,
  cex = 0.9
)

dev.off()


############################################################
# 9. COMPARACIÓN POSTERIOR CON LA CLASE REAL
############################################################

cat("\n============================================================\n")
cat("PASO 8: COMPARACIÓN CLÚSTERES - ESPECIE REAL\n")
cat("============================================================\n")

# Esta tabla no se utiliza para entrenar. Únicamente ayuda a explicar
# si los clústeres descubiertos coinciden con las clases conocidas.
tabla_cluster_especie <- table(
  Cluster = coordenadas$Cluster,
  Species = coordenadas$Species
)

cat("\nTabla de contingencia:\n")
print(tabla_cluster_especie)

write.csv(
  as.data.frame.matrix(tabla_cluster_especie),
  file = file.path(
    CARPETA_RESULTADOS,
    "09_tabla_cluster_vs_especie.csv"
  )
)

# Guardamos las coordenadas finales con clúster y especie.
write.csv(
  coordenadas,
  file = file.path(
    CARPETA_RESULTADOS,
    "10_resultado_completo_muestras.csv"
  ),
  row.names = FALSE
)


############################################################
# 10. INTERPRETACIÓN AUTOMÁTICA PARA EL EXAMEN
############################################################

cat("\n============================================================\n")
cat("PASO 9: INTERPRETACIÓN FINAL\n")
cat("============================================================\n")

porcentaje_2d <- 100 * sum(varianza_explicada[1:2])
porcentaje_3d <- 100 * sum(varianza_explicada[1:3])

interpretacion_silueta <- if (silueta_media >= 0.50) {
  "La estructura de clústeres es razonablemente clara."
} else if (silueta_media >= 0.25) {
  "La estructura de clústeres es débil o moderada y existe solapamiento."
} else {
  "La estructura de clústeres es poco definida y los grupos se solapan."
}

texto_interpretacion <- c(
  "INTERPRETACIÓN DEL EJEMPLO NO SUPERVISADO",
  "=========================================",
  "",
  paste0(
    "Se aplicó PCA sobre las cuatro variables numéricas ",
    "estandarizadas del conjunto iris."
  ),
  paste0(
    "Las dos primeras componentes explican conjuntamente el ",
    round(porcentaje_2d, 2),
    "% de la variabilidad total."
  ),
  paste0(
    "Las tres primeras componentes explican conjuntamente el ",
    round(porcentaje_3d, 2),
    "% de la variabilidad total."
  ),
  paste0(
    "Después se aplicó K-means con k = ",
    NUMERO_CLUSTERS,
    " sobre los predictores estandarizados."
  ),
  paste0(
    "El índice de silueta medio fue ",
    round(silueta_media, 3),
    ". ",
    interpretacion_silueta
  ),
  paste0(
    "La etiqueta Species se excluyó completamente de PCA y K-means ",
    "y solo se utilizó al final para interpretar la correspondencia ",
    "entre los clústeres y las especies reales."
  ),
  "",
  "FRASE MODELO PARA EL EXAMEN",
  "---------------------------",
  paste0(
    "PCA permitió reducir los datos a dos y tres dimensiones, ",
    "conservando respectivamente el ",
    round(porcentaje_2d, 2),
    "% y el ",
    round(porcentaje_3d, 2),
    "% de la varianza. K-means identificó ",
    NUMERO_CLUSTERS,
    " grupos y la silueta media fue ",
    round(silueta_media, 3),
    ", por lo que ",
    tolower(interpretacion_silueta)
  )
)

cat(paste(texto_interpretacion, collapse = "\n"))
cat("\n")

writeLines(
  texto_interpretacion,
  con = file.path(
    CARPETA_RESULTADOS,
    "11_interpretacion_final.txt"
  )
)


############################################################
# 11. MENSAJE FINAL
############################################################

cat("\n============================================================\n")
cat("EJEMPLO FINALIZADO CORRECTAMENTE\n")
cat("============================================================\n")
cat(
  "Todos los resultados se han guardado en:\n",
  normalizePath(
    CARPETA_RESULTADOS,
    winslash = "/",
    mustWork = FALSE
  ),
  "\n",
  sep = ""
)
