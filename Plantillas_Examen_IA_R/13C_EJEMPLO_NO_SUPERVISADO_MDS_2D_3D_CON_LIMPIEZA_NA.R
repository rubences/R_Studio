############################################################
# 13C_EJEMPLO_NO_SUPERVISADO_MDS_2D_3D_CON_LIMPIEZA_NA.R
############################################################
#
# EJEMPLO NUEVO, COMPLETO Y EJECUTABLE
# ------------------------------------
# Objetivo:
#
#   1. Trabajar con un conjunto que contiene valores NA reales.
#   2. Detectar y cuantificar los datos ausentes.
#   3. Eliminar filas o columnas con demasiada información perdida.
#   4. Imputar los NA restantes mediante la mediana.
#   5. Estandarizar los predictores.
#   6. Aplicar un método NO supervisado: MDS clásico.
#   7. Representar el resultado en 2 dimensiones.
#   8. Representar el resultado en 3 dimensiones.
#   9. Obtener grupos mediante K-means.
#  10. Evaluar los grupos mediante el índice de silueta.
#
# DATASET
# -------
# Se utiliza airquality, incluido de serie en R.
# Este conjunto contiene valores NA reales en Ozone y Solar.R.
#
# MÉTODO DE REDUCCIÓN
# -------------------
# MDS clásico o escalamiento multidimensional.
#
# MUY IMPORTANTE
# --------------
# Month se conserva únicamente como información auxiliar para
# interpretar los resultados. No se utiliza para formar los grupos.
############################################################


############################################################
# 0. LIMPIEZA DEL ENTORNO Y CONFIGURACIÓN
############################################################

rm(list = ls())

SEMILLA <- 1995

# Número de clústeres que se buscarán con K-means.
NUMERO_CLUSTERS <- 3

# Si una columna supera este porcentaje de NA, se eliminará.
UMBRAL_NA_COLUMNAS <- 0.40

# Si una fila supera este porcentaje de NA, se eliminará.
UMBRAL_NA_FILAS <- 0.50

CARPETA_RESULTADOS <- "resultados_ejemplo_13C_MDS"

dir.create(
  CARPETA_RESULTADOS,
  showWarnings = FALSE,
  recursive = TRUE
)

set.seed(SEMILLA)


############################################################
# 1. COMPROBACIÓN E INSTALACIÓN DE PAQUETES
############################################################

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

  siguen_faltando <- paquetes[
    !vapply(paquetes, requireNamespace, logical(1), quietly = TRUE)
  ]

  if (length(siguen_faltando) > 0) {

    stop(
      paste0(
        "\nERROR: no se pudieron cargar estos paquetes: ",
        paste(siguen_faltando, collapse = ", "),
        ".\nInstálalos manualmente con:\n",
        "install.packages(c(",
        paste(
          sprintf('"%s"', siguen_faltando),
          collapse = ", "
        ),
        "))"
      ),
      call. = FALSE
    )
  }
}

asegurar_paquetes(
  c(
    "ggplot2",
    "cluster",
    "scatterplot3d"
  )
)

library(ggplot2)
library(cluster)
library(scatterplot3d)


############################################################
# 2. FUNCIONES AUXILIARES PARA LIMPIAR LOS NA
############################################################

# Genera una tabla con el número y porcentaje de NA de cada columna.
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


# Sustituye valores infinitos por NA.
# Esto evita que scale(), dist() o MDS fallen.
convertir_infinitos_en_na <- function(dataframe) {

  for (nombre in names(dataframe)) {

    if (is.numeric(dataframe[[nombre]])) {

      dataframe[[nombre]][
        !is.finite(dataframe[[nombre]])
      ] <- NA
    }
  }

  dataframe
}


# Imputa cada variable numérica usando su mediana.
imputar_mediana <- function(dataframe) {

  medianas <- vapply(
    dataframe,
    function(x) median(x, na.rm = TRUE),
    numeric(1)
  )

  if (any(!is.finite(medianas))) {

    stop(
      paste0(
        "ERROR: alguna variable no tiene ningún valor válido. ",
        "Debe eliminarse antes de imputar."
      ),
      call. = FALSE
    )
  }

  resultado <- dataframe

  for (nombre in names(resultado)) {

    resultado[[nombre]][
      is.na(resultado[[nombre]])
    ] <- medianas[[nombre]]
  }

  list(
    datos = resultado,
    medianas = medianas
  )
}


############################################################
# 3. CARGA E INSPECCIÓN DE LOS DATOS
############################################################

cat("\n============================================================\n")
cat("PASO 1: CARGAR E INSPECCIONAR AIRQUALITY\n")
cat("============================================================\n")

datos_originales <- airquality

# Identificador para poder localizar cada fila original.
datos_originales$id_fila_original <- seq_len(
  nrow(datos_originales)
)

cat("\nDimensiones originales:\n")
print(dim(datos_originales))

cat("\nEstructura original:\n")
str(datos_originales)

cat("\nPrimeras filas:\n")
print(head(datos_originales))

cat("\nNúmero total de NA:\n")
print(sum(is.na(datos_originales)))

resumen_na_original <- crear_resumen_na(
  datos_originales
)

cat("\nResumen de NA por variable:\n")
print(resumen_na_original)

write.csv(
  resumen_na_original,
  file = file.path(
    CARPETA_RESULTADOS,
    "01_resumen_na_original.csv"
  ),
  row.names = FALSE
)


############################################################
# 4. SELECCIÓN DE VARIABLES
############################################################

cat("\n============================================================\n")
cat("PASO 2: SEPARAR PREDICTORES Y METADATOS\n")
cat("============================================================\n")

# Month se conserva para interpretar los clústeres.
metadatos <- data.frame(
  id_fila_original = datos_originales$id_fila_original,
  Month = factor(
    datos_originales$Month,
    levels = 5:9,
    labels = c(
      "Mayo",
      "Junio",
      "Julio",
      "Agosto",
      "Septiembre"
    )
  ),
  Day = datos_originales$Day
)

# Se utilizan solamente medidas ambientales.
predictores <- datos_originales[
  ,
  c(
    "Ozone",
    "Solar.R",
    "Wind",
    "Temp"
  ),
  drop = FALSE
]

predictores <- convertir_infinitos_en_na(
  predictores
)

if (!all(vapply(predictores, is.numeric, logical(1)))) {

  stop(
    "ERROR: todos los predictores deben ser numéricos.",
    call. = FALSE
  )
}


############################################################
# 5. ELIMINAR FILAS CON DEMASIADOS NA
############################################################

cat("\n============================================================\n")
cat("PASO 3: ELIMINAR FILAS CON DEMASIADOS NA\n")
cat("============================================================\n")

porcentaje_na_fila <- rowMeans(
  is.na(predictores)
)

filas_eliminar <- porcentaje_na_fila >
  UMBRAL_NA_FILAS

cat(
  "\nFilas eliminadas por superar el ",
  100 * UMBRAL_NA_FILAS,
  "% de NA: ",
  sum(filas_eliminar),
  "\n",
  sep = ""
)

if (any(filas_eliminar)) {

  tabla_filas_eliminadas <- data.frame(
    id_fila_original = metadatos$id_fila_original[
      filas_eliminar
    ],
    Porcentaje_NA = 100 * porcentaje_na_fila[
      filas_eliminar
    ]
  )

  write.csv(
    tabla_filas_eliminadas,
    file = file.path(
      CARPETA_RESULTADOS,
      "02_filas_eliminadas_por_na.csv"
    ),
    row.names = FALSE
  )
}

predictores <- predictores[
  !filas_eliminar,
  ,
  drop = FALSE
]

metadatos <- metadatos[
  !filas_eliminar,
  ,
  drop = FALSE
]

rownames(predictores) <- NULL
rownames(metadatos) <- NULL


############################################################
# 6. ELIMINAR COLUMNAS CON DEMASIADOS NA
############################################################

cat("\n============================================================\n")
cat("PASO 4: ELIMINAR COLUMNAS CON DEMASIADOS NA\n")
cat("============================================================\n")

porcentaje_na_columna <- colMeans(
  is.na(predictores)
)

columnas_eliminar <- names(
  porcentaje_na_columna[
    porcentaje_na_columna > UMBRAL_NA_COLUMNAS
  ]
)

cat("\nPorcentaje de NA por predictor:\n")
print(
  round(
    100 * porcentaje_na_columna,
    2
  )
)

if (length(columnas_eliminar) > 0) {

  cat(
    "\nColumnas eliminadas por exceso de NA:\n"
  )

  print(columnas_eliminar)

  predictores <- predictores[
    ,
    setdiff(
      names(predictores),
      columnas_eliminar
    ),
    drop = FALSE
  ]

} else {

  cat(
    "\nNo es necesario eliminar ninguna columna.\n"
  )
}

if (ncol(predictores) < 3) {

  stop(
    paste0(
      "ERROR: después de eliminar columnas no quedan ",
      "al menos tres variables para la representación 3D."
    ),
    call. = FALSE
  )
}


############################################################
# 7. IMPUTACIÓN MEDIANTE LA MEDIANA
############################################################

cat("\n============================================================\n")
cat("PASO 5: IMPUTAR LOS NA RESTANTES\n")
cat("============================================================\n")

resultado_imputacion <- imputar_mediana(
  predictores
)

predictores_limpios <- resultado_imputacion$datos
medianas_imputacion <- resultado_imputacion$medianas

tabla_medianas <- data.frame(
  Variable = names(medianas_imputacion),
  Mediana_utilizada = as.numeric(
    medianas_imputacion
  )
)

cat("\nMedianas usadas para sustituir los NA:\n")
print(tabla_medianas)

write.csv(
  tabla_medianas,
  file = file.path(
    CARPETA_RESULTADOS,
    "03_medianas_imputacion.csv"
  ),
  row.names = FALSE
)

cat("\nNA restantes después de imputar:\n")
print(sum(is.na(predictores_limpios)))

if (anyNA(predictores_limpios)) {

  stop(
    "ERROR: todavía quedan valores NA.",
    call. = FALSE
  )
}


############################################################
# 8. ELIMINAR VARIABLES SIN VARIABILIDAD
############################################################

cat("\n============================================================\n")
cat("PASO 6: COMPROBAR LA VARIABILIDAD\n")
cat("============================================================\n")

varianzas <- vapply(
  predictores_limpios,
  var,
  numeric(1)
)

variables_sin_variacion <- names(
  varianzas[
    !is.finite(varianzas) |
      varianzas == 0
  ]
)

if (length(variables_sin_variacion) > 0) {

  cat(
    "\nVariables eliminadas por varianza cero:\n"
  )

  print(variables_sin_variacion)

  predictores_limpios <- predictores_limpios[
    ,
    setdiff(
      names(predictores_limpios),
      variables_sin_variacion
    ),
    drop = FALSE
  ]

} else {

  cat(
    "\nTodas las variables conservan variabilidad.\n"
  )
}


############################################################
# 9. ESTANDARIZACIÓN
############################################################

cat("\n============================================================\n")
cat("PASO 7: ESTANDARIZAR LOS DATOS\n")
cat("============================================================\n")

X_escalada <- scale(
  predictores_limpios
)

cat("\nMedias tras el escalado:\n")
print(
  round(
    colMeans(X_escalada),
    6
  )
)

cat("\nDesviaciones típicas tras el escalado:\n")
print(
  round(
    apply(
      X_escalada,
      2,
      sd
    ),
    6
  )
)

datos_limpios_guardar <- cbind(
  metadatos,
  predictores_limpios
)

write.csv(
  datos_limpios_guardar,
  file = file.path(
    CARPETA_RESULTADOS,
    "04_dataset_limpio_e_imputado.csv"
  ),
  row.names = FALSE
)


############################################################
# 10. MDS CLÁSICO EN TRES DIMENSIONES
############################################################

cat("\n============================================================\n")
cat("PASO 8: APLICAR MDS CLÁSICO\n")
cat("============================================================\n")

matriz_distancias <- dist(
  X_escalada,
  method = "euclidean"
)

# Se calculan tres dimensiones de una sola vez.
# Las dos primeras se usarán para la representación 2D.
modelo_mds <- cmdscale(
  matriz_distancias,
  k = 3,
  eig = TRUE,
  add = TRUE
)

coordenadas_mds <- as.data.frame(
  modelo_mds$points
)

names(coordenadas_mds) <- c(
  "Dimension_1",
  "Dimension_2",
  "Dimension_3"
)

# Proporción orientativa explicada por los autovalores positivos.
autovalores_positivos <- modelo_mds$eig[
  modelo_mds$eig > 0
]

porcentaje_dimensiones <- 100 *
  autovalores_positivos /
  sum(autovalores_positivos)

tabla_autovalores <- data.frame(
  Dimension = paste0(
    "Dimension_",
    seq_along(autovalores_positivos)
  ),
  Autovalor = autovalores_positivos,
  Porcentaje = porcentaje_dimensiones,
  Porcentaje_acumulado = cumsum(
    porcentaje_dimensiones
  )
)

cat("\nPrimeros autovalores de MDS:\n")
print(
  head(
    tabla_autovalores,
    10
  )
)

write.csv(
  tabla_autovalores,
  file = file.path(
    CARPETA_RESULTADOS,
    "05_autovalores_mds.csv"
  ),
  row.names = FALSE
)

saveRDS(
  modelo_mds,
  file = file.path(
    CARPETA_RESULTADOS,
    "modelo_mds.rds"
  )
)


############################################################
# 11. K-MEANS SOBRE LOS DATOS LIMPIOS
############################################################

cat("\n============================================================\n")
cat("PASO 9: FORMAR CLÚSTERES CON K-MEANS\n")
cat("============================================================\n")

set.seed(SEMILLA)

modelo_kmeans <- kmeans(
  X_escalada,
  centers = NUMERO_CLUSTERS,
  nstart = 50,
  iter.max = 100
)

coordenadas_mds$Cluster <- factor(
  modelo_kmeans$cluster,
  levels = seq_len(NUMERO_CLUSTERS),
  labels = paste0(
    "Cluster_",
    seq_len(NUMERO_CLUSTERS)
  )
)

coordenadas_mds$id_fila_original <-
  metadatos$id_fila_original

coordenadas_mds$Month <-
  metadatos$Month

coordenadas_mds$Day <-
  metadatos$Day

cat("\nTamaño de cada clúster:\n")
print(
  table(
    coordenadas_mds$Cluster
  )
)

saveRDS(
  modelo_kmeans,
  file = file.path(
    CARPETA_RESULTADOS,
    "modelo_kmeans.rds"
  )
)


############################################################
# 12. EVALUACIÓN MEDIANTE SILUETA
############################################################

cat("\n============================================================\n")
cat("PASO 10: EVALUAR LOS CLÚSTERES\n")
cat("============================================================\n")

silueta <- cluster::silhouette(
  modelo_kmeans$cluster,
  matriz_distancias
)

silueta_media <- mean(
  silueta[
    ,
    "sil_width"
  ]
)

cat(
  "\nÍndice de silueta medio: ",
  round(silueta_media, 4),
  "\n",
  sep = ""
)

tabla_silueta <- as.data.frame(
  silueta
)

tabla_silueta$id_fila_original <-
  metadatos$id_fila_original

write.csv(
  tabla_silueta,
  file = file.path(
    CARPETA_RESULTADOS,
    "06_silueta_por_observacion.csv"
  ),
  row.names = FALSE
)

png(
  filename = file.path(
    CARPETA_RESULTADOS,
    "07_grafico_silueta.png"
  ),
  width = 1200,
  height = 850,
  res = 140
)

plot(
  silueta,
  main = paste0(
    "Silueta de K-means; media = ",
    round(silueta_media, 3)
  )
)

dev.off()


############################################################
# 13. REPRESENTACIÓN MDS EN DOS DIMENSIONES
############################################################

cat("\n============================================================\n")
cat("PASO 11: REPRESENTAR MDS EN 2D\n")
cat("============================================================\n")

grafico_mds_2d <- ggplot(
  coordenadas_mds,
  aes(
    x = Dimension_1,
    y = Dimension_2,
    colour = Cluster,
    shape = Cluster
  )
) +
  geom_point(
    size = 3,
    alpha = 0.85
  ) +
  labs(
    title = "MDS clásico en dos dimensiones",
    subtitle = paste0(
      "Datos limpiados, imputados y agrupados con K-means; k = ",
      NUMERO_CLUSTERS
    ),
    x = "Dimensión 1",
    y = "Dimensión 2",
    colour = "Clúster",
    shape = "Clúster"
  ) +
  theme_minimal(
    base_size = 12
  )

print(
  grafico_mds_2d
)

ggsave(
  filename = file.path(
    CARPETA_RESULTADOS,
    "08_mds_2d_clusters.png"
  ),
  plot = grafico_mds_2d,
  width = 9,
  height = 6,
  dpi = 160
)


############################################################
# 14. REPRESENTACIÓN MDS EN TRES DIMENSIONES
############################################################

cat("\n============================================================\n")
cat("PASO 12: REPRESENTAR MDS EN 3D\n")
cat("============================================================\n")

colores_clusters <- grDevices::rainbow(
  NUMERO_CLUSTERS
)

color_por_observacion <- colores_clusters[
  modelo_kmeans$cluster
]

png(
  filename = file.path(
    CARPETA_RESULTADOS,
    "09_mds_3d_clusters.png"
  ),
  width = 1300,
  height = 950,
  res = 140
)

scatterplot3d::scatterplot3d(
  x = coordenadas_mds$Dimension_1,
  y = coordenadas_mds$Dimension_2,
  z = coordenadas_mds$Dimension_3,
  color = color_por_observacion,
  pch = 19,
  angle = 55,
  xlab = "Dimensión 1",
  ylab = "Dimensión 2",
  zlab = "Dimensión 3",
  main = "MDS clásico en tres dimensiones",
  grid = TRUE,
  box = TRUE
)

legend(
  "topright",
  legend = paste0(
    "Cluster_",
    seq_len(NUMERO_CLUSTERS)
  ),
  col = colores_clusters,
  pch = 19,
  cex = 0.9
)

dev.off()


############################################################
# 15. COMPARACIÓN AUXILIAR CON EL MES
############################################################

cat("\n============================================================\n")
cat("PASO 13: INTERPRETAR LOS CLÚSTERES POR MES\n")
cat("============================================================\n")

tabla_cluster_mes <- table(
  Cluster = coordenadas_mds$Cluster,
  Mes = coordenadas_mds$Month
)

cat("\nTabla clúster frente a mes:\n")
print(
  tabla_cluster_mes
)

write.csv(
  as.data.frame.matrix(
    tabla_cluster_mes
  ),
  file = file.path(
    CARPETA_RESULTADOS,
    "10_cluster_vs_mes.csv"
  )
)

write.csv(
  coordenadas_mds,
  file = file.path(
    CARPETA_RESULTADOS,
    "11_coordenadas_mds_completas.csv"
  ),
  row.names = FALSE
)


############################################################
# 16. INTERPRETACIÓN AUTOMÁTICA
############################################################

cat("\n============================================================\n")
cat("PASO 14: INTERPRETACIÓN FINAL\n")
cat("============================================================\n")

interpretacion_silueta <- if (
  silueta_media >= 0.50
) {

  "los grupos presentan una separación razonablemente clara"

} else if (
  silueta_media >= 0.25
) {

  "los grupos muestran una separación débil o moderada"

} else {

  "los grupos están muy solapados y la estructura es poco definida"
}

porcentaje_2d <- if (
  length(porcentaje_dimensiones) >= 2
) {

  sum(
    porcentaje_dimensiones[1:2]
  )

} else {

  NA_real_
}

porcentaje_3d <- if (
  length(porcentaje_dimensiones) >= 3
) {

  sum(
    porcentaje_dimensiones[1:3]
  )

} else {

  NA_real_
}

texto_interpretacion <- c(
  "INTERPRETACIÓN DEL EJEMPLO NO SUPERVISADO",
  "=========================================",
  "",
  paste0(
    "El conjunto airquality contenía valores ausentes reales. ",
    "Primero se cuantificaron los NA, se eliminaron las filas y ",
    "columnas que superaban los umbrales establecidos y los NA ",
    "restantes se sustituyeron por la mediana de cada variable."
  ),
  paste0(
    "Después se estandarizaron las variables y se aplicó MDS ",
    "clásico para obtener representaciones en dos y tres dimensiones."
  ),
  paste0(
    "Las dos primeras dimensiones representan aproximadamente el ",
    round(porcentaje_2d, 2),
    "% de la información asociada a los autovalores positivos."
  ),
  paste0(
    "Las tres primeras dimensiones representan aproximadamente el ",
    round(porcentaje_3d, 2),
    "%."
  ),
  paste0(
    "K-means identificó ",
    NUMERO_CLUSTERS,
    " clústeres y el índice de silueta medio fue ",
    round(silueta_media, 3),
    "; por tanto, ",
    interpretacion_silueta,
    "."
  ),
  "",
  "FRASE MODELO PARA EL EXAMEN",
  "---------------------------",
  paste0(
    "Tras limpiar los datos, imputar los valores ausentes con la ",
    "mediana y estandarizar las variables, se aplicó MDS clásico. ",
    "La representación en 2D y 3D permitió visualizar ",
    NUMERO_CLUSTERS,
    " grupos obtenidos mediante K-means. La silueta media fue ",
    round(silueta_media, 3),
    ", lo que indica que ",
    interpretacion_silueta,
    "."
  )
)

cat(
  paste(
    texto_interpretacion,
    collapse = "\n"
  )
)

cat("\n")

writeLines(
  texto_interpretacion,
  con = file.path(
    CARPETA_RESULTADOS,
    "12_interpretacion_final.txt"
  )
)


############################################################
# 17. INFORMACIÓN DE LA SESIÓN Y MENSAJE FINAL
############################################################

writeLines(
  capture.output(
    sessionInfo()
  ),
  con = file.path(
    CARPETA_RESULTADOS,
    "13_sessionInfo.txt"
  )
)

cat("\n============================================================\n")
cat("EJEMPLO FINALIZADO CORRECTAMENTE\n")
cat("============================================================\n")

cat(
  "Resultados guardados en:\n",
  normalizePath(
    CARPETA_RESULTADOS,
    winslash = "/",
    mustWork = FALSE
  ),
  "\n",
  sep = ""
)
