############################################################
# 13_EXAMEN_PROBABLE_2D_3D_Y_DOS_SUPERVISADOS.R
############################################################
#
# OBJETIVO DE ESTA PLANTILLA
# --------------------------
# Esta plantilla está preparada para un enunciado parecido a:
#
#   1) Aplicar UN método no supervisado en 2 dimensiones.
#   2) Aplicar EL MISMO método no supervisado en 3 dimensiones.
#   3) Entrenar DOS métodos supervisados.
#   4) Evaluar, comparar e interpretar los resultados.
#
# Además, deja cubiertos varios apartados que suelen pedirse como
# complemento: preprocesamiento, partición train/test, validación
# cruzada, matriz de confusión, Accuracy, Sensitivity, Specificity,
# F1, ROC/AUC, importancia de variables y guardado de resultados.
#
# IMPORTANTE
# ----------
# - Ejecuta el archivo por bloques y en orden, de arriba abajo.
# - Modifica ÚNICAMENTE el bloque "CONFIGURACIÓN: SOLO EDITAR AQUÍ".
# - No uses la variable objetivo para ajustar el método no supervisado.
#   Solo puede utilizarse después para colorear y comprobar visualmente.
# - Todos los resultados se guardan automáticamente en resultados/.
############################################################

rm(list = ls())

# Cargamos las funciones auxiliares del paquete de plantillas.
source("00_UTILIDADES.R")

# Comprobamos que RStudio está situado en la carpeta del proyecto.
comprobar_directorio_proyecto()

############################################################
# CONFIGURACIÓN: SOLO EDITAR AQUÍ
############################################################
CONFIG <- list(

  # Nombre del CSV dentro de la carpeta data/.
  archivo = file.path("data", "dataset_expresiongenes_cancer.csv"),

  # Nombre EXACTO de la variable que se desea predecir.
  objetivo = "primaryormetastasis",

  # Columnas que no deben emplearse como predictores.
  # Añade aquí identificadores de paciente, muestra, fecha, etc.
  columnas_excluir = c("id", "ID", "sample", "patient"),

  # Máximo de predictores numéricos que se utilizarán.
  # Un valor moderado evita errores por demasiadas variables.
  # Escribe NULL para utilizar todas las variables numéricas válidas.
  max_predictores = 50,

  # Método no supervisado disponible:
  #   "PCA"  = opción más segura, rápida y fácil de explicar.
  #   "UMAP" = opción no lineal, útil si el profesor la solicita.
  metodo_no_supervisado = "PCA",

  # Número de clústeres que se utilizarán únicamente para colorear
  # las representaciones reducidas y calcular la silueta.
  numero_clusters = 2,

  # Los DOS métodos supervisados que se compararán.
  # Opciones habituales de caret:
  #   "knn", "svmLinear", "svmRadial", "svmPoly",
  #   "rpart", "rf", "gbm", "nb", "lda", "qda", "rda".
  modelos_supervisados = c("knn", "svmRadial"),

  # Clase positiva para sensibilidad, especificidad, F1 y ROC.
  # Debe escribirse exactamente como aparece en el CSV.
  # En un problema binario, NULL selecciona automáticamente el
  # primer nivel e informa claramente de la elección.
  # En un problema multiclase debe permanecer en NULL.
  clase_positiva = NULL,

  # Proporción reservada para entrenamiento.
  proporcion_train = 0.80,

  # Número máximo de particiones de validación cruzada.
  folds = 5,

  # Número de combinaciones de hiperparámetros que caret probará.
  tune_length = 8,

  # Semilla para poder reproducir exactamente los resultados.
  semilla = 1995,

  # TRUE permite usar la clase real únicamente para colorear una
  # figura posterior. No interviene en PCA, UMAP ni K-means.
  colorear_tambien_por_clase_real = TRUE
)
############################################################
# FIN DE CONFIGURACIÓN
############################################################

############################################################
# 0. COMPROBACIONES PREVIAS
############################################################

mensaje_paso(0, "Comprobar la configuración y los paquetes")

# El enunciado pide exactamente dos métodos supervisados.
if (length(CONFIG$modelos_supervisados) != 2) {
  error_claro(
    "En CONFIG$modelos_supervisados debes escribir exactamente DOS métodos."
  )
}

# Evitamos que se repita el mismo algoritmo dos veces.
if (length(unique(CONFIG$modelos_supervisados)) != 2) {
  error_claro("Los dos métodos supervisados deben ser diferentes.")
}

# Validamos el nombre del método no supervisado.
CONFIG$metodo_no_supervisado <- toupper(CONFIG$metodo_no_supervisado)
if (!CONFIG$metodo_no_supervisado %in% c("PCA", "UMAP")) {
  error_claro("metodo_no_supervisado solo puede ser 'PCA' o 'UMAP'.")
}

# Paquetes siempre necesarios.
paquetes_necesarios <- c(
  "caret", "ggplot2", "cluster", "pROC", "scatterplot3d"
)

# UMAP solo es necesario cuando se selecciona ese método.
if (CONFIG$metodo_no_supervisado == "UMAP") {
  paquetes_necesarios <- c(paquetes_necesarios, "uwot")
}

# Paquetes adicionales requeridos por cada método de caret.
paquetes_por_modelo <- list(
  knn       = character(0),
  svmLinear = "kernlab",
  svmRadial = "kernlab",
  svmPoly   = "kernlab",
  rpart     = "rpart",
  rf        = "randomForest",
  gbm       = "gbm",
  nb        = "klaR",
  lda       = "MASS",
  qda       = "MASS",
  rda       = "klaR"
)

for (metodo in CONFIG$modelos_supervisados) {
  if (!metodo %in% names(paquetes_por_modelo)) {
    error_claro(
      paste0(
        "El método '", metodo, "' no está contemplado en esta plantilla. ",
        "Usa una de estas opciones: ",
        paste(names(paquetes_por_modelo), collapse = ", "), "."
      )
    )
  }
  paquetes_necesarios <- c(
    paquetes_necesarios,
    paquetes_por_modelo[[metodo]]
  )
}

comprobar_paquetes(unique(paquetes_necesarios))
mensaje_ok("Configuración válida y paquetes disponibles.")

############################################################
# 1. CARGA E INSPECCIÓN DE LOS DATOS
############################################################

mensaje_paso(1, "Cargar e inspeccionar el conjunto de datos")

# La función abrirá un selector de archivos si no encuentra el CSV.
datos_originales <- leer_csv_seguro(CONFIG$archivo)

# Mostramos dimensiones, tipos de columnas, duplicados y valores NA.
inspeccionar_datos(datos_originales, "datos_originales")

# Comprobamos que exista la variable objetivo.
validar_columna(datos_originales, CONFIG$objetivo, "datos_originales")

cat("\nDistribución original de la variable objetivo:\n")
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
  guardar_tabla(tabla_varianza, "13_pca_varianza_explicada.csv")

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
  "13_no_supervisado_coordenadas_2d_3d.csv"
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
  "13_no_supervisado_2d_clusters.png",
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
    "13_no_supervisado_2d_clase_real.png",
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
  filename = file.path("resultados", "13_no_supervisado_3d_clusters.png"),
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
mensaje_ok("Gráfico 3D guardado en resultados/13_no_supervisado_3d_clusters.png")

# Figura 3D adicional coloreada por clase real.
if (isTRUE(CONFIG$colorear_tambien_por_clase_real)) {
  niveles_clase <- levels(resultado_no_supervisado$Clase_real)
  paleta_clases <- grDevices::rainbow(length(niveles_clase))
  colores_clases <- paleta_clases[
    as.integer(resultado_no_supervisado$Clase_real)
  ]

  png(
    filename = file.path("resultados", "13_no_supervisado_3d_clase_real.png"),
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
# 3. PREPARACIÓN DEL PROBLEMA SUPERVISADO
############################################################

mensaje_paso(3, "Preparar los datos para clasificación supervisada")

# Esta función conserva la variable objetivo y los predictores numéricos,
# imputa valores ausentes y elimina variables constantes.
datos_supervisados <- preparar_datos_supervisados(
  df = datos_originales,
  objetivo = CONFIG$objetivo,
  columnas_excluir = CONFIG$columnas_excluir,
  max_predictores = CONFIG$max_predictores,
  clase_positiva = CONFIG$clase_positiva
)

# Determinamos explícitamente la clase positiva que se utilizará.
# caret::twoClassSummary considera el primer nivel como evento positivo.
clase_positiva_utilizada <- NULL

if (nlevels(datos_supervisados[[CONFIG$objetivo]]) == 2) {
  niveles_actuales <- levels(datos_supervisados[[CONFIG$objetivo]])

  if (is.null(CONFIG$clase_positiva)) {
    clase_positiva_utilizada <- niveles_actuales[1]
    mensaje_aviso(
      paste0(
        "No se indicó clase positiva. Se utilizará automáticamente '",
        clase_positiva_utilizada, "'."
      )
    )
  } else {
    clase_positiva_utilizada <- make.names(CONFIG$clase_positiva)
  }

  datos_supervisados[[CONFIG$objetivo]] <- factor(
    datos_supervisados[[CONFIG$objetivo]],
    levels = c(
      clase_positiva_utilizada,
      setdiff(niveles_actuales, clase_positiva_utilizada)
    )
  )
}

cat("\nClases que utilizará la clasificación:\n")
print(table(datos_supervisados[[CONFIG$objetivo]]))
cat("Niveles, en orden:",
    paste(levels(datos_supervisados[[CONFIG$objetivo]]), collapse = ", "),
    "\n")
if (!is.null(clase_positiva_utilizada)) {
  cat("Clase positiva utilizada:", clase_positiva_utilizada, "\n")
}

# ------------------------------------------------------------------
# 3.1. Partición estratificada train/test
# ------------------------------------------------------------------

mensaje_paso(3.1, "Dividir los datos en entrenamiento y prueba")

particion <- dividir_train_test(
  df = datos_supervisados,
  objetivo = CONFIG$objetivo,
  proporcion_train = CONFIG$proporcion_train,
  semilla = CONFIG$semilla
)

train <- particion$train
test <- particion$test

cat("\nDistribución en TRAIN:\n")
print(table(train[[CONFIG$objetivo]]))
cat("\nDistribución en TEST:\n")
print(table(test[[CONFIG$objetivo]]))

# ------------------------------------------------------------------
# 3.2. Escalado sin fuga de información
# ------------------------------------------------------------------

mensaje_paso(3.2, "Estandarizar usando solo el conjunto de entrenamiento")

# Las medias y desviaciones se calculan exclusivamente con train.
# Después, los mismos parámetros se aplican a test.
escalado <- escalar_train_test(
  train = train,
  test = test,
  objetivo = CONFIG$objetivo
)

train_escalado <- escalado$train
test_escalado <- escalado$test

mensaje_ok("Train y test se han estandarizado sin fuga de información.")

############################################################
# 4. ENTRENAMIENTO DE LOS DOS MÉTODOS SUPERVISADOS
############################################################

mensaje_paso(4, "Configurar validación cruzada")

# caret adapta automáticamente el número de folds al tamaño de la
# clase menos frecuente para evitar particiones imposibles.
control_caret <- crear_control_caret(
  y = train_escalado[[CONFIG$objetivo]],
  max_folds = CONFIG$folds,
  repeticiones = 1,
  guardar_predicciones = TRUE
)

# En clasificación binaria se optimiza ROC; en multiclase, Accuracy.
es_binario <- nlevels(train_escalado[[CONFIG$objetivo]]) == 2
metrica_caret <- if (es_binario) "ROC" else "Accuracy"

formula_modelo <- stats::as.formula(
  paste(CONFIG$objetivo, "~ .")
)

resultados_modelos <- list()

# Función auxiliar para extraer una métrica sin romper el script si
# caret no la ofrece para un problema determinado.
extraer_metrica <- function(vector_o_matriz, nombre) {
  if (is.null(vector_o_matriz)) return(NA_real_)
  if (is.matrix(vector_o_matriz)) return(NA_real_)
  if (!nombre %in% names(vector_o_matriz)) return(NA_real_)
  as.numeric(vector_o_matriz[[nombre]])
}

for (metodo in CONFIG$modelos_supervisados) {

  cat("\n============================================================\n")
  cat("ENTRENANDO MÉTODO SUPERVISADO:", metodo, "\n")
  cat("============================================================\n")

  argumentos_extra <- list()

  # Algunos métodos necesitan argumentos específicos.
  if (metodo == "rf") argumentos_extra$ntree <- 500
  if (metodo == "gbm") argumentos_extra$verbose <- FALSE

  set.seed(CONFIG$semilla)

  modelo <- tryCatch(
    do.call(
      caret::train,
      c(
        list(
          form = formula_modelo,
          data = train_escalado,
          method = metodo,
          trControl = control_caret,
          metric = metrica_caret,
          tuneLength = CONFIG$tune_length
        ),
        argumentos_extra
      )
    ),
    error = function(e) {
      error_claro(
        paste0(
          "El modelo '", metodo, "' no pudo entrenarse. Detalle: ",
          conditionMessage(e)
        )
      )
    }
  )

  print(modelo)
  cat("\nMejores hiperparámetros:\n")
  print(modelo$bestTune)

  # Predicción de clases sobre test.
  prediccion_clase <- predict(
    modelo,
    newdata = test_escalado,
    type = "raw"
  )

  # Predicción de probabilidades. Puede no estar disponible en algún
  # método, por lo que se protege con tryCatch().
  prediccion_probabilidad <- tryCatch(
    predict(
      modelo,
      newdata = test_escalado,
      type = "prob"
    ),
    error = function(e) NULL
  )

  # Matriz de confusión y métricas.
  matriz_confusion <- evaluar_clasificacion(
    real = test_escalado[[CONFIG$objetivo]],
    predicho = prediccion_clase,
    clase_positiva = clase_positiva_utilizada
  )

  # AUC solo puede calcularse en clasificación binaria con probabilidad.
  roc_objeto <- NULL
  auc_valor <- NA_real_

  if (
    es_binario &&
    !is.null(clase_positiva_utilizada) &&
    !is.null(prediccion_probabilidad)
  ) {
    columna_positiva <- clase_positiva_utilizada

    if (columna_positiva %in% names(prediccion_probabilidad)) {
      roc_objeto <- calcular_roc_binaria(
        real = test_escalado[[CONFIG$objetivo]],
        probabilidad_positiva = prediccion_probabilidad[[columna_positiva]],
        clase_positiva = clase_positiva_utilizada
      )
      auc_valor <- as.numeric(pROC::auc(roc_objeto))
    } else {
      mensaje_aviso(
        paste0(
          "No se encontró la columna de probabilidad de la clase positiva: ",
          columna_positiva
        )
      )
    }
  }

  # Tabla de predicciones del método actual.
  tabla_predicciones <- data.frame(
    real = test_escalado[[CONFIG$objetivo]],
    predicho = prediccion_clase
  )

  if (!is.null(prediccion_probabilidad)) {
    tabla_predicciones <- cbind(
      tabla_predicciones,
      prediccion_probabilidad
    )
  }

  guardar_tabla(
    tabla_predicciones,
    paste0("13_", metodo, "_predicciones_test.csv")
  )

  # Guardamos la matriz de confusión como tabla.
  tabla_cm <- as.data.frame(matriz_confusion$table)
  guardar_tabla(
    tabla_cm,
    paste0("13_", metodo, "_matriz_confusion.csv")
  )

  # Guardamos el gráfico de ajuste de hiperparámetros.
  ejecutar_seguro(
    paste0("Gráfico de ajuste de ", metodo),
    {
      png(
        filename = file.path(
          "resultados",
          paste0("13_", metodo, "_ajuste_hiperparametros.png")
        ),
        width = 1100,
        height = 850,
        res = 150
      )
      plot(modelo)
      dev.off()
    }
  )

  # Importancia de variables cuando el método la permite.
  importancia <- tryCatch(
    caret::varImp(modelo, scale = TRUE),
    error = function(e) NULL
  )

  if (!is.null(importancia)) {
    tabla_importancia <- importancia$importance
    tabla_importancia$Variable <- rownames(tabla_importancia)
    rownames(tabla_importancia) <- NULL

    guardar_tabla(
      tabla_importancia,
      paste0("13_", metodo, "_importancia_variables.csv")
    )

    ejecutar_seguro(
      paste0("Importancia de variables de ", metodo),
      {
        png(
          filename = file.path(
            "resultados",
            paste0("13_", metodo, "_importancia_variables.png")
          ),
          width = 1100,
          height = 850,
          res = 150
        )
        plot(importancia, top = min(20, nrow(tabla_importancia)))
        dev.off()
      }
    )
  }

  # Guardamos el modelo para poder abrirlo sin volver a entrenar.
  saveRDS(
    modelo,
    file = file.path("resultados", paste0("13_", metodo, "_modelo.rds"))
  )

  # Almacenamos todo para la comparación final.
  resultados_modelos[[metodo]] <- list(
    modelo = modelo,
    prediccion_clase = prediccion_clase,
    prediccion_probabilidad = prediccion_probabilidad,
    matriz_confusion = matriz_confusion,
    roc = roc_objeto,
    auc = auc_valor
  )
}

############################################################
# 5. COMPARACIÓN FINAL DE LOS DOS MÉTODOS
############################################################

mensaje_paso(5, "Comparar los dos métodos supervisados")

tabla_comparacion <- do.call(
  rbind,
  lapply(names(resultados_modelos), function(nombre_metodo) {

    cm <- resultados_modelos[[nombre_metodo]]$matriz_confusion

    data.frame(
      Metodo = nombre_metodo,
      Accuracy = as.numeric(cm$overall["Accuracy"]),
      Kappa = as.numeric(cm$overall["Kappa"]),
      Sensitivity = if (es_binario) {
        extraer_metrica(cm$byClass, "Sensitivity")
      } else {
        NA_real_
      },
      Specificity = if (es_binario) {
        extraer_metrica(cm$byClass, "Specificity")
      } else {
        NA_real_
      },
      Precision = if (es_binario) {
        extraer_metrica(cm$byClass, "Pos Pred Value")
      } else {
        NA_real_
      },
      F1 = if (es_binario) {
        extraer_metrica(cm$byClass, "F1")
      } else {
        NA_real_
      },
      AUC = resultados_modelos[[nombre_metodo]]$auc,
      stringsAsFactors = FALSE
    )
  })
)

# Ordenamos por AUC si está disponible; de lo contrario, por Accuracy.
if (es_binario && any(is.finite(tabla_comparacion$AUC))) {
  tabla_comparacion <- tabla_comparacion[
    order(tabla_comparacion$AUC, decreasing = TRUE, na.last = TRUE),
  ]
  criterio_seleccion <- "AUC"
} else {
  tabla_comparacion <- tabla_comparacion[
    order(tabla_comparacion$Accuracy, decreasing = TRUE),
  ]
  criterio_seleccion <- "Accuracy"
}

rownames(tabla_comparacion) <- NULL
print(tabla_comparacion)

guardar_tabla(
  tabla_comparacion,
  "13_comparacion_dos_modelos_supervisados.csv"
)

mejor_modelo <- tabla_comparacion$Metodo[1]
cat("\nMejor método según", criterio_seleccion, ":", mejor_modelo, "\n")

# ------------------------------------------------------------------
# 5.1. Curvas ROC conjuntas
# ------------------------------------------------------------------

if (
  es_binario &&
  !is.null(clase_positiva_utilizada) &&
  all(vapply(resultados_modelos, function(x) !is.null(x$roc), logical(1)))
) {

  mensaje_paso(5.1, "Crear la comparación conjunta de curvas ROC")

  nombres_modelos <- names(resultados_modelos)
  colores_roc <- seq_along(nombres_modelos)

  png(
    filename = file.path("resultados", "13_curvas_roc_dos_modelos.png"),
    width = 1100,
    height = 850,
    res = 150
  )

  plot(
    resultados_modelos[[nombres_modelos[1]]]$roc,
    col = colores_roc[1],
    lwd = 3,
    legacy.axes = TRUE,
    main = "Comparación ROC de los dos métodos supervisados"
  )

  if (length(nombres_modelos) > 1) {
    for (i in 2:length(nombres_modelos)) {
      lines(
        resultados_modelos[[nombres_modelos[i]]]$roc,
        col = colores_roc[i],
        lwd = 3
      )
    }
  }

  leyenda_roc <- vapply(
    nombres_modelos,
    function(nombre) {
      paste0(
        nombre,
        " (AUC = ",
        round(resultados_modelos[[nombre]]$auc, 3),
        ")"
      )
    },
    character(1)
  )

  legend(
    "bottomright",
    legend = leyenda_roc,
    col = colores_roc,
    lwd = 3,
    bty = "n"
  )

  abline(a = 0, b = 1, lty = 2)
  dev.off()

  mensaje_ok("Curvas ROC guardadas correctamente.")
}

############################################################
# 6. INFORME AUTOMÁTICO PARA COPIAR EN LA RESPUESTA
############################################################

mensaje_paso(6, "Generar una interpretación automática")

lineas_informe <- c(
  "============================================================",
  "BORRADOR DE INTERPRETACIÓN PARA EL EXAMEN",
  "============================================================",
  "",
  "1. PREPROCESAMIENTO",
  paste0(
    "Se conservaron predictores numéricos, se imputaron los valores ",
    "ausentes mediante la mediana, se eliminaron variables constantes ",
    "y se estandarizaron las variables."
  ),
  "",
  "2. MÉTODO NO SUPERVISADO",
  paste0(
    "Se aplicó ", CONFIG$metodo_no_supervisado,
    " sin utilizar la variable objetivo."
  ),
  paste0(
    "La representación en 2D obtuvo una silueta media de ",
    round(silueta_media_2d, 3),
    ", mientras que la representación en 3D obtuvo ",
    round(silueta_media_3d, 3), "."
  ),
  paste0(
    "La comparación con las clases reales se realizó únicamente después ",
    "del ajuste, con finalidad descriptiva."
  ),
  "",
  "3. MÉTODOS SUPERVISADOS",
  paste0(
    "Se compararon ",
    paste(CONFIG$modelos_supervisados, collapse = " y "),
    " mediante una partición estratificada train/test y validación cruzada."
  ),
  paste0(
    "El escalado se calculó exclusivamente con el conjunto de entrenamiento ",
    "para evitar fuga de información."
  ),
  "",
  "4. RESULTADO",
  paste0(
    "El método seleccionado fue ", mejor_modelo,
    " porque obtuvo el mejor valor de ", criterio_seleccion, "."
  ),
  "",
  "5. ADVERTENCIA DE INTERPRETACIÓN",
  paste0(
    "No debe afirmarse que el método es definitivamente superior sin ",
    "considerar el tamaño muestral, el balance de clases, la variabilidad ",
    "de la validación cruzada y el coste clínico de falsos positivos y ",
    "falsos negativos."
  ),
  "",
  "Consulta también:",
  "- 13_comparacion_dos_modelos_supervisados.csv",
  "- las matrices de confusión de cada modelo,",
  "- los gráficos 2D y 3D,",
  "- la curva ROC conjunta si el problema es binario."
)

cat(paste(lineas_informe, collapse = "\n"), "\n")

writeLines(
  lineas_informe,
  con = file.path("resultados", "13_INTERPRETACION_AUTOMATICA.txt")
)

############################################################
# 7. RESUMEN DE ARCHIVOS GENERADOS
############################################################

cat("\n============================================================\n")
cat("ANÁLISIS COMPLETADO SIN ERRORES\n")
cat("============================================================\n")
cat("Revisa la carpeta resultados/. Encontrarás:\n")
cat("1) Coordenadas y clústeres del método no supervisado.\n")
cat("2) Gráficos en 2D y 3D.\n")
cat("3) Predicciones y matrices de confusión de los dos modelos.\n")
cat("4) Comparación de métricas.\n")
cat("5) Curvas ROC, cuando proceda.\n")
cat("6) Importancia de variables, cuando el método la permita.\n")
cat("7) Un borrador de interpretación listo para adaptar.\n")
cat("============================================================\n")
