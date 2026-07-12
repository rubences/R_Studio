############################################################
# 13D_EJEMPLO_DOS_SUPERVISADOS_LDA_ARBOL_CON_LIMPIEZA_NA.R
############################################################
#
# EJEMPLO NUEVO, COMPLETO Y EJECUTABLE
# ------------------------------------
# Objetivo:
#
#   1. Trabajar con un conjunto de clasificación que contiene NA.
#   2. Simular un escenario con muchos valores ausentes.
#   3. Eliminar filas y columnas con demasiados NA.
#   4. Dividir los datos en entrenamiento y prueba.
#   5. Calcular las medianas SOLO con el conjunto de entrenamiento.
#   6. Imputar train y test sin fuga de información.
#   7. Entrenar dos métodos supervisados:
#         - Análisis discriminante lineal: LDA.
#         - Árbol de decisión: CART/rpart.
#   8. Evaluar matriz de confusión, Accuracy, Kappa,
#      sensibilidad, especificidad, precisión, F1,
#      balanced accuracy, ROC y AUC.
#   9. Comparar ambos modelos y seleccionar el mejor.
#
# DATASET
# -------
# Se utiliza biopsy, incluido en el paquete MASS.
# El objetivo es clasificar tumores como:
#
#   malignant -> clase positiva.
#   benign    -> clase negativa.
#
# PRUEBA DE LIMPIEZA
# ------------------
# biopsy contiene NA reales. Además, este ejemplo introduce NA
# adicionales de forma reproducible para comprobar que el proceso
# de limpieza funciona incluso con un volumen elevado de ausentes.
############################################################


############################################################
# 0. LIMPIEZA DEL ENTORNO Y CONFIGURACIÓN
############################################################

rm(list = ls())

SEMILLA <- 1995

PROPORCION_TRAIN <- 0.80

NUMERO_FOLDS <- 5

REPETICIONES_CV <- 3

CLASE_POSITIVA <- "malignant"

# Columnas con más de un 40 % de NA en entrenamiento se eliminan.
UMBRAL_NA_COLUMNAS <- 0.40

# Filas con más de un 60 % de NA en predictores se eliminan.
UMBRAL_NA_FILAS <- 0.60

# Porcentaje de NA artificiales introducidos en cada predictor.
PORCENTAJE_NA_SIMULADO <- 0.12

CARPETA_RESULTADOS <-
  "resultados_ejemplo_13D_supervisado"

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
    "caret",
    "MASS",
    "rpart",
    "rpart.plot",
    "pROC"
  )
)

library(caret)
library(MASS)
library(rpart)
library(rpart.plot)
library(pROC)


############################################################
# 2. FUNCIONES AUXILIARES DE LIMPIEZA
############################################################

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


# Introduce NA artificiales únicamente en los predictores.
# Se usa para probar el procedimiento de limpieza.
introducir_na_controlados <- function(
  dataframe,
  columnas,
  porcentaje,
  semilla
) {

  set.seed(semilla)

  resultado <- dataframe

  cantidad_por_columna <- max(
    1,
    floor(
      porcentaje * nrow(resultado)
    )
  )

  for (nombre in columnas) {

    indices <- sample(
      seq_len(
        nrow(resultado)
      ),
      size = cantidad_por_columna,
      replace = FALSE
    )

    resultado[
      indices,
      nombre
    ] <- NA
  }

  resultado
}


# Imputa train y test con las medianas calculadas SOLO en train.
imputar_train_test <- function(
  train_predictores,
  test_predictores
) {

  medianas <- vapply(
    train_predictores,
    function(x) median(x, na.rm = TRUE),
    numeric(1)
  )

  if (any(!is.finite(medianas))) {

    stop(
      paste0(
        "ERROR: alguna columna de entrenamiento no posee ",
        "valores válidos para calcular su mediana."
      ),
      call. = FALSE
    )
  }

  train_imputado <- train_predictores
  test_imputado <- test_predictores

  for (nombre in names(train_imputado)) {

    train_imputado[[nombre]][
      is.na(train_imputado[[nombre]])
    ] <- medianas[[nombre]]

    test_imputado[[nombre]][
      is.na(test_imputado[[nombre]])
    ] <- medianas[[nombre]]
  }

  list(
    train = train_imputado,
    test = test_imputado,
    medianas = medianas
  )
}


############################################################
# 3. CARGA DEL DATASET BIOPSY
############################################################

cat("\n============================================================\n")
cat("PASO 1: CARGAR EL DATASET BIOPSY\n")
cat("============================================================\n")

data(
  "biopsy",
  package = "MASS"
)

datos <- biopsy

# El identificador se conserva como nombre de fila, pero no se usa
# como predictor porque no posee significado clínico predictivo.
rownames(datos) <- paste0(
  "ID_",
  datos$ID
)

datos$ID <- NULL

# La clase positiva se coloca como primer nivel para caret.
datos$class <- factor(
  datos$class,
  levels = c(
    "malignant",
    "benign"
  )
)

datos <- convertir_infinitos_en_na(
  datos
)

cat("\nDimensiones originales:\n")
print(
  dim(datos)
)

cat("\nEstructura original:\n")
str(datos)

cat("\nDistribución de clases:\n")
print(
  table(datos$class)
)

cat("\nNA reales antes de simular más ausentes:\n")
print(
  sum(is.na(datos))
)


############################################################
# 4. INTRODUCIR MUCHOS NA PARA PROBAR LA LIMPIEZA
############################################################

cat("\n============================================================\n")
cat("PASO 2: SIMULAR UN ESCENARIO CON MUCHOS NA\n")
cat("============================================================\n")

columnas_predictoras <- setdiff(
  names(datos),
  "class"
)

datos <- introducir_na_controlados(
  dataframe = datos,
  columnas = columnas_predictoras,
  porcentaje = PORCENTAJE_NA_SIMULADO,
  semilla = SEMILLA
)

# V9 se fuerza a superar el 40 % de NA para demostrar que una
# columna excesivamente incompleta debe eliminarse.
set.seed(
  SEMILLA + 1
)

indices_v9 <- sample(
  seq_len(
    nrow(datos)
  ),
  size = floor(
    0.55 * nrow(datos)
  ),
  replace = FALSE
)

datos$V9[
  indices_v9
] <- NA

resumen_na_inicial <- crear_resumen_na(
  datos
)

cat("\nResumen de NA después de la simulación:\n")
print(
  resumen_na_inicial
)

write.csv(
  resumen_na_inicial,
  file = file.path(
    CARPETA_RESULTADOS,
    "01_resumen_na_inicial.csv"
  ),
  row.names = FALSE
)


############################################################
# 5. ELIMINAR FILAS EXCESIVAMENTE INCOMPLETAS
############################################################

cat("\n============================================================\n")
cat("PASO 3: ELIMINAR FILAS CON DEMASIADOS NA\n")
cat("============================================================\n")

columnas_predictoras <- setdiff(
  names(datos),
  "class"
)

porcentaje_na_fila <- rowMeans(
  is.na(
    datos[
      ,
      columnas_predictoras,
      drop = FALSE
    ]
  )
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
    ID = rownames(datos)[
      filas_eliminar
    ],
    Porcentaje_NA = 100 *
      porcentaje_na_fila[
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

datos <- datos[
  !filas_eliminar,
  ,
  drop = FALSE
]

# La variable objetivo nunca debe contener NA.
datos <- datos[
  !is.na(datos$class),
  ,
  drop = FALSE
]

cat(
  "\nFilas restantes: ",
  nrow(datos),
  "\n",
  sep = ""
)

cat("\nDistribución de clases después de limpiar filas:\n")
print(
  table(datos$class)
)


############################################################
# 6. PARTICIÓN ESTRATIFICADA TRAIN / TEST
############################################################

cat("\n============================================================\n")
cat("PASO 4: DIVIDIR EN TRAIN Y TEST\n")
cat("============================================================\n")

set.seed(
  SEMILLA
)

indices_train <- caret::createDataPartition(
  y = datos$class,
  p = PROPORCION_TRAIN,
  list = FALSE
)

train_raw <- datos[
  indices_train,
  ,
  drop = FALSE
]

test_raw <- datos[
  -indices_train,
  ,
  drop = FALSE
]

train_raw$class <- factor(
  train_raw$class,
  levels = c(
    "malignant",
    "benign"
  )
)

test_raw$class <- factor(
  test_raw$class,
  levels = levels(
    train_raw$class
  )
)

cat(
  "\nMuestras de entrenamiento: ",
  nrow(train_raw),
  "\n",
  sep = ""
)

cat(
  "Muestras de prueba: ",
  nrow(test_raw),
  "\n",
  sep = ""
)

cat("\nClases en entrenamiento:\n")
print(
  table(train_raw$class)
)

cat("\nClases en prueba:\n")
print(
  table(test_raw$class)
)


############################################################
# 7. ELIMINAR COLUMNAS CON DEMASIADOS NA
############################################################

cat("\n============================================================\n")
cat("PASO 5: ELIMINAR COLUMNAS CON DEMASIADOS NA\n")
cat("============================================================\n")

predictores_train_raw <- train_raw[
  ,
  setdiff(
    names(train_raw),
    "class"
  ),
  drop = FALSE
]

predictores_test_raw <- test_raw[
  ,
  names(predictores_train_raw),
  drop = FALSE
]

# La decisión se toma usando solamente TRAIN.
porcentaje_na_train <- colMeans(
  is.na(
    predictores_train_raw
  )
)

columnas_eliminar <- names(
  porcentaje_na_train[
    porcentaje_na_train >
      UMBRAL_NA_COLUMNAS
  ]
)

cat("\nPorcentaje de NA en TRAIN:\n")
print(
  round(
    100 * porcentaje_na_train,
    2
  )
)

if (length(columnas_eliminar) > 0) {

  cat("\nColumnas eliminadas:\n")
  print(
    columnas_eliminar
  )

  predictores_train_raw <-
    predictores_train_raw[
      ,
      setdiff(
        names(predictores_train_raw),
        columnas_eliminar
      ),
      drop = FALSE
    ]

  predictores_test_raw <-
    predictores_test_raw[
      ,
      names(predictores_train_raw),
      drop = FALSE
    ]

} else {

  cat(
    "\nNo se elimina ninguna columna.\n"
  )
}

if (ncol(predictores_train_raw) == 0) {

  stop(
    "ERROR: no queda ningún predictor.",
    call. = FALSE
  )
}


############################################################
# 8. IMPUTAR TRAIN Y TEST SIN FUGA DE INFORMACIÓN
############################################################

cat("\n============================================================\n")
cat("PASO 6: IMPUTAR CON MEDIANAS DE TRAIN\n")
cat("============================================================\n")

resultado_imputacion <- imputar_train_test(
  train_predictores =
    predictores_train_raw,
  test_predictores =
    predictores_test_raw
)

train_predictores <-
  resultado_imputacion$train

test_predictores <-
  resultado_imputacion$test

medianas_train <-
  resultado_imputacion$medianas

tabla_medianas <- data.frame(
  Variable = names(
    medianas_train
  ),
  Mediana_train = as.numeric(
    medianas_train
  )
)

cat("\nMedianas calculadas en TRAIN:\n")
print(
  tabla_medianas
)

write.csv(
  tabla_medianas,
  file = file.path(
    CARPETA_RESULTADOS,
    "03_medianas_calculadas_en_train.csv"
  ),
  row.names = FALSE
)

if (
  anyNA(train_predictores) ||
    anyNA(test_predictores)
) {

  stop(
    "ERROR: todavía quedan valores NA tras imputar.",
    call. = FALSE
  )
}


############################################################
# 9. ELIMINAR VARIABLES DE VARIANZA CERO
############################################################

cat("\n============================================================\n")
cat("PASO 7: ELIMINAR VARIABLES SIN VARIABILIDAD\n")
cat("============================================================\n")

indices_nzv <- caret::nearZeroVar(
  train_predictores
)

if (length(indices_nzv) > 0) {

  variables_nzv <- names(
    train_predictores
  )[
    indices_nzv
  ]

  cat(
    "\nVariables eliminadas por varianza nula o casi nula:\n"
  )

  print(
    variables_nzv
  )

  train_predictores <-
    train_predictores[
      ,
      -indices_nzv,
      drop = FALSE
    ]

  test_predictores <-
    test_predictores[
      ,
      names(train_predictores),
      drop = FALSE
    ]

} else {

  cat(
    "\nNo se detectan variables de varianza casi nula.\n"
  )
}

train <- cbind(
  train_predictores,
  class = train_raw$class
)

test <- cbind(
  test_predictores,
  class = test_raw$class
)

train$class <- factor(
  train$class,
  levels = c(
    "malignant",
    "benign"
  )
)

test$class <- factor(
  test$class,
  levels = levels(
    train$class
  )
)

cat("\nNA finales en TRAIN:\n")
print(
  sum(is.na(train))
)

cat("\nNA finales en TEST:\n")
print(
  sum(is.na(test))
)

write.csv(
  train,
  file = file.path(
    CARPETA_RESULTADOS,
    "04_train_limpio.csv"
  ),
  row.names = TRUE
)

write.csv(
  test,
  file = file.path(
    CARPETA_RESULTADOS,
    "05_test_limpio.csv"
  ),
  row.names = TRUE
)


############################################################
# 10. CONTROL DE ENTRENAMIENTO
############################################################

cat("\n============================================================\n")
cat("PASO 8: CONFIGURAR LA VALIDACIÓN CRUZADA\n")
cat("============================================================\n")

control_entrenamiento <- caret::trainControl(
  method = "repeatedcv",
  number = NUMERO_FOLDS,
  repeats = REPETICIONES_CV,
  classProbs = TRUE,
  summaryFunction = caret::twoClassSummary,
  savePredictions = "final",
  allowParallel = FALSE
)

METRICA_A_OPTIMIZAR <- "ROC"


############################################################
# 11. MODELO 1: ANÁLISIS DISCRIMINANTE LINEAL
############################################################

cat("\n============================================================\n")
cat("PASO 9: ENTRENAR LDA\n")
cat("============================================================\n")

set.seed(
  SEMILLA
)

modelo_lda <- caret::train(
  class ~ .,
  data = train,
  method = "lda",
  metric = METRICA_A_OPTIMIZAR,
  trControl = control_entrenamiento,
  preProcess = c(
    "center",
    "scale"
  )
)

cat("\nResumen de LDA:\n")
print(
  modelo_lda
)

saveRDS(
  modelo_lda,
  file = file.path(
    CARPETA_RESULTADOS,
    "modelo_lda.rds"
  )
)

# Guardamos los coeficientes discriminantes.
if (
  !is.null(
    modelo_lda$finalModel$scaling
  )
) {

  write.csv(
    as.data.frame(
      modelo_lda$finalModel$scaling
    ),
    file = file.path(
      CARPETA_RESULTADOS,
      "06_coeficientes_lda.csv"
    ),
    row.names = TRUE
  )
}


############################################################
# 12. MODELO 2: ÁRBOL DE DECISIÓN
############################################################

cat("\n============================================================\n")
cat("PASO 10: ENTRENAR ÁRBOL DE DECISIÓN\n")
cat("============================================================\n")

set.seed(
  SEMILLA
)

modelo_arbol <- caret::train(
  class ~ .,
  data = train,
  method = "rpart",
  metric = METRICA_A_OPTIMIZAR,
  trControl = control_entrenamiento,
  tuneLength = 20
)

cat("\nResumen del árbol:\n")
print(
  modelo_arbol
)

cat("\nMejor valor de complejidad cp:\n")
print(
  modelo_arbol$bestTune
)

saveRDS(
  modelo_arbol,
  file = file.path(
    CARPETA_RESULTADOS,
    "modelo_arbol.rds"
  )
)

png(
  filename = file.path(
    CARPETA_RESULTADOS,
    "07_arbol_de_decision.png"
  ),
  width = 1500,
  height = 1000,
  res = 150
)

rpart.plot::rpart.plot(
  modelo_arbol$finalModel,
  type = 2,
  extra = 104,
  fallen.leaves = TRUE,
  main = "Árbol de decisión para clasificar biopsias"
)

dev.off()


############################################################
# 13. COMPARACIÓN INTERNA EN VALIDACIÓN CRUZADA
############################################################

cat("\n============================================================\n")
cat("PASO 11: COMPARAR VALIDACIÓN CRUZADA\n")
cat("============================================================\n")

comparacion_cv <- caret::resamples(
  list(
    LDA = modelo_lda,
    Arbol = modelo_arbol
  )
)

cat("\nResumen de validación cruzada:\n")
print(
  summary(
    comparacion_cv
  )
)

writeLines(
  capture.output(
    summary(
      comparacion_cv
    )
  ),
  con = file.path(
    CARPETA_RESULTADOS,
    "08_resumen_validacion_cruzada.txt"
  )
)

png(
  filename = file.path(
    CARPETA_RESULTADOS,
    "09_comparacion_auc_validacion_cruzada.png"
  ),
  width = 1200,
  height = 850,
  res = 140
)

print(
  caret::bwplot(
    comparacion_cv,
    metric = "ROC",
    main = "AUC en validación cruzada"
  )
)

dev.off()


############################################################
# 14. FUNCIÓN DE EVALUACIÓN SOBRE TEST
############################################################

evaluar_modelo <- function(
  modelo,
  nombre_modelo,
  datos_test
) {

  cat("\n------------------------------------------------------------\n")
  cat("EVALUACIÓN EN TEST: ", nombre_modelo, "\n", sep = "")
  cat("------------------------------------------------------------\n")

  clase_predicha <- predict(
    modelo,
    newdata = datos_test,
    type = "raw"
  )

  probabilidades <- predict(
    modelo,
    newdata = datos_test,
    type = "prob"
  )

  if (
    !CLASE_POSITIVA %in%
      names(probabilidades)
  ) {

    stop(
      paste0(
        "ERROR: no existe la probabilidad de la clase ",
        CLASE_POSITIVA,
        "."
      ),
      call. = FALSE
    )
  }

  probabilidad_positiva <-
    probabilidades[
      [
        CLASE_POSITIVA
      ]
    ]

  matriz_confusion <- caret::confusionMatrix(
    data = clase_predicha,
    reference = datos_test$class,
    positive = CLASE_POSITIVA
  )

  print(
    matriz_confusion
  )

  objeto_roc <- pROC::roc(
    response = datos_test$class,
    predictor = probabilidad_positiva,
    levels = c(
      "benign",
      "malignant"
    ),
    direction = "<",
    quiet = TRUE
  )

  valor_auc <- as.numeric(
    pROC::auc(
      objeto_roc
    )
  )

  extraer <- function(
    vector,
    nombre
  ) {

    if (
      is.null(vector) ||
        !nombre %in% names(vector)
    ) {

      return(
        NA_real_
      )
    }

    as.numeric(
      vector[
        [
          nombre
        ]
      ]
    )
  }

  tabla_metricas <- data.frame(
    Modelo = nombre_modelo,
    Accuracy = extraer(
      matriz_confusion$overall,
      "Accuracy"
    ),
    Kappa = extraer(
      matriz_confusion$overall,
      "Kappa"
    ),
    Sensibilidad = extraer(
      matriz_confusion$byClass,
      "Sensitivity"
    ),
    Especificidad = extraer(
      matriz_confusion$byClass,
      "Specificity"
    ),
    Precision = extraer(
      matriz_confusion$byClass,
      "Pos Pred Value"
    ),
    F1 = extraer(
      matriz_confusion$byClass,
      "F1"
    ),
    Balanced_Accuracy = extraer(
      matriz_confusion$byClass,
      "Balanced Accuracy"
    ),
    AUC = valor_auc,
    stringsAsFactors = FALSE
  )

  cat("\nResumen de métricas:\n")
  print(
    tabla_metricas
  )

  tabla_predicciones <- data.frame(
    ID = rownames(
      datos_test
    ),
    Clase_real = datos_test$class,
    Clase_predicha = clase_predicha,
    Probabilidad_malignant =
      probabilidad_positiva,
    Acierto =
      clase_predicha ==
      datos_test$class
  )

  write.csv(
    as.data.frame(
      matriz_confusion$table
    ),
    file = file.path(
      CARPETA_RESULTADOS,
      paste0(
        "matriz_confusion_",
        nombre_modelo,
        ".csv"
      )
    ),
    row.names = FALSE
  )

  write.csv(
    tabla_predicciones,
    file = file.path(
      CARPETA_RESULTADOS,
      paste0(
        "predicciones_",
        nombre_modelo,
        ".csv"
      )
    ),
    row.names = FALSE
  )

  writeLines(
    capture.output(
      matriz_confusion
    ),
    con = file.path(
      CARPETA_RESULTADOS,
      paste0(
        "evaluacion_completa_",
        nombre_modelo,
        ".txt"
      )
    )
  )

  list(
    nombre = nombre_modelo,
    clases = clase_predicha,
    probabilidades =
      probabilidad_positiva,
    matriz_confusion =
      matriz_confusion,
    roc = objeto_roc,
    metricas = tabla_metricas,
    predicciones =
      tabla_predicciones
  )
}


############################################################
# 15. EVALUACIÓN DE AMBOS MODELOS
############################################################

cat("\n============================================================\n")
cat("PASO 12: EVALUAR AMBOS MODELOS EN TEST\n")
cat("============================================================\n")

resultado_lda <- evaluar_modelo(
  modelo = modelo_lda,
  nombre_modelo = "LDA",
  datos_test = test
)

resultado_arbol <- evaluar_modelo(
  modelo = modelo_arbol,
  nombre_modelo = "Arbol",
  datos_test = test
)


############################################################
# 16. TABLA COMPARATIVA
############################################################

cat("\n============================================================\n")
cat("PASO 13: COMPARACIÓN FINAL\n")
cat("============================================================\n")

tabla_comparativa <- rbind(
  resultado_lda$metricas,
  resultado_arbol$metricas
)

tabla_comparativa <- tabla_comparativa[
  order(
    -tabla_comparativa$AUC,
    -tabla_comparativa$Accuracy
  ),
  ,
  drop = FALSE
]

rownames(
  tabla_comparativa
) <- NULL

cat("\nTabla comparativa ordenada:\n")
print(
  tabla_comparativa
)

write.csv(
  tabla_comparativa,
  file = file.path(
    CARPETA_RESULTADOS,
    "10_comparacion_metricas_test.csv"
  ),
  row.names = FALSE
)

mejor_modelo <-
  tabla_comparativa$Modelo[
    1
  ]

cat(
  "\nModelo seleccionado: ",
  mejor_modelo,
  "\n",
  sep = ""
)


############################################################
# 17. CURVAS ROC
############################################################

cat("\n============================================================\n")
cat("PASO 14: REPRESENTAR LAS CURVAS ROC\n")
cat("============================================================\n")

png(
  filename = file.path(
    CARPETA_RESULTADOS,
    "11_curvas_roc_comparadas.png"
  ),
  width = 1100,
  height = 850,
  res = 140
)

plot(
  resultado_lda$roc,
  lwd = 3,
  legacy.axes = TRUE,
  main = "Curvas ROC sobre el conjunto test"
)

plot(
  resultado_arbol$roc,
  add = TRUE,
  lwd = 3,
  lty = 2
)

abline(
  a = 0,
  b = 1,
  lty = 3
)

legend(
  "bottomright",
  legend = c(
    paste0(
      "LDA; AUC = ",
      round(
        resultado_lda$metricas$AUC,
        3
      )
    ),
    paste0(
      "Árbol; AUC = ",
      round(
        resultado_arbol$metricas$AUC,
        3
      )
    )
  ),
  lwd = 3,
  lty = c(
    1,
    2
  ),
  bty = "n"
)

dev.off()


############################################################
# 18. IMPORTANCIA DE VARIABLES
############################################################

cat("\n============================================================\n")
cat("PASO 15: IMPORTANCIA DE VARIABLES\n")
cat("============================================================\n")

guardar_importancia <- function(
  modelo,
  nombre_modelo
) {

  importancia <- tryCatch(
    caret::varImp(
      modelo,
      scale = TRUE
    ),
    error = function(e) NULL
  )

  if (is.null(importancia)) {

    warning(
      paste0(
        "No se pudo calcular la importancia para ",
        nombre_modelo,
        "."
      ),
      call. = FALSE
    )

    return(
      invisible(NULL)
    )
  }

  tabla_importancia <-
    importancia$importance

  tabla_importancia$Variable <-
    rownames(
      tabla_importancia
    )

  rownames(
    tabla_importancia
  ) <- NULL

  write.csv(
    tabla_importancia,
    file = file.path(
      CARPETA_RESULTADOS,
      paste0(
        "importancia_variables_",
        nombre_modelo,
        ".csv"
      )
    ),
    row.names = FALSE
  )

  png(
    filename = file.path(
      CARPETA_RESULTADOS,
      paste0(
        "importancia_variables_",
        nombre_modelo,
        ".png"
      )
    ),
    width = 1100,
    height = 800,
    res = 140
  )

  print(
    plot(
      importancia,
      top = 10,
      main = paste(
        "Importancia de variables -",
        nombre_modelo
      )
    )
  )

  dev.off()

  invisible(
    tabla_importancia
  )
}

guardar_importancia(
  modelo_lda,
  "LDA"
)

guardar_importancia(
  modelo_arbol,
  "Arbol"
)


############################################################
# 19. INTERPRETACIÓN AUTOMÁTICA
############################################################

cat("\n============================================================\n")
cat("PASO 16: INTERPRETACIÓN FINAL\n")
cat("============================================================\n")

fila_mejor <- tabla_comparativa[
  tabla_comparativa$Modelo ==
    mejor_modelo,
  ,
  drop = FALSE
]

texto_interpretacion <- c(
  "INTERPRETACIÓN DEL EJEMPLO SUPERVISADO",
  "======================================",
  "",
  paste0(
    "El conjunto biopsy contenía valores ausentes reales y se ",
    "introdujeron NA adicionales para simular un fichero muy ",
    "incompleto."
  ),
  paste0(
    "Se eliminaron las filas con más del ",
    100 * UMBRAL_NA_FILAS,
    "% de predictores ausentes y, utilizando únicamente el ",
    "conjunto de entrenamiento, se eliminaron las columnas con ",
    "más del ",
    100 * UMBRAL_NA_COLUMNAS,
    "% de NA."
  ),
  paste0(
    "Los valores restantes se imputaron con medianas calculadas ",
    "solo en entrenamiento y después se aplicaron tanto a train ",
    "como a test, evitando fuga de información."
  ),
  paste0(
    "Se compararon LDA y un árbol de decisión mediante ",
    NUMERO_FOLDS,
    " folds de validación cruzada repetidos ",
    REPETICIONES_CV,
    " veces."
  ),
  paste0(
    "El modelo seleccionado fue ",
    mejor_modelo,
    ", con AUC = ",
    round(
      fila_mejor$AUC,
      3
    ),
    ", Accuracy = ",
    round(
      fila_mejor$Accuracy,
      3
    ),
    ", Sensibilidad = ",
    round(
      fila_mejor$Sensibilidad,
      3
    ),
    ", Especificidad = ",
    round(
      fila_mejor$Especificidad,
      3
    ),
    " y F1 = ",
    round(
      fila_mejor$F1,
      3
    ),
    "."
  ),
  "",
  "FRASE MODELO PARA EL EXAMEN",
  "---------------------------",
  paste0(
    "Antes de entrenar los modelos se analizó el patrón de valores ",
    "ausentes. Las filas y columnas excesivamente incompletas se ",
    "eliminaron y los NA restantes se imputaron con medianas ",
    "calculadas exclusivamente sobre entrenamiento. Después se ",
    "compararon LDA y un árbol de decisión sobre un test ",
    "independiente. El mejor modelo fue ",
    mejor_modelo,
    " al presentar la mejor combinación de AUC y exactitud."
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
# 20. INFORMACIÓN DE LA SESIÓN Y MENSAJE FINAL
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
