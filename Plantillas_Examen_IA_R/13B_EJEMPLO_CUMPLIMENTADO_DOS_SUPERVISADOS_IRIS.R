############################################################
# 13B_EJEMPLO_CUMPLIMENTADO_DOS_SUPERVISADOS_IRIS.R
############################################################
#
# EJEMPLO COMPLETO Y EJECUTABLE
# -----------------------------
# Este archivo resuelve un ejercicio típico de examen:
#
#   1. Preparar un problema de clasificación binaria.
#   2. Dividir los datos en entrenamiento y prueba.
#   3. Entrenar dos métodos supervisados:
#         - k vecinos más cercanos: k-NN.
#         - máquina de vectores soporte: SVM radial.
#   4. Ajustar hiperparámetros mediante validación cruzada.
#   5. Evaluar ambos modelos sobre datos no vistos.
#   6. Calcular matriz de confusión, accuracy, sensibilidad,
#      especificidad, precisión, F1, balanced accuracy y AUC.
#   7. Dibujar y comparar las curvas ROC.
#   8. Elegir el mejor modelo y justificar la decisión.
#
# DATASET UTILIZADO
# -----------------
# Se utiliza iris, incluido de serie en R.
#
# PROBLEMA BINARIO CREADO
# -----------------------
# La variable objetivo será:
#
#   Virginica     -> la muestra pertenece a iris virginica.
#   No_virginica  -> la muestra pertenece a setosa o versicolor.
#
# CLASE POSITIVA
# --------------
# Virginica.
############################################################


############################################################
# 0. LIMPIEZA DEL ENTORNO Y CONFIGURACIÓN
############################################################

rm(list = ls())

# Configuración ya cumplimentada.
SEMILLA <- 1995
PROPORCION_TRAIN <- 0.80
NUMERO_FOLDS <- 5
REPETICIONES_CV <- 3
CLASE_POSITIVA <- "Virginica"
CARPETA_RESULTADOS <- "resultados_ejemplo_13B"

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
        ".\nInstálalos manualmente antes de ejecutar el ejemplo:\n",
        "install.packages(c(",
        paste(sprintf('"%s"', siguen_faltando), collapse = ", "),
        "))"
      ),
      call. = FALSE
    )
  }
}

# caret entrena y evalúa los modelos.
# kernlab contiene la implementación de svmRadial.
# pROC calcula las curvas ROC y el AUC.
asegurar_paquetes(
  c("caret", "kernlab", "pROC")
)

library(caret)
library(kernlab)
library(pROC)


############################################################
# 2. CREACIÓN DEL PROBLEMA DE CLASIFICACIÓN
############################################################

cat("\n============================================================\n")
cat("PASO 1: PREPARAR EL CONJUNTO DE DATOS\n")
cat("============================================================\n")

datos <- iris

# Creamos la variable objetivo binaria.
datos$objetivo <- ifelse(
  datos$Species == "virginica",
  "Virginica",
  "No_virginica"
)

# Colocamos la clase positiva en primer lugar.
# caret::twoClassSummary toma el primer nivel como evento positivo.
datos$objetivo <- factor(
  datos$objetivo,
  levels = c("Virginica", "No_virginica")
)

# Eliminamos Species porque contiene directamente la información con
# la que se construyó la variable objetivo y produciría fuga de datos.
datos$Species <- NULL

cat("\nDimensiones del dataset preparado:\n")
print(dim(datos))

cat("\nEstructura del dataset:\n")
str(datos)

cat("\nPrimeras filas:\n")
print(head(datos))

cat("\nDistribución de la variable objetivo:\n")
print(table(datos$objetivo))

cat("\nProporciones de las clases:\n")
print(round(prop.table(table(datos$objetivo)), 4))

if (anyNA(datos)) {
  stop(
    "ERROR: el conjunto contiene valores ausentes.",
    call. = FALSE
  )
}


############################################################
# 3. PARTICIÓN ESTRATIFICADA TRAIN / TEST
############################################################

cat("\n============================================================\n")
cat("PASO 2: DIVIDIR EN ENTRENAMIENTO Y PRUEBA\n")
cat("============================================================\n")

set.seed(SEMILLA)

indices_train <- caret::createDataPartition(
  y = datos$objetivo,
  p = PROPORCION_TRAIN,
  list = FALSE
)

train <- datos[indices_train, , drop = FALSE]
test <- datos[-indices_train, , drop = FALSE]

# Conservamos exactamente los mismos niveles en ambos conjuntos.
train$objetivo <- factor(
  train$objetivo,
  levels = c("Virginica", "No_virginica")
)

test$objetivo <- factor(
  test$objetivo,
  levels = levels(train$objetivo)
)

cat("\nNúmero de muestras en TRAIN:", nrow(train), "\n")
cat("Número de muestras en TEST:", nrow(test), "\n")

cat("\nDistribución de clases en TRAIN:\n")
print(table(train$objetivo))

cat("\nDistribución de clases en TEST:\n")
print(table(test$objetivo))

# Comprobación defensiva: ambas clases deben aparecer en train y test.
if (any(table(train$objetivo) == 0)) {
  stop(
    "ERROR: alguna clase no aparece en entrenamiento.",
    call. = FALSE
  )
}

if (any(table(test$objetivo) == 0)) {
  stop(
    "ERROR: alguna clase no aparece en prueba.",
    call. = FALSE
  )
}

write.csv(
  train,
  file = file.path(
    CARPETA_RESULTADOS,
    "01_datos_entrenamiento.csv"
  ),
  row.names = FALSE
)

write.csv(
  test,
  file = file.path(
    CARPETA_RESULTADOS,
    "02_datos_prueba.csv"
  ),
  row.names = FALSE
)


############################################################
# 4. CONFIGURACIÓN DE LA VALIDACIÓN CRUZADA
############################################################

cat("\n============================================================\n")
cat("PASO 3: CONFIGURAR VALIDACIÓN CRUZADA\n")
cat("============================================================\n")

# repeatedcv realiza validación cruzada repetida.
# classProbs = TRUE solicita probabilidades.
# twoClassSummary calcula ROC, sensibilidad y especificidad.
# savePredictions conserva las predicciones internas del mejor ajuste.
control_entrenamiento <- caret::trainControl(
  method = "repeatedcv",
  number = NUMERO_FOLDS,
  repeats = REPETICIONES_CV,
  classProbs = TRUE,
  summaryFunction = caret::twoClassSummary,
  savePredictions = "final",
  allowParallel = FALSE
)

# ROC será la métrica utilizada para elegir los hiperparámetros.
METRICA_A_OPTIMIZAR <- "ROC"


############################################################
# 5. ENTRENAMIENTO DEL PRIMER MODELO: k-NN
############################################################

cat("\n============================================================\n")
cat("PASO 4: ENTRENAR MODELO 1 - k-NN\n")
cat("============================================================\n")

set.seed(SEMILLA)

modelo_knn <- caret::train(
  objetivo ~ .,
  data = train,
  method = "knn",
  metric = METRICA_A_OPTIMIZAR,
  trControl = control_entrenamiento,

  # El centrado y el escalado se estiman únicamente dentro
  # del proceso de entrenamiento, evitando fuga de información.
  preProcess = c("center", "scale"),

  # Se prueban varios valores de k.
  tuneLength = 15
)

cat("\nResumen del modelo k-NN:\n")
print(modelo_knn)

cat("\nMejor hiperparámetro de k-NN:\n")
print(modelo_knn$bestTune)

saveRDS(
  modelo_knn,
  file = file.path(
    CARPETA_RESULTADOS,
    "modelo_knn.rds"
  )
)


############################################################
# 6. ENTRENAMIENTO DEL SEGUNDO MODELO: SVM RADIAL
############################################################

cat("\n============================================================\n")
cat("PASO 5: ENTRENAR MODELO 2 - SVM RADIAL\n")
cat("============================================================\n")

set.seed(SEMILLA)

modelo_svm <- caret::train(
  objetivo ~ .,
  data = train,
  method = "svmRadial",
  metric = METRICA_A_OPTIMIZAR,
  trControl = control_entrenamiento,

  # SVM también depende de la escala de los predictores.
  preProcess = c("center", "scale"),

  # caret probará combinaciones de sigma y C.
  tuneLength = 12
)

cat("\nResumen del modelo SVM radial:\n")
print(modelo_svm)

cat("\nMejores hiperparámetros de SVM radial:\n")
print(modelo_svm$bestTune)

saveRDS(
  modelo_svm,
  file = file.path(
    CARPETA_RESULTADOS,
    "modelo_svm_radial.rds"
  )
)


############################################################
# 7. COMPARACIÓN INTERNA EN VALIDACIÓN CRUZADA
############################################################

cat("\n============================================================\n")
cat("PASO 6: COMPARAR VALIDACIÓN CRUZADA\n")
cat("============================================================\n")

comparacion_cv <- caret::resamples(
  list(
    kNN = modelo_knn,
    SVM_radial = modelo_svm
  )
)

cat("\nResumen de la comparación interna:\n")
print(summary(comparacion_cv))

captura_comparacion_cv <- capture.output(
  summary(comparacion_cv)
)

writeLines(
  captura_comparacion_cv,
  con = file.path(
    CARPETA_RESULTADOS,
    "03_resumen_validacion_cruzada.txt"
  )
)

png(
  filename = file.path(
    CARPETA_RESULTADOS,
    "04_comparacion_validacion_cruzada.png"
  ),
  width = 1200,
  height = 850,
  res = 140
)

print(
  caret::bwplot(
    comparacion_cv,
    metric = "ROC",
    main = "Comparación del AUC en validación cruzada"
  )
)

dev.off()


############################################################
# 8. FUNCIÓN DE EVALUACIÓN SOBRE EL CONJUNTO TEST
############################################################

evaluar_modelo <- function(modelo, nombre_modelo, datos_test) {

  cat("\n------------------------------------------------------------\n")
  cat("EVALUACIÓN EN TEST:", nombre_modelo, "\n")
  cat("------------------------------------------------------------\n")

  # Predicción de la clase.
  clases_predichas <- predict(
    modelo,
    newdata = datos_test,
    type = "raw"
  )

  # Predicción de probabilidades.
  probabilidades <- predict(
    modelo,
    newdata = datos_test,
    type = "prob"
  )

  if (!CLASE_POSITIVA %in% names(probabilidades)) {
    stop(
      paste0(
        "ERROR: no existe la columna de probabilidad '",
        CLASE_POSITIVA,
        "'."
      ),
      call. = FALSE
    )
  }

  probabilidad_positiva <- probabilidades[[CLASE_POSITIVA]]

  # Matriz de confusión.
  matriz_confusion <- caret::confusionMatrix(
    data = clases_predichas,
    reference = datos_test$objetivo,
    positive = CLASE_POSITIVA
  )

  print(matriz_confusion)

  # Curva ROC. En levels se indica:
  #   primer nivel  -> clase negativa;
  #   segundo nivel -> clase positiva.
  objeto_roc <- pROC::roc(
    response = datos_test$objetivo,
    predictor = probabilidad_positiva,
    levels = c("No_virginica", "Virginica"),
    direction = "<",
    quiet = TRUE
  )

  valor_auc <- as.numeric(pROC::auc(objeto_roc))

  # Función auxiliar para extraer una métrica de forma segura.
  extraer <- function(vector, nombre) {
    if (is.null(vector) || !nombre %in% names(vector)) {
      return(NA_real_)
    }
    as.numeric(vector[[nombre]])
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

  cat("\nMétricas resumidas:\n")
  print(tabla_metricas)

  # Tabla de predicciones por muestra.
  tabla_predicciones <- data.frame(
    Fila_test = rownames(datos_test),
    Clase_real = datos_test$objetivo,
    Clase_predicha = clases_predichas,
    Probabilidad_Virginica = probabilidad_positiva,
    Acierto = clases_predichas == datos_test$objetivo
  )

  # Guardamos la matriz de confusión.
  write.csv(
    as.data.frame(matriz_confusion$table),
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

  # Guardamos las predicciones.
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

  # Guardamos la salida textual completa de confusionMatrix.
  writeLines(
    capture.output(matriz_confusion),
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
    clases = clases_predichas,
    probabilidades = probabilidad_positiva,
    matriz_confusion = matriz_confusion,
    roc = objeto_roc,
    metricas = tabla_metricas,
    predicciones = tabla_predicciones
  )
}


############################################################
# 9. EVALUACIÓN DE LOS DOS MODELOS
############################################################

cat("\n============================================================\n")
cat("PASO 7: EVALUAR LOS MODELOS EN TEST\n")
cat("============================================================\n")

resultado_knn <- evaluar_modelo(
  modelo = modelo_knn,
  nombre_modelo = "kNN",
  datos_test = test
)

resultado_svm <- evaluar_modelo(
  modelo = modelo_svm,
  nombre_modelo = "SVM_radial",
  datos_test = test
)


############################################################
# 10. TABLA COMPARATIVA FINAL
############################################################

cat("\n============================================================\n")
cat("PASO 8: COMPARACIÓN FINAL DE MÉTRICAS\n")
cat("============================================================\n")

tabla_comparativa <- rbind(
  resultado_knn$metricas,
  resultado_svm$metricas
)

# Ordenamos primero por AUC y después por Accuracy.
tabla_comparativa <- tabla_comparativa[
  order(
    -tabla_comparativa$AUC,
    -tabla_comparativa$Accuracy
  ),
  ,
  drop = FALSE
]

rownames(tabla_comparativa) <- NULL

cat("\nTabla comparativa ordenada:\n")
print(tabla_comparativa)

write.csv(
  tabla_comparativa,
  file = file.path(
    CARPETA_RESULTADOS,
    "05_comparacion_metricas_test.csv"
  ),
  row.names = FALSE
)

mejor_modelo <- tabla_comparativa$Modelo[1]

cat(
  "\nModelo seleccionado según AUC y Accuracy:",
  mejor_modelo,
  "\n"
)


############################################################
# 11. CURVAS ROC DE LOS DOS MODELOS
############################################################

cat("\n============================================================\n")
cat("PASO 9: REPRESENTAR CURVAS ROC\n")
cat("============================================================\n")

png(
  filename = file.path(
    CARPETA_RESULTADOS,
    "06_curvas_roc_comparadas.png"
  ),
  width = 1100,
  height = 850,
  res = 140
)

plot(
  resultado_knn$roc,
  lwd = 3,
  legacy.axes = TRUE,
  main = "Comparación de curvas ROC en el conjunto test"
)

plot(
  resultado_svm$roc,
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
      "k-NN; AUC = ",
      round(resultado_knn$metricas$AUC, 3)
    ),
    paste0(
      "SVM radial; AUC = ",
      round(resultado_svm$metricas$AUC, 3)
    )
  ),
  lwd = 3,
  lty = c(1, 2),
  bty = "n"
)

dev.off()


############################################################
# 12. IMPORTANCIA DE VARIABLES
############################################################

cat("\n============================================================\n")
cat("PASO 10: IMPORTANCIA DE VARIABLES\n")
cat("============================================================\n")

guardar_importancia <- function(modelo, nombre_modelo) {

  importancia <- tryCatch(
    caret::varImp(modelo, scale = TRUE),
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
    return(invisible(NULL))
  }

  tabla_importancia <- importancia$importance
  tabla_importancia$Variable <- rownames(tabla_importancia)
  rownames(tabla_importancia) <- NULL

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

  invisible(tabla_importancia)
}

guardar_importancia(
  modelo_knn,
  "kNN"
)

guardar_importancia(
  modelo_svm,
  "SVM_radial"
)


############################################################
# 13. INTERPRETACIÓN AUTOMÁTICA
############################################################

cat("\n============================================================\n")
cat("PASO 11: INTERPRETACIÓN FINAL\n")
cat("============================================================\n")

fila_mejor <- tabla_comparativa[
  tabla_comparativa$Modelo == mejor_modelo,
  ,
  drop = FALSE
]

texto_interpretacion <- c(
  "INTERPRETACIÓN DEL EJEMPLO SUPERVISADO",
  "======================================",
  "",
  paste0(
    "Se construyó un problema de clasificación binaria para ",
    "distinguir muestras Virginica frente a No_virginica."
  ),
  paste0(
    "Se reservó el ",
    round(100 * (1 - PROPORCION_TRAIN), 0),
    "% de los datos como conjunto test independiente."
  ),
  paste0(
    "Los modelos k-NN y SVM radial se ajustaron mediante ",
    NUMERO_FOLDS,
    " folds de validación cruzada repetidos ",
    REPETICIONES_CV,
    " veces."
  ),
  paste0(
    "El modelo seleccionado fue ",
    mejor_modelo,
    ", con AUC = ",
    round(fila_mejor$AUC, 3),
    ", Accuracy = ",
    round(fila_mejor$Accuracy, 3),
    ", Sensibilidad = ",
    round(fila_mejor$Sensibilidad, 3),
    ", Especificidad = ",
    round(fila_mejor$Especificidad, 3),
    " y F1 = ",
    round(fila_mejor$F1, 3),
    "."
  ),
  "",
  "FRASE MODELO PARA EL EXAMEN",
  "---------------------------",
  paste0(
    "Tras comparar ambos clasificadores sobre un conjunto de prueba ",
    "no utilizado durante el entrenamiento, se selecciona ",
    mejor_modelo,
    " porque presenta la mejor combinación de AUC y exactitud. ",
    "La matriz de confusión, la sensibilidad, la especificidad y F1 ",
    "permiten comprobar que el rendimiento no depende únicamente ",
    "del porcentaje global de aciertos."
  )
)

cat(paste(texto_interpretacion, collapse = "\n"))
cat("\n")

writeLines(
  texto_interpretacion,
  con = file.path(
    CARPETA_RESULTADOS,
    "07_interpretacion_final.txt"
  )
)


############################################################
# 14. INFORMACIÓN DE LA SESIÓN
############################################################

# Resulta útil si existe una diferencia de versiones entre ordenadores.
writeLines(
  capture.output(sessionInfo()),
  con = file.path(
    CARPETA_RESULTADOS,
    "08_sessionInfo.txt"
  )
)


############################################################
# 15. MENSAJE FINAL
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
