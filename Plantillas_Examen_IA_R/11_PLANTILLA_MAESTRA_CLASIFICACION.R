############################################################
# PLANTILLA MAESTRA DE CLASIFICACIÓN
# Flujo completo: carga -> limpieza -> partición -> modelos -> evaluación
############################################################

rm(list = ls())
# Si se ejecuta desde la raíz del repositorio, entrar en la carpeta
# de las plantillas para que las rutas relativas funcionen.
if (!file.exists("00_UTILIDADES.R") &&
    file.exists(file.path("Plantillas_Examen_IA_R", "00_UTILIDADES.R"))) {
  setwd("Plantillas_Examen_IA_R")
}

source("00_UTILIDADES.R")
comprobar_directorio_proyecto()
comprobar_paquetes(c("caret", "ggplot2", "rpart", "randomForest", "gbm", "pROC"))

############################################################
# CONFIGURACIÓN: SOLO EDITAR AQUÍ
############################################################
CONFIG <- list(
  archivo = file.path("data", "dataset_expresiongenes_cancer.csv"),
  objetivo = "primaryormetastasis",
  columnas_excluir = c("id", "sample", "patient"),
  max_predictores = 50,
  clase_positiva = NULL,  # En binario escribe el nombre EXACTO, p. ej. "Metastatic".
  proporcion_train = 0.8,
  semilla = 1995,
  folds = 5,
  modelos = c("knn", "svmLinear", "svmRadial", "rpart", "rf", "gbm")
)
############################################################
# FIN DE CONFIGURACIÓN
############################################################

mensaje_paso(1, "Cargar")
raw <- leer_csv_seguro(CONFIG$archivo)
inspeccionar_datos(raw)

mensaje_paso(2, "Limpiar y definir objetivo")
datos <- preparar_datos_supervisados(
  raw,
  CONFIG$objetivo,
  CONFIG$columnas_excluir,
  CONFIG$max_predictores,
  CONFIG$clase_positiva
)
print(table(datos[[CONFIG$objetivo]]))

mensaje_paso(3, "Dividir en entrenamiento y test")
partes <- dividir_train_test(datos, CONFIG$objetivo, CONFIG$proporcion_train, CONFIG$semilla)
train <- partes$train
test <- partes$test

mensaje_paso(4, "Escalar usando solo el entrenamiento")
escalado <- escalar_train_test(train, test, CONFIG$objetivo)
train_s <- escalado$train
test_s <- escalado$test

mensaje_paso(5, "Configurar validación cruzada")
control <- crear_control_caret(train[[CONFIG$objetivo]], CONFIG$folds)
metrica <- if (nlevels(train[[CONFIG$objetivo]]) == 2) "ROC" else "Accuracy"
formula_modelo <- as.formula(paste(CONFIG$objetivo, "~ ."))

# Los modelos basados en distancias/márgenes usan datos escalados.
modelos_escalados <- c("knn", "svmLinear", "svmRadial", "svmPoly")
resultados <- list()

mensaje_paso(6, "Entrenar modelos")
for (metodo in CONFIG$modelos) {
  cat("\n================ MODELO:", metodo, "================\n")
  usar_escalado <- metodo %in% modelos_escalados
  datos_train <- if (usar_escalado) train_s else train
  datos_test <- if (usar_escalado) test_s else test

  argumentos_extra <- list()
  if (metodo == "rf") argumentos_extra$ntree <- 500
  if (metodo == "gbm") argumentos_extra$verbose <- FALSE

  set.seed(CONFIG$semilla)
  modelo <- tryCatch(
    do.call(
      caret::train,
      c(
        list(
          form = formula_modelo,
          data = datos_train,
          method = metodo,
          trControl = control,
          metric = metrica,
          tuneLength = 6
        ),
        argumentos_extra
      )
    ),
    error = function(e) {
      mensaje_aviso(paste0("El modelo ", metodo, " falló: ", conditionMessage(e)))
      NULL
    }
  )

  if (is.null(modelo)) next

  pred <- predict(modelo, newdata = datos_test, type = "raw")
  prob <- tryCatch(predict(modelo, newdata = datos_test, type = "prob"), error = function(e) NULL)
  cm <- evaluar_clasificacion(datos_test[[CONFIG$objetivo]], pred, CONFIG$clase_positiva)

  resultados[[metodo]] <- list(
    modelo = modelo,
    pred = pred,
    prob = prob,
    cm = cm,
    test_usado = datos_test
  )
}

mensaje_paso(7, "Comparar")
if (length(resultados) == 0) error_claro("No se pudo entrenar ningún modelo.")

comparacion <- data.frame(
  modelo = names(resultados),
  accuracy = vapply(resultados, function(x) unname(x$cm$overall["Accuracy"]), numeric(1)),
  kappa = vapply(resultados, function(x) unname(x$cm$overall["Kappa"]), numeric(1))
)
comparacion <- comparacion[order(comparacion$accuracy, decreasing = TRUE), ]
print(comparacion)
guardar_tabla(comparacion, "maestra_clasificacion_comparacion.csv")

mejor_nombre <- comparacion$modelo[1]
cat("Mejor modelo por accuracy en test:", mejor_nombre, "\n")
mejor <- resultados[[mejor_nombre]]

# Guardamos predicciones del mejor modelo.
salida_predicciones <- data.frame(
  real = mejor$test_usado[[CONFIG$objetivo]],
  predicho = mejor$pred
)
if (!is.null(mejor$prob)) salida_predicciones <- cbind(salida_predicciones, mejor$prob)
guardar_tabla(salida_predicciones, "maestra_clasificacion_mejor_modelo_predicciones.csv")

mensaje_paso(8, "ROC del mejor modelo si el problema es binario")
if (nlevels(test[[CONFIG$objetivo]]) == 2 && !is.null(CONFIG$clase_positiva) && !is.null(mejor$prob)) {
  positivo <- make.names(CONFIG$clase_positiva)
  if (positivo %in% names(mejor$prob)) {
    roc_obj <- calcular_roc_binaria(mejor$test_usado[[CONFIG$objetivo]], mejor$prob[[positivo]], CONFIG$clase_positiva)
    png(file.path("resultados", "maestra_clasificacion_mejor_modelo_roc.png"), width = 1000, height = 800, res = 140)
    plot(roc_obj, print.auc = TRUE, legacy.axes = TRUE, main = paste("ROC -", mejor_nombre))
    dev.off()
  }
}

cat("\nRESPUESTA MODELO PARA EL EXAMEN\n")
cat("Se realizó una partición estratificada train/test y el ajuste de hiperparámetros\n")
cat("se efectuó mediante validación cruzada únicamente sobre entrenamiento.\n")
cat("El escalado se estimó con entrenamiento para evitar fuga de información.\n")
cat("El modelo seleccionado fue", mejor_nombre, "por presentar el mejor rendimiento final en test.\n")
