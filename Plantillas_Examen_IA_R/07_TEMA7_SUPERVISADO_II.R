############################################################
# TEMA 7. OTROS MÉTODOS SUPERVISADOS II
# Bagging, Naive Bayes, Random Forest y GBM
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
comprobar_paquetes(c("caret", "ggplot2", "randomForest", "gbm"))

############################################################
# CONFIGURACIÓN: SOLO EDITAR AQUÍ
############################################################
CONFIG <- list(
  archivo = file.path("data", "dataset_expresiongenes_cancer.csv"),
  objetivo = "primaryormetastasis",
  columnas_excluir = c("id", "sample", "patient"),
  max_predictores = 50,
  usar_iris_si_falta = TRUE,
  proporcion_train = 0.8,
  semilla = 1995,
  clase_positiva = NULL,
  folds = 5,
  tune_length = 5,
  ejecutar_bagging = TRUE,
  ejecutar_naive_bayes = TRUE,
  ejecutar_random_forest = TRUE,
  ejecutar_gbm = TRUE
)
############################################################
# FIN DE CONFIGURACIÓN
############################################################

mensaje_paso(1, "Cargar y preparar datos")
if (file.exists(CONFIG$archivo)) {
  raw <- leer_csv_seguro(CONFIG$archivo)
} else if (isTRUE(CONFIG$usar_iris_si_falta)) {
  mensaje_aviso("No se encontró el CSV. Se usará iris como ejemplo.")
  raw <- iris
  CONFIG$objetivo <- "Species"
  CONFIG$columnas_excluir <- character(0)
  CONFIG$max_predictores <- 4
  CONFIG$clase_positiva <- NULL
} else {
  raw <- leer_csv_seguro(CONFIG$archivo)
}

datos <- preparar_datos_supervisados(
  raw, CONFIG$objetivo, CONFIG$columnas_excluir,
  CONFIG$max_predictores, CONFIG$clase_positiva
)
partes <- dividir_train_test(datos, CONFIG$objetivo, CONFIG$proporcion_train, CONFIG$semilla)
train <- partes$train
test <- partes$test

control <- crear_control_caret(train[[CONFIG$objetivo]], CONFIG$folds)
metrica <- if (nlevels(train[[CONFIG$objetivo]]) == 2) "ROC" else "Accuracy"
formula_modelo <- as.formula(paste(CONFIG$objetivo, "~ ."))
resultados <- list()

entrenar <- function(nombre, metodo, ...) {
  cat("\nEntrenando", nombre, "\n")
  set.seed(CONFIG$semilla)
  modelo <- ejecutar_seguro(
    nombre,
    caret::train(
      formula_modelo,
      data = train,
      method = metodo,
      trControl = control,
      metric = metrica,
      tuneLength = CONFIG$tune_length,
      ...
    )
  )
  if (is.null(modelo)) return(NULL)

  print(modelo)
  pred <- predict(modelo, newdata = test, type = "raw")
  prob <- tryCatch(predict(modelo, newdata = test, type = "prob"), error = function(e) NULL)
  cm <- evaluar_clasificacion(test[[CONFIG$objetivo]], pred, CONFIG$clase_positiva)
  resultados[[nombre]] <<- list(modelo = modelo, pred = pred, prob = prob, cm = cm)
  invisible(modelo)
}

# ------------------------------------------------------------------
# Bagging de árboles
# ------------------------------------------------------------------
if (isTRUE(CONFIG$ejecutar_bagging)) {
  mensaje_paso(2, "Bagging")
  entrenar("Bagging", "treebag")
}

# ------------------------------------------------------------------
# Naive Bayes
# ------------------------------------------------------------------
if (isTRUE(CONFIG$ejecutar_naive_bayes)) {
  mensaje_paso(3, "Naive Bayes")
  if (!requireNamespace("klaR", quietly = TRUE)) {
    mensaje_aviso("Falta klaR; se omite Naive Bayes.")
  } else {
    entrenar("Naive_Bayes", "nb")
  }
}

# ------------------------------------------------------------------
# Random Forest
# ------------------------------------------------------------------
if (isTRUE(CONFIG$ejecutar_random_forest)) {
  mensaje_paso(4, "Random Forest")
  entrenar("Random_Forest", "rf", ntree = 500, importance = TRUE)

  if (!is.null(resultados$Random_Forest)) {
    importancia <- caret::varImp(resultados$Random_Forest$modelo, scale = TRUE)
    print(importancia)
    png(file.path("resultados", "tema7_importancia_random_forest.png"), width = 1100, height = 850, res = 140)
    plot(importancia, top = min(20, ncol(train) - 1), main = "Importancia de variables - Random Forest")
    dev.off()
  }
}

# ------------------------------------------------------------------
# Gradient Boosting Machine
# ------------------------------------------------------------------
if (isTRUE(CONFIG$ejecutar_gbm)) {
  mensaje_paso(5, "Gradient Boosting Machine")
  entrenar("GBM", "gbm", verbose = FALSE)

  if (!is.null(resultados$GBM)) {
    importancia_gbm <- caret::varImp(resultados$GBM$modelo, scale = TRUE)
    print(importancia_gbm)
    png(file.path("resultados", "tema7_importancia_gbm.png"), width = 1100, height = 850, res = 140)
    plot(importancia_gbm, top = min(20, ncol(train) - 1), main = "Importancia de variables - GBM")
    dev.off()
  }
}

# ------------------------------------------------------------------
# Comparación
# ------------------------------------------------------------------
mensaje_paso(6, "Comparar modelos")
if (length(resultados) > 0) {
  comparacion <- data.frame(
    modelo = names(resultados),
    accuracy = vapply(resultados, function(x) unname(x$cm$overall["Accuracy"]), numeric(1)),
    kappa = vapply(resultados, function(x) unname(x$cm$overall["Kappa"]), numeric(1))
  )
  comparacion <- comparacion[order(comparacion$accuracy, decreasing = TRUE), ]
  print(comparacion)
  guardar_tabla(comparacion, "tema7_comparacion.csv")
}

cat("\nQUÉ INTERPRETAR EN EL EXAMEN\n")
cat("- Bagging entrena modelos en muestras bootstrap y promedia sus predicciones.\n")
cat("- Random Forest añade selección aleatoria de variables en cada división.\n")
cat("- Naive Bayes usa el teorema de Bayes y asume independencia condicional.\n")
cat("- GBM construye modelos secuenciales para corregir errores anteriores.\n")
cat("- La importancia de variables no implica causalidad.\n")
