############################################################
# TEMA 6. OTROS MÉTODOS SUPERVISADOS I
# k-NN, SVM y árbol de decisión
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
comprobar_paquetes(c("caret", "ggplot2", "rpart", "rpart.plot", "pROC"))

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
  tune_length = 8,
  ejecutar_knn = TRUE,
  ejecutar_svm_lineal = TRUE,
  ejecutar_svm_radial = TRUE,
  ejecutar_svm_polinomica = FALSE,
  ejecutar_arbol = TRUE
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

# k-NN y SVM dependen de distancias/márgenes; se escalan con estadísticas de train.
escalado <- escalar_train_test(train, test, CONFIG$objetivo)
train_s <- escalado$train
test_s <- escalado$test

control <- crear_control_caret(train_s[[CONFIG$objetivo]], CONFIG$folds)
metrica <- if (nlevels(train_s[[CONFIG$objetivo]]) == 2) "ROC" else "Accuracy"
formula_modelo <- as.formula(paste(CONFIG$objetivo, "~ ."))
resultados <- list()

entrenar_y_evaluar <- function(nombre, metodo, datos_train, datos_test, ...) {
  cat("\nEntrenando", nombre, "con método caret =", metodo, "\n")
  set.seed(CONFIG$semilla)
  modelo <- caret::train(
    formula_modelo,
    data = datos_train,
    method = metodo,
    trControl = control,
    metric = metrica,
    tuneLength = CONFIG$tune_length,
    ...
  )
  print(modelo)
  pred <- predict(modelo, newdata = datos_test, type = "raw")
  prob <- tryCatch(predict(modelo, newdata = datos_test, type = "prob"), error = function(e) NULL)
  cm <- evaluar_clasificacion(datos_test[[CONFIG$objetivo]], pred, CONFIG$clase_positiva)
  resultados[[nombre]] <<- list(modelo = modelo, pred = pred, prob = prob, cm = cm)
  invisible(modelo)
}

# ------------------------------------------------------------------
# k-NN
# ------------------------------------------------------------------
if (isTRUE(CONFIG$ejecutar_knn)) {
  mensaje_paso(2, "k vecinos más cercanos")
  entrenar_y_evaluar("kNN", "knn", train_s, test_s)
  plot(resultados$kNN$modelo)
}

# ------------------------------------------------------------------
# SVM lineal
# ------------------------------------------------------------------
if (isTRUE(CONFIG$ejecutar_svm_lineal)) {
  mensaje_paso(3, "SVM lineal")
  entrenar_y_evaluar("SVM_lineal", "svmLinear", train_s, test_s)
  plot(resultados$SVM_lineal$modelo)
}

# ------------------------------------------------------------------
# SVM radial
# ------------------------------------------------------------------
if (isTRUE(CONFIG$ejecutar_svm_radial)) {
  mensaje_paso(4, "SVM con kernel radial gaussiano")
  entrenar_y_evaluar("SVM_radial", "svmRadial", train_s, test_s)
  plot(resultados$SVM_radial$modelo)
}

# ------------------------------------------------------------------
# SVM polinómica
# ------------------------------------------------------------------
if (isTRUE(CONFIG$ejecutar_svm_polinomica)) {
  mensaje_paso(5, "SVM polinómica")
  entrenar_y_evaluar("SVM_polinomica", "svmPoly", train_s, test_s)
  plot(resultados$SVM_polinomica$modelo)
}

# ------------------------------------------------------------------
# Árbol de decisión
# ------------------------------------------------------------------
if (isTRUE(CONFIG$ejecutar_arbol)) {
  mensaje_paso(6, "Árbol de decisión")

  # El árbol no necesita escalado; usamos train y test originales.
  entrenar_y_evaluar("Arbol", "rpart", train, test)
  rpart.plot::rpart.plot(
    resultados$Arbol$modelo$finalModel,
    type = 2,
    extra = 104,
    fallen.leaves = TRUE,
    main = "Árbol de decisión final"
  )
  png(file.path("resultados", "tema6_arbol.png"), width = 1400, height = 900, res = 140)
  rpart.plot::rpart.plot(resultados$Arbol$modelo$finalModel, type = 2, extra = 104, fallen.leaves = TRUE)
  dev.off()
}

# ------------------------------------------------------------------
# Comparación y ROC binaria
# ------------------------------------------------------------------
mensaje_paso(7, "Comparar rendimiento")
if (length(resultados) > 0) {
  comparacion <- data.frame(
    modelo = names(resultados),
    accuracy = vapply(resultados, function(x) unname(x$cm$overall["Accuracy"]), numeric(1)),
    kappa = vapply(resultados, function(x) unname(x$cm$overall["Kappa"]), numeric(1))
  )
  comparacion <- comparacion[order(comparacion$accuracy, decreasing = TRUE), ]
  print(comparacion)
  guardar_tabla(comparacion, "tema6_comparacion.csv")

  if (nlevels(test[[CONFIG$objetivo]]) == 2 && !is.null(CONFIG$clase_positiva)) {
    positivo <- make.names(CONFIG$clase_positiva)
    roc_list <- list()
    for (nombre in names(resultados)) {
      prob <- resultados[[nombre]]$prob
      if (!is.null(prob) && positivo %in% names(prob)) {
        roc_list[[nombre]] <- calcular_roc_binaria(test[[CONFIG$objetivo]], prob[[positivo]], CONFIG$clase_positiva)
      }
    }
    if (length(roc_list) > 0) {
      png(file.path("resultados", "tema6_curvas_roc.png"), width = 1100, height = 850, res = 140)
      primero <- TRUE
      for (nombre in names(roc_list)) {
        plot(roc_list[[nombre]], add = !primero, legacy.axes = TRUE, main = "Curvas ROC")
        primero <- FALSE
      }
      legend("bottomright", legend = names(roc_list), lwd = 2)
      dev.off()
    }
  }
}

cat("\nQUÉ INTERPRETAR EN EL EXAMEN\n")
cat("- k-NN necesita escalado y el hiperparámetro k controla suavidad.\n")
cat("- SVM lineal busca un hiperplano de margen máximo.\n")
cat("- SVM radial modela fronteras no lineales mediante C y sigma.\n")
cat("- El árbol produce reglas interpretables; cp controla la poda.\n")
cat("- La validación cruzada ajusta hiperparámetros; el test se usa al final.\n")
