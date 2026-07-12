############################################################
# TEMA 5. ANÁLISIS DISCRIMINANTE
# LDA, QDA, RDA y FDA
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
comprobar_paquetes(c("caret", "MASS", "ggplot2"))

############################################################
# CONFIGURACIÓN: SOLO EDITAR AQUÍ
############################################################
CONFIG <- list(
  archivo = file.path("data", "dataset_expresiongenes_cancer.csv"),
  objetivo = "primaryormetastasis",
  columnas_excluir = c("id", "sample", "patient"),
  max_predictores = 20,
  usar_iris_si_falta = TRUE,
  proporcion_train = 0.8,
  semilla = 1995,
  clase_positiva = NULL,
  ejecutar_lda = TRUE,
  ejecutar_qda = TRUE,
  ejecutar_rda = TRUE,
  ejecutar_fda = TRUE
)
############################################################
# FIN DE CONFIGURACIÓN
############################################################

mensaje_paso(1, "Cargar y preparar datos")
if (file.exists(CONFIG$archivo)) {
  datos_raw <- leer_csv_seguro(CONFIG$archivo)
} else if (isTRUE(CONFIG$usar_iris_si_falta)) {
  mensaje_aviso("No se encontró el CSV. Se utilizará iris como ejemplo multiclase.")
  datos_raw <- iris
  CONFIG$objetivo <- "Species"
  CONFIG$columnas_excluir <- character(0)
  CONFIG$clase_positiva <- NULL
  CONFIG$max_predictores <- 4
} else {
  datos_raw <- leer_csv_seguro(CONFIG$archivo)
}

# Esta función conserva el objetivo y únicamente predictores numéricos limpios.
datos <- preparar_datos_supervisados(
  datos_raw,
  objetivo = CONFIG$objetivo,
  columnas_excluir = CONFIG$columnas_excluir,
  max_predictores = CONFIG$max_predictores,
  clase_positiva = CONFIG$clase_positiva
)
cat("Clases disponibles:\n")
print(table(datos[[CONFIG$objetivo]]))

partes <- dividir_train_test(datos, CONFIG$objetivo, CONFIG$proporcion_train, CONFIG$semilla)
train <- partes$train
test <- partes$test

# El escalado se calcula con train para evitar fuga de información.
escalado <- escalar_train_test(train, test, CONFIG$objetivo)
train_s <- escalado$train
test_s <- escalado$test

# Fórmula objetivo ~ todas las demás variables.
formula_modelo <- stats::as.formula(paste(CONFIG$objetivo, "~ ."))

resultados <- list()

# Función común para evaluar cada modelo.
evaluar_modelo <- function(nombre, predicho, probabilidades = NULL) {
  cat("\nRESULTADOS DE", nombre, "\n")
  cm <- evaluar_clasificacion(test_s[[CONFIG$objetivo]], predicho, CONFIG$clase_positiva)
  resultados[[nombre]] <<- list(confusion = cm, probabilidades = probabilidades)
  invisible(cm)
}

# ------------------------------------------------------------------
# LDA
# ------------------------------------------------------------------
if (isTRUE(CONFIG$ejecutar_lda)) {
  mensaje_paso(2, "LDA: análisis discriminante lineal")
  lda_modelo <- MASS::lda(formula_modelo, data = train_s)
  print(lda_modelo)
  lda_pred <- predict(lda_modelo, newdata = test_s)
  evaluar_modelo("LDA", lda_pred$class, lda_pred$posterior)

  # Proyección del conjunto de entrenamiento sobre funciones discriminantes.
  lda_train <- predict(lda_modelo, newdata = train_s)
  if (ncol(as.matrix(lda_train$x)) >= 2) {
    df_lda <- data.frame(
      LD1 = lda_train$x[, 1],
      LD2 = lda_train$x[, 2],
      Clase = train_s[[CONFIG$objetivo]]
    )
    p_lda <- ggplot2::ggplot(df_lda, ggplot2::aes(LD1, LD2, color = Clase)) +
      ggplot2::geom_point(size = 2.4, alpha = 0.8) +
      ggplot2::stat_ellipse(na.rm = TRUE) +
      ggplot2::theme_minimal() +
      ggplot2::labs(title = "Proyección LDA del entrenamiento")
    print(p_lda)
    guardar_ggplot(p_lda, "tema5_lda_proyeccion.png")
  } else {
    cat("Solo existe una función discriminante porque hay dos clases.\n")
  }
}

# ------------------------------------------------------------------
# QDA
# ------------------------------------------------------------------
if (isTRUE(CONFIG$ejecutar_qda)) {
  mensaje_paso(3, "QDA: análisis discriminante cuadrático")

  # QDA estima una matriz de covarianza por clase. Si hay más variables que casos
  # por clase, la matriz puede ser singular. Reducimos automáticamente si hace falta.
  min_casos_clase <- min(table(train_s[[CONFIG$objetivo]]))
  max_qda <- max(1, min(ncol(train_s) - 1, min_casos_clase - 2))
  predictores_qda <- setdiff(names(train_s), CONFIG$objetivo)[seq_len(max_qda)]

  if (length(predictores_qda) < ncol(train_s) - 1) {
    mensaje_aviso(paste0("QDA se limita a ", length(predictores_qda), " predictores para evitar singularidad."))
  }

  formula_qda <- as.formula(paste(CONFIG$objetivo, "~", paste(predictores_qda, collapse = " + ")))
  qda_modelo <- ejecutar_seguro("QDA", MASS::qda(formula_qda, data = train_s))
  if (!is.null(qda_modelo)) {
    qda_pred <- predict(qda_modelo, newdata = test_s)
    evaluar_modelo("QDA", qda_pred$class, qda_pred$posterior)
  }
}

# ------------------------------------------------------------------
# RDA
# ------------------------------------------------------------------
if (isTRUE(CONFIG$ejecutar_rda)) {
  mensaje_paso(4, "RDA: análisis discriminante regularizado")
  if (!requireNamespace("klaR", quietly = TRUE)) {
    mensaje_aviso("Falta klaR; se omite RDA.")
  } else {
    rda_modelo <- klaR::rda(formula_modelo, data = train_s)
    print(rda_modelo)
    rda_pred <- predict(rda_modelo, newdata = test_s)

    # Dependiendo de la versión, predict.rda puede devolver vector o lista.
    rda_clase <- if (is.list(rda_pred) && !is.null(rda_pred$class)) rda_pred$class else rda_pred
    rda_prob <- if (is.list(rda_pred)) rda_pred$posterior %||% rda_pred$posterior.values else NULL
    evaluar_modelo("RDA", rda_clase, rda_prob)
  }
}

# ------------------------------------------------------------------
# FDA
# ------------------------------------------------------------------
if (isTRUE(CONFIG$ejecutar_fda)) {
  mensaje_paso(5, "FDA: análisis discriminante flexible")
  if (!requireNamespace("mda", quietly = TRUE)) {
    mensaje_aviso("Falta mda; se omite FDA.")
  } else {
    fda_modelo <- ejecutar_seguro("FDA", mda::fda(formula_modelo, data = train_s))
    if (!is.null(fda_modelo)) {
      fda_pred <- predict(fda_modelo, newdata = test_s, type = "class")
      evaluar_modelo("FDA", fda_pred)
    }
  }
}

# ------------------------------------------------------------------
# Comparación de exactitudes
# ------------------------------------------------------------------
mensaje_paso(6, "Comparar modelos")
if (length(resultados) > 0) {
  tabla_comparacion <- data.frame(
    modelo = names(resultados),
    accuracy = vapply(resultados, function(x) unname(x$confusion$overall["Accuracy"]), numeric(1)),
    kappa = vapply(resultados, function(x) unname(x$confusion$overall["Kappa"]), numeric(1))
  )
  tabla_comparacion <- tabla_comparacion[order(tabla_comparacion$accuracy, decreasing = TRUE), ]
  print(tabla_comparacion)
  guardar_tabla(tabla_comparacion, "tema5_comparacion_modelos.csv")
}

cat("\nQUÉ INTERPRETAR EN EL EXAMEN\n")
cat("- LDA asume una covarianza común y produce fronteras lineales.\n")
cat("- QDA estima una covarianza por clase y produce fronteras cuadráticas.\n")
cat("- RDA regulariza y es más estable con muchas variables o colinealidad.\n")
cat("- FDA permite fronteras más flexibles mediante transformaciones no paramétricas.\n")
cat("- La comparación debe hacerse sobre test, no sobre entrenamiento.\n")
