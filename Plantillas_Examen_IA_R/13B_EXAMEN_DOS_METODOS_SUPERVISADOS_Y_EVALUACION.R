############################################################
# 13B_EXAMEN_DOS_METODOS_SUPERVISADOS_Y_EVALUACION.R
############################################################
#
# OBJETIVO
# --------
# Resolver de forma segura un ejercicio que solicite:
#
#   1) preparar un problema de clasificación;
#   2) entrenar exactamente DOS métodos supervisados;
#   3) ajustar hiperparámetros mediante validación cruzada;
#   4) evaluar ambos modelos sobre un conjunto de prueba;
#   5) comparar matrices de confusión, métricas y curvas ROC;
#   6) justificar qué modelo elegir.
#
# MÉTODOS DISPONIBLES
# -------------------
# knn, svmLinear, svmRadial, svmPoly, rpart, rf, gbm,
# nb, lda, qda y rda.
#
# IMPORTANTE
# ----------
# - Deben indicarse exactamente DOS modelos diferentes.
# - El conjunto test no participa en el entrenamiento.
# - El escalado se calcula únicamente con train.
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

# Si se ejecuta desde la raíz del repositorio, entrar en la carpeta
# de las plantillas para que las rutas relativas funcionen.
if (!file.exists("00_UTILIDADES.R") &&
    file.exists(file.path("Plantillas_Examen_IA_R", "00_UTILIDADES.R"))) {
  setwd("Plantillas_Examen_IA_R")
}

source("00_UTILIDADES.R")
comprobar_directorio_proyecto()

############################################################
# CONFIGURACIÓN: SOLO EDITAR AQUÍ
############################################################
CONFIG <- list(

  # CSV situado dentro de la carpeta data/.
  archivo = file.path("data", "dataset_expresiongenes_cancer.csv"),

  # Variable que se desea predecir.
  objetivo = "primaryormetastasis",

  # Identificadores u otras columnas que no deben usarse como predictores.
  columnas_excluir = c("id", "ID", "sample", "patient"),

  # Límite de predictores numéricos. NULL utiliza todos los válidos.
  max_predictores = 50,

  # Escribe exactamente DOS métodos distintos.
  modelos_supervisados = c("knn", "svmRadial"),

  # Clase positiva para sensibilidad, especificidad, F1 y ROC.
  # NULL selecciona automáticamente el primer nivel en problemas binarios.
  clase_positiva = NULL,

  # Proporción destinada al entrenamiento.
  proporcion_train = 0.80,

  # Número máximo de folds de validación cruzada.
  folds = 5,

  # Número de combinaciones de hiperparámetros probadas por caret.
  tune_length = 8,

  # Semilla para reproducibilidad.
  semilla = 1995
)
############################################################
# FIN DE CONFIGURACIÓN
############################################################

############################################################
# 0. COMPROBACIONES PREVIAS
############################################################

mensaje_paso(0, "Comprobar configuración y paquetes")

if (length(CONFIG$modelos_supervisados) != 2) {
  error_claro("Debes escribir exactamente DOS métodos supervisados.")
}
if (length(unique(CONFIG$modelos_supervisados)) != 2) {
  error_claro("Los dos métodos supervisados deben ser diferentes.")
}

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

paquetes_necesarios <- c("caret", "pROC")
for (metodo in CONFIG$modelos_supervisados) {
  if (!metodo %in% names(paquetes_por_modelo)) {
    error_claro(
      paste0(
        "El método '", metodo, "' no está contemplado. Opciones: ",
        paste(names(paquetes_por_modelo), collapse = ", "), "."
      )
    )
  }
  paquetes_necesarios <- c(paquetes_necesarios, paquetes_por_modelo[[metodo]])
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

cat("\nDistribución original de la variable objetivo:\n")
print(table(datos_originales[[CONFIG$objetivo]], useNA = "ifany"))

############################################################
# 2. PREPARACIÓN DEL PROBLEMA SUPERVISADO
############################################################

mensaje_paso(2, "Preparar los datos para clasificación supervisada")

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
# 2.1. Partición estratificada train/test
# ------------------------------------------------------------------

mensaje_paso(2.1, "Dividir los datos en entrenamiento y prueba")

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
# 2.2. Escalado sin fuga de información
# ------------------------------------------------------------------

mensaje_paso(2.2, "Estandarizar usando solo el conjunto de entrenamiento")

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
# 3. ENTRENAMIENTO DE LOS DOS MÉTODOS SUPERVISADOS
############################################################

mensaje_paso(3, "Configurar validación cruzada")

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
    paste0("13B_", metodo, "_predicciones_test.csv")
  )

  # Guardamos la matriz de confusión como tabla.
  tabla_cm <- as.data.frame(matriz_confusion$table)
  guardar_tabla(
    tabla_cm,
    paste0("13B_", metodo, "_matriz_confusion.csv")
  )

  # Guardamos el gráfico de ajuste de hiperparámetros.
  ejecutar_seguro(
    paste0("Gráfico de ajuste de ", metodo),
    {
      png(
        filename = file.path(
          "resultados",
          paste0("13B_", metodo, "_ajuste_hiperparametros.png")
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
      paste0("13B_", metodo, "_importancia_variables.csv")
    )

    ejecutar_seguro(
      paste0("Importancia de variables de ", metodo),
      {
        png(
          filename = file.path(
            "resultados",
            paste0("13B_", metodo, "_importancia_variables.png")
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
    file = file.path("resultados", paste0("13B_", metodo, "_modelo.rds"))
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
# 4. COMPARACIÓN FINAL DE LOS DOS MÉTODOS
############################################################

mensaje_paso(4, "Comparar los dos métodos supervisados")

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
  "13B_comparacion_dos_modelos_supervisados.csv"
)

mejor_modelo <- tabla_comparacion$Metodo[1]
cat("\nMejor método según", criterio_seleccion, ":", mejor_modelo, "\n")

# ------------------------------------------------------------------
# 4.1. Curvas ROC conjuntas
# ------------------------------------------------------------------

if (
  es_binario &&
  !is.null(clase_positiva_utilizada) &&
  all(vapply(resultados_modelos, function(x) !is.null(x$roc), logical(1)))
) {

  mensaje_paso(4.1, "Crear la comparación conjunta de curvas ROC")

  nombres_modelos <- names(resultados_modelos)
  colores_roc <- seq_along(nombres_modelos)

  png(
    filename = file.path("resultados", "13B_curvas_roc_dos_modelos.png"),
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
# 5. INTERPRETACIÓN AUTOMÁTICA
############################################################

mensaje_paso(5, "Generar una interpretación para el examen")

fila_mejor <- tabla_comparacion[1, , drop = FALSE]

lineas_informe <- c(
  "============================================================",
  "BORRADOR DE RESPUESTA: DOS MÉTODOS SUPERVISADOS",
  "============================================================",
  "",
  "1. PREPROCESAMIENTO",
  paste0(
    "Se conservaron predictores numéricos, se imputaron los valores ausentes ",
    "mediante la mediana y se eliminaron las variables constantes."
  ),
  paste0(
    "La muestra se dividió de forma estratificada en ",
    round(100 * CONFIG$proporcion_train), "% para entrenamiento y ",
    round(100 * (1 - CONFIG$proporcion_train)), "% para prueba."
  ),
  "El escalado se estimó exclusivamente con el conjunto de entrenamiento para evitar fuga de información.",
  "",
  "2. ENTRENAMIENTO",
  paste0(
    "Se entrenaron los modelos ", paste(CONFIG$modelos_supervisados, collapse = " y "),
    " mediante validación cruzada y ajuste automático de hiperparámetros."
  ),
  "",
  "3. EVALUACIÓN",
  "Los modelos se evaluaron sobre el conjunto de prueba mediante matriz de confusión, Accuracy y Kappa.",
  if (es_binario) {
    "Al tratarse de un problema binario también se calcularon sensibilidad, especificidad, precisión, F1 y AUC."
  } else {
    "Al tratarse de un problema multiclase, la selección principal se basa en Accuracy."
  },
  "",
  "4. SELECCIÓN DEL MODELO",
  paste0(
    "El modelo seleccionado fue ", mejor_modelo,
    " porque obtuvo el mejor valor de ", criterio_seleccion, "."
  ),
  paste0(
    "Su Accuracy en test fue ", round(fila_mejor$Accuracy, 3),
    if (es_binario && is.finite(fila_mejor$AUC)) {
      paste0(" y su AUC fue ", round(fila_mejor$AUC, 3), ".")
    } else {
      "."
    }
  ),
  "",
  "5. INTERPRETACIÓN RESPONSABLE",
  "La decisión final no debe basarse únicamente en Accuracy: también deben considerarse el balance de clases, sensibilidad, especificidad y el coste de los falsos positivos y falsos negativos."
)

cat(paste(lineas_informe, collapse = "\n"), "\n")
writeLines(
  lineas_informe,
  con = file.path("resultados", "13B_INTERPRETACION_DOS_SUPERVISADOS.txt")
)

############################################################
# 6. RESUMEN FINAL
############################################################

cat("\n============================================================\n")
cat("ANÁLISIS SUPERVISADO COMPLETADO\n")
cat("============================================================\n")
cat("Revisa la carpeta resultados/. Encontrarás:\n")
cat("1) Predicciones de ambos modelos.\n")
cat("2) Matrices de confusión.\n")
cat("3) Comparación de métricas.\n")
cat("4) Curvas ROC, cuando el problema sea binario.\n")
cat("5) Importancia de variables, cuando el método la permita.\n")
cat("6) Modelos guardados en formato RDS.\n")
cat("7) Interpretación automática.\n")
cat("============================================================\n")
