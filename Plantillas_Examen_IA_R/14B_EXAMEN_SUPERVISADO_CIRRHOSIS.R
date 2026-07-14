############################################################
# 14B_EXAMEN_SUPERVISADO_CIRRHOSIS.R
############################################################
#
# EXAMEN 14B - CLASIFICACIÓN SUPERVISADA
# --------------------------------------
# Objetivo:
#   1) usar el conjunto reducido de la parte 1;
#   2) dividir en train/test;
#   3) entrenar al menos dos clasificadores;
#   4) evaluar precisión, recall, F1 y AUC-ROC;
#   5) comparar resultados y justificar el mejor modelo.
#
# Estrategia:
#   - Se trabaja sobre las coordenadas PCA 3D generadas en 14A.
#   - Se predice Status (D = evento positivo, es decir, no supervivencia).
#   - Se comparan tres modelos: regresión logística, kNN y árbol.
############################################################

rm(list = ls())

if (!file.exists("00_UTILIDADES.R") &&
    file.exists(file.path("Plantillas_Examen_IA_R", "00_UTILIDADES.R"))) {
  setwd("Plantillas_Examen_IA_R")
}

if (!file.exists("00_UTILIDADES.R")) {
  stop(
    paste0(
      "\nERROR: no se encuentra 00_UTILIDADES.R. ",
      "Abre Plantillas_Examen_IA_R.Rproj y ejecuta este script desde la carpeta del proyecto.\n",
      "Directorio actual: ", getwd()
    ),
    call. = FALSE
  )
}

source("00_UTILIDADES.R")
comprobar_directorio_proyecto()

############################################################
# CONFIGURACIÓN
############################################################
CONFIG <- list(
  archivo_reducido = file.path("resultados", "14A_no_supervisado_coordenadas.csv"),
  objetivo = "Status",
  clase_positiva = "D",
  columnas_modelo = c("Dim1_3D", "Dim2_3D", "Dim3_3D"),
  modelos = c("glm", "knn", "rpart"),
  proporcion_train = 0.80,
  folds = 5,
  tune_length = 8,
  semilla = 1995
)

############################################################
# 0. COMPROBACIONES PREVIAS
############################################################

mensaje_paso(0, "Comprobar configuración y paquetes")

paquetes_necesarios <- c("caret", "pROC", "class", "rpart", "ggplot2")
comprobar_paquetes(paquetes_necesarios)
mensaje_ok("Paquetes disponibles.")

if (!file.exists(CONFIG$archivo_reducido)) {
  error_claro(
    paste0(
      "No se encontró el archivo reducido de la parte 1: '", CONFIG$archivo_reducido, "'. ",
      "Ejecuta primero 14A_EXAMEN_METODO_NO_SUPERVISADO_2D_3D_CIRRHOSIS.R."
    )
  )
}

set.seed(CONFIG$semilla)

############################################################
# 1. CARGA E INSPECCIÓN DEL CONJUNTO REDUCIDO
############################################################

mensaje_paso(1, "Cargar e inspeccionar el conjunto reducido")

datos_reducidos <- leer_csv_seguro(CONFIG$archivo_reducido)
inspeccionar_datos(datos_reducidos, "datos_reducidos")
validar_columna(datos_reducidos, CONFIG$objetivo, "datos_reducidos")

cat("\nDistribución original de la variable objetivo:\n")
print(table(datos_reducidos[[CONFIG$objetivo]], useNA = "ifany"))

mensaje_paso(1.1, "Resumen básico de valores ausentes y variables")

resumen_na <- data.frame(
  Variable = names(datos_reducidos),
  Numero_NA = vapply(datos_reducidos, function(x) sum(is.na(x)), numeric(1)),
  Porcentaje_NA = 100 * vapply(datos_reducidos, function(x) mean(is.na(x)), numeric(1)),
  stringsAsFactors = FALSE
)
guardar_tabla(resumen_na[order(-resumen_na$Porcentaje_NA, resumen_na$Variable), ], "14B_resumen_na.csv")

############################################################
# 2. PREPARACIÓN DEL PROBLEMA SUPERVISADO
############################################################

mensaje_paso(2, "Preparar variables para clasificación")

validar_columna(datos_reducidos, CONFIG$objetivo, "datos_reducidos")
for (col in CONFIG$columnas_modelo) validar_columna(datos_reducidos, col, "datos_reducidos")

datos_modelo <- data.frame(
  Status = preparar_objetivo(datos_reducidos[[CONFIG$objetivo]], clase_positiva = CONFIG$clase_positiva),
  Dim1_3D = datos_reducidos$Dim1_3D,
  Dim2_3D = datos_reducidos$Dim2_3D,
  Dim3_3D = datos_reducidos$Dim3_3D
)

clase_positiva_utilizada <- CONFIG$clase_positiva
datos_modelo$Status <- factor(
  datos_modelo$Status,
  levels = c(clase_positiva_utilizada, setdiff(levels(datos_modelo$Status), clase_positiva_utilizada))
)

cat("\nClases utilizadas en el problema supervisado:\n")
print(table(datos_modelo$Status))
cat("Niveles, en orden:", paste(levels(datos_modelo$Status), collapse = ", "), "\n")
cat("Clase positiva:", clase_positiva_utilizada, "\n")

############################################################
# 3. DIVISIÓN TRAIN / TEST
############################################################

mensaje_paso(3, "Dividir en entrenamiento y prueba")

particion <- dividir_train_test(
  df = datos_modelo,
  objetivo = "Status",
  proporcion_train = CONFIG$proporcion_train,
  semilla = CONFIG$semilla
)

train <- particion$train
test <- particion$test

cat("\nDistribución en TRAIN:\n")
print(table(train$Status))
cat("\nDistribución en TEST:\n")
print(table(test$Status))

mensaje_paso(3.1, "Estandarizar usando solo TRAIN")

escalado <- escalar_train_test(train = train, test = test, objetivo = "Status")
train_escalado <- escalado$train
test_escalado <- escalado$test

############################################################
# 4. ENTRENAMIENTO DE MODELOS
############################################################

mensaje_paso(4, "Entrenar modelos supervisados")

control_caret <- crear_control_caret(
  y = train_escalado$Status,
  max_folds = CONFIG$folds,
  repeticiones = 1,
  guardar_predicciones = TRUE
)

formula_modelo <- Status ~ .

modelos_entrenados <- list()

for (metodo in CONFIG$modelos) {
  cat("\n============================================================\n")
  cat("ENTRENANDO:", metodo, "\n")
  cat("============================================================\n")

  set.seed(CONFIG$semilla)

  argumentos_extra <- list()
  if (metodo == "glm") argumentos_extra$family <- binomial

  modelo <- do.call(
    caret::train,
    c(
      list(
        form = formula_modelo,
        data = train_escalado,
        method = metodo,
        trControl = control_caret,
        metric = "ROC",
        tuneLength = CONFIG$tune_length
      ),
      argumentos_extra
    )
  )

  print(modelo)
  if (!is.null(modelo$bestTune)) {
    cat("\nMejores hiperparámetros:\n")
    print(modelo$bestTune)
  }

  prediccion_clase <- predict(modelo, newdata = test_escalado, type = "raw")
  prediccion_probabilidad <- predict(modelo, newdata = test_escalado, type = "prob")

  matriz_confusion <- evaluar_clasificacion(
    real = test_escalado$Status,
    predicho = prediccion_clase,
    clase_positiva = clase_positiva_utilizada
  )

  roc_objeto <- calcular_roc_binaria(
    real = test_escalado$Status,
    probabilidad_positiva = prediccion_probabilidad[[clase_positiva_utilizada]],
    clase_positiva = clase_positiva_utilizada
  )

  auc_valor <- as.numeric(pROC::auc(roc_objeto))

  tabla_predicciones <- data.frame(
    real = test_escalado$Status,
    predicho = prediccion_clase,
    probabilidad_positiva = prediccion_probabilidad[[clase_positiva_utilizada]]
  )
  guardar_tabla(tabla_predicciones, paste0("14B_", metodo, "_predicciones_test.csv"))
  guardar_tabla(as.data.frame(matriz_confusion$table), paste0("14B_", metodo, "_matriz_confusion.csv"))

  modelos_entrenados[[metodo]] <- list(
    modelo = modelo,
    prediccion_clase = prediccion_clase,
    prediccion_probabilidad = prediccion_probabilidad,
    matriz_confusion = matriz_confusion,
    roc = roc_objeto,
    auc = auc_valor
  )
}

############################################################
# 5. COMPARACIÓN DE MODELOS
############################################################

mensaje_paso(5, "Comparar el rendimiento de los modelos")

extraer_metrica <- function(objeto, nombre) {
  if (is.null(objeto) || is.matrix(objeto) || !nombre %in% names(objeto)) return(NA_real_)
  as.numeric(objeto[[nombre]])
}

tabla_comparacion <- do.call(
  rbind,
  lapply(names(modelos_entrenados), function(nombre_modelo) {
    cm <- modelos_entrenados[[nombre_modelo]]$matriz_confusion
    data.frame(
      Modelo = nombre_modelo,
      Accuracy = as.numeric(cm$overall["Accuracy"]),
      Precision = extraer_metrica(cm$byClass, "Pos Pred Value"),
      Recall = extraer_metrica(cm$byClass, "Sensitivity"),
      F1 = extraer_metrica(cm$byClass, "F1"),
      AUC = modelos_entrenados[[nombre_modelo]]$auc,
      stringsAsFactors = FALSE
    )
  })
)

tabla_comparacion <- tabla_comparacion[order(tabla_comparacion$AUC, tabla_comparacion$F1, decreasing = TRUE), ]
rownames(tabla_comparacion) <- NULL
print(tabla_comparacion)

guardar_tabla(tabla_comparacion, "14B_comparacion_modelos.csv")

mejor_modelo <- tabla_comparacion$Modelo[1]

############################################################
# 6. CURVA ROC COMPARADA
############################################################

mensaje_paso(6, "Guardar curva ROC comparada")

png(filename = file.path("resultados", "14B_curvas_roc_comparadas.png"), width = 1200, height = 900, res = 150)
plot(modelos_entrenados[[names(modelos_entrenados)[1]]]$roc, col = "steelblue", lwd = 2, main = "ROC de los modelos supervisados")
cols <- c("steelblue", "firebrick", "darkgreen")
for (i in seq_along(modelos_entrenados)) {
  if (i == 1) next
  plot(modelos_entrenados[[i]]$roc, add = TRUE, col = cols[i], lwd = 2)
}
legend(
  "bottomright",
  legend = paste0(names(modelos_entrenados), " (AUC=", round(vapply(modelos_entrenados, `[[`, numeric(1), "auc"), 3), ")"),
  col = cols[seq_along(modelos_entrenados)],
  lwd = 2,
  cex = 0.9
)
dev.off()
mensaje_ok("Curva ROC guardada en resultados/14B_curvas_roc_comparadas.png")

############################################################
# 7. INTERPRETACIÓN FINAL
############################################################

mensaje_paso(7, "Generar respuesta de reflexión")

mejor_fila <- tabla_comparacion[1, ]

interpretacion <- c(
  "============================================================",
  "BORRADOR DE RESPUESTA: CLASIFICACIÓN SUPERVISADA",
  "============================================================",
  "",
  paste0("Se utilizó el conjunto reducido de la parte 1 (coordenadas PCA 3D) y se dividió en train/test con proporción ", CONFIG$proporcion_train, "."),
  paste0("Se entrenaron tres modelos supervisados: regresión logística, kNN y árbol de decisión."),
  paste0("La clase positiva se fijó en '", clase_positiva_utilizada, "' para evaluar el evento clínico de no supervivencia."),
  paste0("El mejor modelo según AUC y F1 fue '", mejor_modelo, "' con Accuracy = ", round(mejor_fila$Accuracy, 3), ", Precision = ", round(mejor_fila$Precision, 3), ", Recall = ", round(mejor_fila$Recall, 3), ", F1 = ", round(mejor_fila$F1, 3), " y AUC = ", round(mejor_fila$AUC, 3), "."),
  "",
  "Conclusión: si un modelo tiene mayor AUC y F1, separa mejor las clases y mantiene un mejor equilibrio entre falsos positivos y falsos negativos.",
  paste0("En esta configuración, '", mejor_modelo, "' fue el más conveniente porque ofreció el mejor compromiso global entre discriminación y equilibrio de clases."),
  ""
)

cat(paste(interpretacion, collapse = "\n"), "\n")
writeLines(interpretacion, con = file.path("resultados", "14B_INTERPRETACION_FINAL.txt"))

############################################################
# 8. RESUMEN FINAL
############################################################

cat("\n============================================================\n")
cat("ANÁLISIS SUPERVISADO COMPLETADO\n")
cat("============================================================\n")
cat("Revisa la carpeta resultados/. Encontrarás:\n")
cat("1) Predicciones de cada modelo.\n")
cat("2) Matrices de confusión.\n")
cat("3) Comparación de métricas.\n")
cat("4) Curva ROC comparada.\n")
cat("5) Interpretación final lista para el examen.\n")
cat("============================================================\n")
