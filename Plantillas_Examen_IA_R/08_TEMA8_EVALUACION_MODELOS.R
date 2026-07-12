############################################################
# TEMA 8. EVALUACIÓN DE MODELOS
# Clasificación, ROC/PR y regresión
############################################################

rm(list = ls())
source("00_UTILIDADES.R")
comprobar_directorio_proyecto()
comprobar_paquetes(c("caret", "pROC", "ggplot2"))

############################################################
# CONFIGURACIÓN: SOLO EDITAR AQUÍ
############################################################
CONFIG <- list(
  clase_positiva = "Positivo",
  umbral = 0.50,
  ejecutar_ejemplo_clasificacion = TRUE,
  ejecutar_ejemplo_regresion = TRUE
)
############################################################
# FIN DE CONFIGURACIÓN
############################################################

# ------------------------------------------------------------------
# FUNCIONES DE MÉTRICAS EXPLÍCITAS
# ------------------------------------------------------------------
metricas_binarias_manual <- function(real, predicho, positiva) {
  real <- factor(real)
  predicho <- factor(predicho, levels = levels(real))
  positiva <- make.names(positiva)

  if (nlevels(real) != 2) error_claro("Esta función manual requiere dos clases.")
  negativa <- setdiff(levels(real), positiva)
  if (length(negativa) != 1) error_claro("Clase positiva incorrecta.")

  TP <- sum(real == positiva & predicho == positiva)
  TN <- sum(real == negativa & predicho == negativa)
  FP <- sum(real == negativa & predicho == positiva)
  FN <- sum(real == positiva & predicho == negativa)

  division_segura <- function(a, b) ifelse(b == 0, NA_real_, a / b)

  data.frame(
    TP = TP, TN = TN, FP = FP, FN = FN,
    accuracy = division_segura(TP + TN, TP + TN + FP + FN),
    sensitivity_recall = division_segura(TP, TP + FN),
    specificity = division_segura(TN, TN + FP),
    precision = division_segura(TP, TP + FP),
    npv = division_segura(TN, TN + FN),
    f1 = division_segura(2 * TP, 2 * TP + FP + FN),
    balanced_accuracy = mean(c(
      division_segura(TP, TP + FN),
      division_segura(TN, TN + FP)
    ), na.rm = TRUE)
  )
}

# ------------------------------------------------------------------
# EJEMPLO DE CLASIFICACIÓN BINARIA
# Sustituye estos vectores por las clases y probabilidades de tu modelo.
# ------------------------------------------------------------------
if (isTRUE(CONFIG$ejecutar_ejemplo_clasificacion)) {
  mensaje_paso(1, "Evaluación de clasificación binaria")

  # Valores reales y probabilidad estimada de la clase positiva.
  real_original <- c("Positivo", "Negativo", "Positivo", "Negativo", "Positivo",
                     "Negativo", "Negativo", "Positivo", "Positivo", "Negativo")
  prob_positiva <- c(0.90, 0.10, 0.80, 0.30, 0.40, 0.20, 0.70, 0.65, 0.55, 0.45)

  # Convertimos a factor con nombres válidos para evitar problemas de caret.
  real <- preparar_objetivo(real_original, CONFIG$clase_positiva)
  positiva <- make.names(CONFIG$clase_positiva)
  negativa <- setdiff(levels(real), positiva)

  # Aplicación del umbral: si p >= umbral se predice positiva.
  predicho <- factor(
    ifelse(prob_positiva >= CONFIG$umbral, positiva, negativa),
    levels = levels(real)
  )

  cat("Matriz de confusión con umbral", CONFIG$umbral, "\n")
  cm <- evaluar_clasificacion(real, predicho, CONFIG$clase_positiva)

  cat("\nCálculo manual de métricas:\n")
  tabla_metricas <- metricas_binarias_manual(real, predicho, CONFIG$clase_positiva)
  print(tabla_metricas)
  guardar_tabla(tabla_metricas, "tema8_metricas_binarias.csv")

  mensaje_paso(2, "Curva ROC y AUC")
  roc_obj <- calcular_roc_binaria(real, prob_positiva, CONFIG$clase_positiva)
  auc_valor <- as.numeric(pROC::auc(roc_obj))
  cat("AUC ROC:", round(auc_valor, 4), "\n")

  png(file.path("resultados", "tema8_roc.png"), width = 1000, height = 800, res = 140)
  plot(roc_obj, legacy.axes = TRUE, print.auc = TRUE, main = "Curva ROC")
  abline(a = 0, b = 1, lty = 2)
  dev.off()

  mensaje_paso(3, "Buscar un umbral mediante índice de Youden")
  mejor <- pROC::coords(
    roc_obj,
    x = "best",
    best.method = "youden",
    ret = c("threshold", "sensitivity", "specificity"),
    transpose = FALSE
  )
  print(mejor)

  mensaje_paso(4, "Curva Precision-Recall")
  if (requireNamespace("PRROC", quietly = TRUE)) {
    scores_pos <- prob_positiva[real == positiva]
    scores_neg <- prob_positiva[real != positiva]
    pr <- PRROC::pr.curve(scores.class0 = scores_pos, scores.class1 = scores_neg, curve = TRUE)
    cat("AUC Precision-Recall:", round(pr$auc.integral, 4), "\n")
    png(file.path("resultados", "tema8_precision_recall.png"), width = 1000, height = 800, res = 140)
    plot(pr, main = "Curva Precision-Recall")
    dev.off()
  } else {
    mensaje_aviso("Falta PRROC; se omite la curva Precision-Recall.")
  }

  mensaje_paso(5, "Efecto de distintos umbrales")
  umbrales <- seq(0.1, 0.9, by = 0.05)
  tabla_umbrales <- do.call(rbind, lapply(umbrales, function(u) {
    pred_u <- factor(ifelse(prob_positiva >= u, positiva, negativa), levels = levels(real))
    m <- metricas_binarias_manual(real, pred_u, CONFIG$clase_positiva)
    cbind(umbral = u, m)
  }))
  print(tabla_umbrales)
  guardar_tabla(tabla_umbrales, "tema8_metricas_por_umbral.csv")
}

# ------------------------------------------------------------------
# EJEMPLO DE REGRESIÓN
# ------------------------------------------------------------------
if (isTRUE(CONFIG$ejecutar_ejemplo_regresion)) {
  mensaje_paso(6, "Evaluación de regresión")

  y_real <- c(3.0, -0.5, 2.0, 7.0, 4.5, 6.2)
  y_pred <- c(2.5, 0.0, 2.0, 8.0, 4.0, 5.8)

  errores <- y_real - y_pred
  MAE <- mean(abs(errores))
  MSE <- mean(errores^2)
  RMSE <- sqrt(MSE)
  R2 <- 1 - sum(errores^2) / sum((y_real - mean(y_real))^2)

  regresion_metricas <- data.frame(MAE = MAE, MSE = MSE, RMSE = RMSE, R2 = R2)
  print(regresion_metricas)
  guardar_tabla(regresion_metricas, "tema8_metricas_regresion.csv")

  df_reg <- data.frame(Real = y_real, Predicho = y_pred, Residuo = errores)
  p_reg <- ggplot2::ggplot(df_reg, ggplot2::aes(Real, Predicho)) +
    ggplot2::geom_point(size = 3) +
    ggplot2::geom_abline(intercept = 0, slope = 1, linetype = 2) +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = "Valores reales frente a predichos")
  print(p_reg)
  guardar_ggplot(p_reg, "tema8_regresion_real_predicho.png")

  p_res <- ggplot2::ggplot(df_reg, ggplot2::aes(Predicho, Residuo)) +
    ggplot2::geom_point(size = 3) +
    ggplot2::geom_hline(yintercept = 0, linetype = 2) +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = "Gráfico de residuos")
  print(p_res)
  guardar_ggplot(p_res, "tema8_regresion_residuos.png")
}

cat("\nQUÉ INTERPRETAR EN EL EXAMEN\n")
cat("- Accuracy puede ser engañosa con clases desbalanceadas.\n")
cat("- Sensibilidad prioriza detectar positivos; especificidad descartar negativos.\n")
cat("- F1 combina precisión y sensibilidad.\n")
cat("- ROC evalúa todos los umbrales; PR es útil con positivos raros.\n")
cat("- En regresión, RMSE penaliza más los errores grandes que MAE.\n")
