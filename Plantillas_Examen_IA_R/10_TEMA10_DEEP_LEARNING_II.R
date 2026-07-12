############################################################
# TEMA 10. INTRODUCCIÓN AL APRENDIZAJE PROFUNDO II
# Pérdidas, optimización, forward/backprop e hiperparámetros
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

############################################################
# CONFIGURACIÓN: SOLO EDITAR AQUÍ
############################################################
CONFIG <- list(
  semilla = 123,
  ejecutar_calculos_perdida = TRUE,
  ejecutar_red = TRUE,
  activacion = "relu",
  optimizador = "adam",
  learning_rate = 0.001,
  epochs = 100,
  batch_size = 16,
  patience = 12
)
############################################################
# FIN DE CONFIGURACIÓN
############################################################

# ------------------------------------------------------------------
# FUNCIONES DE PÉRDIDA CALCULADAS A MANO
# ------------------------------------------------------------------
if (isTRUE(CONFIG$ejecutar_calculos_perdida)) {
  mensaje_paso(1, "Funciones de pérdida")

  y_true_reg <- c(3, -0.5, 2, 7)
  y_pred_reg <- c(2.5, 0.0, 2, 8)

  mse <- mean((y_true_reg - y_pred_reg)^2)
  mae <- mean(abs(y_true_reg - y_pred_reg))
  cat("MSE:", mse, "\n")
  cat("MAE:", mae, "\n")

  # BCE. Recortamos probabilidades para evitar log(0), que sería -Inf.
  y_true_bin <- c(1, 0, 1, 0)
  y_pred_bin <- c(0.9, 0.1, 0.8, 0.3)
  epsilon <- 1e-7
  p_segura <- pmin(pmax(y_pred_bin, epsilon), 1 - epsilon)
  bce <- -mean(y_true_bin * log(p_segura) + (1 - y_true_bin) * log(1 - p_segura))
  cat("Binary cross-entropy:", bce, "\n")

  y_true_multi <- matrix(c(
    1, 0, 0,
    0, 1, 0,
    0, 0, 1
  ), ncol = 3, byrow = TRUE)
  y_pred_multi <- matrix(c(
    0.7, 0.2, 0.1,
    0.1, 0.8, 0.1,
    0.2, 0.2, 0.6
  ), ncol = 3, byrow = TRUE)
  y_pred_multi <- pmin(pmax(y_pred_multi, epsilon), 1 - epsilon)
  cce <- -mean(rowSums(y_true_multi * log(y_pred_multi)))
  cat("Categorical cross-entropy:", cce, "\n")
}

# ------------------------------------------------------------------
# DEMOSTRACIÓN MANUAL DE FORWARD EN UNA NEURONA
# ------------------------------------------------------------------
mensaje_paso(2, "Forward propagation de una neurona")
x <- c(0.5, -1.2, 0.3)
pesos <- c(0.8, -0.4, 0.1)
sesgo <- 0.2
z <- sum(x * pesos) + sesgo
relu <- max(0, z)
sigmoide <- 1 / (1 + exp(-z))
cat("Combinación lineal z =", z, "\n")
cat("Salida ReLU =", relu, "\n")
cat("Salida sigmoide =", sigmoide, "\n")

# ------------------------------------------------------------------
# RED CON EARLY STOPPING Y OPTIMIZADOR CONFIGURABLE
# ------------------------------------------------------------------
if (isTRUE(CONFIG$ejecutar_red)) {
  mensaje_paso(3, "Preparar red neuronal con callbacks")

  usar_keras3 <- requireNamespace("keras3", quietly = TRUE)
  usar_keras <- requireNamespace("keras", quietly = TRUE)
  if (!usar_keras3 && !usar_keras) {
    error_claro("No está instalado keras3 ni keras.")
  }
  comprobar_paquetes(c("tensorflow", "caret", "ggplot2"))
  if (usar_keras3) library(keras3) else library(keras)
  library(tensorflow)

  set.seed(CONFIG$semilla)
  try(tensorflow::set_random_seed(CONFIG$semilla), silent = TRUE)

  data(iris)
  X <- as.matrix(iris[, 1:4])
  y <- as.factor(iris$Species)
  idx <- caret::createDataPartition(y, p = 0.8, list = FALSE)
  X_train_raw <- X[idx, , drop = FALSE]
  X_test_raw <- X[-idx, , drop = FALSE]
  y_train_factor <- y[idx]
  y_test_factor <- y[-idx]

  mu <- colMeans(X_train_raw)
  sigma <- apply(X_train_raw, 2, sd)
  X_train <- sweep(sweep(X_train_raw, 2, mu, "-"), 2, sigma, "/")
  X_test <- sweep(sweep(X_test_raw, 2, mu, "-"), 2, sigma, "/")

  y_train <- to_categorical(as.integer(y_train_factor) - 1, num_classes = nlevels(y))
  y_test <- to_categorical(as.integer(y_test_factor) - 1, num_classes = nlevels(y))

  modelo <- keras_model_sequential() %>%
    layer_dense(units = 32, activation = CONFIG$activacion, input_shape = ncol(X_train)) %>%
    layer_dropout(rate = 0.20) %>%
    layer_dense(units = 16, activation = CONFIG$activacion) %>%
    layer_dense(units = nlevels(y), activation = "softmax")

  # Creamos el optimizador con learning rate explícito.
  optimizador <- switch(
    tolower(CONFIG$optimizador),
    "sgd" = optimizer_sgd(learning_rate = CONFIG$learning_rate),
    "rmsprop" = optimizer_rmsprop(learning_rate = CONFIG$learning_rate),
    "adam" = optimizer_adam(learning_rate = CONFIG$learning_rate),
    error_claro("Optimizador no válido. Usa 'sgd', 'rmsprop' o 'adam'.")
  )

  modelo %>% compile(
    optimizer = optimizador,
    loss = "categorical_crossentropy",
    metrics = c("accuracy")
  )

  callbacks <- list(
    callback_early_stopping(
      monitor = "val_loss",
      patience = CONFIG$patience,
      restore_best_weights = TRUE
    ),
    callback_reduce_lr_on_plateau(
      monitor = "val_loss",
      factor = 0.5,
      patience = max(2, floor(CONFIG$patience / 3)),
      min_lr = 1e-6
    )
  )

  mensaje_paso(4, "Entrenar con forward, backpropagation y actualización de pesos")
  historia <- modelo %>% fit(
    X_train,
    y_train,
    epochs = CONFIG$epochs,
    batch_size = CONFIG$batch_size,
    validation_split = 0.2,
    callbacks = callbacks,
    verbose = 1
  )

  mensaje_paso(5, "Evaluar")
  evaluacion <- modelo %>% evaluate(X_test, y_test, verbose = 0)
  print(evaluacion)

  prob <- modelo %>% predict(X_test, verbose = 0)
  pred_int <- apply(prob, 1, which.max)
  pred_factor <- factor(levels(y)[pred_int], levels = levels(y))
  print(caret::confusionMatrix(pred_factor, y_test_factor))

  metricas <- historia$metrics
  df_hist <- data.frame(
    epoca = seq_along(metricas$loss),
    loss = metricas$loss,
    val_loss = metricas$val_loss,
    accuracy = metricas$accuracy,
    val_accuracy = metricas$val_accuracy
  )
  guardar_tabla(df_hist, "tema10_historia.csv")

  p <- ggplot2::ggplot(df_hist, ggplot2::aes(epoca)) +
    ggplot2::geom_line(ggplot2::aes(y = loss, linetype = "train")) +
    ggplot2::geom_line(ggplot2::aes(y = val_loss, linetype = "validation")) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = paste("Pérdida con", CONFIG$optimizador),
      y = "Loss",
      linetype = "Conjunto"
    )
  print(p)
  guardar_ggplot(p, "tema10_loss.png")
}

cat("\nQUÉ INTERPRETAR EN EL EXAMEN\n")
cat("- Forward propagation calcula la salida de la red.\n")
cat("- La función de pérdida cuantifica el error.\n")
cat("- Backpropagation calcula gradientes respecto a pesos y sesgos.\n")
cat("- El optimizador usa esos gradientes para actualizar parámetros.\n")
cat("- Learning rate, batch size y epochs son hiperparámetros de entrenamiento.\n")
cat("- Early stopping reduce sobreajuste y restaura los mejores pesos.\n")
