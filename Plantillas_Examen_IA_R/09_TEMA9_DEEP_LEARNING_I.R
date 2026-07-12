############################################################
# TEMA 9. INTRODUCCIÓN AL APRENDIZAJE PROFUNDO I
# Preparación de datos, tensores, nodos y activaciones
############################################################

rm(list = ls())
source("00_UTILIDADES.R")
comprobar_directorio_proyecto()
comprobar_paquetes(c("caret", "ggplot2", "tensorflow"), detener = FALSE)

############################################################
# CONFIGURACIÓN: SOLO EDITAR AQUÍ
############################################################
CONFIG <- list(
  semilla = 123,
  proporcion_train = 0.8,
  neuronas_capa_oculta = 15,
  activacion = "relu",
  epochs = 100,
  batch_size = 16,
  validation_split = 0.2,
  verbose = 1
)
############################################################
# FIN DE CONFIGURACIÓN
############################################################

# Detectamos si está instalado keras3 o keras clásico.
usar_keras3 <- requireNamespace("keras3", quietly = TRUE)
usar_keras <- requireNamespace("keras", quietly = TRUE)
if (!usar_keras3 && !usar_keras) {
  error_claro("No está instalado keras3 ni keras. Ejecuta la instalación antes del examen.")
}

if (usar_keras3) {
  library(keras3)
} else {
  library(keras)
}
library(tensorflow)

mensaje_paso(1, "Comprobar TensorFlow")
config_tf <- tryCatch(tensorflow::tf_config(), error = function(e) e)
if (inherits(config_tf, "error")) {
  error_claro(paste0("TensorFlow no está operativo: ", conditionMessage(config_tf)))
}
print(config_tf)

# Semillas para reproducibilidad en R y TensorFlow.
set.seed(CONFIG$semilla)
try(tensorflow::set_random_seed(CONFIG$semilla), silent = TRUE)

mensaje_paso(2, "Cargar y preparar Iris")
data(iris)
X_df <- iris[, 1:4]
y_factor <- as.factor(iris$Species)

# Partición estratificada antes de escalar.
indices <- caret::createDataPartition(y_factor, p = CONFIG$proporcion_train, list = FALSE)
X_train_df <- X_df[indices, , drop = FALSE]
X_test_df <- X_df[-indices, , drop = FALSE]
y_train_factor <- y_factor[indices]
y_test_factor <- y_factor[-indices]

# Escalado usando media/desviación del entrenamiento.
medias <- vapply(X_train_df, mean, numeric(1))
desv <- vapply(X_train_df, sd, numeric(1))
X_train <- sweep(sweep(as.matrix(X_train_df), 2, medias, "-"), 2, desv, "/")
X_test <- sweep(sweep(as.matrix(X_test_df), 2, medias, "-"), 2, desv, "/")

# Keras espera índices de clase 0, 1, 2 para one-hot encoding.
y_train_int <- as.integer(y_train_factor) - 1
y_test_int <- as.integer(y_test_factor) - 1
numero_clases <- nlevels(y_factor)
y_train <- to_categorical(y_train_int, num_classes = numero_clases)
y_test <- to_categorical(y_test_int, num_classes = numero_clases)

cat("Forma X_train:", paste(dim(X_train), collapse = " x "), "\n")
cat("Forma y_train one-hot:", paste(dim(y_train), collapse = " x "), "\n")
cat("Ejemplo de etiqueta one-hot:\n")
print(y_train[1, ])

mensaje_paso(3, "Construir la red neuronal")
modelo <- keras_model_sequential() %>%
  layer_dense(
    units = CONFIG$neuronas_capa_oculta,
    activation = CONFIG$activacion,
    input_shape = ncol(X_train),
    name = "capa_oculta"
  ) %>%
  layer_dense(
    units = numero_clases,
    activation = "softmax",
    name = "salida"
  )

modelo %>% compile(
  optimizer = "adam",
  loss = "categorical_crossentropy",
  metrics = c("accuracy")
)

print(modelo)

mensaje_paso(4, "Entrenar")
historia <- modelo %>% fit(
  x = X_train,
  y = y_train,
  epochs = CONFIG$epochs,
  batch_size = CONFIG$batch_size,
  validation_split = CONFIG$validation_split,
  verbose = CONFIG$verbose
)

mensaje_paso(5, "Evaluar sobre test")
evaluacion <- modelo %>% evaluate(X_test, y_test, verbose = 0)
print(evaluacion)

probabilidades <- modelo %>% predict(X_test, verbose = 0)
clases_predichas_int <- apply(probabilidades, 1, which.max) - 1
clases_predichas <- factor(
  levels(y_factor)[clases_predichas_int + 1],
  levels = levels(y_factor)
)

cm <- caret::confusionMatrix(clases_predichas, y_test_factor)
print(cm)

mensaje_paso(6, "Graficar la historia de entrenamiento")
# Compatibilidad: las métricas pueden estar en historia$metrics.
metricas <- historia$metrics
if (is.null(metricas)) error_claro("No se encontraron métricas en el objeto history.")

epocas <- seq_along(metricas$loss)
df_hist <- data.frame(
  epoca = epocas,
  loss = metricas$loss,
  val_loss = metricas$val_loss,
  accuracy = metricas$accuracy,
  val_accuracy = metricas$val_accuracy
)

p_loss <- ggplot2::ggplot(df_hist, ggplot2::aes(epoca)) +
  ggplot2::geom_line(ggplot2::aes(y = loss, linetype = "Entrenamiento")) +
  ggplot2::geom_line(ggplot2::aes(y = val_loss, linetype = "Validación")) +
  ggplot2::theme_minimal() +
  ggplot2::labs(title = "Evolución de la pérdida", y = "Categorical cross-entropy", linetype = "Conjunto")
print(p_loss)
guardar_ggplot(p_loss, "tema9_loss.png")

p_acc <- ggplot2::ggplot(df_hist, ggplot2::aes(epoca)) +
  ggplot2::geom_line(ggplot2::aes(y = accuracy, linetype = "Entrenamiento")) +
  ggplot2::geom_line(ggplot2::aes(y = val_accuracy, linetype = "Validación")) +
  ggplot2::theme_minimal() +
  ggplot2::labs(title = "Evolución de la exactitud", y = "Accuracy", linetype = "Conjunto")
print(p_acc)
guardar_ggplot(p_acc, "tema9_accuracy.png")

guardar_tabla(df_hist, "tema9_historia_entrenamiento.csv")

cat("\nQUÉ INTERPRETAR EN EL EXAMEN\n")
cat("- X_train es una matriz/tensor de muestras por características.\n")
cat("- y_train se codifica one-hot para clasificación multiclase.\n")
cat("- ReLU introduce no linealidad en la capa oculta.\n")
cat("- Softmax genera probabilidades que suman 1.\n")
cat("- La pérdida categorical cross-entropy compara distribución real y predicha.\n")
