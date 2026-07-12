############################################################
# PRUEBA RÁPIDA DEL ENTORNO
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

mensaje_paso(1, "Comprobar paquetes esenciales")
comprobar_paquetes(c("caret", "ggplot2", "MASS", "pROC", "cluster"))
mensaje_ok("Paquetes esenciales disponibles.")

mensaje_paso(2, "Probar una clasificación sencilla")
data(iris)
iris$Species <- as.factor(iris$Species)
set.seed(1995)
idx <- caret::createDataPartition(iris$Species, p = 0.8, list = FALSE)
train <- iris[idx, ]
test <- iris[-idx, ]
modelo <- MASS::lda(Species ~ ., data = train)
pred <- predict(modelo, newdata = test)$class
print(caret::confusionMatrix(pred, test$Species))
mensaje_ok("LDA ejecutado correctamente.")

mensaje_paso(3, "Comprobar backend de redes neuronales")
keras_disponible <- requireNamespace("keras3", quietly = TRUE) || requireNamespace("keras", quietly = TRUE)
if (!keras_disponible) {
  mensaje_aviso("Keras no está instalado. Los temas 9 y 10 no funcionarán hasta instalarlo.")
} else {
  cat("Keras está instalado. Se comprobará TensorFlow.\n")
  if (!requireNamespace("tensorflow", quietly = TRUE)) {
    mensaje_aviso("Falta el paquete tensorflow.")
  } else {
    version_tf <- tryCatch(tensorflow::tf_config(), error = function(e) e)
    if (inherits(version_tf, "error")) {
      mensaje_aviso(paste0("TensorFlow no está operativo: ", conditionMessage(version_tf)))
    } else {
      print(version_tf)
      mensaje_ok("Backend TensorFlow detectado.")
    }
  }
}

cat("\n============================================================\n")
cat("ENTORNO PREPARADO PARA LOS TEMAS 1 A 8.\n")
cat("Revisa el aviso anterior para confirmar los temas 9 y 10.\n")
cat("============================================================\n")
