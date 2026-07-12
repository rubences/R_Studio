############################################################
# INSTALACIÓN DEL BACKEND DE KERAS/TENSORFLOW
# Ejecutar UNA SOLA VEZ en casa, con Internet.
# No ejecutar durante el examen.
############################################################

rm(list = ls())

cat("Este proceso puede descargar Python y TensorFlow y tardar varios minutos.\n")
cat("Cierra entrenamientos abiertos antes de continuar.\n\n")

if (requireNamespace("keras3", quietly = TRUE)) {
  cat("Se ha detectado keras3. Se intentará instalar su backend recomendado.\n")
  keras3::install_keras()
} else if (requireNamespace("keras", quietly = TRUE)) {
  cat("Se ha detectado keras clásico. Se intentará instalar Keras/TensorFlow.\n")
  keras::install_keras()
} else {
  stop("Instala primero el paquete R 'keras3' o 'keras'.")
}

cat("\nComprobación de TensorFlow:\n")
if (!requireNamespace("tensorflow", quietly = TRUE)) {
  stop("Falta el paquete R 'tensorflow'.")
}
print(tensorflow::tf_config())
cat("\nBACKEND KERAS/TENSORFLOW PREPARADO.\n")
