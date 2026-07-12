############################################################
# INSTALACIÓN PREVIA DE PAQUETES
# Ejecutar en casa, con Internet, NO durante el examen
############################################################

rm(list = ls())
options(repos = c(CRAN = "https://cloud.r-project.org"))

paquetes_cran <- c(
  "caret", "ggplot2", "dplyr", "tidyr", "gridExtra",
  "Rtsne", "Rdimtools", "uwot", "ica",
  "cluster", "factoextra", "dbscan", "ggdendro", "pheatmap",
  "MASS", "klaR", "mda",
  "rpart", "rpart.plot", "kernlab",
  "randomForest", "gbm", "xgboost", "ipred",
  "pROC", "PRROC", "glmnet"
)

# Instalar únicamente los paquetes que falten.
faltan_cran <- paquetes_cran[!vapply(paquetes_cran, requireNamespace, logical(1), quietly = TRUE)]
if (length(faltan_cran) > 0) {
  install.packages(faltan_cran, dependencies = TRUE)
} else {
  cat("Todos los paquetes CRAN ya estaban instalados.\n")
}

# RDRToolbox se distribuye mediante Bioconductor.
if (!requireNamespace("BiocManager", quietly = TRUE)) {
  install.packages("BiocManager")
}
if (!requireNamespace("RDRToolbox", quietly = TRUE)) {
  BiocManager::install("RDRToolbox", ask = FALSE, update = FALSE)
}

# Deep learning: intentamos usar keras3; si el curso emplea keras clásico,
# también se acepta. La instalación del backend debe hacerse antes del examen.
if (!requireNamespace("keras3", quietly = TRUE) && !requireNamespace("keras", quietly = TRUE)) {
  install.packages("keras3")
}
if (!requireNamespace("tensorflow", quietly = TRUE)) {
  install.packages("tensorflow")
}

cat("\nCOMPROBACIÓN FINAL\n")
todos <- c(paquetes_cran, "RDRToolbox", "tensorflow")
estado <- data.frame(
  paquete = todos,
  instalado = vapply(todos, requireNamespace, logical(1), quietly = TRUE)
)
print(estado)

if (any(!estado$instalado)) {
  stop("Quedan paquetes sin instalar. Revisa la tabla anterior.")
}

cat("\nPaquetes de R instalados correctamente.\n")
cat("Ahora ejecuta 00_PRUEBA_DEL_ENTORNO.R.\n")
cat("Para Keras puede ser necesario instalar el backend Python una sola vez.\n")
