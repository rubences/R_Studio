############################################################
# PLANTILLA MAESTRA NO SUPERVISADA
# Carga -> limpieza -> PCA/UMAP -> clustering -> interpretación
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
comprobar_paquetes(c("ggplot2", "uwot", "cluster"))

############################################################
# CONFIGURACIÓN: SOLO EDITAR AQUÍ
############################################################
CONFIG <- list(
  archivo_datos = file.path("data", "data.csv"),
  archivo_etiquetas = file.path("data", "labels.csv"),
  columna_etiqueta = "Class",
  columnas_excluir = c("id", "ID"),
  max_variables = 500,
  numero_clusters = 5,
  semilla = 1234,
  usar_etiquetas_solo_para_visualizar = TRUE
)
############################################################
# FIN DE CONFIGURACIÓN
############################################################

mensaje_paso(1, "Cargar datos")
raw <- leer_csv_seguro(CONFIG$archivo_datos)

clase <- NULL
if (file.exists(CONFIG$archivo_etiquetas)) {
  labels <- leer_csv_seguro(CONFIG$archivo_etiquetas)
  validar_columna(labels, CONFIG$columna_etiqueta, "labels")
  if (nrow(labels) != nrow(raw)) error_claro("Número de etiquetas incompatible.")
  clase <- as.factor(labels[[CONFIG$columna_etiqueta]])
}

mensaje_paso(2, "Crear matriz numérica estandarizada")
excluir <- CONFIG$columnas_excluir
if (!is.numeric(raw[[1]])) excluir <- unique(c(excluir, names(raw)[1]))
X <- preparar_matriz_numerica(raw, excluir, CONFIG$max_variables, imputar = TRUE, escalar = TRUE)

mensaje_paso(3, "PCA")
pca <- prcomp(X, center = FALSE, scale. = FALSE)
var_exp <- pca$sdev^2 / sum(pca$sdev^2)
n90 <- which(cumsum(var_exp) >= 0.90)[1]
cat("Componentes para 90 % de varianza:", n90, "\n")

mensaje_paso(4, "UMAP")
set.seed(CONFIG$semilla)
n_neighbors <- max(2, min(round(0.15 * nrow(X)), nrow(X) - 1))
umap_y <- uwot::umap(X, n_neighbors = n_neighbors, n_components = 2, min_dist = 0.1, verbose = TRUE)

mensaje_paso(5, "K-means")
k <- max(2, min(as.integer(CONFIG$numero_clusters), nrow(X) - 1))
set.seed(CONFIG$semilla)
km <- kmeans(X, centers = k, nstart = 50)
sil <- cluster::silhouette(km$cluster, dist(X))
cat("Silueta media:", mean(sil[, "sil_width"]), "\n")

mensaje_paso(6, "Visualizar")
df <- data.frame(
  PCA1 = pca$x[, 1], PCA2 = pca$x[, 2],
  UMAP1 = umap_y[, 1], UMAP2 = umap_y[, 2],
  Cluster = factor(km$cluster)
)
if (!is.null(clase)) df$EtiquetaReal <- clase

p_pca <- ggplot2::ggplot(df, ggplot2::aes(PCA1, PCA2, color = Cluster)) +
  ggplot2::geom_point(size = 2.5) +
  ggplot2::theme_minimal() +
  ggplot2::labs(
    title = "Clústeres sobre PCA",
    x = sprintf("PC1 (%.2f%%)", 100 * var_exp[1]),
    y = sprintf("PC2 (%.2f%%)", 100 * var_exp[2])
  )
print(p_pca)
guardar_ggplot(p_pca, "maestra_no_supervisado_pca_clusters.png")

p_umap <- ggplot2::ggplot(df, ggplot2::aes(UMAP1, UMAP2, color = Cluster)) +
  ggplot2::geom_point(size = 2.5) +
  ggplot2::theme_minimal() +
  ggplot2::labs(title = "Clústeres sobre UMAP")
print(p_umap)
guardar_ggplot(p_umap, "maestra_no_supervisado_umap_clusters.png")

guardar_tabla(df, "maestra_no_supervisado_coordenadas_clusters.csv")

if (!is.null(clase)) {
  cat("\nComparación clúster-etiqueta, solo posterior al clustering:\n")
  print(table(Cluster = km$cluster, Etiqueta = clase))
}

cat("\nRESPUESTA MODELO PARA EL EXAMEN\n")
cat("Las variables se imputaron, se eliminaron variables constantes y se estandarizaron.\n")
cat("PCA permitió cuantificar la varianza explicada; UMAP permitió visualizar estructura no lineal.\n")
cat("K-means se calculó sin utilizar etiquetas y la calidad interna se resumió mediante silueta.\n")
