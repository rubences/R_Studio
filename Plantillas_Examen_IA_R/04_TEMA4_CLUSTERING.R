############################################################
# TEMA 4. CLUSTERING
# K-means, PAM, jerárquico y DBSCAN
############################################################

rm(list = ls())
source("00_UTILIDADES.R")
comprobar_directorio_proyecto()
comprobar_paquetes(c("ggplot2", "cluster", "factoextra"))

############################################################
# CONFIGURACIÓN: SOLO EDITAR AQUÍ
############################################################
CONFIG <- list(
  archivo = file.path("data", "data.csv"),
  columna_etiqueta = NULL,       # Ej.: "Class". NULL si no hay etiqueta.
  columnas_excluir = c("id", "ID"),
  numero_variables = 50,
  usar_iris_si_falta = TRUE,
  numero_clusters = 3,
  semilla = 1234,
  ejecutar_kmeans = TRUE,
  ejecutar_pam = TRUE,
  ejecutar_jerarquico = TRUE,
  ejecutar_dbscan = TRUE,
  eps_dbscan = NULL,             # NULL = estimación orientativa automática.
  minPts_dbscan = NULL
)
############################################################
# FIN DE CONFIGURACIÓN
############################################################

mensaje_paso(1, "Cargar y preparar datos")
if (file.exists(CONFIG$archivo)) {
  datos <- leer_csv_seguro(CONFIG$archivo)
} else if (isTRUE(CONFIG$usar_iris_si_falta)) {
  mensaje_aviso("No se encontró el CSV. Se usará iris como ejemplo.")
  datos <- iris
  CONFIG$columna_etiqueta <- "Species"
  CONFIG$columnas_excluir <- character(0)
} else {
  datos <- leer_csv_seguro(CONFIG$archivo)
}

etiqueta_real <- NULL
if (!is.null(CONFIG$columna_etiqueta)) {
  validar_columna(datos, CONFIG$columna_etiqueta)
  etiqueta_real <- as.factor(datos[[CONFIG$columna_etiqueta]])
  cat("Etiquetas reales disponibles únicamente para comparación posterior.\n")
}

X <- preparar_matriz_numerica(
  datos,
  columnas_excluir = unique(c(CONFIG$columnas_excluir, CONFIG$columna_etiqueta)),
  max_variables = CONFIG$numero_variables,
  imputar = TRUE,
  escalar = TRUE
)

k <- max(2, min(as.integer(CONFIG$numero_clusters), nrow(X) - 1))
cat("Se utilizarán", k, "clústeres.\n")

# PCA solo para representar los clústeres en dos dimensiones.
pca_vis <- prcomp(X, center = FALSE, scale. = FALSE)
coords <- pca_vis$x[, 1:2, drop = FALSE]

plot_clusters <- function(cluster, titulo, archivo) {
  df <- data.frame(PC1 = coords[, 1], PC2 = coords[, 2], Cluster = as.factor(cluster))
  if (!is.null(etiqueta_real)) df$EtiquetaReal <- etiqueta_real

  p <- ggplot2::ggplot(df, ggplot2::aes(PC1, PC2, color = Cluster)) +
    ggplot2::geom_point(size = 2.6, alpha = 0.85) +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = titulo, subtitle = "Representación PCA; el clustering se calculó con todas las variables")
  print(p)
  guardar_ggplot(p, archivo)
  invisible(df)
}

comparar_con_etiqueta <- function(cluster, nombre) {
  if (!is.null(etiqueta_real)) {
    cat("\nTabla clúster frente a etiqueta real para", nombre, ":\n")
    print(table(Cluster = cluster, Etiqueta = etiqueta_real))
    cat("IMPORTANTE: la numeración de los clústeres es arbitraria.\n")
  }
}

# ------------------------------------------------------------------
# Selección orientativa de k mediante WSS y silueta
# ------------------------------------------------------------------
mensaje_paso(2, "Diagnóstico del número de clústeres")
max_k <- min(10, nrow(X) - 1)
if (max_k >= 2) {
  set.seed(CONFIG$semilla)
  wss <- vapply(1:max_k, function(k_i) {
    kmeans(X, centers = k_i, nstart = 20, iter.max = 100)$tot.withinss
  }, numeric(1))
  tabla_wss <- data.frame(k = 1:max_k, WSS = wss)
  p_wss <- ggplot2::ggplot(tabla_wss, ggplot2::aes(k, WSS)) +
    ggplot2::geom_line() + ggplot2::geom_point() +
    ggplot2::scale_x_continuous(breaks = 1:max_k) +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = "Método del codo", x = "Número de clústeres")
  print(p_wss)
  guardar_ggplot(p_wss, "tema4_metodo_codo.png")
}

# ------------------------------------------------------------------
# K-means
# ------------------------------------------------------------------
if (isTRUE(CONFIG$ejecutar_kmeans)) {
  mensaje_paso(3, "K-means")
  set.seed(CONFIG$semilla)
  km <- kmeans(X, centers = k, nstart = 50, iter.max = 200)
  print(km)
  cat("Tamaño de cada clúster:\n")
  print(table(km$cluster))
  cat("Proporción de variabilidad entre clústeres:",
      round(km$betweenss / km$totss, 4), "\n")

  sil_km <- cluster::silhouette(km$cluster, dist(X))
  cat("Silueta media K-means:", round(mean(sil_km[, "sil_width"]), 4), "\n")

  df_km <- plot_clusters(km$cluster, paste0("K-means, k = ", k), "tema4_kmeans.png")
  guardar_tabla(df_km, "tema4_kmeans_asignaciones.csv")
  comparar_con_etiqueta(km$cluster, "K-means")
}

# ------------------------------------------------------------------
# PAM / k-medoids
# ------------------------------------------------------------------
if (isTRUE(CONFIG$ejecutar_pam)) {
  mensaje_paso(4, "PAM o k-medoids")
  pam_resultado <- cluster::pam(X, k = k, stand = FALSE)
  cat("Silueta media PAM:", round(pam_resultado$silinfo$avg.width, 4), "\n")
  df_pam <- plot_clusters(pam_resultado$clustering, paste0("PAM, k = ", k), "tema4_pam.png")
  guardar_tabla(df_pam, "tema4_pam_asignaciones.csv")
  comparar_con_etiqueta(pam_resultado$clustering, "PAM")
}

# ------------------------------------------------------------------
# Clustering jerárquico aglomerativo
# ------------------------------------------------------------------
if (isTRUE(CONFIG$ejecutar_jerarquico)) {
  mensaje_paso(5, "Clustering jerárquico aglomerativo")
  D <- dist(X, method = "euclidean")
  metodos <- c("single", "complete", "average", "ward.D2")

  resultados_h <- list()
  for (metodo in metodos) {
    cat("\nMétodo de enlace:", metodo, "\n")
    hc <- hclust(D, method = metodo)
    grupos <- cutree(hc, k = k)
    resultados_h[[metodo]] <- list(modelo = hc, grupos = grupos)

    png(file.path("resultados", paste0("tema4_dendrograma_", gsub("\\.", "_", metodo), ".png")),
        width = 1200, height = 800, res = 140)
    plot(hc, labels = FALSE, hang = -1, main = paste("Dendrograma", metodo), xlab = "Muestras")
    rect.hclust(hc, k = k, border = 2:6)
    dev.off()

    comparar_con_etiqueta(grupos, paste("jerárquico", metodo))
  }

  # Usamos Ward.D2 para una representación de ejemplo.
  grupos_ward <- resultados_h[["ward.D2"]]$grupos
  df_ward <- plot_clusters(grupos_ward, paste0("Jerárquico Ward.D2, k = ", k), "tema4_ward.png")
  guardar_tabla(df_ward, "tema4_ward_asignaciones.csv")
}

# ------------------------------------------------------------------
# Clustering jerárquico divisivo (DIANA)
# ------------------------------------------------------------------
mensaje_paso(6, "Clustering jerárquico divisivo DIANA")
diana_modelo <- cluster::diana(X, metric = "euclidean", stand = FALSE)
grupo_diana <- cutree(as.hclust(diana_modelo), k = k)
cat("Coeficiente divisivo:", round(diana_modelo$dc, 4), "\n")
comparar_con_etiqueta(grupo_diana, "DIANA")

# ------------------------------------------------------------------
# DBSCAN
# ------------------------------------------------------------------
if (isTRUE(CONFIG$ejecutar_dbscan)) {
  mensaje_paso(7, "DBSCAN")
  if (!requireNamespace("dbscan", quietly = TRUE)) {
    mensaje_aviso("Falta el paquete dbscan; se omite este bloque.")
  } else {
    minPts <- CONFIG$minPts_dbscan %||% max(4, 2 * ncol(X))
    minPts <- min(as.integer(minPts), max(2, nrow(X) - 1))

    # Estimación orientativa: percentil 90 de la distancia al minPts-ésimo vecino.
    if (is.null(CONFIG$eps_dbscan)) {
      vecinos <- dbscan::kNNdist(X, k = minPts)
      eps <- as.numeric(stats::quantile(vecinos, probs = 0.90, na.rm = TRUE))
      mensaje_aviso(paste0("eps estimado automáticamente en ", round(eps, 4), ". Revísalo con el gráfico kNNdist."))
    } else {
      eps <- as.numeric(CONFIG$eps_dbscan)
    }

    db <- dbscan::dbscan(X, eps = eps, minPts = minPts)
    cat("Parámetros: eps =", eps, ", minPts =", minPts, "\n")
    print(db)
    cat("El clúster 0 representa ruido.\n")

    df_db <- plot_clusters(db$cluster, paste0("DBSCAN, eps = ", round(eps, 3)), "tema4_dbscan.png")
    guardar_tabla(df_db, "tema4_dbscan_asignaciones.csv")
    comparar_con_etiqueta(db$cluster, "DBSCAN")
  }
}

cat("\nQUÉ INTERPRETAR EN EL EXAMEN\n")
cat("- K-means y PAM necesitan fijar k.\n")
cat("- K-means usa centroides; PAM usa medoides y suele ser más robusto a atípicos.\n")
cat("- El dendrograma muestra fusiones; el método de enlace cambia el resultado.\n")
cat("- DBSCAN detecta grupos por densidad y puede marcar ruido como clúster 0.\n")
cat("- La silueta resume cohesión interna y separación entre grupos.\n")
