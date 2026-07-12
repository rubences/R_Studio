############################################################
# TEMA 3. REDUCCIÓN DE DIMENSIONALIDAD II
# LLE, Laplacian Eigenmaps, MVU, UMAP e ICA
############################################################

rm(list = ls())
source("00_UTILIDADES.R")
comprobar_directorio_proyecto()
comprobar_paquetes(c("ggplot2", "Rdimtools", "uwot", "ica"))

############################################################
# CONFIGURACIÓN: SOLO EDITAR AQUÍ
############################################################
CONFIG <- list(
  archivo_datos = file.path("data", "data.csv"),
  archivo_etiquetas = file.path("data", "labels.csv"),
  columna_etiqueta = "Class",
  columnas_excluir_datos = c("id", "ID"),
  numero_variables = 500,
  escalar = TRUE,
  semilla = 1234,
  ejecutar_lle = TRUE,
  ejecutar_le = TRUE,
  ejecutar_mvu = FALSE,   # MVU puede ser muy lento; actívalo solo si se solicita.
  ejecutar_umap = TRUE,
  ejecutar_ica = TRUE,
  proporcion_vecinos = 0.15,
  max_muestras_mvu = 120
)
############################################################
# FIN DE CONFIGURACIÓN
############################################################

mensaje_paso(1, "Cargar y preparar datos")
datos_raw <- leer_csv_seguro(CONFIG$archivo_datos)
etiquetas_raw <- leer_csv_seguro(CONFIG$archivo_etiquetas)
validar_columna(etiquetas_raw, CONFIG$columna_etiqueta, "etiquetas")
if (nrow(datos_raw) != nrow(etiquetas_raw)) error_claro("Debe existir una etiqueta por muestra.")
clase <- as.factor(etiquetas_raw[[CONFIG$columna_etiqueta]])

columnas_excluir <- CONFIG$columnas_excluir_datos
if (!is.numeric(datos_raw[[1]])) columnas_excluir <- unique(c(columnas_excluir, names(datos_raw)[1]))
X <- preparar_matriz_numerica(
  datos_raw,
  columnas_excluir = columnas_excluir,
  max_variables = CONFIG$numero_variables,
  imputar = TRUE,
  escalar = CONFIG$escalar
)
cat("Matriz final:", nrow(X), "x", ncol(X), "\n")

proporcion <- min(max(as.numeric(CONFIG$proporcion_vecinos), 0.02), 0.90)
k_vecinos <- max(2, min(round(proporcion * nrow(X)), nrow(X) - 1))
cat("Número seguro de vecinos:", k_vecinos, "\n")

plot_embedding <- function(Y, titulo, archivo, clase_local = clase) {
  Y <- as.matrix(Y)
  if (nrow(Y) != length(clase_local) || ncol(Y) < 2) {
    error_claro(paste0(titulo, " devolvió dimensiones incompatibles."))
  }
  df <- data.frame(Dim1 = Y[, 1], Dim2 = Y[, 2], Clase = clase_local)
  p <- ggplot2::ggplot(df, ggplot2::aes(Dim1, Dim2, color = Clase)) +
    ggplot2::geom_point(size = 2.6, alpha = 0.85) +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = titulo, x = "Dimensión 1", y = "Dimensión 2", color = "Grupo")
  print(p)
  guardar_ggplot(p, archivo)
  guardar_tabla(df, sub("png$", "csv", archivo))
  invisible(df)
}

# ------------------------------------------------------------------
# LLE
# ------------------------------------------------------------------
if (isTRUE(CONFIG$ejecutar_lle)) {
  mensaje_paso(2, "Locally Linear Embedding (LLE)")
  if (!requireNamespace("RDRToolbox", quietly = TRUE)) {
    mensaje_aviso("RDRToolbox no está instalado. Se omite LLE.")
  } else {
    lle <- ejecutar_seguro("LLE", RDRToolbox::LLE(X, dim = 2, k = k_vecinos))
    if (!is.null(lle)) plot_embedding(lle, paste0("LLE (k = ", k_vecinos, ")"), "tema3_lle.png")
  }
}

# ------------------------------------------------------------------
# Laplacian Eigenmaps
# ------------------------------------------------------------------
if (isTRUE(CONFIG$ejecutar_le)) {
  mensaje_paso(3, "Laplacian Eigenmaps")
  le <- ejecutar_seguro(
    "Laplacian Eigenmaps",
    Rdimtools::do.lapeig(
      X,
      ndim = 2,
      type = c("proportion", proporcion),
      weighted = FALSE
    )
  )
  if (!is.null(le)) plot_embedding(le$Y, "Laplacian Eigenmaps", "tema3_le.png")
}

# ------------------------------------------------------------------
# MVU
# ------------------------------------------------------------------
if (isTRUE(CONFIG$ejecutar_mvu)) {
  mensaje_paso(4, "Maximum Variance Unfolding (MVU)")

  # MVU requiere resolver un problema costoso. Limitamos muestras de forma explícita.
  set.seed(CONFIG$semilla)
  n_sub <- min(nrow(X), as.integer(CONFIG$max_muestras_mvu))
  indices_mvu <- if (n_sub < nrow(X)) sort(sample(seq_len(nrow(X)), n_sub)) else seq_len(nrow(X))
  X_mvu <- X[indices_mvu, , drop = FALSE]
  clase_mvu <- clase[indices_mvu]

  if (n_sub < nrow(X)) {
    mensaje_aviso(paste0("MVU se ejecutará sobre una submuestra reproducible de ", n_sub, " casos."))
  }

  mvu <- ejecutar_seguro(
    "MVU",
    Rdimtools::do.mvu(X_mvu, ndim = 2, type = c("proportion", min(proporcion, 0.30)))
  )
  if (!is.null(mvu)) plot_embedding(mvu$Y, "MVU", "tema3_mvu.png", clase_mvu)
}

# ------------------------------------------------------------------
# UMAP
# ------------------------------------------------------------------
if (isTRUE(CONFIG$ejecutar_umap)) {
  mensaje_paso(5, "UMAP")
  set.seed(CONFIG$semilla)
  umap_y <- uwot::umap(
    X,
    n_neighbors = k_vecinos,
    n_components = 2,
    min_dist = 0.1,
    metric = "euclidean",
    scale = FALSE,
    verbose = TRUE,
    ret_model = FALSE
  )
  plot_embedding(umap_y, paste0("UMAP (n_neighbors = ", k_vecinos, ")"), "tema3_umap.png")
}

# ------------------------------------------------------------------
# ICA
# ------------------------------------------------------------------
if (isTRUE(CONFIG$ejecutar_ica)) {
  mensaje_paso(6, "Independent Component Analysis (ICA)")
  set.seed(CONFIG$semilla)
  ica_resultado <- ica::ica(X, nc = 2, method = "fast")
  plot_embedding(ica_resultado$S, "ICA: componentes independientes", "tema3_ica.png")
}

cat("\nQUÉ INTERPRETAR EN EL EXAMEN\n")
cat("- LLE conserva relaciones lineales locales entre vecinos.\n")
cat("- LE utiliza el grafo de vecindad y el espectro del laplaciano.\n")
cat("- MVU despliega la variedad maximizando varianza con restricciones locales.\n")
cat("- UMAP conserva estructura local y parte de la global; depende de n_neighbors y min_dist.\n")
cat("- ICA busca componentes estadísticamente independientes.\n")
cat("- Los ejes de estos métodos no equivalen a porcentaje de varianza de PCA.\n")
