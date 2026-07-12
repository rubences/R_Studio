############################################################
# TEMA 2. REDUCCIÓN DE DIMENSIONALIDAD I
# PCA, MDS, Isomap y t-SNE
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
comprobar_paquetes(c("ggplot2", "Rtsne"))

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
  ejecutar_pca = TRUE,
  ejecutar_mds = TRUE,
  ejecutar_isomap = TRUE,
  ejecutar_tsne = TRUE,
  vecinos_isomap = 5,
  perplexity_tsne = 30
)
############################################################
# FIN DE CONFIGURACIÓN
############################################################

mensaje_paso(1, "Cargar datos y etiquetas")
datos_raw <- leer_csv_seguro(CONFIG$archivo_datos)
etiquetas_raw <- leer_csv_seguro(CONFIG$archivo_etiquetas)
validar_columna(etiquetas_raw, CONFIG$columna_etiqueta, "etiquetas")

if (nrow(datos_raw) != nrow(etiquetas_raw)) {
  error_claro(
    paste0(
      "data.csv tiene ", nrow(datos_raw), " filas y labels.csv tiene ",
      nrow(etiquetas_raw), ". Debe existir una etiqueta por muestra."
    )
  )
}

clase <- as.factor(etiquetas_raw[[CONFIG$columna_etiqueta]])
cat("Distribución de etiquetas:\n")
print(table(clase))

mensaje_paso(2, "Preparar la matriz numérica")
# Además de identificadores conocidos, excluimos automáticamente la primera columna
# si no es numérica. Así evitamos incluir nombres de muestra en el análisis.
columnas_excluir <- CONFIG$columnas_excluir_datos
if (!is.numeric(datos_raw[[1]])) columnas_excluir <- unique(c(columnas_excluir, names(datos_raw)[1]))

X <- preparar_matriz_numerica(
  datos_raw,
  columnas_excluir = columnas_excluir,
  max_variables = CONFIG$numero_variables,
  imputar = TRUE,
  escalar = CONFIG$escalar
)
cat("Matriz final:", nrow(X), "muestras x", ncol(X), "variables\n")

# Función común para dibujar cualquier embedding bidimensional.
graficar_embedding <- function(coordenadas, titulo, eje_x = "Dimensión 1", eje_y = "Dimensión 2", archivo) {
  df_plot <- data.frame(
    Dim1 = coordenadas[, 1],
    Dim2 = coordenadas[, 2],
    Clase = clase
  )
  p <- ggplot2::ggplot(df_plot, ggplot2::aes(Dim1, Dim2, color = Clase)) +
    ggplot2::geom_point(size = 2.6, alpha = 0.85) +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = titulo, x = eje_x, y = eje_y, color = "Grupo")
  print(p)
  guardar_ggplot(p, archivo)
  invisible(df_plot)
}

# ------------------------------------------------------------------
# PCA
# ------------------------------------------------------------------
if (isTRUE(CONFIG$ejecutar_pca)) {
  mensaje_paso(3, "PCA: análisis de componentes principales")

  pca <- stats::prcomp(X, center = FALSE, scale. = FALSE)
  varianza <- pca$sdev^2
  prop_varianza <- varianza / sum(varianza)
  acumulada <- cumsum(prop_varianza)
  numero_90 <- which(acumulada >= 0.90)[1]

  tabla_varianza <- data.frame(
    componente = seq_along(prop_varianza),
    proporcion = prop_varianza,
    porcentaje = 100 * prop_varianza,
    acumulada = acumulada,
    acumulada_porcentaje = 100 * acumulada
  )
  print(head(tabla_varianza, 10))
  cat("Componentes necesarias para explicar al menos el 90 %:", numero_90, "\n")
  guardar_tabla(tabla_varianza, "tema2_pca_varianza.csv")

  eje_x <- sprintf("PC1 (%.2f%%)", 100 * prop_varianza[1])
  eje_y <- sprintf("PC2 (%.2f%%)", 100 * prop_varianza[2])
  pca_df <- graficar_embedding(
    pca$x[, 1:2, drop = FALSE],
    "PCA: primeras dos componentes",
    eje_x, eje_y,
    "tema2_pca.png"
  )
  guardar_tabla(pca_df, "tema2_pca_coordenadas.csv")

  p_scree <- ggplot2::ggplot(head(tabla_varianza, min(20, nrow(tabla_varianza))),
                             ggplot2::aes(componente, porcentaje)) +
    ggplot2::geom_col() +
    ggplot2::geom_line(ggplot2::aes(y = acumulada_porcentaje, group = 1)) +
    ggplot2::geom_point(ggplot2::aes(y = acumulada_porcentaje)) +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = "Scree plot y varianza acumulada", y = "Porcentaje")
  print(p_scree)
  guardar_ggplot(p_scree, "tema2_pca_scree.png")
}

# ------------------------------------------------------------------
# MDS clásico
# ------------------------------------------------------------------
if (isTRUE(CONFIG$ejecutar_mds)) {
  mensaje_paso(4, "MDS: escalamiento multidimensional clásico")

  distancias <- stats::dist(X, method = "euclidean")
  mds <- stats::cmdscale(distancias, k = 2, eig = TRUE, x.ret = TRUE)

  # cmdscale puede devolver autovalores negativos en datos no perfectamente euclídeos.
  autovalores_positivos <- pmax(mds$eig, 0)
  prop_mds <- autovalores_positivos / sum(autovalores_positivos)
  cat("Proporción aproximada asociada a Dim1 y Dim2:",
      round(prop_mds[1], 4), round(prop_mds[2], 4), "\n")

  mds_df <- graficar_embedding(
    mds$points,
    "MDS clásico",
    "Dimensión 1", "Dimensión 2",
    "tema2_mds.png"
  )
  guardar_tabla(mds_df, "tema2_mds_coordenadas.csv")
}

# ------------------------------------------------------------------
# Isomap
# ------------------------------------------------------------------
if (isTRUE(CONFIG$ejecutar_isomap)) {
  mensaje_paso(5, "Isomap")

  if (!requireNamespace("RDRToolbox", quietly = TRUE)) {
    mensaje_aviso("RDRToolbox no está instalado. Se omite Isomap.")
  } else {
    k_iso <- max(2, min(as.integer(CONFIG$vecinos_isomap), nrow(X) - 1))
    cat("Vecinos utilizados por Isomap:", k_iso, "\n")

    resultado_iso <- ejecutar_seguro(
      "Isomap",
      RDRToolbox::isomap(data = X, dims = 1:2, k = k_iso, plotResiduals = FALSE)
    )

    if (!is.null(resultado_iso)) {
      # En los materiales, la salida bidimensional se encuentra en $dim2.
      coordenadas_iso <- resultado_iso$dim2 %||% resultado_iso$Y %||% resultado_iso
      coordenadas_iso <- as.matrix(coordenadas_iso)
      if (ncol(coordenadas_iso) < 2) error_claro("Isomap no devolvió dos dimensiones.")
      iso_df <- graficar_embedding(
        coordenadas_iso[, 1:2, drop = FALSE],
        paste0("Isomap (k = ", k_iso, ")"),
        "Dimensión 1", "Dimensión 2",
        "tema2_isomap.png"
      )
      guardar_tabla(iso_df, "tema2_isomap_coordenadas.csv")
    }
  }
}

# ------------------------------------------------------------------
# t-SNE
# ------------------------------------------------------------------
if (isTRUE(CONFIG$ejecutar_tsne)) {
  mensaje_paso(6, "t-SNE")
  set.seed(CONFIG$semilla)

  # Rtsne exige perplexity < (n - 1) / 3.
  limite_perplexity <- max(1, floor((nrow(X) - 1) / 3) - 1)
  perplexity_segura <- min(as.numeric(CONFIG$perplexity_tsne), limite_perplexity)
  if (perplexity_segura != CONFIG$perplexity_tsne) {
    mensaje_aviso(paste0("Perplexity reducida automáticamente a ", perplexity_segura, "."))
  }

  tsne <- Rtsne::Rtsne(
    X,
    dims = 2,
    perplexity = perplexity_segura,
    check_duplicates = FALSE,
    pca = TRUE,
    verbose = TRUE
  )

  tsne_df <- graficar_embedding(
    tsne$Y,
    paste0("t-SNE (perplexity = ", perplexity_segura, ")"),
    "Dimensión 1", "Dimensión 2",
    "tema2_tsne.png"
  )
  guardar_tabla(tsne_df, "tema2_tsne_coordenadas.csv")
}

cat("\nQUÉ INTERPRETAR EN EL EXAMEN\n")
cat("- PCA es lineal y permite cuantificar varianza explicada.\n")
cat("- MDS intenta conservar distancias entre muestras.\n")
cat("- Isomap conserva distancias geodésicas sobre una variedad.\n")
cat("- t-SNE prioriza vecindades locales; sus ejes no tienen significado directo.\n")
cat("- Las etiquetas se usan solo para colorear, no para calcular la reducción.\n")
