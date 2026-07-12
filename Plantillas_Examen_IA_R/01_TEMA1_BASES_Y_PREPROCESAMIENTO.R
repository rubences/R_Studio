############################################################
# TEMA 1. BASES DE R Y PREPROCESAMIENTO
############################################################

rm(list = ls())
source("00_UTILIDADES.R")
comprobar_directorio_proyecto()

############################################################
# CONFIGURACIÓN: SOLO EDITAR AQUÍ
############################################################
CONFIG <- list(
  archivo = file.path("data", "dataset_expresiongenes_cancer.csv"),
  separador = ",",
  decimal = ".",
  columna_objetivo = "primaryormetastasis",
  columnas_excluir = c("id"),
  usar_ejemplo_si_falta = TRUE
)
############################################################
# FIN DE CONFIGURACIÓN
############################################################

mensaje_paso(1, "Cargar los datos")

if (file.exists(CONFIG$archivo)) {
  datos <- leer_csv_seguro(CONFIG$archivo, sep = CONFIG$separador, dec = CONFIG$decimal)
} else if (isTRUE(CONFIG$usar_ejemplo_si_falta)) {
  mensaje_aviso("No se encontró el CSV. Se utilizará iris únicamente como demostración.")
  datos <- iris
  CONFIG$columna_objetivo <- "Species"
  CONFIG$columnas_excluir <- character(0)
} else {
  datos <- leer_csv_seguro(CONFIG$archivo, sep = CONFIG$separador, dec = CONFIG$decimal)
}

mensaje_paso(2, "Inspeccionar estructura y calidad")
inspeccionar_datos(datos, "datos")

cat("\nNombres de columnas:\n")
print(names(datos))

cat("\nResumen estadístico:\n")
print(summary(datos))

# Contamos NA por columna y ordenamos de mayor a menor.
na_por_columna <- sort(colSums(is.na(datos)), decreasing = TRUE)
print(na_por_columna[na_por_columna > 0])

# Contamos filas duplicadas.
filas_duplicadas <- duplicated(datos)
cat("Filas duplicadas detectadas:", sum(filas_duplicadas), "\n")

mensaje_paso(3, "Crear una copia limpia sin modificar los datos originales")
datos_limpios <- datos

# Eliminamos duplicados completos. Si el examen no lo pide, puedes comentar esta línea.
datos_limpios <- datos_limpios[!duplicated(datos_limpios), , drop = FALSE]

# Convertimos cadenas vacías en NA solo en columnas de texto.
for (nombre in names(datos_limpios)) {
  if (is.character(datos_limpios[[nombre]])) {
    datos_limpios[[nombre]][trimws(datos_limpios[[nombre]]) == ""] <- NA_character_
  }
}

# Imputamos columnas numéricas con su mediana.
columnas_numericas <- names(datos_limpios)[vapply(datos_limpios, is.numeric, logical(1))]
for (nombre in columnas_numericas) {
  if (anyNA(datos_limpios[[nombre]])) {
    mediana <- median(datos_limpios[[nombre]], na.rm = TRUE)
    datos_limpios[[nombre]][is.na(datos_limpios[[nombre]])] <- mediana
  }
}

# Si existe objetivo, se convierte a factor.
if (CONFIG$columna_objetivo %in% names(datos_limpios)) {
  datos_limpios[[CONFIG$columna_objetivo]] <- as.factor(datos_limpios[[CONFIG$columna_objetivo]])
  cat("\nDistribución de clases:\n")
  print(table(datos_limpios[[CONFIG$columna_objetivo]], useNA = "ifany"))
  cat("\nProporciones de clase:\n")
  print(prop.table(table(datos_limpios[[CONFIG$columna_objetivo]])))
}

mensaje_paso(4, "Seleccionar predictores numéricos")
matriz_x <- preparar_matriz_numerica(
  datos_limpios,
  columnas_excluir = unique(c(CONFIG$columnas_excluir, CONFIG$columna_objetivo)),
  max_variables = NULL,
  imputar = TRUE,
  escalar = FALSE
)

cat("Matriz de predictores:", nrow(matriz_x), "filas x", ncol(matriz_x), "columnas\n")

mensaje_paso(5, "Calcular estadísticos básicos")
media_variables <- colMeans(matriz_x)
desviacion_variables <- apply(matriz_x, 2, sd)
varianza_variables <- apply(matriz_x, 2, var)

estadisticos <- data.frame(
  variable = colnames(matriz_x),
  media = media_variables,
  desviacion = desviacion_variables,
  varianza = varianza_variables
)
estadisticos <- estadisticos[order(estadisticos$varianza, decreasing = TRUE), ]
print(head(estadisticos, 10))
guardar_tabla(estadisticos, "tema1_estadisticos_variables.csv")

mensaje_paso(6, "Estandarizar los predictores")
matriz_x_escalada <- scale(matriz_x)
cat("Media aproximada tras escalar (primeras variables):\n")
print(round(colMeans(matriz_x_escalada)[1:min(5, ncol(matriz_x_escalada))], 6))
cat("Desviación aproximada tras escalar:\n")
print(round(apply(matriz_x_escalada, 2, sd)[1:min(5, ncol(matriz_x_escalada))], 6))

mensaje_paso(7, "Representación gráfica básica")
comprobar_paquetes("ggplot2")

# Elegimos las dos primeras variables numéricas para un diagrama de dispersión.
if (ncol(matriz_x) >= 2) {
  df_grafico <- data.frame(
    X1 = matriz_x[, 1],
    X2 = matriz_x[, 2]
  )
  if (CONFIG$columna_objetivo %in% names(datos_limpios)) {
    df_grafico$Clase <- datos_limpios[[CONFIG$columna_objetivo]]
    grafico <- ggplot2::ggplot(df_grafico, ggplot2::aes(X1, X2, color = Clase)) +
      ggplot2::geom_point(size = 2, alpha = 0.8) +
      ggplot2::theme_minimal() +
      ggplot2::labs(
        title = "Relación entre las dos primeras variables numéricas",
        x = colnames(matriz_x)[1],
        y = colnames(matriz_x)[2]
      )
  } else {
    grafico <- ggplot2::ggplot(df_grafico, ggplot2::aes(X1, X2)) +
      ggplot2::geom_point(size = 2, alpha = 0.8) +
      ggplot2::theme_minimal()
  }
  print(grafico)
  guardar_ggplot(grafico, "tema1_dispersion_basica.png")
}

mensaje_paso(8, "Guardar la copia limpia")
guardar_tabla(datos_limpios, "tema1_datos_limpios.csv")

cat("\nQUÉ INTERPRETAR EN EL EXAMEN\n")
cat("1. Número de filas, columnas y tipos de variables.\n")
cat("2. Existencia de NA, duplicados y variables constantes.\n")
cat("3. Por qué se convierte el objetivo en factor.\n")
cat("4. Por qué se escala antes de algoritmos basados en distancias.\n")
