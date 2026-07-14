############################################################
# 00_UTILIDADES.R
# Funciones comunes para todas las plantillas del examen
############################################################

# Este archivo NO ejecuta análisis por sí solo.
# Las demás plantillas lo cargan mediante source("00_UTILIDADES.R").

# ------------------------------------------------------------------
# Operador auxiliar: devuelve y cuando x es NULL
# ------------------------------------------------------------------
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

# ------------------------------------------------------------------
# Mensajes visuales para saber en qué paso estamos
# ------------------------------------------------------------------
mensaje_paso <- function(numero, texto) {
  cat("\n", paste(rep("=", 72), collapse = ""), "\n", sep = "")
  cat(sprintf("PASO %s: %s\n", numero, texto))
  cat(paste(rep("=", 72), collapse = ""), "\n", sep = "")
}

mensaje_ok <- function(texto) {
  cat(sprintf("[OK] %s\n", texto))
}

mensaje_aviso <- function(texto) {
  warning(texto, call. = FALSE, immediate. = TRUE)
}

error_claro <- function(texto) {
  stop(paste0("\nERROR: ", texto), call. = FALSE)
}

# ------------------------------------------------------------------
# Comprueba que el proyecto se abrió correctamente
# ------------------------------------------------------------------
comprobar_directorio_proyecto <- function() {
  archivo_clave <- "00_UTILIDADES.R"
  if (!file.exists(archivo_clave)) {
    error_claro(
      paste0(
        "No se encuentra ", archivo_clave, ". ",
        "Abre Plantillas_Examen_IA_R.Rproj con RStudio antes de ejecutar. ",
        "Directorio actual: ", normalizePath(getwd(), winslash = "/", mustWork = FALSE)
      )
    )
  }
  dir.create("resultados", showWarnings = FALSE, recursive = TRUE)
  dir.create("data", showWarnings = FALSE, recursive = TRUE)
  invisible(TRUE)
}

# ------------------------------------------------------------------
# Comprueba paquetes sin instalarlos durante el examen
# ------------------------------------------------------------------
comprobar_paquetes <- function(paquetes, detener = TRUE) {
  paquetes <- unique(paquetes)
  faltan <- paquetes[!vapply(paquetes, requireNamespace, logical(1), quietly = TRUE)]

  if (length(faltan) > 0) {
    texto <- paste0(
      "Faltan estos paquetes: ", paste(faltan, collapse = ", "), ". ",
      "Ejecuta 00_INSTALAR_Y_COMPROBAR_PAQUETES.R antes del examen."
    )
    if (detener) error_claro(texto) else mensaje_aviso(texto)
  }

  invisible(faltan)
}

# ------------------------------------------------------------------
# Construye una ruta dentro del proyecto
# ------------------------------------------------------------------
ruta_proyecto <- function(...) {
  file.path(...)
}

# ------------------------------------------------------------------
# Lectura segura de CSV
# ------------------------------------------------------------------
leer_csv_seguro <- function(ruta,
                            sep = ",",
                            dec = ".",
                            stringsAsFactors = FALSE,
                            permitir_elegir = TRUE) {
  ruta_final <- ruta

  # Si la ruta no existe y estamos en RStudio, permitimos elegir el archivo.
  if (!file.exists(ruta_final) && permitir_elegir && interactive()) {
    cat("No se encontró: ", ruta_final, "\n", sep = "")
    cat("Se abrirá una ventana para que selecciones el CSV correcto.\n")
    ruta_final <- file.choose()
  }

  if (!file.exists(ruta_final)) {
    error_claro(
      paste0(
        "No se encuentra el archivo '", ruta_final, "'. ",
        "Cópialo en la carpeta data/ o corrige el nombre en CONFIGURACIÓN."
      )
    )
  }

  objeto <- tryCatch(
    read.csv(
      ruta_final,
      sep = sep,
      dec = dec,
      stringsAsFactors = stringsAsFactors,
      check.names = FALSE
    ),
    error = function(e) {
      error_claro(paste0("No se pudo leer el CSV. Detalle: ", conditionMessage(e)))
    }
  )

  if (nrow(objeto) == 0) error_claro("El CSV no contiene filas.")
  if (ncol(objeto) == 0) error_claro("El CSV no contiene columnas.")

  attr(objeto, "ruta_origen") <- ruta_final
  mensaje_ok(sprintf("CSV leído: %d filas y %d columnas.", nrow(objeto), ncol(objeto)))
  objeto
}

# ------------------------------------------------------------------
# Inspección básica de un data frame
# ------------------------------------------------------------------
inspeccionar_datos <- function(df, nombre = "datos") {
  cat("\nResumen de ", nombre, ":\n", sep = "")
  cat("- Dimensiones: ", nrow(df), " filas x ", ncol(df), " columnas\n", sep = "")
  cat("- Filas duplicadas: ", sum(duplicated(df)), "\n", sep = "")
  cat("- Valores ausentes: ", sum(is.na(df)), "\n", sep = "")
  cat("- Tipos de columnas:\n")
  print(table(vapply(df, function(x) class(x)[1], character(1))))
  cat("\nPrimeras filas:\n")
  print(utils::head(df))
  invisible(df)
}

# ------------------------------------------------------------------
# Valida que exista una columna
# ------------------------------------------------------------------
validar_columna <- function(df, columna, nombre_objeto = "datos") {
  if (!columna %in% names(df)) {
    error_claro(
      paste0(
        "La columna '", columna, "' no existe en ", nombre_objeto, ". ",
        "Columnas disponibles: ", paste(names(df), collapse = ", ")
      )
    )
  }
  invisible(TRUE)
}

# ------------------------------------------------------------------
# Convierte la variable objetivo a factor y comprueba sus clases
# ------------------------------------------------------------------
preparar_objetivo <- function(y, clase_positiva = NULL) {
  y <- droplevels(as.factor(y))

  if (anyNA(y)) error_claro("La variable objetivo contiene NA. Elimínalos o impútalos antes.")
  if (nlevels(y) < 2) error_claro("La variable objetivo debe contener al menos dos clases.")

  # caret necesita nombres de clases válidos para generar probabilidades.
  niveles_originales <- levels(y)
  niveles_validos <- make.names(niveles_originales, unique = TRUE)
  levels(y) <- niveles_validos
  attr(y, "mapa_niveles") <- setNames(niveles_validos, niveles_originales)

  if (!is.null(clase_positiva)) {
    clase_valida <- make.names(clase_positiva)
    if (!clase_valida %in% levels(y)) {
      error_claro(
        paste0(
          "La clase positiva '", clase_positiva, "' no está entre las clases: ",
          paste(levels(y), collapse = ", ")
        )
      )
    }
  }

  y
}

# ------------------------------------------------------------------
# Extrae una matriz numérica limpia para métodos no supervisados
# ------------------------------------------------------------------
preparar_matriz_numerica <- function(df,
                                     columnas_excluir = character(0),
                                     max_variables = NULL,
                                     imputar = TRUE,
                                     escalar = TRUE) {
  existentes_excluir <- intersect(columnas_excluir, names(df))
  candidatos <- setdiff(names(df), existentes_excluir)
  numericas <- candidatos[vapply(df[candidatos], is.numeric, logical(1))]

  if (length(numericas) == 0) {
    error_claro("No hay columnas numéricas disponibles después de excluir identificadores/etiquetas.")
  }

  if (!is.null(max_variables)) {
    max_variables <- max(1, as.integer(max_variables))
    numericas <- head(numericas, max_variables)
  }

  x <- as.data.frame(df[numericas], check.names = FALSE)

  # Convertimos Inf y -Inf en NA para poder tratarlos.
  for (nombre in names(x)) {
    x[[nombre]][!is.finite(x[[nombre]])] <- NA_real_
  }

  # Eliminamos columnas completamente vacías.
  todo_na <- vapply(x, function(z) all(is.na(z)), logical(1))
  if (any(todo_na)) {
    mensaje_aviso(paste0("Se eliminan columnas completamente vacías: ", paste(names(x)[todo_na], collapse = ", ")))
    x <- x[!todo_na]
  }

  # Imputación por mediana, calculada columna a columna.
  if (imputar && anyNA(x)) {
    for (nombre in names(x)) {
      if (anyNA(x[[nombre]])) {
        mediana <- stats::median(x[[nombre]], na.rm = TRUE)
        x[[nombre]][is.na(x[[nombre]])] <- mediana
      }
    }
    mensaje_ok("Valores ausentes imputados con la mediana de cada variable.")
  }

  if (anyNA(x)) error_claro("Persisten valores NA en los predictores.")

  # Eliminamos variables de varianza cero porque rompen el escalado y no informan.
  varianzas <- vapply(x, stats::var, numeric(1))
  var_cero <- !is.finite(varianzas) | varianzas <= .Machine$double.eps
  if (any(var_cero)) {
    mensaje_aviso(paste0("Se eliminan variables de varianza cero: ", paste(names(x)[var_cero], collapse = ", ")))
    x <- x[!var_cero]
  }

  if (ncol(x) == 0) error_claro("No quedan predictores después de la limpieza.")

  matriz <- as.matrix(x)
  storage.mode(matriz) <- "double"

  if (escalar) {
    matriz <- scale(matriz)
    # Una comprobación final evita propagar NA por desviaciones típicas nulas.
    if (anyNA(matriz)) error_claro("El escalado produjo NA; revisa variables constantes.")
  }

  attr(matriz, "columnas_utilizadas") <- colnames(x)
  matriz
}

# ------------------------------------------------------------------
# Prepara un data frame supervisado con objetivo + predictores numéricos
# ------------------------------------------------------------------
preparar_datos_supervisados <- function(df,
                                        objetivo,
                                        columnas_excluir = character(0),
                                        max_predictores = NULL,
                                        clase_positiva = NULL) {
  validar_columna(df, objetivo)

  columnas_excluir <- unique(c(columnas_excluir, objetivo))
  y <- preparar_objetivo(df[[objetivo]], clase_positiva)
  x <- preparar_matriz_numerica(
    df,
    columnas_excluir = columnas_excluir,
    max_variables = max_predictores,
    imputar = TRUE,
    escalar = FALSE
  )

  resultado <- data.frame(y, as.data.frame(x, check.names = TRUE), check.names = TRUE)
  names(resultado)[1] <- objetivo
  resultado[[objetivo]] <- y
  resultado
}

# ------------------------------------------------------------------
# Partición estratificada train/test
# ------------------------------------------------------------------
dividir_train_test <- function(df, objetivo, proporcion_train = 0.8, semilla = 1995) {
  comprobar_paquetes("caret")
  validar_columna(df, objetivo)

  proporcion_train <- as.numeric(proporcion_train)
  if (proporcion_train <= 0 || proporcion_train >= 1) {
    error_claro("proporcion_train debe estar entre 0 y 1, por ejemplo 0.8.")
  }

  set.seed(semilla)
  indices <- caret::createDataPartition(df[[objetivo]], p = proporcion_train, list = FALSE)
  train <- df[indices, , drop = FALSE]
  test <- df[-indices, , drop = FALSE]

  # Comprobamos que todas las clases aparecen en ambos subconjuntos.
  faltan_train <- setdiff(levels(df[[objetivo]]), unique(as.character(train[[objetivo]])))
  faltan_test <- setdiff(levels(df[[objetivo]]), unique(as.character(test[[objetivo]])))

  if (length(faltan_train) > 0 || length(faltan_test) > 0) {
    error_claro(
      paste0(
        "La partición dejó clases vacías. Faltan en train: ", paste(faltan_train, collapse = ", "),
        "; faltan en test: ", paste(faltan_test, collapse = ", "),
        ". Cambia la semilla o usa más observaciones."
      )
    )
  }

  train[[objetivo]] <- droplevels(train[[objetivo]])
  test[[objetivo]] <- factor(test[[objetivo]], levels = levels(train[[objetivo]]))

  mensaje_ok(sprintf("Partición creada: train=%d, test=%d.", nrow(train), nrow(test)))
  list(train = train, test = test, indices_train = indices)
}

# ------------------------------------------------------------------
# Escalado correcto: calcula media y desviación SOLO con train
# ------------------------------------------------------------------
escalar_train_test <- function(train, test, objetivo) {
  predictores <- setdiff(names(train), objetivo)

  medias <- vapply(train[predictores], mean, numeric(1), na.rm = TRUE)
  desv <- vapply(train[predictores], sd, numeric(1), na.rm = TRUE)
  desv[!is.finite(desv) | desv == 0] <- 1

  train_escalado <- train
  test_escalado <- test

  train_escalado[predictores] <- sweep(
    sweep(as.matrix(train[predictores]), 2, medias, "-"),
    2, desv, "/"
  )
  test_escalado[predictores] <- sweep(
    sweep(as.matrix(test[predictores]), 2, medias, "-"),
    2, desv, "/"
  )

  train_escalado[[objetivo]] <- train[[objetivo]]
  test_escalado[[objetivo]] <- test[[objetivo]]

  list(
    train = train_escalado,
    test = test_escalado,
    medias = medias,
    desviaciones = desv
  )
}

# ------------------------------------------------------------------
# Número seguro de folds para validación cruzada
# ------------------------------------------------------------------
numero_folds_seguro <- function(y, max_folds = 5) {
  minimo_clase <- min(table(y))
  folds <- min(as.integer(max_folds), as.integer(minimo_clase))
  if (folds < 2) error_claro("No hay suficientes casos por clase para validación cruzada.")
  folds
}

# ------------------------------------------------------------------
# Control de entrenamiento de caret adaptado al número de clases
# ------------------------------------------------------------------
crear_control_caret <- function(y,
                                max_folds = 5,
                                repeticiones = 1,
                                guardar_predicciones = TRUE) {
  comprobar_paquetes("caret")
  folds <- numero_folds_seguro(y, max_folds)
  binario <- nlevels(y) == 2

  argumentos_control <- list(
    method = if (repeticiones > 1) "repeatedcv" else "cv",
    number = folds,
    classProbs = TRUE,
    summaryFunction = if (binario) caret::twoClassSummary else caret::multiClassSummary,
    savePredictions = if (guardar_predicciones) "final" else "none",
    allowParallel = FALSE
  )

  # Algunas versiones de caret fallan si repeats recibe NULL.
  # Este argumento solo tiene sentido con validación cruzada repetida.
  if (repeticiones > 1) {
    argumentos_control$repeats <- repeticiones
  }

  do.call(caret::trainControl, argumentos_control)
}

# ------------------------------------------------------------------
# Matriz de confusión y métricas
# ------------------------------------------------------------------
evaluar_clasificacion <- function(real, predicho, clase_positiva = NULL) {
  comprobar_paquetes("caret")

  real <- droplevels(as.factor(real))
  predicho <- factor(predicho, levels = levels(real))

  if (anyNA(predicho)) {
    error_claro("Hay predicciones que no coinciden con los niveles de la variable real.")
  }

  positivo_valido <- NULL
  if (nlevels(real) == 2) {
    positivo_valido <- if (is.null(clase_positiva)) levels(real)[1] else make.names(clase_positiva)
    if (!positivo_valido %in% levels(real)) {
      error_claro(paste0("Clase positiva inválida. Opciones: ", paste(levels(real), collapse = ", ")))
    }
  }

  cm <- caret::confusionMatrix(
    data = predicho,
    reference = real,
    positive = positivo_valido,
    mode = "everything"
  )

  print(cm)
  invisible(cm)
}

# ------------------------------------------------------------------
# ROC binaria segura
# ------------------------------------------------------------------
calcular_roc_binaria <- function(real, probabilidad_positiva, clase_positiva) {
  comprobar_paquetes("pROC")
  real <- droplevels(as.factor(real))
  positivo <- make.names(clase_positiva)

  if (nlevels(real) != 2) error_claro("La ROC binaria requiere exactamente dos clases.")
  if (!positivo %in% levels(real)) error_claro("La clase positiva no existe en la variable real.")
  if (length(probabilidad_positiva) != length(real)) error_claro("Probabilidades y clases tienen distinta longitud.")
  if (any(!is.finite(probabilidad_positiva))) error_claro("Las probabilidades contienen NA o Inf.")

  negativo <- setdiff(levels(real), positivo)
  roc_obj <- pROC::roc(
    response = real,
    predictor = as.numeric(probabilidad_positiva),
    levels = c(negativo, positivo),
    direction = "<",
    quiet = TRUE
  )
  print(roc_obj)
  roc_obj
}

# ------------------------------------------------------------------
# Guarda un data frame como CSV con nombre controlado
# ------------------------------------------------------------------
guardar_tabla <- function(objeto, nombre_archivo) {
  ruta <- file.path("resultados", nombre_archivo)
  utils::write.csv(objeto, ruta, row.names = FALSE)
  mensaje_ok(paste0("Tabla guardada en: ", ruta))
  invisible(ruta)
}

# ------------------------------------------------------------------
# Guarda un gráfico ggplot
# ------------------------------------------------------------------
guardar_ggplot <- function(grafico, nombre_archivo, ancho = 8, alto = 6, dpi = 150) {
  comprobar_paquetes("ggplot2")
  ruta <- file.path("resultados", nombre_archivo)
  ggplot2::ggsave(ruta, plot = grafico, width = ancho, height = alto, dpi = dpi)
  mensaje_ok(paste0("Gráfico guardado en: ", ruta))
  invisible(ruta)
}

# ------------------------------------------------------------------
# Ejecuta un bloque opcional y muestra un error legible sin cerrar R
# ------------------------------------------------------------------
ejecutar_seguro <- function(nombre, expresion) {
  cat("\n--- Ejecutando: ", nombre, " ---\n", sep = "")
  tryCatch(
    force(expresion),
    error = function(e) {
      mensaje_aviso(paste0(nombre, " no pudo completarse: ", conditionMessage(e)))
      NULL
    }
  )
}
