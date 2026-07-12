# Plantillas ultra guiadas para el examen de Algoritmos e Inteligencia Artificial en R

Estas plantillas están preparadas para poder usarlas **aunque partas de cero**. Cada archivo incluye:

- un bloque inicial llamado **CONFIGURACIÓN: SOLO EDITAR AQUÍ**;
- comprobaciones antes de ejecutar el algoritmo;
- mensajes claros de progreso;
- comentarios que explican qué hace cada instrucción;
- tratamiento de errores frecuentes;
- guardado automático de tablas y gráficos en `resultados/`;
- una sección final llamada **QUÉ INTERPRETAR EN EL EXAMEN**.

## 1. Qué debes hacer antes del examen

1. Descomprime esta carpeta completa.
2. Abre el archivo `Plantillas_Examen_IA_R.Rproj` con RStudio. Esto coloca automáticamente el directorio de trabajo en la carpeta correcta y evita usar rutas fijas con `setwd()`.
3. Ejecuta una vez, con conexión a Internet, `00_INSTALAR_Y_COMPROBAR_PAQUETES.R`.
4. Si los temas 9 y 10 entran en el examen, ejecuta también `00B_INSTALAR_BACKEND_KERAS.R` una sola vez.
5. Copia los CSV que vayas a utilizar dentro de la carpeta `data/`.
6. Ejecuta `00_PRUEBA_DEL_ENTORNO.R` y comprueba que termina con el mensaje `ENTORNO PREPARADO`.
7. En el examen, abre únicamente la plantilla del tema que te pidan y modifica el bloque **SOLO EDITAR AQUÍ**.

> No ejecutes `install.packages()` durante el examen salvo que el profesor lo permita. La instalación puede tardar, pedir compiladores o fallar por falta de Internet.

## 2. Orden recomendado de uso

| Archivo | Cuándo usarlo |
|---|---|
| `01_TEMA1_BASES_Y_PREPROCESAMIENTO.R` | Cargar, inspeccionar, limpiar, transformar y describir datos. |
| `02_TEMA2_REDUCCION_DIMENSIONALIDAD_I.R` | PCA, MDS, Isomap y t-SNE. |
| `03_TEMA3_REDUCCION_DIMENSIONALIDAD_II.R` | LLE, Laplacian Eigenmaps, MVU, UMAP e ICA. |
| `04_TEMA4_CLUSTERING.R` | K-means, PAM, jerárquico y DBSCAN. |
| `05_TEMA5_ANALISIS_DISCRIMINANTE.R` | LDA, QDA, RDA y FDA. |
| `06_TEMA6_SUPERVISADO_I.R` | k-NN, SVM lineal, SVM radial/polinómica y árbol. |
| `07_TEMA7_SUPERVISADO_II.R` | Bagging, Naive Bayes, Random Forest y GBM. |
| `08_TEMA8_EVALUACION_MODELOS.R` | Matriz de confusión, métricas, ROC, PR y regresión. |
| `09_TEMA9_DEEP_LEARNING_I.R` | Preparación de datos, tensores y red neuronal con Iris. |
| `10_TEMA10_DEEP_LEARNING_II.R` | Pérdidas, optimizadores, entrenamiento y callbacks. |
| `11_PLANTILLA_MAESTRA_CLASIFICACION.R` | Resolver de principio a fin una práctica de clasificación. |
| `12_PLANTILLA_MAESTRA_NO_SUPERVISADO.R` | Resolver una práctica completa de reducción y clustering. |

## 3. Regla de oro durante el examen

Ejecuta el código **por bloques**, de arriba abajo. Después de cada bloque:

1. lee el mensaje que aparece en la consola;
2. comprueba que no aparece texto rojo;
3. revisa con `str(objeto)` que el objeto existe y tiene la forma esperada;
4. no continúes si la plantilla muestra `ERROR`.

## 4. Datos esperados

### Reducción de dimensionalidad

Las plantillas de los temas 2 y 3 están preparadas para el formato empleado en los materiales:

- `data/data.csv`: filas = muestras, columnas = identificador + genes;
- `data/labels.csv`: una fila por muestra y una columna de clase, normalmente `Class`.

### Clasificación supervisada

Las plantillas de los temas 5 a 8 admiten un único CSV, por ejemplo:

- `data/dataset_expresiongenes_cancer.csv`;
- variable objetivo: `primaryormetastasis`;
- el resto de columnas numéricas se utilizan como predictores, excepto las que indiques en `columnas_excluir`.

### Redes neuronales

La plantilla del tema 9 utiliza `iris`, incluido en R, por lo que sirve para comprobar que Keras funciona sin necesitar un CSV externo.

## 5. Errores frecuentes y solución inmediata

| Mensaje o problema | Causa habitual | Solución |
|---|---|---|
| `No se encuentra el archivo` | El CSV no está en `data/` o el nombre no coincide. | Copia el archivo, revisa mayúsculas, espacios y extensión. |
| `there is no package called...` | Falta un paquete. | Ejecuta `00_INSTALAR_Y_COMPROBAR_PAQUETES.R` antes del examen. |
| `object ... not found` | Se ejecutó un bloque sin ejecutar los anteriores. | Reinicia R y ejecuta desde el principio, por secciones. |
| `contrasts can be applied only to factors with 2 or more levels` | En train o test solo quedó una clase. | Aumenta la muestra, cambia la semilla o usa partición estratificada. |
| `system is computationally singular` | Demasiadas variables, colinealidad o pocas muestras. | Reduce predictores; usa RDA; activa selección automática. |
| `perplexity is too large` | La perplejidad de t-SNE es demasiado alta. | La plantilla la calcula automáticamente; no la aumentes. |
| `k must be smaller than number of observations` | Demasiados vecinos o clústeres. | Reduce `k`; la plantilla aplica límites seguros. |
| ROC invertida o AUC extraña | Se eligió mal la clase positiva o columna de probabilidad. | Define `clase_positiva` y revisa `levels(y)`. |
| Keras no encuentra Python/TensorFlow | Backend no instalado o entorno roto. | Ejecuta la instalación antes del examen y prueba `00_PRUEBA_DEL_ENTORNO.R`. |

## 6. Qué archivos no debes modificar

- `00_UTILIDADES.R`: contiene funciones comunes y comprobaciones.
- `Plantillas_Examen_IA_R.Rproj`: configura el proyecto.
- La carpeta `resultados/`: es donde se guardan automáticamente las salidas.

## 7. Qué entregar si piden código y resultados

Entrega como mínimo:

1. el script `.R` utilizado;
2. la tabla de resultados o matriz de confusión;
3. los gráficos relevantes;
4. una interpretación breve del resultado;
5. la semilla empleada;
6. la justificación del preprocesamiento y del algoritmo.

En `GUIA_INTERPRETACION_EXAMEN.md` tienes frases modelo para redactar la interpretación sin confundir conceptos.
