# Checklist rápida para el examen con RStudio

Este repositorio es un **kit de preparación para examen**. La idea no es modificar todo el código, sino usar las plantillas base que ya están preparadas, adaptar la configuración al enunciado y saber interpretar los resultados.

La prioridad durante el examen es:

1. identificar rápido el tipo de ejercicio;
2. abrir la plantilla correcta;
3. editar solo el bloque de configuración;
4. ejecutar por bloques;
5. interpretar correctamente tablas, métricas y gráficos.

---

## 0. Antes de empezar

Abrir el proyecto desde RStudio usando:

```txt
Plantillas_Examen_IA_R/Plantillas_Examen_IA_R.Rproj
```

Después ejecutar:

```r
00_PRUEBA_DEL_ENTORNO.R
```

Si falla, no seguir con el examen todavía. Primero revisar entorno, paquetes o ruta del proyecto.

Si el examen incluye deep learning, el backend debe estar probado antes con:

```r
00B_INSTALAR_BACKEND_KERAS.R
```

No conviene instalar paquetes durante el examen salvo que sea estrictamente necesario.

---

## 1. Leer el enunciado y clasificar el problema

La primera pregunta importante es:

> ¿Existe una variable objetivo/clase que hay que predecir?

### Si hay variable objetivo

Es un problema **supervisado**.

Palabras clave habituales:

```txt
clasificar
predecir clase
variable objetivo
train/test
matriz de confusión
accuracy
sensibilidad
especificidad
ROC
AUC
```

Usar principalmente:

```txt
11_PLANTILLA_MAESTRA_CLASIFICACION.R
```

O, si el enunciado pide métodos concretos:

```txt
05_TEMA5_ANALISIS_DISCRIMINANTE.R
06_TEMA6_SUPERVISADO_I.R
07_TEMA7_SUPERVISADO_II.R
08_TEMA8_EVALUACION_MODELOS.R
```

### Si no hay variable objetivo

Es un problema **no supervisado**.

Palabras clave habituales:

```txt
agrupar
clusters
reducir dimensionalidad
visualizar en 2D/3D
PCA
MDS
t-SNE
UMAP
k-means
jerárquico
DBSCAN
silueta
```

Usar principalmente:

```txt
12_PLANTILLA_MAESTRA_NO_SUPERVISADO.R
```

O, si el enunciado pide métodos concretos:

```txt
02_TEMA2_REDUCCION_DIMENSIONALIDAD_I.R
03_TEMA3_REDUCCION_DIMENSIONALIDAD_II.R
04_TEMA4_CLUSTERING.R
```

---

## 2. Qué tocar en cada plantilla

Buscar siempre este bloque:

```r
# CONFIGURACIÓN: SOLO EDITAR AQUÍ
```

En condiciones normales, editar solo esa sección.

Parámetros típicos a cambiar:

```r
archivo_datos
columna_objetivo
columnas_excluir
clase_positiva
semilla
proporcion_train
numero_clusters
metodo
```

No conviene modificar el resto del script durante el examen salvo que se sepa exactamente qué se está haciendo.

---

## 3. Flujo para ejercicios supervisados

Archivo recomendado:

```txt
11_PLANTILLA_MAESTRA_CLASIFICACION.R
```

Del enunciado hay que identificar:

```txt
1. Nombre del CSV.
2. Variable objetivo.
3. Columnas identificadoras a excluir.
4. Modelos que pide comparar.
5. Clase positiva, si el problema es binario.
```

Ejemplo mental:

```txt
Dataset: cancer.csv
Objetivo: primaryormetastasis
Excluir: id, paciente, muestra
Modelos: LDA y árbol
Clase positiva: metastatic
```

Resultados importantes:

```txt
matriz de confusión
accuracy
sensitivity / sensibilidad
specificity / especificidad
balanced accuracy
ROC
AUC
validación cruzada
```

Frases útiles para interpretar:

```txt
El modelo con mejor rendimiento global fue X porque obtuvo mayor AUC/accuracy en test.

La matriz de confusión muestra que el modelo clasifica correctamente la mayoría de los casos, aunque comete errores especialmente en la clase Y.

La sensibilidad indica la capacidad para detectar correctamente la clase positiva.

La especificidad indica la capacidad para detectar correctamente la clase negativa.

Si el AUC está cerca de 1, el modelo separa bien las clases. Si está cerca de 0.5, el modelo no discrimina mejor que el azar.
```

Si las clases están desbalanceadas, no alcanza con mirar solo accuracy. En ese caso conviene escribir:

```txt
Como las clases pueden estar desbalanceadas, conviene mirar también sensibilidad, especificidad, balanced accuracy y AUC.
```

---

## 4. Flujo para ejercicios no supervisados

Archivo recomendado:

```txt
12_PLANTILLA_MAESTRA_NO_SUPERVISADO.R
```

Del enunciado hay que identificar:

```txt
1. Nombre del CSV.
2. Columnas identificadoras a excluir.
3. Método pedido: PCA, MDS, UMAP, clustering, etc.
4. Número de clusters, si lo indican.
5. Si piden gráfico 2D o 3D.
```

Resultados importantes:

```txt
varianza explicada
gráfico 2D
gráfico 3D
clusters
silueta
tabla cluster vs clase real, si existe
```

### Interpretar PCA

```txt
La primera componente principal resume la mayor parte de la variabilidad de los datos.

La segunda componente explica la siguiente mayor variabilidad, siendo ortogonal a la primera.

Si los grupos aparecen separados en el plano PC1-PC2, significa que las variables originales contienen información suficiente para diferenciarlos parcialmente.

Si los grupos aparecen mezclados, no hay una separación clara en las dos primeras componentes.
```

### Interpretar MDS

```txt
MDS representa las observaciones en pocas dimensiones intentando conservar las distancias originales.

Puntos cercanos en el gráfico representan muestras similares.

Puntos alejados representan muestras con perfiles diferentes.
```

### Interpretar clustering

```txt
El clustering agrupa observaciones similares sin usar una variable objetivo.

Una buena solución debería mostrar clusters compactos y separados.

El índice de silueta mide qué tan bien asignada está cada observación a su grupo.

Valores de silueta cercanos a 1 indican buena asignación; valores cercanos a 0 indican solapamiento; valores negativos sugieren mala asignación.
```

---

## 5. Si piden comparar dos modelos

### Comparación supervisada

Comparar usando:

```txt
accuracy
AUC
sensibilidad
especificidad
matriz de confusión
validación cruzada
```

Respuesta modelo:

```txt
Comparando ambos modelos, X obtiene mejor rendimiento porque presenta mayor AUC y mejor accuracy en el conjunto de test. Además, mantiene una sensibilidad/especificidad más equilibrada, por lo que generaliza mejor que Y.
```

Si un modelo tiene mayor accuracy pero peor sensibilidad:

```txt
Aunque X tiene mayor accuracy, Y puede ser preferible si interesa detectar mejor la clase positiva, ya que presenta mayor sensibilidad.
```

### Comparación no supervisada

Comparar usando:

```txt
separación visual
silueta
interpretabilidad
coherencia de clusters
```

Respuesta modelo:

```txt
El método X ofrece una representación más clara porque los grupos aparecen más separados visualmente y el índice de silueta es mayor. Por tanto, parece capturar mejor la estructura interna de los datos.
```

---

## 6. Errores frecuentes

### No encuentra el archivo

Revisar:

```txt
data/
nombre exacto del CSV
mayúsculas/minúsculas
extensión .csv
```

Evitar rutas absolutas si se puede.

### La columna no existe

Ejecutar o revisar:

```r
names(datos)
```

Copiar el nombre exacto de la columna.

### Solo queda una clase en train o test

Probar otra semilla:

```r
semilla <- otro_numero
```

O cambiar la proporción train/test si la plantilla lo permite.

### Problemas con ROC / AUC

Probablemente está mal definida la clase positiva:

```r
clase_positiva
```

Revisar niveles:

```r
levels(y)
```

---

## 7. Archivos importantes

Tener localizados como mínimo:

```txt
README.md
GUIA_INTERPRETACION_EXAMEN.md
Plantillas_Examen_IA_R/MAPA_DE_PLANTILLAS.md
Plantillas_Examen_IA_R/11_PLANTILLA_MAESTRA_CLASIFICACION.R
Plantillas_Examen_IA_R/12_PLANTILLA_MAESTRA_NO_SUPERVISADO.R
```

Si el examen va por temas concretos:

```txt
02_TEMA2_REDUCCION_DIMENSIONALIDAD_I.R       PCA / MDS / t-SNE / Isomap
03_TEMA3_REDUCCION_DIMENSIONALIDAD_II.R      UMAP / ICA / LLE
04_TEMA4_CLUSTERING.R                        clustering
05_TEMA5_ANALISIS_DISCRIMINANTE.R            LDA / QDA / RDA
06_TEMA6_SUPERVISADO_I.R                     kNN / SVM / árbol
07_TEMA7_SUPERVISADO_II.R                    Random Forest / GBM / Naive Bayes
08_TEMA8_EVALUACION_MODELOS.R                métricas / ROC
```

---

## 8. Orden mental durante el examen

```txt
1. Leo el enunciado.
2. Detecto si es supervisado o no supervisado.
3. Elijo plantilla.
4. Copio el CSV en data/.
5. Edito solo CONFIGURACIÓN.
6. Ejecuto por bloques.
7. Reviso resultados/.
8. Leo gráficos y tablas.
9. Escribo interpretación.
10. Entrego código, resultados y explicación.
```

---

## 9. Frases comodín para cerrar respuestas

### Supervisado

```txt
En conclusión, el modelo seleccionado es adecuado porque obtiene buen rendimiento en test y mantiene métricas equilibradas. La interpretación debe considerar tanto la matriz de confusión como las métricas globales, especialmente si las clases están desbalanceadas.
```

### No supervisado

```txt
En conclusión, el método permite explorar la estructura interna de los datos. La calidad de la agrupación debe valorarse mediante la separación visual, la coherencia de los grupos y el índice de silueta.
```

---

## Idea clave

No hace falta memorizar todo el código.

Lo importante es saber:

```txt
qué plantilla abrir,
qué parámetro tocar,
qué resultado mirar,
y cómo interpretar lo obtenido.
```

Código sin interpretación es solo ejecutar botones. Interpretación sin resultados es humo. En el examen hacen falta las dos cosas.
