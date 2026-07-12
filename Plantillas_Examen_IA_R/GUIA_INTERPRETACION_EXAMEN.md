# Guía de interpretación y frases modelo

## Reducción de dimensionalidad

### PCA

- “La primera componente explica el **X %** de la varianza y la segunda el **Y %**.”
- “Las primeras **N** componentes acumulan al menos el **90 %** de la variabilidad.”
- “PCA es lineal y maximiza la varianza proyectada; no utiliza las etiquetas para calcular las componentes.”
- “La separación visual entre grupos sugiere estructura, pero no demuestra por sí sola capacidad predictiva.”

### MDS

- “MDS intenta conservar en el espacio reducido las distancias entre observaciones.”
- “Una representación clara indica que las distancias originales se conservan razonablemente en dos dimensiones.”

### Isomap, LLE, LE, MVU, UMAP y t-SNE

- “Son métodos no lineales orientados a conservar estructura local o geométrica.”
- “Los ejes no tienen una interpretación directa equivalente a la varianza explicada de PCA.”
- “El resultado depende de hiperparámetros como el número de vecinos, la perplejidad o la distancia mínima.”
- “t-SNE y UMAP son especialmente útiles para visualización; las distancias globales deben interpretarse con cautela.”

### ICA

- “ICA busca componentes estadísticamente independientes, no componentes de máxima varianza.”

## Clustering

- “El clustering es no supervisado: las etiquetas reales no se usan para construir los grupos.”
- “K-means minimiza la variación dentro de los clústeres y funciona mejor con grupos aproximadamente esféricos.”
- “Los números de clúster son arbitrarios; clúster 1 no significa clase 1.”
- “El dendrograma representa fusiones sucesivas; el corte elegido determina el número final de clústeres.”
- “La silueta cercana a 1 indica buena cohesión y separación; cercana a 0 indica solapamiento.”

## Clasificación

### Matriz de confusión

- “La exactitud representa la proporción total de aciertos.”
- “La sensibilidad mide cuántos positivos reales se detectan.”
- “La especificidad mide cuántos negativos reales se descartan correctamente.”
- “La precisión o valor predictivo positivo mide cuántas predicciones positivas eran realmente positivas.”
- “F1 equilibra precisión y sensibilidad.”
- “Balanced Accuracy es útil cuando las clases están desbalanceadas.”

### ROC y PR

- “El AUC-ROC resume la capacidad de ordenar positivos por encima de negativos a través de todos los umbrales.”
- “La curva Precision-Recall suele ser más informativa cuando la clase positiva es poco frecuente.”
- “El umbral 0,5 no siempre es óptimo; debe ajustarse al coste de falsos positivos y falsos negativos.”

### Sobreajuste

- “Una diferencia grande entre entrenamiento y test sugiere sobreajuste.”
- “La validación cruzada se utiliza para ajustar hiperparámetros sin utilizar el conjunto de test.”
- “El test se reserva para la evaluación final.”

## Algoritmos supervisados

- **LDA:** asume covarianza común y fronteras lineales.
- **QDA:** permite una covarianza por clase y fronteras cuadráticas; necesita más datos.
- **RDA:** regulariza las covarianzas y es útil cuando hay muchas variables o colinealidad.
- **k-NN:** depende de distancias; requiere escalado.
- **SVM lineal:** busca un hiperplano de margen máximo.
- **SVM radial:** modela fronteras no lineales mediante kernel gaussiano.
- **Árbol:** interpretable, pero puede sobreajustar si crece demasiado.
- **Bagging:** promedia muchos modelos entrenados con muestras bootstrap y reduce varianza.
- **Random Forest:** añade selección aleatoria de variables a bagging.
- **Naive Bayes:** aplica Bayes bajo una hipótesis fuerte de independencia condicional.
- **GBM:** construye modelos secuencialmente para corregir errores anteriores.

## Redes neuronales

- “La salida forward produce una predicción; la pérdida cuantifica el error.”
- “Backpropagation calcula gradientes y el optimizador actualiza pesos y sesgos.”
- “Una caída de la pérdida de entrenamiento y validación indica aprendizaje.”
- “Si la pérdida de entrenamiento sigue bajando pero la de validación sube, aparece sobreajuste.”
- “Early stopping detiene el entrenamiento cuando la validación deja de mejorar.”
