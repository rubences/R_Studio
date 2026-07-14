# Plantillas ultra guiadas para el examen de Algoritmos e Inteligencia Artificial en R

https://github.com/rubences/R_Studio.git

He utilizado estas plantillas para realizar el examen, como se demuestra en los repositorios previos, había preparado dos plantillas para modelos supervisados y no supervisados, que son las plantillas, 13A y 13B. A partir de esas durante esos días he ido afinando la información como puedes ver en los commits de los repositorios incluso con un ejemplo. para ajustarla a que solamente tuviesemos que cambiar el tipo de datos que nosotros teníamos, en el conjunto de dataset y la variable a definir.

En el caso del del no supervidado, solamente del repositorio original 13A al 14A he cambiado lo siguiente:

CONFIG <- list(

  # CSV situado dentro de la carpeta data/.
  archivo = file.path("data", "dataset_expresiongenes_cancer.csv"),

  # Etiqueta real. Se excluye totalmente del método no supervisado.
  # Solo se utiliza después para una comparación descriptiva.
  objetivo = "primaryormetastasis",

  # Identificadores u otras columnas que no deben analizarse.
  columnas_excluir = c("id", "ID", "sample", "patient"),

  # Límite de variables numéricas para evitar problemas de memoria.
  # Escribe NULL para utilizar todas las variables válidas.
  max_predictores = 50,

  # "PCA" es la opción más segura. "UMAP" es no lineal.
  metodo_no_supervisado = "PCA",

  # Número de grupos que se buscarán con K-means.
  numero_clusters = 2,

  # Semilla para que el resultado sea reproducible.
  semilla = 1995,

  # TRUE genera gráficos adicionales coloreados por la clase real.
  colorear_tambien_por_clase_real = TRUE
)

Y el 14A sería lo siguiente:

CONFIG <- list(
  archivo = file.path("data", "cirrhosis.csv"),
  separador = ";",
  objetivo = "Status",
  columnas_excluir = c("Status"),
  metodo_no_supervisado = "PCA",
  numero_clusters = 3,
  semilla = 1995,
  colorear_tambien_por_objetivo = TRUE
)

Bien el resto permanece constante.

Me ha dado un error al buscar la variable de que no la reconocía y he arrancado el power shell para visualizar el conjunto de datos y es que estaba escribiendo status en lugar de Status en un primer lugar y en un segundo lugar he escrito Stratus, hasta que lo he localizado. Como puede comprobar el resto de fichero pernanece constante.

========================================================================
PASO 0: Comprobar configuración y paquetes
========================================================================
[OK] Paquetes disponibles.

========================================================================
PASO 1: Cargar e inspeccionar el conjunto reducido
========================================================================
[OK] CSV leído: 312 filas y 9 columnas.

Resumen de datos_reducidos:
- Dimensiones: 312 filas x 9 columnas
- Filas duplicadas: 0
- Valores ausentes: 0
- Tipos de columnas:

character   integer   numeric 
        1         3         5 

Primeras filas:
  muestra    Dim1_2D    Dim2_2D Cluster_2D    Dim1_3D    Dim2_3D    Dim3_3D
1       1 -3.7475961 -1.4191169          2 -3.7475961 -1.4191169 -0.3712262
2       2  1.6530015  0.5391647          1  1.6530015  0.5391647 -2.1627686
3       3 -1.0451872 -2.9807548          2 -1.0451872 -2.9807548 -0.4805517
4       4 -0.5676489 -1.3798222          2 -0.5676489 -1.3798222 -1.4457570
5       5 -0.1018042 -0.8385998          2 -0.1018042 -0.8385998  1.2332200
6       6  1.3293454 -1.3073654          1  1.3293454 -1.3073654 -0.4829584
  Cluster_3D Status
1          2      D
2          1      C
3          2      D
4          2      D
5          2      C
6          1      D

Distribución original de la variable objetivo:

  C   D 
187 125 

========================================================================
PASO 1.1: Resumen básico de valores ausentes y variables
========================================================================
[OK] Tabla guardada en: resultados/14B_resumen_na.csv

========================================================================
PASO 2: Preparar variables para clasificación
========================================================================

Clases utilizadas en el problema supervisado:

  D   C 
125 187 
Niveles, en orden: D, C 
Clase positiva: D 

========================================================================
PASO 3: Dividir en entrenamiento y prueba
========================================================================
[OK] Partición creada: train=250, test=62.

Distribución en TRAIN:

  D   C 
100 150 

Distribución en TEST:

 D  C 
25 37 

========================================================================
PASO 3.1: Estandarizar usando solo TRAIN
========================================================================

========================================================================
PASO 4: Entrenar modelos supervisados
========================================================================

============================================================
ENTRENANDO: glm 
============================================================
Loading required package: ggplot2
Loading required package: lattice
Generalized Linear Model 

250 samples
  3 predictor
  2 classes: 'D', 'C' 

No pre-processing
Resampling: Cross-Validated (5 fold) 
Summary of sample sizes: 200, 200, 200, 200, 200 
Resampling results:

  ROC        Sens  Spec
  0.8503333  0.64  0.88


Mejores hiperparámetros:
  parameter
1      none
Confusion Matrix and Statistics

          Reference
Prediction  D  C
         D 20  3
         C  5 34
                                          
               Accuracy : 0.871           
                 95% CI : (0.7615, 0.9426)
    No Information Rate : 0.5968          
    P-Value [Acc > NIR] : 2.335e-06       
                                          
                  Kappa : 0.7284          
                                          
 Mcnemar's Test P-Value : 0.7237          
                                          
            Sensitivity : 0.8000          
            Specificity : 0.9189          
         Pos Pred Value : 0.8696          
         Neg Pred Value : 0.8718          
              Precision : 0.8696          
                 Recall : 0.8000          
                     F1 : 0.8333          
             Prevalence : 0.4032          
         Detection Rate : 0.3226          
   Detection Prevalence : 0.3710          
      Balanced Accuracy : 0.8595          
                                          
       'Positive' Class : D               
                                          

Call:
roc.default(response = real, predictor = as.numeric(probabilidad_positiva),     levels = c(negativo, positivo), direction = "<", quiet = TRUE)

Data: as.numeric(probabilidad_positiva) in 37 controls (real C) < 25 cases (real D).
Area under the curve: 0.8941
[OK] Tabla guardada en: resultados/14B_glm_predicciones_test.csv
[OK] Tabla guardada en: resultados/14B_glm_matriz_confusion.csv

============================================================
ENTRENANDO: knn 
============================================================
k-Nearest Neighbors 

250 samples
  3 predictor
  2 classes: 'D', 'C' 

No pre-processing
Resampling: Cross-Validated (5 fold) 
Summary of sample sizes: 200, 200, 200, 200, 200 
Resampling results across tuning parameters:

  k   ROC        Sens  Spec     
   5  0.7768333  0.60  0.8400000
   7  0.7860000  0.61  0.8533333
   9  0.8008333  0.60  0.8733333
  11  0.8166667  0.59  0.8666667
  13  0.8215000  0.61  0.8733333
  15  0.8200000  0.59  0.8866667
  17  0.8203333  0.56  0.9000000
  19  0.8213333  0.57  0.8866667

ROC was used to select the optimal model using the largest value.
The final value used for the model was k = 13.

Mejores hiperparámetros:
   k
5 13
Confusion Matrix and Statistics

          Reference
Prediction  D  C
         D 18  6
         C  7 31
                                          
               Accuracy : 0.7903          
                 95% CI : (0.6682, 0.8834)
    No Information Rate : 0.5968          
    P-Value [Acc > NIR] : 0.001009        
                                          
                  Kappa : 0.5615          
                                          
 Mcnemar's Test P-Value : 1.000000        
                                          
            Sensitivity : 0.7200          
            Specificity : 0.8378          
         Pos Pred Value : 0.7500          
         Neg Pred Value : 0.8158          
              Precision : 0.7500          
                 Recall : 0.7200          
                     F1 : 0.7347          
             Prevalence : 0.4032          
         Detection Rate : 0.2903          
   Detection Prevalence : 0.3871          
      Balanced Accuracy : 0.7789          
                                          
       'Positive' Class : D               
                                          

Call:
roc.default(response = real, predictor = as.numeric(probabilidad_positiva),     levels = c(negativo, positivo), direction = "<", quiet = TRUE)

Data: as.numeric(probabilidad_positiva) in 37 controls (real C) < 25 cases (real D).
Area under the curve: 0.8643
[OK] Tabla guardada en: resultados/14B_knn_predicciones_test.csv
[OK] Tabla guardada en: resultados/14B_knn_matriz_confusion.csv

============================================================
ENTRENANDO: rpart 
============================================================
CART 

250 samples
  3 predictor
  2 classes: 'D', 'C' 

No pre-processing
Resampling: Cross-Validated (5 fold) 
Summary of sample sizes: 200, 200, 200, 200, 200 
Resampling results across tuning parameters:

  cp          ROC        Sens  Spec     
  0.00000000  0.7501667  0.65  0.7800000
  0.06285714  0.7560000  0.67  0.8200000
  0.12571429  0.7483333  0.71  0.7866667
  0.18857143  0.7483333  0.71  0.7866667
  0.25142857  0.7483333  0.71  0.7866667
  0.31428571  0.7483333  0.71  0.7866667
  0.37714286  0.7483333  0.71  0.7866667
  0.44000000  0.6916667  0.55  0.8333333

ROC was used to select the optimal model using the largest value.
The final value used for the model was cp = 0.06285714.

Mejores hiperparámetros:
          cp
2 0.06285714
Confusion Matrix and Statistics

          Reference
Prediction  D  C
         D 21  9
         C  4 28
                                          
               Accuracy : 0.7903          
                 95% CI : (0.6682, 0.8834)
    No Information Rate : 0.5968          
    P-Value [Acc > NIR] : 0.001009        
                                          
                  Kappa : 0.578           
                                          
 Mcnemar's Test P-Value : 0.267257        
                                          
            Sensitivity : 0.8400          
            Specificity : 0.7568          
         Pos Pred Value : 0.7000          
         Neg Pred Value : 0.8750          
              Precision : 0.7000          
                 Recall : 0.8400          
                     F1 : 0.7636          
             Prevalence : 0.4032          
         Detection Rate : 0.3387          
   Detection Prevalence : 0.4839          
      Balanced Accuracy : 0.7984          
                                          
       'Positive' Class : D               
                                          

Call:
roc.default(response = real, predictor = as.numeric(probabilidad_positiva),     levels = c(negativo, positivo), direction = "<", quiet = TRUE)

Data: as.numeric(probabilidad_positiva) in 37 controls (real C) < 25 cases (real D).
Area under the curve: 0.7984
[OK] Tabla guardada en: resultados/14B_rpart_predicciones_test.csv
[OK] Tabla guardada en: resultados/14B_rpart_matriz_confusion.csv

========================================================================
PASO 5: Comparar el rendimiento de los modelos
========================================================================
  Modelo  Accuracy Precision Recall        F1       AUC
1    glm 0.8709677 0.8695652   0.80 0.8333333 0.8940541
2    knn 0.7903226 0.7500000   0.72 0.7346939 0.8643243
3  rpart 0.7903226 0.7000000   0.84 0.7636364 0.7983784
[OK] Tabla guardada en: resultados/14B_comparacion_modelos.csv

========================================================================
PASO 6: Guardar curva ROC comparada
========================================================================
[OK] Curva ROC guardada en resultados/14B_curvas_roc_comparadas.png

========================================================================
PASO 7: Generar respuesta de reflexión
========================================================================
============================================================
BORRADOR DE RESPUESTA: CLASIFICACIÓN SUPERVISADA
============================================================

Se utilizó el conjunto reducido de la parte 1 (coordenadas PCA 3D) y se dividió en train/test con proporción 0.8.
Se entrenaron tres modelos supervisados: regresión logística, kNN y árbol de decisión.
La clase positiva se fijó en 'D' para evaluar el evento clínico de no supervivencia.
El mejor modelo según AUC y F1 fue 'glm' con Accuracy = 0.871, Precision = 0.87, Recall = 0.8, F1 = 0.833 y AUC = 0.894.

Conclusión: si un modelo tiene mayor AUC y F1, separa mejor las clases y mantiene un mejor equilibrio entre falsos positivos y falsos negativos.
En esta configuración, 'glm' fue el más conveniente porque ofreció el mejor compromiso global entre discriminación y equilibrio de clases.
 

============================================================
ANÁLISIS SUPERVISADO COMPLETADO
============================================================
Revisa la carpeta resultados/. Encontrarás:
1) Predicciones de cada modelo.
2) Matrices de confusión.
3) Comparación de métricas.
4) Curva ROC comparada.
5) Interpretación final lista para el examen.
============================================================
> source("/workspaces/R_Studio/Plantillas_Examen_IA_R/14B_EXAMEN_SUPERVISADO_CIRRHOSIS.R", encoding = "UTF-8")

========================================================================
PASO 0: Comprobar configuración y paquetes
========================================================================
[OK] Paquetes disponibles.

========================================================================
PASO 1: Cargar e inspeccionar el conjunto reducido
========================================================================
[OK] CSV leído: 312 filas y 9 columnas.

Resumen de datos_reducidos:
- Dimensiones: 312 filas x 9 columnas
- Filas duplicadas: 0
- Valores ausentes: 0
- Tipos de columnas:

character   integer   numeric 
        1         3         5 

Primeras filas:
  muestra    Dim1_2D    Dim2_2D Cluster_2D    Dim1_3D    Dim2_3D    Dim3_3D
1       1 -3.7475961 -1.4191169          2 -3.7475961 -1.4191169 -0.3712262
2       2  1.6530015  0.5391647          1  1.6530015  0.5391647 -2.1627686
3       3 -1.0451872 -2.9807548          2 -1.0451872 -2.9807548 -0.4805517
4       4 -0.5676489 -1.3798222          2 -0.5676489 -1.3798222 -1.4457570
5       5 -0.1018042 -0.8385998          2 -0.1018042 -0.8385998  1.2332200
6       6  1.3293454 -1.3073654          1  1.3293454 -1.3073654 -0.4829584
  Cluster_3D Status
1          2      D
2          1      C
3          2      D
4          2      D
5          2      C
6          1      D

Distribución original de la variable objetivo:

  C   D 
187 125 

========================================================================
PASO 1.1: Resumen básico de valores ausentes y variables
========================================================================
[OK] Tabla guardada en: resultados/14B_resumen_na.csv

========================================================================
PASO 2: Preparar variables para clasificación
========================================================================

Clases utilizadas en el problema supervisado:

  D   C 
125 187 
Niveles, en orden: D, C 
Clase positiva: D 

========================================================================
PASO 3: Dividir en entrenamiento y prueba
========================================================================
[OK] Partición creada: train=250, test=62.

Distribución en TRAIN:

  D   C 
100 150 

Distribución en TEST:

 D  C 
25 37 

========================================================================
PASO 3.1: Estandarizar usando solo TRAIN
========================================================================

========================================================================
PASO 4: Entrenar modelos supervisados
========================================================================

============================================================
ENTRENANDO: glm 
============================================================
Generalized Linear Model 

250 samples
  3 predictor
  2 classes: 'D', 'C' 

No pre-processing
Resampling: Cross-Validated (5 fold) 
Summary of sample sizes: 200, 200, 200, 200, 200 
Resampling results:

  ROC        Sens  Spec
  0.8503333  0.64  0.88


Mejores hiperparámetros:
  parameter
1      none
Confusion Matrix and Statistics

          Reference
Prediction  D  C
         D 20  3
         C  5 34
                                          
               Accuracy : 0.871           
                 95% CI : (0.7615, 0.9426)
    No Information Rate : 0.5968          
    P-Value [Acc > NIR] : 2.335e-06       
                                          
                  Kappa : 0.7284          
                                          
 Mcnemar's Test P-Value : 0.7237          
                                          
            Sensitivity : 0.8000          
            Specificity : 0.9189          
         Pos Pred Value : 0.8696          
         Neg Pred Value : 0.8718          
              Precision : 0.8696          
                 Recall : 0.8000          
                     F1 : 0.8333          
             Prevalence : 0.4032          
         Detection Rate : 0.3226          
   Detection Prevalence : 0.3710          
      Balanced Accuracy : 0.8595          
                                          
       'Positive' Class : D               
                                          

Call:
roc.default(response = real, predictor = as.numeric(probabilidad_positiva),     levels = c(negativo, positivo), direction = "<", quiet = TRUE)

Data: as.numeric(probabilidad_positiva) in 37 controls (real C) < 25 cases (real D).
Area under the curve: 0.8941
[OK] Tabla guardada en: resultados/14B_glm_predicciones_test.csv
[OK] Tabla guardada en: resultados/14B_glm_matriz_confusion.csv

============================================================
ENTRENANDO: knn 
============================================================
k-Nearest Neighbors 

250 samples
  3 predictor
  2 classes: 'D', 'C' 

No pre-processing
Resampling: Cross-Validated (5 fold) 
Summary of sample sizes: 200, 200, 200, 200, 200 
Resampling results across tuning parameters:

  k   ROC        Sens  Spec     
   5  0.7768333  0.60  0.8400000
   7  0.7860000  0.61  0.8533333
   9  0.8008333  0.60  0.8733333
  11  0.8166667  0.59  0.8666667
  13  0.8215000  0.61  0.8733333
  15  0.8200000  0.59  0.8866667
  17  0.8203333  0.56  0.9000000
  19  0.8213333  0.57  0.8866667

ROC was used to select the optimal model using the largest value.
The final value used for the model was k = 13.

Mejores hiperparámetros:
   k
5 13
Confusion Matrix and Statistics

          Reference
Prediction  D  C
         D 18  6
         C  7 31
                                          
               Accuracy : 0.7903          
                 95% CI : (0.6682, 0.8834)
    No Information Rate : 0.5968          
    P-Value [Acc > NIR] : 0.001009        
                                          
                  Kappa : 0.5615          
                                          
 Mcnemar's Test P-Value : 1.000000        
                                          
            Sensitivity : 0.7200          
            Specificity : 0.8378          
         Pos Pred Value : 0.7500          
         Neg Pred Value : 0.8158          
              Precision : 0.7500          
                 Recall : 0.7200          
                     F1 : 0.7347          
             Prevalence : 0.4032          
         Detection Rate : 0.2903          
   Detection Prevalence : 0.3871          
      Balanced Accuracy : 0.7789          
                                          
       'Positive' Class : D               
                                          

Call:
roc.default(response = real, predictor = as.numeric(probabilidad_positiva),     levels = c(negativo, positivo), direction = "<", quiet = TRUE)

Data: as.numeric(probabilidad_positiva) in 37 controls (real C) < 25 cases (real D).
Area under the curve: 0.8643
[OK] Tabla guardada en: resultados/14B_knn_predicciones_test.csv
[OK] Tabla guardada en: resultados/14B_knn_matriz_confusion.csv

============================================================
ENTRENANDO: rpart 
============================================================
CART 

250 samples
  3 predictor
  2 classes: 'D', 'C' 

No pre-processing
Resampling: Cross-Validated (5 fold) 
Summary of sample sizes: 200, 200, 200, 200, 200 
Resampling results across tuning parameters:

  cp          ROC        Sens  Spec     
  0.00000000  0.7501667  0.65  0.7800000
  0.06285714  0.7560000  0.67  0.8200000
  0.12571429  0.7483333  0.71  0.7866667
  0.18857143  0.7483333  0.71  0.7866667
  0.25142857  0.7483333  0.71  0.7866667
  0.31428571  0.7483333  0.71  0.7866667
  0.37714286  0.7483333  0.71  0.7866667
  0.44000000  0.6916667  0.55  0.8333333

ROC was used to select the optimal model using the largest value.
The final value used for the model was cp = 0.06285714.

Mejores hiperparámetros:
          cp
2 0.06285714
Confusion Matrix and Statistics

          Reference
Prediction  D  C
         D 21  9
         C  4 28
                                          
               Accuracy : 0.7903          
                 95% CI : (0.6682, 0.8834)
    No Information Rate : 0.5968          
    P-Value [Acc > NIR] : 0.001009        
                                          
                  Kappa : 0.578           
                                          
 Mcnemar's Test P-Value : 0.267257        
                                          
            Sensitivity : 0.8400          
            Specificity : 0.7568          
         Pos Pred Value : 0.7000          
         Neg Pred Value : 0.8750          
              Precision : 0.7000          
                 Recall : 0.8400          
                     F1 : 0.7636          
             Prevalence : 0.4032          
         Detection Rate : 0.3387          
   Detection Prevalence : 0.4839          
      Balanced Accuracy : 0.7984          
                                          
       'Positive' Class : D               
                                          

Call:
roc.default(response = real, predictor = as.numeric(probabilidad_positiva),     levels = c(negativo, positivo), direction = "<", quiet = TRUE)

Data: as.numeric(probabilidad_positiva) in 37 controls (real C) < 25 cases (real D).
Area under the curve: 0.7984
[OK] Tabla guardada en: resultados/14B_rpart_predicciones_test.csv
[OK] Tabla guardada en: resultados/14B_rpart_matriz_confusion.csv

========================================================================
PASO 5: Comparar el rendimiento de los modelos
========================================================================
  Modelo  Accuracy Precision Recall        F1       AUC
1    glm 0.8709677 0.8695652   0.80 0.8333333 0.8940541
2    knn 0.7903226 0.7500000   0.72 0.7346939 0.8643243
3  rpart 0.7903226 0.7000000   0.84 0.7636364 0.7983784
[OK] Tabla guardada en: resultados/14B_comparacion_modelos.csv

========================================================================
PASO 6: Guardar curva ROC comparada
========================================================================
[OK] Curva ROC guardada en resultados/14B_curvas_roc_comparadas.png

========================================================================
PASO 7: Generar respuesta de reflexión
========================================================================
============================================================
BORRADOR DE RESPUESTA: CLASIFICACIÓN SUPERVISADA
============================================================

Se utilizó el conjunto reducido de la parte 1 (coordenadas PCA 3D) y se dividió en train/test con proporción 0.8.
Se entrenaron tres modelos supervisados: regresión logística, kNN y árbol de decisión.
La clase positiva se fijó en 'D' para evaluar el evento clínico de no supervivencia.
El mejor modelo según AUC y F1 fue 'glm' con Accuracy = 0.871, Precision = 0.87, Recall = 0.8, F1 = 0.833 y AUC = 0.894.

Conclusión: si un modelo tiene mayor AUC y F1, separa mejor las clases y mantiene un mejor equilibrio entre falsos positivos y falsos negativos.
En esta configuración, 'glm' fue el más conveniente porque ofreció el mejor compromiso global entre discriminación y equilibrio de clases.
 

============================================================
ANÁLISIS SUPERVISADO COMPLETADO
============================================================
Revisa la carpeta resultados/. Encontrarás:
1) Predicciones de cada modelo.
2) Matrices de confusión.
3) Comparación de métricas.
4) Curva ROC comparada.
5) Interpretación final lista para el examen.
============================================================
> source("/workspaces/R_Studio/Plantillas_Examen_IA_R/14A_EXAMEN_METODO_NO_SUPERVISADO_2D_3D_CIRRHOSIS.R", encoding = "UTF-8")

========================================================================
PASO 0: Comprobar configuración y paquetes
========================================================================
[OK] Configuración válida y paquetes disponibles.

========================================================================
PASO 1: Cargar e inspeccionar el conjunto de datos
========================================================================
[OK] CSV leído: 312 filas y 19 columnas.

Resumen de datos_originales:
- Dimensiones: 312 filas x 19 columnas
- Filas duplicadas: 0
- Valores ausentes: 64
- Tipos de columnas:

character   integer   numeric 
        7         7         5 

Primeras filas:
  N_Days            Drug   Age Sex Ascites Hepatomegaly Spiders Edema Bilirubin
1    400 D-penicillamine 21464   F       Y            Y       Y     Y      14.5
2   4500 D-penicillamine 20617   F       N            Y       Y     N       1.1
3   1012 D-penicillamine 25594   M       N            N       N     S       1.4
4   1925 D-penicillamine 19994   F       N            Y       Y     S       1.8
5   1504         Placebo 13918   F       N            Y       Y     N       3.4
6   2503         Placebo 24201   F       N            Y       N     N       0.8
  Cholesterol Albumin Copper Alk_Phos   SGOT Tryglicerides Platelets
1         261    2.60    156   1718.0 137.95           172       190
2         302    4.14     54   7394.8 113.52            88       221
3         176    3.48    210    516.0  96.10            55       151
4         244    2.54     64   6121.8  60.63            92       183
5         279    3.53    143    671.0 113.15            72       136
6         248    3.98     50    944.0  93.00            63        NA
  Prothrombin Stage Status
1        12.2     4      D
2        10.6     3      C
3        12.0     4      D
4        10.3     4      D
5        10.9     3      C
6        11.0     3      D

Distribución de la variable de referencia (solo descriptiva):

  C   D 
187 125 

========================================================================
PASO 1.1: Resumen de valores ausentes y descriptivos básicos
========================================================================
[OK] Tabla guardada en: resultados/14A_resumen_na.csv

Columnas con más NA:
                   Variable Numero_NA Porcentaje_NA
Tryglicerides Tryglicerides        30     9.6153846
Cholesterol     Cholesterol        28     8.9743590
Platelets         Platelets         4     1.2820513
Copper               Copper         2     0.6410256
Age                     Age         0     0.0000000
Albumin             Albumin         0     0.0000000
Alk_Phos           Alk_Phos         0     0.0000000
Ascites             Ascites         0     0.0000000
Bilirubin         Bilirubin         0     0.0000000
Drug                   Drug         0     0.0000000
[OK] Tabla guardada en: resultados/14A_resumen_numerico.csv

Resumen numérico:
                   Variable        Media  Mediana       Desvio  Minimo   Maximo
N_Days               N_Days  2006.362179  1839.50 1123.2808430   41.00  4556.00
Age                     Age 18269.442308 18187.50 3864.8054068 9598.00 28650.00
Bilirubin         Bilirubin     3.256090     1.35    4.5303153    0.30    28.00
Cholesterol     Cholesterol   369.510563   309.50  231.9445450  120.00  1775.00
Albumin             Albumin     3.520000     3.55    0.4198920    1.96     4.64
Copper               Copper    97.648387    73.00   85.6139199    4.00   588.00
Alk_Phos           Alk_Phos  1982.655769  1259.00 2140.3888245  289.00 13862.40
SGOT                   SGOT   122.556346   114.70   56.6995249   26.35   457.25
Tryglicerides Tryglicerides   124.702128   108.00   65.1486387   33.00   598.00
Platelets         Platelets   261.935065   257.00   95.6087423   62.00   563.00
Prothrombin     Prothrombin    10.725641    10.60    1.0043232    9.00    17.10
Stage                 Stage     3.032051     3.00    0.8778802    1.00     4.00

Frecuencias de variables categóricas:

--- Drug ---

D-penicillamine         Placebo 
            158             154 

--- Sex ---

  F   M 
276  36 

--- Ascites ---

  N   Y 
288  24 

--- Hepatomegaly ---

  N   Y 
152 160 

--- Spiders ---

  N   Y 
222  90 

--- Edema ---

  N   S   Y 
263  29  20 

--- Status ---

  C   D 
187 125 

========================================================================
PASO 2: Preparar la matriz numérica para el método no supervisado
========================================================================
[OK] Valores ausentes imputados con la mediana de cada variable.

Matriz utilizada en el análisis no supervisado:
- Muestras: 312 
- Variables: 12 

========================================================================
PASO 3: Aplicar PCA y extraer 2D / 3D
========================================================================
[OK] Tabla guardada en: resultados/14A_varianza_explicada_pca.csv

Varianza explicada por PC1 + PC2: 41.3 %
Varianza explicada por PC1 + PC2 + PC3: 50.81 %

========================================================================
PASO 4: Aplicar K-means y calcular silueta
========================================================================

Silueta media en 2D: 0.4277 
Silueta media en 3D: 0.3478 

========================================================================
PASO 5: Guardar coordenadas y clústeres
========================================================================
[OK] Tabla guardada en: resultados/14A_no_supervisado_coordenadas.csv

Comparación posterior entre clústeres y Status (solo descriptiva):
       Status
Cluster   C   D
      1 151  34
      2  26  63
      3  10  28

========================================================================
PASO 6: Crear la visualización 2D
========================================================================
[OK] Gráfico guardado en: resultados/14A_pca_2d_clusters.png
[OK] Gráfico guardado en: resultados/14A_pca_2d_status.png

========================================================================
PASO 7: Crear la visualización 3D
========================================================================
[OK] Gráfico 3D guardado correctamente.
[OK] Gráfico 3D por Status guardado correctamente.

========================================================================
PASO 8: Generar una interpretación para el examen
========================================================================
============================================================
BORRADOR DE RESPUESTA: REDUCCIÓN DE DIMENSIONES NO SUPERVISADA
============================================================

Se cargó el conjunto cirrhosis.csv y se realizó una inspección básica de tipos, valores ausentes y estadísticas descriptivas.
Las variables numéricas se limpiaron, se imputaron con la mediana cuando fue necesario y se estandarizaron antes de aplicar PCA.
La variable de referencia 'Status' no participó en el ajuste no supervisado; solo se utilizó para una comparación descriptiva posterior.
PCA se redujo a 2D y 3D. La varianza explicada por PC1+PC2 fue de 41.3% y por PC1+PC2+PC3 de 50.81%.
Sobre las coordenadas reducidas se aplicó K-means con k = 3 para visualizar agrupamientos potenciales.
La silueta media fue 0.428 en 2D y 0.348 en 3D. La representación más favorable fue la de 2D.
La estructura de grupos es débil y debe interpretarse con cautela.

Conclusión: PCA es una opción segura para este conjunto porque permite resumir la información numérica en pocas componentes y facilita la comparación visual en 2D/3D.
 

============================================================
ANÁLISIS NO SUPERVISADO COMPLETADO
============================================================
Revisa la carpeta resultados/. Encontrarás:
1) Resumen de NA y descriptivos.
2) Tabla de varianza explicada de PCA.
3) Coordenadas reducidas y clústeres.
4) Gráficos 2D y 3D.
5) Interpretación final lista para el examen.
============================================================

Para El aparate de Supervisado con Cirrhosis he seguido la misma dinamica.

El 13 original era el fichero que tenía preparado. y solo he tenido que modificar las variables.

Este es el 13 original:

############################################################
# CONFIGURACIÓN: SOLO EDITAR AQUÍ
############################################################
CONFIG <- list(

  # CSV situado dentro de la carpeta data/.
  archivo = file.path("data", "dataset_expresiongenes_cancer.csv"),

  # Variable que se desea predecir.
  objetivo = "primaryormetastasis",

  # Identificadores u otras columnas que no deben usarse como predictores.
  columnas_excluir = c("id", "ID", "sample", "patient"),

  # Límite de predictores numéricos. NULL utiliza todos los válidos.
  max_predictores = 50,

  # Escribe exactamente DOS métodos distintos.
  modelos_supervisados = c("knn", "svmRadial"),

  # Clase positiva para sensibilidad, especificidad, F1 y ROC.
  # NULL selecciona automáticamente el primer nivel en problemas binarios.
  clase_positiva = NULL,

  # Proporción destinada al entrenamiento.
  proporcion_train = 0.80,

  # Número máximo de folds de validación cruzada.
  folds = 5,

  # Número de combinaciones de hiperparámetros probadas por caret.
  tune_length = 8,

  # Semilla para reproducibilidad.
  semilla = 1995
)

Y esta es la modificación para el 14:

CONFIG <- list(
  archivo_reducido = file.path("resultados", "14A_no_supervisado_coordenadas.csv"),
  objetivo = "Status",
  clase_positiva = "D",
  columnas_modelo = c("Dim1_3D", "Dim2_3D", "Dim3_3D"),
  modelos = c("glm", "knn", "rpart"),
  proporcion_train = 0.80,
  folds = 5,
  tune_length = 8,
  semilla = 1995
)

Ahora vamos al analisis del 14:

========================================================================
PASO 0: Comprobar configuración y paquetes
========================================================================
[OK] Paquetes disponibles.

========================================================================
PASO 1: Cargar e inspeccionar el conjunto reducido
========================================================================
[OK] CSV leído: 312 filas y 9 columnas.

Resumen de datos_reducidos:
- Dimensiones: 312 filas x 9 columnas
- Filas duplicadas: 0
- Valores ausentes: 0
- Tipos de columnas:

character   integer   numeric 
        1         3         5 

Primeras filas:
  muestra    Dim1_2D    Dim2_2D Cluster_2D    Dim1_3D    Dim2_3D    Dim3_3D
1       1 -3.7475961 -1.4191169          2 -3.7475961 -1.4191169 -0.3712262
2       2  1.6530015  0.5391647          1  1.6530015  0.5391647 -2.1627686
3       3 -1.0451872 -2.9807548          2 -1.0451872 -2.9807548 -0.4805517
4       4 -0.5676489 -1.3798222          2 -0.5676489 -1.3798222 -1.4457570
5       5 -0.1018042 -0.8385998          2 -0.1018042 -0.8385998  1.2332200
6       6  1.3293454 -1.3073654          1  1.3293454 -1.3073654 -0.4829584
  Cluster_3D Status
1          2      D
2          1      C
3          2      D
4          2      D
5          2      C
6          1      D

Distribución original de la variable objetivo:

  C   D 
187 125 

========================================================================
PASO 1.1: Resumen básico de valores ausentes y variables
========================================================================
[OK] Tabla guardada en: resultados/14B_resumen_na.csv

========================================================================
PASO 2: Preparar variables para clasificación
========================================================================

Clases utilizadas en el problema supervisado:

  D   C 
125 187 
Niveles, en orden: D, C 
Clase positiva: D 

========================================================================
PASO 3: Dividir en entrenamiento y prueba
========================================================================
[OK] Partición creada: train=250, test=62.

Distribución en TRAIN:

  D   C 
100 150 

Distribución en TEST:

 D  C 
25 37 

========================================================================
PASO 3.1: Estandarizar usando solo TRAIN
========================================================================

========================================================================
PASO 4: Entrenar modelos supervisados
========================================================================

============================================================
ENTRENANDO: glm 
============================================================
Generalized Linear Model 

250 samples
  3 predictor
  2 classes: 'D', 'C' 

No pre-processing
Resampling: Cross-Validated (5 fold) 
Summary of sample sizes: 200, 200, 200, 200, 200 
Resampling results:

  ROC        Sens  Spec
  0.8503333  0.64  0.88


Mejores hiperparámetros:
  parameter
1      none
Confusion Matrix and Statistics

          Reference
Prediction  D  C
         D 20  3
         C  5 34
                                          
               Accuracy : 0.871           
                 95% CI : (0.7615, 0.9426)
    No Information Rate : 0.5968          
    P-Value [Acc > NIR] : 2.335e-06       
                                          
                  Kappa : 0.7284          
                                          
 Mcnemar's Test P-Value : 0.7237          
                                          
            Sensitivity : 0.8000          
            Specificity : 0.9189          
         Pos Pred Value : 0.8696          
         Neg Pred Value : 0.8718          
              Precision : 0.8696          
                 Recall : 0.8000          
                     F1 : 0.8333          
             Prevalence : 0.4032          
         Detection Rate : 0.3226          
   Detection Prevalence : 0.3710          
      Balanced Accuracy : 0.8595          
                                          
       'Positive' Class : D               
                                          

Call:
roc.default(response = real, predictor = as.numeric(probabilidad_positiva),     levels = c(negativo, positivo), direction = "<", quiet = TRUE)

Data: as.numeric(probabilidad_positiva) in 37 controls (real C) < 25 cases (real D).
Area under the curve: 0.8941
[OK] Tabla guardada en: resultados/14B_glm_predicciones_test.csv
[OK] Tabla guardada en: resultados/14B_glm_matriz_confusion.csv

============================================================
ENTRENANDO: knn 
============================================================
k-Nearest Neighbors 

250 samples
  3 predictor
  2 classes: 'D', 'C' 

No pre-processing
Resampling: Cross-Validated (5 fold) 
Summary of sample sizes: 200, 200, 200, 200, 200 
Resampling results across tuning parameters:

  k   ROC        Sens  Spec     
   5  0.7768333  0.60  0.8400000
   7  0.7860000  0.61  0.8533333
   9  0.8008333  0.60  0.8733333
  11  0.8166667  0.59  0.8666667
  13  0.8215000  0.61  0.8733333
  15  0.8200000  0.59  0.8866667
  17  0.8203333  0.56  0.9000000
  19  0.8213333  0.57  0.8866667

ROC was used to select the optimal model using the largest value.
The final value used for the model was k = 13.

Mejores hiperparámetros:
   k
5 13
Confusion Matrix and Statistics

          Reference
Prediction  D  C
         D 18  6
         C  7 31
                                          
               Accuracy : 0.7903          
                 95% CI : (0.6682, 0.8834)
    No Information Rate : 0.5968          
    P-Value [Acc > NIR] : 0.001009        
                                          
                  Kappa : 0.5615          
                                          
 Mcnemar's Test P-Value : 1.000000        
                                          
            Sensitivity : 0.7200          
            Specificity : 0.8378          
         Pos Pred Value : 0.7500          
         Neg Pred Value : 0.8158          
              Precision : 0.7500          
                 Recall : 0.7200          
                     F1 : 0.7347          
             Prevalence : 0.4032          
         Detection Rate : 0.2903          
   Detection Prevalence : 0.3871          
      Balanced Accuracy : 0.7789          
                                          
       'Positive' Class : D               
                                          

Call:
roc.default(response = real, predictor = as.numeric(probabilidad_positiva),     levels = c(negativo, positivo), direction = "<", quiet = TRUE)

Data: as.numeric(probabilidad_positiva) in 37 controls (real C) < 25 cases (real D).
Area under the curve: 0.8643
[OK] Tabla guardada en: resultados/14B_knn_predicciones_test.csv
[OK] Tabla guardada en: resultados/14B_knn_matriz_confusion.csv

============================================================
ENTRENANDO: rpart 
============================================================
CART 

250 samples
  3 predictor
  2 classes: 'D', 'C' 

No pre-processing
Resampling: Cross-Validated (5 fold) 
Summary of sample sizes: 200, 200, 200, 200, 200 
Resampling results across tuning parameters:

  cp          ROC        Sens  Spec     
  0.00000000  0.7501667  0.65  0.7800000
  0.06285714  0.7560000  0.67  0.8200000
  0.12571429  0.7483333  0.71  0.7866667
  0.18857143  0.7483333  0.71  0.7866667
  0.25142857  0.7483333  0.71  0.7866667
  0.31428571  0.7483333  0.71  0.7866667
  0.37714286  0.7483333  0.71  0.7866667
  0.44000000  0.6916667  0.55  0.8333333

ROC was used to select the optimal model using the largest value.
The final value used for the model was cp = 0.06285714.

Mejores hiperparámetros:
          cp
2 0.06285714
Confusion Matrix and Statistics

          Reference
Prediction  D  C
         D 21  9
         C  4 28
                                          
               Accuracy : 0.7903          
                 95% CI : (0.6682, 0.8834)
    No Information Rate : 0.5968          
    P-Value [Acc > NIR] : 0.001009        
                                          
                  Kappa : 0.578           
                                          
 Mcnemar's Test P-Value : 0.267257        
                                          
            Sensitivity : 0.8400          
            Specificity : 0.7568          
         Pos Pred Value : 0.7000          
         Neg Pred Value : 0.8750          
              Precision : 0.7000          
                 Recall : 0.8400          
                     F1 : 0.7636          
             Prevalence : 0.4032          
         Detection Rate : 0.3387          
   Detection Prevalence : 0.4839          
      Balanced Accuracy : 0.7984          
                                          
       'Positive' Class : D               
                                          

Call:
roc.default(response = real, predictor = as.numeric(probabilidad_positiva),     levels = c(negativo, positivo), direction = "<", quiet = TRUE)

Data: as.numeric(probabilidad_positiva) in 37 controls (real C) < 25 cases (real D).
Area under the curve: 0.7984
[OK] Tabla guardada en: resultados/14B_rpart_predicciones_test.csv
[OK] Tabla guardada en: resultados/14B_rpart_matriz_confusion.csv

========================================================================
PASO 5: Comparar el rendimiento de los modelos
========================================================================
  Modelo  Accuracy Precision Recall        F1       AUC
1    glm 0.8709677 0.8695652   0.80 0.8333333 0.8940541
2    knn 0.7903226 0.7500000   0.72 0.7346939 0.8643243
3  rpart 0.7903226 0.7000000   0.84 0.7636364 0.7983784
[OK] Tabla guardada en: resultados/14B_comparacion_modelos.csv

========================================================================
PASO 6: Guardar curva ROC comparada
========================================================================
[OK] Curva ROC guardada en resultados/14B_curvas_roc_comparadas.png

========================================================================
PASO 7: Generar respuesta de reflexión
========================================================================
============================================================
BORRADOR DE RESPUESTA: CLASIFICACIÓN SUPERVISADA
============================================================

Se utilizó el conjunto reducido de la parte 1 (coordenadas PCA 3D) y se dividió en train/test con proporción 0.8.
Se entrenaron tres modelos supervisados: regresión logística, kNN y árbol de decisión.
La clase positiva se fijó en 'D' para evaluar el evento clínico de no supervivencia.
El mejor modelo según AUC y F1 fue 'glm' con Accuracy = 0.871, Precision = 0.87, Recall = 0.8, F1 = 0.833 y AUC = 0.894.

Conclusión: si un modelo tiene mayor AUC y F1, separa mejor las clases y mantiene un mejor equilibrio entre falsos positivos y falsos negativos.
En esta configuración, 'glm' fue el más conveniente porque ofreció el mejor compromiso global entre discriminación y equilibrio de clases.
 

============================================================
ANÁLISIS SUPERVISADO COMPLETADO
============================================================
Revisa la carpeta resultados/. Encontrarás:
1) Predicciones de cada modelo.
2) Matrices de confusión.
3) Comparación de métricas.
4) Curva ROC comparada.
5) Interpretación final lista para el examen.
============================================================




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