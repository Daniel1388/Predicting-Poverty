# Predicting-Poverty

# Problem Set 2: Predicting Poverty
Desarrollado por John Daniel Delgado Vargas, José Julian Parra Montoya y Alison Gissell Ruiz Ruiz.

### Introducción

El objetivo de este proyecto es predecir si un hogar es pobre utilizando la menor cantidad de variables posibles.

El análisis se desarrolla en 5 etapas:

* Limpieza y entendimiento de datos
* Desarrollo de estrategias de selección de variables: regularización, ordenamiento de XGBoost y RFE
* Entrenamiento y selección de modelos
* Consolidación del mejor modelo de predicción.

### Tabla de contenido
-  [Install](#install)
-  [Data](#data)
-  [Scripts](#scripts)
-  [Informe](#informe)

### Install

Este proyecto requiere R y las siguientes librerias instaladas

* library(pacman)
* library(here)
* library(ggplot2)#graficar
* library(tidyverse)#organizar datos
* library(xtable)#tablas
* library(openxlsx)#lectura xlsx
* library(data.table)#manejo de dataframes
* library(kableExtra)#tablas
* library(scales)#graficos
* library(caret)#entrenamiento de modelos
* library(doParallel) #paralelización
* library(randomForest)#algoritmo random forest
* library(fastDummies)# one hot encoding
* library(gamlr)#regularización
* library(xgboost)#algoritmo xgboost
* library(themis)#resampling

Para instalarlas se debe correr el comando install.packages, a continuación un ejemplo de esto.

```bash
install.packages("sandwich")
```

### Data

En la carpeta [`stores`](https://github.com/Daniel1388/Predicting-Poverty/tree/main/stores) se encuentra el set de datos en excel cada uno procesado por los scripts respectivos de los algoritmos como:

* xgb_reg.R.R

Que sirve para ejecurar los scripts: "xgb_clas.R" y "xgb_reg.R.

La Descripción de las variables se puede encontrar en:  https://www.dane.gov.co/index.php/estadisticas-por-tema/mercado-laboral/empleo-y-desempleo/geih-historicos


### Scripts

El proyecto cuenta con los siguientes scripts de R:

* [`descriptive_analysis.R`](https://github.com/Daniel1388/Predicting-Poverty/blob/main/scripts/descriptive_analysis.R)
* [`funcion_descriptivas.R`](https://github.com/Daniel1388/Predicting-Poverty/blob/main/scripts/funcion_descriptivas.R)
* [`regular_lasso_ridge.R`](https://github.com/Daniel1388/Predicting-Poverty/blob/main/scripts/regular_lasso_ridge.R)
* [`regular_regresion.R`](https://github.com/Daniel1388/Predicting-Poverty/blob/main/scripts/regular_regresion.R)
* [`rfe_selection_train_pred.R`](https://github.com/Daniel1388/Predicting-Poverty/blob/main/scripts/rfe_selection_train_pred.R)
* [`xgb_clas.R`](https://github.com/Daniel1388/Predicting-Poverty/blob/main/scripts/xgb_clas.R)
* [`xgb_datapreprocessing.R`](https://github.com/Daniel1388/Predicting-Poverty/blob/main/scripts/xgb_datapreprocessing.R)
* [`xgb_reg.R`](https://github.com/Daniel1388/Predicting-Poverty/blob/main/scripts/xgb_reg.R)
### Informe

El informe se encuentra en la carpeta [`document`](https://github.com/Daniel1388/Predicting-Poverty/blob/main/document/solucion_taller_2.tex) se encuentra en formato .tex y .pdf. En este archivo se resumen los resultados y se explica su interpretación.
