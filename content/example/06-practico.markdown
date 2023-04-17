---
title: "Análisis Factorial Exploratorio II (en construcción)"
linktitle: "6: Análisis Factorial Exploratorio II (en construcción)"
date: "2021-08-30"
menu:
  example:
    parent: Ejemplos
    weight: 6
type: docs
toc: true
editor_options: 
  chunk_output_type: console
---

## 0. Objetivo del práctico

El objetivo de este práctico revisar como realizar un Análisis factorial Exploratorio en R considerando la selección del número de factores, la extracción de factores, la rotación, la selección del modelo final y su interpretación, y el cálculo de puntuaciones factoriales. Este en una continuación del práctico anterior, donde realziamos la gestión de datos y la comprobación de los supuestos del análisis.


## 1. Carga y gestión de datos y librerias

Cargamos nuevamente la base de datos de PNUD 2015, que incluye los siguientes ítem. Con estos esperamos revisar si existen estructuras latentes en como las personas evalúan las oportunidades que entrega Chile. Además eliminamos los casos perdidos y atípicos. Para los detalles eestas decisiones, revisar el práctico anterior. 

![](https://raw.githubusercontent.com/Clases-GabrielSotomayor/pruebapagina/master/static/slides/img/05/Practico.png)


```r
datos <- read.csv2("https://raw.githubusercontent.com/Clases-GabrielSotomayor/pruebapagina/master/static/slides/data/EjemploAF.csv")
#Codificar perdidos como NA
datos[datos==9] <- NA
datos[datos==8] <- NA
#Eliminar perdidos
datosLW <- na.omit(datos)
#Tratamiento de casos atipicos
mean<-colMeans(datosLW[1:9])
Sx<-cov(datosLW[1:9]) #matriz de varianza covariaza 
D2<-mahalanobis(datosLW[1:9],mean,Sx)
datosLW$sigmahala=(1-pchisq(D2, 3))  
datosLW<-datosLW[which(datosLW$sigmahala>0.001),]#dar por perdido o eliminar caso atipico
datosLW$sigmahala<-NULL
```

Cargamos el paquete "psych" que nos permitirá realizar en análisis factorial exporatorio (AFE), así como otras funciones útiles para determinar el número de factores, calcular la matriz de correlaciones, ejecutar el análisis, visualizarlo, entre otras.


```r
library(psych)
```

```
## Warning: package 'psych' was built under R version 4.2.2
```

```r
library(GPArotation)
```

```
## Warning: package 'GPArotation' was built under R version 4.2.3
```

```
## 
## Attaching package: 'GPArotation'
```

```
## The following objects are masked from 'package:psych':
## 
##     equamax, varimin
```

## 2. Análisis factorial exploratorio


3.2 Gráfico de sedimentación
El gráfico de sedimentación (scree plot) nos ayuda a decidir cuántos factores mantener en el análisis. Para ello, utilizaremos la función scree() de la librería psych.


```r
scree(datosLW)
```

<img src="/example/06-practico_files/figure-html/unnamed-chunk-3-1.png" width="672" />

3.3 Selección del número de factores
Para decidir cuántos factores incluir en el análisis factorial, utilizaremos la función fa.parallel() de la librería psych. Esta función utiliza análisis paralelos para determinar cuántos factores se deben retener.


```r
nofactor <- fa.parallel(datosLW, fm="ml", fa="fa")
```

<img src="/example/06-practico_files/figure-html/unnamed-chunk-4-1.png" width="672" />

```
## Parallel analysis suggests that the number of factors =  3  and the number of components =  NA
```

```r
nofactor$fa.values
```

```
## [1]  6.30354064  0.29785640  0.09923415  0.04915881 -0.03041516 -0.03421303
## [7] -0.07018558 -0.15279026 -0.16218210
```

```r
sum(nofactor$fa.values > 1.0)
```

```
## [1] 1
```

```r
sum(nofactor$fa.values > 0.7)
```

```
## [1] 1
```

En este caso, el análisis paralelo sugiere retener 4 factores (valores propios mayores a 1) o 2 factores (valores propios mayores a 0.7).

3.4 Análisis factorial exploratorio con diferentes configuraciones
Ahora realizaremos el análisis factorial exploratorio utilizando diferentes configuraciones, como el número de factores, rotaciones y correlaciones. Esto nos permitirá comparar y seleccionar el mejor modelo.


```r
# Prueba de Modelos segun parallel y grafico
fa(datosLW, nfactors=4, rotate="none")
```

```
## Factor Analysis using method =  minres
## Call: fa(r = datosLW, nfactors = 4, rotate = "none")
## Standardized loadings (pattern matrix) based upon correlation matrix
##        MR1   MR2   MR3   MR4   h2    u2 com
## SALUD 0.80  0.11 -0.14  0.00 0.68 0.325 1.1
## INGR  0.84  0.38 -0.18  0.26 0.95 0.045 1.7
## TRAB  0.86 -0.02 -0.15 -0.11 0.77 0.228 1.1
## EDUC  0.89  0.05 -0.07 -0.20 0.84 0.156 1.1
## VIVI  0.89  0.01 -0.01 -0.18 0.82 0.182 1.1
## SEGUR 0.76  0.31  0.40 -0.01 0.84 0.164 1.9
## MEDIO 0.83 -0.11  0.14  0.05 0.72 0.276 1.1
## LIBER 0.88 -0.42  0.07  0.18 0.98 0.024 1.5
## PROYE 0.85 -0.27 -0.01  0.03 0.79 0.210 1.2
## 
##                        MR1  MR2  MR3  MR4
## SS loadings           6.42 0.52 0.27 0.19
## Proportion Var        0.71 0.06 0.03 0.02
## Cumulative Var        0.71 0.77 0.80 0.82
## Proportion Explained  0.87 0.07 0.04 0.03
## Cumulative Proportion 0.87 0.94 0.97 1.00
## 
## Mean item complexity =  1.3
## Test of the hypothesis that 4 factors are sufficient.
## 
## The degrees of freedom for the null model are  36  and the objective function was  8.74 with Chi Square of  12305.07
## The degrees of freedom for the model are 6  and the objective function was  0.02 
## 
## The root mean square of the residuals (RMSR) is  0 
## The df corrected root mean square of the residuals is  0.01 
## 
## The harmonic number of observations is  1412 with the empirical chi square  1.95  with prob <  0.92 
## The total number of observations was  1412  with Likelihood Chi Square =  27.31  with prob <  0.00013 
## 
## Tucker Lewis Index of factoring reliability =  0.99
## RMSEA index =  0.05  and the 90 % confidence intervals are  0.032 0.07
## BIC =  -16.2
## Fit based upon off diagonal values = 1
## Measures of factor score adequacy             
##                                                    MR1  MR2  MR3  MR4
## Correlation of (regression) scores with factors   0.99 0.95 0.80 0.82
## Multiple R square of scores with factors          0.98 0.90 0.64 0.68
## Minimum correlation of possible factor scores     0.96 0.81 0.28 0.36
```

```r
fa(datosLW, nfactors=1, rotate="none")
```

```
## Factor Analysis using method =  minres
## Call: fa(r = datosLW, nfactors = 1, rotate = "none")
## Standardized loadings (pattern matrix) based upon correlation matrix
##        MR1   h2   u2 com
## SALUD 0.81 0.65 0.35   1
## INGR  0.81 0.65 0.35   1
## TRAB  0.86 0.74 0.26   1
## EDUC  0.89 0.80 0.20   1
## VIVI  0.89 0.79 0.21   1
## SEGUR 0.73 0.53 0.47   1
## MEDIO 0.83 0.70 0.30   1
## LIBER 0.85 0.72 0.28   1
## PROYE 0.84 0.71 0.29   1
## 
##                MR1
## SS loadings    6.3
## Proportion Var 0.7
## 
## Mean item complexity =  1
## Test of the hypothesis that 1 factor is sufficient.
## 
## The degrees of freedom for the null model are  36  and the objective function was  8.74 with Chi Square of  12305.07
## The degrees of freedom for the model are 27  and the objective function was  0.78 
## 
## The root mean square of the residuals (RMSR) is  0.05 
## The df corrected root mean square of the residuals is  0.05 
## 
## The harmonic number of observations is  1412 with the empirical chi square  222.17  with prob <  1.4e-32 
## The total number of observations was  1412  with Likelihood Chi Square =  1097.41  with prob <  5.2e-214 
## 
## Tucker Lewis Index of factoring reliability =  0.884
## RMSEA index =  0.168  and the 90 % confidence intervals are  0.159 0.176
## BIC =  901.58
## Fit based upon off diagonal values = 1
## Measures of factor score adequacy             
##                                                    MR1
## Correlation of (regression) scores with factors   0.98
## Multiple R square of scores with factors          0.96
## Minimum correlation of possible factor scores     0.92
```

```r
# Prueba con 2 y 3 factores y correlación polychoric
fa(datosLW, nfactors=4, rotate="none", cor = "poly")
```

```
## Factor Analysis using method =  minres
## Call: fa(r = datosLW, nfactors = 4, rotate = "none", cor = "poly")
## Standardized loadings (pattern matrix) based upon correlation matrix
##        MR1   MR2   MR3   MR4   h2     u2 com
## SALUD 0.81  0.12 -0.16  0.02 0.70 0.2966 1.1
## INGR  0.86  0.36 -0.17  0.25 0.96 0.0423 1.6
## TRAB  0.87 -0.02 -0.16 -0.10 0.80 0.2011 1.1
## EDUC  0.91  0.05 -0.09 -0.19 0.87 0.1250 1.1
## VIVI  0.90  0.02 -0.02 -0.18 0.85 0.1532 1.1
## SEGUR 0.79  0.31  0.41 -0.02 0.90 0.1029 1.8
## MEDIO 0.85 -0.12  0.14  0.05 0.76 0.2401 1.1
## LIBER 0.89 -0.41  0.08  0.15 1.00 0.0049 1.5
## PROYE 0.87 -0.27  0.00  0.02 0.82 0.1757 1.2
## 
##                        MR1  MR2  MR3  MR4
## SS loadings           6.71 0.50 0.28 0.17
## Proportion Var        0.75 0.06 0.03 0.02
## Cumulative Var        0.75 0.80 0.83 0.85
## Proportion Explained  0.88 0.07 0.04 0.02
## Cumulative Proportion 0.88 0.94 0.98 1.00
## 
## Mean item complexity =  1.3
## Test of the hypothesis that 4 factors are sufficient.
## 
## The degrees of freedom for the null model are  36  and the objective function was  9.86 with Chi Square of  13879.23
## The degrees of freedom for the model are 6  and the objective function was  0.03 
## 
## The root mean square of the residuals (RMSR) is  0 
## The df corrected root mean square of the residuals is  0.01 
## 
## The harmonic number of observations is  1412 with the empirical chi square  1.82  with prob <  0.94 
## The total number of observations was  1412  with Likelihood Chi Square =  36.01  with prob <  2.7e-06 
## 
## Tucker Lewis Index of factoring reliability =  0.987
## RMSEA index =  0.06  and the 90 % confidence intervals are  0.042 0.079
## BIC =  -7.51
## Fit based upon off diagonal values = 1
## Measures of factor score adequacy             
##                                                    MR1  MR2  MR3  MR4
## Correlation of (regression) scores with factors   0.99 0.97 0.85 0.84
## Multiple R square of scores with factors          0.99 0.94 0.73 0.71
## Minimum correlation of possible factor scores     0.97 0.88 0.46 0.41
```

```r
fa(datosLW, nfactors=2, rotate="none", cor = "poly")
```

```
## Factor Analysis using method =  minres
## Call: fa(r = datosLW, nfactors = 2, rotate = "none", cor = "poly")
## Standardized loadings (pattern matrix) based upon correlation matrix
##        MR1   MR2   h2    u2 com
## SALUD 0.82  0.17 0.70 0.302 1.1
## INGR  0.84  0.30 0.80 0.203 1.2
## TRAB  0.87  0.04 0.76 0.236 1.0
## EDUC  0.91  0.10 0.84 0.164 1.0
## VIVI  0.90  0.06 0.82 0.181 1.0
## SEGUR 0.76  0.17 0.61 0.388 1.1
## MEDIO 0.85 -0.12 0.74 0.259 1.0
## LIBER 0.89 -0.40 0.95 0.048 1.4
## PROYE 0.87 -0.27 0.84 0.163 1.2
## 
##                        MR1  MR2
## SS loadings           6.64 0.41
## Proportion Var        0.74 0.05
## Cumulative Var        0.74 0.78
## Proportion Explained  0.94 0.06
## Cumulative Proportion 0.94 1.00
## 
## Mean item complexity =  1.1
## Test of the hypothesis that 2 factors are sufficient.
## 
## The degrees of freedom for the null model are  36  and the objective function was  9.86 with Chi Square of  13879.23
## The degrees of freedom for the model are 19  and the objective function was  0.33 
## 
## The root mean square of the residuals (RMSR) is  0.02 
## The df corrected root mean square of the residuals is  0.03 
## 
## The harmonic number of observations is  1412 with the empirical chi square  52.98  with prob <  4.7e-05 
## The total number of observations was  1412  with Likelihood Chi Square =  465.38  with prob <  1e-86 
## 
## Tucker Lewis Index of factoring reliability =  0.939
## RMSEA index =  0.129  and the 90 % confidence intervals are  0.119 0.139
## BIC =  327.58
## Fit based upon off diagonal values = 1
## Measures of factor score adequacy             
##                                                    MR1  MR2
## Correlation of (regression) scores with factors   0.99 0.89
## Multiple R square of scores with factors          0.97 0.79
## Minimum correlation of possible factor scores     0.95 0.59
```

```r
# Prueba con 3 factores y rotación varimax y promax
fa(datosLW, nfactors=4, rotate="varimax")
```

```
## Factor Analysis using method =  minres
## Call: fa(r = datosLW, nfactors = 4, rotate = "varimax")
## Standardized loadings (pattern matrix) based upon correlation matrix
##        MR2  MR1  MR4  MR3   h2    u2 com
## SALUD 0.39 0.47 0.48 0.26 0.68 0.325 3.5
## INGR  0.28 0.32 0.81 0.35 0.95 0.045 2.0
## TRAB  0.48 0.59 0.38 0.23 0.77 0.228 3.1
## EDUC  0.42 0.65 0.35 0.35 0.84 0.156 3.0
## VIVI  0.46 0.60 0.31 0.38 0.82 0.182 3.2
## SEGUR 0.27 0.27 0.31 0.77 0.84 0.164 1.9
## MEDIO 0.61 0.34 0.27 0.40 0.72 0.276 2.9
## LIBER 0.89 0.28 0.24 0.22 0.98 0.024 1.5
## PROYE 0.71 0.41 0.26 0.22 0.79 0.210 2.2
## 
##                        MR2  MR1  MR4  MR3
## SS loadings           2.59 1.89 1.54 1.36
## Proportion Var        0.29 0.21 0.17 0.15
## Cumulative Var        0.29 0.50 0.67 0.82
## Proportion Explained  0.35 0.26 0.21 0.18
## Cumulative Proportion 0.35 0.61 0.82 1.00
## 
## Mean item complexity =  2.6
## Test of the hypothesis that 4 factors are sufficient.
## 
## The degrees of freedom for the null model are  36  and the objective function was  8.74 with Chi Square of  12305.07
## The degrees of freedom for the model are 6  and the objective function was  0.02 
## 
## The root mean square of the residuals (RMSR) is  0 
## The df corrected root mean square of the residuals is  0.01 
## 
## The harmonic number of observations is  1412 with the empirical chi square  1.95  with prob <  0.92 
## The total number of observations was  1412  with Likelihood Chi Square =  27.31  with prob <  0.00013 
## 
## Tucker Lewis Index of factoring reliability =  0.99
## RMSEA index =  0.05  and the 90 % confidence intervals are  0.032 0.07
## BIC =  -16.2
## Fit based upon off diagonal values = 1
## Measures of factor score adequacy             
##                                                    MR2  MR1  MR4  MR3
## Correlation of (regression) scores with factors   0.97 0.84 0.93 0.84
## Multiple R square of scores with factors          0.94 0.70 0.86 0.70
## Minimum correlation of possible factor scores     0.88 0.41 0.71 0.40
```

```r
fa(datosLW, nfactors=4, rotate="promax")
```

```
## Factor Analysis using method =  minres
## Call: fa(r = datosLW, nfactors = 4, rotate = "promax")
## Standardized loadings (pattern matrix) based upon correlation matrix
##         MR1   MR2   MR4   MR3   h2    u2 com
## SALUD  0.46  0.10  0.34 -0.02 0.68 0.325 2.0
## INGR   0.00  0.00  0.92  0.08 0.95 0.045 1.0
## TRAB   0.72  0.16  0.11 -0.09 0.77 0.228 1.2
## EDUC   0.85  0.01  0.00  0.08 0.84 0.156 1.0
## VIVI   0.75  0.10 -0.06  0.14 0.82 0.182 1.1
## SEGUR  0.03 -0.02  0.06  0.86 0.84 0.164 1.0
## MEDIO  0.13  0.54  0.02  0.25 0.72 0.276 1.5
## LIBER -0.09  1.08  0.01 -0.04 0.98 0.024 1.0
## PROYE  0.28  0.69  0.00 -0.06 0.79 0.210 1.3
## 
##                        MR1  MR2  MR4  MR3
## SS loadings           2.72 2.37 1.25 1.05
## Proportion Var        0.30 0.26 0.14 0.12
## Cumulative Var        0.30 0.57 0.70 0.82
## Proportion Explained  0.37 0.32 0.17 0.14
## Cumulative Proportion 0.37 0.69 0.86 1.00
## 
##  With factor correlations of 
##      MR1  MR2  MR4  MR3
## MR1 1.00 0.82 0.79 0.73
## MR2 0.82 1.00 0.65 0.65
## MR4 0.79 0.65 1.00 0.69
## MR3 0.73 0.65 0.69 1.00
## 
## Mean item complexity =  1.2
## Test of the hypothesis that 4 factors are sufficient.
## 
## The degrees of freedom for the null model are  36  and the objective function was  8.74 with Chi Square of  12305.07
## The degrees of freedom for the model are 6  and the objective function was  0.02 
## 
## The root mean square of the residuals (RMSR) is  0 
## The df corrected root mean square of the residuals is  0.01 
## 
## The harmonic number of observations is  1412 with the empirical chi square  1.95  with prob <  0.92 
## The total number of observations was  1412  with Likelihood Chi Square =  27.31  with prob <  0.00013 
## 
## Tucker Lewis Index of factoring reliability =  0.99
## RMSEA index =  0.05  and the 90 % confidence intervals are  0.032 0.07
## BIC =  -16.2
## Fit based upon off diagonal values = 1
## Measures of factor score adequacy             
##                                                    MR1  MR2  MR4  MR3
## Correlation of (regression) scores with factors   0.97 0.99 0.98 0.93
## Multiple R square of scores with factors          0.94 0.98 0.95 0.86
## Minimum correlation of possible factor scores     0.88 0.96 0.91 0.73
```

```r
# Prueba con 3 factores, rotación y correlación polychoric
fa(datosLW, nfactors=2, rotate="varimax", cor = "poly")
```

```
## Factor Analysis using method =  minres
## Call: fa(r = datosLW, nfactors = 2, rotate = "varimax", cor = "poly")
## Standardized loadings (pattern matrix) based upon correlation matrix
##        MR1  MR2   h2    u2 com
## SALUD 0.72 0.42 0.70 0.302 1.6
## INGR  0.82 0.35 0.80 0.203 1.3
## TRAB  0.67 0.56 0.76 0.236 1.9
## EDUC  0.74 0.54 0.84 0.164 1.8
## VIVI  0.71 0.57 0.82 0.181 1.9
## SEGUR 0.68 0.39 0.61 0.388 1.6
## MEDIO 0.55 0.66 0.74 0.259 1.9
## LIBER 0.39 0.89 0.95 0.048 1.4
## PROYE 0.46 0.79 0.84 0.163 1.6
## 
##                        MR1  MR2
## SS loadings           3.82 3.24
## Proportion Var        0.42 0.36
## Cumulative Var        0.42 0.78
## Proportion Explained  0.54 0.46
## Cumulative Proportion 0.54 1.00
## 
## Mean item complexity =  1.7
## Test of the hypothesis that 2 factors are sufficient.
## 
## The degrees of freedom for the null model are  36  and the objective function was  9.86 with Chi Square of  13879.23
## The degrees of freedom for the model are 19  and the objective function was  0.33 
## 
## The root mean square of the residuals (RMSR) is  0.02 
## The df corrected root mean square of the residuals is  0.03 
## 
## The harmonic number of observations is  1412 with the empirical chi square  52.98  with prob <  4.7e-05 
## The total number of observations was  1412  with Likelihood Chi Square =  465.38  with prob <  1e-86 
## 
## Tucker Lewis Index of factoring reliability =  0.939
## RMSEA index =  0.129  and the 90 % confidence intervals are  0.119 0.139
## BIC =  327.58
## Fit based upon off diagonal values = 1
## Measures of factor score adequacy             
##                                                    MR1  MR2
## Correlation of (regression) scores with factors   0.93 0.95
## Multiple R square of scores with factors          0.86 0.90
## Minimum correlation of possible factor scores     0.72 0.81
```

```r
fa(datosLW, nfactors=4, rotate="promax", cor = "poly")
```

```
## Factor Analysis using method =  minres
## Call: fa(r = datosLW, nfactors = 4, rotate = "promax", cor = "poly")
## Standardized loadings (pattern matrix) based upon correlation matrix
##         MR1   MR2   MR4   MR3   h2     u2 com
## SALUD  0.42  0.09  0.41 -0.03 0.70 0.2966 2.1
## INGR  -0.01  0.00  0.92  0.09 0.96 0.0423 1.0
## TRAB   0.68  0.18  0.15 -0.09 0.80 0.2011 1.3
## EDUC   0.84  0.03  0.02  0.07 0.87 0.1250 1.0
## VIVI   0.74  0.11 -0.04  0.15 0.85 0.1532 1.1
## SEGUR  0.04 -0.02  0.06  0.89 0.90 0.1029 1.0
## MEDIO  0.10  0.57  0.03  0.25 0.76 0.2401 1.4
## LIBER -0.08  1.08  0.01 -0.04 1.00 0.0049 1.0
## PROYE  0.26  0.71  0.00 -0.05 0.82 0.1757 1.3
## 
##                        MR1  MR2  MR4  MR3
## SS loadings           2.63 2.51 1.40 1.11
## Proportion Var        0.29 0.28 0.16 0.12
## Cumulative Var        0.29 0.57 0.73 0.85
## Proportion Explained  0.34 0.33 0.18 0.15
## Cumulative Proportion 0.34 0.67 0.85 1.00
## 
##  With factor correlations of 
##      MR1  MR2  MR4  MR3
## MR1 1.00 0.83 0.81 0.73
## MR2 0.83 1.00 0.68 0.66
## MR4 0.81 0.68 1.00 0.70
## MR3 0.73 0.66 0.70 1.00
## 
## Mean item complexity =  1.3
## Test of the hypothesis that 4 factors are sufficient.
## 
## The degrees of freedom for the null model are  36  and the objective function was  9.86 with Chi Square of  13879.23
## The degrees of freedom for the model are 6  and the objective function was  0.03 
## 
## The root mean square of the residuals (RMSR) is  0 
## The df corrected root mean square of the residuals is  0.01 
## 
## The harmonic number of observations is  1412 with the empirical chi square  1.82  with prob <  0.94 
## The total number of observations was  1412  with Likelihood Chi Square =  36.01  with prob <  2.7e-06 
## 
## Tucker Lewis Index of factoring reliability =  0.987
## RMSEA index =  0.06  and the 90 % confidence intervals are  0.042 0.079
## BIC =  -7.51
## Fit based upon off diagonal values = 1
## Measures of factor score adequacy             
##                                                    MR1  MR2  MR4  MR3
## Correlation of (regression) scores with factors   0.97 1.00 0.98 0.95
## Multiple R square of scores with factors          0.94 1.00 0.96 0.90
## Minimum correlation of possible factor scores     0.89 0.99 0.92 0.81
```

```r
# Decidimos 3 por los valores en loading, pero SEGUR queda sola (es recomendable que 3 o más)
fa(datosLW, nfactors=4, rotate="promax", cor = "poly")
```

```
## Factor Analysis using method =  minres
## Call: fa(r = datosLW, nfactors = 4, rotate = "promax", cor = "poly")
## Standardized loadings (pattern matrix) based upon correlation matrix
##         MR1   MR2   MR4   MR3   h2     u2 com
## SALUD  0.42  0.09  0.41 -0.03 0.70 0.2966 2.1
## INGR  -0.01  0.00  0.92  0.09 0.96 0.0423 1.0
## TRAB   0.68  0.18  0.15 -0.09 0.80 0.2011 1.3
## EDUC   0.84  0.03  0.02  0.07 0.87 0.1250 1.0
## VIVI   0.74  0.11 -0.04  0.15 0.85 0.1532 1.1
## SEGUR  0.04 -0.02  0.06  0.89 0.90 0.1029 1.0
## MEDIO  0.10  0.57  0.03  0.25 0.76 0.2401 1.4
## LIBER -0.08  1.08  0.01 -0.04 1.00 0.0049 1.0
## PROYE  0.26  0.71  0.00 -0.05 0.82 0.1757 1.3
## 
##                        MR1  MR2  MR4  MR3
## SS loadings           2.63 2.51 1.40 1.11
## Proportion Var        0.29 0.28 0.16 0.12
## Cumulative Var        0.29 0.57 0.73 0.85
## Proportion Explained  0.34 0.33 0.18 0.15
## Cumulative Proportion 0.34 0.67 0.85 1.00
## 
##  With factor correlations of 
##      MR1  MR2  MR4  MR3
## MR1 1.00 0.83 0.81 0.73
## MR2 0.83 1.00 0.68 0.66
## MR4 0.81 0.68 1.00 0.70
## MR3 0.73 0.66 0.70 1.00
## 
## Mean item complexity =  1.3
## Test of the hypothesis that 4 factors are sufficient.
## 
## The degrees of freedom for the null model are  36  and the objective function was  9.86 with Chi Square of  13879.23
## The degrees of freedom for the model are 6  and the objective function was  0.03 
## 
## The root mean square of the residuals (RMSR) is  0 
## The df corrected root mean square of the residuals is  0.01 
## 
## The harmonic number of observations is  1412 with the empirical chi square  1.82  with prob <  0.94 
## The total number of observations was  1412  with Likelihood Chi Square =  36.01  with prob <  2.7e-06 
## 
## Tucker Lewis Index of factoring reliability =  0.987
## RMSEA index =  0.06  and the 90 % confidence intervals are  0.042 0.079
## BIC =  -7.51
## Fit based upon off diagonal values = 1
## Measures of factor score adequacy             
##                                                    MR1  MR2  MR4  MR3
## Correlation of (regression) scores with factors   0.97 1.00 0.98 0.95
## Multiple R square of scores with factors          0.94 1.00 0.96 0.90
## Minimum correlation of possible factor scores     0.89 0.99 0.92 0.81
```

```r
fa(datosLW, nfactors=2, rotate="promax", cor = "poly")
```

```
## Factor Analysis using method =  minres
## Call: fa(r = datosLW, nfactors = 2, rotate = "promax", cor = "poly")
## Standardized loadings (pattern matrix) based upon correlation matrix
##         MR1   MR2   h2    u2 com
## SALUD  0.77  0.08 0.70 0.302 1.0
## INGR   0.98 -0.11 0.80 0.203 1.0
## TRAB   0.59  0.33 0.76 0.236 1.6
## EDUC   0.71  0.24 0.84 0.164 1.2
## VIVI   0.64  0.31 0.82 0.181 1.4
## SEGUR  0.73  0.06 0.61 0.388 1.0
## MEDIO  0.32  0.58 0.74 0.259 1.6
## LIBER -0.09  1.04 0.95 0.048 1.0
## PROYE  0.10  0.83 0.84 0.163 1.0
## 
##                        MR1  MR2
## SS loadings           4.05 3.00
## Proportion Var        0.45 0.33
## Cumulative Var        0.45 0.78
## Proportion Explained  0.57 0.43
## Cumulative Proportion 0.57 1.00
## 
##  With factor correlations of 
##      MR1  MR2
## MR1 1.00 0.81
## MR2 0.81 1.00
## 
## Mean item complexity =  1.2
## Test of the hypothesis that 2 factors are sufficient.
## 
## The degrees of freedom for the null model are  36  and the objective function was  9.86 with Chi Square of  13879.23
## The degrees of freedom for the model are 19  and the objective function was  0.33 
## 
## The root mean square of the residuals (RMSR) is  0.02 
## The df corrected root mean square of the residuals is  0.03 
## 
## The harmonic number of observations is  1412 with the empirical chi square  52.98  with prob <  4.7e-05 
## The total number of observations was  1412  with Likelihood Chi Square =  465.38  with prob <  1e-86 
## 
## Tucker Lewis Index of factoring reliability =  0.939
## RMSEA index =  0.129  and the 90 % confidence intervals are  0.119 0.139
## BIC =  327.58
## Fit based upon off diagonal values = 1
## Measures of factor score adequacy             
##                                                    MR1  MR2
## Correlation of (regression) scores with factors   0.97 0.99
## Multiple R square of scores with factors          0.95 0.97
## Minimum correlation of possible factor scores     0.89 0.94
```

```r
# Modelo final
FINAL  <- fa(datosLW, nfactors=2, rotate="promax", cor = "poly")
Final2 <- fa(datosLW, nfactors=4, rotate="promax", cor = "poly")
```

Después de comparar los resultados, decidimos utilizar un modelo con 2 factores y rotación promax. También se puede probar con 4 factores, según las neces
