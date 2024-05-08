---
title: "Análisis Factorial Exploratorio I"
linktitle: "6: Análisis Factorial Exploratorio I"
date: "2023-04-29"
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

El objetivo de este práctico es revisar como realizar la comprobación de supuestos y tratamiento de variables para la realización de un Análisis factorial Exploratorio. 


## 1. Carga y gestión de datos

En primer lugar, cargaremos una base de datos de PNUD 2015, que incluye los siguientes ítem. Con estos esperamos revisar si existen estructuras latentes en como las personas evalúan las oportunidades que entrega Chile. 

![](https://raw.githubusercontent.com/Clases-GabrielSotomayor/pruebapagina/master/static/slides/img/05/Practico.png)

Estos datos están en formato csv (comma separated value), por lo cual podemos leerlos con la función *read.csv2* incluida con r base.


```r
#cargamos los datos 
datos <- read.csv2("https://raw.githubusercontent.com/Clases-GabrielSotomayor/pruebapagina/master/static/slides/data/EjemploAF.csv")
```


A continuación, revisamos los datos y daremos por perdidos los valores no sabe y no responde. 


```r
#   3.    DEFINIR VALORES PERDIDOS para Las variables, intervalares discretas de 7 categorias de las base de Datos PNUD 2015
summary(datos)
```

```
##      SALUD            INGR            TRAB            EDUC      
##  Min.   :1.000   Min.   :1.000   Min.   :1.000   Min.   :1.000  
##  1st Qu.:3.000   1st Qu.:2.000   1st Qu.:3.000   1st Qu.:3.000  
##  Median :4.000   Median :3.000   Median :4.000   Median :4.000  
##  Mean   :4.107   Mean   :3.503   Mean   :4.099   Mean   :3.976  
##  3rd Qu.:5.000   3rd Qu.:5.000   3rd Qu.:5.000   3rd Qu.:5.000  
##  Max.   :9.000   Max.   :9.000   Max.   :9.000   Max.   :9.000  
##       VIVI           SEGUR           MEDIO           LIBER      
##  Min.   :1.000   Min.   :1.000   Min.   :1.000   Min.   :1.000  
##  1st Qu.:3.000   1st Qu.:1.000   1st Qu.:3.000   1st Qu.:4.000  
##  Median :4.000   Median :3.000   Median :4.000   Median :5.000  
##  Mean   :4.078   Mean   :3.235   Mean   :4.182   Mean   :4.525  
##  3rd Qu.:5.000   3rd Qu.:5.000   3rd Qu.:5.000   3rd Qu.:6.000  
##  Max.   :9.000   Max.   :9.000   Max.   :9.000   Max.   :9.000  
##      PROYE      
##  Min.   :1.000  
##  1st Qu.:4.000  
##  Median :5.000  
##  Mean   :4.601  
##  3rd Qu.:6.000  
##  Max.   :9.000
```

```r
#Categorizar como NA 8 y 9 (NS/NR).
datos[datos==9] <- NA
datos[datos==8] <- NA
```

A continuación, exploramos los datos para conocer sus medias y distribución. En este punto es relevante revisar si existe mucha diferencia en sus niveles de variabilidad, ya que esto afectara los resultados del Análisis Factorial Exploratorio.


```r
#   4.    ANALISIS DESCRIPTIVO DE LAS VARIABLES. 
library(summarytools)
print(dfSummary(datos, headings = FALSE, method = "render"))
```

```
## 
## -------------------------------------------------------------------------------------------
## No   Variable    Stats / Values          Freqs (% of Valid)   Graph     Valid     Missing  
## ---- ----------- ----------------------- -------------------- --------- --------- ---------
## 1    SALUD       Mean (sd) : 4.1 (1.8)   1 : 218 (12.3%)      II        1769      11       
##      [integer]   min < med < max:        2 : 143 ( 8.1%)      I         (99.4%)   (0.6%)   
##                  1 < 4 < 7               3 : 251 (14.2%)      II                           
##                  IQR (CV) : 2 (0.4)      4 : 358 (20.2%)      IIII                         
##                                          5 : 415 (23.5%)      IIII                         
##                                          6 : 234 (13.2%)      II                           
##                                          7 : 150 ( 8.5%)      I                            
## 
## 2    INGR        Mean (sd) : 3.4 (1.7)   1 : 335 (19.1%)      III       1752      28       
##      [integer]   min < med < max:        2 : 235 (13.4%)      II        (98.4%)   (1.6%)   
##                  1 < 3 < 7               3 : 343 (19.6%)      III                          
##                  IQR (CV) : 3 (0.5)      4 : 340 (19.4%)      III                          
##                                          5 : 274 (15.6%)      III                          
##                                          6 : 138 ( 7.9%)      I                            
##                                          7 :  87 ( 5.0%)                                   
## 
## 3    TRAB        Mean (sd) : 4 (1.7)     1 : 178 (10.2%)      II        1749      31       
##      [integer]   min < med < max:        2 : 161 ( 9.2%)      I         (98.3%)   (1.7%)   
##                  1 < 4 < 7               3 : 265 (15.2%)      III                          
##                  IQR (CV) : 2 (0.4)      4 : 433 (24.8%)      IIII                         
##                                          5 : 380 (21.7%)      IIII                         
##                                          6 : 209 (11.9%)      II                           
##                                          7 : 123 ( 7.0%)      I                            
## 
## 4    EDUC        Mean (sd) : 3.8 (1.7)   1 : 227 (13.3%)      II        1712      68       
##      [integer]   min < med < max:        2 : 190 (11.1%)      II        (96.2%)   (3.8%)   
##                  1 < 4 < 7               3 : 295 (17.2%)      III                          
##                  IQR (CV) : 2 (0.5)      4 : 369 (21.6%)      IIII                         
##                                          5 : 340 (19.9%)      III                          
##                                          6 : 188 (11.0%)      II                           
##                                          7 : 103 ( 6.0%)      I                            
## 
## 5    VIVI        Mean (sd) : 4 (1.7)     1 : 208 (11.9%)      II        1746      34       
##      [integer]   min < med < max:        2 : 162 ( 9.3%)      I         (98.1%)   (1.9%)   
##                  1 < 4 < 7               3 : 276 (15.8%)      III                          
##                  IQR (CV) : 2 (0.4)      4 : 362 (20.7%)      IIII                         
##                                          5 : 383 (21.9%)      IIII                         
##                                          6 : 237 (13.6%)      II                           
##                                          7 : 118 ( 6.8%)      I                            
## 
## 6    SEGUR       Mean (sd) : 3.2 (1.8)   1 : 481 (27.3%)      IIIII     1764      16       
##      [integer]   min < med < max:        2 : 220 (12.5%)      II        (99.1%)   (0.9%)   
##                  1 < 3 < 7               3 : 297 (16.8%)      III                          
##                  IQR (CV) : 4 (0.6)      4 : 308 (17.5%)      III                          
##                                          5 : 238 (13.5%)      II                           
##                                          6 : 155 ( 8.8%)      I                            
##                                          7 :  65 ( 3.7%)                                   
## 
## 7    MEDIO       Mean (sd) : 4.1 (1.7)   1 : 188 (10.7%)      II        1763      17       
##      [integer]   min < med < max:        2 : 144 ( 8.2%)      I         (99.0%)   (1.0%)   
##                  1 < 4 < 7               3 : 247 (14.0%)      II                           
##                  IQR (CV) : 2 (0.4)      4 : 396 (22.5%)      IIII                         
##                                          5 : 364 (20.6%)      IIII                         
##                                          6 : 288 (16.3%)      III                          
##                                          7 : 136 ( 7.7%)      I                            
## 
## 8    LIBER       Mean (sd) : 4.5 (1.6)   1 : 140 ( 8.0%)      I         1757      23       
##      [integer]   min < med < max:        2 :  88 ( 5.0%)      I         (98.7%)   (1.3%)   
##                  1 < 5 < 7               3 : 202 (11.5%)      II                           
##                  IQR (CV) : 2 (0.4)      4 : 379 (21.6%)      IIII                         
##                                          5 : 445 (25.3%)      IIIII                        
##                                          6 : 325 (18.5%)      III                          
##                                          7 : 178 (10.1%)      II                           
## 
## 9    PROYE       Mean (sd) : 4.5 (1.6)   1 : 109 ( 6.2%)      I         1746      34       
##      [integer]   min < med < max:        2 :  94 ( 5.4%)      I         (98.1%)   (1.9%)   
##                  1 < 5 < 7               3 : 202 (11.6%)      II                           
##                  IQR (CV) : 2 (0.3)      4 : 370 (21.2%)      IIII                         
##                                          5 : 466 (26.7%)      IIIII                        
##                                          6 : 343 (19.6%)      III                          
##                                          7 : 162 ( 9.3%)      I                            
## -------------------------------------------------------------------------------------------
```

##  2.  Comprobación de supuestos

En este bloque de código, se cargan las bibliotecas necesarias para realizar la comprobación de supuestos.


```r
library(psych)
library(MVN)
```

Para la comprobación de supuestos partiremos por generar una base de datos listwise, es decir, en la que se eliminan todos los casos que tienen valores perdidos en alguno de los ítem. Esto es posible en este caso por el número moderado de casos perdidos existentes. Pasamos de tener 1780 casos a 1632.


```r
#   5a.   Crear una base solo con listwise (para test de Mardia)

datosLW <- na.omit(datos)
dim(datosLW)
```

```
## [1] 1632    9
```

A continuación, revisaremos la existencia de casos atípicos multivariantes a partir del cálculo y evaluación de la distancia de Mahalanobis. Primero se crean las medias para cada variable (mean) y la matriz de covarianzas (Sx) para calcular la distancia de mahalanobis con la función correspondiente. 

Luego se calcula el valor p asociado con cada distancia de Mahalanobis D2 y se guarda como una nueva variable: datosLW$sigmahala=(1-pchisq(D2, 3)).

Finalmente se filtran los casos atípicos multivariantes según el valor p de la distancia de mahalanobis, y se elimina la variable creada en el paso anterior.


```r
#Tratamiento de casos atipicos
mean<-colMeans(datosLW[1:9])
Sx<-cov(datosLW[1:9]) #matriz de varianza covariaza 
D2<-mahalanobis(datosLW[1:9],mean,Sx)

datosLW$sigmahala=(1-pchisq(D2, 9))  

datosLW<-datosLW[which(datosLW$sigmahala>0.001),]#dar por perdido o eliminar caso atipico
datosLW$sigmahala<-NULL
```

A continuación, utilizamos la función **mvn**  para evaluar la normalidad multivariante y univariante en un conjunto de datos. Especificamos que utilice el test de Mardia para evaluar la existencia de normalidad multivariante en nuestros datos.   

El output de esta función tiene 4 elementos: en gráfico q-q que compara la distribución multivariante de los datos con una normal, los resultados de la prueba de Mardia para evaluar la normalidad multivariante, los resultados de la prueba de Anderson-Darling para evaluar la normalidad univariante de cada variable en el conjunto de datos y estadísticas descriptivas para cada variable en el conjunto de datos, entre las que resulta particularmente relevante la asimetría, que en caso de no existir normalidad, se espera que se encuentra dentro del rango +-2.


```r
  #Test de Mardia.
MVN::mvn(datosLW,mvnTest	= "mardia",multivariatePlot="qq")
```

<img src="/example/06-practico_files/figure-html/unnamed-chunk-7-1.png" width="672" />

```
## $multivariateNormality
##              Test        Statistic               p value Result
## 1 Mardia Skewness 1717.51387000739 3.02564099408859e-256     NO
## 2 Mardia Kurtosis 33.0903314287217                     0     NO
## 3             MVN             <NA>                  <NA>     NO
## 
## $univariateNormality
##               Test  Variable Statistic   p value Normality
## 1 Anderson-Darling   SALUD     31.3706  <0.001      NO    
## 2 Anderson-Darling   INGR      31.2712  <0.001      NO    
## 3 Anderson-Darling   TRAB      29.5520  <0.001      NO    
## 4 Anderson-Darling   EDUC      28.4630  <0.001      NO    
## 5 Anderson-Darling   VIVI      29.3404  <0.001      NO    
## 6 Anderson-Darling   SEGUR     45.0719  <0.001      NO    
## 7 Anderson-Darling   MEDIO     30.6619  <0.001      NO    
## 8 Anderson-Darling   LIBER     36.5204  <0.001      NO    
## 9 Anderson-Darling   PROYE     36.7923  <0.001      NO    
## 
## $Descriptives
##          n     Mean  Std.Dev Median Min Max 25th 75th        Skew   Kurtosis
## SALUD 1572 4.052799 1.727196      4   1   7    3    5 -0.22291461 -0.7825718
## INGR  1572 3.459288 1.726418      3   1   7    2    5  0.19631825 -0.8448538
## TRAB  1572 4.008270 1.630176      4   1   7    3    5 -0.16483891 -0.6426724
## EDUC  1572 3.812977 1.694905      4   1   7    3    5 -0.05273125 -0.8424847
## VIVI  1572 3.968830 1.703233      4   1   7    3    5 -0.15459228 -0.8210521
## SEGUR 1572 3.202926 1.795437      3   1   7    1    5  0.31135796 -0.9781712
## MEDIO 1572 4.080153 1.692440      4   1   7    3    5 -0.23657352 -0.7497611
## LIBER 1572 4.419847 1.627833      5   1   7    3    6 -0.47010342 -0.3896008
## PROYE 1572 4.481552 1.565962      5   1   7    4    6 -0.48968037 -0.2984462
```

Tanto el test de mardia como la prueba de normalidad univariante para cada variable dan cuenta de que no existe normalidad, lo cual debe tenerse en cuenta al seleccionar la forma de extracción de los factores. También nos indica que más adelante no será posible interpretar la preuba de esfericidad de Barlett para multicolinealdiad, ya que esta supone normalidad multivariante.

A pesar de que no existe normalidad multivariante, encontramos una asimetría moderada, por lo que es posible continuar con el análisis.   

A continuación calculamos la matriz de correlaciones para evaluar la existencia de colinealidad. Esto es relevante porque es necesario que exista suficiente varianza común entre las variables para la extracción de factores comunes. 


```r
#Matriz de Correlaciones 
#Uso de Pearson por caracteristicas de las variables (discretas de baja asimetria)

cor_datos<- cor(datosLW)
print(cor_datos)
```

```
##           SALUD      INGR      TRAB      EDUC      VIVI     SEGUR     MEDIO
## SALUD 1.0000000 0.7003683 0.6409862 0.6787399 0.6464408 0.5103176 0.5834421
## INGR  0.7003683 1.0000000 0.6697089 0.6913384 0.6612182 0.6165792 0.5803900
## TRAB  0.6409862 0.6697089 1.0000000 0.7598912 0.7256791 0.5126788 0.6116159
## EDUC  0.6787399 0.6913384 0.7598912 1.0000000 0.7981664 0.6013051 0.6480850
## VIVI  0.6464408 0.6612182 0.7256791 0.7981664 1.0000000 0.6192391 0.6496336
## SEGUR 0.5103176 0.6165792 0.5126788 0.6013051 0.6192391 1.0000000 0.5950104
## MEDIO 0.5834421 0.5803900 0.6116159 0.6480850 0.6496336 0.5950104 1.0000000
## LIBER 0.5735002 0.5272623 0.6631359 0.6449380 0.6542137 0.4780704 0.7181173
## PROYE 0.5627119 0.5573827 0.7028506 0.6675755 0.6757716 0.4669196 0.6377446
##           LIBER     PROYE
## SALUD 0.5735002 0.5627119
## INGR  0.5272623 0.5573827
## TRAB  0.6631359 0.7028506
## EDUC  0.6449380 0.6675755
## VIVI  0.6542137 0.6757716
## SEGUR 0.4780704 0.4669196
## MEDIO 0.7181173 0.6377446
## LIBER 1.0000000 0.8135928
## PROYE 0.8135928 1.0000000
```

```r
print(det(cor_datos))#Cercano a 0 correlacion multivariante
```

```
## [1] 0.0007194266
```

La matriz de correlaciones da cuenta de un alto nivel de correlación entre las variables. Como criterio general se espera que existan correlaciones de al menos 0,3. 

El determinante de la matriz de correlaciones se calcula con la función det(). Un determinante cercano a 0 indica que existe correlación multivariante entre las variables. En este caso, el determinante es 0.0001593243, lo que sugiere que hay  colinealidad entre las variables.

A continuación se presenta como calcular una matriz de correlaciones policóricas para el caso de estar trabajando con variables ordinales. En caso de variable dicotómicas (0-1) corresponde usar correlaciones tetracóricas mediante la función 'tetrachoric()'.


```r
#Probar con matriz policlorica en caso de estar trabajando con variables ordinales.
polychoric(datosLW)
```

```
## Call: polychoric(x = datosLW)
## Polychoric correlations 
##       SALUD INGR TRAB EDUC VIVI SEGUR MEDIO LIBER PROYE
## SALUD 1.00                                             
## INGR  0.74  1.00                                       
## TRAB  0.67  0.71 1.00                                  
## EDUC  0.71  0.73 0.79 1.00                             
## VIVI  0.67  0.70 0.76 0.83 1.00                        
## SEGUR 0.54  0.66 0.55 0.65 0.67 1.00                   
## MEDIO 0.61  0.62 0.64 0.68 0.68 0.64  1.00             
## LIBER 0.59  0.56 0.69 0.68 0.69 0.52  0.75  1.00       
## PROYE 0.59  0.59 0.73 0.70 0.71 0.51  0.68  0.84  1.00 
## 
##  with tau of 
##           1     2      3      4    5   6
## SALUD -1.18 -0.84 -0.391  0.159 0.81 1.4
## INGR  -0.92 -0.49  0.038  0.555 1.14 1.6
## TRAB  -1.31 -0.87 -0.399  0.263 0.91 1.5
## EDUC  -1.15 -0.71 -0.215  0.338 0.96 1.6
## VIVI  -1.21 -0.80 -0.316  0.223 0.86 1.5
## SEGUR -0.64 -0.28  0.168  0.653 1.15 1.8
## MEDIO -1.24 -0.88 -0.417  0.184 0.76 1.5
## LIBER -1.40 -1.11 -0.672 -0.067 0.62 1.4
## PROYE -1.53 -1.19 -0.711 -0.109 0.60 1.4
```

Por último, chequeamos la existencia de multicolinealidad. Con fines de presentar el código, se calcula la prueba de esfericidad de Bartlett, sin embargo esta no puede interpretarse de manera confiable, ya que esta presupone normalidad multivariante, la cual no existe en este caso. 

La prueba de esfericidad de Bartlett evalúa si la matriz de correlaciones es significativamente diferente de la matriz de identidad. En otras palabras, contrasta la hipótesis nula de que las variables no están correlacionadas en absoluto (es decir, la matriz de correlaciones es igual a la matriz de identidad). La función toma los como argumento la matriz de correlaciones y el n.


```r
#   5c.   MULTICOLINEALIDAD
#Test de esfericidad de Bartlett. Contrastar la hipotesis Nula de Igualdad con Matriz identidad

print(cortest.bartlett(cor_datos,n = nrow(datosLW)))
```

```
## $chisq
## [1] 11341.67
## 
## $p.value
## [1] 0
## 
## $df
## [1] 36
```

En este caso, el valor chi-cuadrado es 12305.07 y el valor p es muy pequeño, lo que indica que se debe rechazar la hipótesis nula de que la matriz de correlaciones es igual a la matriz de identidad. Esto sugiere que hay correlaciones entre las variables y es apropiado continuar con el análisis factorial exploratorio.

La función **KMO()** calcula la medida de adecuación del muestreo de Kaiser-Meyer-Olkin (KMO) para evaluar la adecuación de los datos para el análisis factorial. La medida KMO varía entre 0 y 1, y valores más altos indican que el análisis factorial es más adecuado para los datos. 


```r
#KMO
KMO(datosLW)
```

```
## Kaiser-Meyer-Olkin factor adequacy
## Call: KMO(r = datosLW)
## Overall MSA =  0.93
## MSA for each item = 
## SALUD  INGR  TRAB  EDUC  VIVI SEGUR MEDIO LIBER PROYE 
##  0.95  0.92  0.95  0.94  0.94  0.93  0.94  0.87  0.90
```

El output incluye:

- Overall MSA: La medida KMO general para todo el conjunto de datos.  
- MSA for each item: La medida KMO para cada variable en el conjunto de datos.  

En este caso, la medida KMO general es 0.94, lo que indica una adecuación muy alta para el análisis factorial. Además, las medidas KMO para cada variable también son altas (entre 0.89 y 0.96), lo que sugiere que cada variable contribuye adecuadamente al análisis factorial.
