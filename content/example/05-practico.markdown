---
title: "Análisis Factorial Exploratorio I"
linktitle: "5: Análisis Factorial Exploratorio I"
date: "2023-04-09"
menu:
  example:
    parent: Ejemplos
    weight: 5
type: docs
toc: true
editor_options: 
  chunk_output_type: console
---



## 0. Objetivo del práctico

El objetivo de este práctico es revisar como realizar la comprobación de supuestos y tratamiento de variables para la realización de un Análisis factorial Exploratorio. 


## 1. Carga y gestión de datos

En primer lugar cargaremos una base de datos de PNUD 2015, que incluye los siguientes ítem. Con estos esperamos revisar si existen estructuras latentes en como las personas evalúan las oportunidades que entrega Chile. 

![](https://raw.githubusercontent.com/Clases-GabrielSotomayor/pruebapagina/master/static/slides/img/05/Practico.png)

Estos datos están en formato csv (comma separated value), por lo cual podemos leerlos con la función *read.csv2* incluida con r base.


```r
#cargamos los datos 
datos <- read.csv2("https://raw.githubusercontent.com/Clases-GabrielSotomayor/pruebapagina/master/static/slides/data/EjemploAF.csv")
```


En primer lugar revisamos los datos y daremos por perdidos los valores no sabe y no responde. 


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
summary(datos)
```

```
##      SALUD           INGR            TRAB            EDUC            VIVI      
##  Min.   :1.00   Min.   :1.000   Min.   :1.000   Min.   :1.000   Min.   :1.000  
##  1st Qu.:3.00   1st Qu.:2.000   1st Qu.:3.000   1st Qu.:3.000   1st Qu.:3.000  
##  Median :4.00   Median :3.000   Median :4.000   Median :4.000   Median :4.000  
##  Mean   :4.08   Mean   :3.425   Mean   :4.026   Mean   :3.807   Mean   :3.993  
##  3rd Qu.:5.00   3rd Qu.:5.000   3rd Qu.:5.000   3rd Qu.:5.000   3rd Qu.:5.000  
##  Max.   :7.00   Max.   :7.000   Max.   :7.000   Max.   :7.000   Max.   :7.000  
##  NA's   :11     NA's   :28      NA's   :31      NA's   :68      NA's   :34     
##      SEGUR           MEDIO           LIBER           PROYE      
##  Min.   :1.000   Min.   :1.000   Min.   :1.000   Min.   :1.000  
##  1st Qu.:1.000   1st Qu.:3.000   1st Qu.:4.000   1st Qu.:4.000  
##  Median :3.000   Median :4.000   Median :5.000   Median :5.000  
##  Mean   :3.185   Mean   :4.141   Mean   :4.473   Mean   :4.527  
##  3rd Qu.:5.000   3rd Qu.:5.000   3rd Qu.:6.000   3rd Qu.:6.000  
##  Max.   :7.000   Max.   :7.000   Max.   :7.000   Max.   :7.000  
##  NA's   :16      NA's   :17      NA's   :23      NA's   :34
```

Acontinuación exploramos los datos para conocer sus medias y distribución. En este punto es relvante revisar si existe mucha difderencia en sus niveles de variabilidad porque estos afectara los resutlado del Análisis Factorial Exploratorio.


```r
#   4.    ANALISIS DESCRIPTIVO DE LAS VARIABLES. 

#         Podemos solicitar los descriptivos variable por variable.
summary(datos$SALUD)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##    1.00    3.00    4.00    4.08    5.00    7.00      11
```

```r
summary(datos$INGR)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##   1.000   2.000   3.000   3.425   5.000   7.000      28
```

```r
#         Con este comando se solicitan resultados descriptivos para las 9 variables al mismo tiempo. 

summary(datos)
```

```
##      SALUD           INGR            TRAB            EDUC            VIVI      
##  Min.   :1.00   Min.   :1.000   Min.   :1.000   Min.   :1.000   Min.   :1.000  
##  1st Qu.:3.00   1st Qu.:2.000   1st Qu.:3.000   1st Qu.:3.000   1st Qu.:3.000  
##  Median :4.00   Median :3.000   Median :4.000   Median :4.000   Median :4.000  
##  Mean   :4.08   Mean   :3.425   Mean   :4.026   Mean   :3.807   Mean   :3.993  
##  3rd Qu.:5.00   3rd Qu.:5.000   3rd Qu.:5.000   3rd Qu.:5.000   3rd Qu.:5.000  
##  Max.   :7.00   Max.   :7.000   Max.   :7.000   Max.   :7.000   Max.   :7.000  
##  NA's   :11     NA's   :28      NA's   :31      NA's   :68      NA's   :34     
##      SEGUR           MEDIO           LIBER           PROYE      
##  Min.   :1.000   Min.   :1.000   Min.   :1.000   Min.   :1.000  
##  1st Qu.:1.000   1st Qu.:3.000   1st Qu.:4.000   1st Qu.:4.000  
##  Median :3.000   Median :4.000   Median :5.000   Median :5.000  
##  Mean   :3.185   Mean   :4.141   Mean   :4.473   Mean   :4.527  
##  3rd Qu.:5.000   3rd Qu.:5.000   3rd Qu.:6.000   3rd Qu.:6.000  
##  Max.   :7.000   Max.   :7.000   Max.   :7.000   Max.   :7.000  
##  NA's   :16      NA's   :17      NA's   :23      NA's   :34
```

##  2.  Comprobación de supuestos


```r
library(psych)
library(MVN)
```

Para la comprobación de supuestos partiremos por generar una base de datos listwise, es decir, en al que se eliminan todos los casos que tienen valores perdidos en alguno de los ítem. Esto es posible en este caso por el bajo número de perdidos existentes. 


```r
#   5a.   Crear una base solo con listwise (para test de Mardia)

datosLW <- na.omit(datos)
summary(datosLW)
```

```
##      SALUD            INGR            TRAB            EDUC      
##  Min.   :1.000   Min.   :1.000   Min.   :1.000   Min.   :1.000  
##  1st Qu.:3.000   1st Qu.:2.000   1st Qu.:3.000   1st Qu.:3.000  
##  Median :4.000   Median :3.000   Median :4.000   Median :4.000  
##  Mean   :4.042   Mean   :3.428   Mean   :4.002   Mean   :3.797  
##  3rd Qu.:5.000   3rd Qu.:5.000   3rd Qu.:5.000   3rd Qu.:5.000  
##  Max.   :7.000   Max.   :7.000   Max.   :7.000   Max.   :7.000  
##       VIVI           SEGUR           MEDIO           LIBER      
##  Min.   :1.000   Min.   :1.000   Min.   :1.000   Min.   :1.000  
##  1st Qu.:3.000   1st Qu.:1.000   1st Qu.:3.000   1st Qu.:3.000  
##  Median :4.000   Median :3.000   Median :4.000   Median :5.000  
##  Mean   :3.953   Mean   :3.164   Mean   :4.097   Mean   :4.444  
##  3rd Qu.:5.000   3rd Qu.:5.000   3rd Qu.:5.000   3rd Qu.:6.000  
##  Max.   :7.000   Max.   :7.000   Max.   :7.000   Max.   :7.000  
##      PROYE      
##  Min.   :1.000  
##  1st Qu.:4.000  
##  Median :5.000  
##  Mean   :4.499  
##  3rd Qu.:6.000  
##  Max.   :7.000
```

```r
dim(datosLW)
```

```
## [1] 1632    9
```

A continuación revisaremos la existencia de casos atípicos multivariantes a partir del cálculo y evaluación de la distancia de Mahalanobis. 


```r
#Tratamiento de casos atipicos

mean<-colMeans(datosLW[1:9])
mean
```

```
##    SALUD     INGR     TRAB     EDUC     VIVI    SEGUR    MEDIO    LIBER 
## 4.042279 3.428309 4.002451 3.797181 3.953431 3.163603 4.096814 4.443627 
##    PROYE 
## 4.499387
```

```r
Sx<-cov(datosLW[1:9]) #matriz de varianza covariaza 
Sx
```

```
##          SALUD     INGR     TRAB     EDUC     VIVI    SEGUR    MEDIO    LIBER
## SALUD 3.112252 2.062812 1.742386 1.974246 1.834588 1.509326 1.616996 1.552048
## INGR  2.062812 3.054330 1.851801 1.994341 1.859320 1.842821 1.604125 1.386821
## TRAB  1.742386 1.851801 2.773752 2.112699 1.967619 1.461892 1.633116 1.725460
## EDUC  1.974246 1.994341 2.112699 2.968649 2.244994 1.826580 1.826515 1.719706
## VIVI  1.834588 1.859320 1.967619 2.244994 3.016837 1.850665 1.783175 1.725148
## SEGUR 1.509326 1.842821 1.461892 1.826580 1.850665 3.278552 1.771398 1.342460
## MEDIO 1.616996 1.604125 1.633116 1.826515 1.783175 1.771398 2.999818 1.941083
## LIBER 1.552048 1.386821 1.725460 1.719706 1.725148 1.342460 1.941083 2.741149
## PROYE 1.438714 1.435272 1.719192 1.740832 1.770051 1.273246 1.677558 2.019892
##          PROYE
## SALUD 1.438714
## INGR  1.435272
## TRAB  1.719192
## EDUC  1.740832
## VIVI  1.770051
## SEGUR 1.273246
## MEDIO 1.677558
## LIBER 2.019892
## PROYE 2.539546
```

```r
D2<-mahalanobis(datosLW[1:9],mean,Sx)

datosLW$sigmahala=(1-pchisq(D2, 3))  

datosLW<-datosLW[which(datosLW$sigmahala>0.01),]#dar por perdido o eliminar caso atipico
datosLW$sigmahala<-NULL
```

A continuación utilizamos el test de Mardia para evaluar la existencia de normalidad multivariabnte en nuestros datos.  


```r
  #Test de Mardia.
MVN::mvn(datosLW,mvnTest	= "mardia",multivariatePlot="qq")
```

<img src="/example/05-practico_files/figure-html/unnamed-chunk-7-1.png" width="672" />

```
## $multivariateNormality
##              Test        Statistic              p value Result
## 1 Mardia Skewness 707.263203094999 3.31512649658847e-68     NO
## 2 Mardia Kurtosis 5.72608168319409 1.02776749333344e-08     NO
## 3             MVN             <NA>                 <NA>     NO
## 
## $univariateNormality
##               Test  Variable Statistic   p value Normality
## 1 Anderson-Darling   SALUD     26.3051  <0.001      NO    
## 2 Anderson-Darling   INGR      22.6925  <0.001      NO    
## 3 Anderson-Darling   TRAB      23.5922  <0.001      NO    
## 4 Anderson-Darling   EDUC      23.2917  <0.001      NO    
## 5 Anderson-Darling   VIVI      23.8048  <0.001      NO    
## 6 Anderson-Darling   SEGUR     25.7415  <0.001      NO    
## 7 Anderson-Darling   MEDIO     24.5595  <0.001      NO    
## 8 Anderson-Darling   LIBER     29.3304  <0.001      NO    
## 9 Anderson-Darling   PROYE     30.1904  <0.001      NO    
## 
## $Descriptives
##          n   Mean  Std.Dev Median Min Max 25th 75th        Skew   Kurtosis
## SALUD 1250 4.0808 1.637931      4   1   7    3    5 -0.28143976 -0.6210303
## INGR  1250 3.6000 1.658131      4   1   7    2    5  0.09981431 -0.7657481
## TRAB  1250 3.9928 1.591343      4   1   7    3    5 -0.15736066 -0.5874713
## EDUC  1250 3.9032 1.641946      4   1   7    3    5 -0.13445899 -0.7588844
## VIVI  1250 4.0304 1.620239      4   1   7    3    5 -0.19467953 -0.6997123
## SEGUR 1250 3.4464 1.723689      3   1   7    2    5  0.16701464 -0.8912731
## MEDIO 1250 4.1168 1.591346      4   1   7    3    5 -0.25455490 -0.5893898
## LIBER 1250 4.3576 1.567574      5   1   7    3    5 -0.45581712 -0.3246527
## PROYE 1250 4.4184 1.519075      5   1   7    4    6 -0.49027569 -0.2352500
```

A nivel univariado debemos comprobar que existan niveles moderados de asimetria en nuestra varaibles de interés. 


```r
#   5b.   Observar asimetria

s1<-skew(datosLW$SALUD, type=2, na.rm=T)
s2<-skew(datosLW$INGR, type=2, na.rm=T)
s3<-skew(datosLW$TRAB, type=2, na.rm=T)
s4<-skew(datosLW$EDUC, type=2, na.rm=T)
s5<-skew(datosLW$VIVI, type=2, na.rm=T)
s6<-skew(datosLW$SEGUR, type=2, na.rm=T)
s7<-skew(datosLW$MEDIO, type=2, na.rm=T)
s8<-skew(datosLW$LIBER, type=2, na.rm=T)
s9<-skew(datosLW$PROYE, type=2, na.rm=T)
```

El test KS nos permite evaluar la existencia de normalidad en nuestras variables. 


```r
#Test de KS

ks.test(datosLW$SALUD, "pnorm", mean(datosLW$SALUD, na.rm=T), sd(datosLW$SALUD,na.rm=T))
```

```
## Warning in ks.test.default(datosLW$SALUD, "pnorm", mean(datosLW$SALUD, na.rm =
## T), : ties should not be present for the Kolmogorov-Smirnov test
```

```
## 
## 	Asymptotic one-sample Kolmogorov-Smirnov test
## 
## data:  datosLW$SALUD
## D = 0.15107, p-value < 2.2e-16
## alternative hypothesis: two-sided
```

```r
# ks.test(datosLW$INGR, "pnorm", mean(datosLW$INGR, na.rm=T), sd(datosLW$INGR,na.rm=T))
# ks.test(datosLW$TRAB, "pnorm", mean(datosLW$TRAB, na.rm=T), sd(datosLW$TRAB,na.rm=T))
# ks.test(datosLW$EDUC, "pnorm", mean(datosLW$EDUC, na.rm=T), sd(datosLW$EDUC,na.rm=T))
# ks.test(datosLW$VIVI, "pnorm", mean(datosLW$VIVI, na.rm=T), sd(datosLW$VIVI,na.rm=T))
# ks.test(datosLW$SEGUR, "pnorm", mean(datosLW$SEGUR, na.rm=T), sd(datosLW$SEGUR,na.rm=T))
# ks.test(datosLW$MEDIO, "pnorm", mean(datosLW$MEDIO, na.rm=T), sd(datosLW$MEDIO,na.rm=T))
# ks.test(datosLW$LIBER, "pnorm", mean(datosLW$LIBER, na.rm=T), sd(datosLW$LIBER,na.rm=T))
# ks.test(datosLW$PROYE, "pnorm", mean(datosLW$PROYE, na.rm=T), sd(datosLW$PROYE,na.rm=T))
```

A cotninuación calculamos la matriz de correlaciones para evaluar la existencia de colinealdiad. Esto es relevante porque es necesario qeu exista suficiente varianza común entre las variables para la extracción de factores comunes. 


```r
#Matriz de Correlaciones 
#Uso de Pearson por caracteristicas de las variables (discretas de baja asimetria)

cor_datos<- cor(datosLW)
print(cor_datos)
```

```
##           SALUD      INGR      TRAB      EDUC      VIVI     SEGUR     MEDIO
## SALUD 1.0000000 0.7860467 0.7598535 0.7769382 0.7608442 0.6388922 0.7040935
## INGR  0.7860467 1.0000000 0.7644548 0.7653625 0.7489735 0.7149479 0.6937550
## TRAB  0.7598535 0.7644548 1.0000000 0.8282883 0.8052723 0.6544157 0.7445763
## EDUC  0.7769382 0.7653625 0.8282883 1.0000000 0.8528056 0.7072326 0.7581183
## VIVI  0.7608442 0.7489735 0.8052723 0.8528056 1.0000000 0.7233076 0.7730658
## SEGUR 0.6388922 0.7149479 0.6544157 0.7072326 0.7233076 1.0000000 0.7051467
## MEDIO 0.7040935 0.6937550 0.7445763 0.7581183 0.7730658 0.7051467 1.0000000
## LIBER 0.6981444 0.6588109 0.7543169 0.7584595 0.7768613 0.6143928 0.8286397
## PROYE 0.6994728 0.6787004 0.7805683 0.7654555 0.7885513 0.6205769 0.7753166
##           LIBER     PROYE
## SALUD 0.6981444 0.6994728
## INGR  0.6588109 0.6787004
## TRAB  0.7543169 0.7805683
## EDUC  0.7584595 0.7654555
## VIVI  0.7768613 0.7885513
## SEGUR 0.6143928 0.6205769
## MEDIO 0.8286397 0.7753166
## LIBER 1.0000000 0.8785485
## PROYE 0.8785485 1.0000000
```

```r
print(det(cor_datos))#Cercano a 0 correlacion multivariante
```

```
## [1] 4.558771e-05
```




```r
#Probar con matriz policlorica en caso de estar trabajando con variables ordinales.
polychoric(datosLW)
```

```
## Call: polychoric(x = datosLW)
## Polychoric correlations 
##       SALUD INGR TRAB EDUC VIVI SEGUR MEDIO LIBER PROYE
## SALUD 1.00                                             
## INGR  0.82  1.00                                       
## TRAB  0.79  0.80 1.00                                  
## EDUC  0.80  0.80 0.86 1.00                             
## VIVI  0.78  0.78 0.83 0.88 1.00                        
## SEGUR 0.67  0.75 0.69 0.74 0.76 1.00                   
## MEDIO 0.73  0.73 0.77 0.79 0.80 0.74  1.00             
## LIBER 0.72  0.69 0.78 0.79 0.80 0.66  0.86  1.00       
## PROYE 0.72  0.72 0.81 0.80 0.82 0.67  0.80  0.91  1.00 
## 
##  with tau of 
##           1     2      3      4    5   6
## SALUD -1.27 -0.92 -0.441  0.155 0.87 1.6
## INGR  -1.08 -0.63 -0.048  0.492 1.13 1.7
## TRAB  -1.35 -0.89 -0.389  0.280 0.95 1.6
## EDUC  -1.24 -0.80 -0.291  0.291 0.94 1.7
## VIVI  -1.35 -0.88 -0.369  0.196 0.89 1.6
## SEGUR -0.89 -0.49  0.034  0.571 1.11 1.7
## MEDIO -1.39 -0.97 -0.465  0.196 0.81 1.6
## LIBER -1.44 -1.14 -0.648 -0.022 0.70 1.5
## PROYE -1.53 -1.22 -0.693 -0.056 0.67 1.5
```

Por último chequeamos la existencia de multicolinealidad.


```r
#   5c.   MULTICOLINEALIDAD
#Test de esfericidad de Bartlett.Contrastar la hipotesis Nula de Igualdad con Matriz identidad

print(cortest.bartlett(cor_datos,n = nrow(datosLW)))
```

```
## $chisq
## [1] 12446.53
## 
## $p.value
## [1] 0
## 
## $df
## [1] 36
```

```r
#KMO

KMO(datosLW)
```

```
## Kaiser-Meyer-Olkin factor adequacy
## Call: KMO(r = datosLW)
## Overall MSA =  0.94
## MSA for each item = 
## SALUD  INGR  TRAB  EDUC  VIVI SEGUR MEDIO LIBER PROYE 
##  0.96  0.94  0.96  0.95  0.96  0.94  0.95  0.90  0.92
```


