---
title: "Análisis de Senderos"
linktitle: "10: Análisis de Senderos"
date: "2023-05-29"
menu:
  example:
    parent: Ejemplos
    weight: 10
type: docs
toc: true
editor_options: 
  chunk_output_type: console
---

El análisis de senderos es una técnica estadística que permite descomponer correlaciones en diferentes componentes causales para entender las relaciones directas e indirectas entre diferentes variables. Esta técnica es especialmente útil cuando queremos entender el efecto de una variable sobre otra a través de una o más variables intermedias (o "mediadoras").


```r
# Carga las bibliotecas necesarias
library(haven)
library(MVN)
library(lavaan)
```

```
## This is lavaan 0.6-15
## lavaan is FREE software! Please report any bugs.
```

```r
library(semPlot) 
library(semTable)
```

Las bibliotecas necesarias son haven para la importación de datos, MVN para la evaluación de la normalidad multivariante, lavaan para realizar análisis de senderos, semPlot para la visualización del modelo y semTable para la presentación de los resultados.


```r
# Importar datos
datos <- read_sav(url("https://github.com/Clases-GabrielSotomayor/pruebapagina/raw/master/content/example/input/data/elsoc2016.sav"))
```

Los datos se importan de una URL utilizando la función read_sav de la biblioteca haven.


```r
# Comprobación de supuestos
dim(datos)
```

```
## [1] 2984   12
```

```r
cor(datos,use = "complete.obs")
```

```
##                 izquierda       centro     derecha  indep_ning    castigo1
## izquierda      1.00000000 -0.275763131 -0.14996075 -0.37340777 -0.06289144
## centro        -0.27576313  1.000000000 -0.23131300 -0.57597787 -0.06760531
## derecha       -0.14996075 -0.231312999  1.00000000 -0.31321834  0.06582355
## indep_ning    -0.37340777 -0.575977868 -0.31321834  1.00000000  0.06601813
## castigo1      -0.06289144 -0.067605312  0.06582355  0.06601813  1.00000000
## castigo2      -0.10091438 -0.043850404  0.07201616  0.06766732  0.69260707
## castigo_media -0.08921416 -0.060461296  0.07494671  0.07266677  0.91844663
## rwa1          -0.15739028 -0.037224914  0.10352551  0.08237888  0.16668607
## rwa2          -0.17784532 -0.003068905  0.13467079  0.04589192  0.17876891
## rwa3          -0.10880616 -0.008046829  0.06591458  0.04422838  0.21575505
## rwa4          -0.13835729 -0.018140523  0.06132934  0.07781187  0.21570501
## rwa_media     -0.18246165 -0.021269067  0.11498079  0.07851573  0.24238007
##                  castigo2 castigo_media        rwa1         rwa2         rwa3
## izquierda     -0.10091438   -0.08921416 -0.15739028 -0.177845320 -0.108806158
## centro        -0.04385040   -0.06046130 -0.03722491 -0.003068905 -0.008046829
## derecha        0.07201616    0.07494671  0.10352551  0.134670786  0.065914580
## indep_ning     0.06766732    0.07266677  0.08237888  0.045891917  0.044228383
## castigo1       0.69260707    0.91844663  0.16668607  0.178768907  0.215755047
## castigo2       1.00000000    0.92143515  0.18514469  0.218673914  0.221499937
## castigo_media  0.92143515    1.00000000  0.19131161  0.216206255  0.237678321
## rwa1           0.18514469    0.19131161  1.00000000  0.626344891  0.477367236
## rwa2           0.21867391    0.21620625  0.62634489  1.000000000  0.440335643
## rwa3           0.22149994    0.23767832  0.47736724  0.440335643  1.000000000
## rwa4           0.27024828    0.26438332  0.48362743  0.460584965  0.607326740
## rwa_media      0.27865346    0.28336081  0.82053815  0.792099666  0.789989111
##                      rwa4   rwa_media
## izquierda     -0.13835729 -0.18246165
## centro        -0.01814052 -0.02126907
## derecha        0.06132934  0.11498079
## indep_ning     0.07781187  0.07851573
## castigo1       0.21570501  0.24238007
## castigo2       0.27024828  0.27865346
## castigo_media  0.26438332  0.28336081
## rwa1           0.48362743  0.82053815
## rwa2           0.46058497  0.79209967
## rwa3           0.60732674  0.78998911
## rwa4           1.00000000  0.78906933
## rwa_media      0.78906933  1.00000000
```

```r
summary(datos)
```

```
##    izquierda          centro          derecha         indep_ning    
##  Min.   :0.0000   Min.   :0.0000   Min.   :0.0000   Min.   :0.0000  
##  1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0.0000  
##  Median :0.0000   Median :0.0000   Median :0.0000   Median :0.0000  
##  Mean   :0.1516   Mean   :0.2971   Mean   :0.1101   Mean   :0.4412  
##  3rd Qu.:0.0000   3rd Qu.:1.0000   3rd Qu.:0.0000   3rd Qu.:1.0000  
##  Max.   :1.0000   Max.   :1.0000   Max.   :1.0000   Max.   :1.0000  
##  NA's   :69       NA's   :69       NA's   :69       NA's   :69      
##     castigo1        castigo2     castigo_media        rwa1            rwa2    
##  Min.   :1.000   Min.   :1.000   Min.   :1.000   Min.   :1.000   Min.   :1.0  
##  1st Qu.:4.000   1st Qu.:4.000   1st Qu.:4.000   1st Qu.:3.000   1st Qu.:3.0  
##  Median :4.000   Median :4.000   Median :4.000   Median :4.000   Median :4.0  
##  Mean   :4.199   Mean   :4.155   Mean   :4.177   Mean   :3.578   Mean   :3.7  
##  3rd Qu.:5.000   3rd Qu.:5.000   3rd Qu.:5.000   3rd Qu.:4.000   3rd Qu.:4.0  
##  Max.   :5.000   Max.   :5.000   Max.   :5.000   Max.   :5.000   Max.   :5.0  
##  NA's   :6       NA's   :13      NA's   :5       NA's   :24      NA's   :30   
##       rwa3            rwa4         rwa_media   
##  Min.   :1.000   Min.   :1.000   Min.   :1.00  
##  1st Qu.:3.000   1st Qu.:3.000   1st Qu.:3.25  
##  Median :4.000   Median :4.000   Median :4.00  
##  Mean   :3.689   Mean   :3.715   Mean   :3.67  
##  3rd Qu.:4.000   3rd Qu.:4.000   3rd Qu.:4.00  
##  Max.   :5.000   Max.   :5.000   Max.   :5.00  
##  NA's   :10      NA's   :7       NA's   :2
```

```r
mvn(datos[,c("castigo_media","rwa_media","derecha", "izquierda" , "centro")])
```

```
## $multivariateNormality
##            Test       HZ p value MVN
## 1 Henze-Zirkler 152.3595       0  NO
## 
## $univariateNormality
##               Test      Variable Statistic   p value Normality
## 1 Anderson-Darling castigo_media  191.4103  <0.001      NO    
## 2 Anderson-Darling   rwa_media     85.0380  <0.001      NO    
## 3 Anderson-Darling    derecha     938.3859  <0.001      NO    
## 4 Anderson-Darling   izquierda    859.4875  <0.001      NO    
## 5 Anderson-Darling    centro      637.3425  <0.001      NO    
## 
## $Descriptives
##                  n      Mean   Std.Dev Median Min Max 25th 75th       Skew
## castigo_media 2909 4.1801306 0.7738886      4   1   5 4.00    5 -1.3794682
## rwa_media     2909 3.6695600 0.7732055      4   1   5 3.25    4 -0.7948078
## derecha       2909 0.1103472 0.3133759      0   0   1 0.00    0  2.4859529
## izquierda     2909 0.1519422 0.3590266      0   0   1 0.00    0  1.9382301
## centro        2909 0.2976968 0.4573241      0   0   1 0.00    1  0.8844216
##                 Kurtosis
## castigo_media  3.0965422
## rwa_media      0.8365222
## derecha        4.1813994
## izquierda      1.7573405
## centro        -1.2182168
```

Aquí se revisan varios supuestos antes de ajustar el modelo de senderos. dim(datos) muestra las dimensiones de los datos, cor(datos,use = "complete.obs") muestra las correlaciones entre variables, summary(datos) ofrece un resumen estadístico de las variables, y mvn(datos) evalúa la normalidad multivariante, que es un supuesto clave en el análisis de senderos.


```r
# Especificar y ajustar el modelo de senderos
mod_sendero <-    'castigo_media ~ rwa_media
                   rwa_media ~ derecha + izquierda + centro'
ajus_sendero <- sem(mod_sendero, data=datos)
```
Este bloque de código define y ajusta el modelo de senderos. La función sem() del paquete lavaan se utiliza para ajustar el modelo. En este modelo, 'castigo_media' se modela como una función de 'rwa_media', y 'rwa_media' se modela como una función de 'derecha', 'izquierda', y 'centro'.

```r
# Resumen de los resultados del modelo de senderos
summary(ajus_sendero, fit.measures = T, standardized = T, rsquare = T, modindices = T)
```

```
## lavaan 0.6.15 ended normally after 1 iteration
## 
##   Estimator                                         ML
##   Optimization method                           NLMINB
##   Number of model parameters                         6
## 
##                                                   Used       Total
##   Number of observations                          2909        2984
## 
## Model Test User Model:
##                                                       
##   Test statistic                                21.692
##   Degrees of freedom                                 3
##   P-value (Chi-square)                           0.000
## 
## Model Test Baseline Model:
## 
##   Test statistic                               395.965
##   Degrees of freedom                                 7
##   P-value                                        0.000
## 
## User Model versus Baseline Model:
## 
##   Comparative Fit Index (CFI)                    0.952
##   Tucker-Lewis Index (TLI)                       0.888
## 
## Loglikelihood and Information Criteria:
## 
##   Loglikelihood user model (H0)              -6573.366
##   Loglikelihood unrestricted model (H1)      -6562.520
##                                                       
##   Akaike (AIC)                               13158.733
##   Bayesian (BIC)                             13194.586
##   Sample-size adjusted Bayesian (SABIC)      13175.522
## 
## Root Mean Square Error of Approximation:
## 
##   RMSEA                                          0.046
##   90 Percent confidence interval - lower         0.029
##   90 Percent confidence interval - upper         0.065
##   P-value H_0: RMSEA <= 0.050                    0.591
##   P-value H_0: RMSEA >= 0.080                    0.001
## 
## Standardized Root Mean Square Residual:
## 
##   SRMR                                           0.021
## 
## Parameter Estimates:
## 
##   Standard errors                             Standard
##   Information                                 Expected
##   Information saturated (h1) model          Structured
## 
## Regressions:
##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
##   castigo_media ~                                                       
##     rwa_media         0.284    0.018   15.941    0.000    0.284    0.283
##   rwa_media ~                                                           
##     derecha           0.182    0.047    3.856    0.000    0.182    0.074
##     izquierda        -0.404    0.042   -9.679    0.000   -0.404   -0.187
##     centro           -0.094    0.033   -2.833    0.005   -0.094   -0.056
## 
## Variances:
##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
##    .castigo_media     0.551    0.014   38.138    0.000    0.551    0.920
##    .rwa_media         0.571    0.015   38.138    0.000    0.571    0.956
## 
## R-Square:
##                    Estimate
##     castigo_media     0.080
##     rwa_media         0.044
## 
## Modification Indices:
## 
##             lhs op           rhs     mi    epc sepc.lv sepc.all sepc.nox
## 1 castigo_media  ~       derecha  6.112  0.109   0.109    0.044    0.141
## 2 castigo_media  ~     izquierda  4.879 -0.086  -0.086   -0.040   -0.111
## 3 castigo_media  ~        centro  9.832 -0.094  -0.094   -0.056   -0.122
## 4     rwa_media  ~ castigo_media 13.858 -0.335  -0.335   -0.336   -0.336
## 5       derecha  ~ castigo_media  1.209  0.008   0.008    0.020    0.020
## 6     izquierda  ~ castigo_media  8.243 -0.024  -0.024   -0.053   -0.053
## 7        centro  ~ castigo_media 11.668 -0.036  -0.036   -0.061   -0.061
```
La función summary() proporciona un resumen de los resultados, incluyendo varias medidas de ajuste, coeficientes estandarizados, los coeficientes de determinación (R^2) y los índices de modificación.

```r
# Crear una tabla de resultados en formato APA
semTable(ajus_sendero, type = "html", paramSets = c("loadings", "slopes", "latentcovariances", 
                                                    "fits", "constructed"), file = "resultados_sendero")
```

<table style="padding-right:20px;padding-left:20px;">
<tr><td></td><td colspan = '4'; align = 'center'>Model</td></tr> 
<tr><td></td><td colspan = '1'; align = 'center'>Estimate</td><td colspan = '1'; align = 'center'>Std. Err.</td><td colspan = '1'; align = 'center'>z</td><td colspan = '1'; align = 'center'>p</td></tr>
<tr><td></td><td colspan = '4'; align = 'center'><span style="text-decoration: underline;">Regression Slopes</span></td></tr> <tr><td colspan = '1'; align = 'left'><span style="text-decoration: underline;">castigo_media</span></td></tr>
<tr><td>rwa.media</td><td>0.28</td><td>0.02</td><td>15.94</td><td>.000</td></tr>
 <tr><td colspan = '1'; align = 'left'><span style="text-decoration: underline;">rwa_media</span></td></tr>
<tr><td>derecha</td><td>0.18</td><td>0.05</td><td>3.86</td><td>.000</td></tr>
<tr><td>izquierda</td><td>-0.40</td><td>0.04</td><td>-9.68</td><td>.000</td></tr>
<tr><td>centro</td><td>-0.09</td><td>0.03</td><td>-2.83</td><td>.005</td></tr>
<tr><td></td><td colspan = '4'; align = 'center'><span style="text-decoration: underline;">Fit Indices</span></td></tr>
<tr><td>&chi;<sup>2</sup></td><td>21.69(3)</td><td></td><td></td><td>.000</td></tr>
<tr><td>CFI</td><td>0.95</td><td></td><td></td><td></td></tr>
<tr><td>TLI</td><td>0.89</td><td></td><td></td><td></td></tr>
<tr><td>RMSEA</td><td>0.05</td><td></td><td></td><td></td></tr>
<tr><td colspan = '5'; align = 'left'><sup>+</sup>Fixed parameter</td></tr>
</table><br>
 

Aquí usamos semTable() para generar una tabla con los resultados principales del modelo. Esta tabla incluye las cargas factoriales ("loadings"), los coeficientes de las rutas ("slopes"), las covarianzas latentes ("latentcovariances"), las medidas de ajuste ("fits") y los índices construidos ("constructed"). Los resultados se guardan en un archivo HTML con el nombre "resultados_sendero".



```r
# Mostrar la tabla de resultados en un navegador
browseURL("resultados_sendero.html")
```

Aquí utilizamos browseURL() para abrir el archivo HTML generado en el navegador por defecto. Es una manera eficaz de visualizar la tabla de resultados.


```r
# Especificar y ajustar el modelo de senderos con efectos indirectos
mod_sendero3 <-    'castigo_media ~ a* rwa_media + c*derecha
                    rwa_media ~ b* derecha + izquierda + centro
                    ind_derecha_rwa := a*b
                    total_derecha_rwa := (a*b)+c'
ajus_sendero3 <- sem(mod_sendero3, data=datos)
```
Este bloque de código define y ajusta un modelo de senderos que incluye efectos indirectos. Aquí se está modelando 'castigo_media' como una función de 'rwa_media' y 'derecha', y 'rwa_media' como una función de 'derecha', 'izquierda', y 'centro'. Además, se introducen dos parámetros nuevos, 'ind_derecha_rwa' y 'total_derecha_rwa', que representan los efectos indirectos y totales de 'derecha' sobre 'castigo_media' a través de 'rwa_media', respectivamente.


```r
# Resumen de los resultados del modelo de senderos con efectos indirectos
summary(ajus_sendero3, fit.measures = T, standardized = T, rsquare = T, modindices = T)
```

```
## lavaan 0.6.15 ended normally after 2 iterations
## 
##   Estimator                                         ML
##   Optimization method                           NLMINB
##   Number of model parameters                         7
## 
##                                                   Used       Total
##   Number of observations                          2909        2984
## 
## Model Test User Model:
##                                                       
##   Test statistic                                15.573
##   Degrees of freedom                                 2
##   P-value (Chi-square)                           0.000
## 
## Model Test Baseline Model:
## 
##   Test statistic                               395.965
##   Degrees of freedom                                 7
##   P-value                                        0.000
## 
## User Model versus Baseline Model:
## 
##   Comparative Fit Index (CFI)                    0.965
##   Tucker-Lewis Index (TLI)                       0.878
## 
## Loglikelihood and Information Criteria:
## 
##   Loglikelihood user model (H0)              -6570.307
##   Loglikelihood unrestricted model (H1)      -6562.520
##                                                       
##   Akaike (AIC)                               13154.614
##   Bayesian (BIC)                             13196.443
##   Sample-size adjusted Bayesian (SABIC)      13174.201
## 
## Root Mean Square Error of Approximation:
## 
##   RMSEA                                          0.048
##   90 Percent confidence interval - lower         0.028
##   90 Percent confidence interval - upper         0.072
##   P-value H_0: RMSEA <= 0.050                    0.499
##   P-value H_0: RMSEA >= 0.080                    0.012
## 
## Standardized Root Mean Square Residual:
## 
##   SRMR                                           0.015
## 
## Parameter Estimates:
## 
##   Standard errors                             Standard
##   Information                                 Expected
##   Information saturated (h1) model          Structured
## 
## Regressions:
##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
##   castigo_media ~                                                       
##     rwa_media  (a)    0.279    0.018   15.569    0.000    0.279    0.278
##     derecha    (c)    0.109    0.044    2.475    0.013    0.109    0.044
##   rwa_media ~                                                           
##     derecha    (b)    0.182    0.047    3.856    0.000    0.182    0.074
##     izquierda        -0.404    0.042   -9.679    0.000   -0.404   -0.187
##     centro           -0.094    0.033   -2.833    0.005   -0.094   -0.056
## 
## Variances:
##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
##    .castigo_media     0.549    0.014   38.138    0.000    0.549    0.918
##    .rwa_media         0.571    0.015   38.138    0.000    0.571    0.956
## 
## R-Square:
##                    Estimate
##     castigo_media     0.082
##     rwa_media         0.044
## 
## Defined Parameters:
##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
##     ind_derecha_rw    0.051    0.014    3.743    0.000    0.051    0.021
##     total_derch_rw    0.160    0.046    3.491    0.000    0.160    0.065
## 
## Modification Indices:
## 
##              lhs op           rhs     mi    epc sepc.lv sepc.all sepc.nox
## 1        derecha ~~       derecha  0.000  0.000   0.000    0.000    0.000
## 2        derecha ~~     izquierda  0.000  0.000   0.000       NA    0.000
## 3        derecha ~~        centro  0.000  0.000   0.000       NA    0.000
## 4  castigo_media  ~     izquierda  3.621 -0.075  -0.075   -0.035   -0.097
## 5  castigo_media  ~        centro  6.988 -0.082  -0.082   -0.048   -0.105
## 6      rwa_media  ~ castigo_media  8.075 -0.304  -0.304   -0.304   -0.304
## 7        derecha  ~ castigo_media 15.538 -0.093  -0.093   -0.229   -0.229
## 8        derecha  ~     rwa_media  0.000  0.000   0.000    0.000    0.000
## 9        derecha  ~     izquierda  0.000  0.000   0.000    0.000    0.000
## 10       derecha  ~        centro  0.000  0.000   0.000    0.000    0.000
## 11     izquierda  ~ castigo_media  8.494 -0.025  -0.025   -0.053   -0.053
## 12     izquierda  ~     rwa_media  0.000  0.000   0.000    0.000    0.000
## 13     izquierda  ~       derecha  0.000  0.000   0.000    0.000    0.000
## 14     izquierda  ~        centro  0.000  0.000   0.000    0.000    0.000
## 15        centro  ~ castigo_media 11.707 -0.036  -0.036   -0.061   -0.061
## 16        centro  ~     rwa_media  0.000  0.000   0.000    0.000    0.000
## 17        centro  ~       derecha  0.000  0.000   0.000    0.000    0.000
## 18        centro  ~     izquierda  0.000  0.000   0.000    0.000    0.000
```
Por último, se utiliza summary() para proporcionar un resumen de los resultados del modelo que incluye los efectos indirectos. Esto incluirá las mismas estadísticas que antes, pero ahora también incluirá los efectos indirectos y totales que hemos especificado.


```r
# Diagrama del modelo de senderos
semPaths(ajus_sendero3, # modelo ajustado
         what = "std",  # mostrar cargas estandarizadas
         label.cex = 1, edge.label.cex = 1, # tamaño de las etiquetas y caracteres
         residuals = FALSE, # no mostrar residuos
         edge.color = "black") # color de las flechas
```

<img src="/example/10-practico_files/figure-html/unnamed-chunk-10-1.png" width="672" />
Aquí usamos semPaths() para generar un diagrama del modelo de senderos que hemos ajustado. Los argumentos de esta función son similares a los que hemos usado antes. ajus_sendero3 es el modelo ajustado, what = "std" indica que se deben mostrar las cargas estandarizadas en lugar de las no estandarizadas, label.cex = 1 y edge.label.cex = 1 controlan el tamaño de las etiquetas y de las flechas respectivamente, residuals = FALSE indica que no se deben mostrar los residuos en el diagrama, y edge.color = "black" establece el color de las flechas a negro.

Este diagrama proporciona una representación visual del modelo de senderos que hemos ajustado, lo que puede facilitar su interpretación. En el diagrama, las variables observables se representan como rectángulos, las variables latentes (si las hay) como óvalos, y las relaciones entre variables como flechas. Las cargas factoriales o coeficientes de las rutas se representan junto a las flechas correspondientes.
