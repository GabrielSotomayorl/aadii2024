---
title: "3. Análisis de Regresión Lineal Múltiple"
linktitle: "3. Análisis de Regresión Lineal Múltiple"
date: "2023-03-27"
menu:
  example:
    parent: Ejemplos
    weight: 3
type: docs
toc: true
editor_options:
  chunk_output_type: console
---

# 0. Objetivo del práctico

El objetivo de este práctico es presentar una introducción al Análisis de Regresión Lineal Múltiple,visualizar sus resultados y evaluar el ajuste de los modelos y el cumplimiento de los supuestos de la técnica.   

Para esto haremos uso de la encuesta [CASEN (2020)](http://observatorio.ministeriodesarrollosocial.gob.cl/encuesta-casen-en-pandemia-2020), la mayor encuesta de hogares realizada en Chile, a cargo del Ministerio de Desarrollo Social, de carácter transversal y multipropósito, es el principal instrumento de medición socioeconómica para el diseño y evaluación de la política social. Permite conocer periódicamente la situación socioeconómica de los hogares y de la población que reside en viviendas particulares, a través de preguntas referidas a composición familiar, educación, salud, vivienda, trabajo e ingresos, entre otros aspectos. 

## Cargar paquetes a utilizar
Para iniciar se cargan los paquetes necesarios para realizar el análisis de regresión. Al utilizar la función p_load() del paquete "pacman", si alguno de los paquetes no está instalado, se instalará automáticamente con la función install.packages(). Los paquetes utilizados son:

haven: para cargar archivos SPSS en R.
texreg: para crear tablas de resumen de regresión.
corrplot: para visualizar las correlaciones entre variables.
coefplot: para crear gráficos de coeficientes de regresión.
ggplot2: para crear gráficos.
sjPlot: para crear tablas de resumen de regresión y gráficos.
summarytools: para crear resúmenes de variables.
dplyr: para manipulación de datos.
lmtest: para pruebas de diagnóstico de regresión.
sandwich: para estimar errores robustos en regresiones.



```r
# Cargar paquetes a utilizar
if (!require("pacman")) install.packages("pacman")
```

```
## Loading required package: pacman
```

```r
pacman::p_load(haven,texreg,corrplot,coefplot,ggplot2,sjPlot,summarytools,dplyr,
               lmtest,sandwich)
```

## Importar datos 
A continuación se carga la base de datos de CASEN 2020. La base de datos se encuentra en la página web del Observatorio Social. La función read_spss() del paquete haven se utiliza para cargar la base de datos.

Primero, se descarga un archivo comprimido que contiene la base de datos desde la página web utilizando la función download.file(). Luego, se descomprime el archivo utilizando la función unz() y se carga la base de datos utilizando la función read_sav(). Finalmente, se elimina el archivo temporal que se creó durante el proceso de descarga y descompresión utilizando la función unlink() y remove().


```r
temp <- tempfile() #Creamos un archivo temporal
download.file("http://observatorio.ministeriodesarrollosocial.gob.cl/storage/docs/casen/2020/Casen_en_Pandemia_2020_revisada202209.sav.zip",temp) #descargamos los datos
base <- haven::read_sav(unz(temp, "Casen_en_Pandemia_2020_revisada202209.sav")) #cargamos los datos
unlink(temp); remove(temp) #eliminamos el archivo temporal
```

## Análisis previos

El análisis de regresón lineal requiere una variable dependiente continua, mientras que las varaibles independientes pueden corresponder a cualquier nivel de medición. Podemos revisar el tipo de datos  de las variables que utilizaremos con la función "class()"

```r
class(base$yautcor)
```

```
## [1] "numeric"
```

```r
class(base$esc)
```

```
## [1] "numeric"
```

```r
class(base$edad)
```

```
## [1] "numeric"
```

```r
class(as_factor(base$sexo))
```

```
## [1] "factor"
```

Se utiliza la función dfSummary() del paquete summarytools para crear un resumen de las variables yautcor, esc, edad y sexo de la base de datos. El resumen incluye estadísticas descriptivas y de frecuencia.


```r
print(dfSummary(base[,c("yautcor","esc","edad","sexo")], headings = FALSE, method = "render"))
```

```
## 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
## No   Variable                               Label                                   Stats / Values                   Freqs (% of Valid)      Graph               Valid      Missing  
## ---- -------------------------------------- --------------------------------------- -------------------------------- ----------------------- ------------------- ---------- ---------
## 1    yautcor                                Ingreso autónomo corregido              Mean (sd) : 517331.5 (1163534)   15287 distinct values   :                   102165     83272    
##      [numeric]                                                                      min < med < max:                                         :                   (55.1%)    (44.9%)  
##                                                                                     17 < 320000 < 225200000                                  :                                       
##                                                                                     IQR (CV) : 425000 (2.2)                                  :                                       
##                                                                                                                                              :                                       
## 
## 2    esc                                    Años de escolaridad truncada (edad >=   Mean (sd) : 11.3 (4.3)           23 distinct values                :         148886     36551    
##      [numeric]                              15)                                     min < med < max:                                                   :         (80.3%)    (19.7%)  
##                                                                                     0 < 12 < 22                                                        :   .                         
##                                                                                     IQR (CV) : 6 (0.4)                                             . : : : :                         
##                                                                                                                                              . . : : : : : : .                       
## 
## 3    edad                                   Edad                                    Mean (sd) : 38.4 (22.8)          108 distinct values     . : :   . .         185437     0        
##      [numeric]                                                                      min < med < max:                                         : : : : : :         (100.0%)   (0.0%)   
##                                                                                     0 < 37 < 110                                             : : : : : : .                           
##                                                                                     IQR (CV) : 38 (0.6)                                      : : : : : : :                           
##                                                                                                                                              : : : : : : : : .                       
## 
## 4    sexo                                   Sexo                                    Min  : 1                         1 : 86096 (46.4%)       IIIIIIIII           185437     0        
##      [haven_labelled, vctrs_vctr, double]                                           Mean : 1.5                       2 : 99341 (53.6%)       IIIIIIIIII          (100.0%)   (0.0%)   
##                                                                                     Max  : 2                                                                                         
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
```

A continuación realizamos un análisis de correlaciones bivariadas con el fin de revisar si existe una relación lineal entre la varaible dependiente y las variables independientes, además de comprobar que no existan problemas de colinealidad entre estas últimas.


```r
#correlaciones
cor(base[,c("yautcor","esc","edad","sexo")], use="complete.obs")
```

```
##             yautcor         esc        edad        sexo
## yautcor  1.00000000  0.24719354 -0.02556606 -0.08642795
## esc      0.24719354  1.00000000 -0.41626104  0.02755352
## edad    -0.02556606 -0.41626104  1.00000000  0.01184122
## sexo    -0.08642795  0.02755352  0.01184122  1.00000000
```

```r
mc<-cor(base[,c("yautcor","esc","edad","sexo")], use="complete.obs")
corrplot(mc, method = 'number', type = 'upper')
```

<img src="/example/03-practico_files/figure-html/unnamed-chunk-5-1.png" width="672" />



```r
g=ggplot(base, aes(x=log(yautcor), y=esc)) +
  geom_point()+ geom_smooth(method=lm, se=FALSE)
g
```

```
## `geom_smooth()` using formula = 'y ~ x'
```

<img src="/example/03-practico_files/figure-html/unnamed-chunk-6-1.png" width="672" />


```r
modelo1 <- lm(yautcor~ esc,data = base)
summary(modelo1)
```

```
## 
## Call:
## lm(formula = yautcor ~ esc, data = base)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
##  -1230799   -337875   -131302    137567 224375890 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept) -261505.5    10619.6  -24.62   <2e-16 ***
## esc           67851.0      853.8   79.47   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1142000 on 97031 degrees of freedom
##   (88404 observations deleted due to missingness)
## Multiple R-squared:  0.0611,	Adjusted R-squared:  0.06109 
## F-statistic:  6315 on 1 and 97031 DF,  p-value: < 2.2e-16
```


```r
1-var(modelo1$residuals)/
  var(base$yautcor[!is.na(base$esc)&!is.na(base$edad)&!is.na(base$sexo)],na.rm=T)
```

```
## [1] 0.06110465
```


```r
modelo2 <- lm(yautcor~ esc+ edad+ as_factor(sexo),data = base)
modelo3 <- lm(yautcor~ esc+ edad+ as_factor(sexo),data = base,weights = expr)
```


```r
htmlreg(list(modelo1,modelo2,modelo3), custom.coef.names = c("Intercepto","Escolaridad","Edad","Sexo (ref. Hombre)"))
```

<table class="texreg" style="margin: 10px auto;border-collapse: collapse;border-spacing: 0px;caption-side: bottom;color: #000000;border-top: 2px solid #000000;">
<caption>Statistical models</caption>
<thead>
<tr>
<th style="padding-left: 5px;padding-right: 5px;">&nbsp;</th>
<th style="padding-left: 5px;padding-right: 5px;">Model 1</th>
<th style="padding-left: 5px;padding-right: 5px;">Model 2</th>
<th style="padding-left: 5px;padding-right: 5px;">Model 3</th>
</tr>
</thead>
<tbody>
<tr style="border-top: 1px solid #000000;">
<td style="padding-left: 5px;padding-right: 5px;">Intercepto</td>
<td style="padding-left: 5px;padding-right: 5px;">-261505.49<sup>&#42;&#42;&#42;</sup></td>
<td style="padding-left: 5px;padding-right: 5px;">-594088.01<sup>&#42;&#42;&#42;</sup></td>
<td style="padding-left: 5px;padding-right: 5px;">-674540.62<sup>&#42;&#42;&#42;</sup></td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
<td style="padding-left: 5px;padding-right: 5px;">(10619.62)</td>
<td style="padding-left: 5px;padding-right: 5px;">(18905.38)</td>
<td style="padding-left: 5px;padding-right: 5px;">(18333.01)</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">Escolaridad</td>
<td style="padding-left: 5px;padding-right: 5px;">67850.97<sup>&#42;&#42;&#42;</sup></td>
<td style="padding-left: 5px;padding-right: 5px;">79569.00<sup>&#42;&#42;&#42;</sup></td>
<td style="padding-left: 5px;padding-right: 5px;">87210.88<sup>&#42;&#42;&#42;</sup></td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
<td style="padding-left: 5px;padding-right: 5px;">(853.83)</td>
<td style="padding-left: 5px;padding-right: 5px;">(931.45)</td>
<td style="padding-left: 5px;padding-right: 5px;">(903.12)</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">Edad</td>
<td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
<td style="padding-left: 5px;padding-right: 5px;">6418.43<sup>&#42;&#42;&#42;</sup></td>
<td style="padding-left: 5px;padding-right: 5px;">6726.08<sup>&#42;&#42;&#42;</sup></td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
<td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
<td style="padding-left: 5px;padding-right: 5px;">(226.26)</td>
<td style="padding-left: 5px;padding-right: 5px;">(217.86)</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">Sexo (ref. Hombre)</td>
<td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
<td style="padding-left: 5px;padding-right: 5px;">-225226.51<sup>&#42;&#42;&#42;</sup></td>
<td style="padding-left: 5px;padding-right: 5px;">-243306.54<sup>&#42;&#42;&#42;</sup></td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
<td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
<td style="padding-left: 5px;padding-right: 5px;">(7273.07)</td>
<td style="padding-left: 5px;padding-right: 5px;">(6956.87)</td>
</tr>
<tr style="border-top: 1px solid #000000;">
<td style="padding-left: 5px;padding-right: 5px;">R<sup>2</sup></td>
<td style="padding-left: 5px;padding-right: 5px;">0.06</td>
<td style="padding-left: 5px;padding-right: 5px;">0.08</td>
<td style="padding-left: 5px;padding-right: 5px;">0.10</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">Adj. R<sup>2</sup></td>
<td style="padding-left: 5px;padding-right: 5px;">0.06</td>
<td style="padding-left: 5px;padding-right: 5px;">0.08</td>
<td style="padding-left: 5px;padding-right: 5px;">0.10</td>
</tr>
<tr style="border-bottom: 2px solid #000000;">
<td style="padding-left: 5px;padding-right: 5px;">Num. obs.</td>
<td style="padding-left: 5px;padding-right: 5px;">97033</td>
<td style="padding-left: 5px;padding-right: 5px;">97033</td>
<td style="padding-left: 5px;padding-right: 5px;">97033</td>
</tr>
</tbody>
<tfoot>
<tr>
<td style="font-size: 0.8em;" colspan="4"><sup>&#42;&#42;&#42;</sup>p &lt; 0.001; <sup>&#42;&#42;</sup>p &lt; 0.01; <sup>&#42;</sup>p &lt; 0.05</td>
</tr>
</tfoot>
</table>


```r
coefplot(modelo3)+scale_y_discrete(labels=c("Intercepto","Escolaridad","Edad","Sexo","Sexo (ref. Hombre)"))+
  labs(title = "Gráfico de coeficientes",subtitle = "Modelo 3")+ylab("Coeficientes")
```

<img src="/example/03-practico_files/figure-html/unnamed-chunk-11-1.png" width="672" />

## Revisión de supuestos 


```r
#Casos influyentes
n<- nobs(modelo3) #n de observaciones
k<- length(coef(modelo3)) # n de parametros
dcook<- 4/(n-k-1) #punt de corte


final <- broom::augment_columns(modelo3,data = base)
final$id <- as.numeric(row.names(final))
# identify obs with Cook's D above cutoff
ggplot(final, aes(id, .cooksd))+
  geom_bar(stat="identity", position="identity")+
  xlab("Obs. Number")+ylab("Cook's distance")+
  geom_hline(yintercept=dcook)+
  geom_text(aes(label=ifelse((.cooksd>dcook),id,"")),
            vjust=-0.2, hjust=0.5)
```

<img src="/example/03-practico_files/figure-html/unnamed-chunk-12-1.png" width="672" />

```r
ident<- final %>% filter(.cooksd>dcook)
base2<- final %>% filter(!(id %in% ident$id))

modelo4<-lm(yautcor~ esc+ edad+ as_factor(sexo),data = base2,weights = expr)
htmlreg(list(modelo3,modelo4), custom.coef.names = c("Intercepto","Escolaridad","Edad","Sexo (ref. Hombre)"))
```

<table class="texreg" style="margin: 10px auto;border-collapse: collapse;border-spacing: 0px;caption-side: bottom;color: #000000;border-top: 2px solid #000000;">
<caption>Statistical models</caption>
<thead>
<tr>
<th style="padding-left: 5px;padding-right: 5px;">&nbsp;</th>
<th style="padding-left: 5px;padding-right: 5px;">Model 1</th>
<th style="padding-left: 5px;padding-right: 5px;">Model 2</th>
</tr>
</thead>
<tbody>
<tr style="border-top: 1px solid #000000;">
<td style="padding-left: 5px;padding-right: 5px;">Intercepto</td>
<td style="padding-left: 5px;padding-right: 5px;">-674540.62<sup>&#42;&#42;&#42;</sup></td>
<td style="padding-left: 5px;padding-right: 5px;">-333646.62<sup>&#42;&#42;&#42;</sup></td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
<td style="padding-left: 5px;padding-right: 5px;">(18333.01)</td>
<td style="padding-left: 5px;padding-right: 5px;">(6927.29)</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">Escolaridad</td>
<td style="padding-left: 5px;padding-right: 5px;">87210.88<sup>&#42;&#42;&#42;</sup></td>
<td style="padding-left: 5px;padding-right: 5px;">58232.06<sup>&#42;&#42;&#42;</sup></td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
<td style="padding-left: 5px;padding-right: 5px;">(903.12)</td>
<td style="padding-left: 5px;padding-right: 5px;">(348.59)</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">Edad</td>
<td style="padding-left: 5px;padding-right: 5px;">6726.08<sup>&#42;&#42;&#42;</sup></td>
<td style="padding-left: 5px;padding-right: 5px;">3981.04<sup>&#42;&#42;&#42;</sup></td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
<td style="padding-left: 5px;padding-right: 5px;">(217.86)</td>
<td style="padding-left: 5px;padding-right: 5px;">(81.17)</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">Sexo (ref. Hombre)</td>
<td style="padding-left: 5px;padding-right: 5px;">-243306.54<sup>&#42;&#42;&#42;</sup></td>
<td style="padding-left: 5px;padding-right: 5px;">-156878.56<sup>&#42;&#42;&#42;</sup></td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
<td style="padding-left: 5px;padding-right: 5px;">(6956.87)</td>
<td style="padding-left: 5px;padding-right: 5px;">(2572.57)</td>
</tr>
<tr style="border-top: 1px solid #000000;">
<td style="padding-left: 5px;padding-right: 5px;">R<sup>2</sup></td>
<td style="padding-left: 5px;padding-right: 5px;">0.10</td>
<td style="padding-left: 5px;padding-right: 5px;">0.25</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">Adj. R<sup>2</sup></td>
<td style="padding-left: 5px;padding-right: 5px;">0.10</td>
<td style="padding-left: 5px;padding-right: 5px;">0.25</td>
</tr>
<tr style="border-bottom: 2px solid #000000;">
<td style="padding-left: 5px;padding-right: 5px;">Num. obs.</td>
<td style="padding-left: 5px;padding-right: 5px;">97033</td>
<td style="padding-left: 5px;padding-right: 5px;">94655</td>
</tr>
</tbody>
<tfoot>
<tr>
<td style="font-size: 0.8em;" colspan="3"><sup>&#42;&#42;&#42;</sup>p &lt; 0.001; <sup>&#42;&#42;</sup>p &lt; 0.01; <sup>&#42;</sup>p &lt; 0.05</td>
</tr>
</tfoot>
</table>


```r
#Linealidad
ggplot(modelo4, aes(.fitted, .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_smooth(se = TRUE)
```

```
## `geom_smooth()` using method = 'gam' and formula = 'y ~ s(x, bs = "cs")'
```

<img src="/example/03-practico_files/figure-html/unnamed-chunk-13-1.png" width="672" />


```r
#homogenidad de varianza
car::ncvTest(modelo4)
```

Non-constant Variance Score Test 
Variance formula: ~ fitted.values 
Chisquare = 28265.61, Df = 1, p = < 2.22e-16

```r
model_robust<- coeftest(modelo4, vcov=vcovHC)
htmlreg(list(modelo4,model_robust), custom.coef.names = c("Intercepto","Escolaridad","Edad","Sexo (ref. Hombre)"))
```

<table class="texreg" style="margin: 10px auto;border-collapse: collapse;border-spacing: 0px;caption-side: bottom;color: #000000;border-top: 2px solid #000000;">
<caption>Statistical models</caption>
<thead>
<tr>
<th style="padding-left: 5px;padding-right: 5px;">&nbsp;</th>
<th style="padding-left: 5px;padding-right: 5px;">Model 1</th>
<th style="padding-left: 5px;padding-right: 5px;">Model 2</th>
</tr>
</thead>
<tbody>
<tr style="border-top: 1px solid #000000;">
<td style="padding-left: 5px;padding-right: 5px;">Intercepto</td>
<td style="padding-left: 5px;padding-right: 5px;">-333646.62<sup>&#42;&#42;&#42;</sup></td>
<td style="padding-left: 5px;padding-right: 5px;">-333646.62<sup>&#42;&#42;&#42;</sup></td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
<td style="padding-left: 5px;padding-right: 5px;">(6927.29)</td>
<td style="padding-left: 5px;padding-right: 5px;">(7733.36)</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">Escolaridad</td>
<td style="padding-left: 5px;padding-right: 5px;">58232.06<sup>&#42;&#42;&#42;</sup></td>
<td style="padding-left: 5px;padding-right: 5px;">58232.06<sup>&#42;&#42;&#42;</sup></td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
<td style="padding-left: 5px;padding-right: 5px;">(348.59)</td>
<td style="padding-left: 5px;padding-right: 5px;">(457.22)</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">Edad</td>
<td style="padding-left: 5px;padding-right: 5px;">3981.04<sup>&#42;&#42;&#42;</sup></td>
<td style="padding-left: 5px;padding-right: 5px;">3981.04<sup>&#42;&#42;&#42;</sup></td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
<td style="padding-left: 5px;padding-right: 5px;">(81.17)</td>
<td style="padding-left: 5px;padding-right: 5px;">(86.29)</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">Sexo (ref. Hombre)</td>
<td style="padding-left: 5px;padding-right: 5px;">-156878.56<sup>&#42;&#42;&#42;</sup></td>
<td style="padding-left: 5px;padding-right: 5px;">-156878.56<sup>&#42;&#42;&#42;</sup></td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
<td style="padding-left: 5px;padding-right: 5px;">(2572.57)</td>
<td style="padding-left: 5px;padding-right: 5px;">(2973.76)</td>
</tr>
<tr style="border-top: 1px solid #000000;">
<td style="padding-left: 5px;padding-right: 5px;">R<sup>2</sup></td>
<td style="padding-left: 5px;padding-right: 5px;">0.25</td>
<td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">Adj. R<sup>2</sup></td>
<td style="padding-left: 5px;padding-right: 5px;">0.25</td>
<td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
</tr>
<tr style="border-bottom: 2px solid #000000;">
<td style="padding-left: 5px;padding-right: 5px;">Num. obs.</td>
<td style="padding-left: 5px;padding-right: 5px;">94655</td>
<td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
</tr>
</tbody>
<tfoot>
<tr>
<td style="font-size: 0.8em;" colspan="3"><sup>&#42;&#42;&#42;</sup>p &lt; 0.001; <sup>&#42;&#42;</sup>p &lt; 0.01; <sup>&#42;</sup>p &lt; 0.05</td>
</tr>
</tfoot>
</table>


```r
#multicolinealidad
car::vif(modelo4) #Se espera que no existan valores mayores a 2.5
```

```
##             esc            edad as_factor(sexo) 
##        1.232273        1.230670        1.003348
```


```r
#Normalidad de los residuos
hist(modelo4$residuals)
```

<img src="/example/03-practico_files/figure-html/unnamed-chunk-16-1.png" width="672" />
