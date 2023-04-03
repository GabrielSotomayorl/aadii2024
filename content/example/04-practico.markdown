---
title: "Análisis de regresión logística"
linktitle: "4: Análisis de regresión logística"
date: "2023-04-02"
menu:
  example:
    parent: Ejemplos
    weight: 4
type: docs
toc: true
editor_options: 
  chunk_output_type: console
---
<link href="/rmarkdown-libs/tile-view/tile-view.css" rel="stylesheet" />
<script src="/rmarkdown-libs/tile-view/tile-view.js"></script>
<link href="/rmarkdown-libs/animate.css/animate.xaringan.css" rel="stylesheet" />
<script type="application/json" id="xaringanExtra-editable-docid">{"id":"x6998340b33c4fa8bcafeb28b407cda0","expires":14}</script>
<script src="/rmarkdown-libs/himalaya/himalaya.js"></script>
<script src="/rmarkdown-libs/js-cookie/js.cookie.js"></script>
<link href="/rmarkdown-libs/editable/editable.css" rel="stylesheet" />
<script src="/rmarkdown-libs/editable/editable.js"></script>
<script src="/rmarkdown-libs/clipboard/clipboard.min.js"></script>
<link href="/rmarkdown-libs/shareon/shareon.min.css" rel="stylesheet" />
<script src="/rmarkdown-libs/shareon/shareon.min.js"></script>
<link href="/rmarkdown-libs/xaringanExtra-shareagain/shareagain.css" rel="stylesheet" />
<script src="/rmarkdown-libs/xaringanExtra-shareagain/shareagain.js"></script>
<script src="/rmarkdown-libs/fabric/fabric.min.js"></script>
<link href="/rmarkdown-libs/xaringanExtra-scribble/scribble.css" rel="stylesheet" />
<script src="/rmarkdown-libs/xaringanExtra-scribble/scribble.js"></script>
<script>document.addEventListener('DOMContentLoaded', function() { window.xeScribble = new Scribble({"pen_color":["#FF0000"],"pen_size":3,"eraser_size":30,"palette":[]}) })</script>
<script src="/rmarkdown-libs/clipboard/clipboard.min.js"></script>
<link href="/rmarkdown-libs/xaringanExtra-clipboard/xaringanExtra-clipboard.css" rel="stylesheet" />
<script src="/rmarkdown-libs/xaringanExtra-clipboard/xaringanExtra-clipboard.js"></script>
<script>window.xaringanExtraClipboard(null, {"button":"<i class=\"fa fa-clipboard\">Copiar código<\/i>","success":"<i class=\"fa fa-check\" style=\"color: #90BE6D\">¡Listo!<\/i>","error":"<i class=\"fa fa-times-circle\" style=\"color: #F94144\"><\/i>"})</script>
<link href="/rmarkdown-libs/font-awesome/css/all.css" rel="stylesheet" />
<link href="/rmarkdown-libs/font-awesome/css/v4-shims.css" rel="stylesheet" />






# 0. Objetivo del práctico

El objetivo de este práctico es aprender a ejecutar análisis de regresión logística binaria en R, visualizar sus resultados y evaluar el ajuste de los modelos.   
Para esto haremos uso de la encuesta [CASEN (2020)](http://observatorio.ministeriodesarrollosocial.gob.cl/encuesta-casen-en-pandemia-2020), la mayor encuesta de hogares realizada en Chile, a cargo del Ministerio de Desarrollo Social, de carácter transversal y multipropósito, es el principal instrumento de medición socioeconómica para el diseño y evaluación de la política social. Permite conocer periódicamente la situación socioeconómica de los hogares y de la población que reside en viviendas particulares, a través de preguntas referidas a composición familiar, educación, salud, vivienda, trabajo e ingresos, entre otros aspectos. 


# 1. Carga y preparación de la base de datos.


```r
library(haven)
temp <- tempfile() #Creamos un archivo temporal
download.file("http://observatorio.ministeriodesarrollosocial.gob.cl/storage/docs/casen/2020/Casen_en_Pandemia_2020_revisada202209.sav.zip",temp) #descargamos los datos
casen <- haven::read_sav(unz(temp, "Casen_en_Pandemia_2020_revisada202209.sav")) #cargamos los datos
unlink(temp); remove(temp) #eliminamos el archivo temporal
```

Para ejecutar un modelo de regresión logística necesitamos que nuestra variable dependiente esté codificada con valores 0 y 1. En este caso transformaremos la variable pobreza, que cuenta con tres valores, a una variable dicotómica donde 0 es no pobre y 1 es pobre. 


```r
table(as_factor(casen$pobreza))
```

```
## 
##    Pobres extremos Pobres no extremos          No pobres 
##               8435              12862             164042
```

```r
casen$pobre<-car::recode(casen$pobreza,"1:2=1;3=0")
```

Además filtraremos la base de datos para quedarnos solo con las jefaturas de hogar, de modo de tener solo un caso por hogar.


```r
casen<-casen[casen$pco1==1,]
```


# 2. Estimación del modelo e interpretación de coeficientes

Para estimar un modelo de regresión logística binaria usaremos el comado **glm**


```r
library(texreg)
```

```
## Warning: package 'texreg' was built under R version 4.2.2
```

```
## Version:  1.38.6
## Date:     2022-04-06
## Author:   Philip Leifeld (University of Essex)
## 
## Consider submitting praise using the praise or praise_interactive functions.
## Please cite the JSS article in your publications -- see citation("texreg").
```

```r
#para ver el output en la consola de R, reemplazar función htmlreg por screenreg

htmlreg(glm(pobre~as_factor(sexo)+edad, data=casen, family = "binomial"),
        custom.coef.names = c("Intercepto","Mujer (ref.hombre)","Edad"),
        custom.model.names = "Pobreza según sexo JH",single.row = T)
```

<table class="texreg" style="margin: 10px auto;border-collapse: collapse;border-spacing: 0px;caption-side: bottom;color: #000000;border-top: 2px solid #000000;">
<caption>Statistical models</caption>
<thead>
<tr>
<th style="padding-left: 5px;padding-right: 5px;">&nbsp;</th>
<th style="padding-left: 5px;padding-right: 5px;">Pobreza según sexo JH</th>
</tr>
</thead>
<tbody>
<tr style="border-top: 1px solid #000000;">
<td style="padding-left: 5px;padding-right: 5px;">Intercepto</td>
<td style="padding-left: 5px;padding-right: 5px;">-1.07 (0.05)<sup>&#42;&#42;&#42;</sup></td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">Mujer (ref.hombre)</td>
<td style="padding-left: 5px;padding-right: 5px;">0.39 (0.03)<sup>&#42;&#42;&#42;</sup></td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">Edad</td>
<td style="padding-left: 5px;padding-right: 5px;">-0.03 (0.00)<sup>&#42;&#42;&#42;</sup></td>
</tr>
<tr style="border-top: 1px solid #000000;">
<td style="padding-left: 5px;padding-right: 5px;">AIC</td>
<td style="padding-left: 5px;padding-right: 5px;">40182.30</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">BIC</td>
<td style="padding-left: 5px;padding-right: 5px;">40209.45</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">Log Likelihood</td>
<td style="padding-left: 5px;padding-right: 5px;">-20088.15</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">Deviance</td>
<td style="padding-left: 5px;padding-right: 5px;">40176.30</td>
</tr>
<tr style="border-bottom: 2px solid #000000;">
<td style="padding-left: 5px;padding-right: 5px;">Num. obs.</td>
<td style="padding-left: 5px;padding-right: 5px;">62911</td>
</tr>
</tbody>
<tfoot>
<tr>
<td style="font-size: 0.8em;" colspan="2"><sup>&#42;&#42;&#42;</sup>p &lt; 0.001; <sup>&#42;&#42;</sup>p &lt; 0.01; <sup>&#42;</sup>p &lt; 0.05</td>
</tr>
</tfoot>
</table>

Interpretación: 

Los log-odds predichos de encontrarse en situación de pobreza aumentan en 0,39 en los hogares con jefatura femenina respecto aaquellos con jefatura masculina controlando por edad

En el mismo sentido, los log-odds predichos de ser encontrarse en situación de pobreza disminuyen en 0.03 por cada año más de edad del jefe de hogar, controlando por sexo.



```r
modelo2<-glm(pobre~as_factor(sexo)+edad, data=casen, family = "binomial")
or <- texreg::extract(modelo2)
or@coef <- exp(or@coef)


htmlreg(or,
        custom.coef.names = c("Intercepto","Mujer (ref.hombre)","Edad"),
        custom.model.names = "Pobreza según sexo JH",single.row = T)
```

<table class="texreg" style="margin: 10px auto;border-collapse: collapse;border-spacing: 0px;caption-side: bottom;color: #000000;border-top: 2px solid #000000;">
<caption>Statistical models</caption>
<thead>
<tr>
<th style="padding-left: 5px;padding-right: 5px;">&nbsp;</th>
<th style="padding-left: 5px;padding-right: 5px;">Pobreza según sexo JH</th>
</tr>
</thead>
<tbody>
<tr style="border-top: 1px solid #000000;">
<td style="padding-left: 5px;padding-right: 5px;">Intercepto</td>
<td style="padding-left: 5px;padding-right: 5px;">0.34 (0.05)<sup>&#42;&#42;&#42;</sup></td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">Mujer (ref.hombre)</td>
<td style="padding-left: 5px;padding-right: 5px;">1.47 (0.03)<sup>&#42;&#42;&#42;</sup></td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">Edad</td>
<td style="padding-left: 5px;padding-right: 5px;">0.97 (0.00)<sup>&#42;&#42;&#42;</sup></td>
</tr>
<tr style="border-top: 1px solid #000000;">
<td style="padding-left: 5px;padding-right: 5px;">AIC</td>
<td style="padding-left: 5px;padding-right: 5px;">40182.30</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">BIC</td>
<td style="padding-left: 5px;padding-right: 5px;">40209.45</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">Log Likelihood</td>
<td style="padding-left: 5px;padding-right: 5px;">-20088.15</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">Deviance</td>
<td style="padding-left: 5px;padding-right: 5px;">40176.30</td>
</tr>
<tr style="border-bottom: 2px solid #000000;">
<td style="padding-left: 5px;padding-right: 5px;">Num. obs.</td>
<td style="padding-left: 5px;padding-right: 5px;">62911</td>
</tr>
</tbody>
<tfoot>
<tr>
<td style="font-size: 0.8em;" colspan="2"><sup>&#42;&#42;&#42;</sup>p &lt; 0.001; <sup>&#42;&#42;</sup>p &lt; 0.01; <sup>&#42;</sup>p &lt; 0.05</td>
</tr>
</tfoot>
</table>



# 3. Ajuste del modelo

Como vimos durante la clase, la interpretación de ajuste de los modelos de regresión logística tiene una orientación principalmente comparativa. 


```r
modelonulo<-glm(pobre~1, data=casen, family = "binomial")
modelo1<-glm(pobre~as_factor(sexo), data=casen, family = "binomial")
modelo2<-glm(pobre~as_factor(sexo)+edad, data=casen, family = "binomial")

anova(modelonulo,modelo1, test ="Chisq")
```

```
## Analysis of Deviance Table
## 
## Model 1: pobre ~ 1
## Model 2: pobre ~ as_factor(sexo)
##   Resid. Df Resid. Dev Df Deviance  Pr(>Chi)    
## 1     62910      41314                          
## 2     62909      41071  1    243.1 < 2.2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```



```r
anova(modelo1,modelo2, test ="Chisq")
```

```
## Analysis of Deviance Table
## 
## Model 1: pobre ~ as_factor(sexo)
## Model 2: pobre ~ as_factor(sexo) + edad
##   Resid. Df Resid. Dev Df Deviance  Pr(>Chi)    
## 1     62909      41071                          
## 2     62908      40176  1   894.27 < 2.2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```



```r
library(DescTools)

PseudoR2(modelo1,which="McFadden")
```

```
##    McFadden 
## 0.005884247
```

```r
PseudoR2(modelo2,which="McFadden")
```

```
##   McFadden 
## 0.02753019
```


# 4. Visualización de resultados



```r
library(sjPlot)

plot_model(modelo2,vline.color = "grey")
```

```
## Profiled confidence intervals may take longer time to compute.
##   Use `ci_method="wald"` for faster computation of CIs.
```

<img src="/example/04-practico_files/figure-html/unnamed-chunk-8-1.png" width="672" />
