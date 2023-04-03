---
title: "Importar, explorar y exportar datos"
linktitle: "4: Importar, explorar y exportar datos"
date: "2021-08-23"
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
<script type="application/json" id="xaringanExtra-editable-docid">{"id":"x846609390974a77a85032691c7153de","expires":14}</script>
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


