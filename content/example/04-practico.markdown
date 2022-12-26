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
<script type="application/json" id="xaringanExtra-editable-docid">{"id":"c61759ddce144eaa8d3e9b1f88db4dd8","expires":14}</script>
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






## 0. Objetivo del práctico

Este práctico tiene por objeto introducir a las y los estudiantes del curso en herramientas que les permitan a) **importar bases de datos en diversos formatos**; b)**explorar las variables** de las bases de datos importadas; y c) **exportar bases de datos** procesadas. Para ello, se trabajará con los paquetes `haven` (que forma parte, a su vez, del mundo de paquetes `tidyverse`), y `dplyr`, para poder importar  y exportar los datos, y manipular estos datos, respectivamente.

## 1. Recursos de la práctica

Para el correcto trabajo de estos materiales, deben descargar los datos de la *Encuesta de Caracterización Socioeconómica (CASEN)* en su versión 2020. Para ello, deben dirigirse al [siguiente enlace](https://drive.google.com/drive/folders/1Orgb3Qb9LcjTfjYMdIdy7SWd3xDMrTbG?usp=sharing), descargar el archivo en .zip y luego descomprimirlo en la carpeta _input/data_ de su repositorio. Para descomprimir los [archivos revisen](https://learn-r-uah.netlify.app/resource/unzipping/). Cuando lo hayan logrado verán la base en formato SPPS `Casen en Pandemia 2020 SPSS.sav` y en STATA `Casen en Pandemia 2020 SPSS.dta`

Recuerden siempre consultar el [**manual/libro de códigos**](http://observatorio.ministeriodesarrollosocial.gob.cl/storage/docs/casen/2020/Libro_de_codigos_Base_de_Datos_Casen_en_Pandemia_2020.pdf) antes de trabajar una base de datos.

## 2. Librerias a utilizar

En este práctico utilizaremos seis paquetes

1. `pacman`: este facilita y agiliza la lectura de los paquetes a utilizar en R

2. `sjmisc`: explorar datos

3. `tidyverse`: colección de paquetes, de la cual utilizaremos `dplyr` y `haven`

4. `dplyr`: nos permite seleccionar variables de un set de datos

5. `haven`: cargar y exportar bases de datos en formatos .sav y .dta

6. `readxl` y `writexl`: para cargar y exportar bases de datos en formato .xlsx y .xls


# Pasos del procesamiento

## 1. Cargar librerías

Dado que ya cargamos `pacman` en el práctico anterior, no lo debemos volver a instalar. Ahora bien, si  cargaremos los nuevos paquetes. Les recordamos que cuando luego de un paquete ponemos `::` esto se refiere a que se *"fuerza"* que esa función provenga de *ese paquete*


```r
pacman::p_load(sjmisc,
               sjPlot,
               tidyverse,
               haven,
               readxl,
               writexl)
```

## 2. Importar datos

Para poder realizar análisis estadístico de cualquier tipo, el primer paso - sin considerar, por supuesto, la formulación de un problema de investigación, y la búsqueda de datos que permitan resolverlo - es **importar una base de datos**. Por razones obvias, si no hemos cargados los datos en el *entorno*, no seremos capaces de realizar ningún otro procedimiento, ni de preparación de los datos - por ejemplo, seleccionar variables, recodificarlas, construir variables sumativas, entre otros -, ni de análisis de estos - ya sean descriptivos, relacionales, explicativos, y así. 

Una de las bondades de **R** es que es posible importar y exportar fácilmente datos que se encuentren en *cualquier formato*: ya sea .csv, .dta, .sav, .xlsx y, por supuesto, .rds y .RData. Para poder hacerlo, sin embargo, lo primero es *instalar* y *cargar* las **librerías** que contienen las **funciones** necesarias para la importación de distintos tipos de archivos. 


### 2.1. Cargar set de datos

Una vez cargado el paquete `haven`, procedemos a *importar* los datos anteriormente mencionados. Para ello, en nuestro script, dejamos indicado que a partir de la lectura de los datos con `read_sav()`, crearemos un objeto llamado `datos`. Si este procedimiento se logra, esto aparecerá en el *Enviroment*.





Antes ¿dónde están nuestros datos? Por lo general, nuestros datos los dejaremos en la carpeta `input/data`. En el [siguiente enlace](https://drive.google.com/drive/folders/1Orgb3Qb9LcjTfjYMdIdy7SWd3xDMrTbG?usp=sharing) podrán descargar el archivo .zip que contiene la base de CASEN 2020. Si aún no sabes como descomprimir datos, por favor revisa [aquí](https://learn-r-udp.netlify.app/resource/unzipping/). 

Luego de que hayas **descargado y descomprimido los datos** asegurate de dejar el archivo `.sav` y `.dta` en la carpeta de tu proyecto `input/data`. Los datos tendrán distinto nombre según su formato: `Casen en Pandemia 2020 SPSS.sav` o `Casen en Pandemia 2020 SPSS.dta`.


{{< div "note" >}}
Para **importar** los datos en R debemos tener en consideración tres cosas:

1. Cómo se llaman los datos (en nuestro caso Casen en Pandemia 2020 SPSS)

2. El formato de nuestros datos (en nuestro caso .sav)

3. El lugar de donde están alojados nuestros datos. 
{{< /div >}}

El [paquete `haven`](https://haven.tidyverse.org/) tiene una serie de funciones para cargar datos llamadas `read_*` (luego del asterisco, debemos indicar el formato de los datos). Para ocuparlo solo debes tener en claro los puntos anteriores

`read_*("ruta_hacia_archivo/nombre_archivo.*"``. En nuestro caso:



```r
datos <- read_sav("Casen en Pandemia 2020 SPSS.sav")  # No funciona
datos <- read_sav("input/data/casen en pandemia 2020 SPSS.sav") # No funciona
datos <- read_sav("input/data/Casen en Pandemia 2020 SPSS.sav") 
```

Los dos primeros comandos no funcionan porque

1. La ruta no está bien definida

2. El nombre de la base no es exacto

¡Dos errores muy frecuentes cuando uno se inicia! Ahora, ¿cómo sé si están bien los datos que acabo de cargar? Primero, notarás que en tu **Enviroment** se ha creado un objeto. En nuestro caso objeto posee 650 variables (columnas), y 185.437 filas (u observaciones)

¡Ahora es tu turno! Intenta hacer este procedimiento con los datos en `.dta`. También puedes encontrar este archivo en el enlace.

### 2.1.1 Cargar set de datos en otros formatos. 

Ahora bien, no siempre este procedimiento será tan fácil sobre todo pues no siempre vendrán en formatos "limpios". La gran forma de lidiar con eso es con la **manipulación de datos** (algo que nos tomará tiempo en este curso), pero de todas formas es de gran ayuda importar los datos de manera correcta, pues ello nos puede solucionar varios problemas de codificación y lectura. 

Para poder abordar esto de la mejor manera es necesario utilizar funciones *ad hoc* al formato de nuestros datos. Como se mencionó anteriormente, R es muy flexible en esto. En los siguientes pasos les mostraremos como cargar datos en otros formatos, reconociendo algunos de los *problemas comunes con los cuáles te encontrarás* (de seguro...)

#### a) `.dta`

Este es el formato específico de base de datos para STATA. Por ello, utilizaremos la función `read_dta()` (o `read_stata()`, que es lo mismo), incluida en el paquete `haven`.



#### b) .RData y .rds

¿Y R tiene formato de datos? ¡Sí! Como son propios del programa **no es necesario cargar paquetes**, pues las funciones para importarlos provienen del paquete `base`. La diferencia básica entre `.RData` y `.rds` es que `.rds` puede contener solo un objeto del *Enviroment* de R, mientras que `.RData` puede contener múltiples objetos que han sido guardados en el entorno (¡no sólo datos! también se guardarán gráficos, modelos, entre otros).

{{< div "note" >}}
*Leer un objeto proveniente de un archivo*

readRDS(file = "datos.rds")

*Leer múltiples objetos a un archivo*
load(file = "datos.RData")
{{</div>}}

Además de **nunca olvidar la ruta**, no debes olvidar una diferencia de codificación entre ambas funciones: con `load()` los objetos se cargan *automáticamente* en el ambiente, por lo cual no debemos utilizar operadores de asignación (`<-` o `=`) para crear un nuevo objeto, mientras que en `readRDS()` **sí debemos asignar un nuevo objeto en el ambiente**. 


```r
load(file = "input/data/CASEN.RData")
readRDS(file = "input/data/CASEN.rds")
```

```
## # A tibble: 100 x 5
##       id                                        region    sexo pobreza ocupacion
##    <dbl>                                     <dbl+lbl> <dbl+l> <dbl+l> <chr>    
##  1   502 16 [Región de Ñuble]                          2 [Muj~ 3 [No ~ <NA>     
##  2   184  6 [Región del Libertador Gral. Bernardo O'H~ 2 [Muj~ 3 [No ~ <NA>     
##  3   308  8 [Región del Biobío]                        2 [Muj~ 3 [No ~ <NA>     
##  4   531  6 [Región del Libertador Gral. Bernardo O'H~ 1 [Hom~ 3 [No ~ COSECHA ~
##  5   115  3 [Región de Atacama]                        2 [Muj~ 3 [No ~ <NA>     
##  6   261  5 [Región de Valparaíso]                     2 [Muj~ 3 [No ~ <NA>     
##  7   171  3 [Región de Atacama]                        1 [Hom~ 3 [No ~ ATENCIÓN~
##  8   333  7 [Región del Maule]                         1 [Hom~ 1 [Pob~ RECOLECT~
##  9    69 14 [Región de Los Ríos]                       1 [Hom~ 3 [No ~ EMPRESAR~
## 10   440  4 [Región de Coquimbo]                       1 [Hom~ 2 [Pob~ <NA>     
## # ... with 90 more rows
```


```r
datos <- readRDS(file = "input/data/CASEN.rds")
```


¿Solo contiene datos? ¡No! Además de que puede contener múltiples datos, puede guardar otros *objetos* que se creen en su proceso estadístico. Por ejemplo, *modelos* ¡Lo veremos más adelante!

#### c) `.csv`

Los archivos en `.csv` o `.txt` son de frecuente uso dado que son más livianos que un archivo en `.dta` o `.sav` por el *meta-data* que contienen (información adicional, además de columnas-filas). El paquete `utils` de `R base` tiene una función de muy buena calidad llamada `read.table` (`read.csv()` para archivos en `.csv` y `read.delim()` para archivos en `.txt`)

Su estructura es muy simple `read.*(file = "datos.*")`. Ahora bien tiene una serie de argumentos que permiten leer de mejor manera los archivos **y de seguro los ocuparás**:

- [`sep`]: indica con qué están separadas las columnas (*; , -*)

- [`dec`]: indica como están separados los decimales (*. ,*)

- [`na-strings`]: indica como están codificados los `NA` (¡podría ser más de una forma!)

- [`encoding`]:puede ser `UTF-8` o `Latin-1`

- [`skip`]: indica el *número* de filas que hay que saltarse (no siempre los csv y excel parten en la primera fila). El argumento `nrow` indica el número de filas máximas a leer

- [`stringsAsFactors`]: si se indica verdadero (`TRUE`) los carácteres serán transformados en factores, lo cuál será muy útil en caso de necesitar hacer análisis.


```r
datos <- read.csv("input/data/CASEN.csv", sep=",", 
                  encoding = "UTF-8", stringsAsFactors = F)

head(datos)
```

Muchos problemas, ¡vamos solucionado!


```r
datos <- read.csv("input/data/CASEN.csv", sep=";", 
                  encoding = "Latin-1", stringsAsFactors = F, na.strings = c("No sabe", NA))

head(datos)
```


#### d) `.xlsx` 

A partir del paquete `readxl` de `tidyverse` podremos obtener datos que provienen de Excel (tanto en formato `.xls` como `.xlsx`). Ocuparemos la función `read_excel()`, la cual tiene argumentos similares a los que revisamos en `read.csv()`

{{< div "note" >}}
`read_excel("datos.xlsx", sheet = "Hoja 1", range = "A1:C40")`
`read_excel("datos.xlsx", sheet = 2, skip = 4, na = "No sabe")`
{{</div>}}


```r
datos <- readxl::read_excel(path = "input/data/CASEN.xlsx")
```

¡Hemos sido engañados/as! ¿Cómo solucionar?


```r
datos <- readxl::read_excel(path = "input/data/CASEN.xlsx", sheet = "Hoja1", skip = 1)

datos <- readxl::read_excel(path = "input/data/CASEN.xlsx", sheet = "Hoja1", skip = 1, na = "NA")
```

Para seguir con este ejercicio volvamos a utilizar la base original en `.dta`


```r
datos <- read_dta("input/data/Casen en Pandemia 2020 SPSS.dta") 
```




## 3. Explorar datos 

Lo más probable es que, una vez importados los datos a utilizar, no trabajemos con el total de variables incluidas en estos (que, en este caso, suman un total de *650* columnas ¡imaginen qué locura sería!). Es por ello que debemos **seleccionar** las variables de interés para resolver nuestro problema de investigación (sea el que sea). 

Antes de seleccionar las variables debemos **explorar nuestros datos**, si no ¿cómo saber qué seleccionar y qué no? En R base las funciones clásicas para explorar datos son


```r
View(datos) # Ver datos
names(datos) # Nombre de columnas
dim(datos) # Dimensiones
str(datos) # Estructura de los datos (las clases y categorias de repuesta)
```

A pesar de que son fáciles de aprender, no tienen una visualización muy amena. Un excelente paquete para explorar datos es `sjmisc`, que tiene tres funciones claves:

- [`View_df()`]: que en el visor "Viewer" les mostrará una tabla que tiene el nombre de variable, etiqueta y categorías de respuesta

- [`find_var()`]: que permite indagar en variables que estamos buscando según sus temáticas

- [`frq()`]: nos otorga la distribución univariada de variables categóricas que estamos explorando

- [`descr()`]: nos otorga estadísticos de tendencia central para la variable numérica seleccionada. 

*Ver datos en el visor de RStudio*

```r
sjPlot::view_df(datos)
```

*Buscar variables sobre temáticas relacionadas a "vivienda" (no olvides dejarla entre comillas)*

```r
find_var(datos, "pobreza")
```

```
##   col.nr      var.name
## 1    584       pobreza
## 2    586 pobreza_sinte
## 3    579            lp
##                                                   var.label
## 1                         Situación de pobreza por ingresos
## 2 Situación de pobreza por ingresos sin transferencia Covid
## 3                                          Línea de pobreza
```

```r
find_var(datos, "salario")
```

```
##   col.nr var.name
## 1    108       y1
## 2    375    y0101
## 3    465   y0101h
## 4    557   y0101c
## 5    563  y0101ch
##                                                                var.label
## 1 y1. Mes pasado, monto Sueldo o salario líquido en su trabajo principal
## 2                   Asalariados principal - Sueldos y salarios monetario
## 3                   Asalariados principal - Sueldos y salarios monetario
## 4         Asalariados principal - Sueldos y salarios monetario corregido
## 5         Asalariados principal - Sueldos y salarios monetario corregido
```

**Explorar distirbución de las variables**

```r
frq(datos$pobreza)
```

```
## Situación de pobreza por ingresos (x) <numeric> 
## # total N=185437 valid N=185339 mean=2.84 sd=0.48
## 
## Value |              Label |      N | Raw % | Valid % | Cum. %
## --------------------------------------------------------------
##     1 |    Pobres extremos |   8439 |  4.55 |    4.55 |   4.55
##     2 | Pobres no extremos |  12891 |  6.95 |    6.96 |  11.51
##     3 |          No pobres | 164009 | 88.44 |   88.49 | 100.00
##  <NA> |               <NA> |     98 |  0.05 |    <NA> |   <NA>
```


```r
frq(datos$y1) #¡Qué feo!
```


```
## [[1]]
##           val  label    frq raw.prc valid.prc cum.prc
## 1        1200 <none>      1    0.00      0.00    0.00
## 2        1800 <none>      1    0.00      0.00    0.00
## 3        2000 <none>      1    0.00      0.00    0.01
## 4        2222 <none>      1    0.00      0.00    0.01
## 5        6000 <none>      2    0.00      0.00    0.01
## 6        7000 <none>      1    0.00      0.00    0.02
## 7        8073 <none>      1    0.00      0.00    0.02
## 8       10000 <none>      6    0.00      0.01    0.03
## 9       12000 <none>      1    0.00      0.00    0.04
## 10      14000 <none>      1    0.00      0.00    0.04
## 11      15000 <none>     11    0.01      0.03    0.07
## 12      16000 <none>      2    0.00      0.00    0.07
## 13      17000 <none>      2    0.00      0.00    0.08
## 14      18000 <none>      2    0.00      0.00    0.08
## 15      20000 <none>     22    0.01      0.05    0.14
## 16      21800 <none>      1    0.00      0.00    0.14
## 17      22000 <none>      1    0.00      0.00    0.14
## 18      24000 <none>      3    0.00      0.01    0.15
## 19      25000 <none>      9    0.00      0.02    0.17
## 20      25300 <none>      1    0.00      0.00    0.17
## 21      26000 <none>      1    0.00      0.00    0.18
## 22      27000 <none>      1    0.00      0.00    0.18
## 23      27900 <none>      1    0.00      0.00    0.18
## 24      28000 <none>      3    0.00      0.01    0.19
## 25      29000 <none>      1    0.00      0.00    0.19
## 26      30000 <none>     47    0.03      0.12    0.31
## 27      31000 <none>      1    0.00      0.00    0.31
## 28      32000 <none>      4    0.00      0.01    0.32
## 29      32050 <none>      1    0.00      0.00    0.32
## 30      32650 <none>      1    0.00      0.00    0.32
## 31      33000 <none>      2    0.00      0.00    0.33
## 32      34000 <none>      2    0.00      0.00    0.33
## 33      35000 <none>      8    0.00      0.02    0.35
## 34      36000 <none>      2    0.00      0.00    0.36
## 35      37000 <none>      1    0.00      0.00    0.36
## 36      38000 <none>      1    0.00      0.00    0.36
## 37      40000 <none>     54    0.03      0.13    0.50
## 38      41000 <none>      1    0.00      0.00    0.50
## 39      42000 <none>      3    0.00      0.01    0.51
## 40      43000 <none>      1    0.00      0.00    0.51
## 41      44000 <none>      1    0.00      0.00    0.51
## 42      44600 <none>      1    0.00      0.00    0.51
## 43      45000 <none>     17    0.01      0.04    0.56
## 44      46000 <none>      1    0.00      0.00    0.56
## 45      47000 <none>      1    0.00      0.00    0.56
## 46      48000 <none>      1    0.00      0.00    0.56
## 47      49000 <none>      1    0.00      0.00    0.57
## 48      50000 <none>    102    0.06      0.25    0.82
## 49      52000 <none>      1    0.00      0.00    0.82
## 50      53000 <none>      1    0.00      0.00    0.82
## 51      55000 <none>      3    0.00      0.01    0.83
## 52      56000 <none>      2    0.00      0.00    0.84
## 53      57000 <none>      2    0.00      0.00    0.84
## 54      58000 <none>      1    0.00      0.00    0.84
## 55      60000 <none>    110    0.06      0.27    1.12
## 56      62000 <none>      4    0.00      0.01    1.13
## 57      63000 <none>      1    0.00      0.00    1.13
## 58      64000 <none>      5    0.00      0.01    1.14
## 59      65000 <none>      6    0.00      0.01    1.16
## 60      68000 <none>      4    0.00      0.01    1.17
## 61      70000 <none>     57    0.03      0.14    1.31
## 62      70500 <none>      1    0.00      0.00    1.31
## 63      71000 <none>      2    0.00      0.00    1.31
## 64      72000 <none>      5    0.00      0.01    1.33
## 65      74000 <none>      2    0.00      0.00    1.33
## 66      75000 <none>     18    0.01      0.04    1.38
## 67      77000 <none>      2    0.00      0.00    1.38
## 68      78000 <none>      2    0.00      0.00    1.39
## 69      79000 <none>      1    0.00      0.00    1.39
## 70      80000 <none>    137    0.07      0.34    1.73
## 71      82000 <none>      3    0.00      0.01    1.73
## 72      83000 <none>      1    0.00      0.00    1.74
## 73      84000 <none>      2    0.00      0.00    1.74
## 74      85000 <none>      3    0.00      0.01    1.75
## 75      86000 <none>      2    0.00      0.00    1.75
## 76      87000 <none>      1    0.00      0.00    1.76
## 77      88000 <none>      1    0.00      0.00    1.76
## 78      89000 <none>      1    0.00      0.00    1.76
## 79      90000 <none>     47    0.03      0.12    1.88
## 80      92000 <none>      2    0.00      0.00    1.88
## 81      95000 <none>      6    0.00      0.01    1.90
## 82      96000 <none>      6    0.00      0.01    1.91
## 83      97000 <none>      1    0.00      0.00    1.91
## 84      97500 <none>      2    0.00      0.00    1.92
## 85      98000 <none>      6    0.00      0.01    1.93
## 86     100000 <none>    328    0.18      0.81    2.75
## 87     100003 <none>      1    0.00      0.00    2.75
## 88     100008 <none>      1    0.00      0.00    2.75
## 89     101000 <none>      1    0.00      0.00    2.75
## 90     104000 <none>      3    0.00      0.01    2.76
## 91     105000 <none>      7    0.00      0.02    2.78
## 92     106000 <none>      2    0.00      0.00    2.78
## 93     107000 <none>      2    0.00      0.00    2.79
## 94     108000 <none>      1    0.00      0.00    2.79
## 95     109000 <none>      2    0.00      0.00    2.80
## 96     110000 <none>     38    0.02      0.09    2.89
## 97     111000 <none>      1    0.00      0.00    2.89
## 98     112000 <none>      2    0.00      0.00    2.90
## 99     112500 <none>      1    0.00      0.00    2.90
## 100    113000 <none>      3    0.00      0.01    2.91
## 101    114000 <none>      1    0.00      0.00    2.91
## 102    115000 <none>      5    0.00      0.01    2.92
## 103    116000 <none>      2    0.00      0.00    2.93
## 104    117000 <none>      1    0.00      0.00    2.93
## 105    118000 <none>      2    0.00      0.00    2.93
## 106    118800 <none>      1    0.00      0.00    2.94
## 107    120000 <none>    237    0.13      0.59    3.52
## 108    120300 <none>      1    0.00      0.00    3.53
## 109    121000 <none>      1    0.00      0.00    3.53
## 110    122000 <none>      1    0.00      0.00    3.53
## 111    123000 <none>      3    0.00      0.01    3.54
## 112    124000 <none>      2    0.00      0.00    3.54
## 113    125000 <none>     11    0.01      0.03    3.57
## 114    126000 <none>      2    0.00      0.00    3.58
## 115    127000 <none>      1    0.00      0.00    3.58
## 116    128000 <none>      6    0.00      0.01    3.59
## 117    129000 <none>      1    0.00      0.00    3.59
## 118    130000 <none>    100    0.05      0.25    3.84
## 119    131000 <none>      1    0.00      0.00    3.84
## 120    132000 <none>      7    0.00      0.02    3.86
## 121    133000 <none>      4    0.00      0.01    3.87
## 122    134000 <none>      3    0.00      0.01    3.88
## 123    135000 <none>     10    0.01      0.02    3.90
## 124    136000 <none>      1    0.00      0.00    3.91
## 125    140000 <none>     63    0.03      0.16    4.06
## 126    141000 <none>      1    0.00      0.00    4.07
## 127    141681 <none>      1    0.00      0.00    4.07
## 128    142000 <none>      3    0.00      0.01    4.07
## 129    143000 <none>      1    0.00      0.00    4.08
## 130    144000 <none>      5    0.00      0.01    4.09
## 131    144444 <none>      1    0.00      0.00    4.09
## 132    145000 <none>      7    0.00      0.02    4.11
## 133    146000 <none>      5    0.00      0.01    4.12
## 134    147000 <none>      2    0.00      0.00    4.13
## 135    148000 <none>      1    0.00      0.00    4.13
## 136    150000 <none>    416    0.22      1.03    5.16
## 137    150090 <none>      1    0.00      0.00    5.16
## 138    151000 <none>      2    0.00      0.00    5.17
## 139    152000 <none>      2    0.00      0.00    5.17
## 140    153000 <none>      1    0.00      0.00    5.17
## 141    154000 <none>      1    0.00      0.00    5.18
## 142    155000 <none>      1    0.00      0.00    5.18
## 143    156000 <none>      5    0.00      0.01    5.19
## 144    157000 <none>      2    0.00      0.00    5.20
## 145    157388 <none>      1    0.00      0.00    5.20
## 146    158000 <none>      2    0.00      0.00    5.20
## 147    159000 <none>      1    0.00      0.00    5.21
## 148    159479 <none>      1    0.00      0.00    5.21
## 149    160000 <none>    124    0.07      0.31    5.51
## 150    160250 <none>      1    0.00      0.00    5.52
## 151    161000 <none>      1    0.00      0.00    5.52
## 152    162000 <none>      2    0.00      0.00    5.52
## 153    163000 <none>      2    0.00      0.00    5.53
## 154    164000 <none>      3    0.00      0.01    5.54
## 155    165000 <none>     16    0.01      0.04    5.58
## 156    165800 <none>      1    0.00      0.00    5.58
## 157    166556 <none>      1    0.00      0.00    5.58
## 158    167000 <none>      3    0.00      0.01    5.59
## 159    168000 <none>      3    0.00      0.01    5.60
## 160    169999 <none>      1    0.00      0.00    5.60
## 161    170000 <none>     66    0.04      0.16    5.76
## 162    172000 <none>      4    0.00      0.01    5.77
## 163    173000 <none>      5    0.00      0.01    5.78
## 164    174000 <none>      4    0.00      0.01    5.79
## 165    175000 <none>     13    0.01      0.03    5.83
## 166    176000 <none>      3    0.00      0.01    5.83
## 167    177000 <none>      1    0.00      0.00    5.84
## 168    178000 <none>      6    0.00      0.01    5.85
## 169    179000 <none>      2    0.00      0.00    5.86
## 170    180000 <none>    246    0.13      0.61    6.46
## 171    182000 <none>      7    0.00      0.02    6.48
## 172    183000 <none>      2    0.00      0.00    6.49
## 173    184000 <none>      3    0.00      0.01    6.49
## 174    185000 <none>      4    0.00      0.01    6.50
## 175    186000 <none>      4    0.00      0.01    6.51
## 176    187000 <none>      2    0.00      0.00    6.52
## 177    188000 <none>      3    0.00      0.01    6.53
## 178    189000 <none>      3    0.00      0.01    6.53
## 179    190000 <none>     57    0.03      0.14    6.68
## 180    191000 <none>      1    0.00      0.00    6.68
## 181    192000 <none>      5    0.00      0.01    6.69
## 182    193000 <none>      1    0.00      0.00    6.69
## 183    194000 <none>      3    0.00      0.01    6.70
## 184    195000 <none>      4    0.00      0.01    6.71
## 185    196000 <none>      7    0.00      0.02    6.73
## 186    196736 <none>      1    0.00      0.00    6.73
## 187    197000 <none>      2    0.00      0.00    6.73
## 188    198000 <none>      3    0.00      0.01    6.74
## 189    200000 <none>    854    0.46      2.11    8.85
## 190    200002 <none>      1    0.00      0.00    8.86
## 191    201000 <none>      1    0.00      0.00    8.86
## 192    202000 <none>      1    0.00      0.00    8.86
## 193    203156 <none>      1    0.00      0.00    8.86
## 194    204000 <none>      3    0.00      0.01    8.87
## 195    204800 <none>      1    0.00      0.00    8.87
## 196    205000 <none>      9    0.00      0.02    8.90
## 197    206000 <none>      1    0.00      0.00    8.90
## 198    208000 <none>      3    0.00      0.01    8.91
## 199    209000 <none>      3    0.00      0.01    8.91
## 200    210000 <none>     62    0.03      0.15    9.07
## 201    211000 <none>      4    0.00      0.01    9.08
## 202    212000 <none>      4    0.00      0.01    9.09
## 203    213000 <none>      7    0.00      0.02    9.10
## 204    214000 <none>      5    0.00      0.01    9.12
## 205    215000 <none>     11    0.01      0.03    9.14
## 206    216000 <none>      7    0.00      0.02    9.16
## 207    217000 <none>      4    0.00      0.01    9.17
## 208    218000 <none>      6    0.00      0.01    9.19
## 209    219000 <none>      5    0.00      0.01    9.20
## 210    220000 <none>    174    0.09      0.43    9.63
## 211    221000 <none>      1    0.00      0.00    9.63
## 212    222000 <none>      1    0.00      0.00    9.63
## 213    222400 <none>      1    0.00      0.00    9.64
## 214    223000 <none>      2    0.00      0.00    9.64
## 215    224000 <none>      4    0.00      0.01    9.65
## 216    225000 <none>     50    0.03      0.12    9.78
## 217    226000 <none>      6    0.00      0.01    9.79
## 218    227000 <none>      3    0.00      0.01    9.80
## 219    228000 <none>      2    0.00      0.00    9.80
## 220    229000 <none>      1    0.00      0.00    9.81
## 221    230000 <none>    121    0.07      0.30   10.10
## 222    230500 <none>      1    0.00      0.00   10.11
## 223    231000 <none>      1    0.00      0.00   10.11
## 224    233000 <none>      1    0.00      0.00   10.11
## 225    234000 <none>      2    0.00      0.00   10.12
## 226    235000 <none>      7    0.00      0.02   10.13
## 227    236000 <none>      5    0.00      0.01   10.15
## 228    236858 <none>      1    0.00      0.00   10.15
## 229    237000 <none>      1    0.00      0.00   10.15
## 230    238000 <none>      2    0.00      0.00   10.16
## 231    240000 <none>    168    0.09      0.42   10.57
## 232    241000 <none>      1    0.00      0.00   10.57
## 233    242000 <none>      3    0.00      0.01   10.58
## 234    245000 <none>     14    0.01      0.03   10.62
## 235    246000 <none>      2    0.00      0.00   10.62
## 236    247000 <none>      3    0.00      0.01   10.63
## 237    248000 <none>      7    0.00      0.02   10.65
## 238    249000 <none>      3    0.00      0.01   10.65
## 239    250000 <none>    655    0.35      1.62   12.27
## 240    251000 <none>      2    0.00      0.00   12.28
## 241    252000 <none>      7    0.00      0.02   12.30
## 242    253000 <none>      3    0.00      0.01   12.30
## 243    254000 <none>      4    0.00      0.01   12.31
## 244    255000 <none>      8    0.00      0.02   12.33
## 245    255293 <none>      1    0.00      0.00   12.34
## 246    255760 <none>      1    0.00      0.00   12.34
## 247    256000 <none>      7    0.00      0.02   12.36
## 248    257000 <none>      3    0.00      0.01   12.36
## 249    258000 <none>      3    0.00      0.01   12.37
## 250    259000 <none>      5    0.00      0.01   12.38
## 251    260000 <none>    190    0.10      0.47   12.85
## 252    261000 <none>      5    0.00      0.01   12.87
## 253    261200 <none>      1    0.00      0.00   12.87
## 254    262000 <none>      2    0.00      0.00   12.87
## 255    263000 <none>      6    0.00      0.01   12.89
## 256    263681 <none>      1    0.00      0.00   12.89
## 257    264000 <none>      8    0.00      0.02   12.91
## 258    265000 <none>      8    0.00      0.02   12.93
## 259    266000 <none>      4    0.00      0.01   12.94
## 260    267000 <none>      1    0.00      0.00   12.94
## 261    268000 <none>      1    0.00      0.00   12.94
## 262    269000 <none>      1    0.00      0.00   12.95
## 263    270000 <none>    179    0.10      0.44   13.39
## 264    271000 <none>      2    0.00      0.00   13.40
## 265    272000 <none>      3    0.00      0.01   13.40
## 266    273000 <none>      4    0.00      0.01   13.41
## 267    274000 <none>      3    0.00      0.01   13.42
## 268    275000 <none>     12    0.01      0.03   13.45
## 269    276000 <none>      5    0.00      0.01   13.46
## 270    277000 <none>      7    0.00      0.02   13.48
## 271    278000 <none>      3    0.00      0.01   13.49
## 272    279000 <none>      1    0.00      0.00   13.49
## 273    280000 <none>    457    0.25      1.13   14.62
## 274    280088 <none>      1    0.00      0.00   14.62
## 275    281000 <none>      2    0.00      0.00   14.63
## 276    282000 <none>      5    0.00      0.01   14.64
## 277    283000 <none>      2    0.00      0.00   14.64
## 278    283883 <none>      1    0.00      0.00   14.65
## 279    284000 <none>      3    0.00      0.01   14.65
## 280    285000 <none>     20    0.01      0.05   14.70
## 281    286000 <none>      5    0.00      0.01   14.72
## 282    287000 <none>      3    0.00      0.01   14.72
## 283    288000 <none>      4    0.00      0.01   14.73
## 284    289000 <none>      3    0.00      0.01   14.74
## 285    289460 <none>      1    0.00      0.00   14.74
## 286    289737 <none>      1    0.00      0.00   14.75
## 287    290000 <none>    137    0.07      0.34   15.08
## 288    290500 <none>      1    0.00      0.00   15.09
## 289    291000 <none>      3    0.00      0.01   15.09
## 290    292000 <none>      5    0.00      0.01   15.11
## 291    293000 <none>      2    0.00      0.00   15.11
## 292    293790 <none>      1    0.00      0.00   15.11
## 293    294000 <none>      4    0.00      0.01   15.12
## 294    295000 <none>     14    0.01      0.03   15.16
## 295    296000 <none>      5    0.00      0.01   15.17
## 296    297000 <none>      4    0.00      0.01   15.18
## 297    298000 <none>     12    0.01      0.03   15.21
## 298    299000 <none>      1    0.00      0.00   15.21
## 299    300000 <none>   2304    1.24      5.70   20.91
## 300    300002 <none>      1    0.00      0.00   20.92
## 301    300003 <none>      1    0.00      0.00   20.92
## 302    300008 <none>      1    0.00      0.00   20.92
## 303    300500 <none>      1    0.00      0.00   20.92
## 304    301000 <none>    284    0.15      0.70   21.63
## 305    302000 <none>     15    0.01      0.04   21.66
## 306    303000 <none>      4    0.00      0.01   21.67
## 307    304000 <none>      6    0.00      0.01   21.69
## 308    305000 <none>     37    0.02      0.09   21.78
## 309    306000 <none>     25    0.01      0.06   21.84
## 310    307000 <none>      8    0.00      0.02   21.86
## 311    308000 <none>     12    0.01      0.03   21.89
## 312    308300 <none>      1    0.00      0.00   21.89
## 313    309000 <none>     10    0.01      0.02   21.92
## 314    310000 <none>    341    0.18      0.84   22.76
## 315    310504 <none>      1    0.00      0.00   22.76
## 316    311000 <none>      6    0.00      0.01   22.78
## 317    311100 <none>      1    0.00      0.00   22.78
## 318    312000 <none>     16    0.01      0.04   22.82
## 319    312500 <none>      1    0.00      0.00   22.82
## 320    313000 <none>      3    0.00      0.01   22.83
## 321    314000 <none>     13    0.01      0.03   22.86
## 322    315000 <none>     60    0.03      0.15   23.01
## 323    315692 <none>      1    0.00      0.00   23.01
## 324    316000 <none>     15    0.01      0.04   23.05
## 325    317000 <none>      6    0.00      0.01   23.07
## 326    318000 <none>      9    0.00      0.02   23.09
## 327    318027 <none>      1    0.00      0.00   23.09
## 328    318572 <none>      1    0.00      0.00   23.09
## 329    319000 <none>     61    0.03      0.15   23.24
## 330    320000 <none>   2873    1.55      7.11   30.35
## 331    320031 <none>      1    0.00      0.00   30.36
## 332    320500 <none>     60    0.03      0.15   30.50
## 333    321000 <none>     20    0.01      0.05   30.55
## 334    322000 <none>     32    0.02      0.08   30.63
## 335    323000 <none>      7    0.00      0.02   30.65
## 336    323500 <none>      1    0.00      0.00   30.65
## 337    324000 <none>     12    0.01      0.03   30.68
## 338    324345 <none>      1    0.00      0.00   30.68
## 339    325000 <none>    192    0.10      0.48   31.16
## 340    325090 <none>      1    0.00      0.00   31.16
## 341    326000 <none>    248    0.13      0.61   31.78
## 342    326500 <none>    107    0.06      0.26   32.04
## 343    327000 <none>      8    0.00      0.02   32.06
## 344    328000 <none>      5    0.00      0.01   32.07
## 345    328888 <none>      1    0.00      0.00   32.07
## 346    329000 <none>      6    0.00      0.01   32.09
## 347    329500 <none>      1    0.00      0.00   32.09
## 348    330000 <none>    306    0.17      0.76   32.85
## 349    331000 <none>      7    0.00      0.02   32.87
## 350    332000 <none>      6    0.00      0.01   32.88
## 351    332636 <none>      1    0.00      0.00   32.88
## 352    332703 <none>      1    0.00      0.00   32.89
## 353    333000 <none>      7    0.00      0.02   32.90
## 354    334000 <none>      5    0.00      0.01   32.92
## 355    335000 <none>     20    0.01      0.05   32.97
## 356    336000 <none>     10    0.01      0.02   32.99
## 357    336500 <none>      1    0.00      0.00   32.99
## 358    337000 <none>      3    0.00      0.01   33.00
## 359    338000 <none>     12    0.01      0.03   33.03
## 360    338500 <none>      1    0.00      0.00   33.03
## 361    339000 <none>      1    0.00      0.00   33.03
## 362    340000 <none>    270    0.15      0.67   33.70
## 363    341000 <none>      1    0.00      0.00   33.71
## 364    342000 <none>      6    0.00      0.01   33.72
## 365    343000 <none>      8    0.00      0.02   33.74
## 366    344000 <none>      4    0.00      0.01   33.75
## 367    345000 <none>     19    0.01      0.05   33.80
## 368    346000 <none>      5    0.00      0.01   33.81
## 369    347000 <none>      3    0.00      0.01   33.82
## 370    347336 <none>      1    0.00      0.00   33.82
## 371    348000 <none>      5    0.00      0.01   33.83
## 372    349000 <none>      4    0.00      0.01   33.84
## 373    349600 <none>      1    0.00      0.00   33.84
## 374    350000 <none>   2104    1.13      5.21   39.05
## 375    351000 <none>      4    0.00      0.01   39.06
## 376    352000 <none>      4    0.00      0.01   39.07
## 377    353320 <none>      1    0.00      0.00   39.07
## 378    354000 <none>      6    0.00      0.01   39.09
## 379    355000 <none>     10    0.01      0.02   39.11
## 380    356000 <none>     16    0.01      0.04   39.15
## 381    356947 <none>      1    0.00      0.00   39.15
## 382    357000 <none>      5    0.00      0.01   39.17
## 383    358000 <none>     11    0.01      0.03   39.19
## 384    359000 <none>      7    0.00      0.02   39.21
## 385    360000 <none>    327    0.18      0.81   40.02
## 386    361000 <none>      3    0.00      0.01   40.03
## 387    361350 <none>      1    0.00      0.00   40.03
## 388    362000 <none>      3    0.00      0.01   40.04
## 389    363000 <none>      2    0.00      0.00   40.04
## 390    364000 <none>      2    0.00      0.00   40.05
## 391    365000 <none>     23    0.01      0.06   40.10
## 392    365500 <none>      1    0.00      0.00   40.11
## 393    367000 <none>      6    0.00      0.01   40.12
## 394    367200 <none>      1    0.00      0.00   40.12
## 395    368000 <none>      6    0.00      0.01   40.14
## 396    369000 <none>      2    0.00      0.00   40.14
## 397    369364 <none>      1    0.00      0.00   40.15
## 398    369975 <none>      1    0.00      0.00   40.15
## 399    370000 <none>    212    0.11      0.52   40.67
## 400    371000 <none>      7    0.00      0.02   40.69
## 401    372000 <none>      5    0.00      0.01   40.70
## 402    373000 <none>      1    0.00      0.00   40.70
## 403    374000 <none>      3    0.00      0.01   40.71
## 404    375000 <none>     20    0.01      0.05   40.76
## 405    376000 <none>      3    0.00      0.01   40.77
## 406    377000 <none>      3    0.00      0.01   40.78
## 407    378000 <none>      9    0.00      0.02   40.80
## 408    379000 <none>      7    0.00      0.02   40.82
## 409    379663 <none>      1    0.00      0.00   40.82
## 410    380000 <none>    692    0.37      1.71   42.53
## 411    381000 <none>      2    0.00      0.00   42.54
## 412    381649 <none>      1    0.00      0.00   42.54
## 413    382000 <none>      3    0.00      0.01   42.55
## 414    383000 <none>      4    0.00      0.01   42.56
## 415    384000 <none>      6    0.00      0.01   42.57
## 416    385000 <none>     25    0.01      0.06   42.63
## 417    386000 <none>      6    0.00      0.01   42.65
## 418    387000 <none>      7    0.00      0.02   42.66
## 419    388000 <none>      7    0.00      0.02   42.68
## 420    389000 <none>      4    0.00      0.01   42.69
## 421    390000 <none>    160    0.09      0.40   43.09
## 422    391000 <none>      1    0.00      0.00   43.09
## 423    392000 <none>      3    0.00      0.01   43.10
## 424    393000 <none>      1    0.00      0.00   43.10
## 425    394000 <none>      4    0.00      0.01   43.11
## 426    395000 <none>     14    0.01      0.03   43.14
## 427    395850 <none>      1    0.00      0.00   43.15
## 428    396000 <none>      3    0.00      0.01   43.15
## 429    397000 <none>      1    0.00      0.00   43.16
## 430    398000 <none>      8    0.00      0.02   43.18
## 431    399000 <none>      6    0.00      0.01   43.19
## 432    400000 <none>   2774    1.50      6.86   50.05
## 433    400005 <none>      1    0.00      0.00   50.06
## 434    400600 <none>      1    0.00      0.00   50.06
## 435    401000 <none>      3    0.00      0.01   50.07
## 436    401500 <none>      1    0.00      0.00   50.07
## 437    402000 <none>      5    0.00      0.01   50.08
## 438    402810 <none>      1    0.00      0.00   50.08
## 439    403000 <none>      2    0.00      0.00   50.09
## 440    403800 <none>      1    0.00      0.00   50.09
## 441    403838 <none>      1    0.00      0.00   50.09
## 442    404000 <none>      1    0.00      0.00   50.10
## 443    405000 <none>      9    0.00      0.02   50.12
## 444    406000 <none>      3    0.00      0.01   50.13
## 445    407000 <none>      2    0.00      0.00   50.13
## 446    408000 <none>      2    0.00      0.00   50.14
## 447    409000 <none>      5    0.00      0.01   50.15
## 448    410000 <none>     78    0.04      0.19   50.34
## 449    412000 <none>      8    0.00      0.02   50.36
## 450    413000 <none>      4    0.00      0.01   50.37
## 451    413971 <none>      1    0.00      0.00   50.37
## 452    414000 <none>      2    0.00      0.00   50.38
## 453    415000 <none>     13    0.01      0.03   50.41
## 454    416000 <none>      5    0.00      0.01   50.42
## 455    416365 <none>      1    0.00      0.00   50.43
## 456    417000 <none>      7    0.00      0.02   50.44
## 457    418000 <none>      7    0.00      0.02   50.46
## 458    419000 <none>      7    0.00      0.02   50.48
## 459    420000 <none>    324    0.17      0.80   51.28
## 460    421000 <none>      1    0.00      0.00   51.28
## 461    422000 <none>      3    0.00      0.01   51.29
## 462    423000 <none>      5    0.00      0.01   51.30
## 463    424000 <none>      2    0.00      0.00   51.31
## 464    425000 <none>     22    0.01      0.05   51.36
## 465    426000 <none>      2    0.00      0.00   51.37
## 466    427000 <none>      7    0.00      0.02   51.38
## 467    428000 <none>      2    0.00      0.00   51.39
## 468    429000 <none>      2    0.00      0.00   51.39
## 469    430000 <none>    172    0.09      0.43   51.82
## 470    432000 <none>      6    0.00      0.01   51.83
## 471    433000 <none>      3    0.00      0.01   51.84
## 472    434000 <none>      2    0.00      0.00   51.85
## 473    435000 <none>     14    0.01      0.03   51.88
## 474    436000 <none>      2    0.00      0.00   51.89
## 475    438000 <none>      3    0.00      0.01   51.89
## 476    439000 <none>      4    0.00      0.01   51.90
## 477    440000 <none>     53    0.03      0.13   52.03
## 478    440009 <none>      1    0.00      0.00   52.04
## 479    440215 <none>      1    0.00      0.00   52.04
## 480    442000 <none>      2    0.00      0.00   52.04
## 481    443000 <none>      1    0.00      0.00   52.05
## 482    444000 <none>      3    0.00      0.01   52.05
## 483    445000 <none>      9    0.00      0.02   52.08
## 484    445800 <none>      1    0.00      0.00   52.08
## 485    446000 <none>      5    0.00      0.01   52.09
## 486    447000 <none>      4    0.00      0.01   52.10
## 487    448000 <none>      4    0.00      0.01   52.11
## 488    449000 <none>      2    0.00      0.00   52.12
## 489    449550 <none>      1    0.00      0.00   52.12
## 490    450000 <none>   1287    0.69      3.18   55.30
## 491    452000 <none>      2    0.00      0.00   55.31
## 492    453000 <none>      3    0.00      0.01   55.31
## 493    454000 <none>      2    0.00      0.00   55.32
## 494    455000 <none>      3    0.00      0.01   55.33
## 495    456000 <none>      6    0.00      0.01   55.34
## 496    457000 <none>      2    0.00      0.00   55.35
## 497    458000 <none>      6    0.00      0.01   55.36
## 498    459000 <none>      2    0.00      0.00   55.37
## 499    460000 <none>     96    0.05      0.24   55.60
## 500    460585 <none>      1    0.00      0.00   55.61
## 501    461000 <none>      2    0.00      0.00   55.61
## 502    462000 <none>      4    0.00      0.01   55.62
## 503    462500 <none>      1    0.00      0.00   55.62
## 504    463000 <none>      1    0.00      0.00   55.63
## 505    464000 <none>      1    0.00      0.00   55.63
## 506    465000 <none>      9    0.00      0.02   55.65
## 507    465312 <none>      1    0.00      0.00   55.65
## 508    466000 <none>      1    0.00      0.00   55.66
## 509    468000 <none>      1    0.00      0.00   55.66
## 510    470000 <none>     94    0.05      0.23   55.89
## 511    472000 <none>      1    0.00      0.00   55.89
## 512    473000 <none>      2    0.00      0.00   55.90
## 513    474000 <none>      1    0.00      0.00   55.90
## 514    475000 <none>     14    0.01      0.03   55.94
## 515    476000 <none>      2    0.00      0.00   55.94
## 516    477000 <none>      3    0.00      0.01   55.95
## 517    478000 <none>      2    0.00      0.00   55.95
## 518    479000 <none>      3    0.00      0.01   55.96
## 519    480000 <none>    288    0.16      0.71   56.67
## 520    482000 <none>      3    0.00      0.01   56.68
## 521    483000 <none>      1    0.00      0.00   56.68
## 522    484000 <none>      2    0.00      0.00   56.69
## 523    485000 <none>      7    0.00      0.02   56.70
## 524    487000 <none>      4    0.00      0.01   56.71
## 525    488000 <none>      2    0.00      0.00   56.72
## 526    489000 <none>      2    0.00      0.00   56.72
## 527    490000 <none>     68    0.04      0.17   56.89
## 528    491000 <none>      3    0.00      0.01   56.90
## 529    495000 <none>     10    0.01      0.02   56.93
## 530    495200 <none>      1    0.00      0.00   56.93
## 531    496000 <none>      5    0.00      0.01   56.94
## 532    497000 <none>      2    0.00      0.00   56.94
## 533    498000 <none>      4    0.00      0.01   56.95
## 534    499318 <none>      1    0.00      0.00   56.96
## 535    500000 <none>   2213    1.19      5.48   62.43
## 536    500002 <none>      1    0.00      0.00   62.44
## 537    500003 <none>      1    0.00      0.00   62.44
## 538    500005 <none>      1    0.00      0.00   62.44
## 539    501000 <none>      3    0.00      0.01   62.45
## 540    502000 <none>      1    0.00      0.00   62.45
## 541    503000 <none>      1    0.00      0.00   62.45
## 542    503640 <none>      1    0.00      0.00   62.45
## 543    505000 <none>      6    0.00      0.01   62.47
## 544    505189 <none>      1    0.00      0.00   62.47
## 545    506000 <none>      4    0.00      0.01   62.48
## 546    507000 <none>      2    0.00      0.00   62.49
## 547    508000 <none>      3    0.00      0.01   62.49
## 548    509000 <none>      2    0.00      0.00   62.50
## 549    510000 <none>     22    0.01      0.05   62.55
## 550    511000 <none>      2    0.00      0.00   62.56
## 551    512000 <none>      4    0.00      0.01   62.57
## 552    513000 <none>      4    0.00      0.01   62.58
## 553    513700 <none>      1    0.00      0.00   62.58
## 554    515000 <none>      7    0.00      0.02   62.60
## 555    516000 <none>      4    0.00      0.01   62.61
## 556    517000 <none>      5    0.00      0.01   62.62
## 557    518000 <none>      5    0.00      0.01   62.63
## 558    519000 <none>      2    0.00      0.00   62.64
## 559    520000 <none>    117    0.06      0.29   62.93
## 560    522000 <none>      2    0.00      0.00   62.93
## 561    522002 <none>      1    0.00      0.00   62.93
## 562    523000 <none>      2    0.00      0.00   62.94
## 563    524000 <none>      1    0.00      0.00   62.94
## 564    525000 <none>      8    0.00      0.02   62.96
## 565    526000 <none>      3    0.00      0.01   62.97
## 566    527000 <none>      3    0.00      0.01   62.98
## 567    528000 <none>      4    0.00      0.01   62.99
## 568    529000 <none>      1    0.00      0.00   62.99
## 569    530000 <none>     74    0.04      0.18   63.17
## 570    531000 <none>      1    0.00      0.00   63.17
## 571    532000 <none>      1    0.00      0.00   63.18
## 572    533000 <none>      1    0.00      0.00   63.18
## 573    533583 <none>      1    0.00      0.00   63.18
## 574    534000 <none>      1    0.00      0.00   63.18
## 575    535000 <none>     11    0.01      0.03   63.21
## 576    536000 <none>      4    0.00      0.01   63.22
## 577    537000 <none>      3    0.00      0.01   63.23
## 578    538000 <none>      5    0.00      0.01   63.24
## 579    540000 <none>     66    0.04      0.16   63.40
## 580    541000 <none>      1    0.00      0.00   63.41
## 581    543000 <none>      1    0.00      0.00   63.41
## 582    545000 <none>     10    0.01      0.02   63.43
## 583    546000 <none>      1    0.00      0.00   63.44
## 584    547000 <none>      5    0.00      0.01   63.45
## 585    548000 <none>      2    0.00      0.00   63.45
## 586    550000 <none>    483    0.26      1.20   64.65
## 587    551000 <none>      2    0.00      0.00   64.65
## 588    552000 <none>      3    0.00      0.01   64.66
## 589    554000 <none>      2    0.00      0.00   64.67
## 590    556000 <none>      4    0.00      0.01   64.68
## 591    557000 <none>      2    0.00      0.00   64.68
## 592    558000 <none>      3    0.00      0.01   64.69
## 593    560000 <none>     54    0.03      0.13   64.82
## 594    561000 <none>      1    0.00      0.00   64.83
## 595    563000 <none>      1    0.00      0.00   64.83
## 596    564000 <none>      1    0.00      0.00   64.83
## 597    565000 <none>      4    0.00      0.01   64.84
## 598    568000 <none>      1    0.00      0.00   64.84
## 599    569000 <none>      1    0.00      0.00   64.84
## 600    570000 <none>     41    0.02      0.10   64.95
## 601    572000 <none>      1    0.00      0.00   64.95
## 602    574000 <none>      1    0.00      0.00   64.95
## 603    575000 <none>      6    0.00      0.01   64.97
## 604    575760 <none>      1    0.00      0.00   64.97
## 605    575900 <none>      1    0.00      0.00   64.97
## 606    576000 <none>      2    0.00      0.00   64.98
## 607    577000 <none>      1    0.00      0.00   64.98
## 608    578000 <none>      1    0.00      0.00   64.98
## 609    579000 <none>      1    0.00      0.00   64.98
## 610    580000 <none>     98    0.05      0.24   65.23
## 611    581000 <none>      1    0.00      0.00   65.23
## 612    581866 <none>      1    0.00      0.00   65.23
## 613    582000 <none>      1    0.00      0.00   65.23
## 614    583000 <none>      1    0.00      0.00   65.24
## 615    584000 <none>      2    0.00      0.00   65.24
## 616    584600 <none>      1    0.00      0.00   65.24
## 617    585000 <none>      2    0.00      0.00   65.25
## 618    586000 <none>      4    0.00      0.01   65.26
## 619    587000 <none>      1    0.00      0.00   65.26
## 620    588000 <none>      1    0.00      0.00   65.26
## 621    589000 <none>      4    0.00      0.01   65.27
## 622    590000 <none>     30    0.02      0.07   65.35
## 623    592000 <none>      3    0.00      0.01   65.35
## 624    594576 <none>      1    0.00      0.00   65.36
## 625    595000 <none>      2    0.00      0.00   65.36
## 626    598000 <none>      2    0.00      0.00   65.37
## 627    599000 <none>      2    0.00      0.00   65.37
## 628    600000 <none>   1585    0.85      3.92   69.29
## 629    600005 <none>      1    0.00      0.00   69.30
## 630    600048 <none>      1    0.00      0.00   69.30
## 631    601000 <none>      1    0.00      0.00   69.30
## 632    603000 <none>      1    0.00      0.00   69.30
## 633    604000 <none>      1    0.00      0.00   69.31
## 634    606000 <none>      1    0.00      0.00   69.31
## 635    607000 <none>      1    0.00      0.00   69.31
## 636    608000 <none>      2    0.00      0.00   69.32
## 637    610000 <none>     18    0.01      0.04   69.36
## 638    612000 <none>      3    0.00      0.01   69.37
## 639    613000 <none>      3    0.00      0.01   69.38
## 640    614000 <none>      2    0.00      0.00   69.38
## 641    615000 <none>      4    0.00      0.01   69.39
## 642    616000 <none>      1    0.00      0.00   69.39
## 643    618000 <none>      3    0.00      0.01   69.40
## 644    619000 <none>      1    0.00      0.00   69.40
## 645    620000 <none>     69    0.04      0.17   69.57
## 646    623000 <none>      1    0.00      0.00   69.58
## 647    624000 <none>      3    0.00      0.01   69.58
## 648    625000 <none>      2    0.00      0.00   69.59
## 649    626000 <none>      1    0.00      0.00   69.59
## 650    628000 <none>      1    0.00      0.00   69.59
## 651    630000 <none>     38    0.02      0.09   69.69
## 652    631000 <none>      1    0.00      0.00   69.69
## 653    632000 <none>      1    0.00      0.00   69.69
## 654    634000 <none>      1    0.00      0.00   69.69
## 655    634459 <none>      1    0.00      0.00   69.70
## 656    635000 <none>      1    0.00      0.00   69.70
## 657    637000 <none>      1    0.00      0.00   69.70
## 658    638000 <none>      1    0.00      0.00   69.70
## 659    640000 <none>     31    0.02      0.08   69.78
## 660    643000 <none>      1    0.00      0.00   69.78
## 661    645000 <none>      2    0.00      0.00   69.79
## 662    647000 <none>      1    0.00      0.00   69.79
## 663    649000 <none>      1    0.00      0.00   69.79
## 664    650000 <none>    459    0.25      1.14   70.93
## 665    657000 <none>      1    0.00      0.00   70.93
## 666    658000 <none>      1    0.00      0.00   70.93
## 667    659000 <none>      1    0.00      0.00   70.94
## 668    660000 <none>     23    0.01      0.06   70.99
## 669    664761 <none>      1    0.00      0.00   71.00
## 670    665000 <none>      2    0.00      0.00   71.00
## 671    666000 <none>      1    0.00      0.00   71.00
## 672    666666 <none>      1    0.00      0.00   71.01
## 673    667000 <none>      1    0.00      0.00   71.01
## 674    668000 <none>      2    0.00      0.00   71.01
## 675    668800 <none>      1    0.00      0.00   71.02
## 676    669000 <none>      2    0.00      0.00   71.02
## 677    670000 <none>     42    0.02      0.10   71.12
## 678    671000 <none>      3    0.00      0.01   71.13
## 679    672000 <none>      2    0.00      0.00   71.14
## 680    672196 <none>      1    0.00      0.00   71.14
## 681    675000 <none>      2    0.00      0.00   71.14
## 682    676000 <none>      1    0.00      0.00   71.15
## 683    677000 <none>      1    0.00      0.00   71.15
## 684    679000 <none>      1    0.00      0.00   71.15
## 685    680000 <none>     71    0.04      0.18   71.33
## 686    680458 <none>      1    0.00      0.00   71.33
## 687    682000 <none>      1    0.00      0.00   71.33
## 688    683000 <none>      2    0.00      0.00   71.34
## 689    684000 <none>      1    0.00      0.00   71.34
## 690    686000 <none>      1    0.00      0.00   71.34
## 691    687000 <none>      1    0.00      0.00   71.34
## 692    688117 <none>      1    0.00      0.00   71.35
## 693    690000 <none>     28    0.02      0.07   71.42
## 694    691000 <none>      1    0.00      0.00   71.42
## 695    692000 <none>      1    0.00      0.00   71.42
## 696    693000 <none>      1    0.00      0.00   71.42
## 697    694000 <none>      1    0.00      0.00   71.43
## 698    694109 <none>      1    0.00      0.00   71.43
## 699    695000 <none>      1    0.00      0.00   71.43
## 700    697000 <none>      2    0.00      0.00   71.44
## 701    700000 <none>   1227    0.66      3.04   74.47
## 702    700050 <none>      1    0.00      0.00   74.47
## 703    702000 <none>      1    0.00      0.00   74.48
## 704    703000 <none>      1    0.00      0.00   74.48
## 705    708000 <none>      1    0.00      0.00   74.48
## 706    709000 <none>      1    0.00      0.00   74.48
## 707    710000 <none>      7    0.00      0.02   74.50
## 708    711000 <none>      1    0.00      0.00   74.50
## 709    713000 <none>      3    0.00      0.01   74.51
## 710    714000 <none>      1    0.00      0.00   74.51
## 711    715000 <none>      6    0.00      0.01   74.53
## 712    718000 <none>      1    0.00      0.00   74.53
## 713    718508 <none>      1    0.00      0.00   74.53
## 714    720000 <none>     76    0.04      0.19   74.72
## 715    721000 <none>      2    0.00      0.00   74.73
## 716    725000 <none>      4    0.00      0.01   74.74
## 717    730000 <none>     32    0.02      0.08   74.82
## 718    733000 <none>      1    0.00      0.00   74.82
## 719    734000 <none>      1    0.00      0.00   74.82
## 720    735000 <none>      2    0.00      0.00   74.83
## 721    737000 <none>      1    0.00      0.00   74.83
## 722    738000 <none>      2    0.00      0.00   74.83
## 723    739000 <none>      2    0.00      0.00   74.84
## 724    740000 <none>     22    0.01      0.05   74.89
## 725    742184 <none>      1    0.00      0.00   74.89
## 726    743000 <none>      1    0.00      0.00   74.90
## 727    743831 <none>      1    0.00      0.00   74.90
## 728    745000 <none>      2    0.00      0.00   74.90
## 729    746000 <none>      2    0.00      0.00   74.91
## 730    747000 <none>      1    0.00      0.00   74.91
## 731    748000 <none>      1    0.00      0.00   74.91
## 732    750000 <none>    350    0.19      0.87   75.78
## 733    752000 <none>      1    0.00      0.00   75.78
## 734    754000 <none>      1    0.00      0.00   75.79
## 735    756804 <none>      1    0.00      0.00   75.79
## 736    759000 <none>      1    0.00      0.00   75.79
## 737    760000 <none>     26    0.01      0.06   75.85
## 738    763000 <none>      2    0.00      0.00   75.86
## 739    768000 <none>      1    0.00      0.00   75.86
## 740    769000 <none>      2    0.00      0.00   75.87
## 741    770000 <none>     22    0.01      0.05   75.92
## 742    774000 <none>      1    0.00      0.00   75.92
## 743    775000 <none>      1    0.00      0.00   75.93
## 744    776000 <none>      2    0.00      0.00   75.93
## 745    780000 <none>     58    0.03      0.14   76.08
## 746    782032 <none>      1    0.00      0.00   76.08
## 747    785000 <none>      1    0.00      0.00   76.08
## 748    787000 <none>      1    0.00      0.00   76.08
## 749    788000 <none>      2    0.00      0.00   76.09
## 750    790000 <none>     24    0.01      0.06   76.15
## 751    795000 <none>      1    0.00      0.00   76.15
## 752    797000 <none>      1    0.00      0.00   76.15
## 753    798000 <none>      2    0.00      0.00   76.16
## 754    799000 <none>      1    0.00      0.00   76.16
## 755    800000 <none>   1250    0.67      3.09   79.25
## 756    803000 <none>      2    0.00      0.00   79.26
## 757    805000 <none>      2    0.00      0.00   79.26
## 758    808698 <none>      1    0.00      0.00   79.26
## 759    809000 <none>      1    0.00      0.00   79.27
## 760    810000 <none>      3    0.00      0.01   79.27
## 761    812000 <none>      1    0.00      0.00   79.28
## 762    814000 <none>      1    0.00      0.00   79.28
## 763    818000 <none>      1    0.00      0.00   79.28
## 764    820000 <none>     35    0.02      0.09   79.37
## 765    821000 <none>      1    0.00      0.00   79.37
## 766    822000 <none>      1    0.00      0.00   79.37
## 767    823000 <none>      2    0.00      0.00   79.38
## 768    824000 <none>      2    0.00      0.00   79.38
## 769    825000 <none>      3    0.00      0.01   79.39
## 770    827000 <none>      1    0.00      0.00   79.39
## 771    828000 <none>      1    0.00      0.00   79.40
## 772    829000 <none>      1    0.00      0.00   79.40
## 773    830000 <none>     34    0.02      0.08   79.48
## 774    832000 <none>      2    0.00      0.00   79.49
## 775    832405 <none>      1    0.00      0.00   79.49
## 776    834000 <none>      2    0.00      0.00   79.49
## 777    835000 <none>      1    0.00      0.00   79.50
## 778    838000 <none>      1    0.00      0.00   79.50
## 779    840000 <none>     13    0.01      0.03   79.53
## 780    845000 <none>      3    0.00      0.01   79.54
## 781    847000 <none>      1    0.00      0.00   79.54
## 782    850000 <none>    254    0.14      0.63   80.17
## 783    852000 <none>      1    0.00      0.00   80.17
## 784    853000 <none>      2    0.00      0.00   80.18
## 785    854000 <none>      1    0.00      0.00   80.18
## 786    858463 <none>      1    0.00      0.00   80.18
## 787    859000 <none>      3    0.00      0.01   80.19
## 788    860000 <none>     24    0.01      0.06   80.25
## 789    861000 <none>      1    0.00      0.00   80.25
## 790    863000 <none>      2    0.00      0.00   80.26
## 791    865000 <none>      1    0.00      0.00   80.26
## 792    869000 <none>      1    0.00      0.00   80.26
## 793    870000 <none>     15    0.01      0.04   80.30
## 794    871000 <none>      1    0.00      0.00   80.30
## 795    873000 <none>      1    0.00      0.00   80.30
## 796    874481 <none>      1    0.00      0.00   80.31
## 797    876000 <none>      1    0.00      0.00   80.31
## 798    879000 <none>      1    0.00      0.00   80.31
## 799    880000 <none>     15    0.01      0.04   80.35
## 800    884000 <none>      1    0.00      0.00   80.35
## 801    885000 <none>      3    0.00      0.01   80.36
## 802    886000 <none>      1    0.00      0.00   80.36
## 803    890000 <none>     28    0.02      0.07   80.43
## 804    892000 <none>      3    0.00      0.01   80.44
## 805    894805 <none>      1    0.00      0.00   80.44
## 806    899000 <none>      1    0.00      0.00   80.44
## 807    900000 <none>    693    0.37      1.71   82.16
## 808    900005 <none>      1    0.00      0.00   82.16
## 809    902000 <none>      1    0.00      0.00   82.16
## 810    903000 <none>      1    0.00      0.00   82.16
## 811    905000 <none>      1    0.00      0.00   82.17
## 812    907000 <none>      1    0.00      0.00   82.17
## 813    910000 <none>      2    0.00      0.00   82.17
## 814    915000 <none>      6    0.00      0.01   82.19
## 815    916000 <none>      1    0.00      0.00   82.19
## 816    917000 <none>      1    0.00      0.00   82.19
## 817    919000 <none>      1    0.00      0.00   82.20
## 818    920000 <none>     34    0.02      0.08   82.28
## 819    923000 <none>      1    0.00      0.00   82.28
## 820    924000 <none>      1    0.00      0.00   82.29
## 821    925000 <none>      2    0.00      0.00   82.29
## 822    930000 <none>     17    0.01      0.04   82.33
## 823    932000 <none>      1    0.00      0.00   82.33
## 824    934000 <none>      1    0.00      0.00   82.34
## 825    935000 <none>      1    0.00      0.00   82.34
## 826    936000 <none>      2    0.00      0.00   82.34
## 827    937000 <none>      1    0.00      0.00   82.35
## 828    938000 <none>      2    0.00      0.00   82.35
## 829    940000 <none>     20    0.01      0.05   82.40
## 830    944000 <none>      1    0.00      0.00   82.40
## 831    945000 <none>      1    0.00      0.00   82.41
## 832    948000 <none>      1    0.00      0.00   82.41
## 833    949000 <none>      1    0.00      0.00   82.41
## 834    950000 <none>    112    0.06      0.28   82.69
## 835    952000 <none>      1    0.00      0.00   82.69
## 836    953000 <none>      1    0.00      0.00   82.69
## 837    956000 <none>      1    0.00      0.00   82.70
## 838    960000 <none>     16    0.01      0.04   82.74
## 839    962000 <none>      1    0.00      0.00   82.74
## 840    963000 <none>      1    0.00      0.00   82.74
## 841    964000 <none>      1    0.00      0.00   82.74
## 842    965000 <none>      1    0.00      0.00   82.75
## 843    968000 <none>      1    0.00      0.00   82.75
## 844    969000 <none>      1    0.00      0.00   82.75
## 845    970000 <none>     17    0.01      0.04   82.79
## 846    971000 <none>      2    0.00      0.00   82.80
## 847    973000 <none>      1    0.00      0.00   82.80
## 848    976641 <none>      1    0.00      0.00   82.80
## 849    978000 <none>      1    0.00      0.00   82.80
## 850    980000 <none>     44    0.02      0.11   82.91
## 851    980777 <none>      1    0.00      0.00   82.92
## 852    983000 <none>      1    0.00      0.00   82.92
## 853    985000 <none>      1    0.00      0.00   82.92
## 854    990000 <none>      7    0.00      0.02   82.94
## 855    992000 <none>      2    0.00      0.00   82.94
## 856    993000 <none>      1    0.00      0.00   82.95
## 857    995000 <none>      4    0.00      0.01   82.96
## 858    996000 <none>      2    0.00      0.00   82.96
## 859    998000 <none>      3    0.00      0.01   82.97
## 860    999000 <none>      1    0.00      0.00   82.97
## 861   1000000 <none>   1252    0.68      3.10   86.07
## 862   1000050 <none>      3    0.00      0.01   86.08
## 863   1000100 <none>      2    0.00      0.00   86.08
## 864   1000500 <none>      1    0.00      0.00   86.08
## 865   1003000 <none>      1    0.00      0.00   86.09
## 866   1005000 <none>      1    0.00      0.00   86.09
## 867   1008000 <none>      3    0.00      0.01   86.10
## 868   1009000 <none>      1    0.00      0.00   86.10
## 869   1010000 <none>      1    0.00      0.00   86.10
## 870   1012000 <none>      2    0.00      0.00   86.11
## 871   1014000 <none>      1    0.00      0.00   86.11
## 872   1017000 <none>      2    0.00      0.00   86.11
## 873   1020000 <none>      8    0.00      0.02   86.13
## 874   1022000 <none>      1    0.00      0.00   86.13
## 875   1024000 <none>      2    0.00      0.00   86.14
## 876   1025000 <none>      1    0.00      0.00   86.14
## 877   1027000 <none>      2    0.00      0.00   86.15
## 878   1028000 <none>      1    0.00      0.00   86.15
## 879   1029000 <none>      1    0.00      0.00   86.15
## 880   1030000 <none>      4    0.00      0.01   86.16
## 881   1036000 <none>      2    0.00      0.00   86.17
## 882   1039000 <none>      1    0.00      0.00   86.17
## 883   1040000 <none>      5    0.00      0.01   86.18
## 884   1046000 <none>      1    0.00      0.00   86.18
## 885   1048000 <none>      1    0.00      0.00   86.19
## 886   1049000 <none>      2    0.00      0.00   86.19
## 887   1050000 <none>     33    0.02      0.08   86.27
## 888   1056000 <none>      2    0.00      0.00   86.28
## 889   1060000 <none>      8    0.00      0.02   86.30
## 890   1070000 <none>      5    0.00      0.01   86.31
## 891   1072000 <none>      1    0.00      0.00   86.31
## 892   1080000 <none>     12    0.01      0.03   86.34
## 893   1082000 <none>      2    0.00      0.00   86.35
## 894   1083000 <none>      1    0.00      0.00   86.35
## 895   1085000 <none>      1    0.00      0.00   86.35
## 896   1089613 <none>      1    0.00      0.00   86.36
## 897   1090000 <none>      4    0.00      0.01   86.36
## 898   1092000 <none>      1    0.00      0.00   86.37
## 899   1094000 <none>      1    0.00      0.00   86.37
## 900   1100000 <none>    346    0.19      0.86   87.23
## 901   1102000 <none>      1    0.00      0.00   87.23
## 902   1110000 <none>      3    0.00      0.01   87.24
## 903   1120000 <none>      8    0.00      0.02   87.26
## 904   1123000 <none>      1    0.00      0.00   87.26
## 905   1125378 <none>      1    0.00      0.00   87.26
## 906   1130000 <none>      7    0.00      0.02   87.28
## 907   1133000 <none>      1    0.00      0.00   87.28
## 908   1138000 <none>      1    0.00      0.00   87.28
## 909   1139000 <none>      1    0.00      0.00   87.29
## 910   1140000 <none>      1    0.00      0.00   87.29
## 911   1143000 <none>      1    0.00      0.00   87.29
## 912   1148000 <none>      1    0.00      0.00   87.29
## 913   1150000 <none>     30    0.02      0.07   87.37
## 914   1155000 <none>      1    0.00      0.00   87.37
## 915   1160000 <none>      7    0.00      0.02   87.39
## 916   1160250 <none>      1    0.00      0.00   87.39
## 917   1163000 <none>      1    0.00      0.00   87.39
## 918   1169000 <none>      1    0.00      0.00   87.39
## 919   1170000 <none>      4    0.00      0.01   87.40
## 920   1176000 <none>      1    0.00      0.00   87.41
## 921   1180000 <none>      7    0.00      0.02   87.42
## 922   1185000 <none>      2    0.00      0.00   87.43
## 923   1190000 <none>      5    0.00      0.01   87.44
## 924   1197000 <none>      1    0.00      0.00   87.44
## 925   1200000 <none>    737    0.40      1.82   89.27
## 926   1220000 <none>      1    0.00      0.00   89.27
## 927   1230000 <none>      1    0.00      0.00   89.27
## 928   1238000 <none>      1    0.00      0.00   89.27
## 929   1240000 <none>      4    0.00      0.01   89.28
## 930   1242000 <none>      1    0.00      0.00   89.29
## 931   1244273 <none>      1    0.00      0.00   89.29
## 932   1250000 <none>     31    0.02      0.08   89.37
## 933   1253000 <none>      1    0.00      0.00   89.37
## 934   1258000 <none>      2    0.00      0.00   89.37
## 935   1260000 <none>      1    0.00      0.00   89.38
## 936   1268000 <none>      2    0.00      0.00   89.38
## 937   1270000 <none>      3    0.00      0.01   89.39
## 938   1280000 <none>      5    0.00      0.01   89.40
## 939   1287275 <none>      1    0.00      0.00   89.40
## 940   1288000 <none>      1    0.00      0.00   89.41
## 941   1290000 <none>      3    0.00      0.01   89.41
## 942   1298000 <none>      1    0.00      0.00   89.42
## 943   1300000 <none>    374    0.20      0.93   90.34
## 944   1304144 <none>      1    0.00      0.00   90.34
## 945   1307000 <none>      2    0.00      0.00   90.35
## 946   1313354 <none>      1    0.00      0.00   90.35
## 947   1320000 <none>      3    0.00      0.01   90.36
## 948   1330000 <none>      2    0.00      0.00   90.36
## 949   1333000 <none>      1    0.00      0.00   90.37
## 950   1338182 <none>      1    0.00      0.00   90.37
## 951   1340000 <none>      1    0.00      0.00   90.37
## 952   1350000 <none>     50    0.03      0.12   90.49
## 953   1360000 <none>      1    0.00      0.00   90.50
## 954   1370000 <none>      1    0.00      0.00   90.50
## 955   1373000 <none>      1    0.00      0.00   90.50
## 956   1380000 <none>      3    0.00      0.01   90.51
## 957   1386000 <none>      1    0.00      0.00   90.51
## 958   1390000 <none>      1    0.00      0.00   90.51
## 959   1395000 <none>      1    0.00      0.00   90.52
## 960   1397000 <none>      1    0.00      0.00   90.52
## 961   1399000 <none>      1    0.00      0.00   90.52
## 962   1400000 <none>    274    0.15      0.68   91.20
## 963   1420000 <none>      2    0.00      0.00   91.20
## 964   1423000 <none>      1    0.00      0.00   91.21
## 965   1425000 <none>      1    0.00      0.00   91.21
## 966   1437000 <none>      1    0.00      0.00   91.21
## 967   1450000 <none>     16    0.01      0.04   91.25
## 968   1458000 <none>      1    0.00      0.00   91.25
## 969   1460000 <none>      1    0.00      0.00   91.26
## 970   1476000 <none>      1    0.00      0.00   91.26
## 971   1480000 <none>      4    0.00      0.01   91.27
## 972   1488000 <none>      1    0.00      0.00   91.27
## 973   1490000 <none>      1    0.00      0.00   91.27
## 974   1500000 <none>    768    0.41      1.90   93.17
## 975   1514000 <none>      1    0.00      0.00   93.18
## 976   1520000 <none>      2    0.00      0.00   93.18
## 977   1525000 <none>      1    0.00      0.00   93.18
## 978   1526000 <none>      1    0.00      0.00   93.19
## 979   1535000 <none>      1    0.00      0.00   93.19
## 980   1550000 <none>      6    0.00      0.01   93.20
## 981   1556000 <none>      1    0.00      0.00   93.21
## 982   1560000 <none>      4    0.00      0.01   93.22
## 983   1560800 <none>      1    0.00      0.00   93.22
## 984   1570000 <none>      1    0.00      0.00   93.22
## 985   1578000 <none>      1    0.00      0.00   93.22
## 986   1580000 <none>      3    0.00      0.01   93.23
## 987   1586000 <none>      1    0.00      0.00   93.23
## 988   1590000 <none>      3    0.00      0.01   93.24
## 989   1595000 <none>      1    0.00      0.00   93.24
## 990   1600000 <none>    220    0.12      0.54   93.79
## 991   1608000 <none>      1    0.00      0.00   93.79
## 992   1620000 <none>      2    0.00      0.00   93.79
## 993   1630000 <none>      1    0.00      0.00   93.80
## 994   1650000 <none>      9    0.00      0.02   93.82
## 995   1657000 <none>      1    0.00      0.00   93.82
## 996   1660000 <none>      1    0.00      0.00   93.82
## 997   1670000 <none>      2    0.00      0.00   93.83
## 998   1690000 <none>      2    0.00      0.00   93.83
## 999   1700000 <none>    166    0.09      0.41   94.25
## 1000  1720000 <none>      1    0.00      0.00   94.25
## 1001  1750000 <none>     14    0.01      0.03   94.28
## 1002  1758000 <none>      1    0.00      0.00   94.28
## 1003  1770000 <none>      2    0.00      0.00   94.29
## 1004  1780000 <none>      3    0.00      0.01   94.30
## 1005  1785000 <none>      1    0.00      0.00   94.30
## 1006  1790000 <none>      1    0.00      0.00   94.30
## 1007  1799000 <none>      1    0.00      0.00   94.30
## 1008  1800000 <none>    330    0.18      0.82   95.12
## 1009  1819437 <none>      1    0.00      0.00   95.12
## 1010  1825000 <none>      1    0.00      0.00   95.13
## 1011  1850000 <none>      9    0.00      0.02   95.15
## 1012  1860000 <none>      2    0.00      0.00   95.15
## 1013  1870000 <none>      2    0.00      0.00   95.16
## 1014  1876000 <none>      1    0.00      0.00   95.16
## 1015  1890000 <none>      2    0.00      0.00   95.17
## 1016  1891781 <none>      1    0.00      0.00   95.17
## 1017  1894000 <none>      1    0.00      0.00   95.17
## 1018  1900000 <none>     85    0.05      0.21   95.38
## 1019  1920000 <none>      1    0.00      0.00   95.38
## 1020  1940000 <none>      1    0.00      0.00   95.39
## 1021  1950000 <none>      5    0.00      0.01   95.40
## 1022  1980000 <none>      1    0.00      0.00   95.40
## 1023  1990000 <none>      1    0.00      0.00   95.40
## 1024  1995000 <none>      1    0.00      0.00   95.41
## 1025  2000000 <none>    456    0.25      1.13   96.53
## 1026  2000008 <none>      1    0.00      0.00   96.54
## 1027  2030000 <none>      1    0.00      0.00   96.54
## 1028  2050000 <none>      4    0.00      0.01   96.55
## 1029  2060000 <none>      1    0.00      0.00   96.55
## 1030  2072000 <none>      1    0.00      0.00   96.55
## 1031  2080000 <none>      1    0.00      0.00   96.56
## 1032  2088808 <none>      1    0.00      0.00   96.56
## 1033  2090000 <none>      1    0.00      0.00   96.56
## 1034  2100000 <none>     65    0.04      0.16   96.72
## 1035  2107000 <none>      1    0.00      0.00   96.72
## 1036  2150000 <none>      2    0.00      0.00   96.73
## 1037  2170000 <none>      1    0.00      0.00   96.73
## 1038  2180000 <none>      1    0.00      0.00   96.73
## 1039  2190000 <none>      1    0.00      0.00   96.74
## 1040  2200000 <none>     73    0.04      0.18   96.92
## 1041  2220000 <none>      1    0.00      0.00   96.92
## 1042  2250000 <none>      4    0.00      0.01   96.93
## 1043  2290000 <none>      1    0.00      0.00   96.93
## 1044  2300000 <none>     77    0.04      0.19   97.12
## 1045  2320000 <none>      1    0.00      0.00   97.13
## 1046  2350000 <none>      3    0.00      0.01   97.13
## 1047  2400000 <none>     51    0.03      0.13   97.26
## 1048  2410000 <none>      1    0.00      0.00   97.26
## 1049  2460000 <none>      1    0.00      0.00   97.26
## 1050  2470000 <none>      2    0.00      0.00   97.27
## 1051  2500000 <none>    206    0.11      0.51   97.78
## 1052  2550000 <none>      1    0.00      0.00   97.78
## 1053  2600000 <none>     32    0.02      0.08   97.86
## 1054  2700000 <none>     36    0.02      0.09   97.95
## 1055  2750000 <none>      2    0.00      0.00   97.95
## 1056  2770000 <none>      1    0.00      0.00   97.96
## 1057  2800000 <none>     51    0.03      0.13   98.08
## 1058  2866000 <none>      1    0.00      0.00   98.09
## 1059  2900000 <none>     20    0.01      0.05   98.13
## 1060  2930000 <none>      1    0.00      0.00   98.14
## 1061  2983550 <none>      1    0.00      0.00   98.14
## 1062  3000000 <none>    197    0.11      0.49   98.63
## 1063  3000200 <none>      1    0.00      0.00   98.63
## 1064  3050000 <none>      1    0.00      0.00   98.63
## 1065  3060000 <none>      1    0.00      0.00   98.63
## 1066  3100000 <none>      7    0.00      0.02   98.65
## 1067  3196000 <none>      1    0.00      0.00   98.65
## 1068  3200000 <none>     31    0.02      0.08   98.73
## 1069  3280000 <none>      1    0.00      0.00   98.73
## 1070  3300000 <none>      8    0.00      0.02   98.75
## 1071  3400000 <none>     11    0.01      0.03   98.78
## 1072  3412000 <none>      1    0.00      0.00   98.78
## 1073  3459000 <none>      1    0.00      0.00   98.79
## 1074  3460000 <none>      1    0.00      0.00   98.79
## 1075  3500000 <none>     99    0.05      0.24   99.03
## 1076  3600000 <none>     12    0.01      0.03   99.06
## 1077  3670000 <none>      1    0.00      0.00   99.06
## 1078  3677137 <none>      1    0.00      0.00   99.07
## 1079  3700000 <none>      9    0.00      0.02   99.09
## 1080  3800000 <none>     20    0.01      0.05   99.14
## 1081  3900000 <none>      1    0.00      0.00   99.14
## 1082  4000000 <none>     99    0.05      0.24   99.39
## 1083  4100000 <none>      5    0.00      0.01   99.40
## 1084  4200000 <none>     11    0.01      0.03   99.43
## 1085  4300000 <none>      2    0.00      0.00   99.43
## 1086  4400000 <none>      2    0.00      0.00   99.44
## 1087  4500000 <none>     34    0.02      0.08   99.52
## 1088  4600000 <none>      8    0.00      0.02   99.54
## 1089  4700000 <none>      4    0.00      0.01   99.55
## 1090  4800000 <none>      5    0.00      0.01   99.56
## 1091  5000000 <none>     65    0.04      0.16   99.72
## 1092  5100000 <none>      1    0.00      0.00   99.73
## 1093  5200000 <none>      4    0.00      0.01   99.74
## 1094  5300000 <none>      1    0.00      0.00   99.74
## 1095  5500000 <none>     12    0.01      0.03   99.77
## 1096  5600000 <none>      3    0.00      0.01   99.77
## 1097  5700000 <none>      4    0.00      0.01   99.78
## 1098  5800000 <none>      1    0.00      0.00   99.79
## 1099  5900000 <none>      1    0.00      0.00   99.79
## 1100  6000000 <none>     41    0.02      0.10   99.89
## 1101  6100000 <none>      1    0.00      0.00   99.89
## 1102  6500000 <none>      2    0.00      0.00   99.90
## 1103  6800000 <none>      1    0.00      0.00   99.90
## 1104  7000000 <none>     11    0.01      0.03   99.93
## 1105  7800000 <none>      1    0.00      0.00   99.93
## 1106  8000000 <none>      8    0.00      0.02   99.95
## 1107  9000000 <none>      3    0.00      0.01   99.96
## 1108  9500000 <none>      2    0.00      0.00   99.96
## 1109  9800000 <none>      1    0.00      0.00   99.97
## 1110 10000000 <none>      5    0.00      0.01   99.98
## 1111 12000000 <none>      4    0.00      0.01   99.99
## 1112 15000000 <none>      3    0.00      0.01  100.00
## 1113 17700000 <none>      1    0.00      0.00  100.00
## 1114 25000000 <none>      1    0.00      0.00  100.00
## 1115       NA   <NA> 145019   78.20        NA      NA
```

Para las variables numeric, en cambio, podemos utilizar la función `descr()` del mismo paquete, que nos indicará las *medidas de tendencia central, dispersión y posición* de la variable.




#### Sobre las clases de las variables

Un punto que ya se hace evidente de empezar a trabajar son como reconocer y trabajar **los distintos tipos de variables en R**. Si bien al inicio no es tan intuitivo ¡no te preocupes! Cuando uno avanza en el aprendizaje del programa de a poco estos conceptos se asimilan más. 

Partamos por lo básico que ustedes ya saben de sus cursos de estadística: **los niveles de medición**.  Sabemos que el nivel de medición de la variable *género* es *nominal* y, por lo tanto, sólo nos permite clasificar a unas personas u otras. Por ello, no podemos, por ejemplo, calcular el promedio del género de una muestra. Por otra parte, la variable *edad* presenta un nivel de medición *de razón*, por lo cual sí podemos realizar con ella operaciones aritméticas y, en consecuencia, es posible, por ejemplo, calcular el promedio de edad de la muestra con que estamos trabajando. 

Como revisamos en el práctico anterior La función `class()`, incluido en el *paquete base* de R, nos permite saber la *clase* de una variable determinada con la cual deseemos trabajar. También nos permite conocer la clase de otros objetos en R, pero no es algo que utilizaremos (aún).

#### En resumen:

| Clase  | Tipo de variable |
|---------:|:--------|
| `numeric` | Cuantitativa |
| `character` | Categórica |
| `Logic` |  Lógica (TRUE, FALSE, NA) |
| `factor` | Categórica con niveles y etiquetas |


## 3.1. Selección de variables

Luego de revisar el nivel de medición de las variables, y reconociendo la distribución de los datos que tenemos, es evidente que lo mejor es solamente trabajar con un *subconjunto* de los datos con las variables que queremos transformar. Si bien en el práctico 5 profundizaremos en la selección de variables, este es un primer adelanto que nos permitirá exportar datos que sólo contengan la información que nos interesa.

Para asegurarnos de saber exactamente qué variables queremos utilizar deberíamos ir al libro de códigos, pero también podemos elegir y decidir de manera más certera qué variables incorporar a partir de la revisión con `find_var()`. En esta sección no profundizaremos en distintas formas de seleccionar datos (¡ello lo veremos **a fondo** en el siguiente práctico!). Solo les mostraremos de manera simple cómo seleccionar algunas variables de nuestro interés:

- `ypchtotcor`: Ingresos del hogar
- `v13`: "Su hogar, ¿bajo qué situación ocupa la vivienda?"
- `v29`: "¿Cuántos dormitorios de uso exclusivo ocupa su hogar en esta vivienda?"
- `p6`: "¿Cuántas personas viven habitualmente en esta vivienda?"

Crearemos una base procesada llamada `datos_proc`, que sólo incluirá estas cuatro variables. 



Como podemos ver en el ambiente (Environment), se creó un nuevo objeto llamado `datos_proc`, que tiene la misma cantidad de observaciones (filas) que datos, pero que sólo incluye 4 de las 650 variables iniciales. 

**IMPORTANTE**: objetos que tengan el mismo nombre **se sobreescriben**. Les recomendamos que, siempre que seleccionen variables (o que manipulen sus datos de cualquier manera), creen un objeto cuyo nombre sea distinto al original. Podemos, por ejemplo, incorporar el sufijo **_proc** como lo hicimos en este práctico.

## 4. Guardar y exportar datos

Por último, una vez que hayamos procesado los datos, es importante que los **guardemos** en una nueva base de datos procesada, para no tener que llevar a cabo el procesamiento otra vez.

Al igual que en el paso de carga de datos, y a partir del flujo de `input-R-output` propuesto, es esperable que estos datos procesados (o intermedios) los guardemos en `output/data`, pues son el resultado de un proceso de manipulación anterior (como pueden ser aquellos que realizamos en nuestro **código de procesamiento**)

El archivo se puede guardar en distintos formatos: 

### a) .RData y .rds


{{< div "note" >}}
*Guardar un objeto a un archivo*

saveRDS(objecto, file = "datos.rds")

*Guardar un objeto a un archivo*
save(objeto1, objeto2, file = "datos.RData)
{{</div>}}

Es lo recomendable, si el resto del análisis lo realizaremos en R. 


```r
save(file = "output/data/datos_proc.RData") #Guardamos el objeto datos_proc en la ruta de trabajo actual, bajo el nombre de datos_proc.RData. 
```

```
## Warning in save(file = "output/data/datos_proc.RData"): nothing specified to be
## save()d
```

```r
saveRDS(datos_proc, file= "output/data/datos_proc.rds") #Guardamos el objeto datos_proc en la ruta de trabajo actual, bajo el nombre de datos_proc.rds. 
```

### b) .sav (`haven`)


```r
write_sav(datos_proc, "output/data/datos_proc.sav") #Guardamos el objeto datos_proc en la ruta de trabajo actual, bajo el nombre de datos_proc.sav. 
```

### b) .dta (`haven`)


```r
write_dta(datos_proc, "output/data/datos_proc.dta") #Guardamos el objeto datos_proc en la ruta de trabajo actual, bajo el nombre de datos_proc.dta. 
```

### b) .csv


```r
write.csv(datos_proc, "output/data/datos_proc.csv") #Guardamos el objeto datos_proc en la ruta de trabajo actual, bajo el nombre de datos_proc.csv. 
```

### b) .xlsx


```r
writexl::write_xlsx(datos_proc, "output/data/datos_proc.xlsx") #Guardamos el objeto datos_proc en la ruta de trabajo actual, bajo el nombre de datos_proc.xlsx. 
```

## Resumen

¡Eso es todo por este práctico! Aquí, aprendimos a: 
- 1) Importar datos en diferentes formatos.
- 2) Seleccionar variables (y hacer una revisión de ciertos aspectos de estas).
- 3) Guardar y exportar los datos procesados, en distintos formatos.

