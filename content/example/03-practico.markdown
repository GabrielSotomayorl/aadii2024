---
title: "Tipos de datos"
linktitle: "3: Tipos de datos"
date: "2021-08-16"
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

El objetivo de este práctico es presentar una introducción a los tipos de datos que podemos encontrarnos trabajando con R. Esto constituye uno de los pilares fundamentales para el correcto procesamiento, análisis y presentación de resultados. 

##  Materiales de la sesión

En este práctico trabajaremos con datos de la **Encuesta Suplementaria de Ingresos (*ESI*)** desarrollada por el **Instituto Nacional de Estadísticas** en el año 2020. Esta es "un módulo complementario que se aplica dentro de la Encuesta Nacional de Empleo (ENE) (...) Su objetivo principal es caracterizar los ingresos laborales de las personas que son clasificadas como ocupadas en la ENE y otras fuentes de ingresos de los hogares", contando con un diseño muestral probabilístico, estratificado y bietápico. Pueden encontrar más información [aquí](https://www.ine.cl/estadisticas/sociales/ingresos-y-gastos/encuesta-suplementaria-de-ingresos). 

Recuerden que los archivos asociados a este práctico se pueden descargar aquí:

- [<i class="fas fa-file-archive"></i> `03-class.zip`](https://github.com/learn-R/03-class-datatypes-/raw/main/03-clase.zip) 

## 1. Tipos de datos 

### a) ¿Qué son?

Tanto en R como en la mayoría de lenguajes de programación, contamos con datos de *diversos tipos*, en razón de los cuales podemos ejecutar determinados procedimientos de tratamiento o análisis. 

Los tipos de datos están **íntimamente relacionados con el *nivel de medición* de las variables** a las cuales corresponden. Como habrán aprendido en sus cursos de estadística, la teoría de los niveles de medición contempla cuatro de ellos: 

1. **Nominal**: para variables categóricas o cualitativas. Los números asignados permiten dividir los datos en diferentes grupos, sin la asignación de un orden específico para las categorías entre las cuales se dispersan los datos. Un ejemplo de variable nominal es la comuna de residencia o el género. 

1. **Ordinal**: para variables categóricas o cualitativas. Los números asignados permiten dividir los datos en diferentes grupos y, a la vez, establecer un *orden no exacto* entre ellos. Ello quiere decir que un número más bajo (alto) indicará una menor (mayor) presencia del atributo. Sin embargo, los números no indican distancias concretas: por ejemplo, la distancia entre las categorías 1 y 2 **no necesariamente** es la misma que aquella que existe entre las categorías 2 y 3. Para ilustrarlo más claramente, las escalas Likert representan un ejemplo clásico de variables ordinales: el/la informante puede declarar un mayor o menor grado de acuerdo con las medidas adoptadas por el gobierno durante la pandemia. Sin embargo, no sabemos **cuán más de acuerdo se encuentran**: sólo sabemos que se encuentran más o menos de acuerdo con aquellas medidas en relación con otro/a informante.

1. **Intervalar**: para variables numéricas o cuantitativas. Aquí es posible clasificar y ordenar los datos, estableciendo además distancias concretas entre un valor y otro. Por ello, las variables intervalares nos permiten **ejecutar operaciones aritméticas** y, por lo tanto, es posible calcular estadísticos como la media de una distribución. Un ejemplo de variable intervalar puede ser la cantidad de jugadoras/es presentes en una cancha de basketball durante un partido. 

1. **De razón**: para variables numéricas o cuantitativas. La mayor diferencia con las variables de nivel intervalar es que **incluyen un cero absoluto** que indica **ausencia del atributo**. En el caso anterior, podríamos señalar que, teóricamente, no puede haber cero jugadoras/es en una cancha de basketball durante un partido, pues de ser así no estaría ocurriendo el partido. Sin embargo, sí podemos decir que una persona propietaria de un emprendimiento tiene 2, 4 o 0 trabajadores/as contratados/as. En este caso, el 0 indicaría que *no ha contratado a ningún/a trabajador/a*, y la distancia entre 0 y 2 trabajadores/as es la misma que la que existe entre 2 y 4 trabajadores/as (es decir, una diferencia de 2 trabajadores/as para cada caso).

En la sección **Recursos** de la página web del curso podrán encontrar un artículo explicativo referente a los [tipos de variables](https://learn-r-uah.netlify.app/resource/r-datatypes-example/), donde se presentan además algunos ejemplos para cada una de ellas. 

### b) ¿Qué tipos de datos podemos encontrarnos en R?

Para responder a esta pregunta ¡metamos las manos en la masa! (o en los datos). Como se señaló anteriormente, trabajaremos con la Encuesta Suplementaria de Ingresos en su versión 2020.


```r
pacman::p_load(sjPlot, sjmisc)
```




Estos datos ya han sido procesados para ajustarse a los contenidos del curso. Por ello, contamos con 5 variables que permitirán ilustrar los contenidos que atañen a este práctico


```r
view_df(data, encoding = "LATIN1")
```

<table style="border-collapse:collapse; border:none;">
<caption>Data frame: data</caption>
<tr>
<th style="border-bottom:double; font-style:italic; font-weight:normal; padding:0.2cm; text-align:left; vertical-align:top;">ID</th><th style="border-bottom:double; font-style:italic; font-weight:normal; padding:0.2cm; text-align:left; vertical-align:top;">Name</th><th style="border-bottom:double; font-style:italic; font-weight:normal; padding:0.2cm; text-align:left; vertical-align:top;">Label</th><th style="border-bottom:double; font-style:italic; font-weight:normal; padding:0.2cm; text-align:left; vertical-align:top;">Values</th><th style="border-bottom:double; font-style:italic; font-weight:normal; padding:0.2cm; text-align:left; vertical-align:top;">Value Labels</th>
</tr>
<tr>
<td style="padding:0.2cm; text-align:left; vertical-align:top;">1</td>
<td style="padding:0.2cm; text-align:left; vertical-align:top;">id</td>
<td style="padding:0.2cm; text-align:left; vertical-align:top;">Identificador de individuo</td>
<td style="padding:0.2cm; text-align:left; vertical-align:top;"></td>
<td style="padding:0.2cm; text-align:left; vertical-align:top;">&lt;output omitted&gt;</td>
</tr>
<tr>
<td style="padding:0.2cm; text-align:left; vertical-align:top; background-color:#eeeeee">2</td>
<td style="padding:0.2cm; text-align:left; vertical-align:top; background-color:#eeeeee">a6</td>
<td style="padding:0.2cm; text-align:left; vertical-align:top; background-color:#eeeeee">a6. Â¿Por quÃ© razÃ³n no trabajÃ³ la semana pasada?</td>
<td style="padding:0.2cm; text-align:left; vertical-align:top; background-color:#eeeeee">1<br>2<br>3<br>4<br>5<br>6<br>7<br>8<br>9<br>10<br>11<br>12<br>13<br>14<br>88<br><span style="omit">&lt;...&gt;</span></td>
<td style="padding:0.2cm; text-align:left; vertical-align:top; background-color:#eeeeee">Vacaciones o permisos<br>Licencia mÃ©dica<br>Por horario o jornada variable o flexible<br>Huelga o conflicto laboral<br>Asistencia a cursos de capacitaciÃ³n<br>Problemas de salud<br>SuspensiÃ³n temporal del trabajo<br>No tuvo pedidos o clientes<br>Razones climÃ¡ticas o catÃ¡strofes naturales<br>Su trabajo es ocasional<br>Razones econÃ³micas o tÃ©cnicas de la empresa o negocio<br>Su trabajo es estacional<br>Clausura de negocio o de empresa<br>Otras razones (especificar)<br>No sabe<br><span style="omit">&lt;... truncated&gt;</span></td>
</tr>
<tr>
<td style="padding:0.2cm; text-align:left; vertical-align:top;">3</td>
<td style="padding:0.2cm; text-align:left; vertical-align:top;">a6_otro</td>
<td style="padding:0.2cm; text-align:left; vertical-align:top;">Otras razones de ausencia al trabajo</td>
<td style="padding:0.2cm; text-align:left; vertical-align:top;"></td>
<td style="padding:0.2cm; text-align:left; vertical-align:top;"></td>
</tr>
<tr>
<td style="padding:0.2cm; text-align:left; vertical-align:top; background-color:#eeeeee">4</td>
<td style="padding:0.2cm; text-align:left; vertical-align:top; background-color:#eeeeee">nivel</td>
<td style="padding:0.2cm; text-align:left; vertical-align:top; background-color:#eeeeee">Nivel educacional mas alto aprobado</td>
<td style="padding:0.2cm; text-align:left; vertical-align:top; background-color:#eeeeee"></td>
<td style="padding:0.2cm; text-align:left; vertical-align:top; background-color:#eeeeee">Basica o primaria<br>Media o humanidades<br>Menos que basica/primaria<br>Tecnico profesional<br>Universitaria o mas</td>
</tr>
<tr>
<td style="padding:0.2cm; text-align:left; vertical-align:top;">5</td>
<td style="padding:0.2cm; text-align:left; vertical-align:top;">ingresos</td>
<td style="padding:0.2cm; text-align:left; vertical-align:top;">Ingresos por sueldos y salarios</td>
<td style="padding:0.2cm; text-align:left; vertical-align:top;" colspan="2"><em>range: 0.0-9312239.1</em></td>
</tr>
<tr>
<td style="padding:0.2cm; text-align:left; vertical-align:top; background-color:#eeeeee">6</td>
<td style="padding:0.2cm; text-align:left; vertical-align:top; background-color:#eeeeee">afp</td>
<td style="padding:0.2cm; text-align:left; vertical-align:top; background-color:#eeeeee">Empleador cotiza en prevision (AFP)</td>
<td style="padding:0.2cm; text-align:left; vertical-align:top; background-color:#eeeeee"></td>
<td style="padding:0.2cm; text-align:left; vertical-align:top; background-color:#eeeeee"></td>
</tr>

</table>

¡Vamos a revisar cada una de ellas!

#### I) character

Los datos `character` están directamente asociados a las variables cualitativas (o categóricas). En general, estos datos incluyen palabras, aunque también pueden incorporar números que no representen una variable numérica, como puede ser el RUT de los/as informantes: es un número que no denota una distancia clara entre la inscripción de una persona a la de otra. Suelen ser utilizadas para variables de texto abierto, como es el caso de la variable **a6_otro**, que detalla otras razones por las cuales las/os informantes declararon ausentarse al trabajo durante la semana pasada, más allá de aquellas especificadas en las alternativas de respuesta de la pregunta original. 

Para saber cuál es el tipo de una variable en específico, utilizaremos la función `class()` del paquete base de R


```r
class(data$a6_otro)
```

```
## [1] "character"
```

Si deseamos trabajar con texto abierto, o manipular ese texto ¡debemos trabajar con character! sin embargo, estas variables no tienden a ser las mejores a la hora de presentar nuestros resultados, entre otras razones, porque no nos permiten especificar un orden distinto al alfabético. Para solucionarlo, tenemos las variables de tipo **Factor**

#### II) Factor

Las variables de tipo `Factor` son las ideales a la hora de trabajar con variables de tipo nominal u ordinal. Esto se debe a dos motivos: en primer lugar, su comptabilidad con librerías como `sjPlot`, `sjmisc` o `ggplot2`, que nos permiten presentar de manera cómoda estos datos; así como su compatibilidad con funciones dedicadas a la generación de distintos modelos estadísticos, como modelos de análisis de clases latentes o de regresión. En segundo término, porque cabe la posibilidad de **establecer un orden (distinto al alfabético)** entre las categorías de la variable, lo cual es *fundamental* si trabajamos con variables ordinales, como el **nivel educacional** de las/os informantes


```r
class(data$nivel)
```

```
## [1] "factor"
```


```
## Nivel educacional mas alto aprobado (x) <categorical> 
## # total N=71935 valid N=71152 mean=2.43 sd=1.40
## 
## Value                     |     N | Raw % | Valid % | Cum. %
## ------------------------------------------------------------
## Basica o primaria         | 21374 | 29.71 |   30.04 |  30.04
## Media o humanidades       | 26194 | 36.41 |   36.81 |  66.85
## Menos que basica/primaria |  6417 |  8.92 |    9.02 |  75.87
## Tecnico profesional       |  6003 |  8.35 |    8.44 |  84.31
## Universitaria o mas       | 11164 | 15.52 |   15.69 | 100.00
## <NA>                      |   783 |  1.09 |    <NA> |   <NA>
```

¿Cómo podemos ordenar los factores de una variable de este tipo? ¡Calma! en el práctico 6 profundizaremos como corresponde en la transformación de variables. 

#### III) numeric

Como su nombre lo indica, este tipo de variables nos permiten trabajar con variables con un nivel de medición cuantitativo 


```r
class(data$ingresos)
```

```
## [1] "numeric"
```

Mientras R arroja un error al, por ejemplo, intentar calcular el promedio de nivel educacional


```r
mean(data$nivel)
```

```
## Warning in mean.default(data$nivel): argument is not numeric or logical:
## returning NA
```

```
## [1] NA
```

¡Si podemos calcular la media de ingresos por sueldos y salarios!


```r
mean(data$ingresos)
```

```
## [1] 162615.6
```

¡Mucho cuidado! muchas veces, los datos incluyen variables de tipo `numeric` que, teóricamente, no son variables numéricas (por ejemplo, variables donde 1 == Si, y 2 == No). Por ello, es fundamental que **revisen detalladamente la documentación anexa** de sus datos, a modo de determinar cuáles variables corresponden su tipo de dato con su nivel de medición, y cuáles no. 

#### IV) dbl + lbl (números etiquetados)

Es bastante usual (como veremos en el siguiente práctico) que, aunque estemos trabajando en lenguaje R, los datos que emplearemos estén en formato SPSS (.sav) o Stata (.dta). En ambos casos, los datos (sobre todo las variables categóricas) presentan una estructura de números etiquetados, donde el valor principal de la variable es un número, pero el cual presenta una etiqueta en palabras que identifica a qué refiere cada una de las categorías de la variable. En general, cuando preguntamos sobre la clase de una variable dbl+lbl, obtendremos algo así:


```r
class(data$a6)
```

```
## [1] "haven_labelled" "vctrs_vctr"     "double"
```


```
## a6. ¿Por qué razón no trabajó la semana pasada? (x) <numeric> 
## # total N=71935 valid N=2635 mean=5.39 sd=3.56
## 
## Value |                                                 Label |     N | Raw % | Valid % | Cum. %
## ------------------------------------------------------------------------------------------------
##     1 |                                 Vacaciones o permisos |   227 |  0.32 |    8.61 |   8.61
##     2 |                                       Licencia médica |   823 |  1.14 |   31.23 |  39.85
##     3 |             Por horario o jornada variable o flexible |    86 |  0.12 |    3.26 |  43.11
##     4 |                            Huelga o conflicto laboral |     4 |  0.01 |    0.15 |  43.26
##     5 |                   Asistencia a cursos de capacitación |    24 |  0.03 |    0.91 |  44.17
##     6 |                                    Problemas de salud |    77 |  0.11 |    2.92 |  47.10
##     7 |                       Suspensión temporal del trabajo |   976 |  1.36 |   37.04 |  84.14
##     8 |                            No tuvo pedidos o clientes |   131 |  0.18 |    4.97 |  89.11
##     9 |            Razones climáticas o catástrofes naturales |    27 |  0.04 |    1.02 |  90.13
##    10 |                               Su trabajo es ocasional |    45 |  0.06 |    1.71 |  91.84
##    11 | Razones económicas o técnicas de la empresa o negocio |    29 |  0.04 |    1.10 |  92.94
##    12 |                              Su trabajo es estacional |    12 |  0.02 |    0.46 |  93.40
##    13 |                      Clausura de negocio o de empresa |     8 |  0.01 |    0.30 |  93.70
##    14 |                           Otras razones (especificar) |   166 |  0.23 |    6.30 | 100.00
##    88 |                                               No sabe |     0 |  0.00 |    0.00 | 100.00
##    99 |                                           No responde |     0 |  0.00 |    0.00 | 100.00
##  <NA> |                                                  <NA> | 69300 | 96.34 |    <NA> |   <NA>
```

En este caso, cada número se asocia con una razón por la cual el/la informante se ausentó del trabajo la semana anterior. Por supuesto, es una variable ordinal. 

La mayor parte de las veces, sobre todo por la compatibilidad con librerías de representación gráfica, buscaremos que nuestros números etiquetados se conviertan en factores ¡el práctico 6 se encargará de profundizar en ello!

#### V) logical

¡El último tipo de dato! Los valores lógicos (`logical`) son bastante frecuentes al estar trabajando con datos


```r
class(data$afp)
```

```
## [1] "logical"
```


```
## Empleador cotiza en prevision (AFP) (x) <lgl> 
## # total N=71935 valid N=19976 mean=0.82 sd=0.38
## 
## Value |     N | Raw % | Valid % | Cum. %
## ----------------------------------------
## FALSE |  3505 |  4.87 |   17.55 |  17.55
## TRUE  | 16471 | 22.90 |   82.45 | 100.00
## <NA>  | 51959 | 72.23 |    <NA> |   <NA>
```

Los datos de clase `logical` presentan tres posibles valores: `TRUE` (verdadero), `FALSE` (falso) y `NA` (valor nulo, ausencia de dato). En este caso, las personas con valor `TRUE` están empleados por alguien que si paga cotizaciones en previsión, mientras que las personas con valor `FALSE` no tienen tal característica

## 4. Resumen

¡Aprendimos los posibles tipos de datos con los cuales nos enfrentaremos a trabajar en R! Como resumen:

- Variables cualitativas: `character`, `Factor` o `dbl + lbl`
- Variables cuantitativas: `numeric`
- Valores lógicos: `logical`

Recuerden que es fundamental que se apoyen en la **documentación anexa** a la hora de trabajar con datos ¡así evitarán calcular el promedio de las religiones que declaran las/os informantes!

## 5. Recursos

- [RMarkdown en Ciencia de Datos - Hadley Whickham](https://es.r4ds.hadley.nz/r-markdown.html)
- [R Markdown](https://rmarkdown.rstudio.com/) 
- [Tutoriales Markdown](https://rmarkdown.rstudio.com/lesson-1.html) 
- [cheatsheets](https://www.rstudio.com/wp-content/uploads/2015/02/rmarkdown-cheatsheet.pdf)
- Para practicar ir a [Tutorial de Markdown](https://www.markdowntutorial.com/es/)
