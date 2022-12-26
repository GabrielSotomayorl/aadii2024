---
title: "Muestras complejas y precisión de inferencia estadística"
linktitle: "9: Muestras complejas y precisión de inferencia estadística"
date: "2021-09-16"
menu:
  example:
    parent: Ejemplos
    weight: 9
type: docs
editor_options: 
  chunk_output_type: console
output: 
  html_document: 
    toc: yes
---



## 0. Objetivo del práctico

Este práctico tiene por objetivo introducir el trabajo estadístico con **muestras complejas en R**, es decir, cuando se trabaja con muestras que busquen ser representativas de la población a estudiar, bajo determinados parámetros. Buscaremos calcular estimadores puntuales e intervalos de confianza, que nos permiten reportar el error asociado a la inferencia. 

## 1. Recursos del práctico

El presente práctico será realizado con datos de la *Encuesta de Caracterización Socioeconómica (CASEN)* en su versión 2020. Estos datos ya han sido previamente procesados para su correcta realización, recodificando y eliminando casos perdidos, y renombrando y seleccionando variables. Esto es **muy importante**: para trabajar correctamente con muestreos complejos, **deben** asegurarse de que los datos estén bien preparados para ello. 

Recuerden siempre consultar el [**manual/libro de códigos**](http://observatorio.ministeriodesarrollosocial.gob.cl/storage/docs/casen/2020/Libro_de_codigos_Base_de_Datos_Casen_en_Pandemia_2020.pdf) antes de trabajar una base de datos.

### Material del práctico

- [<i class="fas fa-file-archive"></i> `07-clase.zip`](https://github.com/learn-R/07-class/raw/main/07-clase.zip)


## 2. Librerías a utilizar

Utilizaremos las siguientes librerías:

1. `pacman`: para facilitar y agilizar la lectura de los paquetes a utilizar en R

2. `sjmisc`: para explorar datos y hacer cálculos agrupados

3. `srvyr`: para trabajar con objetos encuesta

4. `survey`: para realizar estimaciones a nivel poblacional en diálogo con `dplyr`

4. `dplyr`: para manipular datos

# Pasos del procesamiento

## 1. Cargar librerías

Cargamos los paquetes anteriormente señalados:


```r
pacman::p_load(tidyverse,
               sjmisc,
               srvyr,
               survey,
               dplyr)
```


## 2. Cargar datos

Como se señaló anteriormente, trabajaremos con datos de CASEN 2020, ya procesados con anterioridad




```r
dim(data)
```

```
## [1] 185339      7
```

Como podemos ver, estos datos tienen 185.339 filas (observaciones), así como 7 columnas. Inspeccionemos: 


```r
head(data)
```

```
## # A tibble: 6 x 7
##     exp varunit varstrat region   pobreza   ing_tot_hog sexo  
##   <dbl>   <dbl>    <dbl> <fct>    <fct>           <dbl> <fct> 
## 1    67       3        1 Tarapacá No pobres      390833 Mujer 
## 2    67       3        1 Tarapacá No pobres      390833 Mujer 
## 3    67       3        1 Tarapacá No pobres      947583 Mujer 
## 4    67       3        1 Tarapacá No pobres      947583 Hombre
## 5    67       3        1 Tarapacá No pobres      947583 Mujer 
## 6    67       3        1 Tarapacá No pobres     3004167 Hombre
```

Tenemos tanto variables numéricas (exp, varunit, varstrat e ing_tot_hog) como factores (region, pobreza y sexo). ¡Explorémoslas!

## 3. Explorar datos

Con `frq()` de `sjmisc`, exploraremos las categorías de las variables factor:


```r
frq(data$region)
```

```
## 
## x <categorical>
## # total N=185339  valid N=185339  mean=8.79  sd=4.23
## 
## Value              |     N | Raw % | Valid % | Cum. %
## -----------------------------------------------------
## Tarapacá           |  8386 |  4.52 |    4.52 |   4.52
## Antofagasta        |  7597 |  4.10 |    4.10 |   8.62
## Atacama            |  7393 |  3.99 |    3.99 |  12.61
## Coquimbo           |  8159 |  4.40 |    4.40 |  17.01
## Valparaíso         | 18501 |  9.98 |    9.98 |  27.00
## OHiggins           | 12346 |  6.66 |    6.66 |  33.66
## Maule              | 12423 |  6.70 |    6.70 |  40.36
## Biobío             | 17669 |  9.53 |    9.53 |  49.89
## La Araucanía       | 11858 |  6.40 |    6.40 |  56.29
## Los Lagos          | 10511 |  5.67 |    5.67 |  61.96
## Aysén              |  4579 |  2.47 |    2.47 |  64.43
## Magallanes         |  5087 |  2.74 |    2.74 |  67.18
## Metropolitana      | 39266 | 21.19 |   21.19 |  88.37
## Los Ríos           |  7992 |  4.31 |    4.31 |  92.68
## Arica y Parinacota |  7224 |  3.90 |    3.90 |  96.57
## Ñuble              |  6348 |  3.43 |    3.43 | 100.00
## <NA>               |     0 |  0.00 |    <NA> |   <NA>
```

```r
frq(data$pobreza)
```

```
## 
## x <categorical>
## # total N=185339  valid N=185339  mean=2.84  sd=0.48
## 
## Value              |      N | Raw % | Valid % | Cum. %
## ------------------------------------------------------
## Pobres extremos    |   8439 |  4.55 |    4.55 |   4.55
## Pobres no extremos |  12891 |  6.96 |    6.96 |  11.51
## No pobres          | 164009 | 88.49 |   88.49 | 100.00
## <NA>               |      0 |  0.00 |    <NA> |   <NA>
```

```r
frq(data$sexo)
```

```
## 
## Sexo (x) <categorical>
## # total N=185339  valid N=185339  mean=1.54  sd=0.50
## 
## Value  |     N | Raw % | Valid % | Cum. %
## -----------------------------------------
## Hombre | 86091 | 46.45 |   46.45 |  46.45
## Mujer  | 99248 | 53.55 |   53.55 | 100.00
## <NA>   |     0 |  0.00 |    <NA> |   <NA>
```

Como podemos ver, *region* provee información sobre la región a la que pertenecen los hogares; *pobreza* nos provee información respecto de la situación de pobreza por ingresos (o no) de los hogares; y *sexo*, información sobre el sexo de la persona que responde el cuestionario. 

Ahora vamos con `descr()` de `sjmisc` para las variables numéricas


```r
descr(data$exp) #Ponderador regional
```

```
## 
## ## Basic descriptive statistics
## 
##  var    type label      n NA.prc   mean     sd   se md trimmed           range
##   dd numeric    dd 185339      0 105.39 157.05 0.36 79   88.54 19042 (4-19046)
##  iqr  skew
##   78 70.57
```

```r
sum(data$exp) #Total de la población
```

```
## [1] 19532480
```

```r
descr(data$varstrat) #Estrato de varianza
```

```
## 
## ## Basic descriptive statistics
## 
##  var    type label      n NA.prc   mean     sd   se  md trimmed       range iqr
##   dd numeric    dd 185339      0 294.34 185.06 0.43 319  298.56 587 (1-588) 346
##   skew
##  -0.22
```

```r
descr(data$varunit) #Conglomerado de varianza
```

```
## 
## ## Basic descriptive statistics
## 
##  var    type label      n NA.prc   mean     sd   se  md trimmed         range
##   dd numeric    dd 185339      0 739.42 439.72 1.02 744  739.27 1493 (1-1494)
##  iqr  skew
##  778 -0.01
```

```r
descr(data$ing_tot_hog)
```

```
## 
## ## Basic descriptive statistics
## 
##  var    type label      n NA.prc    mean      sd      se     md trimmed
##   dd numeric    dd 185339      0 1085076 1574919 3658.26 731009  835756
##                    range    iqr  skew
##  225200000 (0-225200000) 785357 27.45
```

*ing_tot_hog* presenta información respecto de los ingresos totales de los hogares; *exp* es el ponderador regional, cuya suma es el número total de habitantes de la población objetivo; *varstrat* provee información sobre el estrato de varianza; y *varunit* sobre el conglomerado de varianza, es decir, la unidad mínima de observación que, en este caso, son los hogares. Estas últimas tres variables serán **sumamente relevantes** para este práctico. 

## 4. Cálculo con muestras complejas

Una vez explorados nuestros datos, es hora de empezar a realizar nuestros cálculos con muestras complejas. Para ello, lo primero es crear un **objeto encuesta** de modo que, posteriormente, podamos calcular promedios con nuestras variables cuantitativas, y proporciones para nuestras variables categóricas (entre muchos otros estimadores). 

### La función `group_by()`

Antes de empezar a trabajar con objetos encuesta, es fundamental conocer otra de las funciones de `dplyr` que hace posible manipular los datos: `group_by()`. Esta nos permitirá trabajar agrupando los valores a partir de determinadas **columnas**. Calculemos ahora la media de `ing_tot_hog` con la función `mean()`, agrupando por `sexo`:


```r
data %>% 
  group_by(sexo) %>% #Espeficicamos que agruparemos por sexo
  summarise(media = mean(ing_tot_hog)) #Creamos una columna llamada media, calculando la media ingresos con la función `mean`
```

```
## # A tibble: 2 x 2
##   sexo      media
##   <fct>     <dbl>
## 1 Hombre 1125264.
## 2 Mujer  1050215.
```

Es importante destacar que la función `summarise()` (o `summarize()`) de `dplyr` nos permite crear un nuevo data frame con una o más filas para cada combinación y agrupación de variables. En este caso, tenemos dos filas: una para Hombre y otra para Mujer. En caso de no agrupar variables, sólo tendremos una columna que **resuma** las observaciones especificadas. 

Algo interesante es que podemos combinar `group_by()` con algunas funciones de `sjmisc`. Probemos calculando la frecuencia de las diferentes categorías de pobreza, agrupando por sexo:


```r
data %>% 
  group_by(sexo) %>% 
  frq(pobreza)
```

```
## 
## pobreza <categorical>
## # grouped by: Hombre
## # total N=86091  valid N=86091  mean=2.84  sd=0.47
## 
## Value              |     N | Raw % | Valid % | Cum. %
## -----------------------------------------------------
## Pobres extremos    |  3805 |  4.42 |    4.42 |   4.42
## Pobres no extremos |  5807 |  6.75 |    6.75 |  11.16
## No pobres          | 76479 | 88.84 |   88.84 | 100.00
## <NA>               |     0 |  0.00 |    <NA> |   <NA>
## 
## 
## pobreza <categorical>
## # grouped by: Mujer
## # total N=99248  valid N=99248  mean=2.84  sd=0.48
## 
## Value              |     N | Raw % | Valid % | Cum. %
## -----------------------------------------------------
## Pobres extremos    |  4634 |  4.67 |    4.67 |   4.67
## Pobres no extremos |  7084 |  7.14 |    7.14 |  11.81
## No pobres          | 87530 | 88.19 |   88.19 | 100.00
## <NA>               |     0 |  0.00 |    <NA> |   <NA>
```

Obtendremos una tabla por cada una de las categorías de la variable por la cual agrupamos (en este caso, una por cada sexp), que señala las frecuencias absolutas y relativas de cada nivel de pobreza. Por supuesto, también podemos hacer el mismo ejercicio con `descr()`. Probemos haciendo un análisis descriptivos de ingresos para cada sexo:


```r
data %>% 
  group_by(sexo) %>% 
  descr(ing_tot_hog)
```

```
## 
## ## Basic descriptive statistics
## 
## 
## Grouped by: Hombre
## 
##          var    type       label     n NA.prc    mean      sd      se     md
##  ing_tot_hog numeric ing_tot_hog 86091      0 1125264 1718674 5857.53 755156
##   trimmed                   range      iqr  skew
##  864763.9 225200000 (0-225200000) 817406.5 35.86
## 
## 
## Grouped by: Mujer
## 
##          var    type       label     n NA.prc    mean      sd      se     md
##  ing_tot_hog numeric ing_tot_hog 99248      0 1050215 1437734 4563.71 710153
##  trimmed                 range    iqr skew
##   811101 92666667 (0-92666667) 762500 14.2
```

En este caso, obtenemos dos tablas: una por cada `sexo`, cada una indicando las estadísticas de resumen para la variable `ing_tot`

### a) Creación de objeto encuesta (srvyr)

Como ya se señaló, lo primero es crear un **objeto encuesta (survey)**. Este corresponde a una lista (*List*) que nos permitirá realizar los cálculos que deseemos, considerando las varables de diseño (en este caso, varunit, varstrat y exp). Para ello, utilizaremos la función `as_survey_design()` de la librería `srvyr`. Sin embargo, primero crearemos la variable *stratn*, para obtener la cantidad de personas por estrato a nivel poblacional


```r
data <- data %>% 
  group_by(varstrat) %>% #Agrupando por varstrat
  mutate(stratn = sum(exp)) %>%  #Calculamos el total de personas por estrato
  ungroup() #desagrupamos
```

**¡Precaución!** no siempre tendremos la posibilidad de corregir por población finita, por lo cual este paso no es obligatorio. Es fundamental que, antes de trabajar con objetos encuesta, **revisen exhaustivamente la documentación metodológica asociada a sus datos**.

Construimos el objeto encuesta:


```r
casen_regional <- data %>% #Creamos un nuevo objeto llamado casen_regional con la información de data
  as_survey_design(ids = varunit, #Aplicamos diseño muestral, especificando los ids a partir de varunit,
                   strata = varstrat,#los estratos a partir de varstrat,
                   fpc = stratn, #especificando que la estimación es con una población finita
                   weights = exp) #y los ponderadores con exp
```

Fijándonos en nuestro entorno, nos daremos cuenta de que se creó una lista con 9 elementos llamada **casen_regional**. Si bien la información que nos entrega por sí sólo nos resulta algo críptica, este objeto nos permitirá realizar cálculos aplicando las variables de diseño muestral complejo. Algo interesante de los objetos encuesta creados con `srvyr`, es que podemos interactuar con las funciones de `dplyr`, como `group_by()` o `summarise()`. Es decir, **nos permite trabajar con listas como si fueran un data frame** (datos tabulados) incluyendo, por ejemplo, el uso de *metadata*.

![](https://github.com/learn-R/07-class/raw/main/input/img/img-list.png)

*cluster* nos presenta información sobre los conglomerados de varianza, o la unidad mínima de inferencia. *strata*, sobre los estratos de varianza, unidades que integran a los conglomerados de varianza. *prob* y *allprob* proveen información sobre la probabilidad de cada sujeto de ser seleccionado en una muestra al azar. *variables* nos presenta todas las variables incluidas en la lista. Por último, *fpc* provee información sobre los tamaños de la población y la muestra. 

### b) Cálculo de medias para variables numéricas

Una vez creado nuestro objeto encuesta, podemos realizar diversos tipos de cálculos. Calculemos, por ejemplo, la media de ingresos a nivel poblacional, con la función `survey_mean()` de `srvyr`


```r
casen_regional %>% #Con casen_regional
  summarise(ing_medio = survey_mean(ing_tot_hog, na.rm=T)) #Calculamos el ingreso medio poblacional
```

```
## # A tibble: 1 x 2
##   ing_medio ing_medio_se
##       <dbl>        <dbl>
## 1  1156457.       17960.
```

Comparemos con la media calculada a nivel muestral:


```r
data %>% #Con data
  summarise(ing_medio = mean(ing_tot_hog, na.rm=T)) #Calculamos el ingreso medio poblacional
```

```
## # A tibble: 1 x 1
##   ing_medio
##       <dbl>
## 1  1085076.
```

Efectivamente, la estimación a nivel poblacional (o parámetro) es superior a la estimación muestral. Sin embargo, trabajar con muestras complejas siempre significará considerar un posible **error de medición**. El trabajar con muestras representativas (aunque no sean aleatorias) nos permitirá realizar nuestros cálculos, conociendo la probabilidad de equivocarnos en nuestra estimación. Es decir, podemos conocer el **nivel de confianza** (o el **nivel de error**) de nuestras estimaciones, a partir de ciertas características de la muestra.

En virtud de ello, `survey_mean()` también nos permite incorporar los intervalos de confianza, especificando `vartype = "ci"`:


```r
casen_regional %>%#Con casen_regional
  summarise(ing_medio = survey_mean(ing_tot_hog, vartype = "ci", na.rm=T)) #Calculamos el 
```

```
## # A tibble: 1 x 3
##   ing_medio ing_medio_low ing_medio_upp
##       <dbl>         <dbl>         <dbl>
## 1  1156457.      1121220.      1191694.
```

```r
                                                                           #ingreso medio poblacional, 
                                                                           #y sus intervalos de confianza
```

El argumento `level = ` nos permite especificar el nivel de confianza que estamos dispuestas/os a aceptar. Comparemos el cálculo de la media de ingresos a un 95% y a un 99% de confianza


```r
casen_regional %>% #Con casen_regional
  summarise(ing_medio95 = survey_mean(ing_tot_hog, vartype = "ci", level = .95, na.rm=T), #Al 95%
            ing_medio99 = survey_mean(ing_tot_hog, vartype = "ci", level = .99, na.rm=T)) #Al 99%
```

```
## # A tibble: 1 x 6
##   ing_medio95 ing_medio95_low ing_medio95_upp ing_medio99 ing_medio99_low
##         <dbl>           <dbl>           <dbl>       <dbl>           <dbl>
## 1    1156457.        1121220.        1191694.    1156457.        1110121.
## # ... with 1 more variable: ing_medio99_upp <dbl>
```

Trabajar con un mayor nivel de confianza nos permitirá estar más seguras/os de que nuestra estimación incluirá al parámetro poblacional. Sin embargo, esto no es gratis: un mayor nivel de confianza implica una **pérdida de precisión** en nuestros cálculos; y viceversa. Es decir, un mayor nivel de confianza ampliará los límites inferior y superior de nuestros intervalos de confianza, lo cual hace la estimación menos precisa, mientras que un menor nivel de confianza estrechará tales límites, haciendo la estimación más precisa. Sin embargo, en el primer caso, la probabilidad de contener al parámetro poblacional es mayor que en el segundo. La decisión del nivel de confianza con el cual trabajaremos dependerá de qué es lo que queramos calcular, y no es objeto de este curso. 

También es posible utilizar `group_by()` para calcular la media de ingresos (y sus intervalos de confianza) según sexo:


```r
casen_regional %>% #Con casen_regional
  group_by(sexo) %>% #Agrupamos por sexo
  summarise(ing_medio = survey_mean(ing_tot_hog, vartype = "ci", na.rm=T)) #Calculamos el ingreso medio 
```

```
## # A tibble: 2 x 4
##   sexo   ing_medio ing_medio_low ing_medio_upp
##   <fct>      <dbl>         <dbl>         <dbl>
## 1 Hombre  1199636.      1165434.      1233837.
## 2 Mujer   1119514.      1081919.      1157110.
```

```r
                                                                           #poblacional, y sus intervalos de 
                                                                           #confianza
```

Asimismo, es posible calcular la media de ingresos agrupando por región: 


```r
casen_regional %>% #Con casen_regional
  group_by(region) %>% #Agrupamos por region
  summarise(ing_medio = survey_mean(ing_tot_hog, vartype = "ci", na.rm=T)) #Calculamos el ingreso medio 
```

```
## # A tibble: 16 x 4
##    region             ing_medio ing_medio_low ing_medio_upp
##    <fct>                  <dbl>         <dbl>         <dbl>
##  1 Tarapacá            1141383.      1035448.      1247318.
##  2 Antofagasta         1307063.      1187223.      1426903.
##  3 Atacama             1077501.       995455.      1159547.
##  4 Coquimbo             921887.       858537.       985238.
##  5 Valparaíso          1014265.       952490.      1076040.
##  6 OHiggins             951390.       882157.      1020624.
##  7 Maule                863768.       801938.       925598.
##  8 Biobío               945557.       892262.       998852.
##  9 La Araucanía         836515.       782771.       890260.
## 10 Los Lagos            967411.       902996.      1031826.
## 11 Aysén               1252366.      1152001.      1352732.
## 12 Magallanes          1458963.      1317465.      1600461.
## 13 Metropolitana       1408060.      1330071.      1486049.
## 14 Los Ríos             926921.       841025.      1012816.
## 15 Arica y Parinacota   952832.       897814.      1007851.
## 16 Ñuble                788551.       716981.       860122.
```

```r
                                                                           #poblacional, y sus intervalos de 
                                                                           #confianza
```

¿Y si quisiéramos crear una tabla con una columna por nivel de pobreza, cuyas observaciones nos indiquen la media de ingreso para cada una de ellas? Probemos con el siguiente código


```r
ing_pobreza <- casen_regional %>% 
  group_by(pobreza) %>% 
  summarise(ing_medio = survey_mean(ing_tot_hog, vartype = "ci", na.rm = T)) %>% 
  ungroup()  

ing_pobreza_p <- ing_pobreza %>% 
  mutate('Pobres extremos' = c(.$ing_medio[1], .$ing_medio_low[1], .$ing_medio_upp[1]), # Extraemos los valores correspondientes a la primera fila en cada una de nuestras variables
         'Pobres no extremos' = c(.$ing_medio[2], .$ing_medio_low[2], .$ing_medio_upp[2]), # Extraemos los valores correspondientes a la segunda fila en cada una de nuestras variables
         'No pobres' = c(.$ing_medio[3], .$ing_medio_low[3], .$ing_medio_upp[3])) %>% # Extraemos los valores correspondientes a la tercera fila en cada una de nuestras variables
  select('Pobres extremos', 'Pobres no extremos', 'No pobres')

head(ing_pobreza_p) #Visualizamos
```

```
## # A tibble: 3 x 3
##   `Pobres extremos` `Pobres no extremos` `No pobres`
##               <dbl>                <dbl>       <dbl>
## 1           163743.              344812.    1263508.
## 2           155589.              337494.    1224884.
## 3           171896.              352130.    1302132.
```

### c) Cálculo de proporciones para variables categóricas

También podemos utilizar la función `survey_prop()` de `srvyr` para calcular proporciones. Sin embargo, el código para calcular proporciones de variables categóricas **no se construye de la misma manera** que el código para calcular medias de variables continuas:


```r
casen_regional %>% #Con casen_regional
  summarise(prop = survey_prop(pobreza, na.rm = T)) #Y calculamos las proporciones
```

Debemos recurrir a `group_by()` para especificar nuestra variable categórica, y luego realizar el cálculo en `summarise()`


```r
casen_regional %>% #Con casen_regional
  group_by(pobreza) %>% #Agrupamos por pobreza
  summarise(prop = survey_prop(na.rm = T)) #Y calculamos las proporciones
```

```
## # A tibble: 3 x 3
##   pobreza              prop prop_se
##   <fct>               <dbl>   <dbl>
## 1 Pobres extremos    0.0426 0.00125
## 2 Pobres no extremos 0.0656 0.00162
## 3 No pobres          0.892  0.00215
```

De todos modos, hay que considerar que `survey_prop()` y `survey_mean()` son (hasta cierto punto) intercambiables:


```r
casen_regional %>% #Con casen_regional
  group_by(pobreza) %>% #Agrupamos por pobreza
  summarise(prop = survey_mean(na.rm = T)) #Y calculamos las proporciones
```

```
## # A tibble: 3 x 3
##   pobreza              prop prop_se
##   <fct>               <dbl>   <dbl>
## 1 Pobres extremos    0.0426 0.00125
## 2 Pobres no extremos 0.0656 0.00162
## 3 No pobres          0.892  0.00215
```

Muchas veces nos interesa más trabajar con porcentajes que con proporciones. Con `mutate()` podemos multiplicar por 100 nuestras proporciones, y así obtener los porcentajes deseados


```r
casen_regional %>% #Con casen_regional
  group_by(pobreza) %>% #Agrupamos por pobreza
  summarise(prop = survey_prop(na.rm = T))%>% #Calculamos las proporciones
  mutate(per = prop*100) #Creamos una nueva columna multiplicando las proporciones *100 para obtener porcentajes
```

```
## # A tibble: 3 x 4
##   pobreza              prop prop_se   per
##   <fct>               <dbl>   <dbl> <dbl>
## 1 Pobres extremos    0.0426 0.00125  4.26
## 2 Pobres no extremos 0.0656 0.00162  6.56
## 3 No pobres          0.892  0.00215 89.2
```

La función `survey_total()` de `srvyr` nos permite calcular el total de personas en cada categoría a **nivel poblacional**.


```r
casen_regional %>% #Con casen_regional
  group_by(pobreza) %>% #Agrupamos por pobreza
  summarise(prop = survey_prop(na.rm = T), #Calculamos las proporciones
            total = survey_total(na.rm=T))%>% #Y el total por categorías
  mutate(per = prop*100) #Creamos una nueva columna multiplicando las proporciones *100 para obtener porcentajes
```

```
## # A tibble: 3 x 6
##   pobreza              prop prop_se    total total_se   per
##   <fct>               <dbl>   <dbl>    <dbl>    <dbl> <dbl>
## 1 Pobres extremos    0.0426 0.00125   831232   24534.  4.26
## 2 Pobres no extremos 0.0656 0.00162  1280953   31988.  6.56
## 3 No pobres          0.892  0.00215 17420295  231034. 89.2
```

Por supuesto, también es posible incorporar intervalos de confianza, tal como cuando calculamos medias


```r
casen_regional %>% #Con casen_regional
  group_by(pobreza) %>% #Agrupamos por pobreza
  summarise(prop = survey_prop(vartype = "ci", na.rm = T)) #Incorporamos intervalos de confianza
```

```
## # A tibble: 3 x 4
##   pobreza              prop prop_low prop_upp
##   <fct>               <dbl>    <dbl>    <dbl>
## 1 Pobres extremos    0.0426   0.0401   0.0450
## 2 Pobres no extremos 0.0656   0.0624   0.0688
## 3 No pobres          0.892    0.888    0.896
```

Y, además, podemos convertir en porcentajes nuestros intervalos de confianza, tal como lo hicimos con la estimación puntual


```r
casen_regional %>% #Con casen_regional
  group_by(pobreza) %>% #Agrupamos por pobreza
  summarise(prop = survey_prop(vartype = "ci", na.rm = T)) %>% #Incorporamos intervalos de confianza
  mutate(prop = prop*100, #Multiplicamos las proporciones *100,
         prop_low = prop_low*100, #así como el límite inferior 
         prop_upp = prop_upp*100) #y superior, para obtener porcentajes
```

```
## # A tibble: 3 x 4
##   pobreza             prop prop_low prop_upp
##   <fct>              <dbl>    <dbl>    <dbl>
## 1 Pobres extremos     4.26     4.01     4.50
## 2 Pobres no extremos  6.56     6.24     6.88
## 3 No pobres          89.2     88.8     89.6
```

Incluimos el total


```r
casen_regional %>% #Con casen_regional
  group_by(pobreza) %>% #Agrupamos por pobreza
  summarise(prop = survey_prop(vartype = "ci", na.rm = T), #Calculamos las proporciones con intervalos de confianza
            total = survey_total(vartype = "ci", na.rm=T)) %>% #Así como el total por categoría
  mutate(prop = prop*100, #Multiplicamos las proporciones *100,
         prop_low = prop_low*100, #así como el límite inferior 
         prop_upp = prop_upp*100) #y superior, para obtener porcentajes
```

```
## # A tibble: 3 x 7
##   pobreza             prop prop_low prop_upp    total total_low total_upp
##   <fct>              <dbl>    <dbl>    <dbl>    <dbl>     <dbl>     <dbl>
## 1 Pobres extremos     4.26     4.01     4.50   831232   783097.   879367.
## 2 Pobres no extremos  6.56     6.24     6.88  1280953  1218194.  1343712.
## 3 No pobres          89.2     88.8     89.6  17420295 16967009. 17873581.
```

También podemos cruzar dos variables. Veamos, por ejemplo, cómo se distribuye la pobreza según sexo


```r
casen_regional %>% #Creamos un objeto llamado pobreza_reg con datos de casen_regional
  group_by(pobreza, sexo) %>% #Agrupamos por pobreza y sexo
  summarise(prop = survey_prop(vartype = "ci", na.rm = T), #Calculamos las proporciones con intervalos de confianza
            total = survey_total(vartype = "ci", na.rm=T)) %>% #Así como el total por categoría
  mutate(prop = prop*100)
```

```
## # A tibble: 6 x 8
## # Groups:   pobreza [3]
##   pobreza            sexo    prop prop_low prop_upp   total total_low total_upp
##   <fct>              <fct>  <dbl>    <dbl>    <dbl>   <dbl>     <dbl>     <dbl>
## 1 Pobres extremos    Hombre  45.4    0.442    0.466  377149   353131.   401167.
## 2 Pobres extremos    Mujer   54.6    0.534    0.558  454083   425853.   482313.
## 3 Pobres no extremos Hombre  44.7    0.438    0.457  573200   543298.   603102.
## 4 Pobres no extremos Mujer   55.3    0.543    0.562  707753   670391.   745115.
## 5 No pobres          Hombre  46.2    0.459    0.466 8055786  7856474.  8255098.
## 6 No pobres          Mujer   53.8    0.534    0.541 9364509  9094186.  9634832.
```

¡Presentemos sólo lo que nos interesa! Las frecuencias de cada categoría de pobreza agrupando por sexo


```r
pobreza_sexo <- casen_regional %>% #Creamos un objeto llamado pobreza_reg con datos de casen_regional
  group_by(sexo, pobreza) %>% #Agrupamos por region y pobreza
  summarise(prop = survey_prop(vartype = "ci", na.rm = T), #Calculamos las proporciones con intervalos de confianza
            total = survey_total(vartype = "ci", na.rm=T)) %>% #Así como el total por categoría
  mutate(per = prop*100) %>%  #Multiplicamos las proporciones *100 para obtener porcentajes
  ungroup() #desagrupamos

# ¡Extraigamos los valores que nos interesan!

pobreza_sexo_p <- pobreza_sexo %>% 
  mutate(sexo = c('Hombre', 'Mujer', 0, 0, 0, 0), #Dejamos en el orden deseado y rellenamos con 0 
         'Pobres extremos' = c(.$per[1], .$per[4], 0, 0, 0, 0), # Seleccionamos los valores que nos interesan y rellenamos con 0 
         'Pobres no extremos' = c(.$per[2], .$per[5], 0, 0, 0, 0), # Seleccionamos los valores que nos interesan y rellenamos con 0 
         'No pobres' = c(.$per[3], .$per[6], 0, 0, 0, 0)) %>%  # Seleccionamos los valores que nos interesan y rellenamos con 0 
  select(-c(pobreza, starts_with('prop'), starts_with('total'), per)) %>%  #Seleccionamos sólo las variables que nos interesan
  filter(sexo != 0) # Filtramos todas las filas con 0 de relleno

head(pobreza_sexo_p)
```

```
## # A tibble: 2 x 4
##   sexo   `Pobres extremos` `Pobres no extremos` `No pobres`
##   <chr>              <dbl>                <dbl>       <dbl>
## 1 Hombre              4.19                 6.36        89.4
## 2 Mujer               4.31                 6.72        89.0
```

## 5. Resumen

¡Y eso es todo! En este práctico aprendimos a:

1. Crear un objeto encuesta
2. Realizar cálculos de medias a nivel poblacional para variables continuas
3. Realizar cálculos de proporciones y cantidades a nivel poblacional para variables categóricas
4. Crear una tabla ancha (wide) para presentar nuestros cálculos.

## 6. Reporte de progreso

¡Recuerda rellenar tu [reporte de progreso](https://learn-r.formr.org/). En tu correo electrónico está disponible el código mediante al cuál debes acceder para actualizar tu estado de avance del curso.
