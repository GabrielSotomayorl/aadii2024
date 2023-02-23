---
title: "Limpieza de datos"
linktitle: "5: Limpieza de datos"
date: "2021-03-12"
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

El objetivo de este práctico es introducir en la manipulacón de la estructura de los datos que posteriormente serán analizados. Ello incluye la selección de variables, el filtrado de filas, la eliminación de valores nulos, entre otros. 

Es así como esta práctica se enmarca en el proceso de **procesamiento de los datos**. Ello significa que **todos** los procedimientos que aprenderemos en esta sesión deben ser incorporados necesariamente en el código de procesamiento, a modo de elaborar un posterior código de análisis lo más limpio posible. 

## 1. Recursos de la práctica

En este práctico utilizaremos la base de datos de la [**Encuesta de Caracterización Socioeconómica (CASEN)**](http://observatorio.ministeriodesarrollosocial.gob.cl/encuesta-casen-en-pandemia-2020), la cual fue procesada en el [Práctico anterior](https://learn-r-uah.netlify.app/example/04-practico/).Recuerden siempre consultar el [**manual/libro de códigos**](http://observatorio.ministeriodesarrollosocial.gob.cl/storage/docs/casen/2020/Libro_de_codigos_Base_de_Datos_Casen_en_Pandemia_2020.pdf) antes de trabajar una base de datos.

## 2. Librerías a utilizar

En este práctico utilizaremos las siguientes librerías

1. `pacman`: este facilita y agiliza la lectura de los paquetes a utilizar en R

2. `tidyverse`: colección de librerías, de la cual utilizaremos `dplyr`

3. `dplyr`: para manipular datos

4. `sjmisc`: para explorar datos

5. `magrittr`: nos permitirá ocupar dos operadores que en R son **muy** utilizados

# Procesamiento de datos

## 1. Cargar librerías

Lo primero es _cargar las librerías_ a utilizar. Para ello, acudimos a la función `p_load` del paquete `pacman`. Asumimos que ya está instalado, así que sólo lo cargaremos con la función `library()`


```r
pacman::p_load(tidyverse, #Universo de librerías para manipular datos
               haven, #Para cargar datos
               dplyr,#Para manipular datos 
               sjmisc,#Para explorar datos
               magrittr) #Para el operador pipeline (%>%)
```

## 2. Importar bases de datos

Cargaremos los datos 




```r
datos <- read_dta("input/Casen en Pandemia 2020 STATA.dta") 
```

En el panel **Environment**, visualizamos los nuevos objetos, que poseen 185.437 observaciones (o filas) y 650 variables (columnas).

## 3. Explorar datos

A continuación, utilizaremos la función `head()` del paquete `base`, para revisar las primeras cinco filas del set de datos.  


```
## # A tibble: 5 x 650
##          folio     o id_pe~1 id_vi~2 region  provin~3 comuna     zona    area   
##          <dbl> <dbl>   <dbl>   <dbl> <dbl+l> <dbl+lb> <dbl+lbl>  <dbl+l> <dbl+l>
## 1 110110010101     1       5  1.10e9 1 [Reg~ 11 [Iqu~ 1101 [Iqu~ 1 [Urb~ 1 [Urb~
## 2 110110010101     2       6  1.10e9 1 [Reg~ 11 [Iqu~ 1101 [Iqu~ 1 [Urb~ 1 [Urb~
## 3 110110010201     2      31  1.10e9 1 [Reg~ 11 [Iqu~ 1101 [Iqu~ 1 [Urb~ 1 [Urb~
## 4 110110010201     1      32  1.10e9 1 [Reg~ 11 [Iqu~ 1101 [Iqu~ 1 [Urb~ 1 [Urb~
## 5 110110010201     3      30  1.10e9 1 [Reg~ 11 [Iqu~ 1101 [Iqu~ 1 [Urb~ 1 [Urb~
## # ... with 641 more variables: segmento <dbl>, estrato <dbl>, cod_upm <dbl>,
## #   hogar <dbl>, p6_p_con <dbl+lbl>, expr <dbl>, expp <dbl>, expc <dbl>,
## #   varstrat <dbl>, varunit <dbl>, fecha_entrev <date>,
## #   metodologia_entrev <dbl+lbl>, tot_hog <dbl>, numviv <dbl>,
## #   informante_idoneo <dbl+lbl>, tel1 <dbl+lbl>, tel2 <dbl+lbl>,
## #   tel3 <dbl+lbl>, tel4 <dbl+lbl>, tel5 <dbl+lbl>, tel6 <dbl+lbl>,
## #   tel7 <dbl+lbl>, tel8 <dbl+lbl>, p0a <dbl+lbl>, p0b <dbl+lbl>, ...
```

Podemos ver diversas variables, tanto numéricas como con números etiquetados (¡esto último es un *clásico* cuando trabajamos con datos en .dta y .sav!)

## 4. Un aspecto clave antes de comenzar: los operadoresAspectos claves antes de comenzar 

Previo a trabajar con la base de datos, debemos repasar el concepto de **operadores**. Estos símbolos no son de uso exclusivo en R ¡probablemente los conoces desde tus cursos de matemática! Ahora bien, no todos tienen el mismo significado que en otros softwares.

Los **operadores** serán símbolos que permitirán, en los distintos procedimientos de procesamiento, simplificar procesos. Por ejemplo, serán útilizados cuando filtremos nuestros datos para personas de ciertas categorías, cuando calculemos variables nuevas (de manera aritmética o condicional) o, simplemente, cuando queramos hacer procesos "concatenados".

#### Operadores relacionales 

Se usan para hacer comparaciones. Cuando en la *Tabla 1* nos referimos a `un valor`, esto refiere tambien a `variables`

| Símbolo  | Función |
|---------:|:--------|
| `<`      |  Un valor es menor que otro |
| `>`      |  Un valor es mayor que otro |
| `==`     |  Un valor es igual que otro [^1] |
| `<=`     |  Un valor es menor o igual que otro |
| `>=`     |  Un valor es mayor o igual que otro |
| `!=`     |  Un valor es distinto o diferente que otro|
| `%in%`   |  Un valor pertenece al conjunto designado [^2] |
| `is.na()`|  El valor es perdido o `NA` |
| `!is.na()`| El valor es distinto de  `NA` |

[^1]: ¡Atención! Fíjate bien que `==` y `=` son distintos. En R `==` es indicar *"igual a"*, mientras que  `=` es *asignar* (sinónimo de `<-`)

[^2]: Este operador es **muy utilizado**, sirve para indicar que algo está dentro de una cadena de valores. 

#### Operadores aritméticos

Realizan operaciones, como la suma, resta, división, entre otros.

| Símbolo  | Función |
|---------:|:--------|
| `+`      |  Suma |
| `-`      |  Resta|
| `*`     |  Multiplicación |
| `/`     |  División |
| `^`     |  Elevado |

#### Operadores de asignación

Hay dos formas de asignar `objetoA <- objetoB` o `objetoA = objetoB`. Ambas implican que lo que se este realizando en el *objetoB* implica que eso va a producir o generar al *objetoA*

#### Operadores booleanos

Describen relaciones **lógicas** o **condicionales**

| Símbolo  | Función |
|---------:|:--------|
| `&`      |  Indica un *y* lógico |
| `|`      |  Indica un *o* lógico |
| `xor()`  |  Excluye la condición  |
| `!`      |  Distinto de ... |
| `any`    |  Ninguna de las condiciones serán utilizadas |
| `all`    |  Todas las condiciones serán ocupadas |


<img src="../img/example/operad.png" width="60%" />
Figura 1: Resumen de operadores

### Operador pipeline %>% 

¡Aquí mucha atención! Este operador `%>%` (llamado `pipe`) no es un operador que esté contenido en las funciones base del lenguaje R. Más bien proviene del paquete `magrittr` de `tidyverse`, y es de los operadores más útiles y utilizados en R.

¿Para qué sirve? pues nos permite concatenar múltiples funciones y procesos. *Imagina que quieres filtrar una base de datos a partir de tramos etarios. Pero no tienes esa variable creada. ¿Que hacer? La respuesta: concatenar el proceso de creación de variables y luego filtrar.* Eso se puede hacer gracias a ` %>% ` (ya mostraremos como utilizar esta herramienta), que por lo demás es muy fácil de ejecutar.

- `Ctrl + shift + M` Para Windows
- `⌘ + shift + M` Para Mac

## 5. Seleccionar variables

Lo primero que tenemos que considerar cuando trabajaremos con datos cuantitativos es tener claro qué análisis deseamos hacer. ¿Qué queremos investigar? ¿qué fenómeno de la realidad social me resulta interesante o relevante de conocer en profundidad? ¿**qué variables me permiten encontrar las respuestas que estoy buscando**? 

Ello permite destacar la estrecha relación existente entre el problema de investigación y el trabajo en R. En específico, **seleccionar sólo las variables que nos interesan** para trabajar facilitará la posterior manipulación de los datos, hará menos confuso el proceso e, incluso ¡hará que nuestros procedimientos sean menos exigentes en términos de memoria!

Aprenderemos dos maneras de seleccionar las variables que necesitamos

### a) Con R `base`

Si bien puede resultar menos cómodo que trabajar con paquetes como `dplyr`, aprender a trabajar con las librerías de R `base` puede ser sumamente útil en ciertas situaciones particulares. 

En este caso, estaremos interactuando con corchetes ("[ ]"). Para especificar a R que trabajaremos con columnas, debemos incorporar una coma luego del primer corchete ("[, ]"). Seleccionemos la primera variable de nuestros datos


```r
datos[, 1]
```


¡Podemos seleccionar más de una variable! Probemos con las variables número uno, dos y tres


```r
datos[, c(1,2,3)]
```


También podemos seleccionar desde la variable X hasta la Y. En este caso, seleccionemos desde la primera a la quinta:


```r
datos[, 1:5]
```



Y, obviamente, podemos utilizar el **nombre** de las variables


```r
datos[, c('sexo', 'metodologia_entrev')]
```



En este último caso, no olvidemos que el nombre de las variables debe ir entre comillas (""), pues R entiende que las palabras son `character`.

### b) Con `select()` de `dplyr`

De manera similar, la función `select()` de `dplyr` facilita el trabajo a la hora de seleccionar variables. Podemos utilizar tanto los nombres de las variables (¡esta vez no son necesarias las comillas!), como su número. Conozcamos la estructura de la función 

Si queremos incluir las variables `variable1`, `variable2` y `variable3`
{{< div "note" >}}
select(datos, variable1, variable2, variable3)
{{< /div >}}

Si queremos excluir anteponemos un menos `-variable1`
{{< div "note" >}}
select(datos, -variable1)
{{< /div >}}

Hay un montón de maneras de usar `select()` ¡vamos a conocerlas!

#### `select()` por indexación

La indexación refiere al orden que tiene la columna o variable dentro de los datos. Imaginemos que queremos seleccionar:


```r
select(datos, 1, 2) # la primera y la segunda columna

select(datos, 1:4) # la primera hasta la cuarta columna

select(datos, c(1, 4, 5)) # la primera, la cuarta y la quinta columna columna
```



#### `select()` por nombre de columna

Si conocemos el nombre de la variable simplemente lo podemos poner y se seleccionará.


```r
select(datos, edad, sexo, o1)
```



También puedo renombrar en el mismo proceso de selección indicando `nuevo_nombre = nombre_original` en el proceso de selección


```r
select(datos, edad, sexo, ocupacion = o1)
```



#### `select()` para reordenar variables

Podemos seleccionar variables y luego indicar que queremos todo el resto de las variables (`everything()`). Solo por una cosa de orden. Esto será útil sobre todo cuando tengamos identificadores


```r
select(datos, id_persona, sexo, edad, everything())
```



#### `select()` con patrones de texto

Podemos seleccionar variables considerando los prefijos, sufijos o partes de *cómo están nombradas las variables*. Independiente de qué tipo de patrón estes buscando, como todo texto y expresión regular en R (y gran parte de los carácteres) este texto debe venir entre **comillas**. Algunas de las funciones que posibilitan este proceso son:

- `starts_with()`: prefijo 
- `ends_with() `:  sufijo
- `contains()` : contiene una cadena de texto literal
- `matches()` : coincide con una expresión regular


```r
select(datos, starts_with("a"), ends_with("preg"))

# También se pueden combinar con operadores logicos

select(datos, starts_with("y1")&ends_with("preg")) 
select(datos, contains("pobre")|contains("vivienda"))
select(datos, matches("pobreza_|vivienda"))
```



#### `select()` y condiciones lógicas

Si combinamos `select()` con `where()` obtendremos algo así como una frase *"seleciona donde"*, ese *donde* responde a una condición que cumple cierta variable. Por ejemplo, queremos seleccionar todas las variables que son carácteres (`is.character`):


```r
select(datos, where(is.character))
```



Luego de la revisión del libro de códigos y la exploración de datos mediante a funciones como `find_var()` de `sjmisc` decidimos trabajar con las siguientes variables.

- `edad`
- `sexo`
- `s13`: previsión de salud
- `tot_per`: número de personas en el hogar
- `ytoth`: ingresos totales del hogar
- `o1`: ocupación
- `y26d_total`: Monto del IFE
- `y26d_hog`: ¿Alguien recibió el IFE?

¡Apliquémos conocimientos!


```r
select(datos, folio, edad, sexo, prev =592, ocupacion = o1, tot_per, ytoth, starts_with("y26d_")&matches("total|hog"), o2, o3, o4, o6)
```



Es una buena práctica trabajar solo con las columnas que utilizaremos para el análisis, principalmente pues disminuye el *uso de memoria*


```r
datos_proc <- select(datos, folio,  edad, sexo, prev = 592, ocupacion = o1, tot_per, ytoth, starts_with("y26d_")&matches("total|hog"), o2, o3, o4, o6)
```

El nuevo objeto posee650 variables (columnas), pero conserva las filas 185.437 (u observaciones) ¿Qué pasa si quiero trabajar con un *subset* de casos? La respuesta es `filter()`

## 6. Filtrar valores

Asimismo, así como no siempre vamos a trabajar con todas las variables que contiene un conjunto de datos en su versión original (¡imaginen trabajar con las más de 500 variables de CASEN al mismo tiempo!), no siempre desearemos trabajar con todas las observaciones que tenemos en los datos. Tal como en el caso de la selección de variables, podemos recurrir tanto a R `base` como a `dplyr` para filtrar nuestros datos.

### a) Con R `base`

En este caso, en lugar de poner la coma junto al primer corchete ("[, ]"), lo haremos antes del segundo ("[ ,]") ¡veamos cómo hacerlo!

Filtramos para sólo presentar a los hombres


```r
datos_proc[datos_proc$sexo == 2,]
```



Incluso es posible incorporar más de un filtro a la vez ¡simplemente ocuparemos los operadores que revisamos al inicio!


```r
datos_proc[datos_proc$sexo == 2 & datos_proc$edad >= 33,]
```



También podemos ocupar el operador `%in%`


```r
datos_proc[datos_proc$prev %in% c(1,3,4),]
```



### b) Con `filter` de `dplyr`

La función `filter()` de `dplyr` escoge o extrae filas basados en sus valores, subdivide un data frame (*subset*), reteniendo todas las filas que satisfacen sus condiciones.

Con `filter()` será esencial el uso de los **operadores** que ya vimos, dado que las observaciones que preservarán en nuestros datos (y aquellas que no), están definidas por condiciones lógicas (relacionales o booleanas)

{{< div "note" >}}
filter(datos, condicion_para filtrar)
Esta condición para filtrar podría ser, por ejemplo
variable1 >= 3
{{< /div >}}

#### `filter` con números

Imaginémos que queremos una base con las personas mayores de 15 años. Pero también que pertenezcan a hogares con menos de 7 personas. 


```r
filter(datos_proc, edad >= 15)
filter(datos_proc, edad >= 15 & tot_per <7)
```



¿Y si quiero filtrar para saber el valor máximo de ingresos (`ytoth`)?


```r
filter(datos_proc, ytoth == max(ytoth))
```

```
## # A tibble: 1 x 13
##         folio  edad sexo    prev    ocupa~1 tot_per  ytoth y26d_~2 y26d_~3 o2   
##         <dbl> <dbl> <dbl+l> <dbl+l> <dbl+l>   <dbl>  <dbl> <dbl+l> <dbl+l> <dbl>
## 1     7.31e11    41 1 [Hom~ 1 [Sis~ 1 [Sí]        1 2.25e8 2 [No]  NA      NA   
## # ... with 3 more variables: o3 <dbl+lbl>, o4 <dbl+lbl>, o6 <dbl+lbl>, and
## #   abbreviated variable names 1: ocupacion, 2: y26d_hog, 3: y26d_total
```

¡Gana \$225.200.000, es Hombre y tiene 41 años! (y vive solo...)


#### `filter()` con caracteres

Si queremos filtrar por la variable `sexo` solo a las mujeres, tengo dos opciones: o solo selecciono a las mujeres (`==`) o excluyo a los hombres (`!=`).

Ahora bien, antes hay que hacer una precisión importante: en los datos `sexo` es una variable que está como `dbl` y `lbl` (número etiquetado), por lo que no es que en la base aparezcan "Mujeres" y "Hombres", sino que 2 y 1.

Por ello, con el siguiente código aparecerá un error.


```r
filter(datos_proc, sexo == "Mujer")
```

```
## Error in `filter()`:
## ! Problem while computing `..1 = sexo == "Mujer"`.
## Caused by error in `vec_equal()`:
## ! Can't combine `..1` <character> and `..2` <double>.
```

Una función **muy muy útil** (sobre todo cuando trabajemos con regresiones) es `as_factor()` que permite conservar los niveles pero definiendo sus categorías de respuesta en base a la etiqueta que traen (el `lbl`) 


```r
datos_proc$sexo <- as_factor(datos_proc$sexo)
```

¡Ahora si funcionará!


```r
filter(datos_proc, sexo == "Mujer")
filter(datos_proc, sexo != "Hombre")
```



{{< div "note" >}}
**Ojo**. R es *sensible* a cómo está escrito el texto. Si pones el mismo código pero sin respetar mayúsuculas y minúsculas el código no funcionará
{{< /div >}}

¡Por último! ¿Cómo se seleccionan dos condiciones en carácter? Con el operador `%in%`


```r
datos_proc$prev <- as_factor(datos_proc$prev)

filter(datos_proc, prev %in% c("Sistema Público FONASA", "ISAPRE") & edad >= 65)
```



## 7. Tratamiento de casos perdidos (`NA`)

Los datos con los que trabajamos suelen incorporar valores nulos (`NA`), casos perdidos ingresados como valores de clase `Logic`. Estos valores no nos entregan información útil para nuestros análisis, y pueden generar problemas al momento de, por ejemplo, calcular _medidas de tendencia central_, u otros procedimientos estadísticos.

Hay diversas maneras de trabajar los valores nulos, tales como realizar procesos de imputación, entre otros. Sin embargo, la más sencilla consiste en **eliminar los valores nulos** que se encuentran presentes en nuestros datos (aunque no recomendable)

Para ello, lo primero es _identificar valores nulos_ en el set de datos en general, o en alguna variable en específico. Para ello, empleamos la función `is.na()`.


```r
is.na(datos_proc) #Revisamos si hay casos perdidos en el total del set de datos 
is.na(datos_proc$ytoth) #Revisamos si hay casos perdidos en Ingresos per cápita
```



Sin embargo, esto no resulta muy práctico para el análisis. Es por ello que emplearemos la función `sum()` para _contar cuántos valores nulos_ hay en el set de datos en general, o en alguna variable en particular. 


```r
sum(is.na(datos_proc)) #Contamos los valores nulos del set de datos en general, que suman un total de 180.148
```

```
## [1] 623037
```

```r
sum(is.na(datos_proc$ytoth)) #Contaremos los valores nulos de la variable Ingresos per cápita, que alcanzan un total de 98
```

```
## [1] 0
```

Una vez identificamos los _valores nulos_, podemos proceder a **eliminarlos** del set de datos. El comando `na.omit()` eliminará _todas las filas_ que presenten casos perdidos. 


```r
nrow(datos_proc)
```

```
## [1] 185437
```

```r
nrow(na.omit(datos_proc)) #Eliminamos las filas con casos perdidos y contamos el nuevo numero de filas
```

```
## [1] 1920
```

¡La próxima sesión aprenderemos a recodificarlas!

## 8. Resumen con procesamiento de las variables

Hasta ahora, solo hemos creado una base de datos que selecciona variables. Ahora nos resta incorporar en un nuevo objeto los cambios que nos parezcan relevantes para la base de datos procesada que utilizaremos en nuestros análisis. 

Como ya conocemos operadores que permiten concatenar procesos ( `%>%`  y `%$%`) este procedimiento será mucho más fácil. 


```r
datos_proc %>% 
 filter(edad >= 15 & tot_per <7) %>%
 select(folio, sexo, edad, ocupacion, ytoth, tot_per, ife = y26d_hog, o2, o3, o4, o6) 
```



¡Ahora que estamos seguras/os sobre-escribimos la base!

```r
datos_proc <- datos_proc %>% 
               filter(edad >= 15 & tot_per <7) %>%
               select(folio, sexo, edad, ocupacion, ytoth, tot_per, ife = y26d_hog, o2, o3, o4, o6)
```

Podemos visualizar la base resultante a partir de `view_df()` de `sjPlot`

```r
sjPlot::view_df(datos_proc, 
                encoding = "latin1")
```

<table style="border-collapse:collapse; border:none;">
<caption>Data frame: datos_proc</caption>
<tr>
<th style="border-bottom:double; font-style:italic; font-weight:normal; padding:0.2cm; text-align:left; vertical-align:top;">ID</th><th style="border-bottom:double; font-style:italic; font-weight:normal; padding:0.2cm; text-align:left; vertical-align:top;">Name</th><th style="border-bottom:double; font-style:italic; font-weight:normal; padding:0.2cm; text-align:left; vertical-align:top;">Label</th><th style="border-bottom:double; font-style:italic; font-weight:normal; padding:0.2cm; text-align:left; vertical-align:top;">Values</th><th style="border-bottom:double; font-style:italic; font-weight:normal; padding:0.2cm; text-align:left; vertical-align:top;">Value Labels</th>
</tr>
<tr>
<td style="padding:0.2cm; text-align:left; vertical-align:top;">1</td>
<td style="padding:0.2cm; text-align:left; vertical-align:top;">folio</td>
<td style="padding:0.2cm; text-align:left; vertical-align:top;">IdentificaciÃ³n hogar (comuna area seg viv hogar)</td>
<td style="padding:0.2cm; text-align:left; vertical-align:top;" colspan="2"><em>range: NA-NA</em></td>
</tr>
<tr>
<td style="padding:0.2cm; text-align:left; vertical-align:top; background-color:#eeeeee">2</td>
<td style="padding:0.2cm; text-align:left; vertical-align:top; background-color:#eeeeee">sexo</td>
<td style="padding:0.2cm; text-align:left; vertical-align:top; background-color:#eeeeee">Sexo</td>
<td style="padding:0.2cm; text-align:left; vertical-align:top; background-color:#eeeeee"></td>
<td style="padding:0.2cm; text-align:left; vertical-align:top; background-color:#eeeeee">Hombre<br>Mujer</td>
</tr>
<tr>
<td style="padding:0.2cm; text-align:left; vertical-align:top;">3</td>
<td style="padding:0.2cm; text-align:left; vertical-align:top;">edad</td>
<td style="padding:0.2cm; text-align:left; vertical-align:top;">Edad</td>
<td style="padding:0.2cm; text-align:left; vertical-align:top;" colspan="2"><em>range: 15-110</em></td>
</tr>
<tr>
<td style="padding:0.2cm; text-align:left; vertical-align:top; background-color:#eeeeee">4</td>
<td style="padding:0.2cm; text-align:left; vertical-align:top; background-color:#eeeeee">ocupacion</td>
<td style="padding:0.2cm; text-align:left; vertical-align:top; background-color:#eeeeee">o1. La semana pasada, Â¿trabajÃ³ al menos una hora?</td>
<td style="padding:0.2cm; text-align:left; vertical-align:top; background-color:#eeeeee">1<br>2</td>
<td style="padding:0.2cm; text-align:left; vertical-align:top; background-color:#eeeeee">SÃ­<br>No</td>
</tr>
<tr>
<td style="padding:0.2cm; text-align:left; vertical-align:top;">5</td>
<td style="padding:0.2cm; text-align:left; vertical-align:top;">ytoth</td>
<td style="padding:0.2cm; text-align:left; vertical-align:top;">Ingreso total del hogar</td>
<td style="padding:0.2cm; text-align:left; vertical-align:top;" colspan="2"><em>range: 0-225200000</em></td>
</tr>
<tr>
<td style="padding:0.2cm; text-align:left; vertical-align:top; background-color:#eeeeee">6</td>
<td style="padding:0.2cm; text-align:left; vertical-align:top; background-color:#eeeeee">tot_per</td>
<td style="padding:0.2cm; text-align:left; vertical-align:top; background-color:#eeeeee">Total de personas en el hogar</td>
<td style="padding:0.2cm; text-align:left; vertical-align:top; background-color:#eeeeee" colspan="2"><em>range: 1-6</em></td>
</tr>
<tr>
<td style="padding:0.2cm; text-align:left; vertical-align:top;">7</td>
<td style="padding:0.2cm; text-align:left; vertical-align:top;">ife</td>
<td style="padding:0.2cm; text-align:left; vertical-align:top;">y26d_hog. Ãšltimos 12 meses, Â¿alguien recibiÃ³<br>Ingreso Familiar de Emergencia?</td>
<td style="padding:0.2cm; text-align:left; vertical-align:top;">1<br>2<br>9</td>
<td style="padding:0.2cm; text-align:left; vertical-align:top;">SÃ­<br>No<br>No sabe</td>
</tr>
<tr>
<td style="padding:0.2cm; text-align:left; vertical-align:top; background-color:#eeeeee">8</td>
<td style="padding:0.2cm; text-align:left; vertical-align:top; background-color:#eeeeee">o2</td>
<td style="padding:0.2cm; text-align:left; vertical-align:top; background-color:#eeeeee">o2. Aunque no trabajÃ³ la semana pasada, Â¿realizÃ³<br>alguna actividad?</td>
<td style="padding:0.2cm; text-align:left; vertical-align:top; background-color:#eeeeee">1<br>2</td>
<td style="padding:0.2cm; text-align:left; vertical-align:top; background-color:#eeeeee">SÃ­<br>No</td>
</tr>
<tr>
<td style="padding:0.2cm; text-align:left; vertical-align:top;">9</td>
<td style="padding:0.2cm; text-align:left; vertical-align:top;">o3</td>
<td style="padding:0.2cm; text-align:left; vertical-align:top;">o3. Aunque no trabajÃ³, Â¿tenÃ­a algÃºn empleo del<br>cual estuvo ausente temporalmente</td>
<td style="padding:0.2cm; text-align:left; vertical-align:top;">1<br>2</td>
<td style="padding:0.2cm; text-align:left; vertical-align:top;">SÃ­<br>No</td>
</tr>
<tr>
<td style="padding:0.2cm; text-align:left; vertical-align:top; background-color:#eeeeee">10</td>
<td style="padding:0.2cm; text-align:left; vertical-align:top; background-color:#eeeeee">o4</td>
<td style="padding:0.2cm; text-align:left; vertical-align:top; background-color:#eeeeee">o4. Â¿Ha trabajado alguna vez?</td>
<td style="padding:0.2cm; text-align:left; vertical-align:top; background-color:#eeeeee">1<br>2</td>
<td style="padding:0.2cm; text-align:left; vertical-align:top; background-color:#eeeeee">SÃ­<br>No</td>
</tr>
<tr>
<td style="padding:0.2cm; text-align:left; vertical-align:top;">11</td>
<td style="padding:0.2cm; text-align:left; vertical-align:top;">o6</td>
<td style="padding:0.2cm; text-align:left; vertical-align:top;">o6. Â¿BuscÃ³ trabajo remunerado o cuenta propia en<br>las Ãºltimas cuatro semanas?</td>
<td style="padding:0.2cm; text-align:left; vertical-align:top;">1<br>2</td>
<td style="padding:0.2cm; text-align:left; vertical-align:top;">SÃ­<br>No</td>
</tr>

</table>

## 9. Unir datos 

Otra de las herramientas fundamentales a la hora de trabajar con la estructura de los datos es unir dos tabulados distintos con los cuales estemos trabajando. 

Dividamos datos_proc en dos dataframes diferentes, que luego unificaremos


```r
proc_1 <- datos_proc %>% select(folio, sexo, ocupacion, ytoth)
proc_1 <- proc_1[1:908,] #Seleccionamos la mitad de las filas
proc_2 <- datos_proc %>% select(folio, sexo, edad, tot_per, ife)
proc_2 <- proc_2[909:1816,] #Seleccionamos la mitad de las filas
```

### a) `merge`

Una de las funciones más utilizadas para unificar datos es `merge()` del paquete `base` de R. Esta nos permite unir dos tabulados (data frames) que compartan una columna, ojalá una variable que cumpla el rol de identificador (¡como la columna folio!)


```r
merge <- merge(proc_1, proc_2, #Especificamos data frames a unificar
               by = c("folio", "sexo"), #Especificamos la variable a partir de la cual se realiza la unificación (puede ser más de una, como folio y sexo)
               all = T) #Especificamos que queremos mantener total de filas, sumando las de x (proc_1) e y (proc_2)

head(merge)
```

```
##        folio   sexo ocupacion   ytoth edad tot_per ife
## 1 1.1011e+11  Mujer         2  390833   NA      NA  NA
## 2 1.1011e+11 Hombre         1  947583   NA      NA  NA
## 3 1.1011e+11  Mujer         2  947583   NA      NA  NA
## 4 1.1011e+11 Hombre         1 3004167   NA      NA  NA
## 5 1.1011e+11 Hombre         2 3004167   NA      NA  NA
## 6 1.1011e+11  Mujer         2 3004167   NA      NA  NA
```

Podemos ver que ahora el tabulado `merge` tiene 7 columnas, lo cual constituye la suma del número de columnas de los datos iniciales (4 de proc_1 y 5 para comp) menos las columnas compartidas en ambos (folio y sexo). En este caso, dado que no se comparten filas, el número de filas se suma, lo cual resulta en 1.602 + 1.602 = 3.204. 

### b) `bind_cols()`

Otra cosa que podemos hacer es, simplemente, pegar una columna de un dataframe a otro. Para ello recurrimos a la función `bind_cols()` de `dplyr`:


```r
bind_columnas <- bind_cols(proc_1, proc_2)
```

```
## New names:
## * `folio` -> `folio...1`
## * `sexo` -> `sexo...2`
## * `folio` -> `folio...5`
## * `sexo` -> `sexo...6`
```

```r
head(bind_columnas)
```

```
## # A tibble: 6 x 9
##      folio...1 sexo...2 ocupacion   ytoth folio...5 sexo.~1  edad tot_per ife   
##          <dbl> <fct>    <dbl+lbl>   <dbl>     <dbl> <fct>   <dbl>   <dbl> <dbl+>
## 1 110110010101 Mujer    2 [No]     390833   1.10e11 Mujer      50       5 2 [No]
## 2 110110010201 Hombre   1 [Sí]     947583   1.10e11 Mujer      79       5 2 [No]
## 3 110110010201 Mujer    2 [No]     947583   1.10e11 Hombre     53       5 2 [No]
## 4 110110010301 Hombre   1 [Sí]    3004167   1.10e11 Mujer      70       1 2 [No]
## 5 110110010301 Hombre   2 [No]    3004167   1.10e11 Hombre     16       5 2 [No]
## 6 110110010301 Mujer    2 [No]    3004167   1.10e11 Mujer      46       5 2 [No]
## # ... with abbreviated variable name 1: sexo...6
```

Vemos que simplemente pegó  `a` y `b`. Eso implica que tenemos columnas repetidas (a saber, folio y sexo). Cuando estemos seguras/os de que hay columnas repetidas entre dos dataframes que queramos unir, emplearemos `merge()` mientras que, si estos no comparten ninguna columna, recurrimos a `bing_cols()`.

### c) `bind_rows()`

También podemos unir dos dataframes a partir de sus filas, con la función `bind_rows()` de `dplyr`


```r
bind_filas <- bind_rows(proc_1, proc_2)

head(bind_filas)
```

```
## # A tibble: 6 x 7
##          folio sexo   ocupacion   ytoth  edad tot_per ife      
##          <dbl> <fct>  <dbl+lbl>   <dbl> <dbl>   <dbl> <dbl+lbl>
## 1 110110010101 Mujer  2 [No]     390833    NA      NA NA       
## 2 110110010201 Hombre 1 [Sí]     947583    NA      NA NA       
## 3 110110010201 Mujer  2 [No]     947583    NA      NA NA       
## 4 110110010301 Hombre 1 [Sí]    3004167    NA      NA NA       
## 5 110110010301 Hombre 2 [No]    3004167    NA      NA NA       
## 6 110110010301 Mujer  2 [No]    3004167    NA      NA NA
```

`bind_rows()` nos permite pegar filas, independientemente si ambos dataframes comparten las mismas columnas. En caso de que ambos dataframes no compartan alguna columna, esta función rellenará con valores nulos (`NA`). En este caso, la unión es perfecta en tanto proc_1 y proc_2 se conformaron a partir del mismo data frame. 

## 10. Guardar base procesada

Para guardar la base de datos procesada, debes dirigir la ruta desde tu RProject. En este caso, guardaremos datos_proc


```r
saveRDS(datos_proc, file = "output/datos_proc.rds")
```

# Resumen

En ete práctico 

1. Conocimos los **operadores booleanos, de asignación y condicionales**
1. Aprendimos a seleccionar variables con R `base` y `dplyr`
1. Aprendimos a filtrar valores con R `base` y `dplyr`
1. Aprendimos a concatenar procesos con el operador `pipe (%>%)` 
1. Unificar datos (´merge()´, ´bind_cols()´ y ´bind_rows()´)
