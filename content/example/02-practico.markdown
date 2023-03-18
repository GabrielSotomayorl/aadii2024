---
title: "2. Uso de bases de datos en R, estadística descritpiva y visualización"
linktitle: "2. Uso de bases de datos en R, estadística descritpiva y visualización"
date: "2021-08-16"
menu:
  example:
    parent: Ejemplos
    weight: 2
type: docs
toc: true
editor_options:
  chunk_output_type: console
---

# 0. Objetivo del práctico

El objetivo de este práctico aprender el uso de funciones y paquetes en R, con el fin de cargar bases de datos, realizar análisis descriptivos y representaciones gráficas básicas.   
Para esto haremos uso de la encuesta [CASEN (2020)](http://observatorio.ministeriodesarrollosocial.gob.cl/encuesta-casen-en-pandemia-2020), la mayor encuesta de hogares realizada en Chile, a cargo del Ministerio de Desarrollo Social, de carácter transversal y multipropósito, es el principal instrumento de medición socioeconómica para el diseño y evaluación de la política social. Permite conocer periódicamente la situación socioeconómica de los hogares y de la población que reside en viviendas particulares, a través de preguntas referidas a composición familiar, educación, salud, vivienda, trabajo e ingresos, entre otros aspectos. 

# 1. Paquetes y funciones en R   
Cuando descargamos e instawlamos R este contiene una cantidad limitada de funciones disponibles, las cuales podemos ampliar mediantes la descarga de distintos paquetes creados por la ocmunidad de usuarios, los cuales nos permitirán, mediante nuevas funciones, ampliar las capacidades del programa, en términos de gestión de bases de datos, análisis estadísticos, visualización de datos, y muchas otras funciones.   

En la siguiente práctica queremos usar la base de datos de CASEN, la cuál está disponibel en formato .sav (SPPS) y .dta (STATA), formatos que no son soportados por la versión base de R. Para poder cargar la base de datos como un objeto en R usaremos el paquete **haven**, el cual incluye funciones para cargar datos a R en diversos formatos.  


```r
# install.packages("haven") 
#este comando nos permite instalar paquetes alojados en CRAN 
library(haven) #Este comando nos permite ejecutar el paquete
```

Una vez cargado el paquete debemos descargar la base de datos y ubicarla en nuestro computador para poder cargarla con la función **read_spss()**. Para esto debemos establecer un **directorio de trabajo**, es decir, la carpeta raíz a partir de la cual R buscará los archivos que queremos cargar en nuestro espacio de trabajo, y donde guardará los output de nuestros análisis.


```r
#La sigueitne función nos permite dentificar el irectorio de trabajo actual
getwd()

#debemos fijar un directorio de trabajo a la carpeta en que tendremos nuestros archivos, por ejemplo:
setwd("C:/Users/Gabriel/Desktop/Directorio R")
# Nota: Debe usarse el simbolo "/" y NO "\"
```


Una vez que hemos fijado nuestro directorio de trabajo podemos cargar nuestra base de datos y asignarla como objeto, para que se guarde en el ambiente  y poder usarla posteriormente en nustros análisis.


```r
casen2020<-read_spss("Casen_en_Pandemia_2020_revisada202209.sav")
```

Una forma alternativa de realziar este proceso es descargar directamente los datos mediante el uso de código. Esto afrece la ventaja de que nos permite ejecutar el código en otros computadores y obtener automáticamente los datos. Un posible problema de esta forma de cargar los datos es que cuando usamos archivos de gran tamaño puede ser muy poco eficiente.  


```r
temp <- tempfile() #Creamos un archivo temporal
download.file("http://observatorio.ministeriodesarrollosocial.gob.cl/storage/docs/casen/2020/Casen_en_Pandemia_2020_revisada202209.sav.zip",temp) #descargamos los datos
casen <- haven::read_sav(unz(temp, "Casen_en_Pandemia_2020_revisada202209.sav")) #cargamos los datos
unlink(temp); remove(temp) #eliminamos el archivo temporal
```

# 2. Estadística Descriptiva  

Para obtener estadísticos descriptivos en R, tales como la media, la mediana o la desviación estándar debemos isar las funciones dedicadas a este 


##  Materiales de la sesión

Recuerden que los archivos asociados a este práctico se pueden descargar aquí:

- [<i class="fas fa-file-archive"></i> `02-class.zip`](https://github.com/learn-R/02-class/raw/main/02-clase.zip) 





## 5. Recursos

- [RMarkdown en Ciencia de Datos - Hadley Whickham](https://es.r4ds.hadley.nz/r-markdown.html)
- [R Markdown](https://rmarkdown.rstudio.com/) 
- [Tutoriales Markdown](https://rmarkdown.rstudio.com/lesson-1.html) 
- [cheatsheets](https://www.rstudio.com/wp-content/uploads/2015/02/rmarkdown-cheatsheet.pdf)
- Para practicar ir a [Tutorial de Markdown](https://www.markdowntutorial.com/es/)
