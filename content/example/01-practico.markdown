---
title: "Introducción al curso de R"
linktitle: "1: Introducción al curso de R"
date: "2021-08-09"
menu:
  example:
    parent: Ejemplos
    weight: 1
type: docs
toc: true
editor_options: 
  chunk_output_type: console
---

## 0. Objetivo del práctico

El objetivo del práctico es introducirnos en Rstudio, debemos recordar que **R es un lenguaje orientado a objetos** y Rstudio es un **Ambiente integrado**, en el cual podemos tener muchas funciones, las cuales veremos a continuación:

## 1. R como calculadora

Debemos recordar que R puede generar múltiples operaciones, una de ellas es realizar diversas operaciones estadísticas



Como primer ejemplo veremos formas de crear objetos, asignandole un número a un vector


```r
x<-5
```

Cómo el objeto queda en el enviroment, después podemos imprimir o llamar al objeto


```r
x
```

```
## [1] 5
```

Podemos crear los objetos que creamos necesarios, esta vez, crearemos un segundo objeto llamado y


```r
y<-10
```

Ahora podemos realizar operaciones con nuestros objetos


```r
x+y
```

```
## [1] 15
```

```r
x-y
```

```
## [1] -5
```

```r
x*y
```

```
## [1] 50
```

```r
x/y
```

```
## [1] 0.5
```

También podemos guardar los resultados como objetos


```r
z<-x^2 
z
```

```
## [1] 25
```

También podemos realizar operaciones lógicas. Para establecer una igualdad usamos doble signo ==


```r
x>y 
```

```
## [1] FALSE
```

```r
15==x+y 
```

```
## [1] TRUE
```

```r
16>x+y
```

```
## [1] TRUE
```

Creación de una variable. "Edad". mediante la función concatenar "c()", podemos crear un objeto que agrupe un conjunto de datos. Para el lenguaje del software esto es un vector, para nosotros una variable, en este caso numérica (numeric): intervalar, continua, cuantitativa.


```r
Edad<- c(18,25,33,38,67,25,35,57,99)
summary(Edad)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   18.00   25.00   35.00   44.11   57.00   99.00
```

```r
table(Edad)
```

```
## Edad
## 18 25 33 35 38 57 67 99 
##  1  2  1  1  1  1  1  1
```

```r
class(Edad)
```

```
## [1] "numeric"
```

También podemos realiar operaciones sobre los vectores


```r
Edad/2
```

```
## [1]  9.0 12.5 16.5 19.0 33.5 12.5 17.5 28.5 49.5
```

```r
Edad-1
```

```
## [1] 17 24 32 37 66 24 34 56 98
```

```r
Edad2<-Edad-1 #y guardar los resultados

Edad/c(1,2)
```

```
## Warning in Edad/c(1, 2): longitud de objeto mayor no es múltiplo de la longitud
## de uno menor
```

```
## [1] 18.0 12.5 33.0 19.0 67.0 12.5 35.0 28.5 99.0
```

Creación de una variable. "Sexo". Se sigue la misma lógica. Variable cualitativa y nominal, dicotómica. Tipo "Character" Categorías: H=Hombre; M=Mujer.


```r
Sexo<-c("H","H","H","M","H","M","M","M")

summary(Sexo)
```

```
##    Length     Class      Mode 
##         8 character character
```

```r
table(Sexo)
```

```
## Sexo
## H M 
## 4 4
```

```r
class (Sexo)
```

```
## [1] "character"
```

También puede expresarse como factor siendo variable dummy (para 1 y 0). 
Variable cualitativa, nominal.


```r
S<-c(1,1,1,0,1,0,0,0,9,9)

#SEXO<-factor(S, levels = c(0,1,9), labels = c("Mujer","Hombre")) #importancia de los errores
SEXO<-factor(S, levels = c(0,1,9), labels = c("Mujer","Hombre","NC"))

summary(SEXO)
```

```
##  Mujer Hombre     NC 
##      4      4      2
```

```r
table(SEXO)
```

```
## SEXO
##  Mujer Hombre     NC 
##      4      4      2
```

Variable Nivel socioecon?mico. Ordinal, cualitativa.
NSE: 1=E, 2=D, 3=C3, 4=C2, 5=C1, 6=AB


```r
P1<-c(1,1,1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,3,3,3,3,3,3,4,4,4,4,5,5,5,6,99,99)

NSE<-factor(P1,levels=c(1,2,3,4,5,6),labels=c("E","D","C3","C2","C1","AB"))
table(NSE)
```

```
## NSE
##  E  D C3 C2 C1 AB 
## 12  7  6  4  3  1
```

```r
summary(NSE) #NA son lo perdidos
```

```
##    E    D   C3   C2   C1   AB NA's 
##   12    7    6    4    3    1    2
```


Podemos seleccionar elementos específicos de los vecotres


```r
NSE[4] #pedimos el cuarto elemento
```

```
## [1] E
## Levels: E D C3 C2 C1 AB
```

```r
NSE[1:9] #los primeros 9
```

```
## [1] E E E E E E E E E
## Levels: E D C3 C2 C1 AB
```

```r
NSE[c(3,4,5,8,24,31)]
```

```
## [1] E  E  E  E  C3 C1
## Levels: E D C3 C2 C1 AB
```

```r
NSE[37]
```

```
## [1] <NA>
## Levels: E D C3 C2 C1 AB
```

```r
NSE[c(T,F)]
```

```
##  [1] E    E    E    E    E    E    D    D    D    D    C3   C3   C3   C2   C2  
## [16] C1   AB   <NA>
## Levels: E D C3 C2 C1 AB
```

```r
NSE[NSE=="AB"]
```

```
## [1] AB   <NA> <NA>
## Levels: E D C3 C2 C1 AB
```

```r
length(NSE)
```

```
## [1] 35
```

```r
class(NSE)
```

```
## [1] "factor"
```

Crear listas


```r
x <- list(u=c(2,3,4), v="abc")
x #el elemento u de la lista es un vector con 3 números, y el elemento v es abc
```

```
## $u
## [1] 2 3 4
## 
## $v
## [1] "abc"
```

ver el elemento u, de la lista x
Pedir elementos


```r
x$u
```

```
## [1] 2 3 4
```

```r
x[[2]]
```

```
## [1] "abc"
```

```r
x[[1]][2]
```

```
## [1] 3
```

```r
str(x) #este comando muestra la estructura de un objeto de manera resumida
```

```
## List of 2
##  $ u: num [1:3] 2 3 4
##  $ v: chr "abc"
```


Crear bases de datos


```r
base<-data.frame(Edad,
           SEXO[1:9],
           NSE[1:9])

base
```

```
##   Edad SEXO.1.9. NSE.1.9.
## 1   18    Hombre        E
## 2   25    Hombre        E
## 3   33    Hombre        E
## 4   38     Mujer        E
## 5   67    Hombre        E
## 6   25     Mujer        E
## 7   35     Mujer        E
## 8   57     Mujer        E
## 9   99        NC        E
```

Ver nombre de las columnas (variables)


```r
colnames(base)
```

```
## [1] "Edad"      "SEXO.1.9." "NSE.1.9."
```

Cambiar nombre de las columnas (variables)


```r
colnames(base)<-c("edad","sexo","nse")
```

Podemos seleccionar variables con el operador $


```r
base$edad #base$variable
```

```
## [1] 18 25 33 38 67 25 35 57 99
```

Tambien podemos usar corchetes base[filas,columnas]


```r
base[5,2]
```

```
## [1] Hombre
## Levels: Mujer Hombre NC
```

```r
base[1:5,2]
```

```
## [1] Hombre Hombre Hombre Mujer  Hombre
## Levels: Mujer Hombre NC
```

```r
base[1:5,c(1,3)]
```

```
##   edad nse
## 1   18   E
## 2   25   E
## 3   33   E
## 4   38   E
## 5   67   E
```

Sobre una columna podemos seleccionar elementos como un un vector


```r
base$edad[1]
```

```
## [1] 18
```

```r
base$edad[base$sexo=="Hombre"] #podemos usar condiciones lógicas
```

```
## [1] 18 25 33 67
```

Podemos aplicar funciones sobre la base y sobre las variables


```r
head(base)  #entrega los primeros elemntos
```

```
##   edad   sexo nse
## 1   18 Hombre   E
## 2   25 Hombre   E
## 3   33 Hombre   E
## 4   38  Mujer   E
## 5   67 Hombre   E
## 6   25  Mujer   E
```

```r
View(base) #Permite ver la base
str(base)
```

```
## 'data.frame':	9 obs. of  3 variables:
##  $ edad: num  18 25 33 38 67 25 35 57 99
##  $ sexo: Factor w/ 3 levels "Mujer","Hombre",..: 2 2 2 1 2 1 1 1 3
##  $ nse : Factor w/ 6 levels "E","D","C3","C2",..: 1 1 1 1 1 1 1 1 1
```

```r
table(base$sexo)
```

```
## 
##  Mujer Hombre     NC 
##      4      4      1
```

```r
help(table)
```

```
## starting httpd help server ... done
```

```r
?table

mean(base$edad)
```

```
## [1] 44.11111
```

Podemos guardar la base de datos


```r
save(base, file = "base.RData") #Se indica primero el objeto a guardar 
                            #y luego el nombre del archivo, entre comillas.
```

limpiar el ambiente de trabajo


```r
remove(Edad) #borrar un objeto particular
remove(list = ls()) #Borrar todo
```




