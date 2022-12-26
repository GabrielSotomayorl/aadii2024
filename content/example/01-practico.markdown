---
title: "Bienvenido/a a R, RStudio y Github"
linktitle: "1: R, RStudio y Github"
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

El objetivo del práctico es introducirnos en herramientas que permiten establecer un flujo de trabajo en R. Para ello, aprenderemos cómo abrir un nuevo **script** y a crear un nuevo **proyecto de R (o Rproject)**. Además, utilizaremos GitHub, que es una plataforma que permite alojar los .Rproj y códigos de cada uno de ustedes. De hecho, tal como mostraremos en el práctico, utilizaremos **GitHub***ClassRoom* para que ustedes puedan subir sus tareas. 

Previo a esto, deben ver el [Tutorial de instalación de R](https://youtu.be/Zj8yL-nx9vM). 

Ahora aprenderemos a crear scripts, proyectos, colaborar y resolver las tareas. 

## 1. Crear un nuevo script (.R)

Para que empecemos a trabajar en el lenguaje R, lo primero es saber cómo abrir un nuevo script; pero *¡¿qué es un script?!* Dicho en términos simples, el script es la *"hoja"* sobre la cual escribiremos y guardaremos cada uno de los códigos que trabajemos cuando estemos trabajando con datos cuantitativos. Si bien podemos ejecutar los códigos directamente desde la **consola**, estos se eliminarán una vez que cerremos la sesión en RStudio:

<img src="/img/example/consola.png" width="60%" />

Así, cuando escribamos en nuestro **script**, podremos archivar los códigos que vayamos ejecutando, para poder reproducirlos posteriormente, compartirlos con otras personas, entre otros. Ahora, lo que nos convoca: **¿cómo puedo abrir un nuevo script?**

Hacerlo es bastante sencillo. En la sección *superior izquierda* de RStudio, debe hacerse click en la hoja con un signo + verde y, luego, seleccionar la opción **R Script** en el menú desplegado: 

<img src="/img/example/open-script.png" width="60%" />

Otra manera de abrir un nuevo script es hacer click en la opción **File** de la *barra superior*

<img src="/img/example/file.png" width="60%" />

para, posteriormente seleccionar **New File > R Script** en los menús desplegados:

<img src="/img/example/new-script.png" width="60%" />

Por último, podemos mantener presionadas las teclas **Ctrl + Shift + N** en Windows, o **⌘ + Shift + N**
en *Mac*.
 
Ya hemos abierto un nuevo script (**¡Genial!**), que se debe ver de la siguiente manera:

<img src="/img/example/script.png" width="60%" />
 
Podemos escribir en él los códigos, que se ejecutarán en la **consola** una vez mantengamos presionadas las teclas **Control + Enter** al inicio de la línea

<img src="/img/example/script-console.png" width="60%" />

Ahora, por supuesto, debemos guardar nuestro script una vez terminemos de trabajar en él, a modo de no perder el trabajo avanzado. Para ello, podemos hacer click en el **disquete** situado en la *barra que se encuentra sobre el código*

<img src="/img/example/save.jpg" width="60%" />

Para luego, elegir el directorio donde deseamos almacenar nuestro script, y asignarle un nombre determinado (en este caso **script1**) que, ojalá, tenga que ver con los procedimientos ejecutados en este (por ejemplo, *procesamiento* o *análisis*). La extensión de los archivos que almacenan scripts es **.R** (*¡no lo olvides!*)

<img src="/img/example/save-script.png" width="60%" />

<img src="/img/example/name-script.png" width="60%" />

Una vez almacenado en una carpeta, cada vez que clickeemos en los casetes o mantengamos presionadas las teclas **Control + S**, el archivo se actualizará a la última versión que hayamos guardado. 

## 2. Crear un nuevo proyecto de R (.Rproj)

¡Todavía nos falta algo para iniciarnos en el flujo de trabajo del curso! debemos crear un nuevo **Proyecto de R o R Project (*.Rproj*)**. ¿Qué es esto, se preguntarán ustedes? en pocas palabras, será la **semilla de nuestra carpeta de trabajo**, es decir, constituye el centro a partir del cual estaremos trabajando el resto de archivos incluidos en nuestro trabajo con los datos, como los *scripts*, los *datos* con los que trabajamos, los *gráficos* que generaremos durante el análisis, y todo aquello vinculado a los procesos que ejecutemos en un trabajo en específico. A lo largo del curso aprenderemos a trabajar adecuadamente con los .Rproj, manteniendo un flujo de trabajo que *facilita la reproductibilidad* de los procesos con los que aprenderemos los contenidos, así como de las tareas que ustedes rendirán durante el semestre. Sin embargo, ahora nos limitaremos a aprender a crear un nuevo proyecto.

¿Cómo lo hacemos? Es bastante sencillo, y se asemeja a la creación de scripts. No obstante, esta vez nos dirigiremos a la sección *superior derecha de RStudio*, donde se encuentra una *R inscrita a un cubo*

<img src="/img/example/rproj.png" width="60%" />

Haremos click en ella, y se desplegará un panel con opciones. Clickearemos en **New Project...**

<img src="/img/example/new-proj.png" width="60%" />

Emergerá una ventana que nos preguntará en qué carpeta queremos alojar nuestro nuevo proyecto. Podemos elegir crear una nueva carpeta haciendo click en **New Directory**, o elegir una carpeta ya existente en nuestro computador, clickeando en **Existing Directory**

<img src="/img/example/new-proj1.png" width="60%" />

Si optamos por lo primero, se nos preguntará qué tipo de proyecto deseamos crear. En este caso, elegiremos **New Project**

<img src="/img/example/new-proj2.png" width="60%" />

Aparecerá una ventana que nos pregunta el nombre que queremos asignarle a la nueva carpeta que crearemos, y en qué carpeta se alojará esta. Si hacemos click en *Browse*, podemos buscar un directorio específico en el cual queramos alojar la nueva carpeta

<img src="/img/example/new-proj3.png" width="60%" />

En caso de elegir lo segundo, se nos solicitará directamente en qué carpeta (ya existente) queremos alojar nuestro proyecto. Una vez más, podemos utilizar *Browse* para navegar entre las carpetas de nuestro computador, hasta hallar aquella en que deseamos alojar este nuevo archivo

<img src="/img/example/new-proj4.png" width="60%" />

¡Listo! ahora sólo debemos ir a nuestra carpeta y buscar el proyecto que hemos creado. Es un archivo de extensión .Rproj, cuyo ícono es una R inscrita en un cubo. 

<img src="/img/example/new-proj5.png" width="60%" />

Es fundamental que, de aquí en adelante, siempre que vayamos a trabajar en alguna tarea con R, **creemos el nuevo proyecto y lo abramos para empezar a trabajar**. Ello setearé nuestro directorio de trabajo directamente en la carpeta donde se encuentra el archivo .Rproj, lo cual nos permitirá trabajar colaborativamente con mayor facilidad (¡algo **fundamental** en el curso!). Además, cada vez que vayamos a retomar nuestro trabajo en algún proyecto, debemos **siempre abrir el proyecto antes que cualquier cosa**, para lo cual debemos hacer doble click sobre su ícono. 

## 3. GitHub

Es un sistema de control de versiones en línea que rastrea los cambios de códigos, facilita la colaboración y el acceso abierto. En este curso será un programa esencial para el desarrollo tanto de clases como prácticos, por lo que vamos a aprender a utilizarlo repasando aspectos básicos. Esto además será profundizado y detallado en clases.


### Ventajas:

Las principales ventajas son:

- Facilita el flujo del trabajo y la colaboración entre usuarios 

- Es de acceso abierto 

- Permite hacer seguimiento a los errores

- Tiene variedades de funciones y plataformas


No obstante, la plataforma central en el curso será **GitHub Classroom**

## 4. Github Classroom

Es una plataforma de Github que facilita la interacción y aprendizaje entre estudiantes y profesores. Utilizaremos esta plataforma a lo largo del curso, principalmente porque permite que las y los estudiantes trabajen individual y colectivamente usando los repositorios de Github. En esta plataforma entregarán sus tareas, recibirán comentarios y retroalimentaciones pudiendo trabajar colaborativamente.

- Pasos para ingresar a Github classroom

1. Crear cuenta: 

2. Dirigirse al [Link](https://n9.cl/954ne ) 

<img src="/img/example/create.png" width="60%" />

3. Clickear en “Crea una cuenta”

4. Introduce tu correo electrónico 

5. Crea una contraseña

6. Introduce el nombre de usuario (similar a su nombre)

7. Clickear en Crear una cuenta

<img src="/img/example/capt.png" width="60%" />

8. Ve al correo electrónico que pusiste anteriormente 

9. Copia el código e introdúcelo en la página


{{< div "note" >}}
Recuerda que el correo que uses debe ser el mismo correo que usaste en pasos anteriores. [Ver tutorial 1](https://youtu.be/9YD-F6-ktes).
{{< /div >}}


## 3. Github Desktop

Esta es una aplicación que permite trabajar los repositorios de Github en los computadores, de forma local e intuitiva, facilitando la coordinación entre las modificaciones realizadas a nivel local y en la plataforma web. Para utilizarlo, debemos

a. Instalar Github Desktop

b. Dirigirse a [Link](https://desktop.github.com/) 

<img src="/img/example/deskdown.png" width="60%" />

c. Crear una carpeta para los contenidos del curso

d. Vincular la dirección de la carpeta

{{< div "note" >}} 

#### Para Windows

   - Clickear en Download for Windows (64bit)

   - Ejecutar el archivo descargado

   - Introduce tu usuario y contraseña

#### Para Mac

   - Clickear en Download for macOS

   - Ejecutar el archivo descargado

   - Introduce tu usuario y contraseña
   
{{< /div >}}


## 4. Botones principales en GitHub Desktop:

- **Clone repository**: Copia un repositorio a tu computador

- **Create new repository**: crea un nuevo repositorio

- **Add existing repository**: incorpora un repositorio existente

<img src="/img/example/con1desk.png" width="60%" />

- **Open the repository in Rstudio**: Abre todo el proyecto en RStudio

- **View the files of your repository in explorer**: Muestra los archivos en la carpeta que se encuentra

- **Open the repository on Github**: abre el repositorio en la página

- **Commit to master**: forma de guardar los cambios

- **Push origin**: manda los cambios al repositorio en línea

<img src="/img/example/con2desk.jpg" width="60%" />


## 5. Conceptos claves de Github

- **Repositorios**: Es donde se alberga el trabajo; una carpeta que contiene todos los archivos y el historial de cambios realizados. Estos se almacenan en la nube.

- **Cloning**: Los repositorios pueden clonarse creando copias locales que extraen toda la información y antiguas versiones del repositorio. Posteriormente podemos modificar los archivos del repositorio clonado, para sincronizar la copia local con la copia en Github. 

- **Commit**: Es la forma de guardar el estado de tu proyecto, como una captura en la que se deja un mensaje informativo.

- **Push**: Esta es la forma en la que se puede incorporar (_subir_) los cambios locales al repositorio de Github.

- **README**: Se utiliza a modo de presentación o introducción de los repositorios.

- **Pull**: Esta es la forma en la que se puede sincronizar (_bajar_) los cambios del repositorio de Github al repositorio local.

#### Flujo de trabajo colectivo y de acceso abierto

- **Forks**: Es otra forma de copiar un repositorio, permitiendo hacer cambios sin afectar al proyecto original.

- **Pull requests**: Es una forma de solicitar que sean incorporados los cambios que has hecho desde un repositorio local, al repositorio original.

<img src="/img/example/concepclav.png" width="60%" />

## 6. GitHub Class Room del curso (learn-R-UAH)

**Pasos para las dinámicas del curso**

### 6.1 Recibir una tarea 

### 6.2 Ingresa el [link](https://classroom.github.com/a/taONcqa7)

  a. El link contiene la tarea, ustedes deben aceptar la tarea la cual creará un _**repositorio**_ que contendrá la *información* y las *instrucciones* de la tarea.

  
<img src="/img/example/ingrestarea.png" width="60%" />
  
  b. Para eso deben ir a _**GitHub Desktop**_, _**clonar** el respositorio desde internet_.
  
<img src="/img/example/paso2tarea.png" width="60%" />
  
  c. Luego deben escoger el repositorio de la tarea, escoger la carpeta en la que se encontrará y clonarlo
  
<img src="/img/example/paso3tarea.png" width="60%" />
  
  d. Después deben abrir el repositorio creado a través de _**RStudio**_, ahí verán el mismo contenido pero en un archivo _**.md**_ 

<img src="/img/example/paso4tarea.png" width="60%" />
  
### 6.3 Resolver una tarea

  a. Ahora resolveremos la **Tarea 0** en conjunto, una vez abierto el [repositorio         clonado](https://classroom.github.com/a/taONcqa7). Recuerden que es en este repositorio donde deben completar y subir la tarea

  b. La tarea se encuentra en un archivo llamado `*README*` con información de los conceptos básicos y el flujo de GitHub, deben leerlo detalladamente.
 
  c. Una vez leido deberán ir al final del archivo en **Tarea 0** y responder las preguntas correspondientes
  
<img src="/img/example/paso5tarea.png" width="60%" />
  
### 6.4 Tarea 0

¡Puedes ver las instrucciones de la [tarea en el siguiente link](/assignment/00-tarea/) o en [GitHub Class Room](https://classroom.github.com/a/taONcqa7)

### 6.5 Subir una tarea

  a. Una vez listo, deberán _**subir**_ la tarea al repositorio remoto a través de GitHub Desktop
  
<img src="/img/example/paso6tarea.png" width="60%" />

### 6.6 Recibir comentarios 

  a. Les llegará un correo con los comentarios y retroalimentación de su tarea
  

## 7. Video tutorial en Youtube

Recuerden que el [video de asociado a este práctico](https://www.youtube.com/watch?v=Zj8yL-nx9vM) y muchos más podrán encontrarlos en el [canal de youtube del curso](https://www.youtube.com/channel/UCqBUeqBttVjS6h8fawK8sWg)

<div class="embed-responsive embed-responsive-16by9">
<iframe class="embed-responsive-item" src="https://www.youtube.com/watch?v=Zj8yL-nx9vM" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>
</div>

