---
title: "Regresiones logísticas"
linktitle: "10: Regresiones lineales y logísticas"
date: "2021-10-25"
start_date: "2021-09-27"
end_date: "2021-11-08"
menu:
  content:
    parent: Contenido
    weight: 10
type: docs
toc: true
bibliography: "../../static/bib/references.bib"
csl: "../../static/bib/chicago-fullnote-bibliography-no-bib.csl"
slides: "10-slides"
---

# Slides

{{% slide-buttons %}}

<ul class="nav nav-tabs" id="slide-tabs" role="tablist">
<li class="nav-item">
<a class="nav-link active" id="repaso-tab" data-toggle="tab" href="#repaso" role="tab" aria-controls="repaso" aria-selected="true">Repaso</a>
</li>
<li class="nav-item">
<a class="nav-link" id="glm-y-svyglm-tab" data-toggle="tab" href="#glm-y-svyglm" role="tab" aria-controls="glm-y-svyglm" aria-selected="false">glm() y svyglm()</a>
</li>
<li class="nav-item">
<a class="nav-link" id="extraer-elementos-tab" data-toggle="tab" href="#extraer-elementos" role="tab" aria-controls="extraer-elementos" aria-selected="false">Extraer elementos</a>
</li>
<li class="nav-item">
<a class="nav-link" id="exponenciación-tab" data-toggle="tab" href="#exponenciación" role="tab" aria-controls="exponenciación" aria-selected="false">Exponenciación</a>
</li>
</ul>

<div id="slide-tabs" class="tab-content">

<div id="repaso" class="tab-pane fade show active" role="tabpanel" aria-labelledby="repaso-tab">

<div class="embed-responsive embed-responsive-16by9">

<iframe class="embed-responsive-item" src="/slides/10-slides.html#1">
</iframe>

</div>

</div>

<div id="glm-y-svyglm" class="tab-pane fade" role="tabpanel" aria-labelledby="glm-y-svyglm-tab">

<div class="embed-responsive embed-responsive-16by9">

<iframe class="embed-responsive-item" src="/slides/10-slides.html#10">
</iframe>

</div>

</div>

<div id="extraer-elementos" class="tab-pane fade" role="tabpanel" aria-labelledby="extraer-elementos-tab">

<div class="embed-responsive embed-responsive-16by9">

<iframe class="embed-responsive-item" src="/slides/10-slides.html#26">
</iframe>

</div>

</div>

<div id="exponenciación" class="tab-pane fade" role="tabpanel" aria-labelledby="exponenciación-tab">

<div class="embed-responsive embed-responsive-16by9">

<iframe class="embed-responsive-item" src="/slides/10-slides.html#26">
</iframe>

</div>

</div>

</div>

{{% div fyi %}}
**¿Cómo ocupar**: Si presionas <kbd>?</kbd> (o <kbd>shift</kbd> + <kbd>/</kbd>) mientras estas viendo las slides, podrás ver los comandos específicos para navegar en ellas.
{{% /div %}}

# Video de la clase

-   [<i class="fas fa-video"></i> `Video de la clase`](https://zoom.us/rec/share/H6L3mr-CE-sMeJRQ4CLua3IT95mPl9ougKki3ndIca2s9JpkNQnCmnoYVVbaIKB4.UrcbRJYeE8F3JVM4?startTime=1635190195000)

-   -   [<i class="fas fa-video"></i> `Video del práctico`](https://zoom.us/rec/share/83hcyjS5wOZ57QhBGiyCsb8wzakRyw6pAcyW0ZzjuKWCpIsHdU6ZsNp56obYnCgW.GI1qpKtgu7_HnDHo?startTime=1635195951000)

# Material de la clase

-   [<i class="fas fa-file-archive"></i> `10.1-clase.zip`](https://github.com/learn-R/10.1-class/raw/main/10.1-clase.zip)

-   [<i class="fas fa-file-archive"></i> `10.2-clase.zip`](https://github.com/learn-R/10.2-class/raw/main/10.2-clase.zip)

-   [<i class="fas fa-laptop-code"></i> Práctico](/example/10.1-practico/)

-   [<i class="fas fa-laptop-code"></i> Práctico](/example/10.2-practico/)

# Tareas

-   Tarea N°5 (entrega viernes a las 23.59)

-   Bonus (entrega miércoles a las 20.00 horas)
