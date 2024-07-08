---
title: "Trabalho Simulação de Eventos Discretos"
subtitle: "Um problema de reparação"
author: "Jaqueline Lamas da Silva"
date: "2023"
output:
  pdf_document:
    latex_engine: xelatex
    fig_width: 5
    fig_height: 4
    fig_caption: yes
  html_document:
    df_print: paged
toc: yes
line-height: 1.5em
fontsize: 12pt
header-includes:
- \renewcommand{\contentsname}{Sumário}
- \usepackage{fancyhdr}
- \usepackage{fontspec}
- \setmainfont{Arial}
- \usepackage {hyperref}
- \hypersetup {colorlinks = true, pdfnewwindow=false, pdfstartview=FitH}
---
\addtolength{\headheight}{2cm}
\lhead{\includegraphics[height=1.5cm]{../../template_pdf_rmd/logo.png}}
\renewcommand{\headrulewidth}{1pt}


\pagestyle{fancyplain}
\renewcommand{\footrulewidth}{0pt}



# Objetivo
Estimar o valor esperado do tempo de falha do sistema. Quando o sistema vai falhar?

* Variável de tempo: $t$

* Variável de estado: $r$, número de máquinas que estão quebradas no tempo t

**Evento**: Quando uma máquina é concertada ou quando uma máquina quebra.


```r
set.seed(6)
n<-4
s<-3
t<-0
r<-0
t.concerto<-Inf

x<-rexp(n,rate=1)
event.list<-sort(x)

MaquinaEstragou<-function(event.list,r,t)
{
  t<-event.list[1]
  r<-r+1 # falhou
  if(r==s+1)
  {
    return(TempoFalha=t)
  }else{
    x<-rexp(1,rate=1) # tempo em que a substituta vai funcionar
    event.list<-sort(c(event.list[-1],x+t))
    if(r==1)
    {
      y<-rexp(1,rate=2) # tempo de concerto
      t.concerto<-t+y  # quando ela ficou pronta
    }
    if(event.list[1]<t.concerto)
    {
      MaquinaEstragou(event.list,r,t)
    }
    else
    {
      MaquinaPronta(event.list,r,t)
    }
  }

}

MaquinaPronta<-function(event.list, r, t)
{
  t<-t.concerto
  r<-r-1
  if(r>0) #tem alguma máquina quebrada?
  {
    y<-rexp(1,rate=2)
    t.concerto<-t+y # quando ela vai ficar pronta
  }
  if(r==0)
  {
    t.concerto<-Inf
  }
  if(event.list[1]<t.concerto)
  {
    MaquinaEstragou(event.list,r,t)
  }
  else
  {
    MaquinaPronta(event.list,r,t)
  }
}

T.falha<-MaquinaEstragou(event.list,r,s)



x<-replicate(1000,rexp(n,rate=1))
Tempos<-apply(x,MARGIN=2,sort)
str(Tempos)
```

```
##  num [1:4, 1:1000] 0.0358 0.3557 0.7167 1.5641 0.0827 ...
```

```r
Tempos[,1]
```

```
## [1] 0.03579145 0.35569986 0.71672128 1.56411692
```

```r
temposDeFalha<-apply(Tempos, MARGIN = 2, FUN = MaquinaEstragou, r=0 , t=0)
```

