gerar.tri<-function()
{
  u<-runif(1)
  if(u<0.5)
  {
    x<-sqrt(2*u)
  }
  else
  {
    x<-2-sqrt(2*(1-u))
  }
  x
}

vet<-replicate(1000, gerar.tri())
hist(vet, freq = F, ylim=c(0,1))

fx<- function(x)
{
  if(x<=1)
  {
    f<-x
  }
  else
  {
    f<-(2-x)
  }
  f
}

fx.vet<-Vectorize(fx)
curve(fx.vet, from=0, to=2, add = T)

Fn<-ecdf(vet)
plot(Fn)

Fx<-function(x)
{
  if(x<=1)
  {
    F.x<-(x^2)/2
  }
  else
  {
    F.x<-(((-x^2)+4*x-2)/2)
  }
  
}
Fx.vet<-Vectorize(Fx)
curve(Fx.vet, from = 0, to=2, add = T, col="blue4", lwd=2)

# Fazendo de forma vetorizada

fx<- function(x)
{
  ifelse(x<=1, x, 2-x)
}
curve(fx, from=0, to=2, add = T)


Fx<-function(x)
{
  ifelse(x<=1,(x^2)/2, (((-x^2)+4*x-2)/2))
}
curve(Fx, from=0, to=2, add = T, col="maroon", lwd=2)


# Lista 1 -----------------
# Geração de 1000 valores
# Comparação histograma
# Comparação ecdf e Fx
# Comparação função de distribuição empírica + teste Kolmogorov
# QQ plot + qqline
# Dividir em 10 intervalos de mesma probabilidade e comparar qtd observada X qtd esperado
# Repetir para 100 intervalos
# Gerar os numeros de maneiras vetorizada
# Outra abordagem para gerar números aleatórios de uma triangular (transformação de variável, prob.2)

u<-runif(1000)
Qx<-function(u)
{
  x<-as.numeric(u<0.5)
  qx<-(sqrt(2*u)^x)*(2-sqrt(2*(1-u))^(1-x))
  return(qx)
}
vet<-Qx(u)
hist(vet)
