#Continuacao da contagem de corridas aula 09042024
# Box- Muller

source("funcoes.R")

u1<-runif(10)
u2<-runif(10)

z1<-sqrt(-2*log(u1))*cos(2*pi*u2)
z2<-sqrt(-2*log(u1))*sin(2*pi*u2)
Z<-rBM(100) # Duas normais independentes

# z1 e z2 são independentes
#Criar o rBM

fx<-function(x){(x-0.5)^3}
k.inv<-integrate(fx, lower=0, upper = 1)
str(k.inv)
k<-1/k.inv$value
help(D)

# Gerar numeros aleatorios dessa densidade fx

dUser<-function(x,n=2) # fdp da V.A.
{
  (x-0.5)^n
}
pUser<-function(x) # Fda da V.A.
{
  4*((x-0.5)^3 + 1/2)
}
curve(pUser, from=0, to=1)
