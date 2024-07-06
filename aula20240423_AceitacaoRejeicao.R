# Objetivo: Simular F com fdp f
# Distribuição alternativa G, com fdp g ( algoritmo eficiente como gerador de g )

# (1) Gerar Y distribuído como G
# Exemplo
curve(dbeta(x, shape1 = 2, shape2 = 5), from = 0, to=1, col="blue")
moda<-(2-1)/(2+5-2)
c<-dbeta(0.2, shape1 = 2, shape2=5)
# A função G vai ser uma uniforme(0,1)

# (2) Gerar U independente de Y

# (3) Se U <= (f(y)/cg(y)) entao X=Y(aceito)
  #       CC. retornar ao passo 1


hist(amostra, freq=F, breaks=100, ylim=c(0,M))
curve(dbeta(x,shape1=2,shape2=5), from=0, to = 1, col="aquamarine4", add=T)
lines(d$x,M*d$y)

d<-density(amostra)
d$y

set.seed(666)
y<-runif(1)
y
u<-runif(1)
u

0.197<=(dbeta(y, 2,5)/(c*dunif(y)))

gera.Beta<-function(c)
{
  y<-runif(1)
  u<-runif(1)
  while(!u<=(dbeta(y, 2,5)/(c*dunif(y))))
  {
    y<-runif(1)
    u<-runif(1)
  }
  return(y)
}
gera.Beta(c)

gera.Beta2<-function(c)
{
  y<-runif(1)
  u<-runif(1)
  razao<-(dbeta(y, 2,5)/(c*dunif(y)))
  ifelse(u<=razao,y, gera.Beta2(c))
}

amostra<-replicate(10000,gera.Beta2(c))
hist(amostra, freq = F)
curve(dbeta(x, shape1 = 2, shape2 = 5), from = 0, to=1, col="orange", add=T, lwd=2)

objeto<-microbenchmark::microbenchmark(gera.Beta(c),gera.Beta2(c), times=10000)
plot(objeto)


# Quantas rodadas para obter 10000
cont<-1
gera.Beta<-function(c)
{
  y<-runif(1)
  u<-runif(1)
  while(!u<=(dbeta(y, 2,5)/(c*dunif(y))))
  {
    y<-runif(1)
    u<-runif(1)
    cont<-cont+1
  }
  return(list(y,cont))
}
gera.Beta(c)
amostra<-replicate(100,gera.Beta(c))
str(amostra)
hist(as.numeric(amostra[1,]))
contador<-sum(as.numeric(amostra[2,]))
contador

dbeta(0.1,shape1 = 2,shape2=4)
# Verificar o aproveitamento dos pontos com um gráfico
# Plotar todos os pontos discriminando por simbolo os pontos que foram aceitos
# dos que nao foram, circulo aberto os que foram aceitos, cruz para os rejeitados
# y vs. c*g(y), com 10 pontos.


gera.Beta<-function(c)
{
  y<-runif(1)
  u<-runif(1)
  while(!u<=(dbeta(y, 2,5)/(c*dunif(y))))
  {
    y<-runif(1)
    u<-runif(1)
    cont<-cont+1
  }
  return(list(y, c*g(y)))
}




