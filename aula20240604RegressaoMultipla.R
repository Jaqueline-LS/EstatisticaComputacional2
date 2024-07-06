

set.seed(666)
c1<-rbinom(1,size=1,prob = 0.95)
c1
c2<-rbinom(1,size=1,prob = 0.95)
c2
c3<-rbinom(1,size=1,prob = 0.95)
c3
c4<-rbinom(1,size=1,prob = 0.95)
c4
c5<-rbinom(1,size=1,prob = 0.95)
c5

chaves<-c(c1,c2,c3,c4,c5)
p<-0.9

chaves<-sample(0:1,size=5, prob=c(1-p,p), replace = T)

if(c3==1)
{
  S1<- c1+c2 - c1*c2
  S2<- c4+c5 - c4*c5
  S1*S2
}else{
  c1*c2*c4*c5
}

sistemaFunciona<-function(p)
{
  c1<-rbinom(1,size=1,prob = p)
  c2<-rbinom(1,size=1,prob = p)
  c3<-rbinom(1,size=1,prob = p)
  c4<-rbinom(1,size=1,prob = p)
  c5<-rbinom(1,size=1,prob = p)
  if(c3==1)
  {
    S1<- c1+c2 - c1*c2
    S2<- c4+c5 - c4*c5
    return(S1*S2)
  }else{
    return(c1*c2*c4*c5)
  }
}
sistemas<-replicate(10000,sistemaFunciona(0.9))
mean(sistemas)

# Exercício 3 da lista

Nmin<-function()
{
  u<-runif(1)
  soma<-u
  n<-1
  while(soma<1)
  {
    u<-runif(1)
    soma<-soma+u
    n<-n+1
  }
  return(n)
}
N<-replicate(100,Nmin())
mean(N)

N<-replicate(1000,Nmin())
mean(N)

N<-replicate(10000,Nmin())
mean(N)


#--------------------------


# Regressão Linear Multipla
set.seed(666)
x1.val<-seq(14,86,length.out=100)
x2.val<-seq(-56,456, length.out=100)

x1<-sample(x1.val, size=25, replace = F)
x2<-sample(x2.val, size=25, replace = F)

erro<-rnorm(25,mean = 0, sd=16)

b0<-150
b1<-(-4)
b2<-2.5

y<-b0+b1*x1+b2*x2 + erro


ml1<-lm(y~x1+x2)
ml1$coefficients




simulaRegressao<-function(x)
{
  b0<-150
  b1<-(-4)
  b2<-2.5
  x1<-sample(x1.val, size=25, replace = F)
  x2<-sample(x2.val, size=25, replace = F)
  erro<-rnorm(25,mean = 0, sd=16)
  y<- b0+b1*x1+b2*x2 + erro
  ml1<-lm(y~x1+x2)
  return(c(b=ml1$coefficients, sigma2=summary(ml1)$sigma))
}
matriz<-replicate(1000, expr = simulaRegressao())

matriz<-t(matriz)
colMeans(matriz)
str(matriz)
b0<-matriz[,1]
b1<-matriz[,2]
b2<-matriz[,3]
sigma2<-matriz[,4]

b0<-150
b1<-(-4)
b2<-2.5

x1<-sample(x1.val, size=50, replace = F)
x2<-sample(x2.val, size=50, replace = F)
erro<-rnorm(50,mean = 0, sd=16)
y<- b0+b1*x1+b2*x2 + erro
ml2<-lm(y~x1)
# O que acontece sem X2
qqnorm(ml2$residuals)
qqline(ml2$residuals)

par(mfrow=c(2,2))
plot(ml2)




# Agulha de Buffon

# Gerar uma uniforme entre 0 e 4
# Gera uma uniforme entre 0 e pi









