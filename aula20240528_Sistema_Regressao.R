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
  S1 <- c1*c4      # Calcula a indicadora para a primeira parte do sistema funcionar, assumindo que nesse cenário o componente 1 e 4 devem funcionar
  S2 <- c2*c5      # Calcula a indicadora para a segunda parte do sistema funcionar, assumindo que nesse cenário o componente 2 e 5 devem funcionar
  S = S1+S2-S1*S2   # Calcula a indicadora para o sistema como um todo, assumindo que nesse cenário S1 ou S2 devem funcionar
}

set.seed(666)
sistemaFunciona<-function(p)
{
  c1<-rbinom(1,size=1,prob = p)  # Gera indicadora para chave 1 (1- Funciona, 0 - Não Funciona)
  c2<-rbinom(1,size=1,prob = p)
  c3<-rbinom(1,size=1,prob = p)
  c4<-rbinom(1,size=1,prob = p)
  c5<-rbinom(1,size=1,prob = p)
  if(c3==1)
  {
    S1<- c1+c2 - c1*c2  # Calcula a indicadora para a primeira parte do sistema funcionar, assumindo que nesse cenário o componente 1 ou 2 devem funcionar
    S2<- c4+c5 - c4*c5
    return(S1*S2)  # Calcula a indicadora para o sistema como um todo, assumindo que nesse cenário S1 e S2 devem funcionar
  }else{
    S1 <- c1*c4      # Calcula a indicadora para a primeira parte do sistema funcionar, assumindo que nesse cenário o componente 1 e 4 devem funcionar
    S2 <- c2*c5  
    return(S1+S2-S1*S2) # Calcula a indicadora para o sistema como um todo, assumindo que nesse cenário S1 ou S2 devem funcionar
  }
}
sistemas<-replicate(10000,sistemaFunciona(0.9))
mean(sistemas)


# Fazer as contas para calcular a probabilidade do sistema funcionar



2*p^2 + 2*p^3 - 5*p^4 + 2*p^5
# Regressao Linear

set.seed(666)
xis<-seq(14,86,length.out=50)
x<-sample(xis, size=15, replace = F)
e<-rnorm(15, mean=0, sd=10)
y<-150 - 4*x + e
r<-cor(x,y)
sd.y<-sd(y)
sd.x<-sd(x)

plot(y~x)

ml<-lm(y~x)

abline(ml, lty=2, col="aquamarine4", lwd=3)

beta0<-ml$coefficients[1]
beta1<-ml$coefficients[2]
obj<-summary(ml)
obj$sigma
obj$fstatistic

r^2
obj$r.squared
pf(obj$fstatistic[1],obj$fstatistic[2],obj$fstatistic[3], lower.tail = F)

# Para recuperar o p valor 
anova2<-anova(ml)
anova2$`Pr(>F)`

resid.observado<-ml$residuals

par(mfrow=c(2,2))
plot(ml)

qqnorm(ml$residuals)
qqline(ml$residuals)
par(mfrow=c(1,1))

shapiro.test(ml$residuals)


x<-sample(xis, size=15, replace = F)
e<-rnorm(15, mean=0, sd=20)
y<-150 - 4*x + e
r<-cor(x,y)
sd.y<-sd(y)
sd.x<-sd(x)

plot(y~x)

ml<-lm(y~x)

abline(ml, lty=2, col="aquamarine4", lwd=3)

beta0<-ml$coefficients[1]
beta1<-ml$coefficients[2]
obj<-summary(ml)
obj$sigma
obj$fstatistic

r^2
obj$r.squared
pf(obj$fstatistic[1],obj$fstatistic[2],obj$fstatistic[3], lower.tail = F)

# Relação do beta 1
beta1<-r*sd(y)/sd(x)

# Para recuperar o p valor 
anova2<-anova(ml)
anova2$`Pr(>F)`

resid.observado<-ml$residuals

par(mfrow=c(2,2))
plot(ml)

qqnorm(ml$residuals)
qqline(ml$residuals)
par(mfrow=c(1,1))

shapiro.test(ml$residuals)


simulaRegressao<-function(x)
{
  x<-sample(xis, size=15, replace = F)
  e<-rnorm(15, mean=0, sd=20)
  y<-150 - 4*x + e
  modelo<-lm(y~x)
  return(c(b=modelo$coefficients, sigma2=summary(modelo)$sigma^2))
}
matriz<-replicate(1000, expr = simulaRegressao())

matriz<-t(matriz)
str(matriz)

hist(matriz[,2],freq = F)
quantile(matriz[,2], probs = c(0.025,0.975))
mean(matriz[,2])
lines(density(matriz[,2]))



