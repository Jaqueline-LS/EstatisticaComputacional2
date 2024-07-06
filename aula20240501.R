pesos<-c(1,1,3)/5
medias<-c(0,1/2,13/12)
desvios<-c(1,2/3,5/9)

dMn<-function(x, w=1, mi=0, dp=1)sum(w*dnorm(x, mi, dp))
dMn(0)
dnorm(0)

dMn(0, pesos, medias, desvios)
dMn(1, pesos, medias, desvios)

dMn(c(0,1),w=pesos, mi=medias, dp=desvios)
dMnorm<-Vectorize(dMn, vectorize.args = "x")
dMnorm(c(0,1),w=pesos, mi=medias, dp=desvios)

curve(dMnorm(x,w=pesos, mi=medias, dp=desvios), from=-3, to=3)

# Numero 4

pesos.4<-c(2,1)/3
medias.4<-0
desvios.4<-c(1,1/10)
dMnorm(c(0,1), pesos.4, medias.4, desvios.4)
curve(dMnorm(x,w=pesos.4, mi=medias.4, dp=desvios.4), from=-3, to=3)

# Geração de números aleatorios
set.seed(666)
u<-runif(10000)
menor.1<-rep(1,1000)
menor.4<-as.numeric(u<=0.4)
menor.2<-as.numeric(u<=0.2)
nivel<-menor.1+menor.4+menor.2
round(cbind(u,menor.1, menor.4, menor.2, nivel),4)
# nivel é o indice 

#rnorm(1,mean = medias[i],sd=desvios)

amostra<-sapply(nivel,FUN=function(x){rnorm(1, mean=medias[x], sd=desvios)})
hist(amostra)


z1<-rnorm(100)
ro<-0.8
z2<-rnorm(100,ro*z1,(1-ro)^2)
plot(z1,z2)

