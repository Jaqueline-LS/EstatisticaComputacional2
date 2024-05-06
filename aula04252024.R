dfx<-function(x) 2* exp(-x^2/2)/(sqrt(2*pi)) 
curve(dfx(x), from = 0, to=4)
curve(dexp(x), from = 0, to=4, add=T)

# Envelopar a candidata
# n*exp
m<-2/sqrt(2*pi)*exp(1/2) #m é o valor do máximo, cacula o max da razao da fx e gx
envelope<-function(x) m*dexp(x)
curve(envelope(x), from = 0, to=4, add=T, col="red")

# Para achar o ponto que tangencia, e obter a minima rejeicao
razao<-function(x)(2*dnorm(x))/dexp(x)
max<-optimize(f=razao, interval = c(0,4), maximum =TRUE)
m<-max$objective


rgy<-function(){-log(runif(1))}

cont<-1
gera.fx<-function(m)
{
  u<-runif(1)
  y<-rgy()
  while(!u<=(dfx(y)/(m*dexp(y))))
  {
    u<-runif(1)
    y<-rgy()
    cont<-cont+1
  }
  return(c(y,cont))
}

amostra<-replicate(10000,gera.fx(m))
hist(amostra[1,], freq = F)
curve(dfx(x), from = 0, to=4, add=T)

contador<-sum(amostra[2,])
contador

fn<-ecdf(amostra)
plot(fn)

# F.x<-function(x)2*integrate(dnorm, lower=0, upper=x)$value
# F.x(1)
# curve(F.x, from=0, to=10, add=T, col="red")

# 2*(pnorm(x)-1/2)


curve(2*(pnorm(x)-1/2), from=0, to=10, add=T, col="blue")



### Atividade 3 ###
# Exemplo do introduction to monte carlo methods



# Gerar Beta (2,5), X1, x2 gamma inden
x1<-rgamma(1E3, shape=2, rate=1)
x2<-rgamma(1E3, shape =5, rate=1)

x<-(x1)/(x1+x2)
hist(x, breaks = 30, freq = F)
curve(dbeta(x,2,5), from=0, to=1, col="aquamarine3", add=T, lwd=2)

# Soma de exp independentes é gamma.
n<-10000
soma<-7
matriz<-matrix(runif(n*soma), ncol = soma)
str(matriz)
exponenciais<-(-log(matriz))
str(exponenciais)
x1<-apply(exponenciais[,1:2],1, sum)
str(x1)
x2<-apply(exponenciais[,3:7],1,sum)
str(x2)
x<-(x1)/(x1+x2)
hist(x, freq = F)
curve(dbeta(x,2,5), from=0, to=1, col="aquamarine3", add=T, lwd=2)

#---------------------

# Comparar as três funções de gerar a Beta(2,5)


