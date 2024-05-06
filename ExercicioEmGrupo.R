# Leptocurtica unimodal

dM2<-function(x, p1=2/3, media1=0, media2=0, sd1=1, sd2=1/10) p1*dnorm(x,media1,sd1)+ (1-p1)*dnorm(x, media2, sd2)

curve(dM2, from=-2, to=2, col="coral2", lwd=2)

rM<-function(n, p1, media1, media2, sd1, sd2) #p1 é a prop da densidade 1
{
  u<-runif(n)
  I<-as.numeric(u<=p1) # 1, distribuição 1 da mistura
  media<-I*media1 + (1-I)*media2
  sd<-I*sd1 + (1-I)*sd2
  return(rnorm(n,media,sd))
}

amostra<-rM(1000, p1=2/3, media1=0, media2=0, sd1=1, sd2=1/10)
hist(amostra, freq = F, breaks = 100)
curve(dM2, from=-2, to=2, col="coral2", lwd=2, add = T)


# Assimétrica Bimodal

dM3<-function(x, p1=3/4, media1=0, media2=3/2, sd1=1, sd2=1/3) p1*dnorm(x,media1,sd1)+ (1-p1)*dnorm(x, media2, sd2)

curve(dM3, from=-3, to=3, col="coral2", lwd=2)

amostra3<-rM(1000,  p1=3/4, media1=0, media2=3/2, sd1=1, sd2=1/3)
hist(amostra3, freq = F, breaks = 100)
curve(dM3, from=-3, to=3, col="coral2", lwd=2, add=T)
