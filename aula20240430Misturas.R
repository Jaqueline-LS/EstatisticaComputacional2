rM1<-function()
{
  u<-runif(1)
  ifelse(u<=0.75, rnorm(1,0,1), rnorm(1,1.5,0.25))
}

dM1<-function(x) 0.75*dnorm(x,0,1)+ 0.25*dnorm(x, 1.5, 0.25)


amostra<- replicate(1000,rM1())
hist(amostra, freq=F, ylim = c(0,0.6), breaks = )
curve(dM1(x), from=-2.5, to=2.5, add=T, col="blue", lwd=2)



curve(dM1(x), from=-2.5, to=2.5, lwd=2)
curve(0.75*dnorm(x,0,1), from=-2.5, to=2.5, add=T, col="red", lwd=2)
curve(0.25*dnorm(x,1.5,0.25), from=-2.5, to=2.5, add=T, col="green", lwd=2)



rM1<-function(n)
{
  u<-runif(n)
  ifelse(u<=0.75, rnorm(n,0,1), rnorm(n,1.5,0.25))
}

amostra<- rM1(1000)
hist(amostra, freq=F, ylim = c(0,0.6), breaks =50 )
curve(dM1(x), from=-2.5, to=2.5, add=T, col="blue", lwd=2)
lines(density(amostra), lty=2, col="red", lwd=2)

# Sem o ifelse
rM1<-function(n, p1=0.75 ,media1=0, media2=1.5, sd1=1, sd2=0.25) #p1 é a prop da densidade 1
{
  u<-runif(n)
  I<-as.numeric(u<=p1) # 1, distribuição 1 da mistura
  media<-I*media1 + (1-I)*media2
  sd<-I*sd1 + (1-I)*sd2
  return(rnorm(n,media,sd))
}

dM1<-function(x, p1=0.75, media1=0, media2=1.5, sd1=1, sd2=0.25) p1*dnorm(x,media1,sd1)+ (1-p1)*dnorm(x, media2, sd2)

p1<-0.75
media1<-0
media2<-1.5
sd1<-0
sd2<-0.25

momento1<-p1*media1 + (1-p1)*media2 # Primeiro momento da mistura
momento2<-p1*(media1^2 + sd1^2) + (1-p1)*(media2^2 + sd2^2)

variancia<- momento2 - (momento1)^2

mean(amostra)
var(amostra)

amostra<- rM1(10000)
hist(amostra, freq=F, ylim = c(0,0.6), breaks =50 )
curve(dM1(x), from=-2.5, to=2.5, add=T, col="blue", lwd=2)
lines(density(amostra), lty=2, col="red", lwd=2)


# Misturas 1/5 N(0,1) + 1/5 N(1/2,2/3) + 3/5 N(13/2, 5/9)

# 2/3 N(0,1) + 1/3 N(0,1/10)

# 1/2 N(0,1) + sum(i=0:4)(1/10 N(i/2 - 1), 1/10)

df4<-function(x) (0.5)*dnorm(x,0,1) + (1/10)*dnorm(x, -1, 1/10)  + (1/10)*dnorm(x, -0.5, 1/10) +  (1/10)*dnorm(x, 0, 1/10) + (1/10)*dnorm(x, 0.5, 1/10) + (1/10)*dnorm(x, 1, 1/10)
curve(df4(x), from=-2, to=2)


rM4<-function()
{
  u<-runif(1000)
  
  
}


# 2/3 N(0,1) + 1/3 N(0,1/10)

rM2<-function(n, p1=2/3, media1=0, media2=0, sd1=1, sd2=1/10) #p1 é a prop da densidade 1
{
  u<-runif(n)
  I<-as.numeric(u<=p1) # 1, distribuição 1 da mistura
  media<-I*media1 + (1-I)*media2
  sd<-I*sd1 + (1-I)*sd2
  return(rnorm(n,media,sd))
}

dM2<-function(x, p1=2/3, media1=0, media2=0, sd1=1, sd2=1/10) p1*dnorm(x,media1,sd1)+ (1-p1)*dnorm(x, media2, sd2)
amostra<-rM2(1000)
hist(amostra, freq=F)  
curve(dM2(x), from=-2.5, to=2.5, add=T, col="blue", lwd=2)




