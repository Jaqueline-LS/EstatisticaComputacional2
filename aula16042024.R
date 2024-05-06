x1<-sort(rep(6:10,1:5), decreasing = T)
x1
y1<-rle(x1)
y1
x2<-c(1,0,0,0,1,0,0,0,0,0,2,0,0)
y2<-rle(x2)
y2


# Alternativa
x3<-c(1,0,0,0,1,2,1,0,0,1,1)
diferencas<-x3[-1]!=x3[-length(x3)]
indice<-c(which(diferencas), length(x3))
diff(indice)



st1<-"aaaabbabbbaabba"
st1.split<-strsplit(st1,"")
str(st1.split)
x4<-st1.split[[1]]
y4<-rle(x4)
y4


# Melhorar o seguinte codigo no forum!!!
gerador.congruencial <- function(x0 = 123456789 ,n = 1,m = 2^31 - 1 ,A = 16807 ,B = 1){
  
  U <- numeric(n)
  
  aux <- x0
  
  for(i in 1:n){
    
    xi <- (aux*A+B)%%m
    
    U[i] <- xi/m
    
    aux <- xi
    
  }
  
  U
  
}

amostra <- gerador.congruencial(n=100)

valor <- amostra[-1]>amostra[-100]
rle(valor)
table(rle(valor)$lengths)



soma <- 0

vet <- numeric(99)

anterior <- valor[1]

for(i in 1:99){
  
  if(valor[i] == anterior){
    
    soma <- soma + 1
    
    vet[i] <- soma
    
  } else {
    
    soma <- 1
    
    vet[i] <- soma
    
  }
  
  anterior <- valor[i]
  
}

table(vet)

########################

(-1)^(1/3)
sign(-1)
raiz1<-(as.complex(-1))^(1/3)
abs(raiz1)
Arg(raiz1)
Arg(raiz1)+(2*pi/3)+(2*pi/3)

raiz2<-(as.complex(-4))^(1/2)
raiz2

raiz3<-(as.complex(4)^(1/2))
raiz3


dUser<-function(x)
{
  ifelse(x>=0 & x<=1, 12*(x-0.5)^2,0)
}
dUser(5)

pUser<-function(x)
{
  ifelse(x>=0 & x<=1,(4*(x-0.5)^3) + 1/2,0)
}
pUser(0.5)

qUser<-function(p)
{
  valor<-p-1/2
  sign(valor)*(abs(valor)/4)**(1/3) + 1/2
}
qUser(0.7)

qUser2<-function(p)
{ 
  x<-1E200
  qUserINT<-function(x,prob)
  {
     (pUser(x)-prob)^2
  }
  
  valor<-optimize(qUserINT,prob=p,maximum = F,lower = -1E200, upper = 1E200)
  
  p<-0.7
  optim(par = 0, qUserINT, prob=p,lower = -Inf, upper = Inf, method = "Brent")
  
  return(valor$minimum)
}

qUser2(0.7)



U1<-runif(1000)
amostra1<-qUser(U1)
hist(amostra1, freq = F)
curve(dUser, from=0, to=1, col="aquamarine3", lty=2, add=T, lwd=2)

U2<-runif(1000)
amostra2<-qUser(U2)

medias<-(amostra1+amostra2)/2
hist(medias, freq = F, breaks = 100)

e1<-function(x) x*dUser(x)
esperanca<-integrate(e1, lower = 0, upper=1)
esperanca$value
e2<-function(x) ((x-1/2)**2)*dUser(x)
var<-integrate(e2, lower=0,upper = 1)
var
erro.padrao<-sqrt(0.15/n)

f1<-function(x) (x**2)-10
curve(f1, from=0, to=3.5)
abline(h=0, v=0, col="grey")
f1.linha<-function(x) 2*x

x0<-3
itera<-function(x.atual)
{
  x.atual-((x.atual**2)-10)/(2*x.atual)
}
x1<-itera(3)
x1

x2<-itera(x1)
x2

x3<-itera(x2)
x3

x4<-itera(x3)
x4
sqrt(10)

hist(medias, freq = F)
curve(dnorm(x,mean=mu, sd=sqrt(sigma2)/n), from=params$a, to=params$b, add=T, col="maroon2", lwd=2)
