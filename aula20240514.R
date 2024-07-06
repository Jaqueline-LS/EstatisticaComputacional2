source("aula20240507Bivariada.R")

# b1<- (y-y0)/(x-x0), com um ponto e uma inclinação constroi a reta
# b0<-y0-b1*x0

centroide <- apply(t(amostra), 2, mean)
plot(t(amostra), pch=19, xlim=c(-3,3), ylim=c(-3,3))
points(x=centroide[1], y=centroide[2], pch=15, col="coral3")
abline(0,ro) # O ro da a inclinação da esperança condicional

# y.hat1<-b1*x1 + (y0-b1*x0)
# y.obs1 - y.hat1

f.reta<-function(x,b1=0,x0,y0)
{
  b1*(x-x0)+y0
}

#chute b1=0.5
b0<-f.reta(0,b1=0.5, x0=centroide[1], y0=centroide[2])

abline(b0,b1, lty=2, col="red4", lwd=3)

x1<-t(amostra)[,1]
y1<-t(amostra)[,2]

y.hat<-f.reta(x1, b1=0.5, x0=centroide[1], y0=centroide[2])
diferencas<-y1-y.hat

Q<-sum(diferencas^2)
Q


# Outra inclinação
b0<-f.reta(0,b1=0.6, x0=centroide[1], y0=centroide[2])

abline(b0,b1, lty=2, col="red4", lwd=3)

x1<-t(amostra)[,1]
y1<-t(amostra)[,2]

y.hat<-f.reta(x1, b1=0.6, x0=centroide[1], y0=centroide[2])
diferencas<-y1-y.hat

Q<-sum(diferencas^2)
Q


# Outra inclinação
b0<-f.reta(0,b1=0.7, x0=centroide[1], y0=centroide[2])

abline(b0,b1, lty=2, col="red4", lwd=3)

x1<-t(amostra)[,1]
y1<-t(amostra)[,2]

y.hat<-f.reta(x1, b1=0.7, x0=centroide[1], y0=centroide[2])
diferencas<-y1-y.hat

Q<-sum(diferencas^2)
Q

# Outra inclinação
b0<-f.reta(0,b1=1, x0=centroide[1], y0=centroide[2])

abline(b0,b1, lty=2, col="red4", lwd=3)

x1<-t(amostra)[,1]
y1<-t(amostra)[,2]

y.hat<-f.reta(x1, b1=1, x0=centroide[1], y0=centroide[2])
diferencas<-y1-y.hat

Q<-sum(diferencas^2)
Q

# para 0.8
b0<-f.reta(0,b1=0.8, x0=centroide[1], y0=centroide[2])

abline(b0,b1, lty=2, col="red4", lwd=3)

x1<-t(amostra)[,1]
y1<-t(amostra)[,2]

y.hat<-f.reta(x1, b1=0.8, x0=centroide[1], y0=centroide[2])
diferencas<-y1-y.hat

Q<-sum(diferencas^2)
Q

# 0.9
b0<-f.reta(0,b1=0.9, x0=centroide[1], y0=centroide[2])

abline(b0,b1, lty=2, col="red4", lwd=3)

x1<-t(amostra)[,1]
y1<-t(amostra)[,2]

y.hat<-f.reta(x1, b1=0.9, x0=centroide[1], y0=centroide[2])
diferencas<-y1-y.hat

Q<-sum(diferencas^2)
Q

somaQ<-function(beta1)
{
  y.hat<-f.reta(x1, b1=beta1, x0=centroide[1], y0=centroide[2])
  diferencas<-y1-y.hat
  
  Q<-sum(diferencas^2)
  return(Q)
}

somaQ<-Vectorize(somaQ, vectorize.args = "beta1")

curve(somaQ,from = 0, to=1.5)
curve(somaQ,from = 0.76, to=0.77)
curve(somaQ,from = 0.764, to=0.768)
curve(somaQ,from = 0.766, to=0.767)


result<-optimise(f=somaQ, interval=c(0,1), maximum = FALSE)
result$minimum
result$objective

# Calcular o b0 para o b1 que minimiza somaQ
b0<-f.reta(0, b1=result$minimum, x0=centroide[1], y0=centroide[2])
abline(b0,result$minimum, col="royalblue4", lwd=3, lty=2)

# Reta otima em relação a algum um criterio

r12<-cor(x1,y1)
s1<-sd(x1)
s2<-sd(y1)

r12*s2/s1 # reta de minimos quadrados, b1 = r12*s2/s1

# optim para um parametro method Brent
optim(par=0, fn=somaQ, method = "Brent",lower=-1, upper = 1)


# Com o optim da para deixar a reta livre e otimzar no b0 e no b1

lm(y1~x1)



somaA<-function(beta1)# Desvios absolutos, distancia euclidiana
{
  y.hat<-f.reta(x1, b1=beta1, x0=centroide[1], y0=centroide[2])
  diferencas<-abs(y1-y.hat)
  
  Q<-sum(diferencas)
  return(Q)
}

somaA<-Vectorize(somaA, vectorize.args = "beta1")
curve(somaA, from = 0, to=1.5)

result2<-optimise(f=somaA, interval=c(0,1), maximum = FALSE)
result2$minimum
result2$objective

b0<-f.reta(0, b1=result$minimum, x0=centroide[1], y0=centroide[2])
abline(b0,result$minimum, col="royalblue4", lwd=3, lty=2)
b0<-f.reta(0, b1=result2$minimum, x0=centroide[1], y0=centroide[2])
abline(b0,result2$minimum, col="maroon1", lwd=3, lty=2)

# Modificar a somaQ para otimizar em b1 e b0

# Temos x1 e x2, otimizar a distancia do ponto a reta!( Qual a melhor reta? b1 e b0 )

