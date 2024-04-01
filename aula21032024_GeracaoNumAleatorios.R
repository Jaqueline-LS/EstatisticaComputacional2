# Geração de numeros Pseudo-aleatorios
# Algoritmo de box-Miller

u1<-runif(1000)
u2<-runif(1000)
ro<-sqrt(-2*log(u1))
z1<- ro*cos(2*pi*u2)
z2<- ro*sin(2*pi*u2)
hist(z1)
hist(z2)

plot(z1,z2)
cor(z1,z2)

# Teste para a correlação ro=0 vs. ro!=0
# Normal bivariada é uma elipse com a inclinação do eixo maior sendo a correlação

