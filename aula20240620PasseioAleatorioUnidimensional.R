# Parâmetros do processo
n.traj <- 50
p <- 0.5
n.passos <- 1000

# Semente
# set.seed(666)

# Definição da Matriz do passeio
passeio<- matrix(NA, nrow = n.passos, ncol=n.traj)

# Simulação das trajetorias
for(j in 1:n.traj)
{
  passeio[1,j]<-0
  for(i in 2:n.passos)
  {
    anterior<- passeio[i-1,j]
    passeio[i,j]<-ifelse(runif(1) < p, anterior+1, anterior-1)
  }
}
#cores<-c("#E69F00","#56B4E9","#009E73")
cores<-colours()[1:50]
# Gráfico de trajetoria
matplot(passeio, type="l", lty=1, lwd=2, col=cores, ylim=range(passeio),
        panel.first=grid(), xlab="Passo", ylab="Estado")

# Pontos dos estados

# for(k in 1:n.traj)
# {
#   points(1:n.passos, passeio[,k], pch=16, col=cores[k])
# }

media<-apply(passeio, MARGIN = 1, mean)
maxima<-apply(passeio, MARGIN = 1, max)
minima<-apply(passeio, MARGIN = 1, min)

lines(media, lwd=4, col="maroon2")
lines(maxima, lwd=4, col="maroon2")
lines(minima, lwd=4, col="maroon2")

# ---------------Passeio Aleatorio bidimencional--------------------------

# Parâmetros do processo

n.passos2D <- 1E4

# Especificação da semente
set.seed(607335)

# Matriz do passeio

passeio.bi<- matrix(NA, nrow = n.passos2D, ncol = 2)

# Ponto inicial

passeio.bi[1,]<-c(0,0)
# Cada linha é a coordenada de um passo

# Definição dos passos aleatórios
rpasso <- matrix(c(1,0,-1,0,0,1,0,-1), nrow=4, ncol=2, byrow=T) # Vai sortear essas linhas e somar na coordenada anterior

# Simulação das trajetórias

for( i in 2:n.passos2D)
{
  direcao<-sample(1:4,size=1)
  passeio.bi[i,]<- passeio.bi[i-1,] + rpasso[direcao, ]
}

# Grafico da trajetória
plot(x=passeio.bi[,1], y=passeio.bi[,2], type="l", col="aquamarine3",
     xlim = range(passeio.bi[,1]), ylim=range(passeio.bi[,2]),
     xlab="X", ylab="Y", panel.first = grid())

# Ponto inicial
points(cbind(passeio.bi[1,1], passeio.bi[1,2]), pch=16,
       col="aquamarine4", cex=2)

# Ponto final
points(cbind(passeio.bi[n.passos2D,1], passeio.bi[n.passos2D,2]), pch=16,
       col="maroon2", cex=2)

conta<-numeric(n.passos2D)
for(i in 1:n.passos2D)
{
  conta[i]<-passeio.bi[i,]==c(0,0)
}
sum(conta)


volta<-sapply(1:n.passos2D, FUN=function(x)sum(passeio.bi[x,1]==0 & passeio.bi[x,2]==0))

sum(volta)-1


# Simulação da media de coincidencias de datas de aniversário, quantas coincidencias têm

duplicated(c(1,2,3,1))

