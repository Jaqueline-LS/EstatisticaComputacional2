# Definindo a quantidade de rodadas
rodadas <- 100000

# Definindo a semente
set.seed(666)

# Criando os pontos
xis <- runif(rodadas)
ypsilon <- runif(rodadas)

nx<-10 # Numero de intervalos em X
ny<-10 # Numero de intervalos em Y

# Plotando o gráfico

plot(xis, ypsilon, type = "p", cex = 0.6)
abline(h = (0:nx)/2, v = (0:ny)/ny, lty = 2)

# Categorizando os vetores "xis" e "ypsilon"
# utilizando a funcao cut, de acordo com o
# numero de breaks definido como "nx" e "ny"

intervalos_x <- cut(xis, breaks = nx, labels = 1:nx)
intervalos_y <- cut(ypsilon, breaks = ny, labels = 1:ny)

# Matriz para contar os pontos em cada intervalo

final_cont <- matrix(0, nrow = ny, ncol = nx)

# Contando os pontos em cada intervalo
for (i in 1:nx) {
  for (j in 1:ny) {
    final_cont[j, i] <- sum(intervalos_x==i & intervalos_y==j)
  }
}

# Resultado final
print(final_cont)

