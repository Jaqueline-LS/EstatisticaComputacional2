geradorU<-function(m=(2^31-1), A=16807, B=1, seed=123456789,n)
{
  u<-numeric(n)
  anterior<-seed
  for(i in 1:n)
  {
    x<-(anterior*A+B)%%m
    u[i]<-x
    anterior<-x
  }
  return(u/m)
}
amostra<-geradorU(n=1000)

mean(amostra)
var(amostra)

obj.hist<-hist(amostra, freq = F, breaks = 10)
curve(dunif(x),from = 0, to = 1, add = T, col="maroon")
observado<-obj.hist$counts
esperado<-replicate(length(observado),100/length(observado))

x.sq<-sum(((observado-esperado)^2)/esperado)

# Se os valores são iguais, a estatistica T~Qui-qua(100-1), 99
1-pchisq(x.sq, df=99)

# Teste X^2 junto com o histograma

fn<-ecdf(amostra)
plot(fn)
curve(punif(x), from = 0, to = 1, add = T, col= "maroon", lwd=2)


plot(amostra[-1], amostra[-100])

# Teste T
t.test(amostra, mu=0.5, alternative = "two.sided")
# Conclusão, não temos evidências significativas para rejeitar H0


ks.test(amostra, "punif")
# H0: amostra oriunda de distribuição uniforme
# H1: cc
# Não há evidências para rejeitar H0, a amostra parece ser oriunda de uma distribuição uniforme

#Quero um valor esperado de 10 por intervalo, 10 intervalos, aqui intervalor iguais de mesma probabilidade (Uniforme)
amostra.hist<-hist(amostra, breaks = 10, freq = F)
amostra.hist$counts
valor.esperado<-replicate(10,10)

sum(((amostra.hist$counts-valor.esperado)^2)/valor.esperado)

p.valor<-pchisq(8.6,df=9, lower.tail = F)


amostra2<-geradorU(seed=666666666, n=100)
amostra2.hist<-hist(amostra2, breaks = 10, freq = F)
amostra2.hist$counts

sum(((amostra2.hist$counts-valor.esperado)^2)/valor.esperado)
p.valor<-pchisq(9.6,df=9, lower.tail = F)


x<-amostra[2:100] # AMOSTRA[-1]
y<-amostra[1:99] # AMOSTRA[-100]
plot(x,y)
abline(h=c(1:10)*0.1, v=(1:10)*0.1, col="pink", lty=2)

amostra3<-geradorU(n=1001)
x<-amostra3[-1] # AMOSTRA[-1]
y<-amostra3[-1000] # AMOSTRA[-100]
plot(x,y)
abline(h=c(1:10)*0.1, v=(1:10)*0.1, col="pink", lty=2)

nx<-10
ny<-10
intervalos_x <- cut(amostra3[-1], breaks = nx, labels = 1:nx)
intervalos_y <- cut(amostra3[-1000], breaks = ny, labels = 1:ny)

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

categorias.x<-cut(amostra3[-1],breaks = (0:10)*0.1, labels=paste0("X",1:10))
sum(table(categorias.x))

categorias.y<-cut(amostra3[-1001],breaks = (0:10)*0.1, labels=paste0("Y",1:10))
sum(table(categorias.y))

categorias.quadrado<-(paste0(categorias.x,"-",categorias.y))
tab<-table(categorias.quadrado)
x.2<-sum(((tab-10)^2)/10)
p.valor<-pchisq(x.2,df=99, lower.tail = F)

amostra4<-geradorU(m=10,A=103,B=17,seed=2,n=100)
# a cada 4 valores a sequencia se repete
# é desejável que os ciclos sejam longos

amostra3<-geradorU(n=1E3)
length(unique(amostra3))

diferencas.1<-diff(amostra, lag = 1)
I.p<-(diferencas.1>0)
I.p<-as.numeric(I.p)
I.n<-1-I.p

# Sequencias de valores 
valor<-amostra[-1]>amostra[-1000]
vetor<-as.numeric(1000)
soma<-0
for(i in 1:99)
{
  if(valor[i]==0)
  {
    vetor[i]<-0
    soma<-0
  }
  else
  {
    soma<-soma+1
    vetor[i]<-soma
  }
}
vetor

table(vetor)
max(vetor)
plot(valor)



#-------------

Rr<-function(N=100,r)
{
  Rr<-((N+1)*(r^2 + r -1) - (r+2)*(r^2-r-1))/(factorial(r+2))
}

corridas<-c(1:10)
valoresEsperados<-Rr(r=corridas)



# ------- Quantas corridas de tamanho n

amostra<-geradorU(n=100)


valor<-amostra[-1]>amostra[-100]
vetor<-as.numeric(99)
soma<-0
anterior<-valor[1]

for(i in 1:99)
{
  if(valor[i]==anterior)
  {
    soma<-soma+1
    vetor[i]<-soma
  }
  else
  {
    soma<-1
    vetor[i]<-soma
  }
  anterior
}

table(vetor)

# Como fazer vetorizados


