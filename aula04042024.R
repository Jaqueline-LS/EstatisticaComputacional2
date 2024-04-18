u1<-runif(1000)
u2<-runif(1000)
tri<-u1+u2
hist(tri)
mean(tri)

u3<-u1+2 # Uniforme (2,3)
hist(u3)

u4<-2*u1
hist(u4) # Uniforme (0,2)

5%%3
14%%3
# 5 e 14 sao congruentes mod 3

# Calcular modulo sem o operador
5-floor(5/3)*3

seed<-123456789
A<-16807
B<-1
m<-(2^31-1)
x1<-(seed*A+B)%%m
u1<-x1/m

x2<-(x1*A+B)%%m
u2<-x2/m


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
amostra<-geradorU(n=100)

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
