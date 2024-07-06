# Enunciado
# Num jogo de futebol qual é a probabilidade de pelo menos 22 fazerem
# aniversário no mesmo dia.

# Simplificação: 
# - Nascimentos ocorrem ao acaso;
# - Descartada a data 29/02
# - Probabilidade de pelo menos uma coincidência
#     - Resolve pelo complementar, 1 - a prob. de ocorrer nenhuma coincidencias
n<-22
factorial(n)*choose(365,n)/365^n

n<-36
factorial(n)*choose(365,n)/365^n

log.prob<-lgamma(366)-lgamma(366-n)-n*log(365)
prop0<-exp(log.prob)
prop0


n<-200

log.prob<-lgamma(366)-lgamma(366-n)-n*log(365)
prop0<-exp(log.prob)
prop0

n<-22
datas<-sample(1:365, size=n, replace = TRUE)
sum(duplicated(datas))
indice<-duplicated(datas)
datas[indice]
datas<-c(datas,9)
sum(duplicated(datas))


n<-22
coincide<-replicate(1000, any(duplicated(sample(1:365, size=n, replace = T))))
#coincide<-replicate(1000, anyDuplicated(sample(1:365, size=n, replace = T))))
sum(coincide)/1000


# Problema da ruída do jogador

#Exemplo

p<-0.55
q<-0.45
i<-10 # valor inicial $
N<-20
p.atingirN<-function(p)
{
  q<-1-p
  ifelse(p==0.5,i/N,(1-(q/p[-11])^i)/(1-(q/p[-11])^N))
}
p.atingirN(0.55) 0.888
p.atingirN(0.5)

p.ruina<-function(N)1-p.atingirN(N)
p.ruina(20)

# Quantidade esperada de jogadas
Ei<-function(Pi)(i-N*Pi)/(q-p)
Pi<-p.atingirN(0.55)
Ei(Pi)

p<-seq(from=0.4, to=0.6, by=0.01)
p.atingirN(p)

# Como simular 


