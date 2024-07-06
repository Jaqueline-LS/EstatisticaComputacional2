# 
# Falhas em equipamentos.
# Há três máquinas em operação em uma linha de produção. A probabilidade de uma máquina falhar em 
# um determinado dia é p, independente da situação das outras máquinas. Somente uma máquina pode ser 
# reparada no mesmo dia (a oficina estará disponível para o próximo dia). A máquina 1 supre as máquinas 
# 2 e 3, ou seja, se a máquina 1 falhar não há produção naquele dia. Se a máquina 1 falhar, ela é sempre 
# reparada primeiro. Se as máquinas 2 e 3 falharem e a 1 continuar em operação, a máquina 2 é reparada 
# primeiro. As máquinas, que estiverem inoperantes permanecem inoperantes durante todo o dia. Máquinas
# em operação permanecem funcionando, mas podem falhar no próximo dia. É certo que uma máquina reparada 
# operará no próximo dia. Considere p = 0,10. A preocupação são os dias sem produção ou com produção 
# reduzida devido à quebra de equipamentos.
# 
# Fila da oficina?
# O que quebrou?
# Quem vai pra oficina? 


M<-matrix(c(0,0.09,0.09,0,0,0.01,0,0.81,
            0.09,0,0.09,0,0.01,0,0,0.81,
            0.09,0.09,0,0.01,0,0,0,0.81,
            0,0.90,0,0,0,0.1,0,0,
            0,0,0.9,0,0,0.1,0,0,
            0,0,0.9,0,0.1,0,0,0,
            0,0,0,0,0,1,0,0,
            0.081,0.081,0.081,0.009,0.009,0.009,0.001,0.729), byrow = T, ncol=8)
M

M.2<-M%*%M
M.4<-M.2%*%M.2
M.16<-M.4%*%M.4
M.32<-M.16%*%M.16


# M1 FUNCIONA
sum(M.32[1,c(2,3,6,8)])

# M2 FUNCIONA
sum(M.32[1,c(8,5,3,1)])

# M3 FUNCIONA
sum(M.32[1,c(8,4,2,1)])

# Produção paralizada
1-sum(M.32[1,c(2,3,6,8)])

# Para p=0.90


# Como simular?

# Inicio do dia
p1<-0.40
p2<-p1
p3<-p1
m1<-sample(c(0,1), size = 1,replace = T, prob = c(1-p1,p1))
m2<-sample(c(0,1), size = 1,replace = T, prob = c(1-p2,p2))
m3<-sample(c(0,1), size = 1,replace = T, prob = c(1-p3,p3))


sorteia<-function(p) sample(c(1,0), size = 1,replace = T, prob = c(p,1-p))
sorteia.vec <- Vectorize(sorteia)
maquinas<-sorteia.vec(p=c(p1,p2,p3))
maquinas

# Se maquinas é 1 0 1
maquinas<-c(1,0,1)
fila<-1-maquinas
(1:3)[fila==1]
maquinas*maquinas[1]
# Fila<-1-maquinas
# Qual maquina foi reparada anteriormente
maquinas[oficina]<-1

fila.oficina<- which(maquinas==0)
oficina<-0
if(length(fila.oficina)!=0) oficina<-min(fila.oficina)


# Como atualizar a fila da oficina
fila.oficina<-fila.oficina[-oficina]

# funciona <- { maquina que saiu da oficina
# maquinas funcionando}



# Fila de 5 caixas








