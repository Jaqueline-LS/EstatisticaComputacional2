P<-matrix(c(1/2,1/6,1/6,1/6,
            1/4,1/4,1/4,1/4,
            1/2,1/6,1/6,1/6,
            1/5,2/5,1/5,1/5), byrow=T, ncol=4)
pi.0<-c(0.25,0.25,0.25,0.25)

pi.1<-t(pi.0)%*%P

P.2<-P%*%P
P.4<-P.2%*%P.2
P.8<-P.4%*%P.4
apply(P.8, MARGIN = 1,sum) # A soma das colunas é 1 (Estocástica)
pi.8<-t(pi.0)%*%P.8


# Distribuição invariante da cadeia, convergência
pi.0<-c(1,0,0,0) # Sai de 1
pi.8<-t(pi.0)%*%P.8

# P é diagonalizavel

# Autovetores à direita(V):

# P V = V %*% Lambda


av<-eigen(P)
av$values # Para matrizes de transição o primeiro autovetor é 1

Lambda<-diag(av$values)
V<-av$vectors

P%*%V==V%*%Lambda

# Reconstituição de "P"

V.inv<-solve(V)
V%*%Lambda%*%V.inv

# Potencia de P, basta elevar a matriz diagonal dos autovalores
P.8<-V%*%(Lambda^8)%*% V.inv



# Autovetores à esquerda(U): Quando a Matriz é simetrica eles são iguais aos da direita
# t(U) %*% P = Lambda %*% t(U)

U<-eigen(t(P))$vectors
U.linha<-t(U)
U.linha.inv<-solve(U.linha)

# Reconstituição da P com os autovetores a esquerda
P.nova<-U.linha.inv%*%Lambda%*%U.linha

sum((P-P.nova)^2)

P.10<-U.linha.inv%*%Lambda^10%*%U.linha

# Primeiro autovetor a esquerda 
u1<-U[,1]

u1/(sum(u1)) # tranformação para encontrar as probabilidades invariantes ( vetor da distribuição invariante )


P<-matrix(c(0.3,0.2,0.5,0,0,0,0,
            0.2,0.4,0.4,0,0,0,0,
            0.6,0,0.4,0,0,0,0,
            0,0,0,0.2,0.8,0,0,
            0,0,0,0.7,0.3,0,0,
            0.1,0.2,0.3,0.1,0,0.2,0.1,
            0.2,0,0,0.6,0,0.1,0.1), ncol=7, byrow = T)
apply(P, MARGIN = 1, sum)

P.2<-P%*%P
P.4<-P.2%*%P.2
P.8<-P.4%*%P.4
P.16<-P.8%*%P.8
P.32<-P.16%*%P.16
t(P.32)
apply(P.32,MARGIN = 1, sum)

#C1={0,1,2}
#C2={3,4}
#T={5,6}

# A probabilidade do estado 5 ser absorvido por C1={0,1,2}
0.330+0.110+0.349

# A probabilidade do estado 5 ser absorvido por C2
1-0.789

# Prob. do estado 6 ser absorvido por c1
0.129+0.043+0.137

# Prob do estado 6 ser absorvido por c2 é
1-(0.129+0.043+0.137)

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

apply(M, MARGIN = 1, sum)


U<-eigen(t(M))$vectors
U.linha<-t(U)
U.linha.inv<-solve(U.linha)

# Reconstituição da P com os autovetores a esquerda
M.nova<-U.linha.inv%*%Lambda%*%U.linha

sum((P-P.nova)^2)
