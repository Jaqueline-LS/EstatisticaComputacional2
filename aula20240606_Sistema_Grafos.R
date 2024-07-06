1-prod(1-chaves)
# Grafos
# Matriz das transições entre os nós
M<-matrix(c(1,1,0,0,1,
            1,0,0,0,0,
            0,1,0,1,0,
            0,1,1,0,0,
            1,0,0,0,1), byrow = T, ncol=5)
M.quad<-M%*%M
M.quad # Numeros de caminhos de comprimento 2 para ir de i a j

M.cubo<-M.quad%*%M
M.cubo # Numero de caminhos de comprimento 3

M.4<-M.quad%*%M.quad
M.4

# Caminho mais curto?


# Para o sistema funcionar a M.3, o linha(L) e coluna (R) deve ser maior ou igual a 1


set.seed(666)
sistemaFunciona<-function(p)
{
  c1<-rbinom(1,size=1,prob = p)  # Gera indicadora para chave 1 (1- Funciona, 0 - Não Funciona)
  c2<-rbinom(1,size=1,prob = p)
  c3<-rbinom(1,size=1,prob = p)
  c4<-rbinom(1,size=1,prob = p)
  c5<-rbinom(1,size=1,prob = p)
  M<-matrix(c(0,c1,c2,0,
              0,0,c3,c4,
              0,c3,0,c5,
              0,0,0,1), byrow = T, ncol=4)
  M.3<-M%*%M%*%M
  status<-min(1,M.3[1,4]) # O caminho maior para ir de L para R é de 3 passos, se em três passos não tem como chegar em R O sistema NAO FUNCIONA!!!
  return(status)
}
sistemas<-replicate(1000,sistemaFunciona(0.9))
mean(sistemas)



# Segunda configuração
sistema2Funciona<-function(p)
{
  c1<-rbinom(1,size=1,prob = p)  # Gera indicadora para chave 1 (1- Funciona, 0 - Não Funciona)
  c2<-rbinom(1,size=1,prob = p)
  c3<-rbinom(1,size=1,prob = p)
  c4<-rbinom(1,size=1,prob = p)
  c5<-rbinom(1,size=1,prob = p)
  c6<-rbinom(1,size=1,prob = p)
  c7<-rbinom(1,size=1,prob = p)
  c8<-rbinom(1,size=1,prob = p)
  
  M<-matrix(c(0,c1,c2,0,0,0,
              0,0,c3,c4,0,0,
              0,c3,0,0,c5,0,
              0,0,0,0,c6,c7,
              0,0,0,c6,0,c8,
              0,0,0,0,0,1), byrow = T, ncol=6)
 
  M.5<-M%*%M%*%M%*%M%*%M
  status<-min(1,M.5[1,6]) # O caminho maior para ir de L para R é de 5 passos, se em três passos não tem como chegar em R O sistema NAO FUNCIONA!!!
  return(status)
}
sistemas<-replicate(10000,sistema2Funciona(0.9))
mean(sistemas)



# Lista Regressao
set.seed(666)
x1.val<-seq(14,86,length.out=100)
x2.val<-seq(-56,456, length.out=100)

n<-25
x1<-sample(x1.val, size=n, replace = F)
x2<-sample(x2.val, size=n, replace = F)

X<-cbind(rep(1,n),x1,x2)

b0<-150
b1<-(-4)
b2<-2.5

erro<-rnorm(n,mean = 0, sd=16)

y<-b0+b1*x1+b2*x2 + erro

md<-lm(y~x1+x2)
md
summary(md)$sigma


beta.hat<-solve(t(X)%*%X)%*%t(X)%*%y

beta.hat<-solve(t(X)%*%X, t(X)%*%y) #mais rapido
# Beta.hat, N.3(beta, sigma^2 * solve(t(X)%*%X))

256*solve(t(X)%*%X)

