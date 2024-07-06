#integração monte carlo 
# caso 1
beta(3,2)

ge <- function(t,x=3,y=2){
  (1-t)**(y-1) * t**(x-1)
}

u <- runif(1000)
mean(ge(u))

# caso 2

h <- function(y){
  1/sqrt(2*pi) * exp(-((y+1)**2 /2 ))
}
 
mean(h(u))


gera.mil <- function(n){
  u <- runif(n)
  mean(h(u))
  
}
verdadeiro <- pnorm(2)-pnorm(1)

# criando uma amostra com mil valores
media1 <- replicate(1000, gera.mil(1000))

hist(media1, freq  = F,col ="lightblue")
mean(media1)
sd(media1)
quantile(media1, c(0.025,0.975))
log10(mean((amostra-verdadeiro)**2))

#criando amostra com 2000 valores

media2 <- replicate(1000, gera.mil(2000))      
hist(media2, freq = F, col = "purple",add = T)
mean(media2)
sd(media2)
quantile(media2, c(0.025,0.975))
log10(mean((media2-verdadeiro)**2))


##########23/05/2024############
# caso 3

  # 2 parametrização
h.linha <- function(y){
  exp(-(y/(1-y))**2) /(1-y)**2
}

mean(h.linha(u))

 # 3° parametrização

u1 <- runif(1000)
u2 <- runif(1000)

h.2linhas <- function(x,y){
  (exp(-x**2))+(exp(-(1/y)**2)/y**2)
} 

mean(h.2linhas(u1,u2))


##### Estimação de pi, pelo metodo Monte Carlo

  # dado (x,y), um vetor aleatório distribuído em quadrado de area 4, a área do círculo pi.
  # X,Y iid, uniforme(-1,1), 
  # x² + y² <= 1
  # P{pto cair dentro do círculo} = pi/4

# gerar u1 e u2 iid ~ uniforme(0,1)
# faça x= 2*u1 - 1 e y = 2*u2 - 1
# I = 1 , se x² + y² <= 1 e 0, cc
# Após b repetições de algoritimo acima a estimação da 

gera.pi <- function(B = 1E3){
  U1 <- runif(B)
  U2 <- runif(B)
  X <- 2*U1 - 1
  Y <- 2*U2 - 1
  I <- X**2 + Y**2 <= 1
  mean(I)*4
}
gera.pi(B=1000)

pi.1000 <- replicate(1000, gera.pi(B=1000))
hist(pi.1000, freq = F, breaks = 50)


# agora vamos calcular a estimativado do ero relativo, para b=10,100,1000,10000,100000

errorelativo <- function(p){
  erro <- abs(p - pi)/pi
  erro
}

pierro.1000 <- errorelativo(pi.1000)

# gerar grafico tamanho da amostra x erro relativo

#Exemplo 1 não finalizado 
amostra <- rcauchy(10)+350
vero <- function(teta){
  u <- prod(dcauchy(amostra-teta))
  u
}

integrate(vero, lower = -Inf,upper = Inf)

#Exemplo 2
#simulando exercício da probabilidade da chaves, onde temos as chaves c1,c2,c3,c4,c5, sendo q a 
#probabilidade de cada uma funcionar é igual a p

# ultima foto
