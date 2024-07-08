set.seed(666)
amostra<-c(2,2,1,1,5,4,4,3,1,2)
indice<-sample(1:10, 10, replace = T)
amostra[indice]


# Objetivo estimar o erro padrao a correlação entre LSAT e GPA
data(law, package = "bootstrap")
data(law82, package = "bootstrap")

str(law)
str(law82)

theta.hat<-with(law, cor(LSAT, GPA))
theta.hat

# Parametros do Bootstrap
set.seed(666)
n<-nrow(law) # tamanho da amostra
B<-200

indice<-sample(1:n, size=n, replace = T)
reamostra<-law[indice,]
theta.hat.b<-with(reamostra, cor(LSAT, GPA))
theta.hat.b # primeira estimativa bootstrap


R<-numeric(B)
for(i in 1:B)
{
  indice<-sample(1:n, size=n, replace = T)
  reamostra<-law[indice,]
  theta.hat.b<-with(reamostra, cor(LSAT, GPA))
  R[i]<-theta.hat.b 
}
R
# Histograma das estimativas bootstrap
hist(R, freq=F)

# Estimativas do coeficiente de correlação
ep.theta<-sd(R)
ep.theta

# Estimativa teorica sob hipotese de normalidade bivariada
(1-theta.hat^2)/sqrt(n-3)

boot::boot()
# Escrever um funçao que retorne o theta.hat.b, em que o 1° argumento seja os dados amostrais e o segundo o vetor de indices

r.func<-function(dados, indice)
{
  with(dados[indice,],cor(LSAT, GPA))
}

set.seed(666)
obj<-boot::boot(data=law,statistic = r.func, R=2000)
obj

str(obj)

# Valor observado da estatistica
obj$t0


#obj$t # As duas mil estimativas da correlação

hist(obj$t, freq =F)

sd(obj$t)
obj$seed[1]

mean(obj$t - obj$t0) # Estimativa bootstrap do vies
# O estimador bootstrap é viesado para o parametro,  devemos estimar o vies!!!!!!!


cv<-abs(mean(obj$t - obj$t0))/sd(obj$t)
cv # se for menor que 0.25 não precisa ajustar o vies


# Cap.8 Rizzo M

# Exemplo 8.5
data(patch, package="bootstrap")
patch

theta.hat<-mean(patch$y)/mean(patch$z)
theta.hat

# Parametros do Bootstrap
set.seed(666)
n<-nrow(patch) # tamanho da amostra
B<-2000
theta.b<-numeric(B)
for(i in 1:B)
{
  indice<-sample(1:n, size=n, replace = T)
  reamostra<-patch[indice,]
  theta.b[i]<-with(reamostra, mean(y)/mean(z) )
}
theta.b

# Estimativas do coeficiente de correlação
ep.theta.b<-sd(theta.b)
ep.theta.b

# Vies de theta.hat
vies.b<-mean(theta.b)-theta.hat
vies.b


#CV
cv<-abs(vies)/ep.theta.b
cv
# Para a estimativa de erro padrao quando o cv for menor que 0.25 não é necessário corrigir o viés 
# do erro padrão


# JACKKNIFE
# reamostra i jackknife todos os valores da amostras menos o valor i
# Para aplicar jackknife a estatística deve ser suave

n<-nrow(patch)
theta.jack<-numeric(n)
for(i in 1:n)
{
  amostra.jack<-patch[-i,]
  theta.jack[i]<-with(amostra.jack, mean(y)/mean(z))
}
theta.jack

# Estimativa jackkinife do vies
vies<-(n-1)*(mean(theta.jack)-theta.hat)
vies

# estimativa jackknife do ep

se<-sqrt((n-1)*mean((theta.jack-mean(theta.jack))^2))

# CV
vies/se


# Intervalo de confiança Bootstrap
# ----------------------IC bootstrap normal padrao----------------------------

# Exemplo para o patch
theta.b # estimativas dos thetas
ep.theta.b
theta.hat # Estatistica observada

alfa<-c(0.025,0.975)
limites<- theta.hat+qnorm(alfa)*ep.theta.b
limites
vies.b

# correção
limites-vies.b


# [ -0.2886184 ; 0.1266142] ( Normal padrão ) Não foi bioequivalente (<=20)

#------- Intervalo de confiança bootstrap básico, p.225------------------------

theta.star.q<-quantile(theta.b, rev(alfa), type = 1) # rev inverte o vetor
limites<-2*theta.hat - theta.star.q
limites
#[ -0.3293 ; 0.08748] ( BASICO ) Não foi bioequivalente (<=20)

#--------------Intervalo de confiança bootstrap percentil-------------------

limites<-quantile(theta.b, probs = alfa, type = 6)
limites
# [-0.2300892 ; 0.1876592 ] (Percentil) Não foi bioequivalente (<=20)


set.seed(666)
# Escrever um funçao que retorne o theta.hat.b, em que o 1° argumento seja os dados amostrais e o segundo o vetor de indices
theta.boot<-function(dados, indice) # funcao da estatistica
{
  reamostra<-dados[indice,]
  with(reamostra,mean(y)/mean(z))
}

boot.obj<-boot::boot(data=patch,statistic = theta.boot, R=2000)
boot::boot.ci(boot.obj,type = c("basic", "norm", "perc")) # os valores não são iguais aos feitos na mão

# Para o law, exemplo 8.10 ----- eu que fiz
obj<-boot::boot(data=law,statistic = r.func, R=2000)
boot::boot.ci(obj)
#----------

#---------------------Intervalo bootstrap t---------------------
boot.t.ci<-function(x,B=500,R=100,level=0.95, statistic)
{
  # calcula o IC bootstrap t
  x<-as.matrix(x)
  n<-nrow(x)
  stat<-numeric(B)
  se<-numeric(B)
  boot.se <- function(x, R, f) {
    #local function to compute the bootstrap
    #estimate of standard error for statistic f(x)
    x <- as.matrix(x)
    m <- nrow(x)
    th <- replicate(R, expr = {
      i <- sample(1:m, size = m, replace = TRUE)
      f(x[i, ])
    })
    return(sd(th))
  }
  for (b in 1:B) {
    j <- sample(1:n, size = n, replace = TRUE)
    y <- x[j, ]
    stat[b] <- statistic(y)
    se[b] <- boot.se(y, R = R, f = statistic)
  }
  stat0 <- statistic(x)
  t.stats <- (stat - stat0) / se
  se0 <- sd(stat)
  alpha <- 1 - level
  Qt <- quantile(t.stats, c(alpha/2, 1-alpha/2), type = 1)
  names(Qt) <- rev(names(Qt))
  CI <- rev(stat0-Qt*se0)
}

dat<-cbind(patch$y, patch$z)
stat<-function(dat)
{
  mean(dat[,1])/mean(dat[,2])
}
ci<-boot.t.ci(dat, statistic = stat, R=200, B=2000)
ci


#-------------BCA INTERVALO------------------------

boot.BCa <- function(x, th0, th, stat, conf = .95) {
    # bootstrap with BCa bootstrap confidence interval
    # th0 is the observed statistic
    # th is the vector of bootstrap replicates
    # stat is the function to compute the statistic
    x <- as.matrix(x)
    n <- nrow(x) #observations in rows
    N <- 1:n
    alpha <- (1 + c(-conf, conf))/2
    zalpha <- qnorm(alpha)
    # the bias correction factor
    z0 <- qnorm(sum(th < th0) / length(th))
    # the acceleration factor (jackknife est.)
    th.jack <- numeric(n)
    for (i in 1:n) {
      J <- N[1:(n-1)]
      th.jack[i] <- stat(x[-i, ], J)
    }
    L <- mean(th.jack) - th.jack
    a <- sum(L^3)/(6 * sum(L^2)^1.5)
    # BCa conf. limits
    adj.alpha <- pnorm(z0 + (z0+zalpha)/(1-a*(z0+zalpha)))
    limits <- quantile(th, adj.alpha, type=6)
    return(list("est"=th0, "BCa"=limits))
}


boot::boot.ci(boot.obj,type = c("bca")) # os valores não são iguais aos feitos na mão


data(ironslag, package = "DAAG")
plot(magnetic~chemical, data=ironslag, pch=16)




# Exercício do livro 
# 8.1
# Estimativa jackknife do vies e do erro padrao para a correlação

data(law, package = "bootstrap")
theta.hat<-cor(law$LSAT, law$GPA)
theta.hat

n<-nrow(law)
theta.J<-numeric(n)

for(i in 1:n)
{
  amostra.J<-law[-i,]
  theta.J[i]<-cor(amostra.J$LSAT, amostra.J$GPA)
}
theta.J

# Vies Jackknife

vies<-(n-1)*(mean(theta.J)-theta.hat)
vies

# Erro padrão Jaccknife

se<-sqrt((n-1)*mean((theta.J-mean(theta.J))^2))
se


# 8.3
# Intervalo de confiança bootstrap t

data(law, package = "bootstrap")
theta.hat<-cor(law$LSAT, law$GPA)
theta.hat

#---------------------Intervalo bootstrap t---------------------
boot.t.ci<-function(x,B=500,R=100,level=0.95, statistic)
{
  # calcula o IC bootstrap t
  x<-as.matrix(x)
  n<-nrow(x)
  stat<-numeric(B)
  se<-numeric(B)
  boot.se <- function(x, R, f) {
    #local function to compute the bootstrap
    #estimate of standard error for statistic f(x)
    x <- as.matrix(x)
    m <- nrow(x)
    th <- replicate(R, expr = {
      i <- sample(1:m, size = m, replace = TRUE)
      f(x[i, ])
    })
    return(sd(th))
  }
  for (b in 1:B) {
    j <- sample(1:n, size = n, replace = TRUE)
    y <- x[j, ]
    stat[b] <- statistic(y)
    se[b] <- boot.se(y, R = R, f = statistic)
  }
  stat0 <- statistic(x)
  t.stats <- (stat - stat0) / se
  se0 <- sd(stat)
  alpha <- 1 - level
  Qt <- quantile(t.stats, c(alpha/2, 1-alpha/2), type = 1)
  names(Qt) <- rev(names(Qt))
  CI <- rev(stat0-Qt*se0)
}

estimador<-function(x)cor(x[,1],x[,2])

CI.t<-boot.t.ci(x=law,statistic = estimador)

CI.t




# 8.4

data(aircondit, package = "boot")
aircondit

#EMV é 1/mean(x)

lambda.hat<-1/mean(aircondit[,1])

set.seed(666)
n<-nrow(aircondit) # tamanho da amostra
B<-2000
lambda.b<-numeric(B)
for(i in 1:B)
{
  indice<-sample(1:n, size=n, replace = T)
  reamostra<-aircondit[indice,]
  lambda.b[i]<-1/mean(reamostra)
}
lambda.b

# Vies Bootstrap
vies<-mean(lambda.b)-lambda.hat
vies

# Erro padrão bootstrap
sd(lambda.b)


#8.5

# Escrever uma funçao que retorne o theta.hat.b, em que o 1° argumento seja os dados amostrais e o segundo o vetor de indices
EMV.exp<-function(dados, indice)
{
  1/(mean(dados[indice,]))
}
boot.obj<-boot::boot(data=aircondit,statistic = EMV.exp, R=2000)
IC<-boot::boot.ci(boot.obj,type = c("basic", "norm", "perc","bca")) 

# Pela propriedade da invariância os intervalos para o tempo médio entre as falhas
# é o inverso

1/IC$normal[c(2,3)] 
cat("IC(Média, 95%), normal: [0; 61.4]")
1/IC$basic[c(4,5)]
cat("IC(Média, 95%), basico: [0; 76.1]")
1/IC$percent[c(4,5)]
cat("IC(Média, 95%), percentil: [46.3; 186.2]")
1/IC$bca[c(4,5)]
cat("IC(Média, 95%), BCA: [56.7; 224.8]")

# Por que são diferentes? 
# O intervalo BCA vale para a transformação feita e ele é possui uma acurácia de segunda ordem.
# Os intervalos normal e básico não são válidos para a transformação feita e eles não possuem acurácia de segunda ordem.
# O intervalo Percentil é válido para a transformação feita mas ele só possui acurácia de primeira ordem.

# 8.6

data(scor, package = "bootstrap")
scor

# função para personalização painel
painel.cor <- function(x, y, digits = 2, cex.cor, ...){
  usr <- par("usr"); on.exit(par(usr=usr))
  par(usr = c(0, 1, 0, 1))
  # correlation coefficient
  r <- cor(x, y)
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste("r= ", txt, sep = "")
  text(0.5, 0.6, txt)
 
}

# scatter plot matrix  
pairs(scor, upper.panel = painel.cor)
