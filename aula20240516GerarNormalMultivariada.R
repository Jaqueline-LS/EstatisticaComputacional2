
source("aula20240514.R")
source("funcoes.R")

choose(100,2) # Retas possíveis nos 100 pontos

lm(y1[c(1,2)]~x1[c(1,2)])
 # for i 1:100 e j i+1:100
# Pegar os 4950 betas e fazer a medias deles

# Fazer a reta que minimiza as somas das distancias de todos os pontos a reta ortogonal ao ponto

# Normal Multivariada

# Suponha Z1, Z2, ... , Zm iid N(0,1)

# X1<-a11*Z1 + a12*Z2 + ... + a1m*Zm + u1
# X2<-a21*Z1 + a22*Z2 + ... + a2m*Zm + u2
# ...
# Xm<-am1*Z1 + am2*Z2 + ... + amm*Zm + um


M<-matrix(c(9,4,2,4,8,3,2,3,7), ncol=3, byrow = F)
mat.chol<-chol(M)
A<-t(mat.chol)
A%*%t(A)

set.seed(666)
mi<-c(3,0,-3)

zl<-rnorm(3, mean=0, sd=1)

xis<-A%*%zl + mi

matZ<-matrix(rnorm(300), ncol=3)

xis<-A%*%t(matZ) + mi
str(xis)

X<-as.data.frame(t(xis))# matriz de dados

plot(X)
hist(X[,1], freq=F)
curve(dnorm(x, mean=3, sd=3), col="aquamarine3", add=T, lwd=3)
hist(X[,2], freq=F)
curve(dnorm(x, mean=0, sd=sqrt(8)), col="aquamarine3", add=T, lwd=3)
hist(X[,3], freq=F)
curve(dnorm(x, mean=-3, sd=sqrt(7)), col="aquamarine3", add=T, lwd=3)
x.barra<-sapply(X, mean)
S<-cov(X)
S
S.inv<-solve(S)
R<-cor(X)
R

X.centrada<-scale(X, center = T, scale=F)
str((X.centrada %*% S.inv %*% t(X.centrada)))
dj.2<-diag((X.centrada %*% S.inv %*% t(X.centrada)))

hist(dj.2, freq=F, breaks = 20)
curve(dchisq(x, df=3), from=0, to=12, add=T, col="aquamarine3",lwd=3)

ks.test(dj.2, "pchisq", df=3)
mean(dj.2<qchisq(0.5,df=3))
mean(dj.2<qchisq(0.95,df=3))


qqnorm(X[,1])
qqline(X[,1])
abline(v=1.96)


qqnorm(X[,2])
qqline(X[,2])
abline(v=1.96)


qqnorm(X[,3])
qqline(X[,3])
abline(v=1.96)

# Limitações do método, metodo numerico embutido ( Cholesky )

