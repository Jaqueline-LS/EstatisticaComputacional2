# Gerar Normal Bivariada, marginais normais e condicionais normais

# Z1 ~ N(0,1), Z2~N(0,1), ro = 0.8
ro<-0.8
z1<-rnorm(100)
z2<-rnorm(100, mean=ro*z1 , sd=sqrt(1-ro^2) ) # Pela condicional N ~ ( u1 + ro(sigma1/sigma2)*(x2 - u2); sigma1^2*(1-ro^2))

plot(z1,z2, pch=19, xlim=c(-3,3), ylim=c(-3,3))


geradorNB<-function(ro)
{
  z1<-rnorm(1)
  z2<-rnorm(1, mean=ro*z1 , sd=sqrt(1-ro^2) ) # Pela condicional N ~ ( u1 + ro(sigma1/sigma2)*(x2 - u2); sigma1^2*(1-ro^2))
  return(c(z1,z2))
}


ro<-0.8
amostra<-replicate(100,geradorNB(ro))

str(amostra)
centroide <- apply(t(amostra), 2, mean)
plot(t(amostra), pch=19, xlim=c(-3,3), ylim=c(-3,3))
points(x=centroide[1], y=centroide[2], pch=15, col="coral3")
abline(0,ro) # O ro da a inclinação da esperança condicional



ro<-0
amostra<-replicate(100,geradorNB(ro))

str(amostra)
centroide <- apply(t(amostra), 2, mean)
plot(t(amostra), pch=19, xlim=c(-3,3), ylim=c(-3,3))
points(x=centroide[1], y=centroide[2], pch=15, col="coral3")
abline(0,ro) # O ro da a inclinação da esperança condicional




set.seed(666)

ro<-0.8
amostra<-replicate(100,geradorNB(ro))
centroide <- apply(t(amostra), 2, mean)
plot(t(amostra), pch=19, xlim=c(-3,3), ylim=c(-3,3))
points(x=centroide[1], y=centroide[2], pch=15, col="coral3")
abline(0,ro) # O ro da a inclinação da esperança condicional

S<-cov(t(amostra))

r<-S[2,1]/prod(sqrt(diag(S)))

R<-cor(t(amostra))
R

cov2cor(S) # pega a matriz de cov e transforma na de correlação

V.inv<-diag(1/sqrt(diag(S)))
R.2<-V.inv%*%S%*%t(V.inv) # outra forma de calcular a R


auto<-eigen(S)
auto$values
auto$vectors

det(S) # Variancia generalizada
sum(diag(S)) # Variancia total, traço da matriz

prod(auto$values) # Produto dos autovalores nos da a variancia generalizada
sum(auto$values) # Soma dos autovalores nos da a variancia total

apply(auto$vectors**2, 2, sum)

e1<-auto$vectors[,1]
e2<-auto$vectors[,2]

e1%*%e1 # Comprimento do vetor e1 ( primeiro autovetor )
e1%*%e2 # Os autovetores são ortogonais

# O primeiro autovetor nos da a direção de maior variabilidade.

Lambda<-diag(auto$values)
E<-auto$vectors

# Decomposição espectral, reconstituir a matriz S com Lambda e E
S.nova<-E%*%Lambda%*%t(E)
S

# Desenhar os autovetores
arrows(centroide[1], centroide[2], x1 = E[1,1], y1 = E[2,1], col="blue4")
arrows(centroide[1], centroide[2], x1 = E[1,2], y1 = E[2,2], col="maroon1")

# Dj^2 = t(xj - x.barra) S^-1 (xj - x.barra)
X<-t(amostra) # Nossa matriz de dados
S.inv<-solve(S)

X.centrada<-scale(X, center = T, scale = F)

# calulo de matriz de cov amostral S, fiz algo errado
(t(X.centrada)%*%X.centrada)/99 # 100 observacoes n-1


dj.2 <- diag(X.centrada %*% S.inv %*% t(X.centrada))
# Todas as 100 distancias ao quadrado

qchisq(0.5,df=2)

mean(dj.2<=qchisq(0.5,df=2))
mean(dj.2<=qchisq(0.95,df=2))

hist(dj.2, freq = F)
curve(dchisq(x, df=2), from=0, to=7, add=T, col="aquamarine3", lwd=2)

# QQ-plot
pontos<-(1:100 - 1/2)/100
teoricos<-qchisq(pontos,df=2)
amostrais<-sort(dj.2)
plot(x=teoricos, y=amostrais)



pt_amostra<-quantile(sort(dj.2), c(0.25,0.75))
pt_teorico<-qchisq(c(0.25, 0.75), df=2)
points(x=pt_teorico, y=pt_amostra, pch=3, col="red", cex=2)
b1<-diff(pt_amostra)/diff(pt_teorico)
b0<-pt_amostra[1]-(b1*pt_teorico[1])
abline(a=b0, b=b1)


