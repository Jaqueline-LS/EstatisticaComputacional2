set.seed(666)
amostra<-rnorm(30,mean=10,sd=5)

#Quantis teoricos, para verificar quantos valores se enquadram em cada uma das 4 categorias que possuem a mesma proporção
q.teoricos<-qnorm(p=c(0,0.25,0.5,0.75,1), mean = 10, sd=5)
categorias<-cut(amostra, breaks = q.teoricos, labels = c("Q1","Q2","Q3","Q4"))
cbind(amostra, categorias)
# A função cut serviu para categorizar a amostra
# 30/4

# quantos valores se encaixam em cada categoria
table(categorias)

# Quantidades observadas
# 13, 5, 3, 9----------
 
#Quantidades esperadas, 30*1/4
# 7.5-7.5-7.5-7.5


amostra2<-rnorm(100, mean = 10, sd=5)
q.teoricos2<-qnorm(p=(0:10/10), mean = 10, sd=5)
categorias2<-cut(amostra2, breaks=q.teoricos2,labels = paste0("Q",1:10))
tabs<-table(categorias2)

sum(((tabs-10)^2)/10) # Estatistica do teste

# Sob H0, Qui-quadrado GL 10-1
pchisq(2.4,9, lower.tail = F)

# Não rejeita H0, a probabilidade de observar um valor tão extremo quanto o observado é grande
# A amostra é de uma normal!!!


#Gerando uma amostra de uma exponencial
amostra3<-rexp(100, rate=0.1)
q.teoricos3<-qnorm(p=(0:10/10), mean = 10, sd=10)
categorias3<-cut(amostra3, breaks=q.teoricos3,labels = paste0("Q",1:10))
tabs<-table(categorias3)

sum(((tabs-10)^2)/10)
pchisq(43,9, lower.tail=F)

# Gerando 100 valores de uma exp(1)
u<-runif(100, min=0, max=1)
x<-(-log(u))

#Verificação da amostra gerada
# Histograma
hist(x, freq = F)
curve(dexp(x), from=0, to=5, add = T)
lines(density(x),lty=2,col="blue",lwd=3)
# A suavização tem problemas com as fronteiras, deu massa para valores nos quais a função não é definida

1+log(100, base=2) #Sturges rule, numero de barras

# Acumulada
Fn<-ecdf(x)
plot(Fn)
curve(pexp, from = 0, to=5, add = T, col="blue4", lwd=2)

# KS teste
ks.test(x, y=pexp)
# H0: Valor da amostra veio de uma exponencial, p>> não rejeita H0


# Montar o QQ plot
probs<-ppoints(100)
q.teoricos<-qexp(probs, rate=1)
q.observado<-sort(x)
plot(x=q.teoricos, y=q.observado)
pt_amostra<-quantile(x, c(0.25,0.75))
pt_teorico<-qexp(c(0.25, 0.75))
points(x=pt_teorico, y=pt_amostra, pch=3, col="red", cex=2)
b1<-diff(pt_amostra)/diff(pt_teorico)
b0<-pt_amostra[1]-(b1*pt_teorico[1])
abline(a=b0, b=b1)

1-pexp(4) # Esperava 2 pontos acima de 4


# X2, usando a estatistica qui-quadrado
q.teoricos5<-qexp(p=(0:10/10), rate=1)
categorias5<-cut(x, breaks=q.teoricos5,labels = paste0("Q",1:10))
tabs<-table(categorias5)
sum(((tabs-10)^2)/10)
pchisq(7.8,9, lower.tail=F)

# Não há evidências para rejeitar H0, p-value=0.5544>0.05.
# Ou seja, a probabilidade de observar um valor mais extremo do que o observado é alta.

