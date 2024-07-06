set.seed(666)
amostra<-rnorm(30, mean=10, sd=5)
# Essa amostra é proveniente de uma população normal?
# Histograma de densidade


pnorm(1)-pnorm(-1)

diff(pnorm(c(-1,1))) # Um desvio padrão
diff(pnorm(c(-2,2))) # Dois desvios
diff(pnorm(c(-2,-1,1,2)))

x_bar<-mean(amostra)
sd_bar<-sd(amostra)
8.727-6.184 
8.727+6.184

mean(amostra<=14.912 & amostra>=2.543)
# Deveria ter 0.6826 a conta empírica deu 0.70

8.727-6.184-6.184
8.727+6.184+6.184

mean(amostra<=2.543 & amostra>=-3.641)
# Esperava 0.1359 encontrou 0.167

mean(amostra<=21.095 & amostra>=14.912)
# Esperava 0.1359 encontrou 0.133

hist(amostra, freq = F)
curve(dnorm(x, mean = x_bar, sd=sd_bar), from=-5, to=25, add = T)
lines(density(amostra),lty=2,col="coral3",lwd=3)


# Funcao de distribuição empirica

f_hat<-ecdf(amostra)
str(f_hat)
plot(f_hat)
f_hat(2.543)
f_hat(21.095)-f_hat(14.912)
diff(f_hat(c(2.543,14.912)))


# Adicionando a curva 
curve(pnorm(x,mean = x_bar, sd=sd_bar), from = -2, to=21, add=TRUE)
diferencas<-f_hat(amostra)-pnorm(amostra, mean=x_bar, sd = sd_bar)

# Verificando a diferença entre a acumulada e a acumulada empirica
sum(diferencas)
soma_quadratica<-sum(diferencas^2)
sum(diferencas)/30
sum(diferencas^2)/30
maximo<-max(diferencas)

#  Kolmogorov-Smirnof, não é tão robusto para normalidade
ks.test(x=amostra, y=pnorm, mean=10, sd=5)
shapiro.test(amostra)
nortest::ad.test(amostra)


am_pois<-rpois(n=30, lambda = 5)
ks.test(x=am_pois, y=pnorm)
shapiro.test(am_pois)
nortest::ad.test(am_pois)

ks.test(x=am_pois, y="ppois", lambda=5)

f_hat_pois<-ecdf(am_pois)
plot(f_hat_pois)
curve(pnorm(x,mean = mean(am_pois), sd=sd(am_pois)), from = 0, to=12, add=TRUE)
curve(ppois(x,lambda = mean(am_pois)), from = 0, to=12, add=TRUE, col="coral2")


sort(amostra)

#Quantil amostral
# Primeiro quantil -1.08, 1/n = 0.033
qnorm(0.033, mean = mean(amostra), sd=sd(amostra))

# Segundo quantil 1.038, 0.067
qnorm(0.067, mean = mean(amostra), sd=sd(amostra))

probs_acumul<-seq_along(amostra)/length(amostra)
quantil_amostra<-sort(amostra)
quantil_teorico<-qnorm(probs_acumul, mean = mean(amostra), sd=sd(amostra))

# correção -1/2, para tirar o quantil 1 

probs_acumul<-(seq_along(amostra)-1/2)/length(amostra)
quantil_amostra<-sort(amostra)
quantil_teorico<-qnorm(probs_acumul, mean = mean(amostra), sd=sd(amostra))
cbind(quantil_amostra, quantil_teorico)

# Q-Q Plot
plot(x=quantil_teorico, y=quantil_amostra)

qqnorm(amostra) # em termos da normal padrão
qqline(amostra, col="darkblue", lwd=2)
pt_amostra<-quantile(x=amostra, c(0.25,0.75))
pt_teorico<-qnorm(c(0.25, 0.75))
diff(pt_teorico) #DIQ

quantil_teorico<-qnorm(probs_acumul, mean = 0, sd=1)
plot(x=quantil_teorico, y=quantil_amostra)

points(x=pt_teorico, y=pt_amostra, pch=3, col="red", cex=2)
# A reta é calculada passando pelo primeiro e terceiro quartil

# caudas pesadas
amostra_t<-rt(30, df=2)
qqnorm(amostra_t)
qqline(amostra_t)



probs<-ppoints(30)
qteorico<-qt(probs, df=2)
qamostra<-sort(amostra_t)
plot(qteorico,qamostra)
pt_amostra<-quantile(qamostra, c(0.25,0.75))
pt_teorico<-qt(c(0.25, 0.75))
points(x=pt_teorico, y=pt_amostra, pch=3, col="red", cex=2)
b1<-diff(pt_amostra)/diff(pt_teorico) #a, y=ax+b
b0<-pt_amostra[1]-(b1*pt_teorico[1]) #b
abline(a=b0, b=b1)



# Q1= 4.55(8.727-0.674*6.184), Q3=12.89(8.727+0.674*6.184)

sum(amostra<=4.55)
sum(amostra>12.89)
sum(amostra>4.55 & amostra<=8.727)
sum(amostra>8.727 & amostra<=12.89)

# Estatistica soma(i:4)(([oi-ei]^2)/ei)
o<-c(10,6,5,9)
e<-c(7.5,7.5,7.5,7.5)
sum(((o-e)^2)/e)

# Se os valores são iguais, a estatistica T~Qui-qua(k-1), k-1=3
1-pchisq(2.27, df=3)
# Não temos evidências para rejeitar H0
# Função Cut

table(cut(amostra, breaks=))
quantis<-c()