set.seed(666)
amostra3<-rexp(100)
plot(faithful)

hist(faithful$eruptions, freq = F, xlim = c(0,7))
lines(density(faithful$eruptions))

hist(faithful$waiting, freq = F)
lines(density(faithful$waiting, freq=F))

# How to optimizr bins in histograms
# h: largura da janela
# Nh: qtd de janelas

#(1) sqrt(272)
#(2) log(272, base = 2)+1
#(3) 
#(4) 3.491*sigma*n^(-1/3)

sd.faithful.w<-sd(faithful$waiting)
3.491*sd.faithful.w/(272^(1/3))
diff(range(faithful$waiting)) / (3.491*sd.faithful.w/(272^(1/3)))


hist(faithful$waiting, freq = F, breaks = "Scott")
lines(density(faithful$waiting, freq=F))

par(mfrow=c(3,2))

# Histograma 1
hist(faithful$waiting, freq = F, breaks = seq(40,100, by=3.5), main="SQRT", ylim=c(0,0.045))
lines(density(faithful$waiting, freq=F))


# Histograma 2
# Assume um simetria dos dados
hist(faithful$waiting, freq = F, breaks = seq(40,100, by=10), main="Sturges", ylim=c(0,0.045))
lines(density(faithful$waiting, freq=F))

# Histograma 3 Rice  ~13
2*(272)^(1/3)
hist(faithful$waiting, freq = F, breaks = seq(40,100, by=4.6153), ylim=c(0,0.045), main="Rice")
lines(density(faithful$waiting, freq=F,))

# Histograma 4
hist(faithful$waiting, freq = F, breaks = seq(40,100, by=7.24), main="Scott", ylim=c(0,0.045))
lines(density(faithful$waiting, freq=F))



# Histograma 5 Fredman Diaconis

iqr<-IQR(faithful$waiting)
h.fd<-2*iqr/(272^(1/3))
hist(faithful$waiting, freq = F, breaks = seq(40,100, by=h.fd), main=" Fredman Diaconis")
lines(density(faithful$waiting, freq=F))


n.h<-function(x, mi, sigma, n)
{
  g1<-mean(((x-mi)/sigma)^3) # mede assimetria
  sigma.g1<-sqrt((6*n*(n-2))/(n+1)*(n+3))
  n.h<-1+ log(n, base=2)+log(1+(abs(g1)/sigma.g1))
  return(n.h)
}

amostra<-rnorm(1000)
mean(amostra)

n.h(faithful$waiting, mean(faithful$waiting), sd(faithful$waiting), 272)
n.h(amostra3, 1, 1, 100)

n.h(amostra,0,1,1000)

mean(scale(faithful$waiting)**3) #Alternativa para o g1
# O g1 estima a assimetria


# Histograma 6
nDoane<-n.h(faithful$waiting, mean(faithful$waiting), sd(faithful$waiting), 272)
hist(faithful$waiting, freq = F, breaks = seq(40,100, by=h.fd), main="Doane")
lines(density(faithful$waiting, freq=F))

par(mfrow=c(1,1))



# No R temos o Sturges, Scott e FD


