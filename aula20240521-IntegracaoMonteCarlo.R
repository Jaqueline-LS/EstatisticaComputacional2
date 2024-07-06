# Caso 1 - Exemplo
beta(3,2)
u<-runif(1000)

g.u<-function(t,x=3,y=2)
{
  ((1-t)^(y-1))*(t^(x-1))
}

mean(g.u(u))

# Caso 2 - Exemplo da normal
verdadeiro<-pnorm(2)-pnorm(1)

h.y<-function(y)
{
  (1/sqrt(2*pi)) * exp( -((y+1)^2)/2 )
}
mean(h.y(u))

estimar.n<-function(n)
{
  u<-runif(n)
  mean(h.y(u))

}
thetas<-replicate(1000,estimar.n(1000))

hist(thetas)
mean(thetas)
sd(thetas)
x=quantile(thetas,probs=c(0.025,0.975))
x
diff(x)

abline(v=x, col="red3", lwd=2)

eqm.hat<-log10((mean((thetas-verdadeiro)^2)))

# Como melhorar a estimativa

thetas2<-replicate(1000,estimar.n(2000))
hist(thetas2, col="pink")
mean(thetas2)
sd(thetas2)
x=quantile(thetas2,probs=c(0.025,0.975))
x
diff(x)
abline(v=x, col="maroon2", lwd=2)
eqm.hat<-log10((mean((thetas2-verdadeiro)^2)))
eqm.hat


# Como melhorar a estimativa, na geração do u, 

thetas2<-replicate(1000,estimar.n(3000))
hist(thetas2, col="pink")
mean(thetas2)
sd(thetas2)
x=quantile(thetas2,probs=c(0.025,0.975))
x
diff(x)
abline(v=x, col="maroon2", lwd=2)
eqm.hat<-log10((mean((thetas2-verdadeiro)^2)))
eqm.hat


# Caso 3
integrate(function(x)exp(-x^2), 0, Inf) 
sqrt(pi)/2

h.caso3<-function(y)
{
  exp((-(1/y - 1)^2))/y^2
}
u<-runif(1000)
mean(h.caso3(u))



# Segunda parametrização




# Quando usar o Método: para integrais multipla!!!


