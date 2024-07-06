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

(16^2)*solve(t(X)%*%X)


Li.sigma<-sum(md$residuals^2) / qchisq(0.975,df=22)
Ls.sigma<-sum(md$residuals^2) / qchisq(0.025,df=22)
