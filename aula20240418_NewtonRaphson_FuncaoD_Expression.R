func2<-function(x) x**3 - 2*x -5
curve(func2,from=-5, to=5, lwd=2, col="blue")
abline(h=0, v=0, col="grey50")

#Procedimento N-R

f.linha2<-function(x) 3*x**2 -2

newton<-function(x.antes){
  x.agora=x.antes-((func2(x.antes)/f.linha2(x.antes)))
  x.agora
}
x0<-2
x1<-newton(x0)
x2<-newton(x1)
x3<-newton(x2)
x4<-newton(x3)

resultados2<-c(x0,x1,x2,x3,x4)
diff(resultados2)


x0<-(-2)
x1<-newton(x0)
x2<-newton(x1)
x3<-newton(x2)
x4<-newton(x3)
x5<-newton(x4)
x6<-newton(x5)
x7<-newton(x6)



resultados2<-c(x0,x1,x2,x3,x4, x5, x6, x7)
diff(resultados2)
points(x=resultados2, y=rep(0, length(resultados2)), pch=19, col="red")
text(x=resultados2, y=rep(0, length(resultados2)), cex=0.85, labels=seq_along(resultados2)-1, font=2, pos=3)

#
b0<-function(xn) func2(xn) - xn*(f.linha2(xn))

abline(a=b0(x0), b=f.linha2(x0), lty=2, col="red")
abline(a=b0(x1), b=f.linha2(x1), lty=2, col="red")

# 3 iterações e já chegou na raiz

# Problemas quando a derivada é proxima de 0
x0<-sqrt(2/3)-0.0001
x1<-newton(x0)
# x1 = -12428.37416661


f2<-expression(x^3 - 2*x -5)
df<-D(f2,"x")
df

func2<-function(x) eval(f2)

func2(0)
func2(c(1,-5,0))

f.linha2<-function(x) eval(df)

f.linha2(sqrt(2/3))

# Resolver a equação e^2x = x+6

f3<-expression(exp(2*x) -x -6)
df3<-D(f3, "x")
func3<-function(x) eval(f3)
f.linha3<-function(x) eval(df3)
curve(func3, xlim=c(-8,2), lwd=2, col="blue")
abline(h=0, v=0, col="grey50")


# N-R


newton3<-function(x0, tol=1E-5)
{
  f3<-expression(exp(2*x) -x -6)
  df3<-D(f3, "x")
  func3<-function(x) eval(f3)
  f.linha3<-function(x) eval(df3)
  
  x.antes<-x0
  x.agora<-0
  epsilon<-1E9
  iter<-0
  
  while(epsilon>tol)
  {
    x.agora=x.antes-((func3(x.antes)/f.linha3(x.antes)))
    x.agora
    iter<-iter+1
    epsilon<-abs(x.agora-x.antes)
    x.antes<-x.agora
  }
  resultado<-list(zero=x.agora, iteracoes=iter)
  return(resultado)
}

r2<-newton3(x0=1)
newton3(x0=100)
r1<-newton3(x0=-6)
r1
newton3(x0=-10000)
newton3(x0=-0.3)

raiz<-uniroot(f=func3, interval = c(0,1))
raiz$root

raiz2<-uniroot(f=func3, interval = c(-7,-5))
raiz2$root
# Aprimorar o metodo colocando a tolerancia em f(x)=0



newton4<-function(x0, tol=1E-5)
{
  f4<-expression(x^3 -5*x)
  df4<-D(f4, "x")
  func4<-function(x) eval(f4)
  f.linha4<-function(x) eval(df4)
  
  x.antes<-x0
  x.agora<-0
  epsilon<-1E9
  iter<-0
  
  while(epsilon>tol)
  {
    x.agora=x.antes-((func4(x.antes)/f.linha4(x.antes)))
    x.agora
    iter<-iter+1
    epsilon<-abs(x.agora-x.antes)
    x.antes<-x.agora
  }
  resultado<-list(zero=x.agora, iteracoes=iter)
  return(resultado)
}

newton4(1)
# Criterio de parada ruim, colocar um criterio no numero de iteracoes
