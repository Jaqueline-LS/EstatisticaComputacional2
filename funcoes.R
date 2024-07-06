rBM<-function(n) # Gerador de duas normais independentes Box-muller
{
  u1<-runif(n)
  u2<-runif(n)
  z1<-sqrt(-2*log(u1))*cos(2*pi*u2)
  z2<-sqrt(-2*log(u1))*sin(2*pi*u2)
  return(list(z1=z1,z2=z2))
}
