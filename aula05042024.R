m<-1000
medias<-seq(-1,1,length.out=31)
sizes<-c(20,41,50,100,1000) #para o n=20 usar a tabela, 0.294, Conover A13

# Função que calcula a taxa de rejeição dado o parâmetro, o tamanho da amostra e
# valor tabelado(d) caso algum tamanho de amostra seja <=40, do jeito que construi 
# apenas 1 dos tamanhos de amostras pode ser <=40
Taxa_mu_N<-function(mu, n, alfa=0.05, d)
{
  if(n<=40)
  {
    amostras<-replicate(1000,rnorm(n, mean=mu))
    estatistica<-apply(amostras, MARGIN = 2 ,FUN= function(x){ ks.test(x,"pnorm")$statistic })
    rejeicao<- sum(estatistica>d) # A funcao which retorna os indices de TRUE
    taxa<-rejeicao/1000
  }
  else
  {
    
    amostras<-replicate(1000,rnorm(n, mean=mu))
    p.valor<-apply(amostras, MARGIN = 2 ,FUN= function(x){ ks.test(x,"pnorm")$p.value })
    rejeicao<- sum(p.valor<alfa) 
    taxa<-rejeicao/1000
  }
  return(taxa)
}

cores<-c(	"#A52A2A", "orange2", "yellow2","green2", "aquamarine3")

plot(x="",xlim = c(-1,1), ylim=c(0,1.5), xlab="média", ylab="taxa de rejeição", main="Gráficos das taxas de rejeição de acordo com o parâmetro média")
abline(h=0.05,lty=2)
for(i in seq_along(sizes))
{
  n<-sizes[i]
  taxas<-sapply(medias,FUN=gerarAmostraN,n=n, QKs.table=0.294)
  lines(x=medias, y=taxas, col=cores[i])
}
legend("topright",legend=paste0("n=",sizes),col = cores, lty = 1)

