gerador.congruencial <- function(x0 = 123456789 ,n = 1,m = 2^31 - 1 ,A = 16807 ,B = 1){
  
  U <- numeric(n)
  
  aux <- x0
  
  for(i in 1:n){
    
    xi <- (aux*A+B)%%m
    
    U[i] <- xi/m
    
    aux <- xi
    
  }
  
  U
  
}

## Primeira solução
soma <- 0

vet <- numeric(99)

anterior <- valor[1]

for(i in 1:99){
  
  if(valor[i] == anterior){
    
    soma <- soma + 1
    
    vet[i] <- soma
    
  } else {
    
    soma <- 1
    
    vet[i] <- soma
    
  }
  
  anterior <- valor[i]
  
}

amostra <- gerador.congruencial(n=100) # Gera a amostra
valor <- amostra[-1]>amostra[-100]
a<-table(vet)
a[3]=a[3]-a[4]
a[2]=a[2]-a[3]-a[4]
a[1]= a[1]-a[2]-a[3]-a[4]
a

## Segunda solução

amostra <- gerador.congruencial(n=100) # Gera a amostra
valor <- amostra[-1]>amostra[-100] # O vetor armazena se o valor anterior é maior ou menor que o próximo (true ou false)
teste <- rle(valor) # a função calcula a quantidade de valores iguais a cada sequencia igual
R<-table(teste$lengths) # seleciona apenas os tamanhos das sequencias iguais e faz o table
R


## Terceira solução
amostra <- gerador.congruencial(n=100) # Gera a amostra
valor <- amostra[-1]>amostra[-100] # O vetor armazena se o valor anterior é maior ou menor que o próximo (true ou false)

##--- Contar os valores "FALSE" ---##
# Vetor que armazena as posições dos valores "TRUE"
indiceT<-c(0,which(valor),100)

# Ao pegar a diferença entre as posições - 1 temos o número de "FALSE" entre as ocorrências de "TRUE"
corridasF<-diff(indiceT)-1

# Retirar os zeros que significam "TRUE" em sequência
corridasF<-corridasF[corridasF!=0] 

##--- Contar os valores "TRUE" ---##
# O mesmo pode ser aplicado para pegar as quantidades de "TRUE"
indiceF<-c(0,which(!valor),100) # Posições de "FALSE"
corridasT<-diff(indiceF)-1 # Quantidades de "TRUE"
corridasT<-corridasT[corridasT!=0] # Retirar os zeros 

# Tabela com o número de corridas de cada tamanho
table(c(corridasF, corridasT))
> 1  2  3  4 
> 39 20  4  2 

# Total de corridas
CorridasTotais<-length(corridasF)+length(corridasT)
> 65

