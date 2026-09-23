# Simulação

#####
# controle fluxo
#####

#loop ou laço

for(i in 1:20){
  print(i)
}

# Exemplo 2 de laço
soma <- 0
for(i in 1:10){
  soma <- soma + i
}
soma

# Exercício em sala
soma_pares <- 0
pares <- c(2,4,6,8,10)
for ( i in pares){
  soma_pares <- soma_pares + i
}
soma_pares
sum(pares)

produto <- 1
for ( i in 1:5){
  produto <- produto*i
}
produto
prod(1:5)

# Funções
minha_soma <- function(vetor_numeros) {
  soma <- 0
  for(i in vetor_numeros){
    soma <- soma + i
  }
  return(soma)
}

minha_soma(1:10)
minha_soma(1:20)  
minha_soma(3:5)

## Simulação

# doença rara: 1/1000
# teste: 
# falso positivo: 5%.
# Verdadeiro positivo: 100%.

# Qual a probabilidade de estar doente,
# dado que o teste foi positivo?

# sample
sample(1:6, 2, replace=TRUE) # com reposição

pessoas <- 1:1000
sorteado <- sample(pessoas, 1)
if(sorteado == 1) {
  teste <- 1
} else {
  teste <- sample(1:20, 1)
}

#####
# mudando a prevalência para 1/100
####
# cria objetos para armazenar dados da simulação
sim <- numeric()
pessoa_sorteada <- numeric()
resultado_teste <- numeric()


pessoas <- 1:100

# loop
for ( i in 1:10000) {
  # sorteia uma pessoa aleatória
  sorteado <- sample(pessoas, 1)
  
  #faz o teste no sorteado
  if(sorteado == 1) {
    teste <- 1
  } else {
    teste <- sample(1:20, 1)
  }
  sim[i] <- i
  pessoa_sorteada[i] <- sorteado
  resultado_teste[i] <- teste
}


#criando banco de dados
bd_sim <- data.frame(sim=sim,
                     pessoa_sorteada = pessoa_sorteada,
                     resultado_teste = resultado_teste)

# carregando biblioteca
library(tidyverse)

#filtrando banco
bd_sim_positivo <- bd_sim %>%
  filter(resultado_teste == 1)

# resumindo os dados
bd_sim_positivo %>%
  summarise(total_doentes = sum(pessoa_sorteada == 1),
            total_pessoas = n(),
            perc = total_doentes/total_pessoas)

# especificando a semente
set.seed(123)
sample(1:6, 1)
