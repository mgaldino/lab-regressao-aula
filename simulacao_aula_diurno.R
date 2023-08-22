# simulação -

# especificando semente, para simulação ser reproduzível
set.seed(2)

# número de amostras
stirling_aprox <- function(n) {
  sqrt(2*pi)*n^(n+1/2)*exp(-n)
}

print(stirling_aprox(10))

sqrt(2*pi*11)*(11/exp(1))^11

# criando minha função
stirling_formula <- function(n) {
  x <- sqrt(2*pi*n)*(n/exp(1))^n
  return(x)
}

# Simulando probabilidade de sair o número 6 em um dado de 6 faces

# especificando semente, para simulação ser reproduzível
set.seed(234)



# número de amostras
n <- 10000

x <- sample(1:6, 1000000, replace=T)
sum(x == 6)
sum(x == 6)/1000000

# 1000 amostras de uma lançamento de dado de 6 lados
resultado <- sample(1:6, n, replace=TRUE)

# frequência relativade 6 é dada por número de 6 / total de amostras
prob_6 <- sum(resultado == 6)/n

# 16,89%
# 1/6 = 16.6666

# função que calcula aprob de sair um número de um dado de 6 faces
prob_x_dado6 <- function(x, n=10000) {
  resultado <- sample(1:6, n, replace=T)
  sum(resultado == x)/n
}

set.seed(234)
prob_x_dado6(1)

set.seed(234)
prob_x_dado6(2)

# exemplo de lista
x <- list(1, 1:3, data.frame(x=1, y=2), list("a"))
x

# número de amostras

vec_amostra <- c(100, 1000, 10000, 100000, 1000000)

# lista vazia para armazenar os resultados das simulações
resultado_lista <- list()

# vetor vazio para armazenar a frequência relativa de 6
vec_prob6 <- numeric()

set.seed(234)
# loop sobre os tamanhos das amostrar
for ( i in 1:length(vec_amostra)) {
  # n amostras de uma lançamento de dado de 6 lados
  resultado_lista[[i]] <- sample(1:6, vec_amostra[i], TRUE)
  
  # frequência relativade 6 é dada por número de 6 / total de amostras
  vec_prob6[i] <- sum(resultado_lista[[i]] == 6)/vec_amostra[i]
  
}


# explicando laço
sum(1:10)
1+2+3+4+5+6+7+8+9+10

x <- 0
for(i in 1:10) {
  x <- x + i
}
x

for ( j in  1:10) {
  for ( i in 1:10) {
    
  }
}
# abrindo o laço
x <- 0
# para i =1
x <- x + 1
# para i =2
x <- x + 2
# para i = 3
x <- x + 3

# para i =4
x <- x + 4

# para i = 5
x <- x + 5

# para i = 6
x <- x + 6

# para i =7
x <- x + 7

# para i = 8
x <- x + 8

# para i =9
x <- x + 9

# para i = 10
x <- x + 10


print(vec_prob6)
