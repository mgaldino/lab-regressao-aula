# Revisão stat e prob noturno

x <- 1:10
y <- 10:1
a <- 2
b <- 3
# quero mostrar que:
# mean(a*x + b*y) == mean(a*x) + mean(b*y) == a*mean(x) + b*mean(y)

x1 <- a*x
y1 <- b*y

mean(a*x + b*y) # 27.5
mean(a*x) + mean(b*y) # 27.5
a*mean(x) + b*mean(y) # 27.5

# e
exp(1)

# raiz quadrada
4^(1/2)
sqrt(4)

# Fórmula de stirling
# sqrt(2*pi*n)*(n/exp(1))^n

factorial(10)
sqrt(2*pi*10)*(10/exp(1))^10
sqrt(2*pi*20)*(20/exp(1))^20
sqrt(2*pi*30)*(30/exp(1))^30

stirling_formula <- function(n){
  sqrt(2*pi*n)*(n/exp(1))^n
}

stirling_formula(10)

# simular a probabilidade de sair número 6 em um dado de 6 lados
# especificando semente, para simulação ser reproduzível

set.seed(234)

# número de amostras
n <- 10000

# 1000 amostras de uma lançamento de dado de 6 lados
resultado <- sample(1:6, n, replace=TRUE)
head(resultado)

# frequência relativade 6 é dada por número de 6 / total de amostras
prob_6 <- sum(resultado == 6)/n

# 16,89%
# 1/6 = 16.6666








