Aula diurno 4

library(PNADcIBGE)
# Importe os dados desejados
data <- get_pnadc(year=2017,
                  quarter=4,
                  vars=c("Ano", "Trimestre", "UF", "V2007", 
                         "VD4020", "VD4035", "V1028"),design=FALSE,
                  savedir=tempdir())

library(tidyverse)
library(tidylog)
data <- data %>%
  select(Ano, Trimestre, UF, V2007, VD4020, VD4035, V1028)

data <- data %>%
  rename(genero = V2007,
         renda = VD4020,
         horas_trabalhadas = VD4035)

## questão 3
a <- 2
b <- 3

data <- data %>%
  filter(!is.na(Renda)) %>%
  filter(!is.na(Horas_trabalhadas))

mean(a*data$Renda + b*data$Horas_trabalhadas) == a*mean(data$Renda) + b*mean(data$Horas_trabalhadas)
left_hand <- mean(a*data$Renda + b*data$Horas_trabalhadas)
right_hand <- a*mean(data$Renda) + b*mean(data$Horas_trabalhadas)
all.equal(left_hand, right_hand)

## Loop ou laço

# imprime i 10x
for ( i in 1:10) {
  print(i)
}

# imprime i 10x, mas devagar
for ( i in 1:10) {
  print(i)
  Sys.sleep(.05)
}

n <- 10
# loop infinito - fail, só que não
for ( i in 1:n) {
  print(i)
  n <- n + 1
}

# semente
set.seed(36)
# número de jogadas/simulações
n <- 1000

# vetor X, para armazenar o resultado de cada uma das n jogadas
X <- numeric()
# x <- rep(NA, n)

# simulando n vezes
for( i in 1:n){
  X[i] <- sample(1:4, size=1)
}

# visualizando as primeiras 20 jogadas
head(X, 20)

# prob X = 1
sum(X==1)/n #23%

# prob X = 2
sum(X==2)/n # 27%

# prob X = 3
sum(X==3)/n # 23,5% ou 24%


# prob X = 4
sum(X==4)/n #27%

#análise de sensibilidade

# número de jogadas/simulações
n <- 1000

# vetor X, para armazenar o resultado de cada uma das n jogadas
X <- numeric()

# número de replicações da simulação
k <- 100

# vetor para armazenar o erro medio
erro_medio <- numeric()

# simulando n vezes
for (j in 1:k) {
  for( i in 1:n){
    X[i] <- sample(1:4, size=1)
  }
  p1 <- sum(X==1)/n
  p2 <- sum(X==2)/n
  p3 <- sum(X==3)/n
  p4 <- sum(X==4)/n
  erro_medio[j] = (abs(p1 - .25) + abs(p2 - .25) + abs(p3 - .25) + abs(p3 - .25)) /4
}

summary(erro_medio)


#Calculando o índice de gini para a desigualdade salarial
# por gênero

df <- data %>%
  rename(horas_trabalhadas = horaS_trabalhadas) %>%
  filter(!is.na(renda)) %>%
  filter(!is.na(horas_trabalhadas)) %>%
  filter(renda > 0) %>%
  filter(horas_trabalhadas > 0) %>%
  mutate(salario = renda/(4.5*horas_trabalhadas)) %>%
  mutate(log_salario = log(salario)) %>%
  mutate(genero = as.character(genero))

# install.packages("dineq")
library(dineq)

df %>%
  group_by(genero) %>%
  summarise(dp = sd(salario),
            dp_log = sd(log_salario),
            indice_gini = gini.wtd(salario, weights = V1028))

df %>%
  group_by(genero) %>%
  summarise(min(log_salario), max(log_salario))


