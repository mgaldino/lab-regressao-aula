library(PNADcIBGE)
library(tidyverse)
library(ggplot2)
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

data %>%
  group_by(UF, genero) %>%
  summarise(renda_media = mean(renda, na.rm=T)) %>%
  ggplot(aes(y=renda_media, x=reorder(UF, -renda_media), fill=genero)) +
  geom_bar(stat="identity", position="dodge") +
  coord_flip()

# Prob (renda entre 1k e 2k| menos de 20h trabalhadas)
data20 <- data %>%
  filter(horas_trabalhadas <= 20) 


data20 %>%
  summarise(num_obs = n(),
         num_renda_interv = sum(renda > 1000 & renda < 2000, na.rm=T),
         num_renda_interv/num_obs)
    
data20 %>%
  filter(!is.na(renda)) %>%
  summarise(num_obs = n(),
            num_renda_interv = sum(renda > 1000 & renda < 2000),
            prob = num_renda_interv/num_obs)

## Laço

for(i in 1:10) {
  print(i)
}

# laço mais devagar
for(i in 1:10) {
  print(i)
  Sys.sleep(1)
}

# laço - sorteio de uma amostra

set.seed(800)
# número de jogadas/simulações
n <- 1000

# vetor X, para armazenar o resultado de cada uma das n jogadas
X <- numeric()

# simulando n vezes
for( i in 1:n){
  X[i] <- sample(1:4, size=1)
}

# visualizando as primeiras 20 jogadas
head(X, 20)

# prob X = 1
sum(X==1)/n

# prob X = 2
sum(X==2)/n

# prob X = 3
sum(X==3)/n

# prob X = 2
sum(X==4)/n

# prob X = 2
sum(X==4)/n

# resumo
summary(X)

## Fazendo o laço manualmente
x <- numeric()
x[1] <- sample(1:4, 1)
x[2] <- sample(1:4, 1)
x[3] <- sample(1:4, 1)


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
