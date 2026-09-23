# Simulação

#números aleatórios

# binomial

rbinom(n=1,size=1, p=.5)

# especificando a semente
set.seed(234)
rbinom(n=1,size=1, p=.5)

# uniforme

runif(n=10, min=0, max=10)
punif(5, min=0, max=10)

# normal
altura <- rnorm(n=1000, mean=170, sd = 15)
hist(altura, breaks=30)

# probabilidade de observar menor que a média?
pnorm(170, mean=170, sd = 15) # 50%

# probabilidade de observar valor menor que média + 2dp
pnorm(200, mean=170, sd = 15)

# probabilidade de observar valor maior que média + 2dp
1 - pnorm(200, mean=170, sd = 15)

# A partir de qual valor a prob é menor do que q?
qnorm(q=.025, mean=0, sd=1)

# gerem 5 simulações de uma normal padrão ccom 100 obs
# Defina o número de histogramas e o tamanho da amostra
n_histogramas <- 5
n_observacoes <- 100

# Configure o layout do gráfico para um painel de 5 histogramas
par(mfrow = c(1, n_histogramas))  # Altere para c(1, n_histogramas) para uma linha de histogramas

# Crie os histogramas
for (i in 1:n_histogramas) {
  # Gere uma amostra da normal padrão
  amostra <- rnorm(n_observacoes)
  
  # Plote o histograma
  hist(amostra, main = paste("Histograma", i), xlab = "Valores", ylab = "Frequência", col = "lightblue", border = "black")
}

# Restaure o layout gráfico padrão
par(mfrow = c(1, 1))

par(mfrow = c(1,5))
hist(x1)
hist(x2)
hist(x3)
hist(x4)
hist(x5)

set.seed(1234)
democracia <- rbinom(200, 1, .4)
gdp_pc <- 500 + 1000*rlnorm(200, sdlog = 1.3) + 
  500*democracia

bd_pib_dem <- data.frame(id = 1:200, 
                         democracia = democracia,
                         pib_percapita = gdp_pc)

library(ggplot2)
library(tidyverse)
bd_pib_dem %>%
  ggplot(aes(y=pib_percapita, x=democracia, group=democracia)) +
  geom_boxplot()

# regressão linear

reg <- lm(pib_percapita ~ democracia, data=bd_pib_dem)
summary(reg)

## Aumentar o n

# simulação aumentando o n
# de 200 até 2000, de 100 em 100
amostras <- seq(from=200, to=2000, by=100)

democracia <- numeric()
gdp_pc <- numeric()
pvalor <- numeric()
for ( i in 1:19){
  democracia <- rbinom(amostras[i], 1, .4)
  gdp_pc <- 500 + 
    1000*rlnorm(amostras[i], sdlog = 1.3) + 
    500*democracia
  
  bd_pib_dem <- data.frame(
    id = 1:amostras[i],
    democracia = democracia,
    pib_percapita = gdp_pc)
  
  reg <- lm(pib_percapita ~ democracia, 
                      data=bd_pib_dem)
  resultado <- summary(reg)
  pvalor[i] <- resultado$coefficients["democracia",
                                   "Pr(>|t|)"]

}

bd_pvalor <- data.frame(pvalor=pvalor, n=amostras)

bd_pvalor %>%
  ggplot(aes(x=n, y=pvalor)) + geom_point()

# amostras repetidas de tamanho igual

democracia <- numeric()
gdp_pc <- numeric()
pvalor <- numeric()
for ( i in 1:1000){
  democracia <- rbinom(200, 1, .4)
  gdp_pc <- 500 + 
    1000*rlnorm(200, sdlog = 1.3) + 
    500*democracia
  
  bd_pib_dem <- data.frame(
    id = 1:200,
    democracia = democracia,
    pib_percapita = gdp_pc)
  
  reg <- lm(log(pib_percapita) ~ democracia, 
            data=bd_pib_dem)
  resultado <- summary(reg)
  pvalor[i] <- resultado$coefficients["democracia",
                                      "Pr(>|t|)"]
  
}

bd_pvalor <- data.frame(pvalor=pvalor,
                        iter_sim = 1:1000)

bd_pvalor %>%
  ggplot(aes(x=iter_sim, y=pvalor)) + geom_point()

# tamanho da lista
length(lista_democracia[[19]])

# fim da simulação






reg1 <- lm(log(pib_percapita) ~ democracia, data=bd_pib_dem)
summary(reg1)

set.seed(123)
vdem <- rpois(200, 4)

gdp_pc <- 500 + 1000*rlnorm(200, sdlog = 1.3) + 
  100*vdem

bd_pib_dem1 <- data.frame(id = 1:200, 
                         democracia = vdem,
                         pib_percapita = gdp_pc)

bd_pib_dem1 %>%
  ggplot(aes(x=democracia, y=pib_percapita)) + geom_point()


bd_pib_dem1 %>%
  ggplot(aes(x=democracia, y=log(pib_percapita))) + geom_point()
