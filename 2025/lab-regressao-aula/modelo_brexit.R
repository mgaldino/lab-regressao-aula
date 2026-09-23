beta_chapeu <- rnorm(n=1000, 10, 20 )
hist(beta_chapeu, breaks = 50)

# obtém o caminho diretório atual do R
getwd()

# carrega bibliotecas
library(data.table)
library(tidyverse)

# importando banco BES
bes <- fread("BES.csv")
# inspecionado o banco bes
glimpse(bes)

# roda regresão linear
reg <- lm(leave ~ age, data = bes)

# resume a regressão
summary(reg)

# tirar notação científica
options(scipen = 99)

# IC
0.0072082 + 2*0.0001739
0.0072082 -  2*0.0001739
