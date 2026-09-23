# importando dados do Brexit

# salvasr arquivo bes.csv, do site: https://simonweschle.github.io/psc400.html
#está na week 3.
# salvar na pasta onde o R está apontando
# para descobrir a pasata, rodar
getwd()

library(data.table)
bes <- fread("BES.csv")

# forma alternativa de importar
# library(readr)
# BES <- read_csv("BES.csv")

# regressão linear
reg <- lm(leave ~ age, data=bes, na.action = na.omit)

# resumo da regressão
summary(reg)

# simulando da Normal com parâmetros da nula
sim_nula <- rnorm(10000, mean =0, sd = 0.0001739)

# evita notação científica
options(scipen = 99)

# histograma
hist(sim_nula)

# Intervalo de Confiança
0.1194782 + 0.0072082*40
# IC
0.1194782 + (0.0072082 + 2*0.0001739)*40
0.1194782 + (0.0072082 - 2*0.0001739)*40
