# aula notunro 16/10 cap 9 MLE

amostra <- rnorm(10, 0 , 1)
mean(amostra)
sd(amostra)

# importando arquivo BES
library(data.table)
library(here)
bes <- fread(here("dados brutos 2024", "BES.csv"))
# bes <- fread("C:\\Users\\fcslab122\\lab-regressao-aula\\lab-regressao-aula\\dados brutos 2024\\BES.csv")


library(tidyverse)
glimpse(bes)

reg <- lm(leave ~age, data = bes)
summary(reg)
