# aula 3 pós greve

n <- 1000
alpha <- 2
beta <- -2
e <- rnorm(n)
x <- rnorm(n)
y <- alpha + beta*x + e
reg <- lm(y ~ x)
summary

n <- 1000
sim_t <- rt(n, df=8)
hist(sim_t)

# 3 df

n <- 1000
sim_t <- rt(n, df=3)
hist(sim_t)

# 20 df

n <- 1000
sim_t <- rt(n, df=203)
hist(sim_t)

# exercício

# criando vetor para guardar os sd
meu_dp <- numeric()

# vetor com os graus de liberdade
df <- 1:100

# tamanho da amostra
n <- 1000

# primeira iteração
meu_dp[1] <- sd(rt(n, df[1]))

#segunda iteração
meu_dp[2] <- sd(rt(n, df[2]))

#terceira iteração
meu_dp[3] <- sd(rt(n, df[3]))

for(i in 1:100) {
  meu_dp[i] <- sd(rt(n, df[i]))
}

df_sim <- data.frame(dp = meu_dp, df = df)

library(ggplot2)
library(tidyverse)
df_sim %>%
  ggplot(aes(x=df, y=dp)) + geom_line() +
  geom_hline(yintercept=1)

?rnorm

qnorm(0.025)
qnorm(.975)

qt(0.025, df=10)
qt(0.025, df = 37)
qt(0.025, df=798)

## inferência preditiva
getwd()
setwd("C:\\Users\\fcslab122\\lab-regressao-aula")
library(here)
library(data.table)

library(data.table)
library(janitor)
#read data1.csv into data frame
presid_18 <- fread("votacao_secao_2018_BR.csv",
encoding = "Latin-1")

# Supondo que seu dataframe seja chamado df
df_resultados <- presid_18 %>%
  dplyr::filter(!NR_VOTAVEL %in% c(95,96)) %>%
  group_by(NR_ZONA, CD_MUNICIPIO, SG_UF, NR_VOTAVEL, NR_TURNO) %>%
  summarise(total_votos = sum(QT_VOTOS)) %>%
  pivot_wider(names_from = NR_TURNO, values_from = total_votos, values_fill = 0) %>%
  clean_names() %>%
  group_by(nr_zona, cd_municipio, sg_uf) %>%
  mutate(total_validos_1t = sum(x1),
         total_validos_2t = sum(x2)) %>%
  dplyr::filter(nr_votavel == 17) %>%
  mutate(percentual_bolso_1t = x1 /total_validos_1t ,
         percentual_bolso_2t = x2 / total_validos_2t)

# remove
# rm(presid_18)

# modelo de regressão

reg1 <- lm(percentual_bolso_2t ~ percentual_bolso_1t, data = df_resultados)
summary(reg1)

# dados de 2022

#read  into data frame
presid_22 <- fread("votacao_secao_2022_BR.csv",
                   encoding = "Latin-1")

df_resultados_22 <- presid_22 %>%
  dplyr::filter(!NR_VOTAVEL %in% c(95,96)) %>%
  group_by(NR_ZONA, CD_MUNICIPIO, SG_UF, NR_VOTAVEL, NR_TURNO) %>%
  summarise(total_votos = sum(QT_VOTOS)) %>%
  pivot_wider(names_from = NR_TURNO, values_from = total_votos, values_fill = 0) %>%
  clean_names() %>%
  group_by(nr_zona, cd_municipio, sg_uf) %>%
  mutate(total_validos_1t = sum(x1),
         total_validos_2t = sum(x2)) %>%
  dplyr::filter(nr_votavel == 22) %>%
  dplyr::filter(total_validos_1t >0) %>%
  mutate(percentual_bolso_1t = x1 /total_validos_1t ,
         percentual_bolso_2t = x2 / total_validos_2t)

df_resultados_22 <- df_resultados_22 %>%
  mutate(y_prev_2t = coef(reg1)[1] + coef(reg1)[2]*percentual_bolso_1t,
         validos_previsto = total_validos_1t*y_prev_2t)

# previsão do resultado eleitoral antes de observar apuração do 2t, supondo comparecimento igual ao 1t
df_resultados_22 %>%
  ungroup() %>%
  summarise(total_bolso = sum(validos_previsto),
            total_valido_previsto = sum(total_validos_1t),
            perc_previsto = total_bolso/total_valido_previsto)

## incerteza
# mesma coisa

#E incerteza nas previsões?

previsoes <- predict(reg1, newdata = df_resultados_22, interval = "prediction", level = .95) %>%
  as.data.frame()

df_resultados_22 <- df_resultados_22 %>%
  ungroup() %>%
  mutate(prev_perc = previsoes$fit,
         prev_perc_lower = previsoes$lwr,
         prev_perc_upper = previsoes$upr,
         validos_prev = total_validos_1t*prev_perc,
         validos_prev_lower = total_validos_1t*prev_perc_lower,
         validos_prev_upper = total_validos_1t*prev_perc_upper)

df_resultados_22 %>%
  summarise(perc_previsto = sum(validos_prev)/sum(total_validos_2t),
            perc_previsto_lower = sum(validos_prev_lower)/sum(total_validos_2t),
            perc_previsto_upper = sum(validos_prev_upper)/sum(total_validos_2t))
