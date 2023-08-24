# resolução da lista 1 - noturno

# 1 Qual a unid análise?

install.packages("devtools")
devtools::install_github("tbrugz/ribge")

#outra forma de usar intall_github

# library(devtools)
# install_github("tbrugz/ribge")

library(ribge)
pop2020 <- populacao_municipios(2020)

View(pop2020)

# 2
library(tidyverse)

# filtrando pra SP
pop_sp_20 <- pop2020 %>%
  filter(uf == "SP")

# selecionando variáveis
pop_sp_20 <- pop_sp_20 %>%
  select(-populacao_str) %>%
  select(-codigo_uf)

# outra forma de selecionar as variáveis
pop_sp_20 <- pop_sp_20 %>%
  select(-c(populacao_str, codigo_uf))

# renomeando variáveis
pop_sp_20 <- pop_sp_20 %>%
  rename(municipio = nome_munic)

# colocando em caixa baixa
pop_sp_20 <- pop_sp_20 %>%
  mutate(municipio = tolower(municipio))

# fazendo tudo de uma vez
pop_sp_20 <- pop2020 %>%
  filter(uf == "SP") %>%
  select(-c(populacao_str, codigo_uf)) %>%
  rename(municipio = nome_munic) %>%
  mutate(municipio = tolower(municipio))

# Quantos munic
# qual o menor e quantos hab

pop_sp_20 %>%
  filter(populacao == min(populacao))

# borá com 838 habitantes

# 3
pop_sp_20 %>%
  summarise(media = mean(populacao),
            mediana = median(populacao),
            variancia = var(populacao),
            desvio_padrao = sd(populacao)) %>%
  knitr::kable(caption = "Estatísticas populacionais de SP em 2020")

# 4

library(ggplot2)
pop_sp_20 %>%
  ggplot(aes(populacao)) + geom_density() +
  scale_x_continuous(labels = scales::dollar) +
  scale_y_continuous(labels = scales::percent)

# Observo uma distribuição concentrada em torno de valores baixos, próximos do zero
# com uma cauda longa
# A mediana é melhor que a média, pois é robusta a valores extremos, como São Paulo

# 5 menos de 50k hab

pop_sp_20 %>%
  filter(populacao < 50000) %>%
  ggplot(aes(populacao)) + geom_density() +
  scale_x_continuous(labels = scales::dollar) +
  scale_y_continuous(labels = scales::percent)
# consigo observar melhor a distribuição dos municípios menores
# antes pela escala, não conseguia ver como era a distribuição desses municípios
pop_sp_20 %>%
  filter(populacao < 50000) %>%
  summarise(n())
# Há 504 municipios com menos de 50k habitantes

# log

pop_sp_20 %>%
  filter(populacao < 50000) %>%
  mutate(populacao_log = log(populacao)) %>%
  ggplot(aes(populacao_log)) + geom_density() +
  scale_x_continuous(labels = scales::dollar) +
  scale_y_continuous(labels = scales::percent)
  
pop_sp_20 %>%
  mutate(populacao_log = log(populacao)) %>%
  ggplot(aes(populacao_log)) + geom_density() +
  scale_x_continuous(labels = scales::dollar) +
  scale_y_continuous(labels = scales::percent)

# 7

pop2020 %>%
  group_by(uf) %>%
  summarise(media_por_uf = mean(populacao)) %>%
  filter(media_por_uf == max(media_por_uf))

pop2020 %>%
  group_by(uf) %>%
  summarise(media_por_uf = mean(populacao)) %>%
  filter(media_por_uf == min(media_por_uf))

pop2020 %>%
  group_by(uf) %>%
  summarise(media_por_uf = mean(populacao)) %>%
  filter(media_por_uf %in% c(max(media_por_uf), min(media_por_uf)))

# explicando %in%
x <- 1:5
y <- 1:10
y %in% x


