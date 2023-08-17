# aula 2 - noturno 16/08

# vamos importar uma base de dados de pib municipais de 2013, do IBGE

# o arquivo está em formato RDS, que é um formato do R, e disponível no meu github. Para importá-lo direto no R, vamos ler o arquivo com a função url e depois import´-lo com a função readRDS. 

pib_cid <- readRDS(url("https://github.com/mgaldino/book-regression/raw/main/dados/pib_cid.RDS"))

# para visualizar os dados que forma importados, temos várias funções
# glimpse, head e View

library(dplyr) # para glimpse

glimpse(pib_cid)

head(pib_cid)

View(pib_cid)

# dado mtcars
glimpse(mtcars)
head(mtcars)
View(mtcars)


# data wrangling

library(tidyverse)

# digamos que quero o pib total médio e o pib per capita médio
# basta usar o comando summarise, que resume os dados e escolher a função mean.

pib_cid %>%
  summarise(pib_medio = mean(pib_total))

pib_cid %>%
  summarise(pib_per_capita_medio = mean(pib_per_capita))

pib_cid %>%
  summarise(maior_pib = max(pib_total))

# qual cidade tem o maior pib?

pib_cid %>%
  filter(pib_total == max(pib_total))

df <- pib_cid %>%
  filter(pib_total == max(pib_total))

# filtrando por uf SP
pib_cid_sp <- pib_cid %>%
  filter(sigla_uf == "SP")

# criar variável: perc_imposto = impostos/pib_total

df <- pib_cid %>%
  mutate(perc_imposto = round(impostos/pib_total, 2))

df_sp <- df %>%
  filter(nome_munic == "São Paulo")

View(df_sp)

View(df)

glimpse(pib_cid_sp)


View(df)



df <- pib_cid %>%
  summarise(pib_medio = mean(pib_total),
            pib_per_capita_medio = mean(pib_per_capita))



### Group by

pib_cid %>%
  group_by(sigla_uf) %>%
  summarise(pib_medio_uf = mean(pib_total))

df <- pib_cid %>%
  group_by(sigla_uf) %>%
  summarise(soma_pib_munic_uf = sum(pib_total),
         soma_impostos_munic_uf = sum(impostos),
         perc_impostos_uf = soma_impostos_munic_uf/soma_pib_munic_uf)

View(df)

# qual é o menor PIB e qual o município que tem o menor PIB
# dica: min() retorna o menor

df <- pib_cid %>%
  filter(pib_total == min(pib_total))

View(df)
