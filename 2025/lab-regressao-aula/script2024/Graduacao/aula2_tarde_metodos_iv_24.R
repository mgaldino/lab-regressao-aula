# R como calculadora

1+1 # soma
2*3 # multiplicação/produto
4/2 # divisão
2^3 # exponenciação
log(-1) # logaritmo
log("a")
1/0

# objetos
pi
options(digits = 10) # aumentando para 10 casas decimais a impressão
pi
print("Aula de métodos IV")
print(pi)

objeto <- 1
objeto = 1

objeto + 1
numero_1 <- 1
numero_1 + 1
1 <- 2
pi <- 2
pi
rm(pi)
pi
rm(list=ls())

# funções
sum(1:10)
help("sum")
?sum
vetor_com_missing <- c(1:10, NA)
print(vetor_com_missing)
sum(vetor_com_missing)
sum(vetor_com_missing, na.rm=TRUE)
sum()

# Datas

## explicando data rapidamente
data_ex <- "2020-10-23"

class(data_ex) # tipo do objeto

data_ex1 <- as.Date(data_ex)
class(data_ex1)

data_ex + 1 # erro

data_ex1 + 1 # dá certo
data_ex1 - 1 # subtrair um dia
data_ex1 + 0:9

# 19 de agosto de 2024 foi uma segunda
# Quero um vetor de datas de dias da semana em agosto
data_base <- as.Date("2024-08-19")
dias_semana_agosto <- data_base + c(-18:-17 ,-14:-10, -7:-3 ,0:4, 7:11, 14)
dias_semana_agosto

primeiro_agosto <- as.Date("2024-08-01")
ultimo_agosto <- as.Date("2024-08-31")

# adaptando código do chatgpt
datas_agosto <- seq.Date(primeiro_agosto, ultimo_agosto, by = "day")
sabados <- seq(3, 31, 7)
domingos <- seq(4, 31, 7)
agosto_dias_semana <- datas_agosto[-c(sabados,domingos )]

# Exemplos
install.packages("data.table")
library(data.table)

## Importando dados
# vamos importar uma base de dados de pib municipais de 2013, do IBGE

# o arquivo está em formato RDS, que é um formato do R, e disponível no meu github. Para importá-lo direto no R, vamos ler o arquivo com a função url e depois import´-lo com a função readRDS. 

pib_cid <- readRDS(url("https://github.com/mgaldino/book-regression/raw/main/dados/pib_cid.RDS"))

# para visualizar os dados que forma importados, temos várias funções
# glimpse, head e View

library(dplyr) # para glimpse
dplyr::glimpse(pib_cid)


## Data Wrangling

# digamos que quero o pib total médio e o pib per capita médio
# basta usar o comando summarise, que resume os dados e escolher a função mean.

df <- pib_cid %>%
  summarise(pib_medio = mean(pib_total),
            pib_per_capita_medio = mean(pib_per_capita))

df


# filtro

df_sp <- pib_cid %>%
  dplyr::filter(sigla_uf == "SP") %>%
  summarise(media_pib = mean(pib_total))

df_sp


# group by
df_uf <- pib_cid %>%
  group_by(sigla_uf) %>%
  summarise(pib_medio = mean(pib_total),
            pib_per_capita_medio = mean(pib_per_capita))


df_uf
print(df_uf, n=27)

df_uf %>%
  print(n=27)

## mutate

pib_cid <- pib_cid %>%
  mutate(pib_sem_impostos = pib_total - impostos)

pib_cid %>%
  glimpse()

pib_cid %>%
  mutate(check_variaveis = pib_sem_impostos - vab_total) %>%
  summarise(sum(check_variaveis))

df_validacao <- pib_cid %>%
  mutate(check_variaveis = pib_sem_impostos - vab_total) %>%
  filter(check_variaveis != 0) %>%
  dplyr::select(sigla_uf, nome_munic,check_variaveis,
                pib_sem_impostos, vab_total )


df_validacao <- pib_cid %>%
  mutate(check_variaveis = all.equal(pib_sem_impostos,vab_total)) %>%
  filter(check_variaveis == FALSE) %>%
  dplyr::select(sigla_uf, nome_munic,check_variaveis,
                pib_sem_impostos, vab_total )

pib_cid %>%
  filter(pib_per_capita > 700000) %>%
  View()

# plot

library(ggplot2)

pib_cid %>%
  ggplot(aes(y=pib_total, x=impostos)) + geom_point() +
  scale_y_continuous(labels = scales::dollar)

pib_cid %>%
  ggplot(aes(y=pib_total, x=impostos)) + geom_point() +
  scale_y_continuous(labels = 
                       scales::label_number(
                         big.mark = ".",
                         decimal.mark = ",")) +
  scale_x_continuous(labels = 
                       scales::label_number(
                         big.mark = ".",
                         decimal.mark = ",")) +
  theme_light() +  theme(text=element_text(size=20)) +
  xlab("Impostos municipais") + ylab("PIB municipal") +
  ggtitle("PIB municipal de 2013 x impostos municipais")

# adicionando nomes dos municípios
pib_cid %>%
  ggplot(aes(y = pib_total, x = impostos, label = nome_munic)) +
  geom_text(size = 3, alpha=.7) + # Ajuste o tamanho do texto conforme necessário
  scale_y_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
  scale_x_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
  theme_light() +
  theme(text = element_text(size = 20)) +
  xlab("Impostos municipais") +
  ylab("PIB municipal") +
  ggtitle("PIB municipal de 2013 x impostos municipais")

# adicionando uf dos municípios
pib_cid %>%
  ggplot(aes(y = pib_total, x = impostos, label = sigla_uf)) +
  geom_text(size = 3, alpha=.5) + # Ajuste o tamanho do texto conforme necessário
  scale_y_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
  scale_x_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
  theme_light() +
  theme(text = element_text(size = 20)) +
  xlab("Impostos municipais") +
  ylab("PIB municipal") +
  ggtitle("PIB municipal de 2013 x impostos municipais")

# zoom em pib < 100mi
pib_cid %>%
  filter(pib_total < 100000 & impostos < 60000 & impostos > 0) %>%
  ggplot(aes(x = pib_total, y = impostos, label = sigla_uf)) +
  geom_text(size = 3, alpha=.5) + # Ajuste o tamanho do texto conforme necessário
  scale_y_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
  scale_x_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
  theme_light() +
  theme(text = element_text(size = 20)) +
  ylab("Impostos municipais") +
  xlab("PIB municipal") +
  ggtitle("PIB municipal de 2013 x impostos municipais") +
  geom_smooth()
