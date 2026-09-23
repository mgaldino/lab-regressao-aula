 
# soma no R

1+1
2*3
4/2
2^3
log(2)
2*(2^3)

print(pi, digits = 20)

# funções
sum(1,2,3)
mean(c(1,2,3))

# Tipos básicos principais

# numeric para números
x <- 7

# character
y <- "a"
z <- "Manoel Galdino"

z

# vetores
c(1,2,3)

# criando objetos
meu_primeiro_vetor <- c(1,2,3)

meu_primeiro_vetor

meu_segundo_vetor <- c("a", "b", "c")

# help
?mean
help("mean")

# matriz

matrix(c(1,2,3,4), nrow=2)

# armazenando minha matriz
mat <- matrix(c(1,2,3,4), nrow=2)
mat

matrix(1:4, nrow=2)

vetor <- 1:4
matrix(vetor, nrow=2)

# data.frame
data.frame(x=1:2, y=3:4)

primeiro_df <- data.frame(x=1:2,
                          y=3:4,
                          z=c("a", "b"))
primeiro_df
as.matrix(primeiro_df, nrow=2)

# datas
exemplo_data_errado <- "2024-08-21"
class(exemplo_data_errado)

exemplo_data_certo <- as.Date(exemplo_data_errado)
exemplo_data_certo
class(exemplo_data_certo)

# erro
exemplo_data_errado + 1

# dá certo
exemplo_data_certo + 1
exemplo_data_certo + 2

# fortmato de datas
as.Date("21/08/2024", format = "%d/%m/%Y")

# aqui dá erro
as.Date("21/08/24", format = "%d/%m/%Y")


# aqui dá certo
as.Date("210824", format = "%d%m%y")

as.Date("21ago24", format = "%d%b%y")

Sys.getlocale()

# Pacotes
install.packages("data.table")
library(data.table)

# função de leitura/importação
?fread

df_fake <- fread("arquivo.csv")

# obter caminho atual
getwd()
df_fake <- fread("C:/Users/fcslab122/lab-regreessao-aula/arquivo.csv")

# muda o caminho padrão do R
setwd("C:/Users/fcslab122/")

# pacote here
library(here)
df_fake <- fread(here("lab-regressao-aula", "arquivo.csv"))

# importando dados do livro
# vamos importar uma base de dados de pib municipais de 2013, do IBGE

# o arquivo está em formato RDS, que é um formato do R, e disponível no meu github. Para importá-lo direto no R, vamos ler o arquivo com a função url e depois import´-lo com a função readRDS. 

pib_cid <- readRDS(url("https://github.com/mgaldino/book-regression/raw/main/dados/pib_cid.RDS"))

# para visualizar os dados que forma importados, temos várias funções
# glimpse, head e View

library(dplyr) # para glimpse
glimpse(pib_cid)

library(tidyverse)

# primeiro verbo - summarise
pib_cid %>%
  summarise(pib_medio = mean(pib_total))

df_pib_medio <- pib_cid %>%
  summarise(pib_medio = mean(pib_total),
            pib_maximo = max(pib_total),
            pib_dp = sd(pib_total))

# segundo verbo - mutate

pib_cid_transformado <- pib_cid %>%
  mutate(pib_sem_impostos = pib_total - impostos)

# atribuindo ao mesmo objeto
pib_cid <- pib_cid %>%
  mutate(pib_sem_impostos = pib_total - impostos)

# removendo objeto
rm(pib_cid_transformado)

# filtrando
pib_cid_sp <- pib_cid %>%
  dplyr::filter(sigla_uf == "SP")

# selecionando

pib_cid_selecionado <- pib_cid %>%
  dplyr::select(ano, sigla_uf, nome_munic, pib_total, pib_per_capita)

View(pib_cid_selecionado)

# group_by
pib_cid_medio_uf <- pib_cid %>%
  group_by(sigla_uf) %>%
  summarise(pib_medio = mean(pib_total))

View(pib_cid_medio_uf)

# group_by com mutate
pib_cid_uf_pad <- pib_cid %>%
  group_by(sigla_uf) %>%
  mutate(
    pib_pad_uf = 
      (pib_total - mean(pib_total))/sd(pib_total))

View(pib_cid_uf_pad)

# gráficos

library(ggplot2)
pib_cid %>%
  ggplot(aes(x=pib_total, y=impostos)) + geom_point() +
  scale_y_continuous(labels = 
                       scales::label_comma(
                         big.mark = ".",
                         decimal.mark = ",")) + 
  scale_x_continuous(labels = 
                       scales::label_comma(
                         big.mark = ".",
                         decimal.mark = ",")) +
  xlab("PIB") + ylab("Impostos") +
  theme_light()

# pra imprimir mais digitos e evitar notação científica
options(scipen=90)
