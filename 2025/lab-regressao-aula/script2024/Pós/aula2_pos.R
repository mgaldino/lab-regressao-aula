# R como calculadora

1+1
2*3
4/3

# Tipos de objetos

x <- 3
x = 3

meu_vetor <- c(1,2,3)
minha_matriz <- matrix(1:4, nrow=2)

meu_data_frame <- data.frame(x=1:2, y=1:4)

# datas

minha_data <- "2024-02-21"
class(minha_data)
minha_data1 <- as.Date(minha_data)
minha_data1 + 0:9

minha_data_outro_form <- "21/02/2024"
as.Date(minha_data_outro_form, format = "%d/%m/%Y")
?as.Date

install.packages("data.table")
library(data.table)
?fread
"arquivo.csv"

banco_exemplo <- fread("C:/Users/fcslab122/lab-regressao-aula/lab-regressao-aula/Dados2024/arquivo.csv")
# pega o caminho/pasta atual
getwd()

install.packages("here")
library(here)

banco_exemplo <- fread(here("Dados2024", "arquivo.csv"))

# importando dados exemplo
# vamos importar uma base de dados de pib municipais de 2013, do IBGE

# o arquivo está em formato RDS, que é um formato do R, e disponível no meu github. Para importá-lo direto no R, vamos ler o arquivo com a função url e depois import´-lo com a função readRDS. 

pib_cid <- readRDS(url("https://github.com/mgaldino/book-regression/raw/main/dados/pib_cid.RDS"))


# para visualizar os dados que forma importados, temos várias funções
# glimpse, head e View

library(dplyr) # para glimpse

# summarise
pib_cid %>%
  summarise(pib_medio = mean(pib_total))

# atribuindo a um objeto
pib_medio <- pib_cid %>%
  summarise(pib_medio = mean(pib_total))

## mutate
pib_cid2 <- pib_cid %>%
  mutate(pib_sem_imposto = pib_total - impostos)

pib_cid <- pib_cid %>%
  mutate(pib_sem_imposto = pib_total - impostos)

# selecionando variávels/colunas
pib_cid_simples <- pib_cid %>%
  select(ano, nome_munic, sigla_uf, pib_total,
         pib_per_capita, pib_sem_imposto, impostos)

#filtrando
pib_cid_simples_sp <- pib_cid_simples %>%
  filter(sigla_uf == "SP")


pib_medio_uf <- pib_cid_simples %>%
  group_by(sigla_uf) %>%
  summarise(pib_medio = mean(pib_total))

pib_padronizado_uf <- pib_cid_simples %>%
  group_by(sigla_uf) %>%
  mutate(pib_padronizado_uf =
           (pib_total - mean(pib_total))/sd(pib_total))
