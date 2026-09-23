## Modelo de previsão eleitoral do RJ

# carregando bibliotecas
library(data.table)
library(tidyverse)
library(here)
library(janitor)
library(tidyr)
library(ggplot2)

# importação alternativa de 2016
# path <- "C:\\Users\\fcslab122\\lab-regressao-aula\\lab-regressao-aula\\dados brutos 2024"
# 

#  tse_rj <- fread(file=paste(path, 
# "votacao_secao_2016_RJ.csv", sep="\\"))

## importando dados de 2016
tse_rj <- fread(here("dados brutos 2024",
                     "votacao_secao_2016_RJ.csv"),
                encoding = "Latin-1")
# inspecionando os dados de 2016
glimpse(tse_rj)

# deixando os nomes das variáveis mais bonitos
tse_mun_rj <- tse_rj %>%
  clean_names() %>%
  filter(nm_municipio == "RIO DE JANEIRO")

glimpse(tse_mun_rj)
rm(tse_rj) # remove o banco de dados do R

View(tse_mun_rj)

# excluindo vereadores
tse_mun_rj_prefeito <- tse_mun_rj %>%
  filter(ds_cargo == "PREFEITO")

View(tse_mun_rj_prefeito)

# excluindo brancos e nulos
# computado total de votos por seção
# filtrando só Crivella e Freixo

tse_mun_rj_prefeito1 <- tse_mun_rj_prefeito %>%
  filter(!grepl("BRANCO|NULO", nm_votavel)) %>%
  group_by(nr_zona, nr_secao, nr_turno) %>%
  mutate(total_validos_secao = sum(qt_votos)) %>%
  filter(grepl("FREIXO|CRIVELLA", nm_votavel)) %>%
  mutate(per_validos = qt_votos/total_validos_secao)

View(tse_mun_rj_prefeito1)

# transformando de formato longo para wide
tse_mun_rj_prefeito_wide <- tse_mun_rj_prefeito1 %>%
  dplyr::select(nm_votavel, nr_zona, 
         nr_secao, nr_turno,per_validos) %>%
  pivot_wider(names_from = nr_turno, values_from=per_validos ) %>%
  rename(perc_validos_1t = "1", 
         perc_validos_2t = "2") %>%
  ungroup() %>%
  group_by(nr_zona, nr_secao) %>%
  arrange(nm_votavel) %>%
  mutate(perc_voto_1t_lead = lead(perc_validos_1t))

# Ficando só com dados do crivella
tse_mun_rj_crivella <- tse_mun_rj_prefeito_wide %>%
  filter(grepl("CRIVELLA", nm_votavel)) %>%
  mutate(dif_crivella_oponente = perc_validos_1t - perc_voto_1t_lead)

# regressão

# mopdelo 0
reg <- lm(perc_validos_2t ~ perc_validos_1t,
          data= tse_mun_rj_crivella)

# modelo 1
reg1 <- lm(perc_validos_2t ~ perc_validos_1t +
             dif_crivella_oponente,
          data= tse_mun_rj_crivella)
summary(reg)
summary(reg1)

# gráfico scatter
tse_mun_rj_crivella %>%
  ggplot(aes(x=perc_validos_1t, y=perc_validos_2t)) +
  geom_point() + geom_smooth(method="lm")

###########################
# importando dados de 2020
############################

tse_rj20 <- fread(here("dados brutos 2024",
                     "votacao_secao_2020_RJ.csv"),
                encoding = "Latin-1")

tse_mun_rj20 <- tse_rj20 %>%
  clean_names() %>%
  filter(nm_municipio == "RIO DE JANEIRO")

rm(tse_rj20) # remove o banco de dados do R

tse_mun_rj_prefeito20 <- tse_mun_rj20 %>%
  filter(ds_cargo == "Prefeito")

tse_mun_rj_prefeito20a <- tse_mun_rj_prefeito20 %>%
  filter(!grepl("BRANCO|NULO", nm_votavel)) %>%
  group_by(nr_zona, nr_secao, nr_turno) %>%
  mutate(total_validos_secao = sum(qt_votos)) %>%
  filter(grepl("PAES|CRIVELLA", nm_votavel)) %>%
  mutate(per_validos = qt_votos/total_validos_secao) 

View(tse_mun_rj_prefeito20a)

tse_mun_rj_prefeito_wide20 <- tse_mun_rj_prefeito20a %>%
  dplyr::select(nm_votavel, nr_zona,
                nr_secao, nr_turno, per_validos, total_validos_secao) %>%
  pivot_wider(names_from = nr_turno, 
              values_from= c(per_validos,total_validos_secao)) %>%
  rename(perc_validos_1t = "per_validos_1", 
         perc_validos_2t = "per_validos_2") %>%
  ungroup() %>%
  group_by(nr_zona, nr_secao) %>%
  arrange(desc(nm_votavel)) %>%
  mutate(perc_voto_1t_lead = lead(perc_validos_1t))

tse_mun_rj_crivella20 <- tse_mun_rj_prefeito_wide20 %>%
  filter(grepl("CRIVELLA", nm_votavel))  %>%
  mutate(dif_crivella_oponente = perc_validos_1t - perc_voto_1t_lead)

###############################
# prevendo 20 a partir de 2016

# computado previsões a partir das regressões
tse_mun_rj_crivella20 <- tse_mun_rj_crivella20 %>%
  ungroup() %>%
  mutate(perc_2t_previsto_modelo1 = 
           coef(reg)[1] + 
           coef(reg)[2]*perc_validos_1t,
         perc_2t_previsto_modelo2 = 
           coef(reg1)[1] + 
           coef(reg1)[2]*perc_validos_1t +
           coef(reg1)[3]*dif_crivella_oponente)

tse_mun_rj_crivella20 <- tse_mun_rj_crivella20 %>%
  mutate(previsao_voto1 = perc_2t_previsto_modelo1*total_validos_secao_2,
         previsao_voto2 = perc_2t_previsto_modelo2*total_validos_secao_2)

# Resultado agregado previsto
tse_mun_rj_crivella20 %>%
  ungroup() %>%
  summarise(resultado_modelo1 = sum(previsao_voto1)/sum(total_validos_secao_2),
            resultado_modelo2 = sum(previsao_voto2)/sum(total_validos_secao_2))


tse_mun_rj_crivella20 <- tse_mun_rj_crivella20 %>%
  ungroup() %>%
  mutate(erro_sq1 = (perc_validos_2t - 
                      perc_2t_previsto1)^2,
         erro_sq2 = (perc_validos_2t - 
                       perc_2t_previsto2)^2)

tse_mun_rj_crivella20 %>%
  summarise(eqm1 = sum(erro_sq1)/n(),
            eqm2 = sum(erro_sq2)/n())
# 0.0382

# votação do crivela real 35.89%

tse_mun_rj_crivella20 %>%
  ggplot(aes(x=perc_validos_2t, y=perc_2t_previsto)) + 
  geom_point() + geom_abline(intercept = 0, slope=1)
