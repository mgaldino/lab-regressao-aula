## Modelo preditivo de voto no 2t RJ

#Carreganbdo bibliotecas

library(data.table)
library(tidyverse)
library(janitor)
library(tidyr)
library(here)

# tse_rj <- fread("C:\\Users\\fcslab122\\lab-regressao-aula\\lab-regressao-aula\\dados brutos 2024\\votacao_secao_2016_RJ.csv")

tse_rj <- fread(here("dados brutos 2024",
                     "votacao_secao_2016_RJ.csv"),
                encoding = "Latin-1")
glimpse(tse_rj)

#filtra só município do RJ
tse_mun_rj <- tse_rj %>%
  clean_names() %>% # arruma nome das variáveis
  filter(nm_municipio == "RIO DE JANEIRO")

glimpse(tse_mun_rj)

#remove banco
rm(tse_rj)

View(tse_mun_rj)
tse_mun_rj_pref <- tse_mun_rj %>%
  filter(ds_cargo != "VEREADOR")

View(tse_mun_rj_pref)


tse_mun_rj_pref_valid <- tse_mun_rj_pref %>%
  filter(!nr_votavel %in% c(95, 96, 97)) %>% # filtra votos válidos
  group_by(nr_turno, nr_zona, nr_secao) %>%
  mutate(qtde_validos = sum(qt_votos)) %>% # calcula total votos válidos
  dplyr::select(nm_votavel, nr_turno, nr_zona, nr_secao,
                qt_votos, qtde_validos) %>% # seleciona variáveis
  mutate(perc_validos = qt_votos/qtde_validos)# calacula percentual de válidos

# transforma de longo para wide
tse_mun_rj_pref_valid_wide <- tse_mun_rj_pref_valid %>%
  pivot_wider(names_from = nr_turno, 
              values_from = c(qt_votos, qtde_validos, perc_validos))

# filtra pra ficar só com Crivella
# tse_mun_rj_pref_valid_wide_criv <- tse_mun_rj_pref_valid_wide %>%
#   filter(grepl("CRIVELLA", nm_votavel))

tse_mun_rj_pref_valid_wide <- tse_mun_rj_pref_valid_wide %>%
  filter(grepl("FREIXO|CRIVELLA", nm_votavel)) %>%
  group_by(nr_zona, nr_secao) %>%
  arrange(nm_votavel) %>%
  mutate(aux = lead(perc_validos_1),
         dif_votos = perc_validos_1 - aux)

tse_mun_rj_pref_valid_wide_criv <- tse_mun_rj_pref_valid_wide %>%
  filter(grepl("CRIVELLA", nm_votavel))

# grepl("ar", c("carro", "arma", "ar", "amor"))

# Regressão linear

reg1 <- lm(perc_validos_2 ~ perc_validos_1,
           data= tse_mun_rj_pref_valid_wide_criv)
summary(reg1)

reg2 <- lm(perc_validos_2 ~ perc_validos_1 +
             dif_votos,
           data= tse_mun_rj_pref_valid_wide_criv)
summary(reg2)
# plotando os dados
tse_mun_rj_pref_valid_wide_criv %>%
  ggplot(aes(y=perc_validos_2, x=perc_validos_1)) +
  geom_point(colour= "grey") + geom_abline(intercept = 0.367854,
                              slope = 0.784869,
                             colour = "blue")
##############
## Dados 2020
#################


tse_rj20 <- fread(here("dados brutos 2024",
                     "votacao_secao_2020_RJ.csv"),
                encoding = "Latin-1")
glimpse(tse_rj20)

#filtra só município do RJ
tse_mun_rj20 <- tse_rj20 %>%
  clean_names() %>% # arruma nome das variáveis
  filter(nm_municipio == "RIO DE JANEIRO")

glimpse(tse_mun_rj20)

#remove banco
rm(tse_rj20)

View(tse_mun_rj20)
tse_mun_rj_pref20 <- tse_mun_rj20 %>%
  filter(ds_cargo != "Vereador")

View(tse_mun_rj_pref20)


tse_mun_rj_pref_valid20 <- tse_mun_rj_pref20 %>%
  filter(!nr_votavel %in% c(95, 96, 97)) %>% # filtra votos válidos
  group_by(nr_turno, nr_zona, nr_secao) %>%
  mutate(qtde_validos = sum(qt_votos)) %>% # calcula total votos válidos
  dplyr::select(nm_votavel, nr_turno, nr_zona, nr_secao,
                qt_votos, qtde_validos) %>% # seleciona variáveis
  mutate(perc_validos = qt_votos/qtde_validos)# calacula percentual de válidos

# transforma de longo para wide
tse_mun_rj_pref_valid_wide20 <- tse_mun_rj_pref_valid20 %>%
  pivot_wider(names_from = nr_turno, 
              values_from = c(qt_votos, qtde_validos, perc_validos))

# filtra pra ficar só com Crivella

tse_mun_rj_pref_valid_wide20 <- tse_mun_rj_pref_valid_wide20 %>%
  filter(grepl("PAES|CRIVELLA", nm_votavel))%>%
  group_by(nr_zona, nr_secao) %>%
  arrange(desc(nm_votavel)) %>%
  mutate(aux = lead(perc_validos_1),
         dif_votos = perc_validos_1 - aux)

tse_mun_rj_pref_valid_wide_criv20 <- tse_mun_rj_pref_valid_wide20 %>%
  filter(grepl("CRIVELLA", nm_votavel)) %>%
  mutate(perc_validos_previsto_2t_reg1 =
           coef(reg1)[1] + 
           coef(reg1)[2]*perc_validos_1,
         perc_validos_previsto_2t_reg2 =
           coef(reg2)[1] + 
           coef(reg2)[2]*perc_validos_1 +
           coef(reg2)[3]*dif_votos)

tse_mun_rj_pref_valid_wide_criv20 %>%
  ggplot(aes(y=perc_validos_previsto_2t_reg1,
             x = perc_validos_2)) + geom_point() +
  geom_abline(intercept = 0, slope=1) +
  xlim(0,1) + ylim(0,1)

tse_mun_rj_pref_valid_wide_criv20 %>%
  ggplot(aes(y=perc_validos_previsto_2t_reg2,
             x = perc_validos_2)) + geom_point() +
  geom_abline(intercept = 0, slope=1) +
  xlim(0,1) + ylim(0,1)

View(tse_mun_rj_pref_valid_wide_criv20)

# erro quadrático médio dos modelos

tse_mun_rj_pref_valid_wide_criv20 <- tse_mun_rj_pref_valid_wide_criv20 %>%
  mutate(erro_reg1 = perc_validos_2 - perc_validos_previsto_2t_reg1,
         erro_reg2 = perc_validos_2 - perc_validos_previsto_2t_reg2,
         erro_reg1_sq = erro_reg1^2,
         erro_reg2_sq = erro_reg2^2)

tse_mun_rj_pref_valid_wide_criv20 %>%
  ungroup() %>%
  summarise(eqm_reg1 = sum(erro_reg1_sq)/n(),
            eqm_reg2 = sum(erro_reg2_sq)/n())

tse_mun_rj_pref_valid_wide_criv20 %>%
  ungroup() %>%
  mutate(votos_criv_reg1 = perc_validos_previsto_2t_reg1*qtde_validos_2,
         votos_criv_reg2 = perc_validos_previsto_2t_reg2*qtde_validos_2) %>%
  summarise(sum(votos_criv_reg1),
            sum(votos_criv_reg2))
# 913700