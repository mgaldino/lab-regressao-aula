# lista 5

library(PNADcIBGE)
# Importe os dados desejados
data <- get_pnadc(year=2017,
                  quarter=4,
                  vars=c("Ano", "Trimestre", "UF", "V2007", "VD4020", "VD4035"),
                  design=FALSE,
                  savedir=tempdir())

# saveRDS(data, file = "dados_pnad17.rds")
# data <- readRDS(file = "dados_pnad17.rds")

#write.csv2(data, file="dados_pnad12.csv")

library(tidyverse)
library(tidylog)
data <- data %>%
  select(Ano, Trimestre, UF, V2007, VD4020, VD4035)
# Renomeie as vari´aveis:
data <- data %>%
  rename(Sexo = V2007,
         Renda = VD4020,
         Horas_trabalhadas = VD4035)

reg <- lm(Renda ~ Sexo, data= data)
summary(reg)
# $renda_i = \alpha + \beta*sexo_i + e_i$
# $y_i = \alpha + \beta*x_i + e_i$

data %>%
  group_by(Sexo) %>%
  summarise(mean(Renda, na.rm=T))

reg1 <- lm(Renda ~ Horas_trabalhadas, data=data)
summary(reg1)

# exercício

vec_n <- seq(10, 2000, by=10)
quantile_z <- numeric()
quantile_t <- numeric()
for (i in 1:length(vec_n)) {
  quantile_z[i] <- qnorm(.025)
  quantile_t[i] <- qt(.025, vec_n[i])
}

df <- data.frame(my_qt = quantile_t, 
                 my_qz = quantile_z,
                 n = vec_n)

library(ggplot2)
p <- df %>%
  ggplot(aes(x=n, y = my_qz)) + geom_line()

p + geom_line(aes(y=my_qt), data = df) +
  scale_x_continuous(breaks = seq(10, 2000, by=90))

### 
library(data.table)
library(here)
library(janitor)


#read data1.csv into data frame
presid_18 <- fread("votacao_secao_2018_BR.csv", 
                   encoding = "Latin-1")

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

# modelo de regressão

reg1 <- lm(percentual_bolso_2t ~ percentual_bolso_1t, data = df_resultados)
summary(reg1)

df_resultados %>%
  ggplot(aes(x=percentual_bolso_1t, y=percentual_bolso_2t)) + geom_point() + 
  geom_abline(slope = coef(reg1)[2] , intercept = coef(reg1)[1], colour  = "blue")

#
df_previsao <- df_resultados_22 %>%
  mutate(y_previsto = coef(reg1)[1] + coef(reg1)[2]*percentual_bolso_1t)

## 22
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
# Agora que importamos os dados de 22, podemos fazer nossa previsão, usando os resultados do primeiro turno.

df_resultados_22 <- df_resultados_22 %>%
  mutate(y_prev_2t = coef(reg1)[1] + coef(reg1)[2]*percentual_bolso_1t,
         validos_previsto = total_validos_1t*y_prev_2t)

df_resultados_22 %>%
  ungroup() %>%
  summarise(total_bolso = sum(validos_previsto),
            total_valido_previsto = sum(total_validos_1t),
            perc_previsto = total_bolso/total_valido_previsto)

# incerteza preditiva
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
  summarise(perc_previsto = sum(validos_prev)/sum(total_validos_1t),
            perc_previsto_lower = sum(validos_prev_lower)/sum(total_validos_1t),
            perc_previsto_upper = sum(validos_prev_upper)/sum(total_validos_1t))
