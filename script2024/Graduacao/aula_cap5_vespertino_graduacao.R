# Aula cap 5 - vespertino

# se arquivo estiver na paste onde o R aponta (usar getwd() para saber)
# dados_eleicao20 <- readRDS(file="dados_turno1.rds")

# escrevendo o caminho completo do arquivo
dados_eleicao20 <- readRDS(file="C:/Users/fcslab122/lab-regressao-aula/lab-regressao-aula/dados 2024/dados_turno1.rds")

#usando library(here)
# library(here)
# dados_eleicao20 <- readRDS(here("Dados 2024", "dados_turno1.rds"))

library(tidyverse)
glimpse(dados_eleicao20)

# gráficos
library(ggplot2)

## todos os municípios
dados_eleicao20 %>%
  ggplot(aes(x=perc_validos_1t, y=perc_validos_2t)) +
  geom_point() + 
  geom_abline(intercept = 0.269735,
              slope = 0.816412,
              colour="blue")

# por município e candidato
# deixando mais bonito também
dados_eleicao20 %>%
  ggplot(aes(x=perc_validos_1t, y=perc_validos_2t)) +
  geom_point() +
  labs(x = "Percentual de Votos Válidos no 1º Turno",
       y = "Percentual Médio de Votos Válidos no 2º Turno") +
  theme_bw() +
  scale_y_continuous(labels = 
                       scales::label_percent()) +
  scale_x_continuous(labels = 
                       scales::label_percent()) +
  facet_wrap(NM_MUNICIPIO ~ NR_VOTAVEL) +
  geom_smooth(method="lm")

# modelo de regressão linear simples
reg <- lm(perc_validos_2t ~ perc_validos_1t,
          data=dados_eleicao20)
summary(reg)

