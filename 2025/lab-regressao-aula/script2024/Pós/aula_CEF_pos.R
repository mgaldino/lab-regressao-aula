library(tidyverse)

dados_eleicao20 <- readRDS(file="C:/Users/fcslab122/lab-regressao-aula/lab-regressao-aula/dados 2024/dados_turno1.rds")

# library(here)
# dados_eleicao20 <- readRDS(file=here("dados 2024", "dados_turno1.rds"))
glimpse(dados_eleicao20)
View(dados_eleicao20)

library(ggplot2)
dados_eleicao20 %>%
  group_by(NM_MUNICIPIO) %>%
  summarise(n())

dados_eleicao20 %>%
  distinct(NM_MUNICIPIO)

dados_eleicao20 %>%
  ggplot(aes(x=perc_validos_1t, y=perc_validos_2t)) + geom_point()

dados_eleicao20 %>%
  ggplot(aes(x=perc_validos_1t, y=perc_validos_2t)) + geom_point() +
  facet_wrap(vars(NM_MUNICIPIO))

dados_eleicao20 %>%
  ggplot(aes(x=perc_validos_1t, y=perc_validos_2t)) + geom_point() +
  facet_wrap(vars(NM_MUNICIPIO, NR_VOTAVEL)) +
  theme_bw() +
  labs(x = "Votos válidos percentuais no 1o turno",
       y = "Votos válidos percentuais no 2o turno") +
  scale_y_continuous(labels = scales::label_percent()) +
  scale_x_continuous(labels = scales::label_percent())

dados_sp <- dados_eleicao20 %>%
  filter(NM_MUNICIPIO == "SÃO PAULO")

dados_sp %>%
  ggplot(aes(x=perc_validos_1t, y=perc_validos_2t)) + geom_point() +
  facet_wrap(vars(NR_VOTAVEL)) +
  theme_bw() +
  labs(x = "Votos válidos percentuais no 1o turno",
       y = "Votos válidos percentuais no 2o turno") +
  scale_y_continuous(labels = scales::label_percent()) +
  scale_x_continuous(labels = scales::label_percent())

# Calculando a CEF pra SP

# Criar bins para perc_validos_1t
dados_binned <- dados_sp %>%
  group_by(NR_VOTAVEL) %>%
  mutate(bin_1t = cut(perc_validos_1t, breaks = seq(0, 1, by = 0.025)))

# Calcular a média para cada bin
cef_data <- dados_binned %>%
  group_by(bin_1t, NR_VOTAVEL) %>%
  summarise(perc_validos_2t_mean = mean(perc_validos_2t, na.rm = TRUE),
            bin_center = mean(as.numeric(perc_validos_1t), na.rm = TRUE))

# Criar o gráfico
grafico_cef <- ggplot() +
  geom_point(data = dados_binned, aes(x = perc_validos_1t, y = perc_validos_2t), 
             alpha = 0.3, color = "gray") +
  geom_point(data = cef_data, aes(x = bin_center, y = perc_validos_2t_mean), 
             color = "red", size = 2) +
  geom_line(data = cef_data, aes(x = bin_center, y = perc_validos_2t_mean), 
            color = "blue") +
  facet_wrap(~NR_VOTAVEL) +
  labs(title = "Função de Expectativa Condicional (CEF)",
       x = "Percentual de Votos Válidos no 1º Turno",
       y = "Percentual Médio de Votos Válidos no 2º Turno") +
  theme_minimal() +
  scale_x_continuous(limits = c(0, 1), labels = scales::percent) +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent)

print(grafico_cef)

## CEF + Reta de Regressão
ggplot() +
  geom_point(data = dados_binned, aes(x = perc_validos_1t, y = perc_validos_2t), 
             alpha = 0.3, color = "gray") +
  geom_point(data = cef_data, aes(x = bin_center, y = perc_validos_2t_mean), 
             color = "red", size = 2) +
  geom_line(data = cef_data, aes(x = bin_center, y = perc_validos_2t_mean), 
            color = "blue") +
  facet_wrap(~NR_VOTAVEL) +
  geom_smooth(data = dados_binned, method="lm", 
              aes(x = perc_validos_1t, y = perc_validos_2t), 
              se =FALSE, colour="black") +
  labs(title = "Função de Expectativa Condicional (CEF)",
       x = "Percentual de Votos Válidos no 1º Turno",
       y = "Percentual Médio de Votos Válidos no 2º Turno") +
  theme_minimal() +
  scale_x_continuous(limits = c(0, 1), labels = scales::percent) +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent)

# Regressão

reg <- lm(perc_validos_2t ~ perc_validos_1t, data= dados_sp)
summary(reg)

# Regressão pro Bruno Covas
dados_sp_covas <- dados_sp %>%
  filter(NR_VOTAVEL == 45)

reg_covas <- lm(perc_validos_2t ~ perc_validos_1t, data= dados_sp_covas)
summary(reg_covas)

# Regressão Boulos
dados_sp_boulos <- dados_sp %>%
  filter(NR_VOTAVEL == 50)

reg_boulos <- lm(perc_validos_2t ~ perc_validos_1t, data= dados_sp_boulos)
summary(reg_boulos)

novos_dados <- data.frame(perc_validos_1t = 
                            c(.1, .2, .3))


predict(reg_boulos, novos_dados)
