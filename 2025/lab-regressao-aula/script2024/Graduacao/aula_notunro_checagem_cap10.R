# aula noturno cap 10
library(tidyverse)
set.seed(123)

#modelo linear
n <- 5000
df_nl <- data.frame(x = rnorm(n), e = rnorm(n))
df_nl <- df_nl %>%
  mutate(y = 2 + 1.5*x + e)
reg_nl0 <- lm(y ~x, df_nl)
summary(reg_nl0)
df <- data.frame(residuos = residuals(reg_nl0),
                 preditor = df_nl$x)

df %>%
  ggplot(aes(x=preditor, y = residuos)) + geom_point() + 
  geom_smooth(method="lm", se=F)

# Modelo não-linear
df_nl <- df_nl %>%
  mutate(y = 2 + 1.5*x - .5*x^2 + e)

reg_nl <- lm(y ~ x, df_nl)
df <- data.frame(residuos = residuals(reg_nl),
                 preditor = df_nl$x)

df %>%
  ggplot(aes(x=preditor, y = residuos)) + geom_point() + 
  geom_smooth(method="lm", se=F)

# incluindo x^2
df_nl <- df_nl %>%
  mutate(x_sq = x^2)
reg_nl <- lm(y ~ x + x_sq, df_nl)
df <- data.frame(residuos = residuals(reg_nl),
                 preditor = df_nl$x)

df %>%
  ggplot(aes(x=preditor, y = residuos)) + geom_point() + 
  geom_smooth(method="lm", se=F)


df_nl <- df_nl %>%
  mutate(y = 2 + 1.5*x - .5*x^2 + .2*x^3 + e)

reg_nl1 <- lm(y ~ x, df_nl)
df <- data.frame(residuos = residuals(reg_nl1),
                 preditor = df_nl$x)

df %>%
  ggplot(aes(x=preditor, y = residuos)) + geom_point() + 
  geom_smooth(method="lm", se=F)

# log
# Modelo não-linear
df_nl <- df_nl %>%
  mutate(x = x+5)

df_nl <- df_nl %>%
  mutate(y = 2 + 1.5*log(x) + e)

reg_nl <- lm(y ~ x, df_nl)
summary(reg_nl)

df_nl %>%
  ggplot(aes(y=y, x=x)) + geom_point() +
  geom_smooth(method="lm")

df <- data.frame(residuos = residuals(reg_nl),
                 preditor = df_nl$x)
df %>%
  ggplot(aes(x=preditor, y = residuos)) + geom_point() + 
  geom_smooth(method="lm", se=F)

# variáveis omitidas
set.seed(1234)

n <- 1000
df_nl <- data.frame(x1 = rnorm(n), x2 = rnorm(n), 
                    x3 = rnorm(n), e = rnorm(n))
df_nl <- df_nl %>%
  mutate(y = 2 + 1.5*x1 - .5*x2 + e)

reg_nl <- lm(y ~x1, df_nl)

df_nl %>%
  ggplot(aes(y=y, x=x1)) + geom_point() +
  geom_smooth(method="lm", se=F)

df <- data.frame(residuos = residuals(reg_nl),
                 preditor2 = df_nl$x2,
                 preditor3 = df_nl$x3)

df %>%
  ggplot(aes(x=preditor2, y = residuos)) + geom_point() + 
  geom_smooth(method="lm", se=F)

# viés de seleção
set.seed(1234)

n <- 1000
df_nl <- data.frame(x1 = rnorm(n), x4 = rnorm(n), 
                    x3 = rnorm(n), e = rnorm(n))
df_nl <- df_nl %>%
  mutate(x2 = 2*x4 + rnorm(n),
         y = 2 + 1.5*x1 - .5*x4 + e)

reg_nl <- lm(y ~ x1, df_nl)

df_nl %>%
  ggplot(aes(y=y, x=x1)) + geom_point() +
  geom_smooth(method="lm", se=F)

df_nl %>%
  ggplot(aes(y=y, x=x1)) + geom_point() +
  geom_smooth(method="lm", se=F)

df <- data.frame(residuos = residuals(reg_nl),
                 preditor2 = df_nl$x2,
                 preditor3 = df_nl$x3)

df %>%
  ggplot(aes(x=preditor2, y = residuos)) + geom_point() + 
  geom_smooth(method="lm", se=F)

# incluindo x4 na reg

reg_nl <- lm(y ~ x1 + x4, df_nl)

df_nl %>%
  ggplot(aes(y=y, x=x1)) + geom_point() +
  geom_smooth(method="lm", se=F)

df_nl %>%
  ggplot(aes(y=y, x=x1)) + geom_point() +
  geom_smooth(method="lm", se=F)

df <- data.frame(residuos = residuals(reg_nl),
                 preditor2 = df_nl$x2,
                 preditor3 = df_nl$x3)

df %>%
  ggplot(aes(x=preditor2, y = residuos)) + geom_point() + 
  geom_smooth(method="lm", se=F)


## importando dados
library(data.table)
library(here)
presid_al18 <- fread(here("dados brutos 2024","votacao_secao_2018_BR.csv"), encoding = "Latin-1")
# filtrando só AL

presid_al18 <- presid_al18 %>%
  filter(SG_UF == "AL")

# modelo voto em Bolsonaro 1t prediz voto no 2t
# 95 e 96
presid_al18_valido <- presid_al18 %>%
  filter(!NR_VOTAVEL %in% c(95,96)) %>%
  group_by(NR_SECAO,NR_ZONA, CD_MUNICIPIO,NM_MUNICIPIO, NR_TURNO, NR_VOTAVEL ) %>%
  summarise(validos = sum(QT_VOTOS)) %>%
  mutate(bol_bolsonaro = NR_VOTAVEL == 17,
         validos_bolsonaro = sum(validos*bol_bolsonaro)) %>%
  summarise(total_validos = sum(validos),
            validos_bolsonaro = max(validos_bolsonaro),
            perc_bolsonaro = validos_bolsonaro/total_validos) %>%
  dplyr::select(-total_validos) %>%
  pivot_wider(id_cols = c(NR_SECAO, NR_ZONA, CD_MUNICIPIO, NM_MUNICIPIO), names_from = NR_TURNO, values_from = perc_bolsonaro) %>%
  rename(perc_bolso_turno1 = '1',
         perc_bolso_turno2 = '2')

# modelo de regressão

reg1 <- lm(perc_bolso_turno2 ~ perc_bolso_turno1, data = presid_al18_valido)
summary(reg1)

df <- data.frame(residuos = residuals(reg1), 
                 zona = presid_al18_valido$NR_ZONA,
                 secao = presid_al18_valido$NR_SECAO,
                 municipio = presid_al18_valido$NM_MUNICIPIO)

df_randomized <- df %>%
  arrange(municipio, zona, secao) %>%
  mutate(id = 1:n()) %>%
  sample_n(nrow(df)) %>%
  mutate(new_id = 1:n())

df_randomized %>%
  ggplot(aes(x=new_id, y = residuos)) +
  geom_point() + geom_smooth(method="lm", se=F)


df_randomized %>%
  ggplot(aes(x=id, y = residuos)) + geom_point() +
  geom_smooth(method="lm", se=F)

# Criar um vetor de cores
cores <- sort(rainbow(102))  # ou use outra paleta que preferir

df_randomized %>%
  ggplot(aes(x = id, y = residuos, color = factor(municipio))) + 
  geom_point() +
  scale_color_manual(values = cores) +
  theme(legend.position = "none") +  # Remove a legenda
  geom_hline(yintercept = 0)

#3 qq plot da normal

dados_norm <- rnorm(n)
qqnorm(dados_norm)
qqline(dados_norm)

## qqplot da uniforme
dados_unif <- runif(n/2)
dados_norm <- rnorm(n/2)
dados <- c(dados_unif,dados_norm )
qqnorm(dados)
qqline(dados)

set.seed(123)
dados <- rnorm(100, mean = 0, sd = 1)  # Amostra normal

# Teste KS contra a distribuição normal
resultado <- ks.test(residuals(reg1),
                     "pnorm",
                     mean = residuals(reg1),
                     sd = residuals(reg1))
print(resultado)


# Resíduos do modelo
residuos <- residuals(reg1)

# Teste de Shapiro-Wilk
shapiro.test(residuos)
