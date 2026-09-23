library(tidyverse)
set.seed(123)

n <- 1000
df_nl <- data.frame(x = rnorm(n), e = rnorm(n))
df_nl <- df_nl %>%
  mutate(y = 2 + 1.5*x - .5*x^2 + e)

reg_nl <- lm(y ~x, df_nl)
df <- data.frame(residuos = residuals(reg_nl),
                 preditor = df_nl$x)

df %>%
  ggplot(aes(x=preditor, y = residuos)) + geom_point() + 
  geom_smooth(method="lm", se=F)

df <- data.frame(residuos_sq = residuals(reg_nl)^2,
                 preditor = df_nl$x)

df %>%
  ggplot(aes(x=preditor, y = residuos_sq)) + geom_point() + 
  geom_smooth(se=F)

# ajustando modelo não-linear nas variáveis
reg_nl1 <- lm(y ~ x + I(x^2), df_nl)
df <- data.frame(residuos = residuals(reg_nl1),
                 preditor = df_nl$x)

df %>%
  ggplot(aes(x=preditor, y = residuos)) + geom_point() + 
  geom_smooth(se=F)

## QQ plot

x <- rnorm(n)
x <- rnorm( 1000)

q50 <- quantile(x, .5)
q025 <- quantile(x, .025)
q975 <- quantile(x, .975)

print(c(q50, q025, q975))

# erro não normal
x <- rnorm(n)
erro <- rlnorm(n, 3, 1.2)
y <- 5 + 1.5*x + erro

df <- data.frame(x=x, y=y, y1 = log(y))
reg_ln <- lm(y1 ~ x, data=df)
summary(reg_ln)
qqnorm(residuals(reg_ln))
qqline(residuals(reg_ln))

reg_ln <- lm(y ~ x, data=df)
summary(reg_ln)
qqnorm(residuals(reg_ln))
qqline(residuals(reg_ln))

## Modelo de Regressão para SP em 2020
library(here)
library(data.table)
library(tidyverse)
dados_sp20 <- fread(here("dados brutos 2024",
                         "votacao_secao_2020_SP.csv"),
                    encoding="Latin-1")

glimpse(dados_sp20)
dados_sp20 <- dados_sp20 %>%
  filter(NM_MUNICIPIO == "SÃO PAULO" & 
           DS_CARGO == "Prefeito")

dados_sp20_final <- dados_sp20 %>%
  filter(!NM_VOTAVEL %in% c("VOTO NULO", "VOTO BRANCO")) %>%
  mutate(bol_turno = ifelse(NR_TURNO == 1, 0,1)) %>%
  group_by(NR_SECAO, NR_ZONA) %>%
  mutate(total_valido1t = sum(QT_VOTOS*(1-bol_turno)),
         total_valido2t = sum(QT_VOTOS*bol_turno)) %>%
  filter(grepl("BOULOS", NM_VOTAVEL)) 

# dados_sp20_final %>%
#   filter(NR_ZONA == 250  & NR_SECAO == 481) %>%
#   View()
dados_sp20_final <- dados_sp20_final %>%
  mutate(id_zona_secao = paste(NR_ZONA, NR_SECAO, sep="_")) 


dados_sp20_final <- dados_sp20_final %>%
  filter(!id_zona_secao %in% c("250_481", "257_508",
                             "325_685"))

dados_sp20_final <- dados_sp20_final %>%
  mutate(perc_boulots_1t = QT_VOTOS/total_valido1t,
         perc_boulots_2t = QT_VOTOS/total_valido2t) %>%
  dplyr::select(NR_SECAO, NR_ZONA, NR_TURNO,
                perc_boulots_1t, perc_boulots_2t,
                total_valido1t, total_valido2t, id_zona_secao) %>%
  group_by(NR_SECAO, NR_ZONA) %>%
  arrange(NR_SECAO, NR_ZONA) %>%
  mutate(perc_boulots_1t = dplyr::lead(perc_boulots_1t),
         perc_boulots_2t  = dplyr::lag(perc_boulots_2t)) %>%
  group_by(NR_SECAO, NR_ZONA) %>%
  summarise(perc_boulots_1t = max(perc_boulots_1t, na.rm=T),
            perc_boulots_2t = max(perc_boulots_2t, na.rm=T),
            total_valido1t = max(total_valido1t),
            total_valido2t = max(total_valido2t),
            id = max(id_zona_secao)) 

dados_sp20_final %>%
  ungroup() %>%
  summarise(tot = sum(total_valido1t),
            tot1 = sum(total_valido2t),
            perc1t = sum(perc_boulots_1t*total_valido1t)/tot,
            perc2t = sum(perc_boulots_2t*total_valido2t)/tot1)

# zona 250 secao 481

reg <- lm(perc_boulots_2t ~ perc_boulots_1t,
          data = dados_sp20_final)  

summary(reg)

library(ggplot2)
df <- data.frame(resid = residuals(reg),
                 x = dados_sp20_final$perc_boulots_1t)

df %>%
  ggplot(aes(y=resid, x=x)) +
  geom_point() + geom_smooth(method="lm")
