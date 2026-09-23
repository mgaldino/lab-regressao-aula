# aula 2 pós-greve

library(data.table)
library(here)
library(tidyverse)

# lista o nome do arquivo em csv
# unzip(here("dados", "votacao_secao_2018_BR.zip"), list = TRUE)


#read data1.csv into data frame
presid_al18 <- fread(here("votacao_secao_2018_BR.csv"), 
                     encoding = "Latin-1")

glimpse(presid_al18)
# presid_al18 <- fread("votacao_secao_2018_BR.csv", 
#                      encoding = "Latin-1")

# filtrando só AL

presid_al18 <- presid_al18 %>%
  filter(SG_UF =="SC")

# modelo voto em Bolsonaro 1t prediz voto no 2t

# descobre o que é voto nulo e branco
presid_al18 %>%
  group_by(NM_VOTAVEL) %>%
  summarise(max(NR_VOTAVEL))

# 95 e 96
presid_al18_valido <- presid_al18 %>%
  filter(!NR_VOTAVEL %in% c(95,96)) %>%
  group_by(NR_SECAO,NR_ZONA, CD_MUNICIPIO, 
           NR_TURNO, SG_UF, NR_VOTAVEL ) %>%
  summarise(validos = sum(QT_VOTOS)) %>%
  mutate(bol_bolsonaro = NR_VOTAVEL == 17,
         validos_bolsonaro = sum(validos*bol_bolsonaro)) %>%
  summarise(total_validos = sum(validos),
            validos_bolsonaro = max(validos_bolsonaro),
            perc_bolsonaro = validos_bolsonaro/total_validos) %>%
  dplyr::select(-total_validos) %>%
  pivot_wider(id_cols = c(NR_SECAO, NR_ZONA, CD_MUNICIPIO, SG_UF), names_from = NR_TURNO, values_from = perc_bolsonaro) %>%
  rename(perc_bolso_turno1 = '1',
         perc_bolso_turno2 = '2')
# plot

presid_al18_valido %>%
  ggplot(aes(x=perc_bolso_turno1, y=perc_bolso_turno2)) +
  geom_point() 

# modelo de regressão

reg1 <- lm(perc_bolso_turno2 ~ perc_bolso_turno1, data = presid_al18_valido)
summary(reg1)

reg2 <- lm(perc_bolso_turno2 ~ 
             perc_bolso_turno1 + as.factor(SG_UF), data = presid_al18_valido)
summary(reg2)

reg3 <- lm(perc_bolso_turno2 ~ 
             perc_bolso_turno1*as.factor(SG_UF), data = presid_al18_valido)
summary(reg3)

presid_al18_valido %>%
  ggplot(aes(x=perc_bolso_turno1, y=perc_bolso_turno2)) +
  geom_point() + 
  geom_abline(slope = coef(reg1)[2] , 
              intercept = coef(reg1)[1], colour  = "blue")


## Pra SC checagem do modelo

df <- data.frame(residuos = residuals(reg1), preditor = presid_al18_valido$perc_bolso_turno1)

df %>%
  ggplot(aes(x=preditor, y = residuos)) + geom_point() + geom_smooth(method="lm", se=F)

# Homecedasticidade

df <- data.frame(residuos_sq = residuals(reg1)^2, preditor = presid_al18_valido$perc_bolso_turno1)

df %>%
  ggplot(aes(x=preditor, y = residuos_sq)) + geom_point() + geom_smooth(method="lm", se=F)

# valor absoluto
df <- data.frame(residuos_abs = abs(residuals(reg1)), preditor = presid_al18_valido$perc_bolso_turno1)

df %>%
  ggplot(aes(x=preditor, y = residuos_abs)) + geom_point() + geom_smooth(method="lm", se=F)

# há correlação espacial
presid_al18_valido <- presid_al18_valido %>%
  mutate(id_secao = paste0(NR_SECAO, NR_ZONA , CD_MUNICIPIO))

df <- data.frame(residuos = residuals(reg1), id = as.numeric(presid_al18_valido$id_secao))

df %>%
  ggplot(aes(x=id, y = residuos)) + geom_point() + geom_smooth(method="lm", se=F)

# permutation test

library(nullabor)

set.seed(1234)  # Aleatoriza do mesmo jeito sempre

elec_reg <- data.frame(presid_al18_valido, .resid = residuals(reg1), .fitted = fitted(reg1))

shuffled_residuals <- lineup(null_lm(perc_bolso_turno2 ~ perc_bolso_turno1,
                                     method = "rotate"), true = elec_reg,
                             n = 9)
## decrypt("ve5B DEyE l6 GuClylu6 dT")

ggplot(shuffled_residuals, aes(x = .fitted, y = .resid)) +
  geom_point() +
  facet_wrap(vars(.sample))

#
df <- data.frame(residuos = residuals(reg1), preditor = presid_al18_valido$perc_bolso_turno1, 
                 density_points = rnorm(length(residuals(reg1)) , 0, sd(residuals(reg1))))

print(sd(residuals(reg1)))
# aes(y=..density..)

df %>%
  ggplot(aes(residuos)) + 
  geom_histogram() +
  geom_density(aes(density_points), colour = "blue")


#3 Quantile

x <- rnorm( 1000)

q50 <- quantile(x, .5)
q025 <- quantile(x, .025)
q975 <-quantile(x, .975)

print(c(q50, q025, q975))

# QQ plot SC
qqnorm(residuals(reg1))
qqline(residuals(reg1))

#3 Modelo preditivo PE

presid_al18


presid_al18 <- presid_al18 %>%
  filter(SG_UF =="PE")

presid_al18_valido <- presid_al18 %>%
  filter(!NR_VOTAVEL %in% c(95,96)) %>%
  group_by(NR_SECAO,NR_ZONA, CD_MUNICIPIO, 
           NR_TURNO, SG_UF, NR_VOTAVEL ) %>%
  summarise(validos = sum(QT_VOTOS)) %>%
  mutate(bol_haddad = NR_VOTAVEL == 13,
         validos_haddad = sum(validos*bol_haddad)) %>%
  summarise(total_validos = sum(validos),
            validos_haddad = max(validos_haddad),
            perc_bolsonaro = validos_haddad/total_validos) %>%
  dplyr::select(-total_validos) %>%
  tidyr::pivot_wider(id_cols = c(NR_SECAO, NR_ZONA, CD_MUNICIPIO, SG_UF), names_from = NR_TURNO, values_from = perc_bolsonaro) %>%
  rename(perc_haddad_turno1 = '1',
         perc_haddad_turno2 = '2')

glimpse(presid_al18_valido)

reg_haddad <- lm(perc_haddad_turno2  ~ perc_haddad_turno1 ,
                 data = presid_al18_valido)

summary(reg_haddad)

df <- data.frame(y_hat = fitted(reg_haddad), 
                 perc_haddad_turno1 = presid_al18_valido$perc_haddad_turno1 )

df %>%
  ggplot(aes(y=y_hat, x=perc_haddad_turno1)) + geom_point()

presid_al18_valido %>%
  ggplot(aes(perc_haddad_turno1)) + geom_histogram()
