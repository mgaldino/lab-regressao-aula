# aula 2 pós greve - noturno 22/11

library(data.table)

presid_al18 <- fread("votacao_secao_2018_BR.csv",
                     encoding = "Latin-1")
library(tidyverse)
glimpse(presid_al18)

# filtrando só AL

presid_al18 <- presid_al18 %>%
  filter(SG_UF == "AL")

# modelo voto em Bolsonaro 1t prediz voto no 2t

# descobre o que é voto nulo e branco
presid_al18 %>%
  group_by(NM_VOTAVEL) %>%
  summarise(max(NR_VOTAVEL))

presid_al18_valido <- presid_al18 %>%
  filter(!NR_VOTAVEL %in% c(95,96)) %>%
  group_by(NR_SECAO,NR_ZONA, CD_MUNICIPIO, NR_TURNO, NR_VOTAVEL ) %>%
  summarise(validos = sum(QT_VOTOS)) %>%
  mutate(bol_bolsonaro = NR_VOTAVEL == 17,
         validos_bolsonaro = sum(validos*bol_bolsonaro)) %>%
  summarise(total_validos = sum(validos),
            validos_bolsonaro = max(validos_bolsonaro),
            perc_bolsonaro = validos_bolsonaro/total_validos) %>%
  dplyr::select(-total_validos) %>%
  pivot_wider(id_cols = c(NR_SECAO, NR_ZONA, CD_MUNICIPIO), names_from = NR_TURNO, values_from = perc_bolsonaro) %>%
  rename(perc_bolso_turno1 = '1',
         perc_bolso_turno2 = '2')

reg1 <- lm(perc_bolso_turno2 ~ perc_bolso_turno1, data = presid_al18_valido)
summary(reg1)

presid_al18_valido %>%
  ggplot(aes(x=perc_bolso_turno1, y=perc_bolso_turno2)) + geom_point() + 
  geom_abline(slope = coef(reg1)[2] , intercept = coef(reg1)[1], colour  = "blue")


df <- data.frame(residuos_sq = residuals(reg1)^2, preditor = presid_al18_valido$perc_bolso_turno1)

df %>%
  ggplot(aes(x=preditor, y = residuos_sq)) + geom_point() + geom_smooth(method="lm", se=F)

df %>%
  ggplot(aes(x=preditor, y = residuos_sq)) + geom_point() + geom_smooth(se=F)

##

library(nullabor)

set.seed(1234)  # Aleatoriza do mesmo jeito sempre

elec_reg <- data.frame(presid_al18_valido, .resid = residuals(reg1), .fitted = fitted(reg1))

shuffled_residuals <- lineup(null_lm(perc_bolso_turno2 ~ perc_bolso_turno1,
                                     method = "rotate"), true = elec_reg,
                             n = 9)
# decrypt("nq0j aN6N KS 9iDK6KiS Ek")

ggplot(shuffled_residuals, aes(x = .fitted, y = .resid)) +
  geom_point() +
  facet_wrap(vars(.sample))


# qq plot

## quantil

x <- rnorm( 1000)
median(x)
quantile(x, p=.5)
quantile(x, p=.25)

y <- 1:100

# qq plot
qqnorm(residuals(reg1))
qqline(residuals(reg1))
