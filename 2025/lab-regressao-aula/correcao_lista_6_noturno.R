# correção lista 6 noturno
library(data.table)
library(tidyverse)

setwd("C:\\Users\\fcslab122\\Downloads\\microdados_enem_2022\\DADOS")

enem <- fread("MICRODADOS_ENEM_2022.csv")

enem_lista <- enem %>%
  dplyr::select(NU_NOTA_CH, NU_NOTA_MT)

glimpse(enem_lista)

enem_lista <- enem_lista %>%
  dplyr::filter(!is.na(NU_NOTA_CH) & !is.na(NU_NOTA_MT))

# Questão 2

fit <- lm(NU_NOTA_MT ~ NU_NOTA_CH,
          data= enem_lista)

# evitar notação científica
options(scipen=20)
summary(fit)

# Questão 3
summary(fit)$coef[2] -1.96*summary(fit)$coef[4]
summary(fit)$coef[2] +1.96*summary(fit)$coef[4]

confint(fit)
# Problema, muitos dados. Vamos usar uma amostra
enem_lista_a <- enem_lista %>%
  slice_sample(n=10000)

fit1 <- lm(NU_NOTA_MT ~ NU_NOTA_CH,
           data= enem_lista_a)
summary(fit1)
# Questão 4
df <- data.frame(residuos = residuals(fit1), preditor = enem_lista_a$NU_NOTA_CH)

df %>%
  ggplot(aes(x=preditor, y = residuos)) +
  geom_point(alpha=.3) + geom_smooth(method="lm", se=F)

# Resíduos ao quadrado
df <- data.frame(residuos_sq = residuals(fit1)^2, preditor = enem_lista_a$NU_NOTA_CH)

df %>%
  ggplot(aes(x=preditor, y = residuos_sq)) + geom_point() + geom_smooth(method="lm", se=F)


# Normalidade
qqnorm(residuals(fit1))
qqline(residuals(fit1))
hist(residuals(fit1), breaks=100)

library(performance)
check_model(fit1)

x1 <- rnorm(10000, 420, 100)
x2 <- rnorm(9000, 650, 100)
plot(density(c(x1, x2)))
