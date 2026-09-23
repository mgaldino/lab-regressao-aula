# lista 6 diurno
library(data.table)
library(tidyverse)
library(ggplot2)
library(performance)
setwd("C:\\Users\\fcslab122\\Downloads\\microdados_enem_2022")
enem <- fread("Dados\\MICRODADOS_ENEM_2022.csv")

enem_lista <- enem %>%
  dplyr::select(NU_NOTA_CH, NU_NOTA_MT)

glimpse(enem_lista)
summary(enem_lista)
enem_lista <- enem_lista %>%
  dplyr::filter(!is.na(NU_NOTA_CH) &
                  !is.na(NU_NOTA_MT))

fit <- lm(NU_NOTA_MT ~ NU_NOTA_CH,
          data = enem_lista)
options(scipen=99)
summary(fit)
0.9050275 - 1.96*summary(fit)$coef[4]
0.9050275 + 1.96*summary(fit)$coef[4]

confint(fit)

# 4
enem_lista_a <- enem_lista %>%
  dplyr::slice_sample(n=1000)

glimpse(enem_lista_a)

fit1 <- lm(NU_NOTA_MT ~ NU_NOTA_CH,
           data = enem_lista_a)

summary(fit1)


# 4 resíduos contra preditor
df <- data.frame(residuos = residuals(fit1), preditor = enem_lista_a$NU_NOTA_CH)

df %>%
  ggplot(aes(x=preditor, y = residuos)) +
  geom_point() + geom_smooth(method="lm", se=F)

df %>%
  ggplot(aes(x=preditor, y = residuos)) +
  geom_point() + geom_smooth()

performance::check_model(fit1)

# 6
df <- data.frame(residuos = residuals(fit1), preditor = enem_lista_a$NU_NOTA_CH, 
                 density_points = rnorm(length(residuals(fit1)) , 0, sd(residuals(fit1))))


df %>%
  ggplot(aes(residuos)) + geom_histogram(aes(y=..density..)) + geom_density(aes(density_points), colour = "blue")

qqnorm(residuals(fit1))
qqline(residuals(fit1))

x <- rnorm(1000)
y <- rnorm(1000)

qqplot(x,y)
