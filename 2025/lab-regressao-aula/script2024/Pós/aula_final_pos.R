
#
library(tidyverse)
set.seed(1234)
n <- 1000 # número de indivíduos
m <- 4 # número de alternativas (discordo muito, pouco, concordo pouco, muito)
x_subject <- rnorm(n)
erro <- rlogis(n, location = 0, scale = 1)
beta_0 <- .5
beta1 <- 2
z <- beta_0 + beta1*x_subject + erro

tau1 <- -2
tau2 <- 1
tau3 <- 4

y <- ifelse(z <= tau1, "discordo muito",
            ifelse(z <= tau2, "discordo pouco",
                   ifelse(z <= tau3, "concordo pouco", "concordo muito")))

df <- data.frame(y=y, x1 = x_subject)

df <- df %>%
  mutate(y = factor(y, ordered=T))

# checando a ordem
levels(df$y)

# ordem trocada (inversa)
#vamos corrigir

df <- df %>%
  mutate(y = factor(y, levels = c("discordo muito",
                                  "discordo pouco",
                                  "concordo pouco",
                                  "concordo muito"),
                    ordered=T))

# checando a ordem
levels(df$y)

library(ordinal)
reg_ord <- clm(y ~ x1, data = df, link = "logit")

summary(reg_ord)

# calculando probabilidades de cada categoria
library(marginaleffects)

prev <- marginaleffects::avg_comparisons(reg_ord, variables = list(x1 = 1))
prev
 
# Importando dados do LatinoBarometro

library(here)
library(haven)
lb <- readRDS(here("dados brutos 2024" , "Latinobarometro_2023_Esp_Rds_v1_0.rds"))

lb <- lb %>%
  mutate(P5STGBS = haven::as_factor(P5STGBS),
         P5STGBS = factor(P5STGBS, 
                            levels = c("Muy mala", "Mala",
                                       "Regular", "Buena",
                                       "Muy buena"), ordered=TRUE)) %>%
  filter(!is.na(P5STGBS)) %>%
  filter(idenpa == 76)

lb <- lb %>%
  mutate(sexo = haven::as_factor(sexo),
         ideologia = P16ST) %>%
  filter(ideologia %in% 0:10)

lb_reg <- clm(P5STGBS ~ edad + sexo + ideologia,
              data=lb, link="logit")

summary(lb_reg)
avg_comparisons(lb_reg, variables = "edad")
avg_comparisons(lb_reg, variables = list(edad = 10))
avg_comparisons(lb_reg, variables = "sexo")


# Multinomial

# fit_basic <- multinom(y ~ x, data = df)


lb <- readRDS(here("dados brutos 2024" , "Latinobarometro_2023_Esp_Rds_v1_0.rds"))

lb <- lb %>%
  mutate(P5STGBS = haven::as_factor(P5STGBS),
         P5STGBS = factor(P5STGBS, 
                          levels = c("Muy mala", "Mala",
                                     "Regular", "Buena",
                                     "Muy buena"), ordered=FALSE)) %>%
  filter(!is.na(P5STGBS)) %>%
  filter(idenpa == 76)

lb <- lb %>%
  mutate(sexo = haven::as_factor(sexo),
         ideologia = P16ST) %>%
  filter(ideologia %in% 0:10)

library(nnet)
fit_mn <- multinom(P5STGBS ~ edad + sexo + ideologia + I(ideologia^2),
                   data=lb)
summary(fit_mn)
avg_comparisons(fit_mn, variables = "ideologia")
avg_comparisons(fit_mn, variables = list(ideologia = 5))
avg_comparisons(fit_mn, variables = "sexo")

library(broom)
tidy(fit_mn, conf.int = TRUE)
