# Cap 5 e 6, noturno


library(PNADcIBGE)
library(tidyverse)
library(ggplot2)
# Importe os dados desejados
data <- get_pnadc(year=2017,
                  quarter=4,
                  vars=c("Ano", "Trimestre", "UF", "V2007", 
                         "VD4020", "VD4035", "V2010", "V1028"),design=FALSE,
                  savedir=tempdir())

data <- data %>%
  select(Ano, Trimestre, UF, V2007, V2010, VD4020, VD4035, V1028)

data <- data %>%
  rename(genero = V2007,
         renda = VD4020,
         raca = V2010,
         horas_trabalhadas = VD4035)

library(ggplot2)

df <- data %>%
  filter(!is.na(renda)) %>%
  filter(!is.na(horas_trabalhadas)) %>%
  filter(renda > 0) %>%
  filter(horas_trabalhadas > 0) %>%
  mutate(salario = renda/(4.5*horas_trabalhadas)) %>%
  mutate(log_salario = log(salario)) %>%
  mutate(genero = as.character(genero))

p1 <- df %>%
  ggplot(aes(salario)) + geom_density(aes(weight=V1028)) + theme_bw(base_size = 22)

print(p1)


## EQM - média e mediana por gênero - comparação

df1 <- df %>%
  group_by(genero) %>%
  mutate(cond_exp = mean(log_salario),
         erro = log_salario - cond_exp,
         erro_sq = erro^2)

df1 %>%
  ungroup() %>%
  summarise(eqm = sum(erro_sq))

# mediana
df2 <- df %>%
  group_by(genero) %>%
  mutate(cond_exp = median(log_salario),
         erro = log_salario - cond_exp,
         erro_sq = erro^2)

df2 %>%
  ungroup() %>%
  summarise(eqm = sum(erro_sq))

###

hist(rnorm(10000,0,1))

## CEF com x binário

x <- rbinom(1000, size=2, .5)
u <- rnorm(1000)
y <- x^2 + u
df <- data.frame(y=y, x=x)
df %>%
  group_by(x) %>%
  summarise(exp_cond = mean(y)) %>%
  ggplot(aes(y=exp_cond, x=x)) + geom_smooth()

df %>%
  ggplot(aes(y=y, x=x)) + geom_point() +
  scale_y_continuous(breaks = -6:6)
