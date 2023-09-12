## Aula 5 diurno

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
  mutate(genero = as.character(genero)) %>%
  mutate(log10_salario = log10(salario))

df %>%
  group_by(genero) %>%
  summarise(media_condicional = mean(renda),
            mediana_condicional = median(renda),
            eqm_media_cond = sum((renda - media_condicional)^2),
            eqm_mediana_cond = sum((renda - mediana_condicional)^2)) %>%
  summarise(eqm_media = sum(eqm_media_cond),
            eqm_mediana = sum(eqm_mediana_cond))

df %>%
  group_by(genero) %>%
  summarise(media_condicional = mean(salario),
            mediana_condicional = median(salario),
            eqm_media_cond = sum((salario - media_condicional)^2),
            eqm_mediana_cond = sum((salario - mediana_condicional)^2)) %>%
  summarise(eqm_media = sum(eqm_media_cond),
            eqm_mediana = sum(eqm_mediana_cond))

df %>%
  group_by(genero) %>%
  summarise(media_condicional = mean(log_salario),
            mediana_condicional = median(log_salario),
            eqm_media_cond = sum((log_salario - media_condicional)^2),
            eqm_mediana_cond = sum((log_salario - mediana_condicional)^2)) %>%
  summarise(eq_media = sum(eqm_media_cond),
            eq_mediana = sum(eqm_mediana_cond))

df <- df %>%
  mutate(log10_salario = log10(salario))
# p1 <- df %>%
#   ggplot(aes(salario)) + geom_density(aes(weight=V1028)) + theme_bw(base_size = 22)
# 
# print(p1)

p2 <- df %>%
  ggplot(aes(log_salario)) + geom_density(aes(weight=V1028)) + 
  theme_bw(base_size = 22) 

print(p2)
 
df %>%
  ggplot(aes(log10_salario)) + geom_density(aes(weight=V1028)) + 
  theme_bw(base_size = 22) 

summary(df)
df %>%
  group_by(genero) %>%
  summarise(weighted.mean(log(renda), V1028))

# Cap 6 CEF

set.seed(234)
n <- 10000
hist(rnorm(n,0,1))

x <- rnorm(n)
u <- rnorm(n)
y <- x^2 + u

df <- data.frame(x=x, y=y)

df %>%
  ggplot(aes(x=x, y=y)) + geom_point()

m1 <- mean(y)

m2 <- median(y)
erro1 <- y - m1
erro2 <- y - m2

print(sum(erro1^2))

print(sum(erro2^2))

## Regressão

reg <- lm(y ~ x + z + w + x:w, data = df)
summary(reg)
