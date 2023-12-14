# aula 13 dez noturno

#multicolineraridade

n <- 100
x1 <- rnorm(n)
x2 <- x1*2 + 10
y <- x1 + rnorm(n)
reg <- lm(y ~ x1 + x2)
summary(reg)

reg <- lm(y ~x2 + x1)
summary(reg)

#
x3 <- rnorm(n)
x4 <- .7*x1 + .5*x3 - 10
x4[100] <- rnorm(1) - 10
reg <- lm(y ~ x1 + x3 + x4)
summary(reg)

# relação nã-linear
x5 <- x1^2
reg <- lm(y ~ x1 + x3 + x5)
summary(reg)


## Erro padrão robusto

library(ggplot2)
library(tidyverse)
set.seed(123)

x <- c(1:8, 10, 15)
y <- c(5 + rnorm(8,mean=0, sd = 1.5), 40, 65)
df <- data.frame(y=y, x=x)

df %>%
  ggplot(aes(x=x, y=y)) + geom_point() +
  geom_smooth(method="lm")

fit <- lm(y ~x, data=df)
summary(fit)

# erro padrão usual
n <- length(y)
sigma2 <- sigma(fit)^2
mat_I <- diag(n)
X <- model.matrix(fit)


omega <- sigma2*mat_I
bread <- solve(t(X)%*%X)
meat <- (t(X) %*% omega %*% X)
vce <- bread %*% meat %*% bread
sqrt(diag(vce))


# HC1

sigma2_hc1 <- residuals(fit)^2*(n/(n-2))
omega <- sigma2_hc1*mat_I
bread <- solve(t(X)%*%X)
meat <- (t(X) %*% omega %*% X)
vce <- bread %*% meat %*% bread
sqrt(diag(vce))

# hc3
sigma2_hc3 <- residuals(fit)^2/(1 - hatvalues(fit))^2
omega <- sigma2_hc3*mat_I
bread <- solve(t(X)%*%X)
meat <- (t(X) %*% omega %*% X)
vce <- bread %*% meat %*% bread
sqrt(diag(vce))

# 
library(lmtest)
library(sandwich)

coeftest(fit, vcovHC(fit, "HC3"))
coeftest(fit, vcovHC(fit, "HC1"))

# IC
coefci(fit, vcov. = vcovHC(fit, "HC3"))

# plot leverage
plot(fit, which = 5)

y1 <- y[-10]
x1 <- x[-10]
fit1 <- lm(y1 ~x1)
summary(fit1)


y1 <- y[-5]
x1 <- x[-5]
fit1 <- lm(y1 ~x1)
summary(fit1)

# exemplo de heterocedasticidade
x <- rnorm(1000, mean=3)
e <- rnorm(1000, 0, x^2) # o DP do erro é igual a .5*x^2
a <- 2
b <- -2
y <- a + b*x + e

df <- data.frame(y=y, x=x)

df %>%
  ggplot(aes(y=y, x=x)) + geom_point() + 
  geom_smooth(method="lm")

m_sim <- lm(y ~ x, data=df)
# plots de checagem do modelo
library(performance)
check_model(m_sim)

library(stargazer)
f2 <- coeftest(m_sim, vcovHC(m_sim, type = "HC0"))
f3 <- coeftest(m_sim, vcovHC(m_sim, type = "HC1"))
f4 <- coeftest(m_sim, vcovHC(m_sim, type = "HC2"))
f5 <- coeftest(m_sim, vcovHC(m_sim, type = "HC3"))
stargazer(m_sim, 
          f2,
          f3,
          f4,
          f5, type = "text")


# latinobarometro
library(tidyverse)
library(sjlabelled) # pra remover labelled variables
library(haven)
library(janitor)
library(lubridate)

library(lubridate)

## dados
# https://www.latinobarometro.org/latContents.jsp

lat_bar23 <- read_dta("Latinobarometro_2023_Eng_Stata_v1_0.dta") %>%
  mutate(S17 = as.Date(as.character(S17), "%Y%m%d")) %>%
  clean_names()

# get_label(lat_bar23)

lat_bar23 <- lat_bar23 %>%
  mutate(data_base = as.Date(paste(diareal, mesreal, "2023", sep="-"), "%d-%m-%Y"),
         idade = year(as.period(interval(s17,data_base))),
         econ_12_meses = ifelse(p6stgbs %in% c(1,2), "better", 
                                ifelse(p6stgbs == 8, NA, "other")),
         econ_12_meses = relevel(as.factor(econ_12_meses), ref = "other"),
         aprovacao_presidente = ifelse(p15stgbs == 0, NA, p15stgbs),
         ideologia = ifelse(p16st %in% c(97, 98, 99), NA, p16st),
         votaria_governo = ifelse(perpart == 4, NA,
                                  ifelse(perpart == 1, 1, 0)),
         genero = factor(sexo, labels = c("homem", "mulher")),
         evangelico = ifelse(s1 %in% c(0,98), NA,
                             ifelse(s1 %in% c(2,3,4,5), 1, 0))) # não considera adventista, testemunha Jeová, Mórmon

br_latbar_23 <- lat_bar23 %>%
  mutate(idenpa = remove_all_labels(idenpa)) %>% # haven_labelled problems
  filter(idenpa == 76) %>% ## seelciona brasil
  filter(!is.na(votaria_governo) & !is.na(evangelico) & !is.na(ideologia) & !is.na(econ_12_meses))

glimpse(br_latbar_23)

reg_full <- lm(votaria_governo ~ ideologia + idade + genero + econ_12_meses + evangelico, data=br_latbar_23 )
summary(reg_full)

reg_res <- lm(votaria_governo ~ ideologia + idade + genero + econ_12_meses , data=br_latbar_23 )
summary(reg_res)

resid_res <- resid(reg_res)
resid_full <- resid(reg_full)
r_partial_evangelico <- (sum(resid_res^2) - sum(resid_full^2))/sum(resid_res^2)
print(r_partial_evangelico)
