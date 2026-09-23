# Termo interação aula grad diurno

set.seed(123)
n <- 1000
x1 <- rnorm(n)
x2 <- rbinom(n, 1, .5)
alpha <- 2
beta1 <- 2
beta2 <- -1
beta3 <- 1
y <- alpha + beta1*x1 + beta2*x2 + beta3*x1*x2 + rnorm(n)
reg <- lm(y ~ x1*x2)

x3 <- x1*x2
reg1 <- lm(y ~ x1 + x2 + x3)
summary(reg)
summary(reg1)

library(ggplot2)
library(tidyverse)
new_data <- data.frame(x1=x1, x2=0, x3 = 0)

# quando x2=0, qual o y previsto?
df <- data.frame(y_x2_0 = predict(reg, newdata = new_data),
                 x1=x1)

# quando x2=1, qual o y previsto?
new_data1 <- data.frame(y=y, x1=x1, x2=1, x3 = x1*1)

df1 <- data.frame(y_x2_1 = predict(reg, newdata = new_data1),
                 x1=x1)
p <- df %>%
  ggplot(aes(y=y_x2_0, x=x1 )) + geom_point() +
  geom_smooth(method="lm")
print(p)

p <- p + geom_point(data=df1,
                    aes(y=y_x2_1, x=x1), colour="blue")
print(p)

library(marginaleffects)
library(sjPlot)

marginaleffects::plot_slopes(reg, variables = "x1", condition = "x2")
sjPlot::plot_model(reg, type = "pred", terms = c("x1", "x2"))

# termo interação com variáveis contínuas

set.seed(123)
x1 <- rnorm(n)
x2 <- rnorm(n)
y <- alpha + beta1*x1 + beta2*x2 + beta3*x1*x2 + rnorm(n)
reg2 <- lm(y ~x1*x2)
summary(reg2)
sjPlot::plot_model(reg2, type = "pred", terms = c("x1", "x2"))

reg2 <- lm(y ~x1 + x2 + x1:x2)

## erro padrão robusto

library(ggplot2)
library(tidyverse)
set.seed(123)

x <- c(1:8, 10, 15)
y <- c(5 + rnorm(8,sd = 1.5), 40, 65)
df <- data.frame(y=y, x=x)

df %>%
  ggplot(aes(x=x, y=y)) + geom_point() + geom_smooth(method="lm")

fit <- lm(y ~x)
df <- data.frame(residuos_sq = residuals(fit)^2, x = df$x)
summary(fit)
df %>%
  ggplot(aes(y=residuos_sq, x=x)) + geom_point()

X <- model.matrix(fit)
X_t_X <- t(X)%*%X
pao <- solve(X_t_X)

n <- length(y)
sigma2 <- sigma(fit)^2
Identidade <- diag(n)
var_erro <- sigma2*Identidade
carne <- t(X)%*%var_erro%*%X
var_b_x <- pao%*%carne%*%pao
sqrt(diag(var_b_x))


library(lmtest)
library(sandwich)

coeftest(fit, vcovHC(fit, "HC3"))
# IC
coefci(fit, vcov. = vcovHC(fit, type = 'HC3'))


# latino barometro
library(here)
latinobar <- readRDS(here("dados brutos 2024",
                          "Latinobarometro_2023_Esp_Rds_v1_0.rds"))
glimpse(latinobar)

# pergunta P62ST

aux <- latinobar %>%
  group_by(idenpa) %>%
  mutate(num_obs = n()) %>%
  filter(P62ST_4 == -1) %>%
  summarise(perc_n_conhecem = n()/num_obs)

View(aux)
bra <- latinobar %>%
  filter(idenpa == 76)

# tarefas
# P1ST transformar em binária (0 ou 1)
# p15stgbs transformar em binária (factor)
# sexo - binária (factor)

bra <- bra %>%
  dplyr::filter(P1ST !=0 & P15STGBS != 0) %>%
  mutate(P1ST = ifelse(P1ST %in% c(1,2), 1, 0),
         P15STGBS = ifelse(P15STGBS == 2, 1, 0),
         P15STGBS = as.factor(P15STGBS),
         sexo = factor(sexo, labels = c("homem", "mulher")))

reg_bra <- lm(P1ST ~ P15STGBS + sexo + edad, data=bra)
summary(reg_bra)
# rodar um modelo lm P1ST ~ p15stgbs + sexo + edad
coeftest(reg_bra, vcovHC(reg_bra))

# termo de interação

reg_bra2 <- lm(P1ST ~ P15STGBS*sexo + edad, data=bra)
summary(reg_bra2)

marginaleffects::plot_slopes(reg_bra2, variables = "P15STGBS", condition = "sexo")
sjPlot::plot_model(reg_bra2, type = "int", terms = c("P15STGBS", "sexo"))
