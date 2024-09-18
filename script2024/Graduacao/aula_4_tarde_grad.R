# aula grad tarde

set.seed(234)
n <- 1000
x <- rnorm(n)
u <- rnorm(n)
y <- x^2 + u

df <- data.frame(y=y, x=x, u=u)
library(ggplot2)
library(tidyverse)
df %>%
  ggplot(aes(y=y, x=x)) + geom_point() +
  geom_smooth(method = "lm", se=FALSE)

df <- df %>%
  mutate(bin = cut(x, seq(-3.5, 3.5, by=.5 )))

cef <- df %>%
  group_by(bin) %>%
  mutate(esp_y_dado_x = mean(y),
         erro = y - esp_y_dado_x)

cef %>%
  ungroup() %>%
  summarise(esp_erro = mean(erro))

cef %>%
  summarise(esp_erro_cond = mean(erro)) 

## Reta de Regressão pelo R
reg <- lm(y ~ x, data=df)
summary(reg)

## calculando coeficientes manualmente
beta <- cov(df$y, df$x)/var(df$x)
alpha <- mean(df$y) - beta*mean(df$x)

# Exercício em sala

# Simulem y = 10 + 2*x + erro
# x <- rnorm(n)
# erro <- rnorm(n, 0, 5)
# calculem manualmente e usando lm
# verifiquem que dá aproximadamente igual

set.seed(123)
n <- 1000
x <- rnorm(n)
erro <- rnorm(n, mean=0, sd=5)
y <- 10 + 2*x + erro
df_exercicio_reg <- data.frame(y=y, x=x, erro=erro)
beta <- cov(df_exercicio_reg$y, df_exercicio_reg$x)/var(df_exercicio_reg$x)
alpha <- mean(df_exercicio_reg$y) - beta*mean(df_exercicio_reg$x)

# pelo R
reg1 <- lm(y ~x, data = df_exercicio_reg)
summary(reg1)

# previsões manuais
(y_hat <- alpha + beta*0) # x = 0
(y_hat <- alpha + beta*1) # x = 1
(y_hat <- alpha + beta*2) # x = 2
(y_hat <- alpha + beta*-1) # x = -1

# previsões automáticas pelo
df_predict = data.frame(x=c(0,1,2,-1))
predict(reg1, newdata=df_predict)

df_exercicio_reg %>%
  ggplot(aes(y=y, x=x)) + geom_point() +
  geom_abline(intercept = alpha, slope=beta)

## Regressão múltipla

x1 <- rnorm(n)
x2 <- rnorm(n)
erro <- rnorm(n)
y <- 10 + 2*x1 - 3*x2 + erro
df_reg_m <- data.frame(y=y, x1=x1, x2=x2, erro = erro)
reg2 <- lm(y ~ x1 + x2, data = df_reg_m )
summary(reg2)
