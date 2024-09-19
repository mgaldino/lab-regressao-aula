## Aula noite cap 6

set.seed(234)
n <- 1000
x <- rnorm(n)
u <- rnorm(n)
y <- x^2 + u

library(ggplot2)
library(tidyverse)
sim_cef <- data.frame(y=y, x=x, u=u)

sim_cef %>%
  ggplot(aes(y=y, x=x)) + geom_point()

# exemplo de uso da função cut
cut(1:10, c(0,5,11))

# criando os grupos com cut no banco de dados
sim_cef <- sim_cef %>%
  mutate(grupo = cut(x, seq(-3.5, 3.5, by=.5)))

sim_cef <- sim_cef %>%
  group_by(grupo) %>%
  mutate(y_medio_condicional = mean(y),
         erro = y - y_medio_condicional)

sim_cef %>%
  group_by(grupo) %>%
  summarise(erro_medio_condicional = mean(erro))

sim_cef %>%
  ungroup() %>% # desagrupando
  summarise(erro_medio_condicional = mean(erro))


## Simulação da Reta

set.seed(1234)
n <- 1000
x <- rnorm(n)
u <- rnorm(n)
y <- 2 + 1.3*x + u

df <- data.frame(y=y, x=x)
df %>%
  ggplot(aes(x, y)) + geom_point() + geom_smooth(se=F, method="lm") +
  geom_abline(slope= .5, intercept = 1, colour="red") +
  geom_abline(slope= 3, intercept = 3, colour="green", size=1) +
  geom_abline(slope= 0, intercept = 2, colour="grey", size=1)

beta <- cov(df$y, df$x)/var(df$x)
alpha <- mean(df$y) - beta*mean(df$x)
beta
alpha

df %>%
  ggplot(aes(x, y)) + geom_point() + 
  geom_smooth(se=F, method="lm") +
  geom_abline(slope= beta, intercept = alpha, colour="red")
  
# regressão
reg <- lm(y ~x, data=df)
summary(reg)
