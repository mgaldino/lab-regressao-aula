## Aula vespertino cap 12 - parte I

set.seed(1234)
n <- 200
x1 <- rnorm(n)
x2 <- rnorm(n)
alpha <- 2
beta1 <- -2
beta2 <- 2.5
erro <- rnorm(n)
y <- alpha + beta1*x1 + beta2*x2 + erro

reg <- lm(y ~ x1 + x2)
summary(reg)
x_mat <- cbind(1, x1, x2)
x_mat_t <- t(x_mat)
prod_x <- x_mat_t %*% x_mat
inv_prod <- solve(prod_x)

x_y <- x_mat_t %*% y
beta_hat <- inv_prod %*% x_y

## Simulação regressão
set.seed(123)
n <- 1000
u <- rnorm(n)
x1 <- u + rnorm(n)
x2 <- u + rnorm(n)
y <- x1 + x2 + rnorm(n)

df <- data.frame(y=y, x1=x1, x2=x2)
reg <- lm(y ~ x1 + x2, data=df)
summary(reg)

reg1 <- lm(y ~x1, data=df)
summary(reg1)

reg_aux <- lm(x1 ~ x2, data=df)
summary(reg_aux)

library(tidyverse)
df <- df %>%
  mutate(novo_x1 = residuals(reg_aux))

reg2 <- lm(y ~ novo_x1, data=df)
summary(reg2)


## 
set.seed(123)
n <- 1000
u <- rnorm(n)
x1 <- u + rnorm(n)
x2 <- u + rnorm(n)
y <- x1 + -2*x2 + rnorm(n)

df <- data.frame(y=y, x1=x1, x2=x2)
reg <- lm(y ~ x1 + x2, data=df)
summary(reg)

reg1 <- lm(y ~x1, data=df)
summary(reg1)

reg_aux <- lm(x1 ~ x2, data=df)
summary(reg_aux)

library(tidyverse)
df <- df %>%
  mutate(novo_x1 = residuals(reg_aux))

reg2 <- lm(y ~ novo_x1, data=df)
summary(reg2)
