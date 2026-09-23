library(marginaleffects)
library(sjPlot)
set.seed(123)
n <- 100
x1 <- rnorm(n)
x2 <- rnorm(n)
x3 <- rbinom(n, size=1, p=.5)
a <- 2
b1 <- 1.5
b2 <- -1
b3 <- 1
b4 <- 1
b5 <- 1
y <- a+ b1*x1 + b2*x2 + b3*x3*x3 + b4*x1*x3 + b5*x2*x3 + rnorm(n)

reg1 <- lm(y ~ x1*x3 + x2*x3)
summary(reg1)
reg2 <- lm(y ~ x1 + x2 + x3 + x1:x3 + x2:x3 )
summary(reg2)
x3 <- x1*x2
reg3 <- lm(y ~ x1 + x2 + x3)
summary(reg3)


# Pacote marginaleffects
plot_slopes(reg1, variables = "x1", condition = "x2")
plot_model(reg1, type = "pred", terms = c("x1", "x2"))


## erro padrão robusto

library(ggplot2)
library(tidyverse)
set.seed(123)

x <- c(1:8, 10, 15)
y <- c(5 + rnorm(8,sd = 1.5), 40, 65)
df <- data.frame(y=y, x=x)

df %>%
  ggplot(aes(x=x, y=y)) + geom_point() +
  geom_smooth(method = "lm", se=F)

## erro-padrão manual
fit <- lm(y ~x)
summary(fit)

n <- length(y)
x_mat <- model.matrix(fit)
x_t_x <- t(x_mat)%*%x_mat
pao <- solve(x_t_x)

sigma2 <- sigma(fit)^2
identidade <- diag(n)
carne <- t(x_mat)%*%(sigma2*identidade)%*%x_mat
var_beta_hat <- pao%*%carne%*%pao
erro_padrao <- sqrt(diag(var_beta_hat))

# matriz H
mat_h <- x_mat%*%pao%*%t(x_mat)


library(lmtest)
library(sandwich)

coeftest(fit, vcovHC(fit, "HC3"))
coefci(fit, vcov. = vcovHC(fit, type = "HC3"))

## simulando hererocedasticidade com n grande
set.seed(123)
n <- 1000000
x <- rnorm(n)
y <- 2 + 1.5*x + rnorm(n, 0, 1.1*(x/3)^2)
df <- data.frame(y=y, x=x)

fit1 <- lm(y~x, data=df)

df %>%
  ggplot(aes(y=y, x=x)) + geom_point()
system.time(coeftest(fit1, vcovHC(fit1, "HC3")))
system.time(coeftest(fit1, vcovHC(fit1, "HC1")))
