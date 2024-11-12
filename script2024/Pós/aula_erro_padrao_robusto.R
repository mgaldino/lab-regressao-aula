set.seed(123)
n <- 1000
u <- rnorm(n)
x1 <- u + rnorm(n)
x2 <- u + rnorm(n)
y <- x1 + x2 + rnorm(n)

reg <- lm(y ~ x1 + x2)

reg2 <- lm(x1 ~ x2)
novo_x1 <- residuals(reg2)
reg3 <- lm(y ~novo_x1)

summary(reg)
summary(reg3)


## Termo de interação
library(marginaleffects)
library(sjPlot)
set.seed(123)
n <- 100
x1 <- rnorm(n)
x2 <- rbinom(n, size=1, p=.5)
a <- 2
b1 <- 1.5
b2 <- -1
b3 <- 1
y <- a+ b1*x1 + b2*x2 + b3*x2*x1 + rnorm(n)

reg1 <- lm(y ~ x1*x2)
summary(reg1)

reg2 <-  lm(y ~ x2/x1)
summary(reg2)
