# aula 12 dez diurno
n <- 1000
x1 <- rnorm(n)
x2 <- x1*2
y <- x1 + rnorm(n)
reg <- lm(y ~x1 + x2)
summary(reg)


# outro exemplo
x3 <- rnorm(n)
x4 <- .3*x1 + .4*x3
reg <- lm(y ~x1 + x3 + x4)
summary(reg)

# relação não-linear
x5 <- x1^2
reg <- lm(y ~x1 + x3 + x5)
summary(reg)

# aula 12 dez diurno
n <- 1000
x1 <- rnorm(n)
x2 <- x1*2
x2[1000] <- rnorm(1)
cor(x1,x2)
y <- x1 + rnorm(n)
reg <- lm(y ~x1 + x2)
summary(reg)



# sim erro padrõa sanduícer na mão
library(ggplot2)
library(tidyverse)
set.seed(123)

x <- c(1:8, 10, 15)
y <- c(rnorm(8, mean=5, sd = 1.5), 40, 65)
df <- data.frame(y=y, x=x)

df %>%
  ggplot(aes(x=x, y=y)) + geom_point()

df %>%
  ggplot(aes(x=x, y=y)) + geom_point() + geom_smooth(method="lm")

fit <- lm(y ~x)
summary(fit)

# Sanduíche
n <- length(y)
sigma2 <- sigma(fit)^2
mat_I <- diag(n)
X <- model.matrix(fit)

omega <- sigma2*mat_I
bread <- solve(t(X)%*%X)
meat <- (t(X) %*% omega %*% X)
vce <- bread %*% meat %*% bread
sqrt(diag(vce))

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

library(fixest)
fit1 <- feols(y ~x, data=df, vcov = "HC1")
summary(fit1)
?etable

library(sensemakr)

# loads dataset
data("darfur")

# runs regression model
model <- lm(peacefactor ~ directlyharmed + age + farmer_dar + herder_dar +
              pastvoted + hhsize_darfur + female + village, data = darfur)

summary(model)

sensitivity <- sensemakr(model = model, 
                         treatment = "directlyharmed",
                         benchmark_covariates = "female",
                         kd = 1:3)
sensitivity
