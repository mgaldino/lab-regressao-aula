# Pós cap 2 Hansen - parte 1

# efeitos lineares

set.seed(123)
n <- 1000
x <- rnorm(n)
erro <- rnorm(n)
y <- 10 - 2*x + erro
sim_reg <- data.frame(y=y, x=x, erro=erro)

reg <- lm(y ~ x, data=sim_reg)
summary(reg)

# efeitos lineares com dois preditores
x1 <- rnorm(n)
x2 <- rnorm(x)
erro2 <- erro
y <- 10 -2*x1 + 3*x2 + erro2
sim_reg1 <- data.frame(y=y, x1=x1, x2=x2, erro2)

reg1 <- lm(y ~ x1 + x2, data=sim_reg1)
summary(reg1)
new_x <- data.frame(x1 = 0:1, x2 = c(1,1))
predict(reg1, newdata = new_x)
11.06220  - 13.03441 

## efeitos não lineares
x1 <- rnorm(n)
erro3 <- erro
y <- 10 - 2*x1 + 3*x1^2 + erro3

sim_reg2 <- data.frame(y=y, x1=x1, erro3)
# sim_reg2 <- data.frame(y=y, x1=x1, x2 = x1^2, erro3)

reg2 <- lm(y ~ x1 + I(x1^2), data=sim_reg2)
summary(reg2)

new_x <- data.frame(x1 = 0:3)
predict(reg2, newdata = new_x)







