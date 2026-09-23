
n <- 1000
aux <- rnorm(n)
x <- ifelse(aux < -1, 0, 
            ifelse(aux > 1, 2, 1))
e <- rnorm(n)
x1 <- ifelse(x == 1, 1, 0)
x2 <- ifelse(x == 2, 2, 0)
alpha <- 2
beta1 <- 3
beta2 <- -1
y <- alpha + beta1*x1 + beta2*x2 + e

df <- data.frame(y=y, x = factor(x,
                                 labels = c("cat1", "cat2", "cat3")))
View(df)
class(df$x)
reg <- lm (y ~ x, data=df)
summary(reg)

library(tidyverse)
df <- df %>%
  mutate(x = relevel(x, ref="cat2"))

reg1 <- lm (y ~ x, data=df)
summary(reg1)

# dois preditores categóricos

<- 1000
aux <- rnorm(n)
x <- ifelse(aux < -1, 0, 
            ifelse(aux > 1, 2, 1))
e <- rnorm(n)
x1 <- ifelse(x == 1, 1, 0)
x2 <- ifelse(x == 2, 2, 0)
z <- rbinom(n, 1, p=.5)
alpha <- 2
beta1 <- 3
beta2 <- -1
beta3 <- 2
y <- alpha + beta1*x1 + beta2*x2 + beta3*z + e

df <- data.frame(y=y,
                 x = factor(x, labels = 
                              c("cat1", "cat2", "cat3")),
                 z = factor(z, 
                            labels = 
                              c("outra_categ1", "outra_categ2")))
View(df)

reg2 <- lm( y ~ x + z, data=df)
summary(reg2)
cor(x, e)
