set.seed(123)
n <- 1000
x <- rnorm(n)
erro <- rnorm(n)
y <- 2 + 3*x + erro
reg <- lm(y ~x)
summary(reg)


## Resíduos
library(tidyverse)
set.seed(123)

n <- 1000
df_nl <- data.frame(x = rnorm(n), e = rnorm(n))
df_nl <- df_nl %>%
  mutate(y = 2 + 1.5*x - .5*x^2 + e)

reg_nl <- lm(y ~x, df_nl)
df <- data.frame(residuos = residuals(reg_nl),
                 preditor = df_nl$x)

df %>%
  ggplot(aes(x=preditor, y = residuos)) + geom_point() + 
  geom_smooth(se=F)

df_nl <- df_nl %>%
  mutate(x_sq = x^2)
reg_nl <- lm(y ~x + x_sq, df_nl)
summary(reg_nl)

df <- data.frame(residuos = residuals(reg_nl),
                 preditor = df_nl$x)

df %>%
  ggplot(aes(x=preditor, y = residuos)) + geom_point() + 
  geom_smooth(method="lm", se=F)

qqnorm(residuals(reg))
qqline(residuals(reg))

set.seed(123)
var_norm <- rnorm(1000)
qqnorm(var_norm)
qqline(var_norm)

set.seed(123)
var_norm <- rnorm(1000)^2
qqnorm(var_norm)
qqline(var_norm)
