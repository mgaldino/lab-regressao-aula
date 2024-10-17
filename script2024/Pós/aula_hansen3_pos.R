#Pós

set.seed(1234)
n <- 1000
x <- rnorm(n, 0,1)
e <- rnorm(n, 0,1)
y <- 2 - 3*x + e

mat_x <- matrix(c(rep(1,n), x), nrow=n, ncol=2)
mat_y <- matrix(y, nrow=n, ncol=1)
mat_x_t <- t(mat_x)

produto_xx <- mat_x_t %*% mat_x
inv_xx <- solve(produto_xx)

produto_xy <- mat_x_t %*%mat_y

beta_hat <- inv_xx %*% mat_x_t %*%mat_y

# modelo reg brexit

library(data.table)
library(tidyverse)
library(here)
library(readr)

bes <- read_csv(here("dados brutos 2024", "BES.csv"))
View(bes)

reg <- lm(leave ~age, data = bes)
summary(reg)
