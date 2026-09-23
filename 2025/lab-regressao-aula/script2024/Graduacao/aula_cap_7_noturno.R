library(ggplot2)
library(tidyverse)

# vetor de médias
vetor_media <- c(0,0)

sigma_x <- 2
sigma_y <- 2
rho <- .5

matriz_var_cov <- matrix(c(sigma_x^2, 
                           rho*sigma_x*sigma_y,
                           rho*sigma_x*sigma_y,
                           sigma_y^2 ), 
                         nrow=2)

set.seed(345)
norm_bivariada <- MASS::mvrnorm(n = 10000, mu = vetor_media, Sigma  = matriz_var_cov)

bivariada <- as.data.frame(norm_bivariada) %>%
  rename(x = V1, y = V2)
# 
# bivariada %>%
#   ggplot(aes(x)) + geom_density()
# 
# bivariada %>%
#   ggplot(aes(y)) + geom_density()

# bivariada %>%
#   ggplot(aes(x, y)) + geom_density_2d()

bivariada %>%
  ggplot(aes(x, y)) + geom_point()

(beta_hat <- cov(bivariada$y, bivariada$x)/var(bivariada$x))
(alfa_hat <- mean(bivariada$y) - beta_hat*mean(bivariada$x))

reg <- lm(y ~x , data=bivariada)
summary(reg)

# Simulando beta hat não viesado
vetor_media <- c(0,0)

sigma_x <- 2
sigma_y <- 2
rho <- .5
matriz_var_cov <- matrix(c(sigma_x^2, 
                           rho*sigma_x*sigma_y,
                           rho*sigma_x*sigma_y,
                           sigma_y^2 ), 
                         nrow=2)
vec_beta_hat <- numeric()
for( i in 1:100) {

  norm_bivariada <- MASS::mvrnorm(n = 10000, mu = vetor_media, Sigma  = matriz_var_cov)
 
  bivariada <- as.data.frame(norm_bivariada) %>%
    rename(x = V1, y = V2)
  
  reg <- lm(y ~x , data=bivariada)
  vec_beta_hat[i] <- coef(reg)[2]
}

head(vec_beta_hat)
hist(vec_beta_hat)
mean(vec_beta_hat)
sd(vec_beta_hat)

# entendendo o erro padrão

# mudando o desvio padrão do erro
vetor_media <- c(0,0)

sigma_x <- 2
sigma_y <- seq(1.1,11,by=.1)
rho <- .5

erro_padrao <- numeric()
for( i in 1:100) {
  matriz_var_cov <- matrix(c(sigma_x^2, 
                             rho*sigma_x*sigma_y[i],
                             rho*sigma_x*sigma_y[i],
                             sigma_y[i]^2 ), 
                           nrow=2)
  
  norm_bivariada <- MASS::mvrnorm(n = 10000, mu = vetor_media, Sigma  = matriz_var_cov)
  
  bivariada <- as.data.frame(norm_bivariada) %>%
    rename(x = V1, y = V2)
  
  reg <- lm(y ~x , data=bivariada)
  resumo <- summary(reg)
  
  # Acessando o erro padrão do coeficiente do preditor
  erro_padrao[i] <- resumo$coefficients["x", "Std. Error"]
}

df <- data.frame(erro_padrao = erro_padrao, n_sim = 1:100)
df %>%
  ggplot(aes(x=n_sim, y=erro_padrao)) + geom_line()
