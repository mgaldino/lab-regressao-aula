#3 Aula diurno cap 7

library(ggplot2)
library(tidyverse)

# vetor de médias
set.seed(123)
vetor_media <- c(0,0)

sigma_x <- 2
sigma_y <- 2
rho <- .6

matriz_var_cov <- matrix(c(sigma_x^2, rho*sigma_x*sigma_y, 
                           rho*sigma_x*sigma_y, sigma_y^2 ),
                         nrow=2)

norm_bivariada <- MASS::mvrnorm(n = 10000,
                                mu = vetor_media, Sigma  = matriz_var_cov)

bivariada_tibble1 <- as_tibble(norm_bivariada, 
                               .name_repair =
                                 "universal") %>%
  rename(x = '...1', 
         y = '...2')

bivariada_tibble1 %>%
  ggplot(aes(x)) + geom_density()

bivariada_tibble1 %>%
  ggplot(aes(y)) + geom_density()

bivariada_tibble1 %>%
  ggplot(aes(x, y)) + geom_density_2d()

bivariada_tibble1 %>%
  ggplot(aes(x, y)) + geom_point()

bivariada_tibble1 <- bivariada_tibble1 %>%
  mutate(x = x + 10)

bivariada_tibble1 %>%
  ggplot(aes(x, y)) + geom_point()

# var (x) é 16
set.seed(123)
vetor_media <- c(0,0)

sigma_x <- 4
sigma_y <- 2
rho <- .6

matriz_var_cov <- matrix(c(sigma_x^2, rho*sigma_x*sigma_y, 
                           rho*sigma_x*sigma_y, sigma_y^2 ),
                         nrow=2)

norm_bivariada <- MASS::mvrnorm(n = 10000,
                                mu = vetor_media, Sigma  = matriz_var_cov)

bivariada_tibble1 <- as_tibble(norm_bivariada, 
                               .name_repair =
                                 "universal") %>%
  rename(x = '...1', 
         y = '...2')

bivariada_tibble1 %>%
  ggplot(aes(x, y)) + geom_point()


#correlação é .3
set.seed(123)
vetor_media <- c(0,0)

sigma_x <- 2
sigma_y <- 2
rho <- .3

matriz_var_cov <- matrix(c(sigma_x^2, rho*sigma_x*sigma_y, 
                           rho*sigma_x*sigma_y, sigma_y^2 ),
                         nrow=2)

norm_bivariada <- MASS::mvrnorm(n = 10000,
                                mu = vetor_media, Sigma  = matriz_var_cov)

bivariada_tibble1 <- as_tibble(norm_bivariada, 
                               .name_repair =
                                 "universal") %>%
  rename(x = '...1', 
         y = '...2')

bivariada_tibble1 %>%
  ggplot(aes(x, y)) + geom_point()


#correlação é -.3
set.seed(123)
vetor_media <- c(0,0)

sigma_x <- 2
sigma_y <- 2
rho <- -.3

matriz_var_cov <- matrix(c(sigma_x^2, rho*sigma_x*sigma_y, 
                           rho*sigma_x*sigma_y, sigma_y^2 ),
                         nrow=2)

norm_bivariada <- MASS::mvrnorm(n = 10000,
                                mu = vetor_media, Sigma  = matriz_var_cov)

bivariada_tibble1 <- as_tibble(norm_bivariada, 
                               .name_repair =
                                 "universal") %>%
  rename(x = '...1', 
         y = '...2')

bivariada_tibble1 %>%
  ggplot(aes(x, y)) + geom_point()

#correlação é -.9
set.seed(123)
vetor_media <- c(0,0)

sigma_x <- 2
sigma_y <- 2
rho <- -.9

matriz_var_cov <- matrix(c(sigma_x^2, rho*sigma_x*sigma_y, 
                           rho*sigma_x*sigma_y, sigma_y^2 ),
                         nrow=2)

norm_bivariada <- MASS::mvrnorm(n = 10000,
                                mu = vetor_media, Sigma  = matriz_var_cov)

bivariada_tibble1 <- as_tibble(norm_bivariada, 
                               .name_repair =
                                 "universal") %>%
  rename(x = '...1', 
         y = '...2')

bivariada_tibble1 %>%
  ggplot(aes(x, y)) + geom_point()

# Plug-in estimators
# cálculo manual

(beta_hat <- cov(bivariada_tibble1$x,
                 bivariada_tibble1$y)/var(bivariada_tibble1$x))

(alpha_hat <- mean(bivariada_tibble1$y) - 
  beta_hat*mean(bivariada_tibble1$x))

reg <- lm(y ~x, data=bivariada_tibble1)
summary(reg)

##################
## Entendendo o erro padrão do beta_hat
############

# 1. mudando a variância do erro
set.seed(123)
vetor_media <- c(0,0)

n <- 1000
sigma_x <- 2
sigma_y <- c(2,4,6,8)
rho <- -.9

lista_mat_var_cov <- list()
lista_bi_norm <- list()
for (i in 1:4) {
  lista_mat_var_cov[[i]] <- matrix(c(sigma_x^2, 
                                     rho*sigma_x*sigma_y[i], 
                                rho*sigma_x*sigma_y[i], 
                                sigma_y[i]^2 ),
                              nrow=2)
  
  aux <- MASS::mvrnorm(n = n,
                       mu = vetor_media,
                       Sigma  = lista_mat_var_cov[[i]])
  
  lista_bi_norm[[i]] <- as_tibble(aux, 
                                  .name_repair =
                                    "universal") %>%
    rename(x = '...1', 
           y = '...2')
  
}

lista_bi_norm[[1]] %>%
  ggplot(aes(x, y)) + geom_point() +
  geom_smooth(method="lm", se=F)

lista_bi_norm[[2]] %>%
  ggplot(aes(x, y)) + geom_point() +
  geom_smooth(method="lm", se=F)

lista_bi_norm[[3]] %>%
  ggplot(aes(x, y)) + geom_point() +
  geom_smooth(method="lm", se=F)

lista_bi_norm[[4]] %>%
  ggplot(aes(x, y)) + geom_point() +
  geom_smooth(method="lm", se=F)

# computando o "erro" (na verdade é resíduo, mas vamos fingir que é o erro)

reg1 <- lm(y ~ x, data = lista_bi_norm[[1]])
prev1 <- predict(reg1)
erro1 <- lista_bi_norm[[1]]$y - prev1
var(erro1)
sd(erro1)

# erro para banco de dados 2
reg2 <- lm(y ~ x, data = lista_bi_norm[[2]])
prev2 <- predict(reg2)
erro2 <- lista_bi_norm[[2]]$y - prev2
var(erro2)
sd(erro2)

# var erro 3 banco dados
reg3 <- lm(y ~ x, data = lista_bi_norm[[3]])
prev3 <- predict(reg3)
erro3 <- lista_bi_norm[[3]]$y - prev3
var(erro3)
sd(erro3)

# var erro 4o banco dados
reg4 <- lm(y ~ x, data = lista_bi_norm[[4]])
prev4 <- predict(reg4)
erro4 <- lista_bi_norm[[4]]$y - prev4
var(erro4)
sd(erro4)

df_var_erro <- data.frame(var_erro = c(var(erro1),
                                       var(erro2), 
                                       var(erro3),
                                       var(erro4)))
# 
# head(sort(prev1, decreasing = T))
# head(sort(lista_bi_norm[[1]]$y, decreasing=T))

## Aumentando a variância amostral do X
set.seed(123)
vetor_media <- c(0,0)

sigma_x <- 2
sigma_y <- 2
rho <- -.9
# gerando uma população de 100k observações
matriz_var_cov <- matrix(c(sigma_x^2, rho*sigma_x*sigma_y, 
                           rho*sigma_x*sigma_y, sigma_y^2 ),
                         nrow=2)

norm_bivariada <- MASS::mvrnorm(n = 100000,
                                mu = vetor_media, Sigma  = matriz_var_cov)

bivariada_tibble1 <- as_tibble(norm_bivariada, 
                               .name_repair =
                                 "universal") %>%
  rename(x = '...1', 
         y = '...2')

# olhyando primeiro e terceiro quartis
summary(bivariada_tibble1$x)

amostra_bivariada_tibble1 <- bivariada_tibble1 %>%
  filter(x > -1.35 & x < 1.35)

vec_beta_hat <- numeric()
vec_dp_x <- numeric()
for ( i in 1:100) {
  amostra <- amostra_bivariada_tibble1 %>%
    slice_sample(n=1000)
  vec_dp_x[i] <- sd(amostra$x)
  reg <- lm(y ~x , data=amostra)
  vec_beta_hat[i] <- coef(reg)[2]
}
head(vec_dp_x)

true_reg <- lm(y ~x , data=bivariada_tibble1) 
(true_beta <-  coef(true_reg)[2])
mean(vec_beta_hat) # -0.8991572
sd(vec_beta_hat) # 0.0387309

# variância do x será maior

vec_beta_hat <- numeric()
vec_dp_x <- numeric()
for ( i in 1:100) {
  amostra <- bivariada_tibble1 %>%
    slice_sample(n=1000)
  vec_dp_x[i] <- 0.01479609
  reg <- lm(y ~x , data=amostra)
  vec_beta_hat[i] <- coef(reg)[2]
}
head(vec_dp_x)

true_reg <- lm(y ~x , data=bivariada_tibble1) 
(true_beta <-  coef(true_reg)[2])
mean(vec_beta_hat) # -0.9000292
sd(vec_beta_hat) # 0.0387309
