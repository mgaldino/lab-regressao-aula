## Aula de simulação no R

# soma de 1 até 9999 (úmeros ímpares)
sum(seq(1, 9999, by=2))

# mudar notação científica
options(scipen=90)
sum(seq(1, 9999, by=2))

# Simulando uma variável normalmente distribuída
x <- rnorm(n=1000, 0, 1)

#média
mean(x)

## Características da Normal

#simetria em torno da média

# inspecionando os dados (6 primeiros elementos)
head(x)

# inspecionado checagem lógica de que x < média de x (6 primeiros resultados)
head(x < mean(x))

# checagem lógica de que x < média de x
# atribuindo para comparacao
comparacao <- x < mean(x)

# entendendo soma de vetor lógico (TRUE, FALSE)
sum(c(FALSE,TRUE,FALSE, TRUE))

# percentual de casos abaixo da média
sum(comparacao)/length(x)

# percentual de casos acima da média
1 - sum(comparacao)/length(x)

# qtos % dos casos entre -2dp e +2dp da média

# desvio padrão
dp_x <- sd(x)

mean(x) + 2*dp_x
mean(x) - 2*dp_x
comparacao1 <- x > -1.968299 & x < 2.020512

# % de casos entre -2dp e + 2dp da média
sum(comparacao1)/length(x)

# % de casos entre - 1dp e + 1dp da média

comparacao2 <- (x > mean(x) - dp_x & x < mean(x) + dp_x)
sum(comparacao2)/length(x)
# 0.661

#semente para garantir reproducibilidade
set.seed(123)

# refazendo código com a semente
x <- rnorm(1000, 0, 1)
dp_x <- sd(x)
comparacao2 <- (x > mean(x) - dp_x & x < mean(x) + dp_x)
perc_casos <- sum(comparacao2)/length(x)
print(perc_casos, digits = 10)


# Teorema do Limite Central
y <- runif(100, min=0, max=10)
media_y <- mean(y)
sd(y)
dp_populacional_unif <- sqrt((10-0)^2/12)

# 5000 obs
y <- runif(5000, min=0, max=10)
vetor_medias_y <- c(mean(y[1:100]), mean(y[101:200]))

## loop

# somar de 1 até 10

# modo 1 
sum(1:10)

# modo 2
1+2+3+4+5+6+7+8+9+10

# modo 3, usando loop

# cria um objeto que vai receber a soma
soma <- 0

# faz o loop
for(i in 1:10) {
  soma <- i + soma
}

# armazenar números em um vetor
# modo 1
vetor <- 1:10
vetor

# loop para armazenar números em um vetor

# cria um vetor vazio que recebe só números
vetor <- numeric()

# loop que coloca os números no vetor
for(i in 1:10){
  vetor[i] <- i
}

# imprimindo números de 1 a 5
for(i in 1:5) {
  print(i)
}

#exercício em sala de aula
# faça um loop para imprimir números ímpares de 1 até 9

for(i in seq(1, 9, by=2)){
  print(i)
}

# exercício em sala de aula
# faça um loop que compute a média de 1:10, 11:20, 21:30
vetor_ex <- 1:30

# entendendo a lógica para criar o loop
# print(vetor_ex[(1+10*(i-1)):(10*i)])
# print(vetor_ex[(1+10*(i-1)):(10*i)])
# print(vetor_ex[(1+10*(i-1)):(10*i)])

# loop que imprime os vetores
for (i in 1:3){
  print(vetor_ex[(1+10*(i-1)):(10*i)])
}

# agora, loop que armazena as médias dos vetores
resultado <- numeric()
for (i in 1:3){
  resultado[i] <- mean(vetor_ex[(1+10*(i-1)):(10*i)])
}

# imprime o resultado para ver que está certo
resultado

# guardando 1000 médias amostrais
y <- runif(100000, min=0, max=10)

# como quero 1000 médias amostrais, loop de 1 até 1000
# mas cada amostra é de tamanho 100
resultado_medias <- numeric()
for ( i in 1:1000) {
  resultado_medias[i] <- mean(y[(1+100*(i-1)):(100*i)])
}
resultado_medias

# histograma
hist(resultado_medias, breaks=15)

# média das médias
mean(resultado_medias)

# desvio-padrão das médias
sd(resultado_medias)

# % de casos acima e abaixo da média +- DP
dp_xbar <- sd(resultado_medias)
media_xbar <- mean(resultado_medias)
lim_inf <- media_xbar - 2*dp_xbar
lim_sup <- media_xbar + 2*dp_xbar
xbar <- resultado_medias
comparacao3 <- (xbar > lim_inf & xbar < lim_sup)
perc_casos <- sum(comparacao3)/length(xbar)
print(perc_casos, digits = 10)

# simulação peso e altura
set.seed(123)
altura <- rnorm(100, 170, sd=12 )
hist(altura, breaks=30)
peso <- 0.5*altura + rnorm(100, 0, 15)
hist(peso)

#cria um banco de dados de peso e altura
dados_peso_alt <- data.frame(peso=peso, altura=altura)

library(ggplot2) # ggplot2 para gráficos
library(tidyverse) # tidyverse para o pipe, %>%

# scatterplot
dados_peso_alt %>%
  ggplot(aes(x=altura, y=peso)) + geom_point()

# scatterplot com reta de regressão ajustada
dados_peso_alt %>%
  ggplot(aes(x=altura, y=peso)) + geom_point() +
  geom_smooth(method="lm")

#correlação
cor(altura, peso)

# correlação de 10% em vez de 50%
set.seed(123)
altura <- rnorm(100, 170, sd=12 )
hist(altura, breaks=30)
peso <- 0.1*altura + rnorm(100, 0, 15)
hist(peso)

dados_peso_alt <- data.frame(peso=peso, altura=altura)

dados_peso_alt %>%
  ggplot(aes(x=altura, y=peso)) + geom_point() +
  geom_smooth(method="lm")

# prompt no chatgpt que deu resposta errada
# 
# No R, gerei uma distribuição de 100k observações de uma uniforme entre 0 e 10 (y <- runif(100000, min=0, max=10). Quero armazenar em um vetor, cujo nome será resultado_medias, 1000 médias de amostras de y, ou seja, cada amostra deve ter 100 elementos e eles não devem se repetir de amostra para amostra. Como você implementaria um código em R para fazer essa tarefa.