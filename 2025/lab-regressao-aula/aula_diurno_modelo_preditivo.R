glimpse(df)

# Instale o pacote ggplot2 se ainda não estiver instalado
# install.packages("ggplot2")

# Carregue as bibliotecas necessárias
library(ggplot2)

# Supondo que você tenha um data.frame chamado df com a variável de resíduos "residuo"
# e uma distribuição normal de referência chamada density_points

# Crie um objeto ggplot com os dados dos resíduos
p <- ggplot(df, aes(x = residuos))

# Adicione o gráfico de densidade dos resíduos
p <- p + geom_density(fill = "blue", alpha = 0.5)

# Adicione a curva de densidade da distribuição normal de referência
p <- p + geom_density(aes(x = density_points), fill = "red", alpha = 0.5)

# Adicione rótulos e título ao gráfico
p <- p + labs(title = "Verificação da Normalidade dos Resíduos",
              x = "Resíduos", y = "Densidade")

# Mostre o gráfico
print(p)


### 2a tentativa

# Instale o pacote ggplot2 se ainda não estiver instalado
# install.packages("ggplot2")

# Carregue as bibliotecas necessárias
library(ggplot2)

# Supondo que você tenha um data.frame chamado df com a variável de resíduos "residuo"
# e uma distribuição normal de referência chamada density_points

# Crie um objeto ggplot com os dados dos resíduos
p <- ggplot(df, aes(x = residuos))

# Adicione um histograma dos resíduos
p <- p + geom_histogram(fill = "blue", alpha = 0.5)

# Adicione a curva de densidade da distribuição normal de referência
p <- p + geom_density(aes(x = density_points), fill = "red", alpha = 0.5)

# Adicione rótulos e título ao gráfico
p <- p + labs(title = "Verificação da Normalidade dos Resíduos",
              x = "Resíduos", y = "Frequência")

# Mostre o gráfico
print(p)

# quarta tentativa

p <- ggplot(df, aes(x = residuos))

# Adicione um histograma dos resíduos
p <- p + geom_histogram( fill = "blue", alpha = 0.5)

# Adicione a curva de densidade da distribuição normal de referência
p <- p + geom_density(aes(x = density_points, y = ..scaled..), fill = "red", alpha = 0.5)

# Adicione rótulos e título ao gráfico
p <- p + labs(title = "Verificação da Normalidade dos Resíduos",
              x = "Resíduos", y = "Frequência")

#3 Quantile

x <- rnorm( 1000)

q50 <- quantile(x, .5)
q025 <- quantile(x, .025)
q975 <-quantile(x, .975)

print(c(q50, q025, q975))

# QQ plot SC
qqnorm(residuals(reg1))
qqline(residuals(reg1))


#3 Modelo preditivo PE

presid_al18


presid_al18 <- presid_al18 %>%
  filter(SG_UF =="SC")